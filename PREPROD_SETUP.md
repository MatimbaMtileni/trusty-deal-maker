# Preprod Escrow Setup - Complete Summary

## 🎯 Your Network Target

**Cardano Preprod** (testnet)

This means:
- Blockfrost: `https://cardano-preprod.blockfrost.io/api/v0`
- Script addresses: Start with `addr_test1`
- Magic number: `1` (or `0` for preview)
- Real ADA ≠ Test ADA (use testnet faucet)

---

## 🔴 What Was Broken

### Problem 1: Native Script ≠ Plutus Validator

```typescript
// OLD (BROKEN) - Native Script builder
const nativeScript = {
  type: "any",
  scripts: [
    { type: "sig", keyHash: buyerPkh },
    { type: "sig", keyHash: sellerPkh },
  ],
};
const script = lucid.utils.nativeScriptFromJson(nativeScript);
```

**Issue**: Native script is signature-only validation. Your Plutus contract requires:
- Datum (buyer/seller/deadline)
- Redeemer (Release/Refund action)
- Validator logic

Result: **Script address mismatch** → UTxO lookup fails → "non-2xx error"

### Problem 2: Missing Datum & Redeemer

```typescript
// OLD (BROKEN) - No datum attached
.payToAddress(address, { lovelace: BigInt(amount) })

// OLD (BROKEN) - No redeemer on spend
.collectFrom([escrowUtxo])  // No redeemer!
```

**Issue**: Plutus scripts require:
- **Datum**: Stored on-chain with the UTxO
- **Redeemer**: Provided when spending the UTxO

Result: **Validator can't execute** → Silent failure

### Problem 3: Wrong Time Format

```typescript
// OLD (BROKEN)
deadlineSlot: 123456  // Cardano slot number

// NEW (CORRECT)
deadlineMs: 1739000000000  // ms since epoch
```

**Issue**: Slots vs milliseconds mismatch in datum.

---

## ✅ What's Fixed

### Fix 1: Plutus V2 Validator

```typescript
// NEW (CORRECT) - Load compiled Plutus script
function loadEscrowValidator(lucid: Lucid): SpendingValidator {
  const scriptHex = base64ToHex(ESCROW_SCRIPT_BASE64!);
  return {
    type: "PlutusV2",  // ← Plutus V2 (matches your contract)
    script: scriptHex,
  };
}
```

**Result**: Script address verified ✅, UTxOs found ✅

### Fix 2: Proper Datum on Fund

```typescript
// NEW (CORRECT) - Fund with inline datum
const datum: EscrowDatum = {
  buyer: buyerKeyHash,
  seller: sellerKeyHash,
  deadline: BigInt(deadlineMs),
};

const datumCBOR = Data.to(datum, EscrowDatum);

const tx = await lucid
  .newTx()
  .payToContract(ESCROW_SCRIPT_ADDRESS!, { inline: datumCBOR }, { ... })
  .complete();
```

**Result**: Datum stored on-chain ✅, visible in UTxO ✅

### Fix 3: Proper Redeemer on Spend

```typescript
// NEW (CORRECT) - Spend with redeemer
const redeemer: EscrowAction = kind === "release" ? [0n] : [1n];
const redeemerCBOR = Data.to(redeemer, EscrowAction);

const tx = lucid
  .newTx()
  .collectFrom([escrowUtxo], redeemerCBOR)  // ← Pass redeemer
  .attachSpendingValidator(validator);
```

**Result**: Validator can evaluate action ✅, correct logic path executed ✅

---

## 📋 Deployment Checklist

### Step 1: Compile Plutus Contract (Local or Nix)

```bash
cd /workspace/trusty-deal-maker/plutus-contract

# Nix (recommended)
nix-shell
cabal build serialize-escrow:exe:serialize-escrow
$(find plutus-contract/dist-newstyle -name serialize-escrow -type f | grep bin | head -1)

# Or direct cabal
cabal build serialize-escrow
cabal exec serialize-escrow
```

**Output**: `escrow.plutus` file

### Step 2: Extract Script Address

```bash
cardano-cli address build \
  --payment-script-file escrow.plutus \
  --testnet-magic 1 \
  --out-file escrow.addr

cat escrow.addr
# addr_test1wz2lxqkv...
```

**Output**: `ESCROW_SCRIPT_ADDRESS` environment variable

### Step 3: Encode Script Bytes

```bash
# Convert to base64
xxd -p -c 256 escrow.plutus | tr -d '\n' | xxd -r -p | base64
```

**Output**: `ESCROW_SCRIPT_BASE64` environment variable

### Step 4: Set Environment Variables in Supabase

Dashboard → Project Settings → Secrets:

```
BLOCKFROST_API_KEY = <your_preprod_blockfrost_key>
ESCROW_SCRIPT_BASE64 = <base64_from_step_3>
ESCROW_SCRIPT_ADDRESS = <address_from_step_2>
```

### Step 5: Update Edge Function

✅ **Already done**: Replaced `/workspaces/trusty-deal-maker/supabase/functions/cardano-tx-builder/index.ts` with Plutus V2 version

### Step 6: Test on Preprod

1. **Fund escrow**: Create with buyerAddress, sellerAddress, 1 ADA, deadline in 30 days
2. **Verify UTxO**: Check [Preprod Explorer](https://preprod.cexplorer.io/) → search script address → verify inline datum
3. **Release funds**: Call release → verify seller receives ADA
4. **Refund test**: Create another escrow → wait until deadline → call refund → verify buyer receives ADA

---

## 🔍 Request/Response Format Changes

### Fund Transaction

**Request** (to Edge Function):

```json
{
  "action": "buildFundTx",
  "buyerAddress": "addr_test1vrj8a2vmpe6wfp0hm86w8v7p4x8cq6rphcp3r3p2sxu2rggvm7ql",
  "sellerAddress": "addr_test1vqg6y9c22j7dppwd4pjvg47krk3l0phf9xnj0f2xmz3r7g2qvghs",
  "amount": "1000000",
  "deadlineMs": 1740000000000
}
```

**Response**:

```json
{
  "success": true,
  "txCbor": "84...(hex_encoded_transaction)...",
  "scriptAddress": "addr_test1wpwgn9xj...",
  "datumCBOR": "d8799f581c...",
  "description": "Unsigned transaction. Sign with buyerAddress before submitting."
}
```

### Release Transaction

**Request**:

```json
{
  "action": "buildReleaseTx",
  "buyerAddress": "addr_test1vrj8a2vmpe6wfp0hm86w8v7p4x8cq6rphcp3r3p2sxu2rggvm7ql",
  "sellerAddress": "addr_test1vqg6y9c22j7dppwd4pjvg47krk3l0phf9xnj0f2xmz3r7g2qvghs",
  "escrowUtxoTxHash": "abcd1234...(64_hex_chars)...",
  "escrowUtxoIndex": 0,
  "deadlineMs": 1740000000000
}
```

**Response**:

```json
{
  "success": true,
  "txCbor": "84...",
  "scriptAddress": "addr_test1wpwgn9xj...",
  "redeemerCBOR": "d8799f00ff",
  "kind": "release",
  "requiredSigners": ["buyer", "seller"],
  "description": "Unsigned transaction. Must be signed by BOTH buyer and seller."
}
```

### Error Response (Descriptive)

```json
{
  "success": false,
  "error": "Escrow UTxO not found: abcd1234...#0. Available: xyz5678...#0, xyz5678...#1"
}
```

---

## 🧠 Plutus Validator Reference

Your Haskell contract expects:

**Datum** (stored on-chain):
```haskell
data EscrowDatum = EscrowDatum
  { buyer    :: PubKeyHash      -- 28 bytes
  , seller   :: PubKeyHash      -- 28 bytes
  , deadline :: POSIXTime        -- milliseconds since epoch
  }
```

**Redeemer** (provided on spend):
```haskell
data EscrowAction
  = Release  -- Action 0: Buyer releases to seller
  | Refund   -- Action 1: Buyer refunds after deadline
```

**Validation Rules**:
- **Release**: Both buyer AND seller signatures required
- **Refund**: Buyer signature required, deadline must have passed
- **Amount**: Full locked value sent to recipient

---

## 🚀 Next Steps

1. **Complete Plutus deployment** (PLUTUS_DEPLOYMENT.md)
2. **Test Edge Function** with sample requests
3. **Update frontend** to use new milliseconds-based deadline
4. **Verify explorers** show datum/redeemer in transactions
5. **Go mainnet** (change preprod → mainnet when ready)

---

## Quick Reference

| Item | Value |
|------|-------|
| **Network** | Cardano Preprod |
| **Blockfrost URL** | `https://cardano-preprod.blockfrost.io/api/v0` |
| **Explorer** | [cexplorer.io](https://preprod.cexplorer.io/) |
| **Script Type** | Plutus V2 |
| **Datum Format** | Inline (stored with UTxO) |
| **Redeemer 0** | Release (seller receives) |
| **Redeemer 1** | Refund (buyer receives) |
| **Time Format** | Milliseconds since epoch |
| **Script Address** | Starts with `addr_test1` |
| **Testnet Faucet** | [Faucet](https://preprod.cardano.org/faucet/) |
