# Cardano Escrow - Plutus V2 Integration Guide

## Quick Summary of Changes

Your old Edge Function was using **Native Scripts** (signature-only validation). Now it uses **Plutus V2** with:

✅ **Datum** (stored on-chain): buyer address, seller address, deadline  
✅ **Redeemer** (spend action): Release (0) or Refund (1)  
✅ **Proper tx building**: `payToContract` with inline datum, `collectFrom` with redeemer  
✅ **Preprod-ready**: Script address derivation + transaction validation  

---

## Prerequisites

Before integrating, you **must** have:

1. **Compiled `.plutus` file** → Follow [PLUTUS_DEPLOYMENT.md](./PLUTUS_DEPLOYMENT.md)
2. **Script address** (preprod) → From `escrow.addr`
3. **Base64-encoded script bytes** → From `escrow.base64`
4. **Environment variables set in Supabase**:
   ```env
   BLOCKFROST_API_KEY=<your_preprod_key>
   ESCROW_SCRIPT_BASE64=<script_bytes>
   ESCROW_SCRIPT_ADDRESS=<account_name>...
   ```

---

## Frontend Integration

### 1. Update Request Payloads

**OLD** (Native Script - ❌ broken):
```typescript
{
  action: "buildFundTx",
  buyerAddress: "addr_test1...",
  sellerAddress: "addr_test1...",
  amount: "1000000", // lovelace
  deadlineSlot: 123456  // ❌ WRONG: using slots
}
```

**NEW** (Plutus V2 - ✅ correct):
```typescript
{
  action: "buildFundTx",
  buyerAddress: "addr_test1...",
  sellerAddress: "addr_test1...",
  amount: "1000000", // lovelace
  deadlineMs: 1739000000000  // ✅ CORRECT: milliseconds since epoch
}
```

### 2. Create Escrow (Fund)

```typescript
async function createEscrow(
  buyerAddress: string,
  sellerAddress: string,
  amountLovelace: bigint,
  deadlineDate: Date
): Promise<string> {
  const response = await fetch(
    `${SUPABASE_URL}/functions/v1/cardano-tx-builder`,
    {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        Authorization: `Bearer ${accessToken}`,
      },
      body: JSON.stringify({
        action: "buildFundTx",
        buyerAddress,
        sellerAddress,
        amount: amountLovelace.toString(),
        deadlineMs: deadlineDate.getTime(),  // ← milliseconds!
      }),
    }
  );

  if (!response.ok) {
    throw new Error(`Build failed: ${await response.text()}`);
  }

  const { txCbor, datumCBOR, scriptAddress } = await response.json();

  console.log("📝 Datum (on-chain):", datumCBOR);
  console.log("🔐 Script address:", scriptAddress);

  // Sign with buyer's wallet
  const wallet = await window.cardano.nami.enable();
  const lucid = await Lucid.new(
    new Blockfrost(
      "https://cardano-preprod.blockfrost.io/api/v0",
      BLOCKFROST_API_KEY
    ),
    "Preprod"
  );
  lucid.selectWallet.fromApi(wallet);

  const signedTx = await lucid.fromTx(txCbor).sign().complete();
  const txHash = await signedTx.submit();

  console.log("✅ Fund transaction submitted:", txHash);
  return txHash;
}
```

### 3. Release Funds (Seller gets paid)

**Requires BOTH signatures** (buyer + seller):

```typescript
async function releaseFunds(
  buyerAddress: string,
  sellerAddress: string,
  escrowUtxoTxHash: string,
  escrowUtxoIndex: number,
  deadlineDate: Date
): Promise<string> {
  const response = await fetch(
    `${SUPABASE_URL}/functions/v1/cardano-tx-builder`,
    {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        Authorization: `Bearer ${accessToken}`,
      },
      body: JSON.stringify({
        action: "buildReleaseTx",
        buyerAddress,
        sellerAddress,
        escrowUtxoTxHash,
        escrowUtxoIndex,
        deadlineMs: deadlineDate.getTime(),
      }),
    }
  );

  if (!response.ok) {
    throw new Error(`Build failed: ${await response.text()}`);
  }

  const { txCbor, requiredSigners } = await response.json();

  console.log("🔏 Required signatures:", requiredSigners); // ["buyer", "seller"]

  // Sign with BOTH buyer and seller wallets
  const wallet = await window.cardano.nami.enable();
  const lucid = await Lucid.new(
    new Blockfrost(
      "https://cardano-preprod.blockfrost.io/api/v0",
      BLOCKFROST_API_KEY
    ),
    "Preprod"
  );
  lucid.selectWallet.fromApi(wallet);

  // This transaction already has both signers configured
  // But the current wallet can only sign once
  // In production, use multi-sign orchestration (CIP-99 or equivalent)
  
  const signedTx = await lucid.fromTx(txCbor).sign().complete();
  const txHash = await signedTx.submit();

  console.log("✅ Release transaction submitted:", txHash);
  return txHash;
}
```

### 4. Refund (Buyer gets paid after deadline)

**Requires only buyer signature** (time checked on-chain):

```typescript
async function refundEscrow(
  buyerAddress: string,
  sellerAddress: string,
  escrowUtxoTxHash: string,
  escrowUtxoIndex: number,
  deadlineDate: Date
): Promise<string> {
  const response = await fetch(
    `${SUPABASE_URL}/functions/v1/cardano-tx-builder`,
    {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
        Authorization: `Bearer ${accessToken}`,
      },
      body: JSON.stringify({
        action: "buildRefundTx",
        buyerAddress,
        sellerAddress,
        escrowUtxoTxHash,
        escrowUtxoIndex,
        deadlineMs: deadlineDate.getTime(),
      }),
    }
  );

  if (!response.ok) {
    throw new Error(`Build failed: ${await response.text()}`);
  }

  const { txCbor, requiredSigners, validFrom } = await response.json();

  console.log("🔏 Required signature:", requiredSigners); // ["buyer"]
  console.log("⏰ Valid from:", new Date(validFrom).toISOString());

  // Sign with buyer's wallet only
  const wallet = await window.cardano.nami.enable();
  const lucid = await Lucid.new(
    new Blockfrost(
      "https://cardano-preprod.blockfrost.io/api/v0",
      BLOCKFROST_API_KEY
    ),
    "Preprod"
  );
  lucid.selectWallet.fromApi(wallet);

  const signedTx = await lucid.fromTx(txCbor).sign().complete();
  const txHash = await signedTx.submit();

  console.log("✅ Refund transaction submitted:", txHash);
  return txHash;
}
```

---

## Key Differences from Old Implementation

| Aspect | Old (Native) | New (Plutus V2) |
|--------|--------------|-----------------|
| **Script Type** | Native (signatures only) | Plutus V2 (validator script) |
| **Datum** | None | Stored on-chain (buyer/seller/deadline) |
| **Redeemer** | None | Required (0=Release, 1=Refund) |
| **Fund TX** | `payToAddress` | **`payToContract`** with inline datum |
| **Spend TX** | `collectFrom` (no redeemer) | **`collectFrom` + redeemer** |
| **Deadline** | Slot number | Milliseconds since epoch |
| **Validation** | Native script logic | Plutus validator + UTxO matching |
| **Error Messages** | "Non-2xx error" (silent) | Descriptive logs + error details |

---

## Testing Checklist

After deployment:

- [ ] Create escrow with `buildFundTx` → verify UTxO at script address on [Preprod Explorer](https://preprod.cexplorer.io/)
- [ ] Verify datum is stored inline with the UTxO
- [ ] Release funds with `buildReleaseTx` → funds go to seller
- [ ] Refund after deadline with `buildRefundTx` → funds go back to buyer
- [ ] Check transaction logs for descriptive error messages (not silent failures)

---

## Troubleshooting

### "ESCROW_SCRIPT_BASE64 not set"

You haven't set the environment variable. Run the compilation steps in [PLUTUS_DEPLOYMENT.md](./PLUTUS_DEPLOYMENT.md) to get the base64 string.

### "Escrow UTxO not found"

The UTxO is not at the configured script address. Check:
1. Is the script address correct? (Should start with `addr_test1`)
2. Did the fund transaction confirm? (Check Blockfrost)
3. Is the UTxO still there? (Or was it already spent?)

### "Escrow UTxO has no datum attached"

The fund transaction didn't include the inline datum. This shouldn't happen with the new Edge Function, but check:
1. Was the funding transaction built with `payToContract(..., { inline: datum }, ...)`?
2. Did the transaction confirm on-chain?

### "Failed to build release transaction"

The spending validator is rejecting the transaction. Check:
1. Are both buyer and seller signatures present? (Required for release)
2. Is the deadline correct? (Should match the datum)
3. Are the amounts correct? (Seller should receive the full escrow amount)

### "Script validation fails on-chain"

The Plutus validator is rejecting the spend. Check:
1. **Release action**: Both buyer and seller signed? ✅
2. **Refund action**: Deadline passed? ✅ Time is after `deadline` in datum?
3. **Amounts**: Recipient receives full `escrowValue`?
4. **Datum**: Does it match what was locked on-chain?

---

## References

- [Plutus Docs](https://plutus.readthedocs.io/)
- [Lucid Cardano](https://github.com/spacebudz/lucid)
- [Blockfrost Preprod](https://docs.blockfrost.io/)
- [Cardano CLI](https://github.com/input-output-hk/cardano-cli)
- [CIP-30: Wallet Integration](https://cips.cardano.org/cips/cip30/)
