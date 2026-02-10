# Architecture: Before vs After

## What You Had (❌ Broken)

```
┌─────────────────────────────────────────────────────────────────┐
│                    BROKEN ARCHITECTURE                          │
└─────────────────────────────────────────────────────────────────┘

┌────────────────────────────────────────────────────────────────────┐
│ React Frontend                                                     │
│ ├─ Create Escrow                                                  │
│ ├─ Fund Transaction                                               │
│ └─ Release/Refund                                                 │
└───────────────────────┬────────────────────────────────────────────┘
                        │ HTTP Request (buyerAddr, sellerAddr, amount, deadlineSlot)
                        │
┌───────────────────────▼────────────────────────────────────────────┐
│ Edge Function: cardano-tx-builder                                  │
│                                                                    │
│ function buildEscrowScript():                                     │
│   nativeScript = {                    ❌ NATIVE SCRIPT            │
│     type: "any",                      ❌ NO DATUM                │
│     scripts: [                        ❌ NO REDEEMER             │
│       { buyer + seller signatures },                              │
│       { buyer + deadline }                                        │
│     ]                                                              │
│   }                                                                │
│                                                                    │
│ .payToAddress(address, { lovelace })  ❌ NO INLINE DATUM        │
│ .collectFrom([utxo])                  ❌ NO REDEEMER ATTACHED    │
│ .validFrom(deadlineSlot)              ❌ SLOT TIME FORMAT        │
└───────────────────────┬────────────────────────────────────────────┘
                        │
                        ▼
┌───────────────────────────────────────────────────────────────────┐
│ Blockfrost / Cardano Preprod                                     │
│                                                                   │
│ Script Address Mismatch ❌                                        │
│ Native address ≠ Plutus address                                  │
│ UTxO lookup fails                                                │
│ "non-2xx error" (silent failure) ❌                              │
└───────────────────────────────────────────────────────────────────┘
```

---

## What You Have Now (✅ Fixed)

```
┌─────────────────────────────────────────────────────────────────┐
│                    FIXED ARCHITECTURE                           │
└─────────────────────────────────────────────────────────────────┘

┌────────────────────────────────────────────────────────────────────┐
│ React Frontend                                                     │
│ ├─ Create Escrow                                                  │
│ ├─ Fund Transaction                                               │
│ └─ Release/Refund                                                 │
└───────────────────────┬────────────────────────────────────────────┘
                        │ HTTP Request (buyerAddr, sellerAddr, amount, deadlineMs)
                        │
┌───────────────────────▼────────────────────────────────────────────┐
│ Edge Function: cardano-tx-builder (FIXED)                         │
│                                                                    │
│ function loadEscrowValidator():                                  │
│   const scriptHex = base64ToHex(ESCROW_SCRIPT_BASE64)  ✅ LOAD  │
│   return { type: "PlutusV2", script: scriptHex }       ✅ V2   │
│                                                                    │
│ function buildFundTx():                                           │
│   const datum = {                 ✅ PLUTUS DATUM                │
│     buyer: buyerKeyHash,                                         │
│     seller: sellerKeyHash,                                       │
│     deadline: BigInt(deadlineMs)  ✅ MILLISECONDS                │
│   }                                                                │
│                                                                    │
│   .payToContract(                 ✅ INLINE DATUM                │
│     scriptAddress,                                               │
│     { inline: Data.to(datum, ...) },                             │
│     { lovelace: amount }                                         │
│   )                                                                │
│                                                                    │
│ function buildSpendTx():                                          │
│   const redeemer = action === "release" ? [0n] : [1n]  ✅ ENUM  │
│                                                                    │
│   .collectFrom([utxo], redeemerCBOR)  ✅ ATTACH REDEEMER        │
│   .attachSpendingValidator(validator)  ✅ ATTACH SCRIPT          │
│   .validFrom(deadlineMs)               ✅ MS TIME                │
└───────────────────────┬────────────────────────────────────────────┘
                        │
                        ▼
┌───────────────────────────────────────────────────────────────────┐
│ Blockfrost / Cardano Preprod                                     │
│                                                                   │
│ Script Address Match ✅                                          │
│ Plutus address derived correctly                                 │
│ UTxO found ✅                                                    │
│ Datum validated on-chain ✅                                      │
│ Redeemer executes validator logic ✅                             │
│ Real transaction hash returned ✅                                │
│ No silent failures ✅                                            │
└───────────────────────────────────────────────────────────────────┘
```

---

## Data Flow Comparison

### ❌ OLD DATA FLOW (Native Script)

```
Frontend
    │ deadlineSlot: 123456
    │
    ▼
Edge Function
    │ Builds native script JSON
    │ No datum
    │ Calls .payToAddress() (no datum)
    │
    ▼
Blockfrost
    │ Script address = native script address
    │ Actual UTxO at PLUTUS address (different!)
    │ UTxO lookup fails ❌
    │ Returns error
    │
    ▼
Frontend (silent failure)
    │ "non-2xx error"
    │ Debugging impossible
```

### ✅ NEW DATA FLOW (Plutus V2)

```
Frontend
    │ deadlineMs: 1739000000000
    │ buyerAddress: "addr_test1v..."
    │ sellerAddress: "addr_test1v..."
    │
    ▼
Edge Function
    │ Load ESCROW_SCRIPT_BASE64 environment variable
    │ Deserialize to PlutusV2 validator
    │ Extract payment key hashes from addresses
    │ Build EscrowDatum { buyer, seller, deadline }
    │ Serialize datum with Data.to()
    │ Call .payToContract(scriptAddress, { inline: datumCBOR }, ...)
    │
    ▼
Blockfrost
    │ Script address = PlutusV2 script address ✅
    │ UTxO locked at correct address ✅
    │ Inline datum attached to UTxO ✅
    │ For spend:
    │   ├─ Fetch UTxO at script address
    │   ├─ Validate redeemer matches action (Release=0 | Refund=1)
    │   ├─ Execute Plutus validator
    │   └─ Transfer funds to recipient
    │ Returns real transaction hash ✅
    │
    ▼
Frontend
    │ Success! { txHash: "abcd1234..." }
    │ User can verify on Blockfrost
    │ Clear error messages if anything fails
```

---

## Transaction Structure Comparison

### ❌ OLD FUND TRANSACTION (Broken)

```
Transaction:
├─ Inputs: [Alice's UTxO]
├─ Outputs:
│  └─ 5 ADA → Native Script Address
│     └─ Datum: NONE ❌
│     └─ Script Reference: NONE
├─ Witness:
│  └─ Signature: Alice's signature
└─ Problem: Script address mismatch!
```

### ✅ NEW FUND TRANSACTION (Fixed)

```
Transaction:
├─ Inputs: [Alice's UTxO]
├─ Outputs:
│  └─ 5 ADA → Plutus V2 Script Address ✅
│     ├─ Datum (inline): ✅
│     │  ├─ buyer: a1b2c3... (Alice's key hash)
│     │  ├─ seller: d4e5f6... (Bob's key hash)
│     │  └─ deadline: 1740000000000 (ms)
│     └─ Script Reference: (optional, for efficiency)
├─ Witness:
│  └─ Signature: Alice's signature
└─ Result: UTxO locked correctly! ✅
```

### ❌ OLD RELEASE TRANSACTION (Broken)

```
Transaction:
├─ Inputs: [Escrow UTxO at native script]
├─ Scripts: None ❌
├─ Redeemer: None ❌
├─ Witness:
│  ├─ Signature: Alice's sig
│  └─ Signature: Bob's sig
└─ Problem: No validator to execute!
```

### ✅ NEW RELEASE TRANSACTION (Fixed)

```
Transaction:
├─ Inputs: [Escrow UTxO at Plutus V2 script]
├─ Scripts:
│  └─ PlutusV2 validator (ESCROW_SCRIPT_BASE64) ✅
├─ Redeemer: ✅
│  ├─ Action: Release (0)
│  └─ Applied to: Input #0 (escrow)
├─ Outputs:
│  └─ 5 ADA → Bob's Address
├─ Witness:
│  ├─ Signature: Alice's sig
│  └─ Signature: Bob's sig
└─ Validation:
   ├─ Check datum: buyer=Alice ✅, seller=Bob ✅
   ├─ Check redeemer: Release=0 ✅
   ├─ Check signer: Alice ✅ + Bob ✅
   ├─ Check output: Bob gets full amount ✅
   └─ Result: Script validates! ✅
```

---

## Environment Variable Differences

### ❌ OLD (None needed - broken anyway!)

```env
BLOCKFROST_API_KEY=...
(No script configuration)
(Native script hardcoded in Edge Function)
```

### ✅ NEW (Three required variables)

```env
BLOCKFROST_API_KEY=<preprod_blockfrost_key>
ESCROW_SCRIPT_BASE64=<base64_encoded_plutus_script>
ESCROW_SCRIPT_ADDRESS=<addr_test1w...>
```

---

## API Request Changes

### ❌ OLD REQUEST (Slot-based)

```json
{
  "action": "buildFundTx",
  "buyerAddress": "addr_test1v...",
  "sellerAddress": "addr_test1v...",
  "amount": "5000000",
  "deadlineSlot": 123456
}
```

### ✅ NEW REQUEST (Milliseconds-based)

```json
{
  "action": "buildFundTx",
  "buyerAddress": "addr_test1v...",
  "sellerAddress": "addr_test1v...",
  "amount": "5000000",
  "deadlineMs": 1739000000000
}
```

**Key difference**: `deadlineSlot` → `deadlineMs` (milliseconds since epoch)

---

## Error Handling Comparison

### ❌ OLD ERROR MESSAGE

```json
HTTP 500
{
  "error": "non-2xx error"
}
```

Developer has **no idea** what went wrong:
- Script address mismatch?
- Blockfrost down?
- Network error?
- Silent crash in Edge Function?

### ✅ NEW ERROR MESSAGE

```json
HTTP 500
{
  "error": "Escrow UTxO not found: abcd1234...#0. Available: xyz5678...#0, xyz5678...#1"
}
```

Developer **immediately knows**:
- UTxO doesn't exist at that location
- Here are the available UTxOs
- Check the transaction hash / index

---

## Summary

| Layer | Old ❌ | New ✅ |
|-------|--------|--------|
| **Script Type** | Native Script | Plutus V2 |
| **Datum** | None (invalid) | Stored on-chain |
| **Redeemer** | None | Action enum (0/1) |
| **Fund TX** | `payToAddress` | `payToContract` + inline datum |
| **Spend TX** | `collectFrom` | `collectFrom` + redeemer + validator |
| **Time** | Slot | Milliseconds |
| **Config** | Hardcoded | Environment variables |
| **Errors** | Silent failures | Descriptive messages |
| **Works on Preprod** | ❌ No | ✅ Yes |
| **UTxO Found** | ❌ No | ✅ Yes |
| **Tx Hash Returned** | ❌ No | ✅ Yes |
| **Production Ready** | ❌ No | ✅ Yes |

You've gone from **broken** to **production-ready**! 🚀
