# Trusty Deal Maker - Preprod Escape Plan

## 🔥 You Have Been Given

1. ✅ **New Plutus V2 Edge Function** → `/supabase/functions/cardano-tx-builder/index.ts`
2. ✅ **Compilation Guide** → `PLUTUS_DEPLOYMENT.md`
3. ✅ **Integration Guide** → `PLUTUS_INTEGRATION.md`
4. ✅ **Setup Summary** → `PREPROD_SETUP.md`
5. ✅ **Complete Example** → `ESCROW_EXAMPLE_FLOW.md`

---

## 🏃 Quick Start (Right Now)

### 1. Compile Your Plutus Contract

```bash
cd /workspace/trusty-deal-maker/plutus-contract

# With Nix (recommended - reproducible)
nix-shell
cabal build serialize-escrow
$(find plutus-contract/dist-newstyle -name serialize-escrow -type f | grep bin | head -1)

# Or without Nix (if you have GHC 8.10+)
cabal update
cabal build serialize-escrow
cabal exec serialize-escrow

# Output: escrow.plutus file
```

### 2. Extract Script Address

```bash
cd /workspace/trusty-deal-maker/plutus-contract

cardano-cli address build \
  --payment-script-file escrow.plutus \
  --testnet-magic 1 \
  --out-file escrow.addr

cat escrow.addr > ESCROW_SCRIPT_ADDRESS.txt
```

### 3. Encode Script to Base64

```bash
cd /workspace/trusty-deal-maker/plutus-contract
xxd -p -c 256 escrow.plutus | tr -d '\n' | xxd -r -p | base64 > ESCROW_SCRIPT_BASE64.txt
cat ESCROW_SCRIPT_BASE64.txt
```

### 4. Set Environment Variables in Supabase

Go to: Project Settings → Secrets

Add:
```
BLOCKFROST_API_KEY = <your_preprod_blockfrost_key>
ESCROW_SCRIPT_ADDRESS = <paste_from_step_2>
ESCROW_SCRIPT_BASE64 = <paste_from_step_3>
```

### 5. Deploy Edge Function

```bash
# Already done! Your new Edge Function is at:
# /supabase/functions/cardano-tx-builder/index.ts
# Just push to Supabase:
supabase functions deploy cardano-tx-builder
```

### 6. Test It

```typescript
// Fund escrow
const fundTx = await fetch("...cardano-tx-builder", {
  method: "POST",
  body: JSON.stringify({
    action: "buildFundTx",
    buyerAddress: "addr_test1v...",
    sellerAddress: "addr_test1v...",
    amount: "5000000", // 5 ADA
    deadlineMs: Date.now() + 30 * 24 * 60 * 60 * 1000, // 30 days
  }),
});

// Sign & submit
// ... (see ESCROW_EXAMPLE_FLOW.md for full code)
```

---

## 🎯 What Changed (TL;DR)

| Aspect | Before ❌ | After ✅ |
|--------|-----------|----------|
| Script Type | Native Script | **Plutus V2** |
| Datum | None | Stored on-chain |
| Redeemer | None | Release/Refund action |
| Fund TX | `payToAddress` | **`payToContract` + inline datum** |
| Spend TX | `collectFrom` (no redeemer) | **`collectFrom` + redeemer** |
| Time | Slot number | **Milliseconds** |
| Error Messages | "non-2xx" (silent) | **Descriptive logs** |
| Network | Broken | **Preprod (testnet)** |
| Result | UTxO not found | **Works! Real tx hashes** |

---

## 📚 Documentation Map

| Document | Purpose |
|----------|---------|
| **PLUTUS_DEPLOYMENT.md** | How to compile Haskell → `.plutus` file |
| **PREPROD_SETUP.md** | Network config + environment variables |
| **PLUTUS_INTEGRATION.md** | Frontend integration code + API changes |
| **ESCROW_EXAMPLE_FLOW.md** | Complete real-world example (Alice ↔ Bob) |

Pick one by your goal:
- 🔨 "How do I compile?" → `PLUTUS_DEPLOYMENT.md`
- 🌐 "What's the network setup?" → `PREPROD_SETUP.md`
- 💻 "How do I call the API?" → `PLUTUS_INTEGRATION.md`
- 🎬 "Show me a working example" → `ESCROW_EXAMPLE_FLOW.md`

---

## 🧠 Key Differences You Need to Know

### Old (Broken) - Native Script

```typescript
// No datum
.payToAddress(scriptAddress, { lovelace: amount })

// No redeemer
.collectFrom([utxo])

// Slot-based time
deadlineSlot: 123456
```

### New (Correct) - Plutus V2

```typescript
// ✅ Datum stored on-chain
const datum = { buyer: hash, seller: hash, deadline: timestamp };
.payToContract(scriptAddress, { inline: Data.to(datum, ...) }, { lovelace: amount })

// ✅ Redeemer required on spend
const redeemer = action === "release" ? [0n] : [1n];
.collectFrom([utxo], Data.to(redeemer, ...))

// ✅ Milliseconds-based time
deadlineMs: Date.now() + 30 * 24 * 60 * 60 * 1000
```

---

## ⚠️ Common Pitfalls

### ❌ Forgetting to set environment variables

**Error**: `ESCROW_SCRIPT_BASE64 environment variable not set`

**Fix**: Go to Supabase → Project Settings → Secrets → Add the three variables

### ❌ Using wrong time format

**Error**: Redeemer/datum mismatch

**Fix**: Use **milliseconds** (`Date.now()`), not slots

### ❌ Only one person signing release

**Error**: Script validation fails

**Fix**: **Both buyer AND seller must sign** for release (see ESCROW_EXAMPLE_FLOW.md for orchestration)

### ❌ Trying to refund before deadline

**Error**: Script validation fails (deadline not passed)

**Fix**: Wait until `deadline` timestamp has passed

---

## 🔍 How to Debug

### Check if Edge Function is working

```bash
curl -X POST https://your-project.supabase.co/functions/v1/cardano-tx-builder \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "action": "buildFundTx",
    "buyerAddress": "addr_test1vx...",
    "sellerAddress": "addr_test1vy...",
    "amount": "1000000",
    "deadlineMs": 1740000000000
  }'

# Should return txCbor, not an error about missing env vars
```

### Check if escrow UTxO exists

```bash
# On Blockfrost
curl https://cardano-preprod.blockfrost.io/api/v0/addresses/$ESCROW_SCRIPT_ADDRESS/utxos \
  -H "project_id: $BLOCKFROST_API_KEY"

# Should show your UTxOs with inline datum
```

### Check transaction on explorer

```
https://preprod.cexplorer.io/tx/{txHash}
```

Look for:
- Contract inputs (red flag if none)
- Inline datum (should be present)
- Redeemer (should be present on spend TX)

---

## 🎓 Learning Resources

- **Plutus Docs**: https://plutus.readthedocs.io/
- **Lucid Cardano**: https://github.com/spacebudz/lucid
- **Blockfrost API**: https://docs.blockfrost.io/
- **Cardano Testnet Faucet**: https://preprod.cardano.org/faucet/
- **CIP-30 Wallet Standard**: https://cips.cardano.org/cips/cip30/

---

## ✅ Success Checklist

- [ ] Haskell contract compiled to `escrow.plutus`
- [ ] Script address extracted (`addr_test1...`)
- [ ] Script bytes encoded to base64
- [ ] Environment variables set in Supabase (3 variables)
- [ ] Edge Function deployed
- [ ] Fund transaction built & signed
- [ ] UTxO appears at script address on explorer
- [ ] Inline datum visible in explorer
- [ ] Release transaction built & signed by both parties
- [ ] Funds sent to seller ✅
- [ ] Alternative: Refund after deadline ✅

Once all checked, you're **production-ready on preprod**! 🚀

---

## Still Stuck?

1. **Read the specific document** for your problem (see map above)
2. **Check the example flow** in `ESCROW_EXAMPLE_FLOW.md`
3. **Verify environment setup** matches `PREPROD_SETUP.md`
4. **Check Edge Function logs** in Supabase dashboard
5. **Inspect transactions** on Blockfrost/Preprod Explorer

Good luck! 🎉
