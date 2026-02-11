# Fund Release/Refund Troubleshooting Guide

## Problem: "Edge function returned a non-2xx status code" when releasing or requesting refund

The improved error handling now provides specific error messages. Here are the most common causes:

---

## 1. **You are not the buyer**

**Error:** `"Only the buyer can release funds to the seller"` or `"Only the buyer can request a refund"`

**Cause:** Only the buyer (the address that created the escrow) can release funds or request a refund.

**Solution:**
- Verify you're logged in with the correct wallet/account that created the escrow
- Check EscrowDetail page - you should see "You" next to your address
- Only the Buyer can release funds

---

## 2. **Escrow is not in "active" status**

**Error:** `"Escrow is not active"`

**Cause:** The escrow has already been completed, refunded, or cancelled.

**Solution:**
- Check the escrow status on the detail page
- Possible states: `pending` → `active` → `completed/refunded`
- You can only release/refund **active** escrows
- If already completed/refunded, the transaction cannot be reversed

---

## 3. **Deadline has not passed yet (Refund only)**

**Error:** `"Cannot refund before deadline has passed"`

**Cause:** You're trying to refund before the deadline expires.

**Solution:**
- Check the deadline on the escrow detail page
- Refunds are only available after the deadline
- Wait until the deadline passes, then try again
- If you need immediate resolution, release the funds to the seller

---

## 4. **Multi-sig escrow not fully signed**

**Error:** `"Both buyer and seller must sign to release funds"`

**Cause:** Multi-signature escrows require both buyer and seller approval before release.

**Solution:**
- Check if escrow requires multi-sig (see escrow details)
- Ensure both buyer AND seller have signed
- After both sign, buyer can release the funds
- Look for "Sign Transaction" button if not yet signed

---

## 5. **Transaction failed on blockchain**

**Error:** Generic transaction failure from `cardano-tx-builder`

**Likely Causes:**
- Insufficient funds for transaction fees
- Invalid wallet connection
- Network connectivity issues
- Script address/configuration mismatch

**Solution:**
- Ensure you have enough ADA to cover transaction fees (₳2-5 ADA is typical)
- Reconnect your wallet
- Check browser console for detailed error logs
- Verify Plutus script is properly configured: 
  ```bash
  npm run dev
  # Open DevTools > Console
  # Check: escrowApi.getScriptConfig()
  ```

---

## 6. **Database/Server Error (500)**

**Error:** `"An error occurred. Please try again."` with status 500

**Cause:** Server-side issue in edge function or database.

**Solution:**
- Check Supabase Dashboard → Functions → Logs
- Look for errors in `escrow-transactions` function
- Verify environment variables are set:
  - `BLOCKFROST_API_KEY`
  - `ESCROW_SCRIPT_BASE64`
  - `ESCROW_SCRIPT_ADDRESS`

---

## Debugging Steps

### Step 1: Check Script Configuration

Open browser DevTools (F12) and run:

```typescript
// In browser console
import { escrowApi } from '@/services/escrowApi';

const config = escrowApi.getScriptConfig();
console.log(config);
// Should show: { scriptBase64: "...", scriptAddress: "addr_test1...", isDeployed: true }
```

### Step 2: Check Supabase Function Logs

1. Go to [Supabase Dashboard](https://app.supabase.com)
2. Select your project
3. Go to **Functions** → **escrow-transactions**
4. Click the activity/logs tab at the bottom
5. Look for recent logs (bottom shows newest)
6. Filter by function invocations to see error details

**Key Log Messages to Look For:**
- `[RELEASE] Processing escrow_id:` - Shows if function received request
- `Buyer check: escrow.buyer_user_id=...` - Shows buyer verification
- `Escrow not active, status:` - Shows current status
- `Deadline check:` - Shows deadline validation for refunds
- `Escrow update error:` - Shows database update issues

### Step 3: Verify Escrow Status and Permissions

```sqlite
-- In Supabase SQL Editor, check your escrow:
SELECT 
  id,
  buyer_address,
  seller_address,
  status,
  deadline,
  requires_multi_sig,
  buyer_signed_at,
  seller_signed_at,
  created_at
FROM escrows
WHERE id = 'YOUR_ESCROW_ID'
LIMIT 1;
```

Check:
- ✅ `status` is `'active'`
- ✅ `buyer_address` matches your wallet
- ✅ `deadline` is in the future (for release) or past (for refund)
- ✅ If `requires_multi_sig=true`, both `*_signed_at` fields are set

### Step 4: Check Transaction History

Verify your transaction was actually submitted:

```sqlite
SELECT 
  id,
  tx_type,
  tx_hash,
  from_address,
  to_address,
  amount,
  created_at
FROM escrow_transactions
WHERE escrow_id = 'YOUR_ESCROW_ID'
ORDER BY created_at DESC
LIMIT 10;
```

---

## Network-Specific Issues

### **Preprod (Testnet)**

- Blockfrost API Key must be for Preprod:
  ```
  BLOCKFROST_API_KEY=preprod*
  ```
- Script address must start with `addr_test1`
- Funds are not real ADA

### **Mainnet**

- Script address must start with `addr1`
- This is for real ADA - use with caution!
- RLS policies on `escrows` table must be properly configured

---

## Common Error Codes

| Status | Meaning | Likely Cause |
|--------|---------|--------------|
| 400 | Bad Request | Missing fields, invalid address/tx format, escrow not active, deadline not passed, multi-sig not complete |
| 401 | Unauthorized | Not authenticated with Supabase |
| 403 | Forbidden | Not the buyer, don't have permission |
| 404 | Not Found | Escrow doesn't exist in database |
| 500 | Server Error | Database error, edge function crash, missing environment variables |

---

## Still Having Issues?

1. **Enable detailed logging:**
   - Add `console.log` statements to `src/services/escrowApi.ts`
   - Check Network tab in DevTools for full response payload

2. **Test the edge function directly:**
   ```typescript
   const response = await fetch(
     'https://YOUR-PROJECT.supabase.co/functions/v1/escrow-transactions',
     {
       method: 'POST',
       headers: {
         'Authorization': `Bearer ${your_token}`,
         'Content-Type': 'application/json',
       },
       body: JSON.stringify({
         action: 'release',
         escrow_id: 'your_escrow_id',
         tx_hash: 'your_tx_hash',
       }),
     }
   );
   console.log(await response.json());
   ```

3. **Check Supabase RLS policies:**
   - Go to **Authentication** → **Policies**
   - Verify `escrows` table allows your user to read/update
   - Verify `escrow_transactions` table allows inserts

---

## Related Documentation

- [PLUTUS_SCRIPT_DEPLOYMENT.md](PLUTUS_SCRIPT_DEPLOYMENT.md) - Script configuration
- [DEPLOY_TO_SUPABASE.md](DEPLOY_TO_SUPABASE.md) - Edge function setup
- [ARCHITECTURE_BEFORE_AFTER.md](ARCHITECTURE_BEFORE_AFTER.md) - System architecture
