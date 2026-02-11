# Quick Debug Steps for Release/Refund Errors

## Before Trying Again

1. **Verify you are the buyer** (the one who created the escrow)
   - Check escrow detail page - should say "You" are the Buyer

2. **Verify escrow status is "active"**
   - Status must be: Pending → **Active** → Completed/Refunded
   - Cannot release/refund if already completed

3. **For refund: verify deadline has passed**
   - Check the deadline on escrow detail
   - You can only refund AFTER the deadline expires
   - To release before deadline, use "Release Funds" button instead

4. **Verify script configuration**
   - Open browser DevTools (F12)
   - Run in console:
   ```javascript
   escrowApi.getScriptConfig()
   ```
   - Should show: `{ scriptBase64: "...", scriptAddress: "addr_test1...", isDeployed: true }`

## If Still Getting Error

1. **Check Supabase Logs:**
   - Go to Supabase Dashboard
   - Functions → escrow-transactions → Logs
   - Look for most recent invocation
   - This will show the actual error from the edge function

2. **Common Errors You'll See:**
   - "Only the buyer can release funds to the seller" → You're not the buyer
   - "Escrow is not active" → Already completed/refunded or still pending
   - "Cannot refund before deadline has passed" → Deadline hasn't arrived yet
   - "Both buyer and seller must sign to release funds" → Multi-sig not complete

3. **Try Again After Fixing Issues**

## Network Tab Debugging

1. Open DevTools > Network tab
2. Click Release or Refund button
3. Look for `escrow-transactions` request
4. Check Response tab for error details

---
See FUND_RELEASE_TROUBLESHOOTING.md for comprehensive guide
