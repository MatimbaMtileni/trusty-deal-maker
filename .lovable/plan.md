

## Problem

The `cardano-tx-builder` edge function fails to deploy due to **7 TypeScript errors** from stale Plutus V2 code (`Data.BigInt`, `Data.Unit`, `localUPLCEval` don't exist in `lucid@0.10.11`). Since it never deploys, calls return "Failed to fetch". Additionally, CORS headers are missing required Supabase client headers.

## Plan

### 1. Rewrite `supabase/functions/cardano-tx-builder/index.ts` — Native Script approach

Replace the entire file. Remove all Plutus V2 constructs (`EscrowDatum`, `EscrowAction`, `Data.BigInt`, `Data.Unit`, `SpendingValidator`, `localUPLCEval`). Use Cardano Native Scripts instead:

- **Fund**: Derive a native script address from buyer PKH + seller PKH + deadline slot, then `payToAddress` the ADA there (no datum needed for native scripts)
- **Release**: Spend from the native script address — requires both buyer + seller signatures
- **Refund**: Spend from the native script address — requires buyer signature + after deadline slot
- Remove `ESCROW_SCRIPT_BASE64` and `ESCROW_SCRIPT_ADDRESS` env var requirements (only `BLOCKFROST_API_KEY` needed)
- Fix CORS headers to include `x-supabase-client-platform, x-supabase-client-platform-version, x-supabase-client-runtime, x-supabase-client-runtime-version`
- Use `nativeUplc: false` instead of `localUPLCEval: false` in `.complete()`

### 2. Fix `supabase/functions/send-wallet-message/lib.ts` build error

The `importBech32` function has a type mismatch. Fix line 44 to destructure properly: `const mod = await import('bech32'); return mod.bech32 ?? mod;`

### 3. Update `src/services/cardano/txBuilder.ts`

Pass `deadlineSlot` (converted from `deadlineMs`) to the edge function so the native script `after` clause works correctly. Add a `dateToSlot` helper using Preprod genesis parameters.

