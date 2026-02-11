# Plutus Script Deployment Guide

## Overview

This guide details how to integrate your compiled Plutus V2 escrow validator script into the Trusty Deal Maker application.

## Values Provided

You have received:

```
ESCROW_SCRIPT_BASE64=TQEAADMiIiAFEgASABFYIEprYbk94qYpQcoLSPegJ13MLCcmSTi/D/U4qY5c/AE/
ESCROW_SCRIPT_ADDRESS=addr_test1wzk86vux4278453f65675865675865675865675865675865675865675
```

## Local Development Setup

### 1. Update Environment Variables

The values have already been added to `.env.local`:

```bash
# View current configuration
cat .env.local | grep -E "ESCROW_SCRIPT|BLOCKFROST"
```

**Note:** `.env.local` is in `.gitignore` and should never be committed.

### 2. Verify Script Loading

Frontend services will automatically load these from the environment:

```typescript
// In any component
import { getScriptAddress, isScriptDeployed } from '@/services/lucidService';

const scriptAddr = getScriptAddress();
const ready = isScriptDeployed();
```

## Supabase Deployment

### 1. Set Edge Function Environment Variables

Navigate to your Supabase project dashboard:

1. Go to **Settings** > **Environment Variables**
2. Add these secrets (NOT the one with VITE_ prefix):

   | Key | Value |
   |-----|-------|
   | `BLOCKFROST_API_KEY` | `preprodYjRkHfcazNkL0xxG9C2RdUbUoTrG7wip` |
   | `ESCROW_SCRIPT_BASE64` | `TQEAADMiIiAFEgASABFYIEprYbk94qYpQcoLSPegJ13MLCcmSTi/D/U4qY5c/AE/` |
   | `ESCROW_SCRIPT_ADDRESS` | `addr_test1wzk86vux4278453f65675865675865675865675865675865675865675` |

3. Click **Save**

### 2. Verify in Functions

The `cardano-tx-builder` and `escrow-transactions` edge functions will automatically use these values:

```typescript
// In Supabase edge functions
const ESCROW_SCRIPT_BASE64 = Deno.env.get("ESCROW_SCRIPT_BASE64");
const ESCROW_SCRIPT_ADDRESS = Deno.env.get("ESCROW_SCRIPT_ADDRESS");
```

### 3. Deploy Edge Functions

Deploy updated functions:

```bash
# If using Supabase CLI
supabase functions deploy cardano-tx-builder --project-id jqdrthsjqckptwbalpuj
supabase functions deploy escrow-transactions --project-id jqdrthsjqckptwbalpuj
```

Or manually trigger redeploy from Supabase dashboard.

## Testing

### Frontend Verification

```typescript
// In browser console or component
import { escrowApi } from '@/services/escrowApi';

// Check script configuration
const config = escrowApi.getScriptConfig();
console.log(config);
// Output: { scriptBase64: "...", scriptAddress: "addr_test1...", isDeployed: true }
```

### Create Test Escrow

```typescript
import { escrowApi } from '@/services/escrowApi';

const result = await escrowApi.createEscrow({
  buyer_address: 'addr_test1...',
  seller_address: 'addr_test1...',
  amount: 5_000_000, // 5 ADA in lovelace
  deadline: new Date(Date.now() + 7 * 24 * 60 * 60 * 1000).toISOString(),
  description: 'Test escrow',
  tx_hash: 'mock-tx-hash-for-test',
});

console.log(result);
```

## Script Registry

The application uses a centralized script registry at [src/services/cardano/scriptRegistry.ts](src/services/cardano/scriptRegistry.ts):

- **Loads** script config from environment
- **Validates** script deployment status
- **Exports** helper functions:
  - `isScriptDeployed()` - Check if Plutus script is ready
  - `getScriptBase64()` - Get script for Lucid
  - `getScriptAddress()` - Get on-chain script address
  - `getScriptVerificationStatus()` - Full status object

## Architecture

```
┌─────────────────────────────────────────────┐
│         Frontend (React + TypeScript)       │
│  - lucidService.ts (script exports)         │
│  - escrowApi.ts (with validation)           │
└─────────────┬───────────────────────────────┘
              │
    ┌─────────▼──────────┐
    │ scriptRegistry.ts   │ ◄── Loads from env.local/env
    │ (Plutus Config)     │
    └────────────────────┘
              │
              │ Passes to
              ▼
┌─────────────────────────────────────────────────┐
│     Supabase Edge Functions                     │
│  - cardano-tx-builder/index.ts                  │
│  - escrow-transactions/index.ts                 │
│  Reads: BLOCKFROST_API_KEY, ESCROW_SCRIPT_*    │
└─────────────────────────────────────────────────┘
              │
              │ Integrates with
              ▼
┌─────────────────────────────────────────────────┐
│     Cardano Blockchain (Preprod)                │
│  - Plutus validator at ESCROW_SCRIPT_ADDRESS    │
│  - Blockfrost for UTxO queries                  │
└─────────────────────────────────────────────────┘
```

## Troubleshooting

### "Plutus escrow script not configured" Error

**Reason:** Environment variables not loaded

**Solution:**
1. Verify `.env.local` has both `VITE_` prefixed values
2. Restart dev server: `npm run dev`
3. Check browser DevTools > Network > Inspect requests

### Supabase Functions Show "ESCROW_SCRIPT_BASE64 not set"

**Reason:** Environment variables not set in Supabase project

**Solution:**
1. Go to Supabase Dashboard > Settings > Environment Variables
2. Add the three secrets listed above
3. Redeploy functions
4. Wait 1-2 minutes for environment to update

### Address Validation Fails in Cardano Transactions

**Reason:** Script address may be invalid or incorrect network

**Solution:**
1. Verify address starts with `addr_test1` (Preprod)
2. Check address length (57+ characters after `addr_test1`)
3. Regenerate using: `cardano-cli address build --payment-script-file escrow.plutus --testnet-magic 1 --out-file escrow.addr`

## Files Modified

- [.env.local](.env.local) - Added script environment variables
- [src/services/cardano/scriptRegistry.ts](src/services/cardano/scriptRegistry.ts) - Updated to load Plutus config
- [src/services/lucidService.ts](src/services/lucidService.ts) - Added script exports
- [src/services/escrowApi.ts](src/services/escrowApi.ts) - Added script verification
- [supabase/config.toml](supabase/config.toml) - Documented required env vars

## Reference

- **Plutus Version:** V2
- **Network:** Cardano Preprod (testnet)
- **Blockfrost API:** https://cardano-preprod.blockfrost.io/api/v0
- **Script Type:** Spending validator for escrow
