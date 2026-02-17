#!/usr/bin/env node
// E2E helper to test the deployed `send-wallet-message` Edge Function.
// Usage:
//  FUNCTION_URL=https://<project>.supabase.co/functions/v1/send-wallet-message \
//  ESCROW_ID=<existing-escrow-uuid> \
//  PUBLISHABLE_KEY=<project-publishable-key> \
//  node scripts/e2e-send-wallet-message.js

import { randomPrivateKey, getPublicKey, sign } from '@noble/ed25519';
import { blake2b } from '@noble/hashes/blake2b';
import { bech32 } from 'bech32';

const FUNCTION_URL = process.env.FUNCTION_URL;
const ESCROW_ID = process.env.ESCROW_ID;
const PUBLISHABLE_KEY = process.env.PUBLISHABLE_KEY || process.env.VITE_SUPABASE_PUBLISHABLE_KEY;

if (!FUNCTION_URL) {
  console.error('ERROR: FUNCTION_URL is required. Example: https://<ref>.supabase.co/functions/v1/send-wallet-message');
  process.exit(1);
}
if (!ESCROW_ID) {
  console.error('ERROR: ESCROW_ID is required (must exist in your Supabase DB).');
  process.exit(1);
}

(async () => {
  // Generate key pair
  const priv = randomPrivateKey();
  const pub = await getPublicKey(priv);
  const pubHex = Buffer.from(pub).toString('hex');

  // Build a test Cardano-style address that contains payment key hash (blake2b-224)
  const pkHash = blake2b(pub, { dkLen: 28 });
  // header byte 0x01 (example), append payment hash and a trailing checksum byte (0x00)
  const addrBytes = new Uint8Array([0x01, ...pkHash, 0x00]);
  const addrWords = bech32.toWords(addrBytes);
  const senderAddress = bech32.encode('addr_test', addrWords, 1023);

  const message = `Hello from E2E test @ ${new Date().toISOString()}`;
  const payloadObj = { escrow_id: ESCROW_ID, content: message, ts: Date.now(), address: senderAddress };
  const payload = JSON.stringify(payloadObj);

  // Sign payload
  const sig = await sign(new TextEncoder().encode(payload), priv);
  const sigHex = Buffer.from(sig).toString('hex');

  console.log('Generated test keypair and address (for this E2E run):');
  console.log('  sender_address:', senderAddress);
  console.log('  public_key:', pubHex);
  console.log('  signature (hex):', sigHex.substring(0, 16) + '...');
  console.log('Calling function with payload:', payloadObj);

  const resp = await fetch(FUNCTION_URL, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      ...(PUBLISHABLE_KEY ? { apikey: PUBLISHABLE_KEY } : {}),
    },
    body: JSON.stringify({ escrow_id: ESCROW_ID, content: message, sender_address: senderAddress, payload, signature: sigHex, key: pubHex }),
  });

  const text = await resp.text();
  console.log('Function response status:', resp.status);
  try {
    console.log('Function response JSON:', JSON.parse(text));
  } catch (err) {
    console.log('Function response text:', text);
  }

  if (!resp.ok) process.exit(2);
})();
