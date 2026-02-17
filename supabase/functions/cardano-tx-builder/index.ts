import { serve } from "https://deno.land/std@0.168.0/http/server.ts";
import {
  Blockfrost,
  Lucid,
  SpendingValidator,
  Data,
  UTxO,
} from "https://deno.land/x/lucid@0.10.11/mod.ts";

/* ============================================================================
   ENVIRONMENT & CONFIGURATION
============================================================================ */

// Load environment variables (do NOT throw at module init - return structured errors at runtime)
const BLOCKFROST_API_KEY = Deno.env.get("BLOCKFROST_API_KEY");
const ESCROW_SCRIPT_BASE64 = Deno.env.get("ESCROW_SCRIPT_BASE64");
const ESCROW_SCRIPT_ADDRESS = Deno.env.get("ESCROW_SCRIPT_ADDRESS");

// Helper to report missing env vars at request time
function missingEnvVars(): string[] {
  const missing: string[] = [];
  if (!BLOCKFROST_API_KEY) missing.push('BLOCKFROST_API_KEY');
  if (!ESCROW_SCRIPT_BASE64) missing.push('ESCROW_SCRIPT_BASE64');
  if (!ESCROW_SCRIPT_ADDRESS) missing.push('ESCROW_SCRIPT_ADDRESS');
  return missing;
}

const corsHeaders = {
  "Access-Control-Allow-Origin": "*",
  "Access-Control-Allow-Headers":
    "authorization, x-client-info, apikey, content-type",
};

/* ============================================================================
   PLUTUS DATA STRUCTURES (matching EscrowContract.hs)
============================================================================ */

// EscrowDatum = { buyer: PubKeyHash, seller: PubKeyHash, deadline: POSIXTime }
const EscrowDatum = Data.Object({
  buyer: Data.Bytes({ minLength: 28, maxLength: 28 }),
  seller: Data.Bytes({ minLength: 28, maxLength: 28 }),
  deadline: Data.BigInt,
});

type EscrowDatum = Data.Static<typeof EscrowDatum>;

// EscrowAction = Release (0) | Refund (1)
const EscrowAction = Data.Enum([
  Data.Unit, // Release = 0
  Data.Unit, // Refund = 1
]);

type EscrowAction = Data.Static<typeof EscrowAction>;

/* ============================================================================
   TYPES
============================================================================ */

interface FundRequest {
  action: "buildFundTx";
  buyerAddress: string;
  sellerAddress: string;
  amount: string; // lovelace
  deadlineMs: number; // milliseconds since epoch
}

interface SpendRequest {
  action: "buildReleaseTx" | "buildRefundTx";
  buyerAddress: string;
  sellerAddress: string;
  escrowUtxoTxHash: string;
  escrowUtxoIndex: number;
  deadlineMs: number;
}

type TxRequest = FundRequest | SpendRequest;

/* ============================================================================
   HELPERS
============================================================================ */

/**
 * Decode base64 to hex string
 */
function base64ToHex(base64: string): string {
  const bytes = atob(base64);
  let hex = "";
  for (let i = 0; i < bytes.length; i++) {
    const byte = bytes.charCodeAt(i);
    hex += ("0" + byte.toString(16)).slice(-2);
  }
  return hex;
}

/**
 * Get the payment credential (public key hash) from an address
 */
function getPaymentKeyHash(lucid: Lucid, address: string): string {
  const details = lucid.utils.getAddressDetails(address);
  const paymentCred = details.paymentCredential;
  
  if (!paymentCred || !paymentCred.hash) {
    throw new Error(`Invalid payment address: ${address}`);
  }
  
  return paymentCred.hash;
}

/**
 * Initialize Lucid for Preprod
 */
async function initLucid(): Promise<Lucid> {
  const lucid = await Lucid.new(
    new Blockfrost(
      "https://cardano-preprod.blockfrost.io/api/v0",
      BLOCKFROST_API_KEY!
    ),
    "Preprod"
  );
  
  return lucid;
}

/**
 * Load the Escrow spending validator from base64-encoded script
 */
function loadEscrowValidator(lucid: Lucid): SpendingValidator {
  const scriptHex = base64ToHex(ESCROW_SCRIPT_BASE64!);
  
  return {
    type: "PlutusV2",
    script: scriptHex,
  };
}

/* ============================================================================
   HTTP HANDLER
============================================================================ */

serve(async (req) => {
  if (req.method === "OPTIONS") {
    return new Response(null, { headers: corsHeaders });
  }

  // Immediate runtime validation for environment
  const missing = missingEnvVars();
  if (missing.length > 0) {
    console.error('[cardano-tx-builder] Missing env vars:', missing.join(', '));
    return new Response(
      JSON.stringify({ success: false, error: `Missing environment variables: ${missing.join(', ')}` }),
      { status: 200, headers: { ...corsHeaders, 'Content-Type': 'application/json' } }
    );
  }

  try {
    const lucid = await initLucid();
    const validator = loadEscrowValidator(lucid);

    let body: TxRequest | null = null;
    try {
      body = await req.json();
    } catch (e) {
      console.error('[cardano-tx-builder] Invalid JSON body', e);
      return new Response(
        JSON.stringify({ success: false, error: 'Invalid JSON body' }),
        { status: 200, headers: { ...corsHeaders, 'Content-Type': 'application/json' } }
      );
    }

    if (!body || typeof (body as any).action !== 'string') {
      return new Response(
        JSON.stringify({ success: false, error: 'Missing or invalid action' }),
        { status: 200, headers: { ...corsHeaders, 'Content-Type': 'application/json' } }
      );
    }

    let result;

    switch (body.action) {
      case 'buildFundTx':
        result = await buildFundTx(lucid, body as FundRequest);
        break;
      case 'buildReleaseTx':
        result = await buildSpendTx(lucid, validator, body as SpendRequest, 'release');
        break;
      case 'buildRefundTx':
        result = await buildSpendTx(lucid, validator, body as SpendRequest, 'refund');
        break;
      default:
        result = { success: false, error: `Unknown action: ${(body as any).action}` };
    }

    // Always return 200 with structured JSON so supabase.functions.invoke receives the body
    return new Response(JSON.stringify(result), {
      status: 200,
      headers: { ...corsHeaders, 'Content-Type': 'application/json' },
    });
  } catch (err) {
    console.error('[cardano-tx-builder] Unexpected error:', err);
    const errorMessage = err instanceof Error ? err.message : 'Unknown error';

    // Return 200 with structured failure so frontend receives error message directly
    return new Response(
      JSON.stringify({ success: false, error: errorMessage }),
      { status: 200, headers: { ...corsHeaders, 'Content-Type': 'application/json' } }
    );
  }
});

/* ============================================================================
   FUND TX: Buyer → Script Address with Datum
============================================================================ */

async function buildFundTx(lucid: Lucid, params: FundRequest): Promise<any> {
  const {
    buyerAddress,
    sellerAddress,
    amount,
    deadlineMs,
  } = params;

  console.log("[buildFundTx] Creating escrow:", {
    buyer: buyerAddress.slice(0, 20) + "...",
    seller: sellerAddress.slice(0, 20) + "...",
    amount,
    deadline: new Date(deadlineMs).toISOString(),
  });

  // Extract payment key hashes
  const buyerKeyHash = getPaymentKeyHash(lucid, buyerAddress);
  const sellerKeyHash = getPaymentKeyHash(lucid, sellerAddress);

  // Build datum
  const datum: EscrowDatum = {
    buyer: buyerKeyHash,
    seller: sellerKeyHash,
    deadline: BigInt(deadlineMs),
  };

  // Serialize datum using Lucid's Data module
  const datumCBOR = Data.to(datum, EscrowDatum);

  console.log("[buildFundTx] Datum:", {
    buyer: buyerKeyHash,
    seller: sellerKeyHash,
    deadline: deadlineMs,
  });

  // Build transaction: send funds to script address with inline datum
  const tx = await lucid
    .newTx()
    .payToContract(
      ESCROW_SCRIPT_ADDRESS!,
      { inline: datumCBOR },
      { lovelace: BigInt(amount) }
    )
    .complete({ localUPLCEval: false });

  console.log("[buildFundTx] Transaction built successfully");

  return {
    success: true,
    txCbor: tx.toString(),
    scriptAddress: ESCROW_SCRIPT_ADDRESS,
    datumCBOR,
    description: "Unsigned transaction. Sign with buyerAddress before submitting.",
  };
}

/* ============================================================================
   SPEND TX: Release or Refund from Script Address
============================================================================ */

async function buildSpendTx(
  lucid: Lucid,
  validator: SpendingValidator,
  params: SpendRequest,
  kind: "release" | "refund"
): Promise<any> {
  const {
    buyerAddress,
    sellerAddress,
    escrowUtxoTxHash,
    escrowUtxoIndex,
    deadlineMs,
  } = params;

  console.log(`[buildSpendTx:${kind}] Starting:`, {
    scriptAddress: ESCROW_SCRIPT_ADDRESS,
    escrowUtxo: `${escrowUtxoTxHash}#${escrowUtxoIndex}`,
  });

  // Extract payment key hashes (needed for redeemer validation)
  const buyerKeyHash = getPaymentKeyHash(lucid, buyerAddress);
  const sellerKeyHash = getPaymentKeyHash(lucid, sellerAddress);

  // Fetch UTxOs at script address
  let utxos: UTxO[];
  try {
    utxos = await lucid.utxosAt(ESCROW_SCRIPT_ADDRESS!);
    console.log(`[buildSpendTx:${kind}] Found ${utxos.length} UTxO(s) at script`);
  } catch (err) {
    throw new Error(
      `Failed to fetch UTxOs at script address: ${err instanceof Error ? err.message : String(err)}`
    );
  }

  // Find the specific escrow UTxO
  const escrowUtxo = utxos.find(
    (u: UTxO) =>
      u.txHash === escrowUtxoTxHash && u.outputIndex === escrowUtxoIndex
  );

  if (!escrowUtxo) {
    console.error(
      `[buildSpendTx:${kind}] Escrow UTxO not found. Available:`,
      utxos.map((u) => `${u.txHash}#${u.outputIndex}`)
    );
    throw new Error(
      `Escrow UTxO not found: ${escrowUtxoTxHash}#${escrowUtxoIndex}`
    );
  }

  // Verify datum is attached
  if (!escrowUtxo.datum) {
    throw new Error("Escrow UTxO has no datum attached");
  }

  console.log(`[buildSpendTx:${kind}] Found escrow UTxO:`, {
    value: escrowUtxo.assets,
    datum: escrowUtxo.datum,
  });

  // Determine recipient and redeemer
  const recipient = kind === "release" ? sellerAddress : buyerAddress;
  const redeemer: EscrowAction = kind === "release" ? [0n] : [1n]; // 0 = Release, 1 = Refund

  console.log(`[buildSpendTx:${kind}] Building ${kind} redeemer:`, redeemer);

  // Serialize redeemer
  const redeemerCBOR = Data.to(redeemer, EscrowAction);

  // Build transaction
  let tx = lucid
    .newTx()
    .collectFrom([escrowUtxo], redeemerCBOR)  // ← Attach redeemer here
    .attachSpendingValidator(validator);

  // For RELEASE: both buyer and seller must sign
  // For REFUND: only buyer must sign (time check done on-chain)
  if (kind === "release") {
    tx = tx.addSigner(buyerAddress).addSigner(sellerAddress);
    console.log(`[buildSpendTx:release] Added signers: buyer + seller`);
  } else if (kind === "refund") {
    tx = tx.addSigner(buyerAddress);
    console.log(`[buildSpendTx:refund] Added signer: buyer`);
    
    // Set time to after deadline for refund validity check
    tx = tx.validFrom(deadlineMs);
  }

  // Send funds to recipient
  tx = tx.payToAddress(recipient, escrowUtxo.assets);

  // Complete the transaction
  let unsignedTx;
  try {
    unsignedTx = await tx.complete({ localUPLCEval: false });
  } catch (err) {
    throw new Error(
      `Failed to build ${kind} transaction: ${err instanceof Error ? err.message : String(err)}`
    );
  }

  console.log(`[buildSpendTx:${kind}] Transaction built successfully`);

  return {
    success: true,
    txCbor: unsignedTx.toString(),
    scriptAddress: ESCROW_SCRIPT_ADDRESS,
    redeemerCBOR,
    kind,
    requiredSigners: kind === "release" ? ["buyer", "seller"] : ["buyer"],
    validFrom: kind === "refund" ? deadlineMs : undefined,
    description:
      kind === "release"
        ? "Unsigned transaction. Must be signed by BOTH buyer and seller."
        : "Unsigned transaction. Must be signed by buyer. Valid only after deadline.",
  };
}
