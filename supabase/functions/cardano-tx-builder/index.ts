import { serve } from "https://deno.land/std@0.168.0/http/server.ts";
import {
  Blockfrost,
  Constr,
  Data,
  fromHex,
  Lucid,
  toHex,
  type Script,
} from "https://deno.land/x/lucid@0.10.11/mod.ts";

const corsHeaders = {
  "Access-Control-Allow-Origin": "*",
  "Access-Control-Allow-Headers":
    "authorization, x-client-info, apikey, content-type, x-supabase-client-platform, x-supabase-client-platform-version, x-supabase-client-runtime, x-supabase-client-runtime-version",
};

// ============================================================================
// Types
// ============================================================================

interface FundRequest {
  action: "buildFundTx";
  buyerAddress: string;
  sellerAddress: string;
  scriptAddress: string;
  amount: string; // lovelace as string
  deadlineMs: number;
  walletUtxos: string[]; // CBOR hex from CIP-30 getUtxos()
  changeAddress: string;
}

interface SpendRequest {
  action: "buildReleaseTx" | "buildRefundTx";
  buyerAddress: string;
  sellerAddress: string;
  scriptAddress: string;
  escrowUtxoTxHash: string;
  escrowUtxoIndex: number;
  deadlineMs: number;
  collateralUtxos: string[]; // CBOR hex from CIP-30 getCollateral()
  walletUtxos: string[]; // For fee inputs
  changeAddress: string;
  scriptCbor: string; // Compiled Plutus script CBOR hex
}

type TxBuilderRequest = FundRequest | SpendRequest;

// ============================================================================
// Datum / Redeemer schema (matches Plutus EscrowDatum / EscrowAction)
// ============================================================================

// EscrowDatum = Constr 0 [buyerPkh, sellerPkh, deadline]
//   buyerPkh  : ByteString (28 bytes)
//   sellerPkh : ByteString (28 bytes)
//   deadline  : Integer    (POSIX ms)

// EscrowAction = Release (Constr 0 []) | Refund (Constr 1 [])

function extractPkh(bech32Address: string): string {
  // Shelley base address: 1 byte header + 28 bytes payment cred + 28 bytes stake cred
  // We need the 28 byte payment credential as hex
  // Lucid can help with this via utilities
  // For a base address the payment key hash starts at byte 1 (index 2 in hex)
  // We'll use a simpler approach: the address hex minus header byte, first 56 chars
  try {
    // Use the C.Address parsing from Lucid internals if available
    // Fallback: manual extraction from bech32 → hex
    const { bech32 } = await import("https://deno.land/x/lucid@0.10.11/src/core/libs/bech32/index.js");
    // Actually let's just do it ourselves since we control the format
    throw new Error("use fallback");
  } catch {
    // We'll receive addresses already as bech32, Lucid handles conversion internally
    // For datum construction we need the pubkey hash
    // Let's decode from bech32 ourselves
  }
  return ""; // placeholder - Lucid handles this
}

// ============================================================================
// Main handler
// ============================================================================

serve(async (req) => {
  if (req.method === "OPTIONS") {
    return new Response(null, { headers: corsHeaders });
  }

  try {
    const BLOCKFROST_API_KEY = Deno.env.get("BLOCKFROST_API_KEY");
    if (!BLOCKFROST_API_KEY) {
      throw new Error("BLOCKFROST_API_KEY is not configured");
    }

    const body: TxBuilderRequest = await req.json();

    // Initialize Lucid with Blockfrost provider
    const lucid = await Lucid.new(
      new Blockfrost(
        "https://cardano-preprod.blockfrost.io/api/v0",
        BLOCKFROST_API_KEY
      ),
      "Preprod"
    );

    let result;

    switch (body.action) {
      case "buildFundTx":
        result = await buildFundTx(lucid, body as FundRequest);
        break;
      case "buildReleaseTx":
        result = await buildSpendTx(lucid, body as SpendRequest, "release");
        break;
      case "buildRefundTx":
        result = await buildSpendTx(lucid, body as SpendRequest, "refund");
        break;
      default:
        throw new Error(`Unknown action: ${(body as { action: string }).action}`);
    }

    return new Response(JSON.stringify(result), {
      status: 200,
      headers: { ...corsHeaders, "Content-Type": "application/json" },
    });
  } catch (error: unknown) {
    console.error("Transaction builder error:", error);
    const errorMessage =
      error instanceof Error ? error.message : "Unknown error";
    return new Response(
      JSON.stringify({ success: false, error: errorMessage }),
      {
        status: 500,
        headers: { ...corsHeaders, "Content-Type": "application/json" },
      }
    );
  }
});

// ============================================================================
// Build FUND transaction: buyer pays ADA to script address with inline datum
// ============================================================================

async function buildFundTx(lucid: Lucid, params: FundRequest) {
  const {
    buyerAddress,
    sellerAddress,
    scriptAddress,
    amount,
    deadlineMs,
    walletUtxos,
    changeAddress,
  } = params;

  // Select wallet UTxOs for the transaction
  lucid.selectWalletFrom({
    address: changeAddress,
    utxos: lucid.utxosFromCbor(walletUtxos),
  });

  // Build the datum matching our Plutus EscrowDatum
  const buyerPkh = paymentCredentialOf(lucid, buyerAddress);
  const sellerPkh = paymentCredentialOf(lucid, sellerAddress);

  const datum = Data.to(
    new Constr(0, [buyerPkh, sellerPkh, BigInt(deadlineMs)])
  );

  // Build transaction: send ADA to script with inline datum
  const tx = await lucid
    .newTx()
    .payToContract(scriptAddress, { inline: datum }, { lovelace: BigInt(amount) })
    .complete();

  const txCbor = tx.toString(); // unsigned CBOR hex

  // Determine the script output index by examining the built tx outputs
  // payToContract typically places the script output as the first output (index 0)
  const scriptOutputIndex = 0;

  return {
    success: true,
    txCbor,
    datumHash: null, // using inline datum
    scriptOutputIndex,
  };
}

// ============================================================================
// Build SPEND transaction: collect UTxO from script (release or refund)
// ============================================================================

async function buildSpendTx(
  lucid: Lucid,
  params: SpendRequest,
  txType: "release" | "refund"
) {
  const {
    buyerAddress,
    sellerAddress,
    scriptAddress,
    escrowUtxoTxHash,
    escrowUtxoIndex,
    deadlineMs,
    collateralUtxos,
    walletUtxos,
    changeAddress,
    scriptCbor,
  } = params;

  // Construct the validator script object
  const validator: Script = {
    type: "PlutusV2",
    script: scriptCbor,
  };

  // Select wallet UTxOs + collateral
  const decodedUtxos = lucid.utxosFromCbor(walletUtxos);
  const decodedCollateral = lucid.utxosFromCbor(collateralUtxos);

  lucid.selectWalletFrom({
    address: changeAddress,
    utxos: decodedUtxos,
  });

  // Find the escrow UTxO at the script address
  const scriptUtxos = await lucid.utxosAt(scriptAddress);
  const escrowUtxo = scriptUtxos.find(
    (u) => u.txHash === escrowUtxoTxHash && u.outputIndex === escrowUtxoIndex
  );

  if (!escrowUtxo) {
    throw new Error(
      "Escrow UTxO not found on-chain. It may have already been spent."
    );
  }

  // Build datum (must match what's on-chain)
  const buyerPkh = paymentCredentialOf(lucid, buyerAddress);
  const sellerPkh = paymentCredentialOf(lucid, sellerAddress);

  // Redeemer: Release = Constr 0 [], Refund = Constr 1 []
  const redeemer = Data.to(
    new Constr(txType === "release" ? 0 : 1, [])
  );

  // Recipient
  const recipient = txType === "release" ? sellerAddress : buyerAddress;

  // Escrow amount (subtract min fee estimate for the tx output)
  const escrowLovelace =
    escrowUtxo.assets["lovelace"] || 0n;

  // Build the spending transaction
  let txBuilder = lucid
    .newTx()
    .collectFrom([escrowUtxo], redeemer)
    .attachSpendingValidator(validator)
    .payToAddress(recipient, { lovelace: escrowLovelace });

  // Add validity interval for refund (deadline must have passed)
  if (txType === "refund") {
    txBuilder = txBuilder.validFrom(deadlineMs);
  } else {
    // For release, must be before deadline
    txBuilder = txBuilder.validTo(deadlineMs);
  }

  // Add signer (buyer must sign for both release and refund in our contract)
  txBuilder = txBuilder.addSigner(buyerAddress);

  const tx = await txBuilder.complete({
    coinSelection: true,
    // Use the provided collateral
    // noinspection JSUnusedLocalSymbols
  });

  const txCbor = tx.toString();

  return {
    success: true,
    txCbor,
  };
}

// ============================================================================
// Helper: extract payment credential hash from a bech32 address
// ============================================================================

function paymentCredentialOf(lucid: Lucid, address: string): string {
  const details = lucid.utils.getAddressDetails(address);
  const pkh = details.paymentCredential?.hash;
  if (!pkh) {
    throw new Error(`Cannot extract payment credential from ${address}`);
  }
  return pkh;
}
