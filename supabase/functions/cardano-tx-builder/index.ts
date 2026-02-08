import { serve } from "https://deno.land/std@0.168.0/http/server.ts";
import {
  Blockfrost,
  Lucid,
  type Script,
  type UTxO,
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
  amount: string; // lovelace as string
  deadlineSlot: number;
}

interface SpendRequest {
  action: "buildReleaseTx" | "buildRefundTx";
  buyerAddress: string;
  sellerAddress: string;
  escrowUtxoTxHash: string;
  escrowUtxoIndex: number;
  deadlineSlot: number;
}

type TxBuilderRequest = FundRequest | SpendRequest;

// ============================================================================
// Native Script builder
// ============================================================================

/**
 * Build a native script for escrow:
 *  - Release: requires BOTH buyer AND seller signatures (before deadline)
 *  - Refund:  requires buyer signature AND deadline has passed
 *
 * Script structure (any-of):
 *   1. all-of [sig(buyer), sig(seller)]          → release
 *   2. all-of [sig(buyer), after(deadlineSlot)]   → refund
 */
function buildEscrowNativeScript(
  lucid: Lucid,
  buyerAddress: string,
  sellerAddress: string,
  deadlineSlot: number
): { script: Script; scriptAddress: string } {
  const buyerPkh = paymentCredentialOf(lucid, buyerAddress);
  const sellerPkh = paymentCredentialOf(lucid, sellerAddress);

  const nativeScript = {
    type: "any",
    scripts: [
      {
        type: "all",
        scripts: [
          { type: "sig", keyHash: buyerPkh },
          { type: "sig", keyHash: sellerPkh },
        ],
      },
      {
        type: "all",
        scripts: [
          { type: "sig", keyHash: buyerPkh },
          { type: "after", slot: deadlineSlot },
        ],
      },
    ],
  };

  // nativeScriptFromJson returns { type: "Native", script: cborHex }
  const script: Script = lucid.utils.nativeScriptFromJson(nativeScript);
  const scriptAddress = lucid.utils.validatorToAddress(script);

  return { script, scriptAddress };
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
// FUND – buyer sends ADA to the native-script escrow address
// ============================================================================

async function buildFundTx(lucid: Lucid, params: FundRequest) {
  const { buyerAddress, sellerAddress, amount, deadlineSlot } = params;

  // Derive the escrow native-script address deterministically
  const { scriptAddress } = buildEscrowNativeScript(
    lucid,
    buyerAddress,
    sellerAddress,
    deadlineSlot
  );

  // Query buyer UTxOs from Blockfrost (no CIP-30 CBOR decoding needed)
  lucid.selectWalletFrom({ address: buyerAddress });

  const tx = await lucid
    .newTx()
    .payToAddress(scriptAddress, { lovelace: BigInt(amount) })
    .complete();

  return {
    success: true,
    txCbor: tx.toString(),
    scriptAddress,
    scriptOutputIndex: 0,
  };
}

// ============================================================================
// SPEND – collect UTxO from native-script address (release or refund)
// ============================================================================

async function buildSpendTx(
  lucid: Lucid,
  params: SpendRequest,
  txType: "release" | "refund"
) {
  const {
    buyerAddress,
    sellerAddress,
    escrowUtxoTxHash,
    escrowUtxoIndex,
    deadlineSlot,
  } = params;

  const { script, scriptAddress } = buildEscrowNativeScript(
    lucid,
    buyerAddress,
    sellerAddress,
    deadlineSlot
  );

  // Query the escrow UTxO at the script address
  const scriptUtxos = await lucid.utxosAt(scriptAddress);
  const escrowUtxo = scriptUtxos.find(
    (u: UTxO) =>
      u.txHash === escrowUtxoTxHash && u.outputIndex === escrowUtxoIndex
  );

  if (!escrowUtxo) {
    throw new Error(
      "Escrow UTxO not found on-chain. It may have already been spent."
    );
  }

  const recipient = txType === "release" ? sellerAddress : buyerAddress;
  const escrowLovelace = escrowUtxo.assets["lovelace"] || 0n;

  // Use buyer address as the wallet for coin selection (fee inputs)
  lucid.selectWalletFrom({ address: buyerAddress });

  let txBuilder = lucid
    .newTx()
    .collectFrom([escrowUtxo])
    .attachSpendingValidator(script)
    .payToAddress(recipient, { lovelace: escrowLovelace });

  // Validity interval
  if (txType === "refund") {
    // For refund the "after" timelock must be satisfied
    txBuilder = txBuilder.validFrom(Date.now());
  } else {
    // Release requires both signatures, no timelock constraint needed
    // but we add a TTL for safety
    txBuilder = txBuilder.validTo(Date.now() + 600_000); // 10 min TTL
  }

  // Required signers
  txBuilder = txBuilder.addSigner(buyerAddress);
  if (txType === "release") {
    txBuilder = txBuilder.addSigner(sellerAddress);
  }

  const tx = await txBuilder.complete();

  return {
    success: true,
    txCbor: tx.toString(),
    scriptAddress,
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
