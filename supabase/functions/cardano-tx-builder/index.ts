 import { serve } from "https://deno.land/std@0.168.0/http/server.ts";
 
 const corsHeaders = {
   'Access-Control-Allow-Origin': '*',
   'Access-Control-Allow-Headers': 'authorization, x-client-info, apikey, content-type, x-supabase-client-platform, x-supabase-client-platform-version, x-supabase-client-runtime, x-supabase-client-runtime-version',
 };
 
 const BLOCKFROST_URL = 'https://cardano-preprod.blockfrost.io/api/v0';
 
 interface TxBuilderRequest {
   action: 'buildFundTx' | 'buildReleaseTx' | 'buildRefundTx';
   buyerAddress?: string;
   sellerAddress?: string;
   scriptAddress?: string;
   scriptHash?: string;
   amount?: string;
   datum?: string;
   redeemer?: string;
   escrowUtxoTxHash?: string;
   escrowUtxoIndex?: number;
   walletUtxos?: string[];
   collateralUtxos?: string[];
 }
 
 serve(async (req) => {
   if (req.method === 'OPTIONS') {
     return new Response(null, { headers: corsHeaders });
   }
 
   try {
     const BLOCKFROST_API_KEY = Deno.env.get('BLOCKFROST_API_KEY');
     if (!BLOCKFROST_API_KEY) {
       throw new Error('BLOCKFROST_API_KEY is not configured');
     }
 
     const body: TxBuilderRequest = await req.json();
     const { action } = body;
 
     // Get protocol parameters for fee calculation
     const protocolParams = await fetchProtocolParams(BLOCKFROST_API_KEY);
 
     let result;
 
     switch (action) {
       case 'buildFundTx':
         result = await buildFundTransaction(body, protocolParams, BLOCKFROST_API_KEY);
         break;
 
       case 'buildReleaseTx':
         result = await buildScriptSpendTransaction(body, protocolParams, 'release', BLOCKFROST_API_KEY);
         break;
 
       case 'buildRefundTx':
         result = await buildScriptSpendTransaction(body, protocolParams, 'refund', BLOCKFROST_API_KEY);
         break;
 
       default:
         throw new Error(`Unknown action: ${action}`);
     }
 
     return new Response(JSON.stringify(result), {
       status: 200,
       headers: { ...corsHeaders, 'Content-Type': 'application/json' },
     });
   } catch (error: unknown) {
     console.error('Transaction builder error:', error);
     const errorMessage = error instanceof Error ? error.message : 'Unknown error';
     return new Response(
       JSON.stringify({ success: false, error: errorMessage }),
       { status: 500, headers: { ...corsHeaders, 'Content-Type': 'application/json' } }
     );
   }
 });
 
 async function fetchProtocolParams(apiKey: string) {
   const response = await fetch(`${BLOCKFROST_URL}/epochs/latest/parameters`, {
     headers: { 'project_id': apiKey },
   });
 
   if (!response.ok) {
     throw new Error('Failed to fetch protocol parameters');
   }
 
   return response.json();
 }
 
 async function fetchUtxos(address: string, apiKey: string) {
   const response = await fetch(`${BLOCKFROST_URL}/addresses/${address}/utxos`, {
     headers: { 'project_id': apiKey },
   });
 
   if (response.status === 404) {
     return [];
   }
 
   if (!response.ok) {
     throw new Error('Failed to fetch UTxOs');
   }
 
   return response.json();
 }
 
 async function buildFundTransaction(
   params: TxBuilderRequest,
   _protocolParams: Record<string, unknown>,
   apiKey: string
 ) {
   const { buyerAddress, scriptAddress, amount, datum, walletUtxos } = params;
 
   if (!buyerAddress || !scriptAddress || !amount || !datum) {
     throw new Error('Missing required parameters for fund transaction');
   }
 
   // For now, we return a placeholder that indicates the transaction needs to be built
   // In a production system, you would use a proper Cardano library (cardano-serialization-lib)
   // to construct the actual transaction CBOR
   
   // Verify the buyer has enough funds
   const utxos = await fetchUtxos(buyerAddress, apiKey);
   const totalLovelace = utxos.reduce((sum: bigint, utxo: { amount: { unit: string; quantity: string }[] }) => {
     const lovelace = utxo.amount.find((a: { unit: string }) => a.unit === 'lovelace');
     return sum + BigInt(lovelace?.quantity || '0');
   }, 0n);
 
   const requiredAmount = BigInt(amount) + 2_000_000n; // Amount + min ADA for fees
 
   if (totalLovelace < requiredAmount) {
     throw new Error(`Insufficient funds. Have ${totalLovelace}, need ${requiredAmount}`);
   }
 
   // Return transaction metadata for the wallet to build
   // The actual transaction building happens client-side with the wallet
   return {
     success: true,
     txMetadata: {
       type: 'fund',
       from: buyerAddress,
       to: scriptAddress,
       amount,
       datum,
       utxoCount: walletUtxos?.length || 0,
     },
     // In production, return actual txCbor built with cardano-serialization-lib
     message: 'Transaction metadata prepared. Wallet will build and sign the transaction.',
   };
 }
 
 async function buildScriptSpendTransaction(
   params: TxBuilderRequest,
   _protocolParams: Record<string, unknown>,
   txType: 'release' | 'refund',
   apiKey: string
 ) {
   const {
     buyerAddress,
     sellerAddress,
     scriptAddress,
     scriptHash,
     datum,
     redeemer,
     escrowUtxoTxHash,
     escrowUtxoIndex,
     collateralUtxos,
   } = params;
 
   if (!scriptAddress || !scriptHash || !datum || !redeemer) {
     throw new Error('Missing required parameters for script spend');
   }
 
   if (!escrowUtxoTxHash || escrowUtxoIndex === undefined) {
     throw new Error('Missing escrow UTxO reference');
   }
 
   // Fetch the escrow UTxO to verify it exists
   const scriptUtxos = await fetchUtxos(scriptAddress, apiKey);
   const escrowUtxo = scriptUtxos.find(
     (u: { tx_hash: string; output_index: number }) => 
       u.tx_hash === escrowUtxoTxHash && u.output_index === escrowUtxoIndex
   );
 
   if (!escrowUtxo) {
     throw new Error('Escrow UTxO not found at script address. It may have been spent already.');
   }
 
   const escrowAmount = escrowUtxo.amount.find((a: { unit: string }) => a.unit === 'lovelace');
   const lovelaceAmount = BigInt(escrowAmount?.quantity || '0');
 
   // Determine recipient based on transaction type
   const recipient = txType === 'release' ? sellerAddress : buyerAddress;
 
   return {
     success: true,
     txMetadata: {
       type: txType,
       scriptAddress,
       scriptHash,
       escrowUtxo: {
         txHash: escrowUtxoTxHash,
         outputIndex: escrowUtxoIndex,
         amount: lovelaceAmount.toString(),
       },
       recipient,
       datum,
       redeemer,
       collateralCount: collateralUtxos?.length || 0,
     },
     message: `${txType} transaction metadata prepared. Wallet will build and sign.`,
   };
 }