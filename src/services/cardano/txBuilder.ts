 // ============================================================================
 // Transaction Builder Service - Builds real Cardano transactions
 // ============================================================================
 
 import { supabase } from '@/integrations/supabase/client';
 import { blockchainService } from './blockchainService';
 import { getEscrowScriptAddress, getEscrowScriptHash } from './scriptRegistry';
 import { createEscrowDatum, serializeDatum, serializeRedeemer, EscrowAction } from './datumBuilder';
 import { adaToLovelace } from './networkGuard';
 import { UTxO, ProtocolParams } from './types';
 
 /** Transaction build result */
 export interface TxBuildResult {
   success: boolean;
   txCbor?: string;
   txHash?: string;
   error?: string;
 }
 
 /** Escrow transaction parameters */
 export interface EscrowTxParams {
   escrowId: string;
   buyerAddress: string;
   sellerAddress: string;
   amount: bigint; // in lovelace
   deadline: Date;
   action: 'fund' | 'release' | 'refund';
   utxoTxHash?: string;
   utxoOutputIndex?: number;
 }
 
 /**
  * Call the edge function to build a transaction
  */
 async function callTxBuilder(params: Record<string, unknown>): Promise<TxBuildResult> {
   const { data, error } = await supabase.functions.invoke('cardano-tx-builder', {
     body: params,
   });
 
   if (error) {
     console.error('[TxBuilder] Edge function error:', error);
     return { success: false, error: error.message };
   }
 
   return data as TxBuildResult;
 }
 
 /**
  * Build a transaction to fund an escrow
  */
 export async function buildFundEscrowTx(
   buyerAddress: string,
   sellerAddress: string,
   amount: bigint,
   deadline: Date,
   walletUtxos: string[] // CBOR-encoded UTxOs from wallet
 ): Promise<TxBuildResult> {
   const scriptAddress = getEscrowScriptAddress();
   const datum = createEscrowDatum(buyerAddress, sellerAddress, deadline);
   const serializedDatum = serializeDatum(datum);
 
   return callTxBuilder({
     action: 'buildFundTx',
     buyerAddress,
     scriptAddress,
     amount: amount.toString(),
     datum: serializedDatum,
     walletUtxos,
   });
 }
 
 /**
  * Build a transaction to release escrow funds to seller
  */
 export async function buildReleaseEscrowTx(
   buyerAddress: string,
   sellerAddress: string,
   deadline: Date,
   escrowUtxoTxHash: string,
   escrowUtxoIndex: number,
   collateralUtxos: string[] // CBOR-encoded collateral UTxOs
 ): Promise<TxBuildResult> {
   const scriptAddress = getEscrowScriptAddress();
   const scriptHash = getEscrowScriptHash();
   const datum = createEscrowDatum(buyerAddress, sellerAddress, deadline);
   const redeemer = serializeRedeemer(EscrowAction.Release);
 
   return callTxBuilder({
     action: 'buildReleaseTx',
     buyerAddress,
     sellerAddress,
     scriptAddress,
     scriptHash,
     datum: serializeDatum(datum),
     redeemer,
     escrowUtxoTxHash,
     escrowUtxoIndex,
     collateralUtxos,
   });
 }
 
 /**
  * Build a transaction to refund escrow to buyer
  */
 export async function buildRefundEscrowTx(
   buyerAddress: string,
   sellerAddress: string,
   deadline: Date,
   escrowUtxoTxHash: string,
   escrowUtxoIndex: number,
   collateralUtxos: string[]
 ): Promise<TxBuildResult> {
   const scriptAddress = getEscrowScriptAddress();
   const scriptHash = getEscrowScriptHash();
   const datum = createEscrowDatum(buyerAddress, sellerAddress, deadline);
   const redeemer = serializeRedeemer(EscrowAction.Refund);
 
   return callTxBuilder({
     action: 'buildRefundTx',
     buyerAddress,
     scriptAddress,
     scriptHash,
     datum: serializeDatum(datum),
     redeemer,
     escrowUtxoTxHash,
     escrowUtxoIndex,
     collateralUtxos,
   });
 }
 
 /**
  * High-level function to execute escrow release with wallet signing
  */
 export async function executeEscrowRelease(
   walletApi: {
     getUtxos: (amount?: string) => Promise<string[] | null>;
     getCollateral: () => Promise<string[] | null>;
     signTx: (tx: string, partialSign?: boolean) => Promise<string>;
     submitTx: (tx: string) => Promise<string>;
   },
   params: {
     buyerAddress: string;
     sellerAddress: string;
     deadline: Date;
     escrowUtxoTxHash: string;
     escrowUtxoIndex: number;
   }
 ): Promise<{ success: boolean; txHash?: string; error?: string }> {
   try {
     // Get collateral from wallet
     const collateral = await walletApi.getCollateral?.();
     if (!collateral || collateral.length === 0) {
       return { success: false, error: 'No collateral available. Please set collateral in your wallet.' };
     }
 
     // Build the release transaction
     const buildResult = await buildReleaseEscrowTx(
       params.buyerAddress,
       params.sellerAddress,
       params.deadline,
       params.escrowUtxoTxHash,
       params.escrowUtxoIndex,
       collateral
     );
 
     if (!buildResult.success || !buildResult.txCbor) {
       return { success: false, error: buildResult.error || 'Failed to build transaction' };
     }
 
     // Sign with wallet (this triggers the wallet popup)
     const signedTx = await walletApi.signTx(buildResult.txCbor, true);
 
     // Submit to blockchain
     const txHash = await walletApi.submitTx(signedTx);
 
     return { success: true, txHash };
   } catch (error) {
     console.error('[TxBuilder] Release execution error:', error);
     return { 
       success: false, 
       error: error instanceof Error ? error.message : 'Transaction failed' 
     };
   }
 }
 
 /**
  * High-level function to execute escrow refund with wallet signing
  */
 export async function executeEscrowRefund(
   walletApi: {
     getUtxos: (amount?: string) => Promise<string[] | null>;
     getCollateral: () => Promise<string[] | null>;
     signTx: (tx: string, partialSign?: boolean) => Promise<string>;
     submitTx: (tx: string) => Promise<string>;
   },
   params: {
     buyerAddress: string;
     sellerAddress: string;
     deadline: Date;
     escrowUtxoTxHash: string;
     escrowUtxoIndex: number;
   }
 ): Promise<{ success: boolean; txHash?: string; error?: string }> {
   try {
     const collateral = await walletApi.getCollateral?.();
     if (!collateral || collateral.length === 0) {
       return { success: false, error: 'No collateral available. Please set collateral in your wallet.' };
     }
 
     const buildResult = await buildRefundEscrowTx(
       params.buyerAddress,
       params.sellerAddress,
       params.deadline,
       params.escrowUtxoTxHash,
       params.escrowUtxoIndex,
       collateral
     );
 
     if (!buildResult.success || !buildResult.txCbor) {
       return { success: false, error: buildResult.error || 'Failed to build transaction' };
     }
 
     const signedTx = await walletApi.signTx(buildResult.txCbor, true);
     const txHash = await walletApi.submitTx(signedTx);
 
     return { success: true, txHash };
   } catch (error) {
     console.error('[TxBuilder] Refund execution error:', error);
     return { 
       success: false, 
       error: error instanceof Error ? error.message : 'Transaction failed' 
     };
   }
 }
 
 /**
  * High-level function to fund escrow with wallet signing
  */
 export async function executeEscrowFund(
   walletApi: {
     getUtxos: (amount?: string) => Promise<string[] | null>;
     signTx: (tx: string, partialSign?: boolean) => Promise<string>;
     submitTx: (tx: string) => Promise<string>;
   },
   params: {
     buyerAddress: string;
     sellerAddress: string;
     amount: bigint;
     deadline: Date;
   }
 ): Promise<{ success: boolean; txHash?: string; error?: string }> {
   try {
     // Get UTxOs from wallet to fund the escrow
     const walletUtxos = await walletApi.getUtxos();
     if (!walletUtxos || walletUtxos.length === 0) {
       return { success: false, error: 'No UTxOs available in wallet' };
     }
 
     const buildResult = await buildFundEscrowTx(
       params.buyerAddress,
       params.sellerAddress,
       params.amount,
       params.deadline,
       walletUtxos
     );
 
     if (!buildResult.success || !buildResult.txCbor) {
       return { success: false, error: buildResult.error || 'Failed to build transaction' };
     }
 
     const signedTx = await walletApi.signTx(buildResult.txCbor, false);
     const txHash = await walletApi.submitTx(signedTx);
 
     return { success: true, txHash };
   } catch (error) {
     console.error('[TxBuilder] Fund execution error:', error);
     return { 
       success: false, 
       error: error instanceof Error ? error.message : 'Transaction failed' 
     };
   }
 }
 
 export const txBuilderService = {
   buildFundEscrowTx,
   buildReleaseEscrowTx,
   buildRefundEscrowTx,
   executeEscrowFund,
   executeEscrowRelease,
   executeEscrowRefund,
 };