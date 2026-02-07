// ============================================================================
// Transaction Builder Service – real Cardano transactions via Lucid edge fn
// ============================================================================

import { supabase } from '@/integrations/supabase/client';
import { getEscrowScriptAddress } from './scriptRegistry';

/** Result from the edge-function tx builder */
export interface TxBuildResult {
  success: boolean;
  txCbor?: string;
  error?: string;
}

/** Parameters for any escrow tx */
export interface EscrowTxParams {
  escrowId: string;
  buyerAddress: string;
  sellerAddress: string;
  amount: bigint; // lovelace
  deadline: Date;
  action: 'fund' | 'release' | 'refund';
  utxoTxHash?: string;
  utxoOutputIndex?: number;
}

// ============================================================================
// Low-level: call the edge function
// ============================================================================

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

// ============================================================================
// CIP-30 wallet shape (subset we need)
// ============================================================================

interface WalletApi {
  getUtxos: (amount?: string) => Promise<string[] | null>;
  getCollateral?: () => Promise<string[] | null>;
  getChangeAddress: () => Promise<string>;
  signTx: (tx: string, partialSign?: boolean) => Promise<string>;
  submitTx: (tx: string) => Promise<string>;
}

// ============================================================================
// FUND – buyer sends ADA to the escrow script
// ============================================================================

export async function executeEscrowFund(
  walletApi: WalletApi,
  params: {
    buyerAddress: string;
    sellerAddress: string;
    amount: bigint;
    deadline: Date;
  }
): Promise<{ success: boolean; txHash?: string; error?: string }> {
  try {
    const walletUtxos = await walletApi.getUtxos();
    if (!walletUtxos?.length) {
      return { success: false, error: 'No UTxOs available in wallet' };
    }

    const changeAddress = await walletApi.getChangeAddress();
    const scriptAddress = getEscrowScriptAddress();

    if (!scriptAddress) {
      return { success: false, error: 'Escrow script not deployed on this network' };
    }

    const buildResult = await callTxBuilder({
      action: 'buildFundTx',
      buyerAddress: params.buyerAddress,
      sellerAddress: params.sellerAddress,
      scriptAddress,
      amount: params.amount.toString(),
      deadlineMs: params.deadline.getTime(),
      walletUtxos,
      changeAddress,
    });

    if (!buildResult.success || !buildResult.txCbor) {
      return { success: false, error: buildResult.error || 'Failed to build transaction' };
    }

    // Wallet signs and submits
    const signedTx = await walletApi.signTx(buildResult.txCbor, false);
    const txHash = await walletApi.submitTx(signedTx);

    return { success: true, txHash };
  } catch (error) {
    console.error('[TxBuilder] Fund error:', error);
    return { success: false, error: errorMsg(error) };
  }
}

// ============================================================================
// RELEASE – buyer authorizes release of funds to seller
// ============================================================================

export async function executeEscrowRelease(
  walletApi: WalletApi,
  params: {
    buyerAddress: string;
    sellerAddress: string;
    deadline: Date;
    escrowUtxoTxHash: string;
    escrowUtxoIndex: number;
    scriptCbor: string;
  }
): Promise<{ success: boolean; txHash?: string; error?: string }> {
  return executeSpend(walletApi, params, 'buildReleaseTx');
}

// ============================================================================
// REFUND – buyer reclaims funds after deadline
// ============================================================================

export async function executeEscrowRefund(
  walletApi: WalletApi,
  params: {
    buyerAddress: string;
    sellerAddress: string;
    deadline: Date;
    escrowUtxoTxHash: string;
    escrowUtxoIndex: number;
    scriptCbor: string;
  }
): Promise<{ success: boolean; txHash?: string; error?: string }> {
  return executeSpend(walletApi, params, 'buildRefundTx');
}

// ============================================================================
// Shared spend logic (release / refund)
// ============================================================================

async function executeSpend(
  walletApi: WalletApi,
  params: {
    buyerAddress: string;
    sellerAddress: string;
    deadline: Date;
    escrowUtxoTxHash: string;
    escrowUtxoIndex: number;
    scriptCbor: string;
  },
  action: 'buildReleaseTx' | 'buildRefundTx'
): Promise<{ success: boolean; txHash?: string; error?: string }> {
  try {
    const collateral = await walletApi.getCollateral?.();
    if (!collateral?.length) {
      return { success: false, error: 'No collateral set. Please configure collateral in your wallet.' };
    }

    const walletUtxos = await walletApi.getUtxos();
    if (!walletUtxos?.length) {
      return { success: false, error: 'No UTxOs available in wallet' };
    }

    const changeAddress = await walletApi.getChangeAddress();
    const scriptAddress = getEscrowScriptAddress();

    if (!scriptAddress) {
      return { success: false, error: 'Escrow script not deployed on this network' };
    }

    const buildResult = await callTxBuilder({
      action,
      buyerAddress: params.buyerAddress,
      sellerAddress: params.sellerAddress,
      scriptAddress,
      escrowUtxoTxHash: params.escrowUtxoTxHash,
      escrowUtxoIndex: params.escrowUtxoIndex,
      deadlineMs: params.deadline.getTime(),
      collateralUtxos: collateral,
      walletUtxos,
      changeAddress,
      scriptCbor: params.scriptCbor,
    });

    if (!buildResult.success || !buildResult.txCbor) {
      return { success: false, error: buildResult.error || 'Failed to build transaction' };
    }

    // Partial sign = true because the script also signs
    const signedTx = await walletApi.signTx(buildResult.txCbor, true);
    const txHash = await walletApi.submitTx(signedTx);

    return { success: true, txHash };
  } catch (error) {
    console.error('[TxBuilder] Spend error:', error);
    return { success: false, error: errorMsg(error) };
  }
}

// ============================================================================
// Helpers
// ============================================================================

function errorMsg(e: unknown): string {
  return e instanceof Error ? e.message : 'Transaction failed';
}

export const txBuilderService = {
  executeEscrowFund,
  executeEscrowRelease,
  executeEscrowRefund,
};
