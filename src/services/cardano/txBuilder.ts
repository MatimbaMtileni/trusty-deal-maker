// ============================================================================
// Transaction Builder Service – Plutus V2 escrow via edge function
// ============================================================================

import { supabase } from '@/integrations/supabase/client';

/** Result from the edge-function tx builder */
export interface TxBuildResult {
  success: boolean;
  txCbor?: string;
  error?: string;
  scriptAddress?: string;
  scriptOutputIndex?: number;
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
// Low-level: call the edge function
// ============================================================================

async function callTxBuilder(params: Record<string, unknown>): Promise<TxBuildResult> {
  const { data, error } = await supabase.functions.invoke('cardano-tx-builder', {
    body: params,
  });

  if (error) {
    console.error('[TxBuilder] Edge function error:', error);
    // Try to provide a more helpful message to the UI
    const parts: string[] = [];
    if (error.message) parts.push(error.message);
    // supabase error may contain details/status
    // include them when available for easier debugging
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const anyErr = error as any;
    if (anyErr.details) parts.push(String(anyErr.details));
    if (anyErr.status) parts.push(`status=${anyErr.status}`);

    return { success: false, error: parts.filter(Boolean).join(' — ') || 'Edge function error' };
  }

  return data as TxBuildResult;
}

// ============================================================================
// Deadline conversion helper
// ============================================================================

/**
 * Convert a JS Date to milliseconds since epoch (POSIX time).
 * This matches the new Plutus V2 Edge Function format.
 */
function dateToMilliseconds(date: Date): number {
  return date.getTime();
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
): Promise<{ success: boolean; txHash?: string; outputIndex?: number; scriptAddress?: string; error?: string }> {
  try {
    const deadlineMs = dateToMilliseconds(params.deadline);

    const buildResult = await callTxBuilder({
      action: 'buildFundTx',
      buyerAddress: params.buyerAddress,
      sellerAddress: params.sellerAddress,
      amount: params.amount.toString(),
      deadlineMs,
    });

    if (!buildResult.success || !buildResult.txCbor) {
      return { success: false, error: buildResult.error || 'Failed to build transaction' };
    }

    // Wallet signs and submits
    const signedTx = await walletApi.signTx(buildResult.txCbor, false);
    const txHash = await walletApi.submitTx(signedTx);

    return {
      success: true,
      txHash,
      outputIndex: buildResult.scriptOutputIndex ?? 0,
      scriptAddress: buildResult.scriptAddress,
    };
  } catch (error) {
    console.error('[TxBuilder] Fund error:', error);
    return { success: false, error: errorMsg(error) };
  }
}

// ============================================================================
// RELEASE – buyer + seller authorize release of funds to seller
// ============================================================================

export async function executeEscrowRelease(
  walletApi: WalletApi,
  params: {
    buyerAddress: string;
    sellerAddress: string;
    deadline: Date;
    escrowUtxoTxHash: string;
    escrowUtxoIndex: number;
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
  },
  action: 'buildReleaseTx' | 'buildRefundTx'
): Promise<{ success: boolean; txHash?: string; error?: string }> {
  try {
    const deadlineMs = dateToMilliseconds(params.deadline);

    const buildResult = await callTxBuilder({
      action,
      buyerAddress: params.buyerAddress,
      sellerAddress: params.sellerAddress,
      escrowUtxoTxHash: params.escrowUtxoTxHash,
      escrowUtxoIndex: params.escrowUtxoIndex,
      deadlineMs,
    });

    if (!buildResult.success || !buildResult.txCbor) {
      return { success: false, error: buildResult.error || 'Failed to build transaction' };
    }

    // Partial sign because release needs both buyer + seller
    const partialSign = action === 'buildReleaseTx';
    const signedTx = await walletApi.signTx(buildResult.txCbor, partialSign);
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
