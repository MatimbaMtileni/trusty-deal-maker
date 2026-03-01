// ============================================================================
// Transaction Builder Service – Native Script escrow via edge function
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
    const parts: string[] = [];
    if (error.message) parts.push(error.message);
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const anyErr = error as any;
    if (anyErr.details) parts.push(String(anyErr.details));
    if (anyErr.status) parts.push(`status=${anyErr.status}`);

    return { success: false, error: parts.filter(Boolean).join(' — ') || 'Edge function error' };
  }

  return data as TxBuildResult;
}

// ============================================================================
// Slot conversion – Preprod genesis parameters
// ============================================================================

// Preprod shelley genesis: slot 0 = 2022-04-01T00:00:00Z, 1 slot = 1 second
const PREPROD_SHELLEY_START_UNIX_MS = 1654041600000; // 2022-06-01T00:00:00Z (shelley start on preprod)
const PREPROD_SHELLEY_START_SLOT = 0;
const SLOT_LENGTH_MS = 1000;

/**
 * Convert a JS Date to a Cardano Preprod slot number.
 */
function dateToSlot(date: Date): number {
  const diffMs = date.getTime() - PREPROD_SHELLEY_START_UNIX_MS;
  return PREPROD_SHELLEY_START_SLOT + Math.floor(diffMs / SLOT_LENGTH_MS);
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
    const deadlineSlot = dateToSlot(params.deadline);

    const buildResult = await callTxBuilder({
      action: 'buildFundTx',
      buyerAddress: params.buyerAddress,
      sellerAddress: params.sellerAddress,
      amount: params.amount.toString(),
      deadlineSlot,
    });

    if (!buildResult.success || !buildResult.txCbor) {
      return { success: false, error: buildResult.error || 'Failed to build transaction' };
    }

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
    const deadlineSlot = dateToSlot(params.deadline);

    const buildResult = await callTxBuilder({
      action,
      buyerAddress: params.buyerAddress,
      sellerAddress: params.sellerAddress,
      escrowUtxoTxHash: params.escrowUtxoTxHash,
      escrowUtxoIndex: params.escrowUtxoIndex,
      deadlineSlot,
    });

    if (!buildResult.success || !buildResult.txCbor) {
      return { success: false, error: buildResult.error || 'Failed to build transaction' };
    }

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
