// ============================================================================
// Script Registry - Native Script Escrow (no Plutus compilation needed)
// ============================================================================

import { TARGET_NETWORK } from './networkGuard';
import type { CardanoNetwork } from './types';

/**
 * Native-script escrow doesn't use a fixed script address.
 * The address is derived deterministically from (buyerPkh, sellerPkh, deadlineSlot)
 * by the edge function at build time.
 *
 * This registry now only tracks network config and deployment status.
 */
export interface ScriptDeployment {
  /** Whether native-script escrow is enabled on this network */
  enabled: boolean;
  /** Human-readable version */
  version: string;
  /** Script type */
  type: 'native';
}

export const ESCROW_SCRIPTS: Record<CardanoNetwork, ScriptDeployment> = {
  preprod: {
    enabled: true,
    version: '2.0.0-native',
    type: 'native',
  },
  preview: {
    enabled: false,
    version: '2.0.0-native',
    type: 'native',
  },
  mainnet: {
    enabled: false,
    version: '2.0.0-native',
    type: 'native',
  },
};

/**
 * Get the current network's script deployment
 */
export function getActiveScript(): ScriptDeployment {
  return ESCROW_SCRIPTS[TARGET_NETWORK];
}

/**
 * Check if the escrow contract is enabled on current network
 */
export function isScriptDeployed(): boolean {
  return getActiveScript().enabled;
}

/**
 * Get script verification status for UI
 */
export function getScriptVerificationStatus(): {
  deployed: boolean;
  network: CardanoNetwork;
  version: string;
  type: string;
} {
  const activeScript = getActiveScript();

  return {
    deployed: activeScript.enabled,
    network: TARGET_NETWORK,
    version: activeScript.version,
    type: activeScript.type,
  };
}
