// ============================================================================
// Script Registry - Plutus V2 Escrow Contract
// ============================================================================

import { TARGET_NETWORK } from './networkGuard';
import type { CardanoNetwork } from './types';

/**
 * Plutus V2 escrow script deployment.
 * The script address and base64 encoding are deployed on-chain
 * and configured via environment variables.
 */
export interface ScriptDeployment {
  /** Whether the script is enabled on this network */
  enabled: boolean;
  /** Human-readable version */
  version: string;
  /** Script type: plutus for V2 validator */
  type: 'plutus' | 'native';
  /** Base64-encoded Plutus script (for Lucid) */
  scriptBase64?: string;
  /** On-chain script address */
  scriptAddress?: string;
}

/**
 * Load Plutus script from environment variables
 */
function loadPlutusScript(): { base64: string | undefined; address: string | undefined } {
  return {
    base64: import.meta.env.VITE_ESCROW_SCRIPT_BASE64,
    address: import.meta.env.VITE_ESCROW_SCRIPT_ADDRESS,
  };
}

export const ESCROW_SCRIPTS: Record<CardanoNetwork, ScriptDeployment> = {
  preprod: {
    enabled: true,
    version: '2.0.0-plutus',
    type: 'plutus',
    ...loadPlutusScript(),
  },
  preview: {
    enabled: false,
    version: '2.0.0-plutus',
    type: 'plutus',
    ...loadPlutusScript(),
  },
  mainnet: {
    enabled: false,
    version: '2.0.0-plutus',
    type: 'plutus',
    ...loadPlutusScript(),
  },
};

/**
 * Get the current network's script deployment
 */
export function getActiveScript(): ScriptDeployment {
  return ESCROW_SCRIPTS[TARGET_NETWORK];
}

/**
 * Check if the escrow contract is enabled and properly configured on current network
 */
export function isScriptDeployed(): boolean {
  const script = getActiveScript();
  return script.enabled && !!script.scriptBase64 && !!script.scriptAddress;
}

/**
 * Get script base64 for Lucid
 */
export function getScriptBase64(): string | undefined {
  return getActiveScript().scriptBase64;
}

/**
 * Get script address for payments
 */
export function getScriptAddress(): string | undefined {
  return getActiveScript().scriptAddress;
}

/**
 * Get script verification status for UI
 */
export function getScriptVerificationStatus(): {
  deployed: boolean;
  network: CardanoNetwork;
  version: string;
  type: string;
  scriptAddress?: string;
} {
  const activeScript = getActiveScript();

  return {
    deployed: isScriptDeployed(),
    network: TARGET_NETWORK,
    version: activeScript.version,
    type: activeScript.type,
    scriptAddress: activeScript.scriptAddress,
  };
}
