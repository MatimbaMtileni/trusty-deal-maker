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
  /** Runtime integrity hash (hex) of script bytes */
  scriptHash?: string;
}

/**
 * Load Plutus script from environment variables
 */
function loadPlutusScript(): { base64: string | undefined; address: string | undefined; hash: string | undefined } {
  return {
    base64: import.meta.env.VITE_ESCROW_SCRIPT_BASE64,
    address: import.meta.env.VITE_ESCROW_SCRIPT_ADDRESS,
    hash: import.meta.env.VITE_ESCROW_SCRIPT_HASH,
  };
}

function decodeBase64(base64: string): Uint8Array {
  const binary = atob(base64);
  const bytes = new Uint8Array(binary.length);
  for (let i = 0; i < binary.length; i++) {
    bytes[i] = binary.charCodeAt(i);
  }
  return bytes;
}

function fnv1a32Hex(input: Uint8Array): string {
  let hash = 0x811c9dc5;
  for (const value of input) {
    hash ^= value;
    hash = Math.imul(hash, 0x01000193);
  }
  return (hash >>> 0).toString(16).padStart(8, '0');
}

export const ESCROW_SCRIPTS: Record<CardanoNetwork, ScriptDeployment> = {
  preprod: {
    enabled: true,
    version: '2.0.0-plutus',
    type: 'plutus',
    scriptBase64: loadPlutusScript().base64,
    scriptAddress: loadPlutusScript().address,
    scriptHash: loadPlutusScript().hash,
  },
  preview: {
    enabled: false,
    version: '2.0.0-plutus',
    type: 'plutus',
    scriptBase64: loadPlutusScript().base64,
    scriptAddress: loadPlutusScript().address,
    scriptHash: loadPlutusScript().hash,
  },
  mainnet: {
    enabled: false,
    version: '2.0.0-plutus',
    type: 'plutus',
    scriptBase64: loadPlutusScript().base64,
    scriptAddress: loadPlutusScript().address,
    scriptHash: loadPlutusScript().hash,
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
 * Get expected runtime script hash from env
 */
export function getExpectedScriptHash(): string | undefined {
  return getActiveScript().scriptHash;
}

/**
 * Compute a runtime fingerprint from script bytes
 */
export function getComputedScriptHash(): string | undefined {
  const scriptBase64 = getScriptBase64();
  if (!scriptBase64) return undefined;

  try {
    return fnv1a32Hex(decodeBase64(scriptBase64));
  } catch {
    return undefined;
  }
}

/**
 * Verify script bytes match expected runtime hash
 */
export function isScriptHashVerified(): boolean {
  const expected = getExpectedScriptHash();
  const computed = getComputedScriptHash();
  if (!expected || !computed) return false;
  return expected.toLowerCase() === computed.toLowerCase();
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
  expectedScriptHash?: string;
  computedScriptHash?: string;
  hashVerified: boolean;
} {
  const activeScript = getActiveScript();

  return {
    deployed: isScriptDeployed(),
    network: TARGET_NETWORK,
    version: activeScript.version,
    type: activeScript.type,
    scriptAddress: activeScript.scriptAddress,
    expectedScriptHash: getExpectedScriptHash(),
    computedScriptHash: getComputedScriptHash(),
    hashVerified: isScriptHashVerified(),
  };
}
