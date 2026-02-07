// ============================================================================
// Script Registry - Plutus Contract Hash Verification
// ============================================================================

import { TARGET_NETWORK } from './networkGuard';
import type { CardanoNetwork } from './types';
import { bech32ToHex } from './addressUtils';

/**
 * Script deployment info per network
 */
export interface ScriptDeployment {
  /** Bech32 script address (addr_test1... or addr1...) */
  address: string;
  /** Script hash (hex) - 28 bytes / 56 hex chars */
  scriptHash: string;
  /** Block where the script was first used */
  deploymentBlock?: number;
  /** Human-readable version */
  version: string;
  /**
   * Compiled Plutus V2 script CBOR (hex).
   * 
   * HOW TO OBTAIN:
   * 1. cd plutus-contract && cabal build
   * 2. cabal run serialize-escrow   → produces escrow.plutus
   * 3. Extract the "cborHex" field from escrow.plutus (TextEnvelope JSON)
   * 4. Paste that hex string here
   * 
   * Without this value, release/refund transactions cannot attach
   * the spending validator and will fail on-chain.
   */
  scriptCbor: string;
}

/**
 * Registry of known escrow contract deployments
 * 
 * IMPORTANT: Update these values after deploying the Plutus contract!
 * Run `cardano-cli address build --payment-script-file escrow.plutus` to get the address
 * Run `cardano-cli transaction policyid --script-file escrow.plutus` to get the hash
 */
export const ESCROW_SCRIPTS: Record<CardanoNetwork, ScriptDeployment> = {
  preprod: {
    // TODO: Replace with actual deployed script address after compilation
    address: 'addr_test1wz5fxflu6hklvkp58ynke5hylm4aqrv7ewl4x6k7q37etwgz9u4a6',
    scriptHash: 'a89327fc6afdbed834721d9a5e4fdebea06cf2cbf53699b780f96576',
    version: '1.0.0',
    deploymentBlock: undefined, // Set after deployment
    // ⚠️  PLACEHOLDER – replace with real compiled CBOR from escrow.plutus
    scriptCbor: '',
  },
  preview: {
    address: '',
    scriptHash: '',
    version: '1.0.0',
    scriptCbor: '',
  },
  mainnet: {
    address: '',
    scriptHash: '',
    version: '1.0.0',
    scriptCbor: '',
  },
};

/**
 * Get the current network's script deployment
 */
export function getActiveScript(): ScriptDeployment {
  return ESCROW_SCRIPTS[TARGET_NETWORK];
}

/**
 * Check if a script address matches our deployed escrow contract
 */
export function verifyScriptAddress(address: string): {
  valid: boolean;
  error?: string;
  scriptHash?: string;
} {
  const activeScript = getActiveScript();
  
  if (!activeScript.address) {
    return {
      valid: false,
      error: `Escrow contract not deployed on ${TARGET_NETWORK}`,
    };
  }

  // Normalize addresses for comparison
  const normalizedInput = address.toLowerCase().trim();
  const normalizedExpected = activeScript.address.toLowerCase();
  
  if (normalizedInput === normalizedExpected) {
    return {
      valid: true,
      scriptHash: activeScript.scriptHash,
    };
  }

  // Try hex comparison
  try {
    const inputHex = bech32ToHex(address);
    const expectedHex = bech32ToHex(activeScript.address);
    
    if (inputHex === expectedHex) {
      return {
        valid: true,
        scriptHash: activeScript.scriptHash,
      };
    }
  } catch {
    // Ignore conversion errors
  }

  return {
    valid: false,
    error: 'Script address does not match deployed escrow contract',
  };
}

/**
 * Verify that a script hash matches our contract
 */
export function verifyScriptHash(hash: string): boolean {
  const activeScript = getActiveScript();
  if (!activeScript.scriptHash) return false;
  
  return hash.toLowerCase() === activeScript.scriptHash.toLowerCase();
}

/**
 * Get the escrow script address for the current network
 */
export function getEscrowScriptAddress(): string | null {
  const activeScript = getActiveScript();
  return activeScript.address || null;
}

/**
 * Get the escrow script hash for the current network
 */
export function getEscrowScriptHash(): string | null {
  const activeScript = getActiveScript();
  return activeScript.scriptHash || null;
}

/**
 * Check if the escrow contract is deployed on current network
 */
export function isScriptDeployed(): boolean {
  const activeScript = getActiveScript();
  return Boolean(activeScript.address && activeScript.scriptHash);
}

/**
 * Validate that a transaction's script reference matches our contract
 * Used when building or verifying escrow transactions
 */
export function validateTransactionScript(txScriptHash: string): {
  valid: boolean;
  message: string;
} {
  const activeScript = getActiveScript();
  
  if (!activeScript.scriptHash) {
    return {
      valid: false,
      message: `Escrow contract not deployed on ${TARGET_NETWORK}`,
    };
  }
  
  if (txScriptHash.toLowerCase() !== activeScript.scriptHash.toLowerCase()) {
    return {
      valid: false,
      message: 'Transaction uses unknown script. This may be a phishing attempt.',
    };
  }
  
  return {
    valid: true,
    message: 'Script verified ✓',
  };
}

/**
 * Generate a script verification badge/status
 */
export function getScriptVerificationStatus(): {
  deployed: boolean;
  network: CardanoNetwork;
  address: string | null;
  version: string;
} {
  const activeScript = getActiveScript();
  
  return {
    deployed: isScriptDeployed(),
    network: TARGET_NETWORK,
    address: activeScript.address || null,
    version: activeScript.version,
  };
}
