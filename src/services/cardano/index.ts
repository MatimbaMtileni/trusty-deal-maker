// ============================================================================
// Cardano Services - Public API
// ============================================================================

// Types
export * from './types';

// Network safety
export {
  TARGET_NETWORK,
  isTestnet,
  validateWalletNetwork,
  validateAddress,
  validateTxHash,
  getExplorerUrl,
  lovelaceToAda,
  adaToLovelace,
  formatAda,
} from './networkGuard';

// Address utilities
export {
  hexToBech32,
  bech32ToHex,
  truncateAddress,
  normalizeAddress,
  isAddressForNetwork,
} from './addressUtils';

// Script registry & verification
export {
  ESCROW_SCRIPTS,
  getActiveScript,
  verifyScriptAddress,
  verifyScriptHash,
  getEscrowScriptAddress,
  getEscrowScriptHash,
  isScriptDeployed,
  validateTransactionScript,
  getScriptVerificationStatus,
  type ScriptDeployment,
} from './scriptRegistry';

// Datum builder
export {
  createEscrowDatum,
  extractPubKeyHash,
  serializeDatum,
  serializeRedeemer,
  hashDatum,
  validateDatum,
  parseDeadline,
  EscrowAction,
} from './datumBuilder';

// Blockchain service
export { blockchainService } from './blockchainService';

// Wallet service
export { walletService } from './walletService';

// Transaction confirmation
export {
  trackTransaction,
  isConfirmed,
  getConfirmationText,
  estimateTimeToConfirmation,
  type TxTrackingState,
  type TrackingOptions,
  type ConfirmationLevel,
} from './txConfirmation';
