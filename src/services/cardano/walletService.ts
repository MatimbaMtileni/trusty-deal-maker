// ============================================================================
// Wallet Service - CIP-30 Wallet Management with Network Safety
// ============================================================================

import { 
  CIP30WalletAPI, 
  InstalledWallet, 
  WalletState,
} from './types';
import { validateWalletNetwork, TARGET_NETWORK, lovelaceToAda } from './networkGuard';
import { blockchainService } from './blockchainService';

/** Supported wallet keys */
const SUPPORTED_WALLETS = [
  'nami', 'lace', 'eternl', 'flint', 'yoroi', 'typhon', 'gerowallet', 'vespr'
] as const;

/** Storage key for wallet reconnection */
const WALLET_STORAGE_KEY = 'cardano_wallet_name';

/** Balance polling interval (ms) */
const BALANCE_POLL_INTERVAL = 30_000;

/**
 * Wallet service singleton
 */
class WalletService {
  private state: WalletState = {
    connected: false,
    connecting: false,
    walletName: null,
    api: null,
    address: null,
    stakeAddress: null,
    balance: 0n,
    networkId: 0,
    error: null,
  };

  private listeners: Set<(state: WalletState) => void> = new Set();
  private balancePollTimer: NodeJS.Timeout | null = null;

  /**
   * Subscribe to wallet state changes
   */
  subscribe(listener: (state: WalletState) => void): () => void {
    this.listeners.add(listener);
    // Immediately notify with current state
    listener(this.state);
    return () => this.listeners.delete(listener);
  }

  private notify(): void {
    this.listeners.forEach(listener => listener(this.state));
  }

  private updateState(updates: Partial<WalletState>): void {
    this.state = { ...this.state, ...updates };
    this.notify();
  }

  /**
   * Detect installed CIP-30 wallets
   */
  getInstalledWallets(): InstalledWallet[] {
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const cardano = (window as any).cardano as Record<string, unknown> | undefined;
    if (!cardano) return [];

    return SUPPORTED_WALLETS
      .filter(key => cardano[key])
      .map(key => {
        const wallet = cardano[key] as Record<string, unknown>;
        return {
          name: String(wallet.name || key),
          icon: String(wallet.icon || ''),
          apiVersion: String(wallet.apiVersion || '1.0.0'),
          isEnabled: typeof wallet.isEnabled === 'function',
        };
      });
  }

  /**
   * Connect to a CIP-30 wallet
   */
  async connect(walletName: string): Promise<void> {
    this.updateState({ connecting: true, error: null });

    try {
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      const cardano = (window as any).cardano as Record<string, unknown> | undefined;
      const walletProvider = cardano?.[walletName.toLowerCase()] as {
        enable?: () => Promise<CIP30WalletAPI>;
        isEnabled?: () => Promise<boolean>;
      } | undefined;

      if (!walletProvider?.enable) {
        throw new Error(`${walletName} wallet is not installed`);
      }

      // Request wallet access
      const api = await walletProvider.enable();

      // Verify network matches
      const networkId = await api.getNetworkId();
      const networkCheck = validateWalletNetwork(networkId);
      
      if (!networkCheck.valid) {
        throw new Error(networkCheck.message);
      }

      // Get addresses
      const usedAddresses = await api.getUsedAddresses();
      const unusedAddresses = await api.getUnusedAddresses();
      const changeAddress = await api.getChangeAddress();
      
      const address = usedAddresses[0] || unusedAddresses[0] || changeAddress;
      
      if (!address) {
        throw new Error('No address found in wallet');
      }

      // Get reward/stake address
      let stakeAddress: string | null = null;
      try {
        const rewardAddresses = await api.getRewardAddresses();
        stakeAddress = rewardAddresses[0] || null;
      } catch {
        // Some wallets don't support this
      }

      // Get balance (CBOR-encoded)
      const balanceCbor = await api.getBalance();
      const balance = this.parseBalanceCbor(balanceCbor);

      // Save wallet name for reconnection
      localStorage.setItem(WALLET_STORAGE_KEY, walletName);

      this.updateState({
        connected: true,
        connecting: false,
        walletName,
        api,
        address,
        stakeAddress,
        balance,
        networkId,
        error: null,
      });

      // Start balance polling
      this.startBalancePolling();

    } catch (error) {
      const message = error instanceof Error ? error.message : 'Failed to connect wallet';
      this.updateState({
        connected: false,
        connecting: false,
        error: message,
      });
      throw error;
    }
  }

  /**
   * Disconnect wallet
   */
  disconnect(): void {
    this.stopBalancePolling();
    localStorage.removeItem(WALLET_STORAGE_KEY);
    
    this.updateState({
      connected: false,
      connecting: false,
      walletName: null,
      api: null,
      address: null,
      stakeAddress: null,
      balance: 0n,
      networkId: 0,
      error: null,
    });
  }

  /**
   * Try to reconnect to previously connected wallet
   */
  async tryReconnect(): Promise<boolean> {
    const savedWallet = localStorage.getItem(WALLET_STORAGE_KEY);
    if (!savedWallet) return false;

    const installed = this.getInstalledWallets();
    const isInstalled = installed.some(
      w => w.name.toLowerCase() === savedWallet.toLowerCase()
    );

    if (!isInstalled) {
      localStorage.removeItem(WALLET_STORAGE_KEY);
      return false;
    }

    try {
      await this.connect(savedWallet);
      return true;
    } catch {
      localStorage.removeItem(WALLET_STORAGE_KEY);
      return false;
    }
  }

  /**
   * Refresh balance from chain
   */
  async refreshBalance(): Promise<void> {
    if (!this.state.api || !this.state.address) return;

    try {
      // Try wallet API first
      const balanceCbor = await this.state.api.getBalance();
      const balance = this.parseBalanceCbor(balanceCbor);
      this.updateState({ balance });
    } catch {
      // Fall back to blockchain query
      try {
        const balance = await blockchainService.getAddressBalance(this.state.address);
        this.updateState({ balance });
      } catch (error) {
        console.error('[Wallet] Failed to refresh balance:', error);
      }
    }
  }

  /**
   * Get UTxOs from wallet
   */
  async getUtxos(): Promise<string[] | null> {
    if (!this.state.api) return null;
    return this.state.api.getUtxos();
  }

  /**
   * Sign a transaction
   */
  async signTx(txCbor: string, partialSign = false): Promise<string> {
    if (!this.state.api) {
      throw new Error('Wallet not connected');
    }
    return this.state.api.signTx(txCbor, partialSign);
  }

  /**
   * Sign data (CIP-30)
   */
  async signData(payload: string): Promise<{ signature: string; key: string }> {
    if (!this.state.api || !this.state.address) {
      throw new Error('Wallet not connected');
    }
    return this.state.api.signData(this.state.address, payload);
  }

  /**
   * Submit a signed transaction via wallet
   */
  async submitTx(signedTxCbor: string): Promise<string> {
    if (!this.state.api) {
      throw new Error('Wallet not connected');
    }
    return this.state.api.submitTx(signedTxCbor);
  }

  /**
   * Get current state
   */
  getState(): WalletState {
    return { ...this.state };
  }

  /**
   * Get balance in ADA
   */
  getBalanceAda(): number {
    return lovelaceToAda(this.state.balance);
  }

  /**
   * Get network name
   */
  getNetworkName(): string {
    return TARGET_NETWORK;
  }

  // -------------------------------------------------------------------------
  // Private helpers
  // -------------------------------------------------------------------------

  private parseBalanceCbor(cbor: string): bigint {
    // CIP-30 getBalance() returns CBOR-encoded Value
    // Value = coin / [coin, multiasset]
    // We need to decode CBOR to extract the lovelace amount
    try {
      const bytes = new Uint8Array(cbor.length / 2);
      for (let i = 0; i < cbor.length; i += 2) {
        bytes[i / 2] = parseInt(cbor.substr(i, 2), 16);
      }
      
      // Simple CBOR parsing for our use case
      // First byte tells us the type
      const firstByte = bytes[0];
      
      // Major type 0: unsigned integer
      if (firstByte <= 0x17) {
        return BigInt(firstByte);
      } else if (firstByte === 0x18) {
        return BigInt(bytes[1]);
      } else if (firstByte === 0x19) {
        return BigInt((bytes[1] << 8) | bytes[2]);
      } else if (firstByte === 0x1a) {
        return BigInt(
          (bytes[1] << 24) | (bytes[2] << 16) | (bytes[3] << 8) | bytes[4]
        );
      } else if (firstByte === 0x1b) {
        // 64-bit integer
        let value = 0n;
        for (let i = 1; i <= 8; i++) {
          value = (value << 8n) | BigInt(bytes[i]);
        }
        return value;
      }
      
      // Major type 4: array (for [coin, multiasset])
      if ((firstByte & 0xe0) === 0x80) {
        // It's an array, first element is lovelace
        // Skip array header and parse first element
        const arrayLen = firstByte & 0x1f;
        if (arrayLen >= 1) {
          // Recursively parse starting from byte 1
          const remaining = cbor.slice(2);
          return this.parseBalanceCbor(remaining);
        }
      }
      
      return 0n;
    } catch (error) {
      console.warn('[Wallet] CBOR parse error, falling back to 0:', error);
      return 0n;
    }
  }

  private startBalancePolling(): void {
    this.stopBalancePolling();
    this.balancePollTimer = setInterval(() => {
      this.refreshBalance();
    }, BALANCE_POLL_INTERVAL);
  }

  private stopBalancePolling(): void {
    if (this.balancePollTimer) {
      clearInterval(this.balancePollTimer);
      this.balancePollTimer = null;
    }
  }
}

// Export singleton instance
export const walletService = new WalletService();
