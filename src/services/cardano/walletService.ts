// ============================================================================
// Wallet Service - CIP-30 Wallet Management with Network Safety
// ============================================================================

import { decode } from 'cbor-x';
import { 
  CIP30WalletAPI, 
  InstalledWallet, 
  WalletState,
} from './types';
import { validateWalletNetwork, TARGET_NETWORK, lovelaceToAda } from './networkGuard';
import { blockchainService } from './blockchainService';
import { hexToBech32 } from './addressUtils';

/** Thrown when a wallet is on the wrong network. UI catches this to show guided switch steps. */
export class WalletNetworkMismatchError extends Error {
  readonly code = 'NETWORK_MISMATCH';
  constructor(
    public readonly walletName: string,
    public readonly expected: string,
    public readonly actual: string,
    message: string,
  ) {
    super(message);
    this.name = 'WalletNetworkMismatchError';
  }
}


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
        throw new Error(`${walletName} wallet is not installed, or the page is running inside an iframe where the extension is not injected. Open the app in a new browser tab.`);
      }

      // Request wallet access (with 60s timeout so we don't hang forever
      // if the wallet popup is blocked or dismissed silently)
      const api = await Promise.race([
        walletProvider.enable(),
        new Promise<never>((_, reject) =>
          setTimeout(
            () => reject(new Error('Wallet approval timed out. Open the wallet extension, approve the connection, and try again. If the app is in an iframe, open it in a new tab.')),
            60_000
          )
        ),
      ]);

      // Verify network matches BEFORE doing any further wallet calls
      const networkId = await api.getNetworkId();
      const networkCheck = validateWalletNetwork(networkId);

      if (!networkCheck.valid) {
        // Reset state and throw typed error so UI can show guided switch steps
        this.updateState({ connected: false, connecting: false, error: networkCheck.message ?? 'Network mismatch' });
        throw new WalletNetworkMismatchError(
          walletName,
          networkCheck.expected,
          networkCheck.actual,
          networkCheck.message ?? 'Network mismatch',
        );
      }


      // Get addresses
      const usedAddresses = await api.getUsedAddresses();
      const unusedAddresses = await api.getUnusedAddresses();
      const changeAddress = await api.getChangeAddress();
      
      const rawAddress = usedAddresses[0] || unusedAddresses[0] || changeAddress;
      
      // Convert hex address to bech32 for display
      const address = hexToBech32(rawAddress);
      
      if (!address) {
        throw new Error('No address found in wallet');
      }

      // Get reward/stake address
      let stakeAddress: string | null = null;
      try {
        const rewardAddresses = await api.getRewardAddresses();
        // Convert stake address to bech32 as well
        stakeAddress = rewardAddresses[0] ? hexToBech32(rewardAddresses[0]) : null;
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
   * Refresh balance from chain.
   * Prefer summing UTxOs (matches what the wallet UI shows across all its
   * derived addresses), then fall back to wallet.getBalance() (which on some
   * wallets like Lace under-reports by returning only the active account),
   * and finally Blockfrost on the active payment address.
   */
  async refreshBalance(): Promise<void> {
    if (!this.state.api || !this.state.address) return;

    // 1) Sum UTxOs — most accurate, matches wallet UI total
    try {
      const utxos = await this.state.api.getUtxos();
      if (utxos && utxos.length > 0) {
        let total = 0n;
        for (const u of utxos) {
          total += this.parseUtxoCoin(u);
        }
        if (total > 0n) {
          this.updateState({ balance: total });
          return;
        }
      }
    } catch (err) {
      console.warn('[Wallet] getUtxos failed, falling back to getBalance:', err);
    }

    // 2) Wallet-reported balance
    try {
      const balanceCbor = await this.state.api.getBalance();
      const balance = this.parseBalanceCbor(balanceCbor);
      this.updateState({ balance });
      return;
    } catch {
      // 3) On-chain query for the active payment address
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
    try {
      // Convert hex string to Uint8Array
      const bytes = new Uint8Array(cbor.length / 2);
      for (let i = 0; i < cbor.length; i += 2) {
        bytes[i / 2] = parseInt(cbor.substr(i, 2), 16);
      }
      
      // Use cbor-x to decode properly
      const decoded = decode(bytes);
      
      // Value can be:
      // - Just a number (coin only, no multi-assets)
      // - An array [coin, multiasset] where coin is the lovelace amount
      if (typeof decoded === 'number') {
        return BigInt(decoded);
      }
      
      if (typeof decoded === 'bigint') {
        return decoded;
      }
      
      if (Array.isArray(decoded)) {
        // First element is the lovelace amount
        const lovelace = decoded[0];
        if (typeof lovelace === 'number') {
          return BigInt(lovelace);
        }
        if (typeof lovelace === 'bigint') {
          return lovelace;
        }
      }
      
      console.warn('[Wallet] Unexpected CBOR structure:', decoded);
      return 0n;
    } catch (error) {
      console.warn('[Wallet] CBOR parse error, falling back to 0:', error);
      return 0n;
    }
  }

  /**
   * Extract the lovelace amount from a single UTxO CBOR returned by
   * CIP-30 getUtxos(). A UTxO is [TransactionInput, TransactionOutput].
   * TransactionOutput is either a legacy array [address, value, ...] or a
   * Babbage map { 0: address, 1: value, 2: datum, 3: script_ref }.
   * Value is `coin` (uint) or `[coin, multiasset]`.
   */
  private parseUtxoCoin(utxoCbor: string): bigint {
    try {
      const bytes = new Uint8Array(utxoCbor.length / 2);
      for (let i = 0; i < utxoCbor.length; i += 2) {
        bytes[i / 2] = parseInt(utxoCbor.substr(i, 2), 16);
      }
      const decoded = decode(bytes);
      if (!Array.isArray(decoded) || decoded.length < 2) return 0n;
      const output = decoded[1];

      let value: unknown;
      if (Array.isArray(output)) {
        value = output[1];
      } else if (output && typeof output === 'object') {
        const map = output as Record<number, unknown> & { get?: (k: number) => unknown };
        value = map[1] ?? map.get?.(1);
      }

      if (typeof value === 'number') return BigInt(value);
      if (typeof value === 'bigint') return value;
      if (Array.isArray(value)) {
        const coin = value[0];
        if (typeof coin === 'number') return BigInt(coin);
        if (typeof coin === 'bigint') return coin;
      }
      return 0n;
    } catch (err) {
      console.warn('[Wallet] parseUtxoCoin failed:', err);
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
