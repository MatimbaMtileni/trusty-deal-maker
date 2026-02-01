import React, { createContext, useContext, useState, useCallback, useEffect } from 'react';
import { WalletInfo, InstalledWallet } from '@/types/escrow';
import { toast } from 'sonner';

// CIP-30 Wallet API types
interface CardanoWalletApi {
  getNetworkId(): Promise<number>;
  getUsedAddresses(): Promise<string[]>;
  getUnusedAddresses(): Promise<string[]>;
  getChangeAddress(): Promise<string>;
  getBalance(): Promise<string>;
}

interface WalletContextType {
  wallet: WalletInfo | null;
  walletApi: CardanoWalletApi | null;
  installedWallets: InstalledWallet[];
  isConnecting: boolean;
  connect: (walletName: string) => Promise<void>;
  disconnect: () => void;
  refreshBalance: () => Promise<void>;
}

const WalletContext = createContext<WalletContextType | null>(null);

export const useWallet = () => {
  const context = useContext(WalletContext);
  if (!context) {
    throw new Error('useWallet must be used within a WalletProvider');
  }
  return context;
};

const WALLET_STORAGE_KEY = 'cardano_connected_wallet';

// Detect installed CIP-30 wallets
const getInstalledWallets = (): InstalledWallet[] => {
  const cardano = (window as any).cardano;
  if (!cardano) return [];

  const walletKeys = ['nami', 'lace', 'eternl', 'flint', 'yoroi', 'typhon', 'gerowallet'];
  const installed: InstalledWallet[] = [];

  for (const key of walletKeys) {
    if (cardano[key]) {
      installed.push({
        name: cardano[key].name || key,
        icon: cardano[key].icon || '',
        version: cardano[key].apiVersion || '1.0.0',
      });
    }
  }
  return installed;
};

// Convert hex address to bech32 (simplified - in production use proper library)
const hexToAddress = (hex: string): string => {
  // For display, we'll show a truncated version. 
  // In production, use @emurgo/cardano-serialization-lib-browser for proper conversion
  return `addr1${hex.substring(0, 50)}`;
};

export const WalletProvider: React.FC<{ children: React.ReactNode }> = ({ children }) => {
  const [wallet, setWallet] = useState<WalletInfo | null>(null);
  const [walletApi, setWalletApi] = useState<CardanoWalletApi | null>(null);
  const [installedWallets, setInstalledWallets] = useState<InstalledWallet[]>([]);
  const [isConnecting, setIsConnecting] = useState(false);

  // Detect installed wallets on mount
  useEffect(() => {
    const detectWallets = () => {
      // Small delay to let wallet extensions inject
      setTimeout(() => {
        setInstalledWallets(getInstalledWallets());
      }, 100);
    };
    detectWallets();
    window.addEventListener('load', detectWallets);
    return () => window.removeEventListener('load', detectWallets);
  }, []);

  const connect = useCallback(async (walletName: string) => {
    setIsConnecting(true);
    try {
      const cardano = (window as any).cardano;
      const walletProvider = cardano?.[walletName.toLowerCase()];
      
      if (!walletProvider) {
        throw new Error(`${walletName} wallet is not installed`);
      }

      // Enable wallet (prompts user for permission)
      const api: CardanoWalletApi = await walletProvider.enable();
      setWalletApi(api);

      // Get wallet info
      const addresses = await api.getUsedAddresses();
      const changeAddress = await api.getChangeAddress();
      const address = addresses[0] || changeAddress;
      
      const balanceHex = await api.getBalance();
      const lovelace = parseInt(balanceHex, 16) || 0;
      const balanceAda = lovelace / 1_000_000;

      const networkId = await api.getNetworkId();

      const installedWallet = installedWallets.find(
        w => w.name.toLowerCase() === walletName.toLowerCase()
      );

      const info: WalletInfo = {
        name: walletName,
        icon: installedWallet?.icon || '💳',
        address: address || 'addr1...',
        balance: balanceAda,
        connected: true,
        networkId,
      };

      setWallet(info);
      localStorage.setItem(WALLET_STORAGE_KEY, walletName);

      // If the user is authenticated with Supabase, link wallet to their profile
      try {
        // Dynamically import to avoid circular dependency during module load
        const { escrowApi } = await import('@/services/escrowApi');
        await escrowApi.updateProfileWallet(info.address);
      } catch (err) {
        // Not critical — user may not be signed in
        console.debug('Could not update profile wallet:', err);
      }

      const networkName = networkId === 1 ? 'Mainnet' : 'Testnet';
      toast.success(`Connected to ${walletName}`, {
        description: `Network: ${networkName} | Balance: ${balanceAda.toLocaleString()} ₳`,
      });
    } catch (error: any) {
      console.error('Wallet connection error:', error);
      toast.error('Connection Failed', { 
        description: error.message || 'Failed to connect wallet' 
      });
      throw error;
    } finally {
      setIsConnecting(false);
    }
  }, [installedWallets]);

  // Auto-reconnect on mount
  useEffect(() => {
    const savedWalletName = localStorage.getItem(WALLET_STORAGE_KEY);
    if (savedWalletName && installedWallets.length > 0) {
      const isInstalled = installedWallets.some(
        w => w.name.toLowerCase() === savedWalletName.toLowerCase()
      );
      if (isInstalled) {
        connect(savedWalletName).catch(() => {
          localStorage.removeItem(WALLET_STORAGE_KEY);
        });
      }
    }
  }, [installedWallets, connect]);

  const disconnect = useCallback(() => {
    setWallet(null);
    setWalletApi(null);
    localStorage.removeItem(WALLET_STORAGE_KEY);
    toast.info('Wallet disconnected');
  }, []);

  const refreshBalance = useCallback(async () => {
    if (!walletApi || !wallet) return;
    try {
      const balanceHex = await walletApi.getBalance();
      const lovelace = parseInt(balanceHex, 16) || 0;
      const balanceAda = lovelace / 1_000_000;
      setWallet(prev => prev ? { ...prev, balance: balanceAda } : null);
    } catch (error) {
      console.error('Error refreshing balance:', error);
    }
  }, [walletApi, wallet]);

  return (
    <WalletContext.Provider
      value={{
        wallet,
        walletApi,
        installedWallets,
        isConnecting,
        connect,
        disconnect,
        refreshBalance,
      }}
    >
      {children}
    </WalletContext.Provider>
  );
};
