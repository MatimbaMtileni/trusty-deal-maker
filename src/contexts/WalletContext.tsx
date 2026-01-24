import React, { createContext, useContext, useState, useEffect, useCallback } from 'react';
import { WalletInfo, WalletType } from '@/types/escrow';
import { generateMockAddress, getStoredWallet, saveWallet, clearWallet, initializeDemoData } from '@/services/mockBlockchain';

interface WalletContextType {
  wallet: WalletInfo | null;
  isConnecting: boolean;
  connect: (walletType: WalletType) => Promise<void>;
  disconnect: () => void;
  updateBalance: (delta: number) => void;
}

const WalletContext = createContext<WalletContextType | undefined>(undefined);

const WALLET_ICONS: Record<WalletType, string> = {
  nami: '🔷',
  lace: '🎴',
  eternl: '♾️',
};

const WALLET_NAMES: Record<WalletType, string> = {
  nami: 'Nami',
  lace: 'Lace',
  eternl: 'Eternl',
};

export const WalletProvider: React.FC<{ children: React.ReactNode }> = ({ children }) => {
  const [wallet, setWallet] = useState<WalletInfo | null>(null);
  const [isConnecting, setIsConnecting] = useState(false);

  // Load wallet from storage on mount
  useEffect(() => {
    const stored = getStoredWallet();
    if (stored) {
      setWallet(stored);
    }
  }, []);

  const connect = async (walletType: WalletType) => {
    setIsConnecting(true);
    
    // Simulate connection delay
    await new Promise(resolve => setTimeout(resolve, 1500));

    const address = generateMockAddress();
    const initialBalance = 10000; // 10,000 ADA for demo

    const walletInfo: WalletInfo = {
      name: WALLET_NAMES[walletType],
      icon: WALLET_ICONS[walletType],
      address,
      balance: initialBalance,
      connected: true,
    };

    setWallet(walletInfo);
    saveWallet(walletInfo);
    
    // Initialize demo data for new users
    initializeDemoData(address);
    
    setIsConnecting(false);
  };

  const disconnect = () => {
    setWallet(null);
    clearWallet();
  };

  const updateBalance = useCallback((delta: number) => {
    setWallet(prev => {
      if (!prev) return null;
      const updated = { ...prev, balance: prev.balance + delta };
      saveWallet(updated);
      return updated;
    });
  }, []);

  return (
    <WalletContext.Provider value={{ wallet, isConnecting, connect, disconnect, updateBalance }}>
      {children}
    </WalletContext.Provider>
  );
};

export const useWallet = () => {
  const context = useContext(WalletContext);
  if (context === undefined) {
    throw new Error('useWallet must be used within a WalletProvider');
  }
  return context;
};
