import { EscrowDatum, EscrowTransaction, CreateEscrowParams, EscrowStatus } from '@/types/escrow';

// Generate mock wallet address
export const generateMockAddress = (): string => {
  const chars = 'abcdefghijklmnopqrstuvwxyz0123456789';
  let addr = 'addr1';
  for (let i = 0; i < 50; i++) {
    addr += chars[Math.floor(Math.random() * chars.length)];
  }
  return addr;
};

// Generate mock transaction hash
export const generateTxHash = (): string => {
  const chars = 'abcdef0123456789';
  let hash = '';
  for (let i = 0; i < 64; i++) {
    hash += chars[Math.floor(Math.random() * chars.length)];
  }
  return hash;
};

// Simulate network delay
export const simulateDelay = (ms: number = 1500): Promise<void> => {
  return new Promise(resolve => setTimeout(resolve, ms));
};

// Mock escrow storage
const ESCROWS_KEY = 'cardano_escrows';
const TRANSACTIONS_KEY = 'cardano_transactions';
const WALLET_KEY = 'cardano_wallet';

// Get all escrows from storage
export const getStoredEscrows = (): EscrowDatum[] => {
  const stored = localStorage.getItem(ESCROWS_KEY);
  if (!stored) return [];
  const escrows = JSON.parse(stored);
  return escrows.map((e: any) => ({
    ...e,
    deadline: new Date(e.deadline),
    createdAt: new Date(e.createdAt),
    updatedAt: new Date(e.updatedAt),
  }));
};

// Save escrows to storage
export const saveEscrows = (escrows: EscrowDatum[]): void => {
  localStorage.setItem(ESCROWS_KEY, JSON.stringify(escrows));
};

// Get transactions from storage
export const getStoredTransactions = (): EscrowTransaction[] => {
  const stored = localStorage.getItem(TRANSACTIONS_KEY);
  if (!stored) return [];
  const txs = JSON.parse(stored);
  return txs.map((t: any) => ({
    ...t,
    timestamp: new Date(t.timestamp),
  }));
};

// Save transactions to storage
export const saveTransactions = (transactions: EscrowTransaction[]): void => {
  localStorage.setItem(TRANSACTIONS_KEY, JSON.stringify(transactions));
};

// Get wallet from storage
export const getStoredWallet = () => {
  const stored = localStorage.getItem(WALLET_KEY);
  return stored ? JSON.parse(stored) : null;
};

// Save wallet to storage
export const saveWallet = (wallet: any): void => {
  localStorage.setItem(WALLET_KEY, JSON.stringify(wallet));
};

// Clear wallet from storage
export const clearWallet = (): void => {
  localStorage.removeItem(WALLET_KEY);
};

// Create a new escrow
export const createEscrow = async (
  buyerAddress: string,
  params: CreateEscrowParams,
  updateBalance: (delta: number) => void
): Promise<{ escrow: EscrowDatum; transaction: EscrowTransaction }> => {
  await simulateDelay(2000);

  const escrowId = `escrow_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
  const now = new Date();

  const escrow: EscrowDatum = {
    id: escrowId,
    buyer: buyerAddress,
    seller: params.seller,
    amount: params.amount,
    deadline: params.deadline,
    status: 'active',
    description: params.description,
    createdAt: now,
    updatedAt: now,
  };

  const transaction: EscrowTransaction = {
    id: `tx_${Date.now()}`,
    escrowId,
    type: 'funded',
    from: buyerAddress,
    amount: params.amount,
    timestamp: now,
    txHash: generateTxHash(),
  };

  // Update storage
  const escrows = getStoredEscrows();
  escrows.push(escrow);
  saveEscrows(escrows);

  const transactions = getStoredTransactions();
  transactions.push(transaction);
  saveTransactions(transactions);

  // Deduct from buyer balance
  updateBalance(-params.amount);

  return { escrow, transaction };
};

// Release escrow funds to seller
export const releaseEscrow = async (
  escrowId: string,
  buyerAddress: string,
  updateSellerBalance?: (delta: number) => void
): Promise<{ escrow: EscrowDatum; transaction: EscrowTransaction }> => {
  await simulateDelay(2000);

  const escrows = getStoredEscrows();
  const escrowIndex = escrows.findIndex(e => e.id === escrowId);
  
  if (escrowIndex === -1) throw new Error('Escrow not found');
  
  const escrow = escrows[escrowIndex];
  if (escrow.status !== 'active') throw new Error('Escrow is not active');
  if (escrow.buyer !== buyerAddress) throw new Error('Only buyer can release funds');

  escrow.status = 'completed';
  escrow.updatedAt = new Date();
  escrows[escrowIndex] = escrow;
  saveEscrows(escrows);

  const transaction: EscrowTransaction = {
    id: `tx_${Date.now()}`,
    escrowId,
    type: 'released',
    from: escrow.buyer,
    to: escrow.seller,
    amount: escrow.amount,
    timestamp: new Date(),
    txHash: generateTxHash(),
  };

  const transactions = getStoredTransactions();
  transactions.push(transaction);
  saveTransactions(transactions);

  return { escrow, transaction };
};

// Refund escrow to buyer
export const refundEscrow = async (
  escrowId: string,
  requesterAddress: string,
  updateBalance: (delta: number) => void
): Promise<{ escrow: EscrowDatum; transaction: EscrowTransaction }> => {
  await simulateDelay(2000);

  const escrows = getStoredEscrows();
  const escrowIndex = escrows.findIndex(e => e.id === escrowId);
  
  if (escrowIndex === -1) throw new Error('Escrow not found');
  
  const escrow = escrows[escrowIndex];
  if (escrow.status !== 'active') throw new Error('Escrow is not active');
  
  // Check if deadline has passed for buyer to refund
  const now = new Date();
  if (escrow.buyer === requesterAddress && now < escrow.deadline) {
    throw new Error('Cannot refund before deadline');
  }

  escrow.status = 'refunded';
  escrow.updatedAt = new Date();
  escrows[escrowIndex] = escrow;
  saveEscrows(escrows);

  const transaction: EscrowTransaction = {
    id: `tx_${Date.now()}`,
    escrowId,
    type: 'refunded',
    from: 'escrow_script',
    to: escrow.buyer,
    amount: escrow.amount,
    timestamp: new Date(),
    txHash: generateTxHash(),
  };

  const transactions = getStoredTransactions();
  transactions.push(transaction);
  saveTransactions(transactions);

  // Return funds to buyer
  updateBalance(escrow.amount);

  return { escrow, transaction };
};

// Get escrows for a specific address
export const getEscrowsForAddress = (address: string): EscrowDatum[] => {
  const escrows = getStoredEscrows();
  return escrows.filter(e => e.buyer === address || e.seller === address);
};

// Get transactions for a specific escrow
export const getTransactionsForEscrow = (escrowId: string): EscrowTransaction[] => {
  const transactions = getStoredTransactions();
  return transactions.filter(t => t.escrowId === escrowId);
};

// Get a single escrow by ID
export const getEscrowById = (escrowId: string): EscrowDatum | undefined => {
  const escrows = getStoredEscrows();
  return escrows.find(e => e.id === escrowId);
};

// Initialize demo data
export const initializeDemoData = (buyerAddress: string): void => {
  const existingEscrows = getStoredEscrows();
  if (existingEscrows.length > 0) return;

  const demoSeller = generateMockAddress();
  const now = new Date();

  const demoEscrows: EscrowDatum[] = [
    {
      id: 'demo_escrow_1',
      buyer: buyerAddress,
      seller: demoSeller,
      amount: 500,
      deadline: new Date(now.getTime() + 7 * 24 * 60 * 60 * 1000), // 7 days
      status: 'active',
      description: 'Web development services - Frontend milestone',
      createdAt: new Date(now.getTime() - 2 * 24 * 60 * 60 * 1000),
      updatedAt: new Date(now.getTime() - 2 * 24 * 60 * 60 * 1000),
    },
    {
      id: 'demo_escrow_2',
      buyer: demoSeller,
      seller: buyerAddress,
      amount: 250,
      deadline: new Date(now.getTime() + 3 * 24 * 60 * 60 * 1000), // 3 days
      status: 'active',
      description: 'Smart contract audit service',
      createdAt: new Date(now.getTime() - 1 * 24 * 60 * 60 * 1000),
      updatedAt: new Date(now.getTime() - 1 * 24 * 60 * 60 * 1000),
    },
    {
      id: 'demo_escrow_3',
      buyer: buyerAddress,
      seller: generateMockAddress(),
      amount: 1000,
      deadline: new Date(now.getTime() - 1 * 24 * 60 * 60 * 1000), // expired
      status: 'completed',
      description: 'NFT artwork commission',
      createdAt: new Date(now.getTime() - 10 * 24 * 60 * 60 * 1000),
      updatedAt: new Date(now.getTime() - 3 * 24 * 60 * 60 * 1000),
    },
  ];

  const demoTransactions: EscrowTransaction[] = [
    {
      id: 'demo_tx_1',
      escrowId: 'demo_escrow_1',
      type: 'funded',
      from: buyerAddress,
      amount: 500,
      timestamp: new Date(now.getTime() - 2 * 24 * 60 * 60 * 1000),
      txHash: generateTxHash(),
    },
    {
      id: 'demo_tx_2',
      escrowId: 'demo_escrow_2',
      type: 'funded',
      from: demoSeller,
      amount: 250,
      timestamp: new Date(now.getTime() - 1 * 24 * 60 * 60 * 1000),
      txHash: generateTxHash(),
    },
    {
      id: 'demo_tx_3',
      escrowId: 'demo_escrow_3',
      type: 'funded',
      from: buyerAddress,
      amount: 1000,
      timestamp: new Date(now.getTime() - 10 * 24 * 60 * 60 * 1000),
      txHash: generateTxHash(),
    },
    {
      id: 'demo_tx_4',
      escrowId: 'demo_escrow_3',
      type: 'released',
      from: buyerAddress,
      to: demoEscrows[2].seller,
      amount: 1000,
      timestamp: new Date(now.getTime() - 3 * 24 * 60 * 60 * 1000),
      txHash: generateTxHash(),
    },
  ];

  saveEscrows(demoEscrows);
  saveTransactions(demoTransactions);
};
