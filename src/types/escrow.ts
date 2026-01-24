export type EscrowStatus = 'pending' | 'active' | 'completed' | 'refunded' | 'expired';

export type UserRole = 'buyer' | 'seller';

export interface EscrowDatum {
  id: string;
  buyer: string;
  seller: string;
  amount: number; // in ADA
  deadline: Date;
  status: EscrowStatus;
  description?: string;
  createdAt: Date;
  updatedAt: Date;
}

export interface EscrowTransaction {
  id: string;
  escrowId: string;
  type: 'created' | 'funded' | 'released' | 'refunded' | 'expired';
  from: string;
  to?: string;
  amount?: number;
  timestamp: Date;
  txHash: string;
}

export interface CreateEscrowParams {
  seller: string;
  amount: number;
  deadline: Date;
  description?: string;
}

export interface WalletInfo {
  name: string;
  icon: string;
  address: string;
  balance: number;
  connected: boolean;
}

export type WalletType = 'nami' | 'lace' | 'eternl';

export interface WalletOption {
  id: WalletType;
  name: string;
  icon: string;
  description: string;
}
