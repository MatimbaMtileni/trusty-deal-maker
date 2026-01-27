import { supabase } from '@/integrations/supabase/client';

const FUNCTION_URL = `${import.meta.env.VITE_SUPABASE_URL}/functions/v1/escrow-transactions`;

interface CreateEscrowParams {
  buyer_address: string;
  seller_address: string;
  amount: number;
  deadline: string;
  description?: string;
  tx_hash: string;
}

interface EscrowActionParams {
  escrow_id: string;
  tx_hash: string;
}

export const escrowApi = {
  async createEscrow(params: CreateEscrowParams) {
    const { data: { session } } = await supabase.auth.getSession();
    if (!session) throw new Error('Not authenticated');

    const response = await fetch(FUNCTION_URL, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${session.access_token}`,
      },
      body: JSON.stringify({
        action: 'create',
        ...params,
      }),
    });

    if (!response.ok) {
      const error = await response.json();
      throw new Error(error.error || 'Failed to create escrow');
    }

    return response.json();
  },

  async releaseEscrow(params: EscrowActionParams) {
    const { data: { session } } = await supabase.auth.getSession();
    if (!session) throw new Error('Not authenticated');

    const response = await fetch(FUNCTION_URL, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${session.access_token}`,
      },
      body: JSON.stringify({
        action: 'release',
        ...params,
      }),
    });

    if (!response.ok) {
      const error = await response.json();
      throw new Error(error.error || 'Failed to release escrow');
    }

    return response.json();
  },

  async refundEscrow(params: EscrowActionParams) {
    const { data: { session } } = await supabase.auth.getSession();
    if (!session) throw new Error('Not authenticated');

    const response = await fetch(FUNCTION_URL, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${session.access_token}`,
      },
      body: JSON.stringify({
        action: 'refund',
        ...params,
      }),
    });

    if (!response.ok) {
      const error = await response.json();
      throw new Error(error.error || 'Failed to refund escrow');
    }

    return response.json();
  },

  async getEscrows() {
    const { data, error } = await supabase
      .from('escrows')
      .select('*')
      .order('created_at', { ascending: false });

    if (error) throw error;
    return data;
  },

  async getEscrowById(id: string) {
    const { data, error } = await supabase
      .from('escrows')
      .select('*')
      .eq('id', id)
      .maybeSingle();

    if (error) throw error;
    return data;
  },

  async getEscrowTransactions(escrowId: string) {
    const { data, error } = await supabase
      .from('escrow_transactions')
      .select('*')
      .eq('escrow_id', escrowId)
      .order('created_at', { ascending: true });

    if (error) throw error;
    return data;
  },

  async updateProfileWallet(walletAddress: string) {
    const { data: { user } } = await supabase.auth.getUser();
    if (!user) throw new Error('Not authenticated');

    const { error } = await supabase
      .from('profiles')
      .update({ wallet_address: walletAddress })
      .eq('user_id', user.id);

    if (error) throw error;
  },
};
