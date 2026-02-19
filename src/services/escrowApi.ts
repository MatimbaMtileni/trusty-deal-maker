import { supabase } from '@/integrations/supabase/client';
import { 
  getScriptBase64, 
  getScriptAddress, 
  isScriptDeployed,
  getExpectedScriptHash,
  getComputedScriptHash,
  isScriptHashVerified,
} from './cardano/scriptRegistry';

const FUNCTION_URL = `${import.meta.env.VITE_SUPABASE_URL}/functions/v1/escrow-transactions`;

/**
 * Verify that the Plutus script is properly configured
 */
function verifyScriptConfiguration(): void {
  if (!isScriptDeployed()) {
    const scriptBase64 = getScriptBase64();
    const scriptAddress = getScriptAddress();
    
    const missingVars = [];
    if (!scriptBase64) missingVars.push('VITE_ESCROW_SCRIPT_BASE64');
    if (!scriptAddress) missingVars.push('VITE_ESCROW_SCRIPT_ADDRESS');
    
    throw new Error(
      `Plutus escrow script not configured. Missing environment variables: ${missingVars.join(', ')}`
    );
  }

  const expectedHash = getExpectedScriptHash();
  if (!expectedHash) {
    throw new Error('Plutus script hash not configured. Missing environment variable: VITE_ESCROW_SCRIPT_HASH');
  }

  if (!isScriptHashVerified()) {
    const computedHash = getComputedScriptHash();
    throw new Error(
      `Plutus script hash mismatch. Expected ${expectedHash}, got ${computedHash || 'unknown'}`
    );
  }
}

interface CreateEscrowParams {
  buyer_address: string;
  seller_address: string;
  amount: number;
  deadline: string;
  description?: string;
  tx_hash: string;
  requires_multi_sig?: boolean;
  utxo_tx_hash?: string;
  utxo_output_index?: number;
}

interface EscrowActionParams {
  escrow_id: string;
  tx_hash: string;
}

 interface SignEscrowParams {
   escrow_id: string;
   signer_address: string;
 }
 
export const escrowApi = {
  async createEscrow(params: CreateEscrowParams) {
    verifyScriptConfiguration();
    
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
    verifyScriptConfiguration();
    
    const { data: { session } } = await supabase.auth.getSession();
    if (!session) throw new Error('Not authenticated');

    try {
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
        let errorMsg = 'Failed to release escrow';
        try {
          const error = await response.json();
          errorMsg = error.error || error.message || errorMsg;
        } catch {
          errorMsg = `Server error (${response.status}): ${response.statusText}`;
        }
        throw new Error(errorMsg);
      }

      return response.json();
    } catch (error) {
      if (error instanceof Error) {
        throw error;
      }
      throw new Error('Failed to release escrow: Unknown error');
    }
  },

  async refundEscrow(params: EscrowActionParams) {
    verifyScriptConfiguration();
    
    const { data: { session } } = await supabase.auth.getSession();
    if (!session) throw new Error('Not authenticated');

    try {
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
        let errorMsg = 'Failed to refund escrow';
        try {
          const error = await response.json();
          errorMsg = error.error || error.message || errorMsg;
        } catch {
          errorMsg = `Server error (${response.status}): ${response.statusText}`;
        }
        throw new Error(errorMsg);
      }

      return response.json();
    } catch (error) {
      if (error instanceof Error) {
        throw error;
      }
      throw new Error('Failed to refund escrow: Unknown error');
    }
  },

   async signEscrow(params: SignEscrowParams) {
     const { data: { session } } = await supabase.auth.getSession();
     if (!session) throw new Error('Not authenticated');
 
     const response = await fetch(FUNCTION_URL, {
       method: 'POST',
       headers: {
         'Content-Type': 'application/json',
         'Authorization': `Bearer ${session.access_token}`,
       },
       body: JSON.stringify({
         action: 'sign',
         ...params,
       }),
     });
 
     if (!response.ok) {
       const error = await response.json();
       throw new Error(error.error || 'Failed to sign escrow');
     }
 
     return response.json();
   },
 
   async getMultiSigStatus(escrowId: string) {
     const { data: { session } } = await supabase.auth.getSession();
     if (!session) throw new Error('Not authenticated');
 
     const response = await fetch(FUNCTION_URL, {
       method: 'POST',
       headers: {
         'Content-Type': 'application/json',
         'Authorization': `Bearer ${session.access_token}`,
       },
       body: JSON.stringify({
         action: 'get_multisig_status',
         escrow_id: escrowId,
       }),
     });
 
     if (!response.ok) {
       const error = await response.json();
       throw new Error(error.error || 'Failed to get multi-sig status');
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

  /**
   * Get Plutus script configuration (for verification)
   */
  getScriptConfig() {
    return {
      scriptBase64: getScriptBase64(),
      scriptAddress: getScriptAddress(),
      isDeployed: isScriptDeployed(),
    };
  },
};
