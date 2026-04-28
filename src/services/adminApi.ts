import { supabase } from '@/integrations/supabase/client';

export type AppRole = 'owner' | 'admin' | 'user';

export type DisputeReasonType =
  | 'item_not_received'
  | 'not_as_described'
  | 'payment_issue'
  | 'communication_breakdown'
  | 'other';

export const DISPUTE_REASON_LABELS: Record<DisputeReasonType, string> = {
  item_not_received: 'Item / service not received',
  not_as_described: 'Not as described',
  payment_issue: 'Payment issue',
  communication_breakdown: 'Communication breakdown',
  other: 'Other',
};

export interface AdminUserRow {
  user_id: string;
  display_name: string | null;
  wallet_address: string | null;
  created_at: string;
  roles: AppRole[];
}

export const adminApi = {
  async listAllEscrows() {
    const { data, error } = await supabase
      .from('escrows')
      .select('*')
      .order('created_at', { ascending: false });
    if (error) throw error;
    return data ?? [];
  },

  async listAllTransactions(limit = 200) {
    const { data, error } = await supabase
      .from('escrow_transactions')
      .select('*')
      .order('created_at', { ascending: false })
      .limit(limit);
    if (error) throw error;
    return data ?? [];
  },

  async listUsers(): Promise<AdminUserRow[]> {
    const { data, error } = await supabase
      .from('admin_users_view' as any)
      .select('*')
      .order('created_at', { ascending: false });
    if (error) throw error;
    return (data ?? []) as unknown as AdminUserRow[];
  },

  async listAuditLog(limit = 200) {
    const { data, error } = await supabase
      .from('admin_audit_log')
      .select('*')
      .order('created_at', { ascending: false })
      .limit(limit);
    if (error) throw error;
    return data ?? [];
  },

  async grantRole(targetUserId: string, role: 'admin' | 'owner') {
    const { data: { user } } = await supabase.auth.getUser();
    if (!user) throw new Error('Not authenticated');
    const { error } = await supabase
      .from('user_roles')
      .insert({ user_id: targetUserId, role, granted_by: user.id });
    if (error) throw error;
    await this.logAction('grant_role', 'user', targetUserId, { role });
  },

  async revokeRole(targetUserId: string, role: 'admin' | 'owner') {
    const { error } = await supabase
      .from('user_roles')
      .delete()
      .eq('user_id', targetUserId)
      .eq('role', role);
    if (error) throw error;
    await this.logAction('revoke_role', 'user', targetUserId, { role });
  },

  async flagDispute(escrowId: string, reasonType: DisputeReasonType, details: string) {
    const { data: { user } } = await supabase.auth.getUser();
    if (!user) throw new Error('Not authenticated');
    const { error } = await supabase
      .from('escrows')
      .update({
        status: 'disputed' as const,
        disputed_at: new Date().toISOString(),
        disputed_by: user.id,
        dispute_reason: details,
        dispute_reason_type: reasonType,
      } as any)
      .eq('id', escrowId);
    if (error) throw error;
    await this.logAction('flag_dispute', 'escrow', escrowId, { reasonType, details });
  },

  async listDisputeEvidence(escrowId: string) {
    const [msgs, atts] = await Promise.all([
      supabase.from('escrow_messages').select('*').eq('escrow_id', escrowId).order('created_at', { ascending: true }),
      supabase.from('escrow_attachments').select('*').eq('escrow_id', escrowId).order('created_at', { ascending: true }),
    ]);
    if (msgs.error) throw msgs.error;
    if (atts.error) throw atts.error;
    return { messages: msgs.data ?? [], attachments: atts.data ?? [] };
  },

  async resolveDispute(
    escrowId: string,
    resolution: 'completed' | 'refunded' | 'active',
    note: string,
  ) {
    const { data: { user } } = await supabase.auth.getUser();
    if (!user) throw new Error('Not authenticated');
    const { error } = await supabase
      .from('escrows')
      .update({
        status: resolution,
        resolved_by: user.id,
        resolved_at: new Date().toISOString(),
        resolution_note: note,
      })
      .eq('id', escrowId);
    if (error) throw error;
    await this.logAction('resolve_dispute', 'escrow', escrowId, { resolution, note });
  },

  async logAction(
    action: string,
    targetType: string | null,
    targetId: string | null,
    metadata: Record<string, unknown> = {},
  ) {
    const { data: { user } } = await supabase.auth.getUser();
    if (!user) return;
    await supabase.from('admin_audit_log').insert({
      actor_user_id: user.id,
      action,
      target_type: targetType,
      target_id: targetId,
      metadata: metadata as any,
    });
  },
};
