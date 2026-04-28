import React, { useEffect, useState } from 'react';
import { Navigate, Link } from 'react-router-dom';
import { motion } from 'framer-motion';
import { Shield, Users as UsersIcon, FileText, AlertTriangle, ScrollText, Loader2, Plus, Trash2, ExternalLink } from 'lucide-react';
import { Card } from '@/components/ui/card';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { Badge } from '@/components/ui/badge';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Textarea } from '@/components/ui/textarea';
import { Table, TableBody, TableCell, TableHead, TableHeader, TableRow } from '@/components/ui/table';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import {
  AlertDialog, AlertDialogAction, AlertDialogCancel, AlertDialogContent,
  AlertDialogDescription, AlertDialogFooter, AlertDialogHeader, AlertDialogTitle, AlertDialogTrigger,
} from '@/components/ui/alert-dialog';
import { useUserRole } from '@/hooks/useUserRole';
import { adminApi, AdminUserRow } from '@/services/adminApi';
import { useToast } from '@/hooks/use-toast';
import { lovelaceToAda } from '@/services/lucidService';
import { format } from 'date-fns';

const formatAddr = (a?: string | null) => (a ? `${a.slice(0, 10)}…${a.slice(-6)}` : '—');

const Admin: React.FC = () => {
  const { isAdmin, isOwner, loading } = useUserRole();
  const { toast } = useToast();

  const [escrows, setEscrows] = useState<any[]>([]);
  const [users, setUsers] = useState<AdminUserRow[]>([]);
  const [auditLog, setAuditLog] = useState<any[]>([]);
  const [transactions, setTransactions] = useState<any[]>([]);
  const [dataLoading, setDataLoading] = useState(true);

  // Resolve dispute dialog state
  const [resolveTarget, setResolveTarget] = useState<any | null>(null);
  const [resolveOutcome, setResolveOutcome] = useState<'completed' | 'refunded'>('completed');
  const [resolveNote, setResolveNote] = useState('');
  const [evidenceMap, setEvidenceMap] = useState<Record<string, { messages: any[]; attachments: any[] }>>({});

  const loadAll = async () => {
    setDataLoading(true);
    try {
      const [e, u, a, t] = await Promise.all([
        adminApi.listAllEscrows(),
        adminApi.listUsers(),
        adminApi.listAuditLog(),
        adminApi.listAllTransactions(),
      ]);
      setEscrows(e);
      setUsers(u);
      setAuditLog(a);
      setTransactions(t);
    } catch (err) {
      toast({ variant: 'destructive', title: 'Failed to load admin data', description: err instanceof Error ? err.message : 'Unknown error' });
    } finally {
      setDataLoading(false);
    }
  };

  useEffect(() => {
    if (isAdmin) loadAll();
  }, [isAdmin]);

  // Lazy-load evidence for disputed escrows when the disputes tab is rendered
  useEffect(() => {
    const disputed = escrows.filter((e) => e.status === 'disputed');
    disputed.forEach((e) => {
      if (!evidenceMap[e.id]) {
        adminApi.listDisputeEvidence(e.id).then((ev) => {
          setEvidenceMap((m) => ({ ...m, [e.id]: ev }));
        }).catch(() => {});
      }
    });
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [escrows]);

  if (loading) {
    return (
      <div className="min-h-screen pt-24 flex items-center justify-center">
        <Loader2 className="h-8 w-8 animate-spin text-primary" />
      </div>
    );
  }

  if (!isAdmin) {
    return <Navigate to="/dashboard" replace />;
  }

  // Stats
  const totalVolume = escrows.reduce((s, e) => s + Number(e.amount || 0), 0);
  const activeCount = escrows.filter((e) => e.status === 'active').length;
  const disputedCount = escrows.filter((e) => e.status === 'disputed').length;
  const completedCount = escrows.filter((e) => e.status === 'completed').length;

  const handleGrant = async (userId: string, role: 'admin' | 'owner') => {
    try {
      await adminApi.grantRole(userId, role);
      toast({ title: 'Role granted', description: `User is now ${role}.` });
      await loadAll();
    } catch (err) {
      toast({ variant: 'destructive', title: 'Failed', description: err instanceof Error ? err.message : 'Unknown' });
    }
  };

  const handleRevoke = async (userId: string, role: 'admin' | 'owner') => {
    try {
      await adminApi.revokeRole(userId, role);
      toast({ title: 'Role revoked' });
      await loadAll();
    } catch (err) {
      toast({ variant: 'destructive', title: 'Failed', description: err instanceof Error ? err.message : 'Unknown' });
    }
  };

  const handleResolve = async () => {
    if (!resolveTarget) return;
    try {
      await adminApi.resolveDispute(resolveTarget.id, resolveOutcome, resolveNote);
      toast({ title: 'Dispute resolved', description: `Marked as ${resolveOutcome}.` });
      setResolveTarget(null);
      setResolveNote('');
      await loadAll();
    } catch (err) {
      toast({ variant: 'destructive', title: 'Failed', description: err instanceof Error ? err.message : 'Unknown' });
    }
  };

  return (
    <div className="min-h-screen pt-24 pb-16">
      <div className="container mx-auto px-4 max-w-7xl">
        <motion.div initial={{ opacity: 0, y: -10 }} animate={{ opacity: 1, y: 0 }} className="mb-8">
          <div className="flex items-center gap-3 mb-2">
            <div className="h-10 w-10 rounded-lg bg-gradient-primary flex items-center justify-center">
              <Shield className="h-5 w-5 text-primary-foreground" />
            </div>
            <div>
              <h1 className="text-3xl font-bold gradient-text">Admin Console</h1>
              <p className="text-sm text-muted-foreground">
                {isOwner ? 'Owner' : 'Admin'} controls — view all escrows, manage users, resolve disputes
              </p>
            </div>
          </div>
        </motion.div>

        {/* Stats */}
        <div className="grid grid-cols-2 md:grid-cols-4 gap-4 mb-8">
          <Card className="glass-card p-4">
            <div className="text-xs text-muted-foreground">Total Volume</div>
            <div className="text-2xl font-bold text-primary">{lovelaceToAda(BigInt(totalVolume)).toLocaleString()} ₳</div>
          </Card>
          <Card className="glass-card p-4">
            <div className="text-xs text-muted-foreground">Active</div>
            <div className="text-2xl font-bold">{activeCount}</div>
          </Card>
          <Card className="glass-card p-4">
            <div className="text-xs text-muted-foreground">Completed</div>
            <div className="text-2xl font-bold text-success">{completedCount}</div>
          </Card>
          <Card className="glass-card p-4">
            <div className="text-xs text-muted-foreground">Disputed</div>
            <div className="text-2xl font-bold text-destructive">{disputedCount}</div>
          </Card>
        </div>

        <Tabs defaultValue="escrows" className="w-full">
          <TabsList className="glass-card mb-6">
            <TabsTrigger value="escrows"><FileText className="h-4 w-4 mr-2" />Escrows</TabsTrigger>
            <TabsTrigger value="disputes"><AlertTriangle className="h-4 w-4 mr-2" />Disputes</TabsTrigger>
            <TabsTrigger value="users"><UsersIcon className="h-4 w-4 mr-2" />Users</TabsTrigger>
            <TabsTrigger value="audit"><ScrollText className="h-4 w-4 mr-2" />Audit Log</TabsTrigger>
          </TabsList>

          {dataLoading ? (
            <div className="flex justify-center py-12"><Loader2 className="h-6 w-6 animate-spin text-primary" /></div>
          ) : (
            <>
              <TabsContent value="escrows">
                <Card className="glass-card p-4 overflow-x-auto">
                  <Table>
                    <TableHeader>
                      <TableRow>
                        <TableHead>Status</TableHead>
                        <TableHead>Amount</TableHead>
                        <TableHead>Buyer</TableHead>
                        <TableHead>Seller</TableHead>
                        <TableHead>Deadline</TableHead>
                        <TableHead>Created</TableHead>
                        <TableHead></TableHead>
                      </TableRow>
                    </TableHeader>
                    <TableBody>
                      {escrows.map((e) => (
                        <TableRow key={e.id}>
                          <TableCell><Badge variant={e.status === 'disputed' ? 'destructive' : 'outline'}>{e.status}</Badge></TableCell>
                          <TableCell className="font-mono">{lovelaceToAda(BigInt(e.amount)).toLocaleString()} ₳</TableCell>
                          <TableCell className="font-mono text-xs">{formatAddr(e.buyer_address)}</TableCell>
                          <TableCell className="font-mono text-xs">{formatAddr(e.seller_address)}</TableCell>
                          <TableCell className="text-xs">{format(new Date(e.deadline), 'MMM d, yyyy')}</TableCell>
                          <TableCell className="text-xs">{format(new Date(e.created_at), 'MMM d, yyyy')}</TableCell>
                          <TableCell>
                            <Link to={`/escrow/${e.id}`}>
                              <Button size="sm" variant="ghost"><ExternalLink className="h-3 w-3" /></Button>
                            </Link>
                          </TableCell>
                        </TableRow>
                      ))}
                      {escrows.length === 0 && (
                        <TableRow><TableCell colSpan={7} className="text-center text-muted-foreground py-6">No escrows yet.</TableCell></TableRow>
                      )}
                    </TableBody>
                  </Table>
                </Card>
              </TabsContent>

              <TabsContent value="disputes">
                <Card className="glass-card p-4">
                  {escrows.filter((e) => e.status === 'disputed').length === 0 ? (
                    <p className="text-center text-muted-foreground py-8">No active disputes.</p>
                  ) : (
                    <div className="space-y-4">
                      {escrows.filter((e) => e.status === 'disputed').map((e) => {
                        const ev = evidenceMap[e.id];
                        return (
                          <div key={e.id} className="border border-destructive/30 rounded-lg p-4 bg-destructive/5">
                            <div className="flex justify-between items-start gap-4 flex-wrap">
                              <div className="flex-1 min-w-[200px]">
                                <div className="flex items-center gap-2 mb-2 flex-wrap">
                                  <Badge variant="destructive">Disputed</Badge>
                                  {e.dispute_reason_type && (
                                    <Badge variant="outline" className="capitalize">
                                      {String(e.dispute_reason_type).replace(/_/g, ' ')}
                                    </Badge>
                                  )}
                                  <span className="font-mono text-sm">{lovelaceToAda(BigInt(e.amount)).toLocaleString()} ₳</span>
                                </div>
                                <p className="text-xs text-muted-foreground mb-1">
                                  Flagged {e.disputed_at ? format(new Date(e.disputed_at), 'PPp') : '—'}
                                </p>
                                {e.dispute_reason && (
                                  <p className="text-sm bg-background/50 p-2 rounded mt-2">{e.dispute_reason}</p>
                                )}
                                <div className="text-xs text-muted-foreground mt-2 space-y-1">
                                  <div>Buyer: <span className="font-mono">{formatAddr(e.buyer_address)}</span></div>
                                  <div>Seller: <span className="font-mono">{formatAddr(e.seller_address)}</span></div>
                                </div>

                                {/* Evidence summary */}
                                <div className="mt-3 text-xs grid grid-cols-2 gap-2">
                                  <div className="bg-background/40 rounded p-2">
                                    <div className="font-semibold mb-1">📎 Attachments ({ev?.attachments.length ?? '…'})</div>
                                    {ev?.attachments.slice(0, 3).map((a: any) => (
                                      <div key={a.id} className="truncate text-muted-foreground">{a.file_name}</div>
                                    ))}
                                    {ev && ev.attachments.length === 0 && <div className="text-muted-foreground">None</div>}
                                  </div>
                                  <div className="bg-background/40 rounded p-2">
                                    <div className="font-semibold mb-1">💬 Messages ({ev?.messages.length ?? '…'})</div>
                                    {ev?.messages.slice(-2).map((m: any) => (
                                      <div key={m.id} className="truncate text-muted-foreground">{m.content}</div>
                                    ))}
                                    {ev && ev.messages.length === 0 && <div className="text-muted-foreground">None</div>}
                                  </div>
                                </div>
                              </div>
                              <div className="flex gap-2">
                                <Link to={`/escrow/${e.id}`}>
                                  <Button size="sm" variant="outline">View Full</Button>
                                </Link>
                                <Button size="sm" onClick={() => { setResolveTarget(e); setResolveOutcome('completed'); }}>Resolve</Button>
                              </div>
                            </div>
                          </div>
                        );
                      })}
                    </div>
                  )}
                </Card>
              </TabsContent>

              <TabsContent value="users">
                <Card className="glass-card p-4 overflow-x-auto">
                  <Table>
                    <TableHeader>
                      <TableRow>
                        <TableHead>User ID</TableHead>
                        <TableHead>Wallet</TableHead>
                        <TableHead>Roles</TableHead>
                        <TableHead>Joined</TableHead>
                        <TableHead className="text-right">Actions</TableHead>
                      </TableRow>
                    </TableHeader>
                    <TableBody>
                      {users.map((u) => {
                        const isUserAdmin = u.roles.includes('admin');
                        const isUserOwner = u.roles.includes('owner');
                        return (
                          <TableRow key={u.user_id}>
                            <TableCell className="font-mono text-xs">{u.user_id.slice(0, 8)}…</TableCell>
                            <TableCell className="font-mono text-xs">{formatAddr(u.wallet_address)}</TableCell>
                            <TableCell>
                              <div className="flex gap-1 flex-wrap">
                                {u.roles.length === 0 && <Badge variant="secondary">user</Badge>}
                                {u.roles.map((r) => (
                                  <Badge key={r} variant={r === 'owner' ? 'default' : 'outline'}>{r}</Badge>
                                ))}
                              </div>
                            </TableCell>
                            <TableCell className="text-xs">{format(new Date(u.created_at), 'MMM d, yyyy')}</TableCell>
                            <TableCell className="text-right space-x-1">
                              {!isUserAdmin && !isUserOwner && (
                                <Button size="sm" variant="outline" onClick={() => handleGrant(u.user_id, 'admin')}>
                                  <Plus className="h-3 w-3 mr-1" />Make Admin
                                </Button>
                              )}
                              {isUserAdmin && !isUserOwner && (
                                <Button size="sm" variant="outline" onClick={() => handleRevoke(u.user_id, 'admin')}>
                                  <Trash2 className="h-3 w-3 mr-1" />Revoke Admin
                                </Button>
                              )}
                              {isOwner && !isUserOwner && (
                                <Button size="sm" variant="ghost" onClick={() => handleGrant(u.user_id, 'owner')}>
                                  Make Owner
                                </Button>
                              )}
                              {isOwner && isUserOwner && (
                                <Button size="sm" variant="ghost" onClick={() => handleRevoke(u.user_id, 'owner')}>
                                  Revoke Owner
                                </Button>
                              )}
                            </TableCell>
                          </TableRow>
                        );
                      })}
                    </TableBody>
                  </Table>
                </Card>
              </TabsContent>

              <TabsContent value="audit">
                <Card className="glass-card p-4 overflow-x-auto">
                  <Table>
                    <TableHeader>
                      <TableRow>
                        <TableHead>When</TableHead>
                        <TableHead>Actor</TableHead>
                        <TableHead>Action</TableHead>
                        <TableHead>Target</TableHead>
                        <TableHead>Details</TableHead>
                      </TableRow>
                    </TableHeader>
                    <TableBody>
                      {auditLog.map((row) => (
                        <TableRow key={row.id}>
                          <TableCell className="text-xs whitespace-nowrap">{format(new Date(row.created_at), 'MMM d, HH:mm')}</TableCell>
                          <TableCell className="font-mono text-xs">{String(row.actor_user_id).slice(0, 8)}…</TableCell>
                          <TableCell><Badge variant="outline">{row.action}</Badge></TableCell>
                          <TableCell className="font-mono text-xs">{row.target_type}/{row.target_id ? String(row.target_id).slice(0, 8) + '…' : '—'}</TableCell>
                          <TableCell className="text-xs"><pre className="whitespace-pre-wrap max-w-[300px]">{JSON.stringify(row.metadata, null, 0)}</pre></TableCell>
                        </TableRow>
                      ))}
                      {auditLog.length === 0 && (
                        <TableRow><TableCell colSpan={5} className="text-center text-muted-foreground py-6">No actions logged yet.</TableCell></TableRow>
                      )}
                    </TableBody>
                  </Table>
                </Card>
              </TabsContent>
            </>
          )}
        </Tabs>

        {/* Resolve Dispute Dialog */}
        <AlertDialog open={!!resolveTarget} onOpenChange={(o) => !o && setResolveTarget(null)}>
          <AlertDialogContent className="glass-card">
            <AlertDialogHeader>
              <AlertDialogTitle>Resolve Dispute</AlertDialogTitle>
              <AlertDialogDescription>
                This updates the escrow status off-chain. On-chain funds still require buyer & seller wallet signatures.
              </AlertDialogDescription>
            </AlertDialogHeader>
            <div className="space-y-4 py-2">
              <div>
                <label className="text-sm font-medium mb-1 block">Outcome</label>
                <Select value={resolveOutcome} onValueChange={(v) => setResolveOutcome(v as any)}>
                  <SelectTrigger><SelectValue /></SelectTrigger>
                  <SelectContent>
                    <SelectItem value="completed">Mark as Completed (favor seller)</SelectItem>
                    <SelectItem value="refunded">Mark as Refunded (favor buyer)</SelectItem>
                    <SelectItem value="active">Reopen as Active</SelectItem>
                  </SelectContent>
                </Select>
              </div>
              <div>
                <label className="text-sm font-medium mb-1 block">Resolution note</label>
                <Textarea value={resolveNote} onChange={(e) => setResolveNote(e.target.value)} placeholder="Explain the decision…" rows={3} />
              </div>
            </div>
            <AlertDialogFooter>
              <AlertDialogCancel>Cancel</AlertDialogCancel>
              <AlertDialogAction onClick={handleResolve}>Confirm Resolution</AlertDialogAction>
            </AlertDialogFooter>
          </AlertDialogContent>
        </AlertDialog>
      </div>
    </div>
  );
};

export default Admin;
