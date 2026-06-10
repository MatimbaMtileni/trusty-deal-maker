import React, { useEffect, useState } from 'react';
import { Link } from 'react-router-dom';
import { motion } from 'framer-motion';
import { CheckCircle2, Loader2, AlertTriangle, ArrowLeft, RefreshCw } from 'lucide-react';
import { Button } from '@/components/ui/button';
import { supabase } from '@/integrations/supabase/client';

interface Counts {
  escrow_transactions: number;
  escrow_pending_release: number;
}

const TransactionsCleared: React.FC = () => {
  const [counts, setCounts] = useState<Counts | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  const verify = async () => {
    setLoading(true);
    setError(null);
    try {
      const [txRes, pendingRes] = await Promise.all([
        supabase.from('escrow_transactions').select('*', { count: 'exact', head: true }),
        supabase.from('escrow_pending_release').select('*', { count: 'exact', head: true }),
      ]);
      if (txRes.error) throw txRes.error;
      if (pendingRes.error) throw pendingRes.error;
      setCounts({
        escrow_transactions: txRes.count ?? 0,
        escrow_pending_release: pendingRes.count ?? 0,
      });
    } catch (e: any) {
      setError(e?.message ?? 'Failed to verify');
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => {
    verify();
  }, []);

  const allCleared =
    counts !== null &&
    counts.escrow_transactions === 0 &&
    counts.escrow_pending_release === 0;

  return (
    <main className="container mx-auto max-w-2xl px-4 py-16">
      <motion.div
        initial={{ opacity: 0, y: 12 }}
        animate={{ opacity: 1, y: 0 }}
        className="glass-card p-8 text-center"
      >
        {loading ? (
          <>
            <Loader2 className="mx-auto h-12 w-12 animate-spin text-primary" />
            <h1 className="mt-4 text-2xl font-bold">Verifying records…</h1>
            <p className="mt-2 text-muted-foreground">
              Checking on-chain and off-chain transaction tables.
            </p>
          </>
        ) : error ? (
          <>
            <AlertTriangle className="mx-auto h-12 w-12 text-destructive" />
            <h1 className="mt-4 text-2xl font-bold">Verification failed</h1>
            <p className="mt-2 text-muted-foreground">{error}</p>
          </>
        ) : allCleared ? (
          <>
            <motion.div
              initial={{ scale: 0.6, opacity: 0 }}
              animate={{ scale: 1, opacity: 1 }}
              transition={{ type: 'spring', stiffness: 200, damping: 15 }}
            >
              <CheckCircle2 className="mx-auto h-16 w-16 text-success" />
            </motion.div>
            <h1 className="mt-4 text-3xl font-bold">All transactions cleared</h1>
            <p className="mt-2 text-muted-foreground">
              Every on-chain and off-chain transaction record has been permanently
              removed from the database.
            </p>
            <div className="mt-6 grid grid-cols-2 gap-4 text-left">
              <div className="rounded-lg border border-border/50 bg-background/30 p-4">
                <p className="text-xs uppercase tracking-wide text-muted-foreground">
                  On-chain tx records
                </p>
                <p className="mt-1 text-2xl font-semibold">
                  {counts!.escrow_transactions}
                </p>
              </div>
              <div className="rounded-lg border border-border/50 bg-background/30 p-4">
                <p className="text-xs uppercase tracking-wide text-muted-foreground">
                  Pending release artifacts
                </p>
                <p className="mt-1 text-2xl font-semibold">
                  {counts!.escrow_pending_release}
                </p>
              </div>
            </div>
          </>
        ) : (
          <>
            <AlertTriangle className="mx-auto h-12 w-12 text-warning" />
            <h1 className="mt-4 text-2xl font-bold">Records still present</h1>
            <p className="mt-2 text-muted-foreground">
              Some transaction rows remain. See counts below.
            </p>
            <div className="mt-6 grid grid-cols-2 gap-4 text-left">
              <div className="rounded-lg border border-border/50 bg-background/30 p-4">
                <p className="text-xs uppercase tracking-wide text-muted-foreground">
                  escrow_transactions
                </p>
                <p className="mt-1 text-2xl font-semibold">
                  {counts!.escrow_transactions}
                </p>
              </div>
              <div className="rounded-lg border border-border/50 bg-background/30 p-4">
                <p className="text-xs uppercase tracking-wide text-muted-foreground">
                  escrow_pending_release
                </p>
                <p className="mt-1 text-2xl font-semibold">
                  {counts!.escrow_pending_release}
                </p>
              </div>
            </div>
          </>
        )}

        <div className="mt-8 flex items-center justify-center gap-3">
          <Button variant="outline" onClick={verify} disabled={loading}>
            <RefreshCw className="mr-2 h-4 w-4" />
            Re-verify
          </Button>
          <Button asChild>
            <Link to="/dashboard">
              <ArrowLeft className="mr-2 h-4 w-4" />
              Back to dashboard
            </Link>
          </Button>
        </div>
      </motion.div>
    </main>
  );
};

export default TransactionsCleared;
