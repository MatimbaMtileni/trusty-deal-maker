// ============================================================================
// TxStatusPanel — Visualizes the 4-phase lifecycle of a release or refund
// transaction: Built → Signed → Submitted → Confirmed.
// ============================================================================

import React from 'react';
import { motion } from 'framer-motion';
import {
  Wrench,
  PenLine,
  Send,
  CheckCircle2,
  Loader2,
  AlertCircle,
  ExternalLink,
} from 'lucide-react';
import { Progress } from '@/components/ui/progress';
import { Button } from '@/components/ui/button';
import { getExplorerUrl } from '@/services/cardano';

export type TxPhase = 'idle' | 'built' | 'signed' | 'submitted' | 'confirmed' | 'error';
export type TxKind = 'release' | 'refund';

interface TxStatusPanelProps {
  kind: TxKind;
  phase: TxPhase;
  txHash?: string | null;
  confirmations?: number;
  requiredConfirmations?: number;
  error?: string | null;
  onClose?: () => void;
}

const STEPS: { id: Exclude<TxPhase, 'idle' | 'error'>; label: string; icon: React.ElementType; hint: string }[] = [
  { id: 'built', label: 'Built', icon: Wrench, hint: 'Transaction constructed off-chain' },
  { id: 'signed', label: 'Signed', icon: PenLine, hint: 'Authorized in your wallet' },
  { id: 'submitted', label: 'Submitted', icon: Send, hint: 'Broadcast to the Cardano network' },
  { id: 'confirmed', label: 'Confirmed', icon: CheckCircle2, hint: 'Finalized on-chain' },
];

const phaseIndex = (p: TxPhase): number => {
  if (p === 'idle') return -1;
  if (p === 'error') return -1;
  return STEPS.findIndex((s) => s.id === p);
};

export const TxStatusPanel: React.FC<TxStatusPanelProps> = ({
  kind,
  phase,
  txHash,
  confirmations = 0,
  requiredConfirmations = 1,
  error,
  onClose,
}) => {
  if (phase === 'idle') return null;

  const current = phaseIndex(phase);
  const title = kind === 'release' ? 'Release Transaction' : 'Refund Transaction';
  const confirmProgress =
    phase === 'confirmed'
      ? 100
      : phase === 'submitted'
        ? Math.min(99, Math.round((confirmations / Math.max(1, requiredConfirmations)) * 100))
        : 0;

  return (
    <motion.div
      initial={{ opacity: 0, y: 8 }}
      animate={{ opacity: 1, y: 0 }}
      className="glass-card p-5 space-y-4"
    >
      <div className="flex items-center justify-between">
        <div>
          <h3 className="font-display font-semibold">{title}</h3>
          <p className="text-xs text-muted-foreground">
            {phase === 'error'
              ? 'Something went wrong'
              : phase === 'confirmed'
                ? 'Funds settled on-chain'
                : `In progress · ${STEPS[Math.max(0, current)]?.hint}`}
          </p>
        </div>
        {txHash && (
          <Button
            variant="ghost"
            size="icon"
            className="h-8 w-8"
            onClick={() => window.open(getExplorerUrl(txHash, 'tx'), '_blank')}
            title="View on explorer"
          >
            <ExternalLink className="h-4 w-4" />
          </Button>
        )}
      </div>

      {/* Stepper */}
      <ol className="grid grid-cols-4 gap-2">
        {STEPS.map((step, idx) => {
          const Icon = step.icon;
          const isDone = phase !== 'error' && idx < current;
          const isActive = phase !== 'error' && idx === current;
          const isFinalDone = phase === 'confirmed' && idx === STEPS.length - 1;

          return (
            <li key={step.id} className="flex flex-col items-center text-center gap-2">
              <div
                className={[
                  'flex h-10 w-10 items-center justify-center rounded-full border transition-colors',
                  isDone || isFinalDone
                    ? 'bg-success/15 border-success/40 text-success'
                    : isActive
                      ? 'bg-primary/15 border-primary/50 text-primary neon-glow'
                      : 'bg-muted/30 border-border text-muted-foreground',
                ].join(' ')}
              >
                {isActive && phase === 'submitted' ? (
                  <Loader2 className="h-4 w-4 animate-spin" />
                ) : (
                  <Icon className="h-4 w-4" />
                )}
              </div>
              <span
                className={`text-[11px] font-medium ${
                  isDone || isFinalDone || isActive ? 'text-foreground' : 'text-muted-foreground'
                }`}
              >
                {step.label}
              </span>
            </li>
          );
        })}
      </ol>

      {/* Connector progress bar reflecting overall lifecycle */}
      <Progress
        value={
          phase === 'confirmed'
            ? 100
            : phase === 'submitted'
              ? 66 + confirmProgress / 3
              : phase === 'signed'
                ? 50
                : phase === 'built'
                  ? 25
                  : 0
        }
        className="h-1.5"
      />

      {/* Confirmation counter when submitted */}
      {phase === 'submitted' && (
        <div className="flex justify-between text-xs text-muted-foreground">
          <span>Awaiting confirmations</span>
          <span className="font-mono">
            {confirmations}/{requiredConfirmations}
          </span>
        </div>
      )}

      {/* Tx hash */}
      {txHash && (
        <p className="text-[11px] text-muted-foreground font-mono break-all">
          {txHash}
        </p>
      )}

      {/* Error */}
      {phase === 'error' && error && (
        <div className="flex items-start gap-2 text-destructive text-sm">
          <AlertCircle className="h-4 w-4 mt-0.5 shrink-0" />
          <span>{error}</span>
        </div>
      )}

      {(phase === 'confirmed' || phase === 'error') && onClose && (
        <Button variant="outline" size="sm" className="w-full" onClick={onClose}>
          Close
        </Button>
      )}
    </motion.div>
  );
};
