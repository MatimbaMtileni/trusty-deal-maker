import React, { useState } from 'react';
import { AlertTriangle } from 'lucide-react';
import { Button } from '@/components/ui/button';
import { Textarea } from '@/components/ui/textarea';
import {
  AlertDialog, AlertDialogAction, AlertDialogCancel, AlertDialogContent,
  AlertDialogDescription, AlertDialogFooter, AlertDialogHeader, AlertDialogTitle, AlertDialogTrigger,
} from '@/components/ui/alert-dialog';
import { adminApi } from '@/services/adminApi';
import { useToast } from '@/hooks/use-toast';

interface Props {
  escrowId: string;
  onFlagged?: () => void;
  disabled?: boolean;
}

export const FlagDisputeButton: React.FC<Props> = ({ escrowId, onFlagged, disabled }) => {
  const [open, setOpen] = useState(false);
  const [reason, setReason] = useState('');
  const [submitting, setSubmitting] = useState(false);
  const { toast } = useToast();

  const handleSubmit = async () => {
    if (!reason.trim()) {
      toast({ variant: 'destructive', title: 'Reason required', description: 'Please describe the dispute.' });
      return;
    }
    setSubmitting(true);
    try {
      await adminApi.flagDispute(escrowId, reason.trim());
      toast({ title: 'Dispute flagged', description: 'An admin will review this escrow.' });
      setOpen(false);
      setReason('');
      onFlagged?.();
    } catch (err) {
      toast({ variant: 'destructive', title: 'Failed to flag', description: err instanceof Error ? err.message : 'Unknown' });
    } finally {
      setSubmitting(false);
    }
  };

  return (
    <AlertDialog open={open} onOpenChange={setOpen}>
      <AlertDialogTrigger asChild>
        <Button variant="outline" size="sm" disabled={disabled} className="border-destructive/40 text-destructive hover:bg-destructive/10">
          <AlertTriangle className="h-4 w-4 mr-2" />
          Flag Dispute
        </Button>
      </AlertDialogTrigger>
      <AlertDialogContent className="glass-card">
        <AlertDialogHeader>
          <AlertDialogTitle>Flag this escrow as disputed</AlertDialogTitle>
          <AlertDialogDescription>
            This freezes normal release/refund actions until an admin reviews. Funds remain locked on-chain until both parties sign.
          </AlertDialogDescription>
        </AlertDialogHeader>
        <Textarea
          value={reason}
          onChange={(e) => setReason(e.target.value)}
          placeholder="Describe what went wrong…"
          rows={4}
        />
        <AlertDialogFooter>
          <AlertDialogCancel>Cancel</AlertDialogCancel>
          <AlertDialogAction onClick={handleSubmit} disabled={submitting}>
            {submitting ? 'Flagging…' : 'Flag Dispute'}
          </AlertDialogAction>
        </AlertDialogFooter>
      </AlertDialogContent>
    </AlertDialog>
  );
};
