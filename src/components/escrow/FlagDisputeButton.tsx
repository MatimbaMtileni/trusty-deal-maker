import React, { useState } from 'react';
import { AlertTriangle } from 'lucide-react';
import { Button } from '@/components/ui/button';
import { Textarea } from '@/components/ui/textarea';
import { Label } from '@/components/ui/label';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import {
  AlertDialog, AlertDialogAction, AlertDialogCancel, AlertDialogContent,
  AlertDialogDescription, AlertDialogFooter, AlertDialogHeader, AlertDialogTitle, AlertDialogTrigger,
} from '@/components/ui/alert-dialog';
import { adminApi, DisputeReasonType } from '@/services/adminApi';
import { useToast } from '@/hooks/use-toast';

interface Props {
  escrowId: string;
  onFlagged?: () => void;
  disabled?: boolean;
}

const REASON_LABELS: Record<DisputeReasonType, string> = {
  item_not_received: 'Item / service not received',
  not_as_described: 'Not as described',
  payment_issue: 'Payment issue',
  communication_breakdown: 'Communication breakdown',
  other: 'Other',
};

export const FlagDisputeButton: React.FC<Props> = ({ escrowId, onFlagged, disabled }) => {
  const [open, setOpen] = useState(false);
  const [reasonType, setReasonType] = useState<DisputeReasonType>('item_not_received');
  const [details, setDetails] = useState('');
  const [submitting, setSubmitting] = useState(false);
  const { toast } = useToast();

  const handleSubmit = async () => {
    if (!details.trim() || details.trim().length < 10) {
      toast({ variant: 'destructive', title: 'More detail required', description: 'Please describe the issue (min 10 characters).' });
      return;
    }
    setSubmitting(true);
    try {
      await adminApi.flagDispute(escrowId, reasonType, details.trim());
      toast({ title: 'Dispute flagged', description: 'An admin will review this escrow. Funds remain locked.' });
      setOpen(false);
      setDetails('');
      setReasonType('item_not_received');
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
        <Button variant="outline" size="sm" disabled={disabled} className="w-full border-destructive/40 text-destructive hover:bg-destructive/10">
          <AlertTriangle className="h-4 w-4 mr-2" />
          Flag Dispute
        </Button>
      </AlertDialogTrigger>
      <AlertDialogContent className="glass-card">
        <AlertDialogHeader>
          <AlertDialogTitle>Flag this escrow as disputed</AlertDialogTitle>
          <AlertDialogDescription>
            This freezes release and refund actions until an admin resolves it. Funds remain locked on-chain until both parties sign.
          </AlertDialogDescription>
        </AlertDialogHeader>
        <div className="space-y-4 py-2">
          <div>
            <Label className="mb-1.5 block">Reason</Label>
            <Select value={reasonType} onValueChange={(v) => setReasonType(v as DisputeReasonType)}>
              <SelectTrigger><SelectValue /></SelectTrigger>
              <SelectContent>
                {(Object.keys(REASON_LABELS) as DisputeReasonType[]).map((k) => (
                  <SelectItem key={k} value={k}>{REASON_LABELS[k]}</SelectItem>
                ))}
              </SelectContent>
            </Select>
          </div>
          <div>
            <Label className="mb-1.5 block">Describe what happened</Label>
            <Textarea
              value={details}
              onChange={(e) => setDetails(e.target.value)}
              placeholder="Be specific. Reference dates, amounts, messages, or attached evidence…"
              rows={4}
            />
            <p className="text-xs text-muted-foreground mt-1">Tip: upload supporting files in the Documents section before flagging.</p>
          </div>
        </div>
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
