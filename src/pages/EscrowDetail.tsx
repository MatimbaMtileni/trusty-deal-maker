import React, { useState, useMemo } from 'react';
import { useParams, Link, useNavigate } from 'react-router-dom';
import { motion } from 'framer-motion';
import { format, formatDistanceToNow, isPast } from 'date-fns';
import {
  ArrowLeft,
  Clock,
  User,
  Coins,
  CheckCircle,
  RotateCcw,
  Loader2,
  AlertCircle,
  ExternalLink,
  Copy,
  Check,
} from 'lucide-react';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Separator } from '@/components/ui/separator';
import {
  AlertDialog,
  AlertDialogAction,
  AlertDialogCancel,
  AlertDialogContent,
  AlertDialogDescription,
  AlertDialogFooter,
  AlertDialogHeader,
  AlertDialogTitle,
  AlertDialogTrigger,
} from '@/components/ui/alert-dialog';
import { useWallet } from '@/contexts/WalletContext';
import { TransactionTimeline } from '@/components/escrow/TransactionTimeline';
import {
  getEscrowById,
  getTransactionsForEscrow,
  releaseEscrow,
  refundEscrow,
} from '@/services/mockBlockchain';
import { useToast } from '@/hooks/use-toast';
import { UserRole } from '@/types/escrow';

const statusConfig = {
  pending: { label: 'Pending', class: 'status-pending', icon: Clock },
  active: { label: 'Active', class: 'status-active', icon: Coins },
  completed: { label: 'Completed', class: 'status-completed', icon: CheckCircle },
  refunded: { label: 'Refunded', class: 'status-refunded', icon: RotateCcw },
  expired: { label: 'Expired', class: 'status-refunded', icon: Clock },
};

const formatAddress = (address: string) => {
  return `${address.slice(0, 10)}...${address.slice(-8)}`;
};

export const EscrowDetail: React.FC = () => {
  const { id } = useParams<{ id: string }>();
  const { wallet, refreshBalance } = useWallet();
  const navigate = useNavigate();
  const { toast } = useToast();
  
  const [isProcessing, setIsProcessing] = useState(false);
  const [copiedField, setCopiedField] = useState<string | null>(null);
  const [refreshKey, setRefreshKey] = useState(0);

  const escrow = useMemo(() => {
    if (!id) return null;
    return getEscrowById(id);
  }, [id, refreshKey]);

  const transactions = useMemo(() => {
    if (!id) return [];
    return getTransactionsForEscrow(id);
  }, [id, refreshKey]);

  const userRole: UserRole | null = useMemo(() => {
    if (!escrow || !wallet) return null;
    if (escrow.buyer === wallet.address) return 'buyer';
    if (escrow.seller === wallet.address) return 'seller';
    return null;
  }, [escrow, wallet]);

  const isDeadlinePassed = escrow ? isPast(escrow.deadline) : false;
  const canRelease = userRole === 'buyer' && escrow?.status === 'active';
  const canRefund = userRole === 'buyer' && escrow?.status === 'active' && isDeadlinePassed;

  const handleCopy = (text: string, field: string) => {
    navigator.clipboard.writeText(text);
    setCopiedField(field);
    setTimeout(() => setCopiedField(null), 2000);
  };

  const handleRelease = async () => {
    if (!escrow || !wallet) return;
    
    setIsProcessing(true);
    try {
      await releaseEscrow(escrow.id, wallet.address);
      toast({
        title: 'Funds Released!',
        description: `${escrow.amount} ADA has been sent to the seller`,
      });
      setRefreshKey(k => k + 1);
    } catch (error) {
      toast({
        variant: 'destructive',
        title: 'Release Failed',
        description: error instanceof Error ? error.message : 'Unknown error',
      });
    } finally {
      setIsProcessing(false);
    }
  };

  const handleRefund = async () => {
    if (!escrow || !wallet) return;
    
    setIsProcessing(true);
    try {
      await refundEscrow(escrow.id, wallet.address);
      await refreshBalance();
      toast({
        title: 'Refund Successful!',
        description: `${escrow.amount} ADA has been returned to your wallet`,
      });
      setRefreshKey(k => k + 1);
    } catch (error) {
      toast({
        variant: 'destructive',
        title: 'Refund Failed',
        description: error instanceof Error ? error.message : 'Unknown error',
      });
    } finally {
      setIsProcessing(false);
    }
  };

  if (!escrow) {
    return (
      <div className="min-h-screen pt-16">
        <div className="container mx-auto px-4 py-20">
          <motion.div
            initial={{ opacity: 0 }}
            animate={{ opacity: 1 }}
            className="text-center"
          >
            <AlertCircle className="h-16 w-16 text-muted-foreground mx-auto mb-4" />
            <h1 className="text-2xl font-bold mb-2">Escrow Not Found</h1>
            <p className="text-muted-foreground mb-6">
              The escrow contract you're looking for doesn't exist.
            </p>
            <Link to="/dashboard">
              <Button variant="outline" className="gap-2">
                <ArrowLeft className="h-4 w-4" />
                Back to Dashboard
              </Button>
            </Link>
          </motion.div>
        </div>
      </div>
    );
  }

  const status = statusConfig[escrow.status];
  const StatusIcon = status.icon;

  return (
    <div className="min-h-screen pt-16">
      <div className="container mx-auto px-4 py-8 max-w-4xl">
        {/* Header */}
        <motion.div
          initial={{ opacity: 0, y: -20 }}
          animate={{ opacity: 1, y: 0 }}
          className="mb-8"
        >
          <Link
            to="/dashboard"
            className="inline-flex items-center gap-2 text-muted-foreground hover:text-foreground mb-4 transition-colors"
          >
            <ArrowLeft className="h-4 w-4" />
            Back to Dashboard
          </Link>

          <div className="flex flex-col sm:flex-row items-start sm:items-center justify-between gap-4">
            <div>
              <div className="flex items-center gap-3 mb-2">
                <h1 className="text-3xl font-bold">Escrow Details</h1>
                <Badge variant="outline" className={status.class}>
                  <StatusIcon className="h-3 w-3 mr-1" />
                  {status.label}
                </Badge>
              </div>
              <p className="text-sm text-muted-foreground font-mono">{escrow.id}</p>
            </div>

            {userRole && (
              <Badge variant="outline" className="text-muted-foreground">
                You are the {userRole === 'buyer' ? '🛒 Buyer' : '💰 Seller'}
              </Badge>
            )}
          </div>
        </motion.div>

        <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
          {/* Main Info */}
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ delay: 0.1 }}
            className="lg:col-span-2 space-y-6"
          >
            {/* Amount Card */}
            <div className="glass-card-glow p-6">
              <p className="text-sm text-muted-foreground mb-2">Escrow Amount</p>
              <p className="text-4xl font-bold gradient-text">{escrow.amount.toLocaleString()} ₳</p>
              {escrow.description && (
                <p className="mt-4 text-muted-foreground">{escrow.description}</p>
              )}
            </div>

            {/* Parties */}
            <div className="glass-card p-6 space-y-4">
              <h3 className="font-semibold flex items-center gap-2">
                <User className="h-4 w-4 text-primary" />
                Parties
              </h3>

              <div className="space-y-3">
                <div className="flex items-center justify-between p-3 rounded-lg bg-muted/30">
                  <div>
                    <p className="text-sm text-muted-foreground">Buyer</p>
                    <p className="font-mono text-sm">{formatAddress(escrow.buyer)}</p>
                  </div>
                  <Button
                    variant="ghost"
                    size="icon"
                    onClick={() => handleCopy(escrow.buyer, 'buyer')}
                  >
                    {copiedField === 'buyer' ? (
                      <Check className="h-4 w-4 text-success" />
                    ) : (
                      <Copy className="h-4 w-4" />
                    )}
                  </Button>
                </div>

                <div className="flex items-center justify-between p-3 rounded-lg bg-muted/30">
                  <div>
                    <p className="text-sm text-muted-foreground">Seller</p>
                    <p className="font-mono text-sm">{formatAddress(escrow.seller)}</p>
                  </div>
                  <Button
                    variant="ghost"
                    size="icon"
                    onClick={() => handleCopy(escrow.seller, 'seller')}
                  >
                    {copiedField === 'seller' ? (
                      <Check className="h-4 w-4 text-success" />
                    ) : (
                      <Copy className="h-4 w-4" />
                    )}
                  </Button>
                </div>
              </div>
            </div>

            {/* Transaction History */}
            <div className="glass-card p-6">
              <h3 className="font-semibold mb-4 flex items-center gap-2">
                <Clock className="h-4 w-4 text-primary" />
                Transaction History
              </h3>
              <TransactionTimeline transactions={transactions} />
            </div>
          </motion.div>

          {/* Sidebar */}
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ delay: 0.2 }}
            className="space-y-6"
          >
            {/* Deadline */}
            <div className="glass-card p-6">
              <h3 className="font-semibold mb-4 flex items-center gap-2">
                <Clock className={`h-4 w-4 ${isDeadlinePassed ? 'text-destructive' : 'text-warning'}`} />
                Deadline
              </h3>
              <p className="text-2xl font-bold mb-1">
                {format(escrow.deadline, 'MMM d, yyyy')}
              </p>
              <p className="text-sm text-muted-foreground mb-2">
                {format(escrow.deadline, 'HH:mm')}
              </p>
              <p className={`text-sm ${isDeadlinePassed ? 'text-destructive' : 'text-warning'}`}>
                {isDeadlinePassed
                  ? `Expired ${formatDistanceToNow(escrow.deadline)} ago`
                  : `${formatDistanceToNow(escrow.deadline)} remaining`}
              </p>
            </div>

            {/* Actions */}
            {escrow.status === 'active' && userRole === 'buyer' && (
              <div className="glass-card p-6 space-y-4">
                <h3 className="font-semibold">Actions</h3>

                {/* Release Button */}
                <AlertDialog>
                  <AlertDialogTrigger asChild>
                    <Button
                      className="w-full btn-gradient gap-2"
                      disabled={!canRelease || isProcessing}
                    >
                      {isProcessing ? (
                        <Loader2 className="h-4 w-4 animate-spin" />
                      ) : (
                        <CheckCircle className="h-4 w-4" />
                      )}
                      Release Funds
                    </Button>
                  </AlertDialogTrigger>
                  <AlertDialogContent className="glass-card">
                    <AlertDialogHeader>
                      <AlertDialogTitle>Release Funds to Seller?</AlertDialogTitle>
                      <AlertDialogDescription>
                        This will transfer {escrow.amount} ADA to the seller. This action cannot be undone.
                      </AlertDialogDescription>
                    </AlertDialogHeader>
                    <AlertDialogFooter>
                      <AlertDialogCancel>Cancel</AlertDialogCancel>
                      <AlertDialogAction onClick={handleRelease} className="btn-gradient">
                        Confirm Release
                      </AlertDialogAction>
                    </AlertDialogFooter>
                  </AlertDialogContent>
                </AlertDialog>

                {/* Refund Button */}
                <AlertDialog>
                  <AlertDialogTrigger asChild>
                    <Button
                      variant="outline"
                      className="w-full gap-2"
                      disabled={!canRefund || isProcessing}
                    >
                      <RotateCcw className="h-4 w-4" />
                      Request Refund
                    </Button>
                  </AlertDialogTrigger>
                  <AlertDialogContent className="glass-card">
                    <AlertDialogHeader>
                      <AlertDialogTitle>Request Refund?</AlertDialogTitle>
                      <AlertDialogDescription>
                        This will return {escrow.amount} ADA to your wallet. This action cannot be undone.
                      </AlertDialogDescription>
                    </AlertDialogHeader>
                    <AlertDialogFooter>
                      <AlertDialogCancel>Cancel</AlertDialogCancel>
                      <AlertDialogAction onClick={handleRefund}>
                        Confirm Refund
                      </AlertDialogAction>
                    </AlertDialogFooter>
                  </AlertDialogContent>
                </AlertDialog>

                {!canRefund && (
                  <p className="text-xs text-muted-foreground">
                    💡 Refund available after deadline passes
                  </p>
                )}
              </div>
            )}

            {escrow.status === 'active' && userRole === 'seller' && (
              <div className="glass-card p-6">
                <div className="flex items-start gap-3">
                  <Clock className="h-5 w-5 text-primary flex-shrink-0 mt-0.5" />
                  <div>
                    <h3 className="font-semibold mb-1">Awaiting Release</h3>
                    <p className="text-sm text-muted-foreground">
                      The buyer will release funds once satisfied. You'll receive {escrow.amount} ADA.
                    </p>
                  </div>
                </div>
              </div>
            )}

            {/* Completed/Refunded Status */}
            {(escrow.status === 'completed' || escrow.status === 'refunded') && (
              <div className="glass-card p-6">
                <div className="flex items-start gap-3">
                  {escrow.status === 'completed' ? (
                    <CheckCircle className="h-5 w-5 text-success flex-shrink-0" />
                  ) : (
                    <RotateCcw className="h-5 w-5 text-warning flex-shrink-0" />
                  )}
                  <div>
                    <h3 className="font-semibold mb-1">
                      {escrow.status === 'completed' ? 'Transaction Complete' : 'Funds Refunded'}
                    </h3>
                    <p className="text-sm text-muted-foreground">
                      {escrow.status === 'completed'
                        ? 'Funds have been released to the seller.'
                        : 'Funds have been returned to the buyer.'}
                    </p>
                  </div>
                </div>
              </div>
            )}

            {/* Dates */}
            <div className="glass-card p-6">
              <h3 className="font-semibold mb-4">Timeline</h3>
              <div className="space-y-3 text-sm">
                <div className="flex justify-between">
                  <span className="text-muted-foreground">Created</span>
                  <span>{format(escrow.createdAt, 'MMM d, yyyy HH:mm')}</span>
                </div>
                <div className="flex justify-between">
                  <span className="text-muted-foreground">Last Updated</span>
                  <span>{format(escrow.updatedAt, 'MMM d, yyyy HH:mm')}</span>
                </div>
              </div>
            </div>
          </motion.div>
        </div>
      </div>
    </div>
  );
};

export default EscrowDetail;
