import React, { useState, useEffect, useMemo } from 'react';
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
  Copy,
  Check,
} from 'lucide-react';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
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
import { useAuth } from '@/contexts/AuthContext';
import { TransactionTimeline } from '@/components/escrow/TransactionTimeline';
import { EscrowChat } from '@/components/escrow/EscrowChat';
import { EscrowAttachments } from '@/components/escrow/EscrowAttachments';
import { EscrowQRShare } from '@/components/escrow/EscrowQRShare';
import { escrowApi } from '@/services/escrowApi';
import { lovelaceToAda } from '@/services/lucidService';
import { executeEscrowRelease, executeEscrowRefund } from '@/services/cardano/txBuilder';
import { useToast } from '@/hooks/use-toast';
import { UserRole, EscrowTransaction } from '@/types/escrow';

const statusConfig = {
  pending: { label: 'Pending', class: 'status-pending', icon: Clock },
  active: { label: 'Active', class: 'status-active', icon: Coins },
  completed: { label: 'Completed', class: 'status-completed', icon: CheckCircle },
  refunded: { label: 'Refunded', class: 'status-refunded', icon: RotateCcw },
  expired: { label: 'Expired', class: 'status-refunded', icon: Clock },
  disputed: { label: 'Disputed', class: 'status-pending', icon: AlertCircle },
};

const formatAddress = (address: string) => {
  return `${address.slice(0, 10)}...${address.slice(-8)}`;
};

interface DbEscrow {
  id: string;
  buyer_address: string;
  seller_address: string;
  amount: number;
  deadline: string;
  status: string;
  description: string | null;
  created_at: string;
  updated_at: string;
   utxo_tx_hash?: string | null;
   utxo_output_index?: number | null;
   requires_multi_sig?: boolean;
   buyer_signed_at?: string | null;
   seller_signed_at?: string | null;
}

interface DbTransaction {
  id: string;
  escrow_id: string;
  tx_type: string;
  tx_hash: string;
  from_address: string;
  to_address: string | null;
  amount: number;
  created_at: string;
}

export const EscrowDetail: React.FC = () => {
  const { id } = useParams<{ id: string }>();
   const { wallet, walletApi, refreshBalance } = useWallet();
  const { user } = useAuth();
  const navigate = useNavigate();
  const { toast } = useToast();
  
  const [escrow, setEscrow] = useState<DbEscrow | null>(null);
  const [transactions, setTransactions] = useState<DbTransaction[]>([]);
  const [loading, setLoading] = useState(true);
  const [isProcessing, setIsProcessing] = useState(false);
  const [copiedField, setCopiedField] = useState<string | null>(null);

  useEffect(() => {
    const fetchData = async () => {
      if (!id) return;

      try {
        const [escrowData, txData] = await Promise.all([
          escrowApi.getEscrowById(id),
          escrowApi.getEscrowTransactions(id),
        ]);
        setEscrow(escrowData);
        setTransactions(txData || []);
      } catch (error) {
        console.error('Failed to fetch escrow:', error);
        toast({
          variant: 'destructive',
          title: 'Failed to load escrow',
          description: error instanceof Error ? error.message : 'Unknown error',
        });
      } finally {
        setLoading(false);
      }
    };

    fetchData();
  }, [id, toast]);

  const displayEscrow = useMemo(() => {
    if (!escrow) return null;
    return {
      id: escrow.id,
      buyer: escrow.buyer_address,
      seller: escrow.seller_address,
      amount: lovelaceToAda(BigInt(escrow.amount)),
      deadline: new Date(escrow.deadline),
      status: escrow.status as keyof typeof statusConfig,
      description: escrow.description,
      createdAt: new Date(escrow.created_at),
      updatedAt: new Date(escrow.updated_at),
    };
  }, [escrow]);

  const displayTransactions: EscrowTransaction[] = useMemo(() => {
    return transactions.map(tx => ({
      id: tx.id,
      escrowId: tx.escrow_id,
      type: tx.tx_type as 'funded' | 'released' | 'refunded',
      from: tx.from_address,
      to: tx.to_address || undefined,
      amount: lovelaceToAda(BigInt(tx.amount)),
      timestamp: new Date(tx.created_at),
      txHash: tx.tx_hash,
    }));
  }, [transactions]);

  const userRole: UserRole | null = useMemo(() => {
    if (!displayEscrow || !wallet) return null;
    if (displayEscrow.buyer === wallet.address) return 'buyer';
    if (displayEscrow.seller === wallet.address) return 'seller';
    return null;
  }, [displayEscrow, wallet]);

  const isDeadlinePassed = displayEscrow ? isPast(displayEscrow.deadline) : false;
  const canRelease = userRole === 'buyer' && displayEscrow?.status === 'active';
  const canRefund = userRole === 'buyer' && displayEscrow?.status === 'active' && isDeadlinePassed;

  const handleCopy = (text: string, field: string) => {
    navigator.clipboard.writeText(text);
    setCopiedField(field);
    setTimeout(() => setCopiedField(null), 2000);
  };

  const handleRelease = async () => {
    if (!displayEscrow || !wallet || !user || !walletApi || !escrow) return;
    
    setIsProcessing(true);
    try {
      toast({
        title: 'Wallet Authorization Required',
        description: 'Please approve the transaction in your wallet...',
      });

      const result = await executeEscrowRelease(walletApi, {
        buyerAddress: escrow.buyer_address,
        sellerAddress: escrow.seller_address,
        deadline: new Date(escrow.deadline),
        escrowUtxoTxHash: escrow.utxo_tx_hash || '',
        escrowUtxoIndex: escrow.utxo_output_index ?? 0,
      });

      if (!result.success || !result.txHash) {
        throw new Error(result.error || 'Transaction failed');
      }

      const { escrow: updatedEscrow, transaction } = await escrowApi.releaseEscrow({
        escrow_id: displayEscrow.id,
        tx_hash: result.txHash,
      });
      
      setEscrow(updatedEscrow);
      setTransactions(prev => [...prev, transaction]);
      
      toast({
        title: 'Funds Released!',
        description: `${displayEscrow.amount} ADA has been sent to the seller. Tx: ${result.txHash.slice(0, 12)}…`,
      });
       
      await refreshBalance();
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Unknown error';
       
      if (errorMessage.includes('rejected') || errorMessage.includes('declined') || errorMessage.includes('cancel') || errorMessage.includes('refuse')) {
        toast({
          variant: 'destructive',
          title: 'Transaction Cancelled',
          description: 'You cancelled the wallet authorization',
        });
        setIsProcessing(false);
        return;
      }
       
      toast({
        variant: 'destructive',
        title: 'Release Failed',
        description: errorMessage,
      });
    } finally {
      setIsProcessing(false);
    }
  };

  const handleRefund = async () => {
    if (!displayEscrow || !wallet || !user || !walletApi || !escrow) return;
    
    setIsProcessing(true);
    try {
      toast({
        title: 'Wallet Authorization Required',
        description: 'Please approve the refund in your wallet...',
      });

      const result = await executeEscrowRefund(walletApi, {
        buyerAddress: escrow.buyer_address,
        sellerAddress: escrow.seller_address,
        deadline: new Date(escrow.deadline),
        escrowUtxoTxHash: escrow.utxo_tx_hash || '',
        escrowUtxoIndex: escrow.utxo_output_index ?? 0,
      });

      if (!result.success || !result.txHash) {
        throw new Error(result.error || 'Transaction failed');
      }

      const { escrow: updatedEscrow, transaction } = await escrowApi.refundEscrow({
        escrow_id: displayEscrow.id,
        tx_hash: result.txHash,
      });
      
      setEscrow(updatedEscrow);
      setTransactions(prev => [...prev, transaction]);
      await refreshBalance();
      
      toast({
        title: 'Refund Successful!',
        description: `${displayEscrow.amount} ADA has been returned to your wallet. Tx: ${result.txHash.slice(0, 12)}…`,
      });
    } catch (error) {
      const errorMessage = error instanceof Error ? error.message : 'Unknown error';
       
      if (errorMessage.includes('rejected') || errorMessage.includes('declined') || errorMessage.includes('cancel') || errorMessage.includes('refuse')) {
        toast({
          variant: 'destructive',
          title: 'Transaction Cancelled',
          description: 'You cancelled the wallet authorization',
        });
        setIsProcessing(false);
        return;
      }
       
      toast({
        variant: 'destructive',
        title: 'Refund Failed',
        description: errorMessage,
      });
    } finally {
      setIsProcessing(false);
    }
  };

  if (loading) {
    return (
      <div className="min-h-screen pt-16 flex items-center justify-center">
        <div className="text-center">
          <Loader2 className="h-8 w-8 animate-spin text-primary mx-auto mb-4" />
          <p className="text-muted-foreground">Loading escrow details...</p>
        </div>
      </div>
    );
  }

  if (!displayEscrow) {
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

  const status = statusConfig[displayEscrow.status] || statusConfig.active;
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
              <p className="text-sm text-muted-foreground font-mono">{displayEscrow.id}</p>
            </div>

            {userRole && (
              <Badge variant="outline" className="text-muted-foreground">
                You are the {userRole === 'buyer' ? '🛒 Buyer' : '💰 Seller'}
              </Badge>
            )}

            {/* QR Share Button */}
            <EscrowQRShare
              escrowId={displayEscrow.id}
              amount={displayEscrow.amount}
              status={displayEscrow.status}
            />
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
              <p className="text-4xl font-bold gradient-text">{displayEscrow.amount.toLocaleString()} ₳</p>
              {displayEscrow.description && (
                <p className="mt-4 text-muted-foreground">{displayEscrow.description}</p>
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
                    <p className="font-mono text-sm">{formatAddress(displayEscrow.buyer)}</p>
                  </div>
                  <Button
                    variant="ghost"
                    size="icon"
                    onClick={() => handleCopy(displayEscrow.buyer, 'buyer')}
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
                    <p className="font-mono text-sm">{formatAddress(displayEscrow.seller)}</p>
                  </div>
                  <Button
                    variant="ghost"
                    size="icon"
                    onClick={() => handleCopy(displayEscrow.seller, 'seller')}
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
              <TransactionTimeline transactions={displayTransactions} />
            </div>

            {/* Messaging */}
            {wallet && (
              <EscrowChat
                escrowId={displayEscrow.id}
                userAddress={wallet.address}
                buyerAddress={displayEscrow.buyer}
                sellerAddress={displayEscrow.seller}
              />
            )}

            {/* Document Attachments */}
            {wallet && (
              <EscrowAttachments
                escrowId={displayEscrow.id}
                userAddress={wallet.address}
                buyerAddress={displayEscrow.buyer}
                sellerAddress={displayEscrow.seller}
              />
            )}
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
                {format(displayEscrow.deadline, 'MMM d, yyyy')}
              </p>
              <p className="text-sm text-muted-foreground mb-2">
                {format(displayEscrow.deadline, 'HH:mm')}
              </p>
              <p className={`text-sm ${isDeadlinePassed ? 'text-destructive' : 'text-warning'}`}>
                {isDeadlinePassed
                  ? `Expired ${formatDistanceToNow(displayEscrow.deadline)} ago`
                  : `${formatDistanceToNow(displayEscrow.deadline)} remaining`}
              </p>
            </div>

            {/* Actions */}
            {displayEscrow.status === 'active' && userRole === 'buyer' && (
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
                        This will transfer {displayEscrow.amount} ADA to the seller. This action cannot be undone.
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
                        This will return {displayEscrow.amount} ADA to your wallet. This action cannot be undone.
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

            {displayEscrow.status === 'active' && userRole === 'seller' && (
              <div className="glass-card p-6">
                <div className="flex items-start gap-3">
                  <Clock className="h-5 w-5 text-primary flex-shrink-0 mt-0.5" />
                  <div>
                    <h3 className="font-semibold mb-1">Awaiting Release</h3>
                    <p className="text-sm text-muted-foreground">
                      The buyer will release funds once satisfied. You'll receive {displayEscrow.amount} ADA.
                    </p>
                  </div>
                </div>
              </div>
            )}

            {/* Completed/Refunded Status */}
            {(displayEscrow.status === 'completed' || displayEscrow.status === 'refunded') && (
              <div className="glass-card p-6">
                <div className="flex items-start gap-3">
                  {displayEscrow.status === 'completed' ? (
                    <CheckCircle className="h-5 w-5 text-success flex-shrink-0" />
                  ) : (
                    <RotateCcw className="h-5 w-5 text-warning flex-shrink-0" />
                  )}
                  <div>
                    <h3 className="font-semibold mb-1">
                      {displayEscrow.status === 'completed' ? 'Transaction Complete' : 'Funds Refunded'}
                    </h3>
                    <p className="text-sm text-muted-foreground">
                      {displayEscrow.status === 'completed'
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
                  <span>{format(displayEscrow.createdAt, 'MMM d, yyyy HH:mm')}</span>
                </div>
                <div className="flex justify-between">
                  <span className="text-muted-foreground">Last Updated</span>
                  <span>{format(displayEscrow.updatedAt, 'MMM d, yyyy HH:mm')}</span>
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
