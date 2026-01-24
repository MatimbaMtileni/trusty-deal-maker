import React, { useState } from 'react';
import { useNavigate } from 'react-router-dom';
import { motion } from 'framer-motion';
import { useForm } from 'react-hook-form';
import { zodResolver } from '@hookform/resolvers/zod';
import { z } from 'zod';
import { format } from 'date-fns';
import { CalendarIcon, Wallet, ArrowRight, Loader2, CheckCircle, AlertCircle } from 'lucide-react';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Textarea } from '@/components/ui/textarea';
import { Calendar } from '@/components/ui/calendar';
import { Popover, PopoverContent, PopoverTrigger } from '@/components/ui/popover';
import {
  Form,
  FormControl,
  FormDescription,
  FormField,
  FormItem,
  FormLabel,
  FormMessage,
} from '@/components/ui/form';
import { useWallet } from '@/contexts/WalletContext';
import { WalletConnectModal } from '@/components/wallet/WalletConnectModal';
import { createEscrow } from '@/services/mockBlockchain';
import { useToast } from '@/hooks/use-toast';
import { cn } from '@/lib/utils';

const formSchema = z.object({
  sellerAddress: z
    .string()
    .min(1, 'Seller address is required')
    .regex(/^addr1[a-z0-9]{50,}$/, 'Invalid Cardano address format'),
  amount: z
    .number({ invalid_type_error: 'Amount is required' })
    .min(10, 'Minimum amount is 10 ADA')
    .max(1000000, 'Maximum amount is 1,000,000 ADA'),
  deadline: z.date({
    required_error: 'Deadline is required',
  }).refine(date => date > new Date(), 'Deadline must be in the future'),
  description: z.string().max(500, 'Description too long').optional(),
});

type FormValues = z.infer<typeof formSchema>;

export const CreateEscrow: React.FC = () => {
  const { wallet, updateBalance } = useWallet();
  const [connectModalOpen, setConnectModalOpen] = useState(false);
  const [isSubmitting, setIsSubmitting] = useState(false);
  const [isSuccess, setIsSuccess] = useState(false);
  const navigate = useNavigate();
  const { toast } = useToast();

  const form = useForm<FormValues>({
    resolver: zodResolver(formSchema),
    defaultValues: {
      sellerAddress: '',
      description: '',
    },
  });

  const onSubmit = async (values: FormValues) => {
    if (!wallet) return;

    if (values.amount > wallet.balance) {
      toast({
        variant: 'destructive',
        title: 'Insufficient balance',
        description: `You need ${values.amount} ADA but only have ${wallet.balance} ADA`,
      });
      return;
    }

    setIsSubmitting(true);

    try {
      const { escrow, transaction } = await createEscrow(
        wallet.address,
        {
          seller: values.sellerAddress,
          amount: values.amount,
          deadline: values.deadline,
          description: values.description,
        },
        updateBalance
      );

      setIsSuccess(true);
      toast({
        title: 'Escrow Created!',
        description: `Successfully locked ${values.amount} ADA in escrow`,
      });

      // Navigate to detail page after short delay
      setTimeout(() => {
        navigate(`/escrow/${escrow.id}`);
      }, 1500);
    } catch (error) {
      toast({
        variant: 'destructive',
        title: 'Transaction Failed',
        description: error instanceof Error ? error.message : 'Unknown error occurred',
      });
    } finally {
      setIsSubmitting(false);
    }
  };

  if (!wallet) {
    return (
      <div className="min-h-screen pt-16">
        <div className="container mx-auto px-4 py-20">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            className="text-center max-w-md mx-auto"
          >
            <div className="h-20 w-20 rounded-full bg-primary/10 flex items-center justify-center mx-auto mb-6">
              <Wallet className="h-10 w-10 text-primary" />
            </div>
            <h1 className="text-2xl font-bold mb-4">Connect Your Wallet</h1>
            <p className="text-muted-foreground mb-6">
              Connect a Cardano wallet to create a new escrow contract.
            </p>
            <Button
              onClick={() => setConnectModalOpen(true)}
              className="btn-gradient gap-2"
            >
              <Wallet className="h-4 w-4" />
              Connect Wallet
            </Button>
          </motion.div>
        </div>
        <WalletConnectModal open={connectModalOpen} onOpenChange={setConnectModalOpen} />
      </div>
    );
  }

  if (isSuccess) {
    return (
      <div className="min-h-screen pt-16">
        <div className="container mx-auto px-4 py-20">
          <motion.div
            initial={{ opacity: 0, scale: 0.9 }}
            animate={{ opacity: 1, scale: 1 }}
            className="text-center max-w-md mx-auto"
          >
            <motion.div
              initial={{ scale: 0 }}
              animate={{ scale: 1 }}
              transition={{ type: 'spring', delay: 0.2 }}
              className="h-20 w-20 rounded-full bg-success/20 flex items-center justify-center mx-auto mb-6"
            >
              <CheckCircle className="h-10 w-10 text-success" />
            </motion.div>
            <h1 className="text-2xl font-bold mb-4">Escrow Created!</h1>
            <p className="text-muted-foreground mb-6">
              Your funds have been securely locked in the escrow contract.
            </p>
            <div className="flex items-center justify-center gap-2 text-primary">
              <Loader2 className="h-4 w-4 animate-spin" />
              <span className="text-sm">Redirecting to escrow details...</span>
            </div>
          </motion.div>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen pt-16">
      <div className="container mx-auto px-4 py-8 max-w-2xl">
        <motion.div
          initial={{ opacity: 0, y: -20 }}
          animate={{ opacity: 1, y: 0 }}
          className="mb-8"
        >
          <h1 className="text-3xl font-bold mb-2">Create New Escrow</h1>
          <p className="text-muted-foreground">
            Lock ADA in a smart contract with conditions for release.
          </p>
        </motion.div>

        {/* Balance Warning */}
        <motion.div
          initial={{ opacity: 0, y: 20 }}
          animate={{ opacity: 1, y: 0 }}
          transition={{ delay: 0.1 }}
          className="glass-card p-4 mb-6 flex items-center gap-3"
        >
          <div className="h-10 w-10 rounded-full bg-primary/10 flex items-center justify-center">
            <span className="text-lg">{wallet.icon}</span>
          </div>
          <div className="flex-1">
            <p className="text-sm text-muted-foreground">Available Balance</p>
            <p className="text-xl font-bold gradient-text">{wallet.balance.toLocaleString()} ₳</p>
          </div>
        </motion.div>

        {/* Form */}
        <motion.div
          initial={{ opacity: 0, y: 20 }}
          animate={{ opacity: 1, y: 0 }}
          transition={{ delay: 0.2 }}
          className="glass-card p-6"
        >
          <Form {...form}>
            <form onSubmit={form.handleSubmit(onSubmit)} className="space-y-6">
              {/* Seller Address */}
              <FormField
                control={form.control}
                name="sellerAddress"
                render={({ field }) => (
                  <FormItem>
                    <FormLabel>Seller Wallet Address</FormLabel>
                    <FormControl>
                      <Input
                        placeholder="addr1..."
                        className="font-mono text-sm"
                        {...field}
                      />
                    </FormControl>
                    <FormDescription>
                      The Cardano address that will receive the funds upon release.
                    </FormDescription>
                    <FormMessage />
                  </FormItem>
                )}
              />

              {/* Amount */}
              <FormField
                control={form.control}
                name="amount"
                render={({ field }) => (
                  <FormItem>
                    <FormLabel>Amount (ADA)</FormLabel>
                    <FormControl>
                      <div className="relative">
                        <Input
                          type="number"
                          placeholder="0"
                          className="pr-12"
                          {...field}
                          onChange={e => field.onChange(parseFloat(e.target.value) || '')}
                        />
                        <span className="absolute right-3 top-1/2 -translate-y-1/2 text-muted-foreground">
                          ₳
                        </span>
                      </div>
                    </FormControl>
                    <FormDescription>
                      This amount will be locked until released or refunded.
                    </FormDescription>
                    <FormMessage />
                  </FormItem>
                )}
              />

              {/* Deadline */}
              <FormField
                control={form.control}
                name="deadline"
                render={({ field }) => (
                  <FormItem className="flex flex-col">
                    <FormLabel>Deadline</FormLabel>
                    <Popover>
                      <PopoverTrigger asChild>
                        <FormControl>
                          <Button
                            variant="outline"
                            className={cn(
                              'w-full justify-start text-left font-normal',
                              !field.value && 'text-muted-foreground'
                            )}
                          >
                            <CalendarIcon className="mr-2 h-4 w-4" />
                            {field.value ? format(field.value, 'PPP') : 'Pick a deadline'}
                          </Button>
                        </FormControl>
                      </PopoverTrigger>
                      <PopoverContent className="w-auto p-0 glass-card" align="start">
                        <Calendar
                          mode="single"
                          selected={field.value}
                          onSelect={field.onChange}
                          disabled={(date) => date < new Date()}
                          initialFocus
                        />
                      </PopoverContent>
                    </Popover>
                    <FormDescription>
                      After this date, you can request a refund if funds haven't been released.
                    </FormDescription>
                    <FormMessage />
                  </FormItem>
                )}
              />

              {/* Description */}
              <FormField
                control={form.control}
                name="description"
                render={({ field }) => (
                  <FormItem>
                    <FormLabel>Description (Optional)</FormLabel>
                    <FormControl>
                      <Textarea
                        placeholder="What is this escrow for?"
                        className="resize-none"
                        rows={3}
                        {...field}
                      />
                    </FormControl>
                    <FormDescription>
                      Add context about the transaction for your records.
                    </FormDescription>
                    <FormMessage />
                  </FormItem>
                )}
              />

              {/* Submit */}
              <Button
                type="submit"
                className="w-full btn-gradient gap-2"
                disabled={isSubmitting}
                size="lg"
              >
                {isSubmitting ? (
                  <>
                    <Loader2 className="h-4 w-4 animate-spin" />
                    Creating Escrow...
                  </>
                ) : (
                  <>
                    Lock Funds in Escrow
                    <ArrowRight className="h-4 w-4" />
                  </>
                )}
              </Button>
            </form>
          </Form>
        </motion.div>

        {/* Info Box */}
        <motion.div
          initial={{ opacity: 0 }}
          animate={{ opacity: 1 }}
          transition={{ delay: 0.4 }}
          className="mt-6 p-4 rounded-lg bg-primary/5 border border-primary/20"
        >
          <div className="flex gap-3">
            <AlertCircle className="h-5 w-5 text-primary flex-shrink-0 mt-0.5" />
            <div className="text-sm text-muted-foreground">
              <p className="font-medium text-foreground mb-1">How it works</p>
              <ul className="list-disc list-inside space-y-1">
                <li>Your ADA will be locked in a smart contract address</li>
                <li>Only you (buyer) can release funds to the seller</li>
                <li>After the deadline, you can reclaim your funds</li>
              </ul>
            </div>
          </div>
        </motion.div>
      </div>
    </div>
  );
};

export default CreateEscrow;
