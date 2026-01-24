import React from 'react';
import { motion, AnimatePresence } from 'framer-motion';
import { X, Wallet, ExternalLink } from 'lucide-react';
import { Dialog, DialogContent, DialogHeader, DialogTitle } from '@/components/ui/dialog';
import { useWallet } from '@/contexts/WalletContext';
import { WalletType, WalletOption } from '@/types/escrow';

const walletOptions: WalletOption[] = [
  {
    id: 'nami',
    name: 'Nami',
    icon: '🔷',
    description: 'Popular Cardano browser wallet',
  },
  {
    id: 'lace',
    name: 'Lace',
    icon: '🎴',
    description: "IOG's official Cardano wallet",
  },
  {
    id: 'eternl',
    name: 'Eternl',
    icon: '♾️',
    description: 'Feature-rich Cardano wallet',
  },
];

interface WalletConnectModalProps {
  open: boolean;
  onOpenChange: (open: boolean) => void;
}

export const WalletConnectModal: React.FC<WalletConnectModalProps> = ({ open, onOpenChange }) => {
  const { connect, isConnecting } = useWallet();

  const handleConnect = async (walletType: WalletType) => {
    await connect(walletType);
    onOpenChange(false);
  };

  return (
    <Dialog open={open} onOpenChange={onOpenChange}>
      <DialogContent className="glass-card border-border/50 sm:max-w-md">
        <DialogHeader>
          <DialogTitle className="flex items-center gap-2 text-xl">
            <Wallet className="h-5 w-5 text-primary" />
            Connect Wallet
          </DialogTitle>
        </DialogHeader>

        <div className="space-y-3 py-4">
          <p className="text-sm text-muted-foreground">
            Select a wallet to connect to the Cardano escrow platform.
          </p>

          <div className="space-y-2">
            {walletOptions.map((wallet, index) => (
              <motion.button
                key={wallet.id}
                initial={{ opacity: 0, x: -20 }}
                animate={{ opacity: 1, x: 0 }}
                transition={{ delay: index * 0.1 }}
                onClick={() => handleConnect(wallet.id)}
                disabled={isConnecting}
                className="w-full glass-card p-4 flex items-center gap-4 hover:border-primary/50 transition-all duration-300 group disabled:opacity-50 disabled:cursor-not-allowed"
              >
                <span className="text-3xl">{wallet.icon}</span>
                <div className="flex-1 text-left">
                  <h3 className="font-semibold text-foreground group-hover:text-primary transition-colors">
                    {wallet.name}
                  </h3>
                  <p className="text-sm text-muted-foreground">{wallet.description}</p>
                </div>
                <ExternalLink className="h-4 w-4 text-muted-foreground group-hover:text-primary transition-colors" />
              </motion.button>
            ))}
          </div>

          {isConnecting && (
            <motion.div
              initial={{ opacity: 0 }}
              animate={{ opacity: 1 }}
              className="flex items-center justify-center gap-2 py-4"
            >
              <div className="h-4 w-4 rounded-full border-2 border-primary border-t-transparent animate-spin" />
              <span className="text-sm text-muted-foreground">Connecting...</span>
            </motion.div>
          )}

          <p className="text-xs text-muted-foreground text-center pt-2">
            This is a demo environment. No real ADA will be used.
          </p>
        </div>
      </DialogContent>
    </Dialog>
  );
};
