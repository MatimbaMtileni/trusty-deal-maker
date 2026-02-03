import React from 'react';
import { motion } from 'framer-motion';
import { Wallet, ExternalLink, AlertCircle, Download } from 'lucide-react';
import { Dialog, DialogContent, DialogHeader, DialogTitle } from '@/components/ui/dialog';
import { Button } from '@/components/ui/button';
import { useWallet } from '@/contexts/WalletContext';

const WALLET_DOWNLOAD_URLS: Record<string, string> = {
  nami: 'https://namiwallet.io',
  lace: 'https://www.lace.io',
  eternl: 'https://eternl.io',
  flint: 'https://flint-wallet.com',
  gerowallet: 'https://gerowallet.io',
  typhon: 'https://typhonwallet.io',
  yoroi: 'https://yoroi-wallet.com',
};

const POPULAR_WALLETS = ['nami', 'lace', 'eternl', 'flint', 'yoroi'];

interface WalletConnectModalProps {
  open: boolean;
  onOpenChange: (open: boolean) => void;
}

export const WalletConnectModal: React.FC<WalletConnectModalProps> = ({ open, onOpenChange }) => {
  const { connect, isConnecting, installedWallets } = useWallet();

  const handleConnect = async (walletName: string) => {
    try {
      await connect(walletName);
      onOpenChange(false);
    } catch (error) {
      // Error is already handled in context with toast
    }
  };

  const installedWalletNames = installedWallets.map(w => w.name.toLowerCase());
  
  // Show installed wallets first, then suggest popular wallets to install
  const notInstalledWallets = POPULAR_WALLETS.filter(
    name => !installedWalletNames.includes(name)
  );

  return (
    <Dialog open={open} onOpenChange={onOpenChange}>
      <DialogContent className="glass-card border-border/50 sm:max-w-md">
        <DialogHeader>
          <DialogTitle className="flex items-center gap-2 text-xl">
            <Wallet className="h-5 w-5 text-primary" />
            Connect Wallet
          </DialogTitle>
        </DialogHeader>

        <div className="space-y-4 py-4">
          <p className="text-sm text-muted-foreground">
            Connect your Cardano wallet to interact with escrow contracts on the blockchain.
          </p>

          {/* Installed Wallets */}
          {installedWallets.length > 0 ? (
            <div className="space-y-2">
              <p className="text-xs font-medium text-muted-foreground uppercase tracking-wide">
                Detected Wallets
              </p>
              {installedWallets.map((wallet, index) => (
                <motion.button
                  key={wallet.name}
                  initial={{ opacity: 0, x: -20 }}
                  animate={{ opacity: 1, x: 0 }}
                  transition={{ delay: index * 0.1 }}
                  onClick={() => handleConnect(wallet.name)}
                  disabled={isConnecting}
                  className="w-full glass-card p-4 flex items-center gap-4 hover:border-primary/50 transition-all duration-300 group disabled:opacity-50 disabled:cursor-not-allowed"
                >
                  {wallet.icon ? (
                    <img 
                      src={wallet.icon} 
                      alt={wallet.name} 
                      className="w-10 h-10 rounded-lg"
                    />
                  ) : (
                    <div className="w-10 h-10 rounded-lg bg-primary/20 flex items-center justify-center">
                      <Wallet className="w-5 h-5 text-primary" />
                    </div>
                  )}
                  <div className="flex-1 text-left">
                    <h3 className="font-semibold text-foreground group-hover:text-primary transition-colors capitalize">
                      {wallet.name}
                    </h3>
                    <p className="text-sm text-muted-foreground">
                      v{wallet.apiVersion}
                    </p>
                  </div>
                  <ExternalLink className="h-4 w-4 text-muted-foreground group-hover:text-primary transition-colors" />
                </motion.button>
              ))}
            </div>
          ) : (
            <div className="glass-card p-4 flex items-start gap-3 border-yellow-500/30 bg-yellow-500/5">
              <AlertCircle className="h-5 w-5 text-yellow-500 mt-0.5 shrink-0" />
              <div>
                <p className="text-sm font-medium text-yellow-500">No wallets detected</p>
                <p className="text-xs text-muted-foreground mt-1">
                  Install a Cardano wallet browser extension to continue.
                </p>
              </div>
            </div>
          )}

          {/* Suggested Wallets to Install */}
          {notInstalledWallets.length > 0 && (
            <div className="space-y-2 pt-2">
              <p className="text-xs font-medium text-muted-foreground uppercase tracking-wide">
                {installedWallets.length > 0 ? 'Other Wallets' : 'Recommended Wallets'}
              </p>
              <div className="grid grid-cols-2 gap-2">
                {notInstalledWallets.map((walletName) => (
                  <a
                    key={walletName}
                    href={WALLET_DOWNLOAD_URLS[walletName]}
                    target="_blank"
                    rel="noopener noreferrer"
                    className="glass-card p-3 flex items-center gap-2 hover:border-primary/30 transition-all text-sm text-muted-foreground hover:text-foreground"
                  >
                    <Download className="h-4 w-4" />
                    <span className="capitalize">{walletName}</span>
                  </a>
                ))}
              </div>
            </div>
          )}

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

          <div className="pt-2 border-t border-border/50">
            <p className="text-xs text-muted-foreground text-center">
              Make sure you're connected to the correct network (Mainnet or Testnet) in your wallet.
            </p>
          </div>
        </div>
      </DialogContent>
    </Dialog>
  );
};
