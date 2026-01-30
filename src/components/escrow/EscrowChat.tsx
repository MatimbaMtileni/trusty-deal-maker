import React, { useState, useEffect, useRef } from 'react';
import { motion, AnimatePresence } from 'framer-motion';
import { Send, MessageCircle, Loader2 } from 'lucide-react';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { ScrollArea } from '@/components/ui/scroll-area';
import { supabase } from '@/integrations/supabase/client';
import { useToast } from '@/hooks/use-toast';
import { format } from 'date-fns';

interface Message {
  id: string;
  escrow_id: string;
  sender_address: string;
  content: string;
  created_at: string;
}

interface EscrowChatProps {
  escrowId: string;
  userAddress: string;
  buyerAddress: string;
  sellerAddress: string;
}

const formatAddress = (address: string) => {
  return `${address.slice(0, 6)}...${address.slice(-4)}`;
};

export const EscrowChat: React.FC<EscrowChatProps> = ({
  escrowId,
  userAddress,
  buyerAddress,
  sellerAddress,
}) => {
  const [messages, setMessages] = useState<Message[]>([]);
  const [newMessage, setNewMessage] = useState('');
  const [loading, setLoading] = useState(true);
  const [sending, setSending] = useState(false);
  const scrollRef = useRef<HTMLDivElement>(null);
  const { toast } = useToast();

  const isParticipant = userAddress === buyerAddress || userAddress === sellerAddress;

  // Fetch initial messages
  useEffect(() => {
    const fetchMessages = async () => {
      try {
        const { data, error } = await supabase
          .from('escrow_messages')
          .select('*')
          .eq('escrow_id', escrowId)
          .order('created_at', { ascending: true });

        if (error) throw error;
        setMessages(data || []);
      } catch (error) {
        console.error('Failed to fetch messages:', error);
      } finally {
        setLoading(false);
      }
    };

    fetchMessages();
  }, [escrowId]);

  // Subscribe to realtime messages
  useEffect(() => {
    const channel = supabase
      .channel(`escrow-messages-${escrowId}`)
      .on(
        'postgres_changes',
        {
          event: 'INSERT',
          schema: 'public',
          table: 'escrow_messages',
          filter: `escrow_id=eq.${escrowId}`,
        },
        (payload) => {
          setMessages(prev => [...prev, payload.new as Message]);
        }
      )
      .subscribe();

    return () => {
      supabase.removeChannel(channel);
    };
  }, [escrowId]);

  // Auto-scroll to bottom on new messages
  useEffect(() => {
    if (scrollRef.current) {
      scrollRef.current.scrollTop = scrollRef.current.scrollHeight;
    }
  }, [messages]);

  const handleSend = async (e: React.FormEvent) => {
    e.preventDefault();
    if (!newMessage.trim() || !isParticipant) return;

    setSending(true);
    try {
      const { error } = await supabase.from('escrow_messages').insert({
        escrow_id: escrowId,
        sender_address: userAddress,
        content: newMessage.trim(),
      });

      if (error) throw error;
      setNewMessage('');
    } catch (error) {
      toast({
        variant: 'destructive',
        title: 'Failed to send message',
        description: error instanceof Error ? error.message : 'Unknown error',
      });
    } finally {
      setSending(false);
    }
  };

  const getSenderLabel = (senderAddress: string) => {
    if (senderAddress === buyerAddress) return 'Buyer';
    if (senderAddress === sellerAddress) return 'Seller';
    return 'Unknown';
  };

  if (!isParticipant) {
    return null;
  }

  return (
    <div className="glass-card p-6">
      <h3 className="font-semibold mb-4 flex items-center gap-2">
        <MessageCircle className="h-4 w-4 text-primary" />
        Messages
      </h3>

      <ScrollArea className="h-64 pr-4" ref={scrollRef}>
        {loading ? (
          <div className="flex items-center justify-center h-full">
            <Loader2 className="h-5 w-5 animate-spin text-muted-foreground" />
          </div>
        ) : messages.length === 0 ? (
          <div className="text-center text-muted-foreground py-8">
            <MessageCircle className="h-8 w-8 mx-auto mb-2 opacity-50" />
            <p className="text-sm">No messages yet</p>
            <p className="text-xs">Start the conversation!</p>
          </div>
        ) : (
          <div className="space-y-3">
            <AnimatePresence initial={false}>
              {messages.map((msg) => {
                const isOwn = msg.sender_address === userAddress;
                return (
                  <motion.div
                    key={msg.id}
                    initial={{ opacity: 0, y: 10 }}
                    animate={{ opacity: 1, y: 0 }}
                    className={`flex flex-col ${isOwn ? 'items-end' : 'items-start'}`}
                  >
                    <div className="flex items-center gap-2 mb-1">
                      <span className="text-xs text-muted-foreground">
                        {getSenderLabel(msg.sender_address)}
                      </span>
                      <span className="text-xs text-muted-foreground/60">
                        {format(new Date(msg.created_at), 'HH:mm')}
                      </span>
                    </div>
                    <div
                      className={`max-w-[80%] rounded-lg px-3 py-2 text-sm ${
                        isOwn
                          ? 'bg-primary text-primary-foreground'
                          : 'bg-muted/50'
                      }`}
                    >
                      {msg.content}
                    </div>
                  </motion.div>
                );
              })}
            </AnimatePresence>
          </div>
        )}
      </ScrollArea>

      <form onSubmit={handleSend} className="mt-4 flex gap-2">
        <Input
          value={newMessage}
          onChange={(e) => setNewMessage(e.target.value)}
          placeholder="Type a message..."
          disabled={sending}
          className="flex-1"
        />
        <Button type="submit" size="icon" disabled={!newMessage.trim() || sending}>
          {sending ? (
            <Loader2 className="h-4 w-4 animate-spin" />
          ) : (
            <Send className="h-4 w-4" />
          )}
        </Button>
      </form>
    </div>
  );
};
