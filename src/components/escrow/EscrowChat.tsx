import React, { useState, useEffect, useRef } from 'react';
import { motion, AnimatePresence } from 'framer-motion';
import { Send, MessageCircle, Loader2 } from 'lucide-react';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { ScrollArea } from '@/components/ui/scroll-area';
import { supabase } from '@/integrations/supabase/client';
import { useToast } from '@/hooks/use-toast';
import { useAuth } from '@/contexts/AuthContext';
import { useNavigate } from 'react-router-dom';
import { format } from 'date-fns';

interface Message {
  id: string;
  escrow_id: string;
  sender_address: string;
  content: string;
  created_at: string;
  // local flag for optimistic UI
  pending?: boolean;
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
  const { user: authUser } = useAuth();
  const navigate = useNavigate();

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
        const msg = error instanceof Error ? error.message : String(error);
        // If permission issue, show a helpful toast
        if (msg.toLowerCase().includes('permission') || msg.toLowerCase().includes('row level')) {
          toast({
            title: 'Chat unavailable',
            description: 'Please sign in (Auth) to view messages for this escrow.',
            variant: 'default',
          });
        } else {
          toast({
            title: 'Failed to load messages',
            description: msg || 'Unknown error',
            variant: 'destructive',
          });
        }
      } finally {
        setLoading(false);
      }
    };

    fetchMessages();
  }, [escrowId, toast]);

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

    if (!authUser) {
      toast({
        title: 'Sign in required',
        description: 'Please sign in to your account to send messages.',
        variant: 'destructive',
      });
      return;
    }

    setSending(true);
    const trimmed = newMessage.trim();

    // Optimistic UI: add a pending message so sender sees it immediately
    const tempId = `temp-${Date.now()}`;
    const tempMsg: Message = {
      id: tempId,
      escrow_id: escrowId,
      sender_address: userAddress,
      content: trimmed,
      created_at: new Date().toISOString(),
      pending: true,
    };

    setMessages((prev) => [...prev, tempMsg]);
    setNewMessage('');

    try {
      const { data: insertedRows, error } = await supabase.from('escrow_messages').insert({
        escrow_id: escrowId,
        sender_address: userAddress,
        content: trimmed,
      }).select();

      if (error) throw new Error(error.message || JSON.stringify(error));

      // Replace temp message with the actual inserted row (if returned)
      if (insertedRows && insertedRows.length > 0) {
        const inserted = insertedRows[0] as Message;
        setMessages((prev) => prev.map((m) => (m.id === tempId ? inserted : m)));

        // Also update escrow metadata so recipient sees activity in lists immediately
        try {
          await supabase
            .from('escrows')
            .update({ last_message_preview: inserted.content, last_message_at: inserted.created_at })
            .eq('id', escrowId);
        } catch (escErr) {
          console.debug('Could not update escrow last_message:', escErr);
        }
      }

      // Trigger email notification for the other participant
      try {
        const recipientAddress = userAddress === buyerAddress ? sellerAddress : buyerAddress;
        // Fire-and-forget; don't block UI on notification failure
        const notifyResp = await fetch(`${import.meta.env.VITE_SUPABASE_URL}/functions/v1/send-notification`, {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            // include publishable key so Supabase can validate the request if configured
            'apikey': import.meta.env.VITE_SUPABASE_PUBLISHABLE_KEY,
          },
          body: JSON.stringify({
            type: 'message_received',
            escrow_id: escrowId,
            recipient_address: recipientAddress,
            data: {
              message: trimmed,
              base_url: window.location.origin,
            },
          }),
        });

        if (!notifyResp.ok) {
          const txt = await notifyResp.text();
          console.warn('Notification function returned non-OK:', notifyResp.status, txt);
        }
      } catch (notifyErr) {
        console.error('Failed to trigger notification:', notifyErr);
      }
    } catch (error) {
      console.error('Error sending message:', error);
      const errMsg =
        error instanceof Error
          ? error.message
          : typeof error === 'string'
          ? error
          : (error && typeof error === 'object' && 'message' in error)
          ? String((error as any).message)
          : JSON.stringify(error);

      // Remove optimistic temp message
      setMessages((prev) => prev.filter((m) => m.id !== tempId));

      const permissionIssue = errMsg.toLowerCase().includes('permission') || errMsg.toLowerCase().includes('row level') || errMsg.toLowerCase().includes('not authenticated');

      toast({
        variant: 'destructive',
        title: 'Failed to send message',
        description: permissionIssue
          ? 'You need to sign in and link your wallet to send messages. Go to Profile → Link Wallet.'
          : errMsg || 'Unknown error',
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

      {!authUser && (
        <div className="mb-4 rounded-md bg-yellow-50 p-3 text-sm text-yellow-800 flex items-center justify-between">
          <div>
            <strong>Sign in to chat</strong>
            <div className="text-xs text-muted-foreground">Connect your account to send and receive messages and enable email notifications.</div>
          </div>
          <div>
            <button
              onClick={() => navigate('/auth')}
              className="ml-4 inline-flex items-center rounded-md bg-yellow-100 px-3 py-1 text-xs font-medium"
            >
              Sign in
            </button>
          </div>
        </div>
      )}

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
