import { serve } from "https://deno.land/std@0.190.0/http/server.ts";
import { createClient } from "https://esm.sh/@supabase/supabase-js@2.49.1";
import { bech32 } from "https://esm.sh/bech32@1.1.4";
import { blake2b } from "https://esm.sh/@noble/hashes@1.4.2/blake2b";
import * as ed25519 from "https://esm.sh/@noble/ed25519@1.7.1";

const corsHeaders = {
  "Access-Control-Allow-Origin": "*",
  "Access-Control-Allow-Headers": "authorization, x-client-info, apikey, content-type",
};

function hexToBytes(hex: string): Uint8Array {
  if (!hex) return new Uint8Array();
  const clean = hex.startsWith('0x') ? hex.slice(2) : hex;
  const bytes = new Uint8Array(clean.length / 2);
  for (let i = 0; i < clean.length; i += 2) {
    bytes[i / 2] = parseInt(clean.substr(i, 2), 16);
  }
  return bytes;
}

function bech32ToBytes(addr: string): Uint8Array | null {
  try {
    const decoded = bech32.decode(addr, 1023);
    const bytes = bech32.fromWords(decoded.words);
    return new Uint8Array(bytes);
  } catch {
    // not bech32 — maybe raw hex
    if (/^[0-9a-fA-F]+$/.test(addr)) {
      return hexToBytes(addr);
    }
    return null;
  }
}

const handler = async (req: Request): Promise<Response> => {
  if (req.method === 'OPTIONS') return new Response(null, { headers: corsHeaders });

  try {
    const SUPABASE_URL = Deno.env.get('SUPABASE_URL')!;
    const SUPABASE_SERVICE_ROLE_KEY = Deno.env.get('SUPABASE_SERVICE_ROLE_KEY')!;

    if (!SUPABASE_URL || !SUPABASE_SERVICE_ROLE_KEY) {
      return new Response(JSON.stringify({ error: 'Server not configured' }), { status: 500, headers: { ...corsHeaders, 'Content-Type': 'application/json' } });
    }

    const supabase = createClient(SUPABASE_URL, SUPABASE_SERVICE_ROLE_KEY);

    const body = await req.json();
    const { escrow_id, content, sender_address, payload, signature, key } = body || {};

    if (!escrow_id || !content || !sender_address || !payload || !signature || !key) {
      return new Response(JSON.stringify({ error: 'Missing required fields' }), { status: 400, headers: { ...corsHeaders, 'Content-Type': 'application/json' } });
    }

    // Verify signature and that public key maps to the provided address using shared helpers
    const { verifySignature, publicKeyMatchesAddress } = await import('./lib.ts');

    const verified = await verifySignature(payload, signature, key);
    if (!verified) {
      return new Response(JSON.stringify({ error: 'Invalid signature' }), { status: 401, headers: { ...corsHeaders, 'Content-Type': 'application/json' } });
    }

    const matches = await publicKeyMatchesAddress(key, String(sender_address));
    if (!matches) {
      return new Response(JSON.stringify({ error: 'Public key does not match address' }), { status: 400, headers: { ...corsHeaders, 'Content-Type': 'application/json' } });
    }

    // Insert message using service role (bypass RLS safely because we've verified ownership)
    const { data: insertedRows, error: insertError } = await supabase
      .from('escrow_messages')
      .insert({ escrow_id, sender_address, content })
      .select();

    if (insertError) {
      console.error('DB insert error:', insertError);
      return new Response(JSON.stringify({ error: insertError.message || String(insertError) }), { status: 500, headers: { ...corsHeaders, 'Content-Type': 'application/json' } });
    }

    const inserted = Array.isArray(insertedRows) && insertedRows.length > 0 ? insertedRows[0] : null;

    // Trigger email notification for recipient (fire-and-forget)
    try {
      const recipientAddress = String(sender_address) === String(inserted?.sender_address) ? (await (async () => {
        // Determine counterparty address from escrow
        const { data: e } = await supabase.from('escrows').select('buyer_address, seller_address').eq('id', escrow_id).maybeSingle();
        if (!e) return null;
        return (e.buyer_address === sender_address) ? e.seller_address : e.buyer_address;
      })()) : null;

      if (recipientAddress) {
        // Call existing notification function (re-use logic)
        await fetch(`${SUPABASE_URL}/functions/v1/send-notification`, {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            'apikey': SUPABASE_SERVICE_ROLE_KEY,
          },
          body: JSON.stringify({
            type: 'message_received',
            escrow_id,
            recipient_address: recipientAddress,
            data: { message: content, base_url: Deno.env.get('APP_BASE_URL') || 'https://trusty-deal-maker.lovable.app' },
          }),
        }).catch((err) => console.warn('notify call failed:', err));
      }
    } catch (notifyErr) {
      console.warn('Notification step failed:', notifyErr);
    }

    return new Response(JSON.stringify({ success: true, message: inserted }), { status: 200, headers: { ...corsHeaders, 'Content-Type': 'application/json' } });

  } catch (err) {
    console.error('Error in send-wallet-message:', err);
    const msg = err instanceof Error ? err.message : String(err);
    return new Response(JSON.stringify({ error: msg }), { status: 500, headers: { ...corsHeaders, 'Content-Type': 'application/json' } });
  }
};

serve(handler);
