import { serve } from "https://deno.land/std@0.190.0/http/server.ts";
import { createClient } from "https://esm.sh/@supabase/supabase-js@2";

const corsHeaders = {
  "Access-Control-Allow-Origin": "*",
  "Access-Control-Allow-Headers": "authorization, x-client-info, apikey, content-type",
};

interface CreateEscrowRequest {
  action: "create";
  buyer_address: string;
  seller_address: string;
  amount: number;
  deadline: string;
  description?: string;
  tx_hash: string;
}

interface UpdateEscrowRequest {
  action: "release" | "refund";
  escrow_id: string;
  tx_hash: string;
}

type EscrowRequest = CreateEscrowRequest | UpdateEscrowRequest;

serve(async (req: Request): Promise<Response> => {
  // Handle CORS preflight
  if (req.method === "OPTIONS") {
    return new Response(null, { headers: corsHeaders });
  }

  try {
    const authHeader = req.headers.get("Authorization");
    if (!authHeader?.startsWith("Bearer ")) {
      return new Response(
        JSON.stringify({ error: "Unauthorized" }),
        { status: 401, headers: { ...corsHeaders, "Content-Type": "application/json" } }
      );
    }

    const supabase = createClient(
      Deno.env.get("SUPABASE_URL")!,
      Deno.env.get("SUPABASE_ANON_KEY")!,
      { global: { headers: { Authorization: authHeader } } }
    );

    // Verify user
    const token = authHeader.replace("Bearer ", "");
    const { data: authData, error: authError } = await supabase.auth.getClaims(token);
    
    if (authError || !authData?.claims) {
      return new Response(
        JSON.stringify({ error: "Unauthorized" }),
        { status: 401, headers: { ...corsHeaders, "Content-Type": "application/json" } }
      );
    }

    const userId = authData.claims.sub;
    const body: EscrowRequest = await req.json();

    if (body.action === "create") {
      // Create new escrow
      const { buyer_address, seller_address, amount, deadline, description, tx_hash } = body as CreateEscrowRequest;

      // Validate required fields
      if (!buyer_address || !seller_address || !amount || !deadline || !tx_hash) {
        return new Response(
          JSON.stringify({ error: "Missing required fields" }),
          { status: 400, headers: { ...corsHeaders, "Content-Type": "application/json" } }
        );
      }

      // Insert escrow
      const { data: escrow, error: escrowError } = await supabase
        .from("escrows")
        .insert({
          buyer_address,
          seller_address,
          amount,
          deadline,
          description,
          buyer_user_id: userId,
          status: "active",
        })
        .select()
        .single();

      if (escrowError) {
        console.error("Escrow creation error:", escrowError);
        return new Response(
          JSON.stringify({ error: escrowError.message }),
          { status: 500, headers: { ...corsHeaders, "Content-Type": "application/json" } }
        );
      }

      // Insert funding transaction
      const { data: transaction, error: txError } = await supabase
        .from("escrow_transactions")
        .insert({
          escrow_id: escrow.id,
          tx_type: "funded",
          tx_hash,
          from_address: buyer_address,
          amount,
        })
        .select()
        .single();

      if (txError) {
        console.error("Transaction creation error:", txError);
      }

      return new Response(
        JSON.stringify({ escrow, transaction }),
        { status: 201, headers: { ...corsHeaders, "Content-Type": "application/json" } }
      );
    }

    if (body.action === "release" || body.action === "refund") {
      const { escrow_id, tx_hash } = body as UpdateEscrowRequest;

      if (!escrow_id || !tx_hash) {
        return new Response(
          JSON.stringify({ error: "Missing escrow_id or tx_hash" }),
          { status: 400, headers: { ...corsHeaders, "Content-Type": "application/json" } }
        );
      }

      // Get escrow
      const { data: escrow, error: fetchError } = await supabase
        .from("escrows")
        .select("*")
        .eq("id", escrow_id)
        .single();

      if (fetchError || !escrow) {
        return new Response(
          JSON.stringify({ error: "Escrow not found" }),
          { status: 404, headers: { ...corsHeaders, "Content-Type": "application/json" } }
        );
      }

      // Verify user is the buyer
      if (escrow.buyer_user_id !== userId) {
        return new Response(
          JSON.stringify({ error: "Only buyer can perform this action" }),
          { status: 403, headers: { ...corsHeaders, "Content-Type": "application/json" } }
        );
      }

      if (escrow.status !== "active") {
        return new Response(
          JSON.stringify({ error: "Escrow is not active" }),
          { status: 400, headers: { ...corsHeaders, "Content-Type": "application/json" } }
        );
      }

      // For refund, check deadline
      if (body.action === "refund") {
        const now = new Date();
        const deadline = new Date(escrow.deadline);
        if (now < deadline) {
          return new Response(
            JSON.stringify({ error: "Cannot refund before deadline" }),
            { status: 400, headers: { ...corsHeaders, "Content-Type": "application/json" } }
          );
        }
      }

      const newStatus = body.action === "release" ? "completed" : "refunded";
      const txType = body.action === "release" ? "released" : "refunded";
      const toAddress = body.action === "release" ? escrow.seller_address : escrow.buyer_address;

      // Update escrow status
      const { data: updatedEscrow, error: updateError } = await supabase
        .from("escrows")
        .update({ status: newStatus })
        .eq("id", escrow_id)
        .select()
        .single();

      if (updateError) {
        console.error("Escrow update error:", updateError);
        return new Response(
          JSON.stringify({ error: updateError.message }),
          { status: 500, headers: { ...corsHeaders, "Content-Type": "application/json" } }
        );
      }

      // Insert transaction
      const { data: transaction, error: txError } = await supabase
        .from("escrow_transactions")
        .insert({
          escrow_id,
          tx_type: txType,
          tx_hash,
          from_address: escrow.buyer_address,
          to_address: toAddress,
          amount: escrow.amount,
        })
        .select()
        .single();

      if (txError) {
        console.error("Transaction insert error:", txError);
      }

      return new Response(
        JSON.stringify({ escrow: updatedEscrow, transaction }),
        { status: 200, headers: { ...corsHeaders, "Content-Type": "application/json" } }
      );
    }

    return new Response(
      JSON.stringify({ error: "Invalid action" }),
      { status: 400, headers: { ...corsHeaders, "Content-Type": "application/json" } }
    );
  } catch (error: unknown) {
    console.error("Edge function error:", error);
    const message = error instanceof Error ? error.message : "Unknown error";
    return new Response(
      JSON.stringify({ error: message }),
      { status: 500, headers: { ...corsHeaders, "Content-Type": "application/json" } }
    );
  }
});
