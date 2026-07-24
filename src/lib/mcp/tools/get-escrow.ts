import { createClient } from "@supabase/supabase-js";
import { defineTool, type ToolContext } from "@lovable.dev/mcp-js";
import { z } from "zod";

function supabaseForUser(ctx: ToolContext) {
  return createClient(process.env.SUPABASE_URL!, process.env.SUPABASE_PUBLISHABLE_KEY!, {
    global: { headers: { Authorization: `Bearer ${ctx.getToken()}` } },
    auth: { persistSession: false, autoRefreshToken: false },
  });
}

export default defineTool({
  name: "get_escrow",
  title: "Get escrow details",
  description: "Get full details for a single escrow the signed-in user participates in.",
  inputSchema: {
    escrow_id: z.string().uuid().describe("The escrow id (UUID)."),
  },
  annotations: { readOnlyHint: true, idempotentHint: true, openWorldHint: false },
  handler: async ({ escrow_id }, ctx) => {
    if (!ctx.isAuthenticated())
      return { content: [{ type: "text", text: "Not authenticated" }], isError: true };
    const client = supabaseForUser(ctx);
    const { data: escrow, error } = await client
      .from("escrows")
      .select("*")
      .eq("id", escrow_id)
      .maybeSingle();
    if (error) return { content: [{ type: "text", text: error.message }], isError: true };
    if (!escrow)
      return { content: [{ type: "text", text: "Escrow not found" }], isError: true };
    const { data: txs } = await client
      .from("escrow_transactions")
      .select("id, tx_hash, action, created_at")
      .eq("escrow_id", escrow_id)
      .order("created_at", { ascending: false });
    return {
      content: [{ type: "text", text: JSON.stringify({ escrow, transactions: txs ?? [] }, null, 2) }],
      structuredContent: { escrow, transactions: txs ?? [] },
    };
  },
});
