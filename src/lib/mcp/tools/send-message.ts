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
  name: "send_message",
  title: "Send escrow message",
  description:
    "Post a chat message to an escrow the signed-in user participates in. Uses the user's linked wallet address as sender.",
  inputSchema: {
    escrow_id: z.string().uuid(),
    content: z.string().trim().min(1).max(4000),
  },
  annotations: { readOnlyHint: false, destructiveHint: false, openWorldHint: false },
  handler: async ({ escrow_id, content }, ctx) => {
    if (!ctx.isAuthenticated())
      return { content: [{ type: "text", text: "Not authenticated" }], isError: true };
    const client = supabaseForUser(ctx);
    const userId = ctx.getUserId();
    const { data: profile } = await client
      .from("profiles")
      .select("wallet_address")
      .eq("user_id", userId)
      .maybeSingle();
    const sender = profile?.wallet_address;
    if (!sender)
      return {
        content: [
          {
            type: "text",
            text: "No wallet linked to your profile. Connect a wallet in the app first.",
          },
        ],
        isError: true,
      };
    const { data, error } = await client
      .from("escrow_messages")
      .insert({ escrow_id, sender_address: sender, content })
      .select()
      .single();
    if (error) return { content: [{ type: "text", text: error.message }], isError: true };
    return {
      content: [{ type: "text", text: `Message sent (id ${data.id})` }],
      structuredContent: { message: data },
    };
  },
});
