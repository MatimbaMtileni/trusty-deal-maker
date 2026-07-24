import { auth, defineMcp } from "@lovable.dev/mcp-js";
import listEscrows from "./tools/list-escrows";
import getEscrow from "./tools/get-escrow";
import listMessages from "./tools/list-messages";
import sendMessage from "./tools/send-message";

const projectRef = import.meta.env.VITE_SUPABASE_PROJECT_ID ?? "project-ref-unset";

export default defineMcp({
  name: "trusty-deal-maker-mcp",
  title: "Trusty Deal Maker",
  version: "0.1.0",
  instructions:
    "Tools for the Trusty Deal Maker Cardano escrow app. Use `list_escrows` to see the signed-in user's escrows, `get_escrow` for details, `list_messages` to read chat, and `send_message` to post a message.",
  auth: auth.oauth.issuer({
    issuer: `https://${projectRef}.supabase.co/auth/v1`,
    acceptedAudiences: "authenticated",
  }),
  tools: [listEscrows, getEscrow, listMessages, sendMessage],
});
