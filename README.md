# Trusty Deal Maker 🚀

**Trusty Deal Maker** is a Cardano escrow application built with React + Vite and backed by Supabase. It allows buyers and sellers to create, fund, and settle escrows, exchange messages and attachments, and receive email notifications for important events (via Resend).

---

## Key features 🔑

- Escrow lifecycle: create, fund, release, refund
- In-app messaging between buyer & seller (real-time via Supabase Realtime)
- File attachments per escrow (private storage bucket)
- Email notifications sent via Resend (Supabase Function integration)
- Cardano wallet integration (CIP-30) and optional wallet linking to user profile
- Strong security: Row-Level Security (RLS) policies protect messages and attachments

---

## Quickstart (local development) 💻

Prerequisites
- Node.js (LTS) and npm
- (Optional) Supabase CLI if you want to run/inspect Functions locally or deploy them

Install & run

```bash
# Install dependencies
npm install

# Start dev server
npm run dev
```

Open http://localhost:5173 (or the port shown) to view the app.

---

## Environment variables & secrets 🔐

There are two kinds of environment values:

1) Client (Vite) envs (used by the frontend)
- `VITE_SUPABASE_URL` (your Supabase project URL)
- `VITE_SUPABASE_PUBLISHABLE_KEY` (your Supabase anon/publishable key)

Set these in a `.env` (or via your hosting provider) so the frontend can connect to Supabase.

2) Server / Function envs (used by Supabase Functions)
- `SUPABASE_URL` (project URL) - available automatically in Supabase Functions environment
- `SUPABASE_SERVICE_ROLE_KEY` (Service Role key) — required by functions that need elevated privileges
- `RESEND_API_KEY` (Resend API key) — used by `send-notification` function to send emails
- `BLOCKFROST_API_KEY` (optional) — used by Cardano / blockchain functions when applicable

How to set function secrets
- Supabase Dashboard (recommended): Project → Settings → API / Secrets → Add key/value (e.g., `RESEND_API_KEY`).
- Supabase CLI: `supabase secrets set RESEND_API_KEY=your_real_key_here` (requires supabase CLI login).

Local function env
- For the `send-notification` function we include `supabase/functions/send-notification/.env.example`. Copy that to `.env` and set your `RESEND_API_KEY` for local testing.
- NOTE: The repo already contains `supabase/functions/send-notification/.env` locally (ignored by git). Do **not** commit real secrets.

---

## Messaging & Notifications behavior 📩

- Messaging uses `escrow_messages` table with RLS policies — you must be an authenticated Supabase user whose `profiles.wallet_address` matches your connected Cardano wallet address to insert and view messages. This prevents address impersonation.
- When a message is sent:
  - A local optimistic message appears instantly for the sender.
  - The message is saved to `escrow_messages` in Supabase.
  - The `escrows` row is updated (`last_message_preview`, `last_message_at`) so the recipient sees activity in lists immediately.
  - A `send-notification` Supabase Function is invoked to email the recipient using Resend (requires `RESEND_API_KEY`).

Troubleshooting messaging failures
- If sending fails with a permission error, ensure you are **signed in** (Auth) and have **linked your wallet** in your profile. The app attempts to auto-link your wallet when you connect it while signed in.
- Check the browser console for errors and check Supabase Function logs (Dashboard → Functions) for email send failures.

---

## Supabase functions & DB migrations ⚙️

This project includes Supabase Functions:
- `supabase/functions/send-notification` — sends email notifications (Resend)
- `supabase/functions/escrow-transactions` — handles secure escrow actions

Migrations are under `supabase/migrations/` and include tables, RLS policies, triggers, and realtime publication for `escrow_messages` and attachments.

Deployment (Functions)
- Deploy functions with the Supabase CLI:

```bash
supabase login
supabase functions deploy send-notification --project-ref <project-ref>
# same for other functions

# For this repo's escrow functions you can use:
./scripts/deploy-supabase-functions.sh
```

Or use the Supabase Dashboard to upload function code.

- If CLI deploy fails with `unexpected list functions status 403`, your account/token likely lacks project privileges or needs re-authentication. See `DEPLOY_TO_SUPABASE.md` troubleshooting for exact recovery steps.

---

## Testing & linters ✅

- Run tests: `npm run test` (uses Vitest)
- Run lint: `npm run lint`
- Build: `npm run build`

---

## Developer notes & tips 🛠️

- Wallet linking: when a user connects a CIP-30 Cardano wallet while signed in, the app calls `escrowApi.updateProfileWallet` to store `profiles.wallet_address` — this is required for messaging RLS.
- Email delivery: the `send-notification` function reads `RESEND_API_KEY` from the runtime env — if missing, email sending is skipped and a message is logged.
- If you want chat to work for un-authenticated wallets (not recommended), you can relax RLS rules but it will allow address impersonation. Use caution.

---

## Where to find things in the repo 🔎

- Frontend: `src/`
- Messaging UI: `src/components/escrow/EscrowChat.tsx`
- Supabase functions: `supabase/functions/`
  - `send-notification/index.ts` — Resend integration
  - `escrow-transactions/index.ts` — escrow action handlers
- DB migrations & RLS: `supabase/migrations/`
- Local example env for functions: `supabase/functions/send-notification/.env.example`

---

## Contribution & support 🤝

Contributions welcome. Open issues or PRs and include a short description and steps to reproduce. If you need help setting up Supabase secrets or deploying functions, I can provide step-by-step instructions.

---

## License

No license file detected in the repository — add a `LICENSE` if you want to define reuse terms.

---

If you'd like, I can also:
- Add a `CONTRIBUTING.md` with developer workflows, or
- Add a GitHub Action to run `npm test` and `npm run lint` on PRs.

Happy to help with the next step — what would you like me to do next? ✨
---

## Email notifications (Resend) 🔧

To enable email notifications for the `send-notification` Supabase Function, set your Resend API key in the function's environment as `RESEND_API_KEY`. **Do NOT commit your real key to the repository.**

- Supabase Dashboard (recommended):
  - Go to Project → Settings → API / Secrets and add `RESEND_API_KEY` with your key.
- Supabase CLI (if you use it):
  - Run `supabase secrets set RESEND_API_KEY=your_real_key_here`.

For local development, copy `supabase/functions/send-notification/.env.example` to `.env` and update the value. Keep that file out of version control.

If you want, I can show step-by-step how to set the secret in the dashboard.

