import React, { useEffect, useState } from "react";
import { useSearchParams } from "react-router-dom";
import { supabase } from "@/integrations/supabase/client";
import { Button } from "@/components/ui/button";
import { Loader2 } from "lucide-react";

// Typed wrapper for the beta supabase.auth.oauth namespace.
type OAuthClient = { name?: string; client_name?: string; redirect_uri?: string };
type AuthorizationDetails = {
  client?: OAuthClient;
  scope?: string;
  redirect_url?: string;
  redirect_to?: string;
};
interface OAuthApi {
  getAuthorizationDetails: (id: string) => Promise<{ data: AuthorizationDetails | null; error: { message: string } | null }>;
  approveAuthorization: (id: string) => Promise<{ data: AuthorizationDetails | null; error: { message: string } | null }>;
  denyAuthorization: (id: string) => Promise<{ data: AuthorizationDetails | null; error: { message: string } | null }>;
}
const oauth = (supabase.auth as unknown as { oauth: OAuthApi }).oauth;

const OAuthConsent: React.FC = () => {
  const [params] = useSearchParams();
  const authorizationId = params.get("authorization_id") ?? "";
  const [details, setDetails] = useState<AuthorizationDetails | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [busy, setBusy] = useState(false);

  useEffect(() => {
    let active = true;
    (async () => {
      if (!authorizationId) return setError("Missing authorization_id");
      const { data: sess } = await supabase.auth.getSession();
      if (!sess.session) {
        const next = window.location.pathname + window.location.search;
        window.location.href = "/auth?next=" + encodeURIComponent(next);
        return;
      }
      const { data, error } = await oauth.getAuthorizationDetails(authorizationId);
      if (!active) return;
      if (error) return setError(error.message);
      const immediate = data?.redirect_url ?? data?.redirect_to;
      if (immediate && !data?.client) {
        window.location.href = immediate;
        return;
      }
      setDetails(data);
    })();
    return () => {
      active = false;
    };
  }, [authorizationId]);

  async function decide(approve: boolean) {
    setBusy(true);
    const { data, error } = approve
      ? await oauth.approveAuthorization(authorizationId)
      : await oauth.denyAuthorization(authorizationId);
    if (error) {
      setBusy(false);
      return setError(error.message);
    }
    const target = data?.redirect_url ?? data?.redirect_to;
    if (!target) {
      setBusy(false);
      return setError("No redirect returned by the authorization server.");
    }
    window.location.href = target;
  }

  if (error)
    return (
      <main className="min-h-screen pt-24 flex items-center justify-center px-4">
        <div className="glass-card-glow p-8 max-w-md w-full text-center">
          <h1 className="text-xl font-bold mb-2">Authorization error</h1>
          <p className="text-sm text-muted-foreground">{error}</p>
        </div>
      </main>
    );
  if (!details)
    return (
      <main className="min-h-screen pt-24 flex items-center justify-center">
        <Loader2 className="h-6 w-6 animate-spin" />
      </main>
    );

  const clientName = details.client?.client_name ?? details.client?.name ?? "an app";

  return (
    <main className="min-h-screen pt-24 flex items-center justify-center px-4">
      <div className="glass-card-glow p-8 max-w-md w-full">
        <h1 className="text-2xl font-bold mb-2">Connect {clientName}</h1>
        <p className="text-sm text-muted-foreground mb-6">
          This lets {clientName} use Trusty Deal Maker as you — reading your escrows, messages, and
          posting on your behalf. This does not bypass this app's permissions or backend policies.
        </p>
        {details.scope ? (
          <p className="text-xs text-muted-foreground mb-6">Requested scope: {details.scope}</p>
        ) : null}
        <div className="flex gap-3">
          <Button className="btn-gradient flex-1" disabled={busy} onClick={() => decide(true)}>
            Approve
          </Button>
          <Button variant="outline" className="flex-1" disabled={busy} onClick={() => decide(false)}>
            Cancel connection
          </Button>
        </div>
      </div>
    </main>
  );
};

export default OAuthConsent;
