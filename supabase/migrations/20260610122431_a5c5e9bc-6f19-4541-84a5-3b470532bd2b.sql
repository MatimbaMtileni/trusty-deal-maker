
DROP POLICY IF EXISTS "Buyer can view pending release" ON public.escrow_pending_release;

CREATE POLICY "Participants can view pending release"
ON public.escrow_pending_release FOR SELECT TO authenticated
USING (
  EXISTS (
    SELECT 1 FROM public.escrows e
    LEFT JOIN public.profiles p ON p.user_id = auth.uid()
    WHERE e.id = escrow_pending_release.escrow_id
      AND (
        e.buyer_user_id = auth.uid()
        OR e.seller_user_id = auth.uid()
        OR e.buyer_address = p.wallet_address
        OR e.seller_address = p.wallet_address
      )
  )
);
