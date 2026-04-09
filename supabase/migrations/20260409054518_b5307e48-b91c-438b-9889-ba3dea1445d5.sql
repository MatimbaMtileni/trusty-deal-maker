-- Add wallet-based SELECT policy for escrow_transactions so seller can read back after insert
CREATE POLICY "Participant by wallet can view transactions"
ON public.escrow_transactions
FOR SELECT
TO authenticated
USING (
  escrow_id IN (
    SELECT e.id FROM public.escrows e
    WHERE e.buyer_address IN (SELECT wallet_address FROM public.profiles WHERE user_id = auth.uid())
       OR e.seller_address IN (SELECT wallet_address FROM public.profiles WHERE user_id = auth.uid())
  )
);