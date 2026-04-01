
-- Allow participants matched by wallet address to update escrow (covers seller without seller_user_id)
CREATE POLICY "Participant by wallet can update escrow"
ON public.escrows
FOR UPDATE
TO authenticated
USING (
  buyer_address IN (SELECT wallet_address FROM public.profiles WHERE user_id = auth.uid())
  OR seller_address IN (SELECT wallet_address FROM public.profiles WHERE user_id = auth.uid())
)
WITH CHECK (
  buyer_address IN (SELECT wallet_address FROM public.profiles WHERE user_id = auth.uid())
  OR seller_address IN (SELECT wallet_address FROM public.profiles WHERE user_id = auth.uid())
);

-- Allow participants matched by wallet address to insert transactions
CREATE POLICY "Participant by wallet can insert transactions"
ON public.escrow_transactions
FOR INSERT
TO authenticated
WITH CHECK (
  escrow_id IN (
    SELECT e.id FROM public.escrows e
    WHERE e.buyer_address IN (SELECT wallet_address FROM public.profiles WHERE user_id = auth.uid())
       OR e.seller_address IN (SELECT wallet_address FROM public.profiles WHERE user_id = auth.uid())
  )
);
