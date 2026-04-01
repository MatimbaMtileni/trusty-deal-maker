CREATE POLICY "Seller can claim seller_user_id"
ON public.escrows
FOR UPDATE
TO authenticated
USING (
  seller_user_id IS NULL
  AND seller_address IN (
    SELECT wallet_address FROM public.profiles WHERE user_id = auth.uid()
  )
)
WITH CHECK (
  seller_user_id = auth.uid()
);