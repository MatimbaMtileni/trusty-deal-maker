
-- 1) Separate table for sensitive pending release data
CREATE TABLE IF NOT EXISTS public.escrow_pending_release (
  escrow_id uuid PRIMARY KEY REFERENCES public.escrows(id) ON DELETE CASCADE,
  tx_cbor text NOT NULL,
  buyer_witness text NOT NULL,
  script_witness text,
  created_at timestamptz NOT NULL DEFAULT now(),
  updated_at timestamptz NOT NULL DEFAULT now()
);

GRANT SELECT, INSERT, UPDATE, DELETE ON public.escrow_pending_release TO authenticated;
GRANT ALL ON public.escrow_pending_release TO service_role;

ALTER TABLE public.escrow_pending_release ENABLE ROW LEVEL SECURITY;

-- Only buyer (by user_id or wallet) can read / mutate pending release artifacts
CREATE POLICY "Buyer can view pending release"
ON public.escrow_pending_release FOR SELECT TO authenticated
USING (
  EXISTS (
    SELECT 1 FROM public.escrows e
    LEFT JOIN public.profiles p ON p.user_id = auth.uid()
    WHERE e.id = escrow_pending_release.escrow_id
      AND (e.buyer_user_id = auth.uid() OR e.buyer_address = p.wallet_address)
  )
);

CREATE POLICY "Buyer can insert pending release"
ON public.escrow_pending_release FOR INSERT TO authenticated
WITH CHECK (
  EXISTS (
    SELECT 1 FROM public.escrows e
    LEFT JOIN public.profiles p ON p.user_id = auth.uid()
    WHERE e.id = escrow_pending_release.escrow_id
      AND (e.buyer_user_id = auth.uid() OR e.buyer_address = p.wallet_address)
  )
);

CREATE POLICY "Buyer can delete pending release"
ON public.escrow_pending_release FOR DELETE TO authenticated
USING (
  EXISTS (
    SELECT 1 FROM public.escrows e
    LEFT JOIN public.profiles p ON p.user_id = auth.uid()
    WHERE e.id = escrow_pending_release.escrow_id
      AND (e.buyer_user_id = auth.uid() OR e.buyer_address = p.wallet_address)
  )
);

-- Admins can view (for support / dispute resolution)
CREATE POLICY "Admins can view pending release"
ON public.escrow_pending_release FOR SELECT TO authenticated
USING (public.is_admin_or_owner(auth.uid()));

-- Updated_at trigger
CREATE TRIGGER trg_escrow_pending_release_updated_at
BEFORE UPDATE ON public.escrow_pending_release
FOR EACH ROW EXECUTE FUNCTION public.update_updated_at_column();

-- 2) Migrate existing data from escrows into the new table
INSERT INTO public.escrow_pending_release (escrow_id, tx_cbor, buyer_witness, script_witness)
SELECT id, pending_release_tx_cbor, pending_release_buyer_witness, pending_release_script_witness
FROM public.escrows
WHERE pending_release_tx_cbor IS NOT NULL
  AND pending_release_buyer_witness IS NOT NULL
ON CONFLICT (escrow_id) DO NOTHING;

-- 3) Drop sensitive columns from escrows
ALTER TABLE public.escrows DROP COLUMN IF EXISTS pending_release_tx_cbor;
ALTER TABLE public.escrows DROP COLUMN IF EXISTS pending_release_buyer_witness;
ALTER TABLE public.escrows DROP COLUMN IF EXISTS pending_release_script_witness;

-- 4) Prevent buyer/seller from forging the other party's signed_at column
CREATE OR REPLACE FUNCTION public.enforce_escrow_signature_ownership()
RETURNS trigger
LANGUAGE plpgsql
SECURITY DEFINER
SET search_path = public
AS $$
DECLARE
  is_admin boolean := false;
BEGIN
  IF auth.uid() IS NULL THEN
    RETURN NEW; -- service role / migrations
  END IF;

  SELECT public.is_admin_or_owner(auth.uid()) INTO is_admin;
  IF is_admin THEN
    RETURN NEW;
  END IF;

  -- Only the buyer (matched by buyer_user_id) may set/change buyer_signed_at
  IF NEW.buyer_signed_at IS DISTINCT FROM OLD.buyer_signed_at THEN
    IF NEW.buyer_user_id IS NULL OR auth.uid() <> NEW.buyer_user_id THEN
      RAISE EXCEPTION 'Only the buyer can set buyer_signed_at'
        USING ERRCODE = 'insufficient_privilege';
    END IF;
  END IF;

  -- Only the seller (matched by seller_user_id) may set/change seller_signed_at
  IF NEW.seller_signed_at IS DISTINCT FROM OLD.seller_signed_at THEN
    IF NEW.seller_user_id IS NULL OR auth.uid() <> NEW.seller_user_id THEN
      RAISE EXCEPTION 'Only the seller can set seller_signed_at'
        USING ERRCODE = 'insufficient_privilege';
    END IF;
  END IF;

  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS trg_enforce_escrow_signature_ownership ON public.escrows;
CREATE TRIGGER trg_enforce_escrow_signature_ownership
BEFORE UPDATE ON public.escrows
FOR EACH ROW EXECUTE FUNCTION public.enforce_escrow_signature_ownership();

-- 5) Storage UPDATE policy mirroring DELETE for escrow-documents
CREATE POLICY "Uploaders can update own escrow documents"
ON storage.objects FOR UPDATE TO authenticated
USING (
  bucket_id = 'escrow-documents'
  AND auth.uid() IS NOT NULL
  AND (storage.foldername(name))[1] IN (
    SELECT (e.id)::text FROM public.escrows e
    JOIN public.escrow_attachments ea ON ea.escrow_id = e.id
    JOIN public.profiles p ON p.user_id = auth.uid()
    WHERE ea.file_path = objects.name AND ea.uploader_address = p.wallet_address
  )
)
WITH CHECK (
  bucket_id = 'escrow-documents'
  AND auth.uid() IS NOT NULL
  AND (storage.foldername(name))[1] IN (
    SELECT (e.id)::text FROM public.escrows e
    JOIN public.escrow_attachments ea ON ea.escrow_id = e.id
    JOIN public.profiles p ON p.user_id = auth.uid()
    WHERE ea.file_path = objects.name AND ea.uploader_address = p.wallet_address
  )
);
