-- =====================================================================
-- 1. BEFORE UPDATE trigger: protect immutable columns on escrows
-- =====================================================================
CREATE OR REPLACE FUNCTION public.enforce_escrow_immutable_columns()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
SET search_path = public
AS $$
DECLARE
  is_admin boolean := false;
BEGIN
  -- Admins/owners (and service role / migrations where auth.uid() IS NULL) bypass
  IF auth.uid() IS NOT NULL THEN
    SELECT public.is_admin_or_owner(auth.uid()) INTO is_admin;
  ELSE
    is_admin := true;
  END IF;

  IF is_admin THEN
    RETURN NEW;
  END IF;

  -- Core economic terms — never editable post-creation by participants
  IF NEW.amount IS DISTINCT FROM OLD.amount THEN
    RAISE EXCEPTION 'amount is immutable' USING ERRCODE = 'insufficient_privilege';
  END IF;
  IF NEW.buyer_address IS DISTINCT FROM OLD.buyer_address THEN
    RAISE EXCEPTION 'buyer_address is immutable' USING ERRCODE = 'insufficient_privilege';
  END IF;
  IF NEW.seller_address IS DISTINCT FROM OLD.seller_address THEN
    RAISE EXCEPTION 'seller_address is immutable' USING ERRCODE = 'insufficient_privilege';
  END IF;
  IF NEW.deadline IS DISTINCT FROM OLD.deadline THEN
    RAISE EXCEPTION 'deadline is immutable' USING ERRCODE = 'insufficient_privilege';
  END IF;
  IF NEW.buyer_user_id IS DISTINCT FROM OLD.buyer_user_id THEN
    RAISE EXCEPTION 'buyer_user_id is immutable' USING ERRCODE = 'insufficient_privilege';
  END IF;

  -- On-chain anchors — set once at funding, never edited later
  IF OLD.script_address IS NOT NULL AND NEW.script_address IS DISTINCT FROM OLD.script_address THEN
    RAISE EXCEPTION 'script_address is immutable once set' USING ERRCODE = 'insufficient_privilege';
  END IF;
  IF OLD.utxo_tx_hash IS NOT NULL AND NEW.utxo_tx_hash IS DISTINCT FROM OLD.utxo_tx_hash THEN
    RAISE EXCEPTION 'utxo_tx_hash is immutable once set' USING ERRCODE = 'insufficient_privilege';
  END IF;
  IF OLD.utxo_output_index IS NOT NULL AND NEW.utxo_output_index IS DISTINCT FROM OLD.utxo_output_index THEN
    RAISE EXCEPTION 'utxo_output_index is immutable once set' USING ERRCODE = 'insufficient_privilege';
  END IF;

  -- Dispute / resolution audit trail — only admins (handled above)
  IF NEW.disputed_at IS DISTINCT FROM OLD.disputed_at
     OR NEW.disputed_by IS DISTINCT FROM OLD.disputed_by
     OR NEW.dispute_reason IS DISTINCT FROM OLD.dispute_reason
     OR NEW.dispute_reason_type IS DISTINCT FROM OLD.dispute_reason_type THEN
    -- Allow participants to flag a dispute (transitioning active -> disputed)
    -- only when these fields move from NULL to a value.
    IF OLD.disputed_at IS NOT NULL THEN
      RAISE EXCEPTION 'dispute fields are immutable once set' USING ERRCODE = 'insufficient_privilege';
    END IF;
  END IF;

  IF NEW.resolved_by IS DISTINCT FROM OLD.resolved_by
     OR NEW.resolved_at IS DISTINCT FROM OLD.resolved_at
     OR NEW.resolution_note IS DISTINCT FROM OLD.resolution_note THEN
    RAISE EXCEPTION 'resolution fields can only be set by admins' USING ERRCODE = 'insufficient_privilege';
  END IF;

  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS escrows_immutable_columns ON public.escrows;
CREATE TRIGGER escrows_immutable_columns
BEFORE UPDATE ON public.escrows
FOR EACH ROW
EXECUTE FUNCTION public.enforce_escrow_immutable_columns();

-- Also wire up the existing state-transition guard if not already
DROP TRIGGER IF EXISTS escrows_state_transition ON public.escrows;
CREATE TRIGGER escrows_state_transition
BEFORE UPDATE ON public.escrows
FOR EACH ROW
EXECUTE FUNCTION public.enforce_escrow_state_transition();


-- =====================================================================
-- 2. BEFORE INSERT trigger: validate new escrows
-- =====================================================================
CREATE OR REPLACE FUNCTION public.validate_escrow_insert()
RETURNS TRIGGER
LANGUAGE plpgsql
SECURITY DEFINER
SET search_path = public
AS $$
DECLARE
  bech32_re text := '^(addr1|addr_test1)[a-z0-9]{53,}$';
  hex_re    text := '^[a-fA-F0-9]{56,}$';
BEGIN
  -- Amount: 10 ADA .. 1,000,000 ADA (lovelace)
  IF NEW.amount < 10000000 OR NEW.amount > 1000000000000 THEN
    RAISE EXCEPTION 'amount must be between 10 and 1,000,000 ADA' USING ERRCODE = 'check_violation';
  END IF;

  -- Deadline: > 1 hour from now, <= 1 year from now
  IF NEW.deadline <= now() + interval '1 hour' THEN
    RAISE EXCEPTION 'deadline must be at least 1 hour in the future' USING ERRCODE = 'check_violation';
  END IF;
  IF NEW.deadline > now() + interval '1 year' THEN
    RAISE EXCEPTION 'deadline must be within 1 year' USING ERRCODE = 'check_violation';
  END IF;

  -- Address format
  IF NEW.buyer_address !~ bech32_re AND NEW.buyer_address !~ hex_re THEN
    RAISE EXCEPTION 'invalid buyer_address format' USING ERRCODE = 'check_violation';
  END IF;
  IF NEW.seller_address !~ bech32_re AND NEW.seller_address !~ hex_re THEN
    RAISE EXCEPTION 'invalid seller_address format' USING ERRCODE = 'check_violation';
  END IF;

  -- Buyer != seller
  IF NEW.buyer_address = NEW.seller_address THEN
    RAISE EXCEPTION 'buyer and seller must be different addresses' USING ERRCODE = 'check_violation';
  END IF;

  -- Description length
  IF NEW.description IS NOT NULL AND length(NEW.description) > 500 THEN
    RAISE EXCEPTION 'description must be 500 characters or less' USING ERRCODE = 'check_violation';
  END IF;

  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS escrows_validate_insert ON public.escrows;
CREATE TRIGGER escrows_validate_insert
BEFORE INSERT ON public.escrows
FOR EACH ROW
EXECUTE FUNCTION public.validate_escrow_insert();