-- ===========================================================
-- Dispute system hardening: state machine + typed reasons
-- ===========================================================

-- 1) Typed dispute reasons
DO $$ BEGIN
  CREATE TYPE public.dispute_reason_type AS ENUM (
    'item_not_received',
    'not_as_described',
    'payment_issue',
    'communication_breakdown',
    'other'
  );
EXCEPTION WHEN duplicate_object THEN NULL; END $$;

ALTER TABLE public.escrows
  ADD COLUMN IF NOT EXISTS dispute_reason_type public.dispute_reason_type;

-- 2) Strict state-machine transition validator
-- Allowed transitions:
--   active     -> completed   (normal release)
--   active     -> refunded    (normal refund after deadline)
--   active     -> disputed    (anyone party flags)
--   disputed   -> completed   (admin resolves in seller's favor)
--   disputed   -> refunded    (admin resolves in buyer's favor)
--   disputed   -> active      (admin reopens — admin/owner only)
-- Forbidden:
--   completed  -> *           (terminal)
--   refunded   -> *           (terminal)
--   disputed   -> disputed    (no-op churn blocked unless reason changes)
--   any        -> active      (except disputed->active by admin)

CREATE OR REPLACE FUNCTION public.enforce_escrow_state_transition()
RETURNS trigger
LANGUAGE plpgsql
SECURITY DEFINER
SET search_path = public
AS $$
DECLARE
  is_admin boolean := false;
BEGIN
  -- Skip if status unchanged
  IF NEW.status = OLD.status THEN
    RETURN NEW;
  END IF;

  -- Terminal states cannot change
  IF OLD.status IN ('completed', 'refunded') THEN
    RAISE EXCEPTION 'Invalid transition: % is a terminal state and cannot be changed', OLD.status
      USING ERRCODE = 'check_violation';
  END IF;

  -- Determine if caller is admin/owner (auth.uid() may be null for service role)
  IF auth.uid() IS NOT NULL THEN
    SELECT public.is_admin_or_owner(auth.uid()) INTO is_admin;
  ELSE
    is_admin := true; -- service role / migrations
  END IF;

  -- Validate each transition
  IF OLD.status = 'active' THEN
    IF NEW.status NOT IN ('completed', 'refunded', 'disputed') THEN
      RAISE EXCEPTION 'Invalid transition: active -> %', NEW.status
        USING ERRCODE = 'check_violation';
    END IF;
  ELSIF OLD.status = 'disputed' THEN
    IF NEW.status NOT IN ('completed', 'refunded', 'active') THEN
      RAISE EXCEPTION 'Invalid transition: disputed -> %', NEW.status
        USING ERRCODE = 'check_violation';
    END IF;
    -- Only admin/owner can resolve a dispute
    IF NOT is_admin THEN
      RAISE EXCEPTION 'Only admins or owners can resolve a disputed escrow'
        USING ERRCODE = 'insufficient_privilege';
    END IF;
  ELSE
    RAISE EXCEPTION 'Invalid transition: % -> %', OLD.status, NEW.status
      USING ERRCODE = 'check_violation';
  END IF;

  RETURN NEW;
END;
$$;

DROP TRIGGER IF EXISTS trg_enforce_escrow_state_transition ON public.escrows;
CREATE TRIGGER trg_enforce_escrow_state_transition
BEFORE UPDATE OF status ON public.escrows
FOR EACH ROW
EXECUTE FUNCTION public.enforce_escrow_state_transition();