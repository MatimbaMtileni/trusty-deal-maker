
-- =========================================================
-- 1) PRIVILEGE_ESCALATION_ADMIN_ROLE
-- Restrict admin grants to owner-only. Admins can still grant
-- non-admin / non-owner roles (e.g. 'user').
-- =========================================================
DROP POLICY IF EXISTS "Admins can grant non-owner roles" ON public.user_roles;

CREATE POLICY "Admins can grant basic roles"
ON public.user_roles
FOR INSERT
TO authenticated
WITH CHECK (
  public.is_admin_or_owner(auth.uid())
  AND role NOT IN ('owner'::app_role, 'admin'::app_role)
);

-- Only owners can grant or revoke admin/owner. (Owner ALL policy already exists.)

-- Prevent users from deleting/updating their own role rows to escalate.
-- (No UPDATE/DELETE policies exist for non-owners — owner ALL covers owner.)

-- =========================================================
-- 2) REALTIME_UNAUTHORIZED_CHANNEL_SUBSCRIPTION
-- Enable RLS on realtime.messages and restrict channel topics
-- to escrow participants only.
-- =========================================================
ALTER TABLE realtime.messages ENABLE ROW LEVEL SECURITY;

DROP POLICY IF EXISTS "Escrow participants can read realtime" ON realtime.messages;
CREATE POLICY "Escrow participants can read realtime"
ON realtime.messages
FOR SELECT
TO authenticated
USING (
  EXISTS (
    SELECT 1 FROM public.escrows e
    LEFT JOIN public.profiles p ON p.user_id = auth.uid()
    WHERE (realtime.topic() = 'escrow:' || e.id::text
           OR realtime.topic() = 'escrow_messages:' || e.id::text
           OR realtime.topic() = e.id::text)
      AND (
        e.buyer_user_id = auth.uid()
        OR e.seller_user_id = auth.uid()
        OR e.buyer_address = p.wallet_address
        OR e.seller_address = p.wallet_address
      )
  )
);

DROP POLICY IF EXISTS "Escrow participants can broadcast realtime" ON realtime.messages;
CREATE POLICY "Escrow participants can broadcast realtime"
ON realtime.messages
FOR INSERT
TO authenticated
WITH CHECK (
  EXISTS (
    SELECT 1 FROM public.escrows e
    LEFT JOIN public.profiles p ON p.user_id = auth.uid()
    WHERE (realtime.topic() = 'escrow:' || e.id::text
           OR realtime.topic() = 'escrow_messages:' || e.id::text
           OR realtime.topic() = e.id::text)
      AND (
        e.buyer_user_id = auth.uid()
        OR e.seller_user_id = auth.uid()
        OR e.buyer_address = p.wallet_address
        OR e.seller_address = p.wallet_address
      )
  )
);
