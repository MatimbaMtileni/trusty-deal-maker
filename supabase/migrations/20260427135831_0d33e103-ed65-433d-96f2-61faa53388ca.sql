
-- 1. Roles enum and table
CREATE TYPE public.app_role AS ENUM ('owner', 'admin', 'user');

CREATE TABLE public.user_roles (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  user_id uuid NOT NULL REFERENCES auth.users(id) ON DELETE CASCADE,
  role public.app_role NOT NULL,
  granted_by uuid,
  created_at timestamptz NOT NULL DEFAULT now(),
  UNIQUE (user_id, role)
);

ALTER TABLE public.user_roles ENABLE ROW LEVEL SECURITY;

-- Security-definer role check (avoids RLS recursion)
CREATE OR REPLACE FUNCTION public.has_role(_user_id uuid, _role public.app_role)
RETURNS boolean
LANGUAGE sql
STABLE
SECURITY DEFINER
SET search_path = public
AS $$
  SELECT EXISTS (
    SELECT 1 FROM public.user_roles
    WHERE user_id = _user_id AND role = _role
  )
$$;

-- Convenience: is_admin OR owner
CREATE OR REPLACE FUNCTION public.is_admin_or_owner(_user_id uuid)
RETURNS boolean
LANGUAGE sql
STABLE
SECURITY DEFINER
SET search_path = public
AS $$
  SELECT EXISTS (
    SELECT 1 FROM public.user_roles
    WHERE user_id = _user_id AND role IN ('admin','owner')
  )
$$;

-- RLS for user_roles
CREATE POLICY "Users can view own roles" ON public.user_roles
  FOR SELECT USING (auth.uid() = user_id);

CREATE POLICY "Admins can view all roles" ON public.user_roles
  FOR SELECT USING (public.is_admin_or_owner(auth.uid()));

CREATE POLICY "Owner can manage all roles" ON public.user_roles
  FOR ALL USING (public.has_role(auth.uid(), 'owner'))
  WITH CHECK (public.has_role(auth.uid(), 'owner'));

CREATE POLICY "Admins can grant non-owner roles" ON public.user_roles
  FOR INSERT WITH CHECK (
    public.is_admin_or_owner(auth.uid()) AND role <> 'owner'
  );

-- 2. Audit log
CREATE TABLE public.admin_audit_log (
  id uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  actor_user_id uuid NOT NULL,
  action text NOT NULL,
  target_type text,
  target_id text,
  metadata jsonb DEFAULT '{}'::jsonb,
  created_at timestamptz NOT NULL DEFAULT now()
);

ALTER TABLE public.admin_audit_log ENABLE ROW LEVEL SECURITY;

CREATE POLICY "Admins can view audit log" ON public.admin_audit_log
  FOR SELECT USING (public.is_admin_or_owner(auth.uid()));

CREATE POLICY "Admins can insert audit log" ON public.admin_audit_log
  FOR INSERT WITH CHECK (
    public.is_admin_or_owner(auth.uid()) AND auth.uid() = actor_user_id
  );

-- 3. Dispute fields on escrows
ALTER TABLE public.escrows
  ADD COLUMN IF NOT EXISTS disputed_at timestamptz,
  ADD COLUMN IF NOT EXISTS disputed_by uuid,
  ADD COLUMN IF NOT EXISTS dispute_reason text,
  ADD COLUMN IF NOT EXISTS resolved_by uuid,
  ADD COLUMN IF NOT EXISTS resolved_at timestamptz,
  ADD COLUMN IF NOT EXISTS resolution_note text;

-- 4. Admin RLS overrides on existing tables
CREATE POLICY "Admins can view all escrows" ON public.escrows
  FOR SELECT USING (public.is_admin_or_owner(auth.uid()));

CREATE POLICY "Admins can update any escrow" ON public.escrows
  FOR UPDATE USING (public.is_admin_or_owner(auth.uid()))
  WITH CHECK (public.is_admin_or_owner(auth.uid()));

CREATE POLICY "Admins can view all transactions" ON public.escrow_transactions
  FOR SELECT USING (public.is_admin_or_owner(auth.uid()));

CREATE POLICY "Admins can view all profiles" ON public.profiles
  FOR SELECT USING (public.is_admin_or_owner(auth.uid()));

CREATE POLICY "Admins can view all messages" ON public.escrow_messages
  FOR SELECT USING (public.is_admin_or_owner(auth.uid()));

CREATE POLICY "Admins can view all attachments" ON public.escrow_attachments
  FOR SELECT USING (public.is_admin_or_owner(auth.uid()));

-- 5. Seed first owner from email
DO $$
DECLARE
  v_user_id uuid;
BEGIN
  SELECT id INTO v_user_id FROM auth.users WHERE email = 'matimbamtileni12@gmail.com' LIMIT 1;
  IF v_user_id IS NOT NULL THEN
    INSERT INTO public.user_roles (user_id, role)
    VALUES (v_user_id, 'owner')
    ON CONFLICT DO NOTHING;
  END IF;
END $$;

-- 6. Helper view: list users with roles for admin UI
CREATE OR REPLACE VIEW public.admin_users_view
WITH (security_invoker = true) AS
SELECT
  p.user_id,
  p.display_name,
  p.wallet_address,
  p.created_at,
  COALESCE(
    array_agg(ur.role) FILTER (WHERE ur.role IS NOT NULL),
    ARRAY[]::public.app_role[]
  ) AS roles
FROM public.profiles p
LEFT JOIN public.user_roles ur ON ur.user_id = p.user_id
GROUP BY p.user_id, p.display_name, p.wallet_address, p.created_at;
