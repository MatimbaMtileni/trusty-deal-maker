#!/usr/bin/env bash
set -euo pipefail

PROJECT_REF="${SUPABASE_PROJECT_REF:-jqdrthsjqckptwbalpuj}"
FUNCTIONS=("cardano-tx-builder" "escrow-transactions")

if ! command -v supabase >/dev/null 2>&1; then
  echo "❌ Supabase CLI is not installed. Install it first: https://supabase.com/docs/guides/cli"
  exit 1
fi

if ! supabase projects list >/tmp/supabase_projects_list.out 2>/tmp/supabase_projects_list.err; then
  echo "❌ Unable to list Supabase projects with current CLI session."

  if grep -E "403|necessary privileges|access-control" /tmp/supabase_projects_list.err >/dev/null 2>&1; then
    echo ""
    echo "Likely cause: your account/token does not have required project privileges."
    echo "Fix steps:"
    echo "  1) Verify you are added to project '${PROJECT_REF}' with Admin/Owner role in Supabase dashboard"
    echo "  2) Re-authenticate CLI: supabase logout && supabase login"
    echo "  3) Link project explicitly: supabase link --project-ref ${PROJECT_REF}"
    echo "  4) Retry with debug: supabase functions deploy cardano-tx-builder --project-ref ${PROJECT_REF} --debug"
  else
    echo ""
    echo "CLI output:"
    cat /tmp/supabase_projects_list.err
  fi

  exit 1
fi

for fn in "${FUNCTIONS[@]}"; do
  echo "Deploying ${fn}..."
  supabase functions deploy "${fn}" --project-ref "${PROJECT_REF}"
done

echo "✅ Function deployment complete for project ${PROJECT_REF}."
