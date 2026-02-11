#!/bin/bash

# Trusty Deal Maker - Plutus Deployment Verification Script
# This script tests the local development environment setup

set -e

echo "=========================================="
echo "🔍 Plutus Deployment Verification"
echo "=========================================="
echo ""

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counter for passed/failed checks
PASSED=0
FAILED=0

# Helper functions
check_pass() {
  echo -e "${GREEN}✅ PASS${NC}: $1"
  ((PASSED++))
}

check_fail() {
  echo -e "${RED}❌ FAIL${NC}: $1"
  ((FAILED++))
}

check_warn() {
  echo -e "${YELLOW}⚠️  WARN${NC}: $1"
}

# Test 1: Check .env.local exists
echo ""
echo -e "${BLUE}[1/8]${NC} Checking .env.local..."
if [ -f .env.local ]; then
  check_pass ".env.local file exists"
else
  check_fail ".env.local file not found"
fi

# Test 2: Check ESCROW_SCRIPT_BASE64
echo ""
echo -e "${BLUE}[2/8]${NC} Checking ESCROW_SCRIPT_BASE64..."
if grep -q "^ESCROW_SCRIPT_BASE64=" .env.local; then
  BASE64=$(grep "^ESCROW_SCRIPT_BASE64=" .env.local | cut -d= -f2 | tr -d '\n')
  if [ ${#BASE64} -gt 10 ]; then
    check_pass "ESCROW_SCRIPT_BASE64 is set (${#BASE64} chars)"
    echo "   Value: ${BASE64:0:20}...${BASE64: -10}"
  else
    check_fail "ESCROW_SCRIPT_BASE64 is too short"
  fi
else
  check_fail "ESCROW_SCRIPT_BASE64 not found in .env.local"
fi

# Test 3: Check VITE_ESCROW_SCRIPT_BASE64
echo ""
echo -e "${BLUE}[3/8]${NC} Checking VITE_ESCROW_SCRIPT_BASE64 (Frontend)..."
if grep -q "^VITE_ESCROW_SCRIPT_BASE64=" .env.local; then
  VITE_BASE64=$(grep "^VITE_ESCROW_SCRIPT_BASE64=" .env.local | cut -d= -f2 | tr -d '\n')
  if [ ${#VITE_BASE64} -gt 10 ]; then
    check_pass "VITE_ESCROW_SCRIPT_BASE64 is set (${#VITE_BASE64} chars)"
  else
    check_fail "VITE_ESCROW_SCRIPT_BASE64 is too short"
  fi
else
  check_fail "VITE_ESCROW_SCRIPT_BASE64 not found in .env.local"
fi

# Test 4: Check ESCROW_SCRIPT_ADDRESS
echo ""
echo -e "${BLUE}[4/8]${NC} Checking ESCROW_SCRIPT_ADDRESS..."
if grep -q "^ESCROW_SCRIPT_ADDRESS=" .env.local; then
  ADDR=$(grep "^ESCROW_SCRIPT_ADDRESS=" .env.local | cut -d= -f2 | tr -d '\n')
  if [[ $ADDR == addr_test1* ]]; then
    check_pass "ESCROW_SCRIPT_ADDRESS is valid Preprod address"
    echo "   Address: ${ADDR:0:20}...${ADDR: -10}"
  else
    check_fail "ESCROW_SCRIPT_ADDRESS doesn't start with addr_test1"
  fi
else
  check_fail "ESCROW_SCRIPT_ADDRESS not found in .env.local"
fi

# Test 5: Check VITE_ESCROW_SCRIPT_ADDRESS
echo ""
echo -e "${BLUE}[5/8]${NC} Checking VITE_ESCROW_SCRIPT_ADDRESS (Frontend)..."
if grep -q "^VITE_ESCROW_SCRIPT_ADDRESS=" .env.local; then
  VITE_ADDR=$(grep "^VITE_ESCROW_SCRIPT_ADDRESS=" .env.local | cut -d= -f2 | tr -d '\n')
  if [[ $VITE_ADDR == addr_test1* ]]; then
    check_pass "VITE_ESCROW_SCRIPT_ADDRESS is valid Preprod address"
  else
    check_fail "VITE_ESCROW_SCRIPT_ADDRESS doesn't start with addr_test1"
  fi
else
  check_fail "VITE_ESCROW_SCRIPT_ADDRESS not found in .env.local"
fi

# Test 6: Check BLOCKFROST_API_KEY
echo ""
echo -e "${BLUE}[6/8]${NC} Checking BLOCKFROST_API_KEY..."
if grep -q "^.*BLOCKFROST_API_KEY=" .env.local; then
  KEY=$(grep "^.*BLOCKFROST_API_KEY=" .env.local | cut -d= -f2 | head -1)
  if [[ $KEY == preprod* ]]; then
    check_pass "BLOCKFROST_API_KEY looks valid"
    echo "   Key: ${KEY:0:15}...${KEY: -5}"
  else
    check_fail "BLOCKFROST_API_KEY doesn't start with 'preprod'"
  fi
else
  check_fail "BLOCKFROST_API_KEY not found in .env.local"
fi

# Test 7: Check scriptRegistry.ts is updated
echo ""
echo -e "${BLUE}[7/8]${NC} Checking scriptRegistry.ts configuration..."
if [ -f src/services/cardano/scriptRegistry.ts ]; then
  if grep -q "loadPlutusScript" src/services/cardano/scriptRegistry.ts; then
    check_pass "scriptRegistry.ts has Plutus V2 setup"
  else
    check_fail "scriptRegistry.ts doesn't have loadPlutusScript function"
  fi
  
  if grep -q "getScriptBase64" src/services/cardano/scriptRegistry.ts; then
    check_pass "scriptRegistry.ts exports getScriptBase64()"
  else
    check_fail "scriptRegistry.ts doesn't export getScriptBase64()"
  fi
else
  check_fail "scriptRegistry.ts not found"
fi

# Test 8: Check lucidService.ts exports
echo ""
echo -e "${BLUE}[8/8]${NC} Checking lucidService.ts exports..."
if [ -f src/services/lucidService.ts ]; then
  if grep -q "getScriptBase64" src/services/lucidService.ts; then
    check_pass "lucidService.ts exports script utilities"
  else
    check_fail "lucidService.ts doesn't export script utilities"
  fi
else
  check_fail "lucidService.ts not found"
fi

# Summary
echo ""
echo "=========================================="
echo "📊 Summary"
echo "=========================================="
echo -e "Passed: ${GREEN}$PASSED${NC}"
echo -e "Failed: ${RED}$FAILED${NC}"

if [ $FAILED -eq 0 ]; then
  echo ""
  echo -e "${GREEN}✅ All checks passed!${NC}"
  echo ""
  echo "Next steps:"
  echo "1. Open Supabase dashboard: https://supabase.com/dashboard/project/jqdrthsjqckptwbalpuj/settings/environment"
  echo "2. Add the three environment variables (see DEPLOY_TO_SUPABASE.md)"
  echo "3. Redeploy functions"
  echo "4. Run: npm run dev"
  echo ""
  exit 0
else
  echo ""
  echo -e "${RED}❌ Some checks failed!${NC}"
  echo ""
  echo "Fix the issues above before deploying to Supabase."
  echo "See PLUTUS_SCRIPT_DEPLOYMENT.md for help."
  echo ""
  exit 1
fi
