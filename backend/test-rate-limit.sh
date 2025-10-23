#!/bin/bash

# Test script for Rate Limiting implementation (T230)
# Caixa Seguradora Premium Reporting API

set -e

API_URL="https://localhost:5001"
HEALTH_ENDPOINT="$API_URL/api/v1/health/ready"

echo "=========================================="
echo "Rate Limiting Test - T230"
echo "=========================================="
echo ""

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${YELLOW}Testing general rate limit (100 requests/minute)${NC}"
echo "Sending 105 requests to $HEALTH_ENDPOINT..."
echo ""

SUCCESS_COUNT=0
RATE_LIMITED_COUNT=0

for i in {1..105}; do
    HTTP_CODE=$(curl -k -s -o /dev/null -w "%{http_code}" "$HEALTH_ENDPOINT" 2>/dev/null)

    if [ "$HTTP_CODE" = "200" ]; then
        SUCCESS_COUNT=$((SUCCESS_COUNT + 1))
        if [ $i -eq 1 ] || [ $i -eq 50 ] || [ $i -eq 100 ]; then
            echo -e "  Request $i: ${GREEN}✓ 200 OK${NC}"
        fi
    elif [ "$HTTP_CODE" = "429" ]; then
        RATE_LIMITED_COUNT=$((RATE_LIMITED_COUNT + 1))
        if [ $RATE_LIMITED_COUNT -eq 1 ]; then
            echo -e "  Request $i: ${RED}✗ 429 Too Many Requests (Rate limit hit!)${NC}"

            # Get full response for first rate-limited request
            RESPONSE=$(curl -k -s "$HEALTH_ENDPOINT" 2>/dev/null)
            RETRY_AFTER=$(curl -k -s -D - -o /dev/null "$HEALTH_ENDPOINT" 2>/dev/null | grep -i "Retry-After" | cut -d' ' -f2 | tr -d '\r')

            echo ""
            echo "  Response headers:"
            curl -k -s -D - -o /dev/null "$HEALTH_ENDPOINT" 2>/dev/null | grep -E "X-Rate-Limit|Retry-After" | sed 's/^/    /'
            echo ""
        fi
    else
        echo -e "  Request $i: ${YELLOW}? $HTTP_CODE${NC}"
    fi
done

echo ""
echo "=========================================="
echo "Results:"
echo "=========================================="
echo -e "${GREEN}Successful requests (200):${NC} $SUCCESS_COUNT"
echo -e "${RED}Rate limited requests (429):${NC} $RATE_LIMITED_COUNT"
echo ""

if [ $RATE_LIMITED_COUNT -gt 0 ]; then
    echo -e "${GREEN}✓ Rate limiting is working correctly!${NC}"
    echo ""
    echo "Expected behavior:"
    echo "  - First 100 requests: 200 OK"
    echo "  - Remaining 5 requests: 429 Too Many Requests"
    echo "  - Response includes Retry-After header"
    echo "  - Response includes X-Rate-Limit-* headers"
else
    echo -e "${YELLOW}⚠ Rate limiting may not be working as expected${NC}"
    echo "Expected at least 5 rate-limited responses"
fi

echo ""
echo "=========================================="
echo ""
echo -e "${YELLOW}Testing authentication endpoint rate limit (5 requests/5 minutes)${NC}"
echo "Sending 6 login attempts..."
echo ""

LOGIN_ENDPOINT="$API_URL/api/v1/auth/login"
LOGIN_SUCCESS=0
LOGIN_RATE_LIMITED=0

for i in {1..6}; do
    HTTP_CODE=$(curl -k -s -o /dev/null -w "%{http_code}" \
        -X POST "$LOGIN_ENDPOINT" \
        -H "Content-Type: application/json" \
        -d '{"username": "test", "password": "wrongpassword"}' \
        2>/dev/null)

    if [ "$HTTP_CODE" = "401" ] || [ "$HTTP_CODE" = "400" ]; then
        LOGIN_SUCCESS=$((LOGIN_SUCCESS + 1))
        echo -e "  Login attempt $i: ${GREEN}✓ Processed (within rate limit)${NC}"
    elif [ "$HTTP_CODE" = "429" ]; then
        LOGIN_RATE_LIMITED=$((LOGIN_RATE_LIMITED + 1))
        echo -e "  Login attempt $i: ${RED}✗ 429 Rate Limited${NC}"

        if [ $LOGIN_RATE_LIMITED -eq 1 ]; then
            echo ""
            echo "  Rate limit headers:"
            curl -k -s -D - -o /dev/null \
                -X POST "$LOGIN_ENDPOINT" \
                -H "Content-Type: application/json" \
                -d '{"username": "test", "password": "wrongpassword"}' \
                2>/dev/null | grep -E "X-Rate-Limit|Retry-After" | sed 's/^/    /'
            echo ""
        fi
    else
        echo -e "  Login attempt $i: ${YELLOW}? $HTTP_CODE${NC}"
    fi

    sleep 0.5
done

echo ""
echo "=========================================="
echo "Login Endpoint Results:"
echo "=========================================="
echo -e "${GREEN}Processed attempts:${NC} $LOGIN_SUCCESS"
echo -e "${RED}Rate limited attempts:${NC} $LOGIN_RATE_LIMITED"
echo ""

if [ $LOGIN_RATE_LIMITED -gt 0 ]; then
    echo -e "${GREEN}✓ Authentication rate limiting is working!${NC}"
    echo ""
    echo "This protects against brute force attacks by limiting"
    echo "login attempts to 5 per 5 minutes per IP address."
else
    echo -e "${YELLOW}⚠ Authentication rate limiting may need adjustment${NC}"
fi

echo ""
echo "=========================================="
echo "Rate Limiting Configuration Summary"
echo "=========================================="
echo ""
echo "General Limits:"
echo "  • 100 requests/minute per IP"
echo "  • 500 requests/15 minutes per IP"
echo "  • 1500 requests/hour per IP"
echo ""
echo "Endpoint-Specific Limits:"
echo "  • Authentication: 5-10 requests/5 minutes"
echo "  • Report Generation: 3-5 requests/minute"
echo "  • Query Endpoints: 30 requests/minute"
echo "  • Mock Data: 10 requests/minute"
echo "  • Batch Jobs: 5-20 requests/minute"
echo ""
echo "All responses include rate limit headers:"
echo "  • X-Rate-Limit-Limit"
echo "  • X-Rate-Limit-Remaining"
echo "  • X-Rate-Limit-Reset"
echo "  • X-Rate-Limit-Policy"
echo ""
echo "Rate limited responses (429) include:"
echo "  • Retry-After header (seconds to wait)"
echo "  • Portuguese error message"
echo ""
echo "For more details, see:"
echo "  backend/src/CaixaSeguradora.Api/RATE_LIMITING_GUIDE.md"
echo ""
