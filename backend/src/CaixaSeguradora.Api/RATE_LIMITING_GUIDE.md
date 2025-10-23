# Rate Limiting Guide - Caixa Seguradora Premium Reporting API

## Overview

This API implements comprehensive rate limiting using AspNetCoreRateLimit to protect against abuse while maintaining availability for legitimate users.

## Implementation Details

### Package
- **AspNetCoreRateLimit 5.0.0**: IP-based rate limiting middleware for ASP.NET Core

### Configuration Location
- **Production**: `appsettings.json`
- **Development**: `appsettings.Development.json`

### Middleware Order
Rate limiting is positioned strategically in the middleware pipeline:
1. Global Exception Handler
2. Request Logging
3. HTTPS Redirection
4. CORS
5. **Rate Limiting** ← positioned here
6. Rate Limit Headers
7. Authentication
8. Authorization

## Rate Limiting Policies

### General Limits (All Endpoints)
- **100 requests/minute** per IP
- **500 requests/15 minutes** per IP
- **1500 requests/hour** per IP

### Authentication Endpoints (`/api/v1/auth/*`)
**Purpose**: Prevent brute force attacks
- **10 requests/5 minutes** per IP (all auth endpoints)
- **5 requests/5 minutes** per IP (login endpoint only)

### Report Generation (`/api/v1/reports/generate/*`)
**Purpose**: Protect expensive CPU/memory operations
- **5 requests/minute** per IP (all report generation)
- **3 requests/minute** per IP (PREMIT generation)
- **3 requests/minute** per IP (PREMCED generation)

### Mock Data Operations (`/api/v1/mock-data/*`)
**Purpose**: Limit data manipulation operations
- **10 requests/minute** per IP (all mock data endpoints)
- **5 requests/5 minutes** per IP (data loading)
- **3 requests/5 minutes** per IP (database reset)

### Query Endpoints
**Purpose**: Manage database load
- **30 requests/minute** per IP for:
  - `/api/v1/premiums/*`
  - `/api/v1/policies/*`
  - `/api/v1/products/*`
  - `/api/v1/clients/*`

### Batch Jobs (`/api/v1/batch-jobs/*`)
**Purpose**: Control background job creation
- **20 requests/minute** per IP (query batch jobs)
- **5 requests/minute** per IP (create batch jobs)

### Dashboard (`/api/v1/dashboard/*`)
**Purpose**: Manage dashboard data load
- **20 requests/minute** per IP

### Export Operations (`/api/v1/export/*`)
**Purpose**: Control file export operations
- **10 requests/minute** per IP

### Whitelisted Endpoints (No Rate Limit)
- `GET /api/v1/health`
- `GET /api/v1/health/ready`
- `GET /swagger/*` (development only)

## Response Headers

All API responses include rate limiting headers:

```http
X-Rate-Limit-Limit: 100                  # Maximum requests allowed
X-Rate-Limit-Remaining: 95               # Requests remaining
X-Rate-Limit-Reset: 1729687200           # Unix timestamp when limit resets
X-Rate-Limit-Policy: 100 per 1m          # Policy description
```

## 429 Too Many Requests Response

When rate limit is exceeded:

**Status Code**: `429 Too Many Requests`

**Headers**:
```http
Retry-After: 60                          # Seconds to wait before retry
X-Rate-Limit-Limit: 100
X-Rate-Limit-Remaining: 0
X-Rate-Limit-Reset: 1729687200
```

**Response Body** (Portuguese):
```json
{
  "statusCode": 429,
  "message": "Limite de requisições excedido. Tente novamente em 60 segundos.",
  "timestamp": "2025-10-23T15:30:00Z"
}
```

## Monitoring & Logging

### Rate Limit Violations
All rate limit violations are logged with warning level:

```
[WARN] Rate limit exceeded for POST /api/v1/auth/login from IP 192.168.1.100. Retry after 300s
```

### Log Data Includes
- HTTP method and path
- Client IP address
- Retry-After duration

### Recommended Monitoring
1. **Track violation frequency**: Monitor warning logs for suspicious patterns
2. **Identify top violators**: Aggregate logs by IP address
3. **Endpoint-specific analysis**: Which endpoints are being abused most
4. **Time-based patterns**: Identify attack patterns (time of day, burst patterns)

## Development vs Production

### Development (`appsettings.Development.json`)
**More permissive limits for testing**:
- General: 200 requests/minute (vs 100 in production)
- Whitelisted IPs: `127.0.0.1`, `::1` (localhost)
- Whitelisted endpoints include Swagger: `GET /swagger/*`

### Production (`appsettings.json`)
**Stricter limits**:
- General: 100 requests/minute
- Endpoint-specific limits enforced strictly
- No IP whitelisting by default

## Configuration Options

### IP Whitelisting
Add trusted IPs to bypass rate limiting:

```json
"IpWhitelist": [
  "192.168.1.100",
  "10.0.0.5"
]
```

### Client Whitelisting
Add trusted client IDs (requires authentication):

```json
"ClientWhitelist": [
  "admin-client-id",
  "monitoring-service"
]
```

### Custom Rules
Add or modify endpoint-specific rules:

```json
"EndpointRules": [
  {
    "Endpoint": "post:/api/v1/your-endpoint",
    "Period": "1m",
    "Limit": 10
  }
]
```

### Period Formats
- `s` = seconds (e.g., `30s` = 30 seconds)
- `m` = minutes (e.g., `5m` = 5 minutes)
- `h` = hours (e.g., `1h` = 1 hour)
- `d` = days (e.g., `1d` = 1 day)

## Testing Rate Limits

### Manual Testing with curl

**Test general rate limit (100/minute)**:
```bash
# Send 101 requests rapidly
for i in {1..101}; do
  curl -s -o /dev/null -w "%{http_code}\n" http://localhost:5000/api/v1/health/ready
done
# First 100 should return 200, 101st should return 429
```

**Test login rate limit (5/5 minutes)**:
```bash
# Send 6 login attempts
for i in {1..6}; do
  curl -X POST http://localhost:5000/api/v1/auth/login \
    -H "Content-Type: application/json" \
    -d '{"username": "test", "password": "test"}' \
    -w "\nStatus: %{http_code}\n"
  sleep 1
done
# First 5 should process, 6th should return 429
```

### Automated Testing

Create integration tests to verify rate limiting:

```csharp
[Fact]
public async Task RateLimit_ExceedsLimit_Returns429()
{
    // Arrange
    var client = _factory.CreateClient();

    // Act: Make 101 requests (limit is 100/minute)
    var tasks = Enumerable.Range(0, 101)
        .Select(_ => client.GetAsync("/api/v1/health/ready"));
    var responses = await Task.WhenAll(tasks);

    // Assert: At least one should be rate limited
    var rateLimitedResponse = responses.FirstOrDefault(r => r.StatusCode == (HttpStatusCode)429);
    Assert.NotNull(rateLimitedResponse);
    Assert.True(rateLimitedResponse.Headers.Contains("Retry-After"));
}
```

## Best Practices

### For API Consumers

1. **Respect rate limit headers**: Check `X-Rate-Limit-Remaining` before making requests
2. **Implement exponential backoff**: When receiving 429, wait longer with each retry
3. **Cache responses**: Reduce redundant API calls
4. **Batch operations**: Use batch endpoints when available instead of multiple single requests

### For API Developers

1. **Monitor violation logs**: Regularly review rate limit violation patterns
2. **Adjust limits based on usage**: Use real-world data to tune rate limits
3. **Whitelist legitimate high-volume clients**: Add trusted systems to whitelist
4. **Document rate limits**: Ensure API documentation includes rate limit policies

## Security Considerations

### DDoS Protection
Rate limiting provides basic DDoS protection but should be complemented with:
- WAF (Web Application Firewall) at infrastructure level
- CDN with DDoS protection (Cloudflare, AWS Shield, etc.)
- Network-level rate limiting (nginx, load balancer)

### Distributed Rate Limiting
Current implementation uses in-memory storage (`MemoryCacheRateLimitCounterStore`).

**For production with multiple API instances**, consider Redis-based storage:

```csharp
// Install: AspNetCoreRateLimit.Redis
builder.Services.AddDistributedRateLimiting<RedisRateLimitCounterStore>();
```

### IP Spoofing
Protect against IP spoofing attacks:
- Use `X-Real-IP` or `X-Forwarded-For` headers when behind reverse proxy
- Configure trusted proxies in ASP.NET Core
- Validate proxy headers to prevent header injection

## Troubleshooting

### Issue: Rate limit not working
**Check**:
1. Middleware order in Program.cs (must be before controllers)
2. Configuration loaded correctly (`IpRateLimiting` section exists)
3. Memory cache service registered (`builder.Services.AddMemoryCache()`)

### Issue: All requests return 429
**Check**:
1. Rate limits too restrictive for your use case
2. IP detection working correctly (check logs for client IP)
3. Whitelist configured if needed

### Issue: Rate limit headers not showing
**Check**:
1. `RateLimitHeadersMiddleware` added after `UseIpRateLimiting()`
2. Middleware extension registered: `app.UseRateLimitHeaders()`

## References

- [AspNetCoreRateLimit Documentation](https://github.com/stefanprodan/AspNetCoreRateLimit)
- [OWASP API Security Top 10](https://owasp.org/www-project-api-security/)
- [RFC 6585 - Additional HTTP Status Codes (429)](https://tools.ietf.org/html/rfc6585#section-4)

---

**Last Updated**: October 23, 2025
**Task**: T230 - User Story 6 (Security & Quality Improvements)
