# Operations Manual - Caixa Seguradora Premium Reporting System

**Version**: 1.0
**Last Updated**: October 23, 2025
**System**: COBOL RG1866B to .NET Migration - SUSEP Circular 360 Reporting

---

## Table of Contents

1. [System Overview](#system-overview)
2. [Architecture](#architecture)
3. [Deployment](#deployment)
4. [Monitoring & Health Checks](#monitoring--health-checks)
5. [Batch Job Operations](#batch-job-operations)
6. [Database Management](#database-management)
7. [Backup & Recovery](#backup--recovery)
8. [Troubleshooting](#troubleshooting)
9. [Performance Tuning](#performance-tuning)
10. [Security Operations](#security-operations)
11. [Incident Response](#incident-response)
12. [Maintenance Procedures](#maintenance-procedures)

---

## System Overview

### Purpose
The Caixa Seguradora Premium Reporting System generates regulatory reports for SUSEP (Superintendência de Seguros Privados) Circular 360 compliance. It replaces the legacy COBOL RG1866B batch program with a modern .NET 9 web application and React frontend.

### Key Components

- **Backend API**: ASP.NET Core 9.0 REST API (Port 5001 HTTPS / 5000 HTTP)
- **Frontend**: React 18+ SPA (Port 5173 in development)
- **Database**: SQLite (Development) / PostgreSQL or SQL Server (Production)
- **Report Outputs**: PREMIT.TXT and PREMCED.TXT fixed-width files

### Critical Requirements

- **Regulatory Compliance**: All financial calculations must match COBOL output byte-for-byte
- **Data Precision**: Use `decimal` type for all financial values (NO `float`/`double`)
- **Performance**: Process 10,000+ records in <5 minutes
- **Availability**: 99.5% uptime during business hours (8am-6pm BRT)

---

## Architecture

### Three-Layer Clean Architecture

```
┌─────────────────────────────────────┐
│  Frontend (React SPA)               │
│  - User interface                   │
│  - Data visualization               │
└───────────────┬─────────────────────┘
                │ HTTPS/REST
┌───────────────▼─────────────────────┐
│  API Layer (ASP.NET Core)           │
│  - Controllers                      │
│  - Authentication/Authorization     │
│  - Rate limiting                    │
└───────────────┬─────────────────────┘
                │
┌───────────────▼─────────────────────┐
│  Core Layer (Business Logic)        │
│  - Premium calculations             │
│  - Cossurance logic                 │
│  - Validation rules                 │
└───────────────┬─────────────────────┘
                │
┌───────────────▼─────────────────────┐
│  Infrastructure Layer                │
│  - Database access (EF Core)        │
│  - File generation (PREMIT/PREMCED) │
│  - External service integrations    │
└─────────────────────────────────────┘
```

### Data Flow

1. User initiates report generation via frontend
2. API validates authentication and authorization
3. API applies rate limiting (100 requests/minute)
4. Core layer validates business rules
5. Infrastructure reads database using cursor-based streaming
6. Core layer performs calculations
7. Infrastructure generates fixed-width output files
8. API returns download link to frontend

---

## Deployment

### Prerequisites

- **.NET 9.0 SDK** (production runtime only on servers)
- **Node.js 20+** (for frontend build)
- **Database**: PostgreSQL 15+ or SQL Server 2022+ (Production)
- **HTTPS Certificate**: Required for production
- **Reverse Proxy**: Nginx or IIS recommended

### Environment Variables

#### Backend (API)

```bash
# Database
ConnectionStrings__DefaultConnection="Server=prod-db;Database=CaixaSeguradora;..."

# Authentication
JwtSettings__SecretKey="<generate-secure-key>"
JwtSettings__Issuer="CaixaSeguradora"
JwtSettings__Audience="CaixaSeguradora.API"
JwtSettings__ExpirationMinutes="60"

# CORS
AllowedOrigins="https://caixaseguradora.com.br"

# Logging
Serilog__MinimumLevel__Default="Information"
Serilog__MinimumLevel__Override__Microsoft="Warning"

# File Output
ReportOutputPath="/var/caixa-reports"
```

#### Frontend

```bash
VITE_API_BASE_URL=https://api.caixaseguradora.com.br
VITE_ENVIRONMENT=production
```

### Deployment Steps

#### 1. Backend Deployment

```bash
# Navigate to API project
cd backend/src/CaixaSeguradora.Api

# Publish for Linux x64
dotnet publish -c Release -r linux-x64 --self-contained false -o /var/www/caixa-api

# Set file permissions
sudo chown -R www-data:www-data /var/www/caixa-api
sudo chmod -R 755 /var/www/caixa-api

# Create systemd service
sudo nano /etc/systemd/system/caixa-api.service
```

**Service File** (`/etc/systemd/system/caixa-api.service`):

```ini
[Unit]
Description=Caixa Seguradora Premium Reporting API
After=network.target

[Service]
Type=notify
WorkingDirectory=/var/www/caixa-api
ExecStart=/usr/bin/dotnet /var/www/caixa-api/CaixaSeguradora.Api.dll
Restart=always
RestartSec=10
KillSignal=SIGINT
SyslogIdentifier=caixa-api
User=www-data
Environment=ASPNETCORE_ENVIRONMENT=Production
Environment=DOTNET_PRINT_TELEMETRY_MESSAGE=false

[Install]
WantedBy=multi-user.target
```

```bash
# Enable and start service
sudo systemctl enable caixa-api
sudo systemctl start caixa-api
sudo systemctl status caixa-api
```

#### 2. Frontend Deployment

```bash
# Build production bundle
cd frontend
npm ci --production=false
npm run build

# Copy to web server
sudo cp -r dist/* /var/www/caixa-frontend/

# Set permissions
sudo chown -R www-data:www-data /var/www/caixa-frontend
sudo chmod -R 755 /var/www/caixa-frontend
```

#### 3. Nginx Configuration

```nginx
# /etc/nginx/sites-available/caixa-seguradora

# Frontend
server {
    listen 443 ssl http2;
    server_name caixaseguradora.com.br;

    ssl_certificate /etc/letsencrypt/live/caixaseguradora.com.br/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/caixaseguradora.com.br/privkey.pem;

    root /var/www/caixa-frontend;
    index index.html;

    location / {
        try_files $uri $uri/ /index.html;
    }

    location /assets {
        expires 1y;
        add_header Cache-Control "public, immutable";
    }
}

# API
server {
    listen 443 ssl http2;
    server_name api.caixaseguradora.com.br;

    ssl_certificate /etc/letsencrypt/live/api.caixaseguradora.com.br/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/api.caixaseguradora.com.br/privkey.pem;

    location / {
        proxy_pass http://localhost:5000;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection keep-alive;
        proxy_set_header Host $host;
        proxy_cache_bypass $http_upgrade;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;

        # Timeouts for long-running report generation
        proxy_read_timeout 600s;
        proxy_connect_timeout 60s;
        proxy_send_timeout 600s;
    }
}
```

```bash
# Enable site and reload nginx
sudo ln -s /etc/nginx/sites-available/caixa-seguradora /etc/nginx/sites-enabled/
sudo nginx -t
sudo systemctl reload nginx
```

#### 4. Database Migration

```bash
cd backend/src/CaixaSeguradora.Api

# Generate SQL migration script
dotnet ef migrations script -o /tmp/migration.sql

# Review script
less /tmp/migration.sql

# Apply to production database
psql -h prod-db -U caixa_admin -d CaixaSeguradora -f /tmp/migration.sql
```

---

## Monitoring & Health Checks

### Health Check Endpoint

**URL**: `https://api.caixaseguradora.com.br/health`

**Response**:
```json
{
  "status": "Healthy",
  "totalDuration": "00:00:00.1234567",
  "entries": {
    "database": {
      "status": "Healthy",
      "description": "Database connection OK"
    },
    "disk_space": {
      "status": "Healthy",
      "description": "500 GB available"
    }
  }
}
```

**Monitoring Script**:

```bash
#!/bin/bash
# /usr/local/bin/check-caixa-health.sh

HEALTH_URL="https://api.caixaseguradora.com.br/health"
ALERT_EMAIL="ops@caixaseguradora.com.br"

RESPONSE=$(curl -s -o /dev/null -w "%{http_code}" $HEALTH_URL)

if [ "$RESPONSE" != "200" ]; then
    echo "ALERT: Health check failed with status $RESPONSE" | \
        mail -s "Caixa Seguradora API Down" $ALERT_EMAIL
    exit 1
fi

exit 0
```

Add to cron: `*/5 * * * * /usr/local/bin/check-caixa-health.sh`

### Log Monitoring

**Locations**:
- **Application Logs**: `/var/log/caixa-api/application.log` (Serilog output)
- **System Logs**: `journalctl -u caixa-api -f`
- **Nginx Access**: `/var/log/nginx/access.log`
- **Nginx Errors**: `/var/log/nginx/error.log`

**Critical Error Patterns**:

```bash
# Monitor for errors
tail -f /var/log/caixa-api/application.log | grep -E "ERROR|FATAL|Exception"

# Monitor for slow queries (>1 second)
tail -f /var/log/caixa-api/application.log | grep "ExecutionTime.*[0-9]{4,}ms"

# Monitor for failed calculations (regulatory risk!)
tail -f /var/log/caixa-api/application.log | grep "Calculation.*failed"
```

### Performance Metrics

Monitor these KPIs:

- **API Response Time**: P95 < 500ms, P99 < 1000ms
- **Report Generation Time**: <5 minutes for 10,000 records
- **Database Query Time**: P95 < 100ms
- **Memory Usage**: <2 GB per API instance
- **CPU Usage**: <70% average
- **Disk I/O**: <80% utilization

**Prometheus Metrics** (if integrated):
- `http_request_duration_seconds`
- `report_generation_duration_seconds`
- `database_query_duration_seconds`
- `dotnet_gc_collection_count`

---

## Batch Job Operations

### Scheduling Report Generation

#### Via API (Programmatic)

```bash
# Create scheduled job
curl -X POST https://api.caixaseguradora.com.br/api/v1/batch/jobs \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $TOKEN" \
  -d '{
    "jobType": "PREMIT_GENERATION",
    "scheduledTime": "2025-10-24T02:00:00Z",
    "parameters": {
      "startDate": "2025-10-01",
      "endDate": "2025-10-31"
    }
  }'
```

#### Via Cron (Linux)

```bash
# /etc/cron.d/caixa-reports

# Generate monthly PREMIT report on 1st day of month at 2am
0 2 1 * * www-data /usr/local/bin/generate-premit-report.sh

# Generate monthly PREMCED report on 1st day of month at 3am
0 3 1 * * www-data /usr/local/bin/generate-premced-report.sh
```

**Script** (`/usr/local/bin/generate-premit-report.sh`):

```bash
#!/bin/bash
set -e

# Calculate last month's date range
LAST_MONTH_START=$(date -d "last month" +%Y-%m-01)
LAST_MONTH_END=$(date -d "$(date +%Y-%m-01) - 1 day" +%Y-%m-%d)

# Get auth token (use service account)
TOKEN=$(curl -s -X POST https://api.caixaseguradora.com.br/api/v1/auth/login \
  -H "Content-Type: application/json" \
  -d '{"username":"batch_service","password":"'$BATCH_SERVICE_PASSWORD'"}' | \
  jq -r '.token')

# Create job
RESPONSE=$(curl -s -X POST https://api.caixaseguradora.com.br/api/v1/batch/jobs \
  -H "Content-Type: application/json" \
  -H "Authorization: Bearer $TOKEN" \
  -d '{
    "jobType": "PREMIT_GENERATION",
    "scheduledTime": "'$(date -u +%Y-%m-%dT%H:%M:%SZ)'",
    "parameters": {
      "startDate": "'$LAST_MONTH_START'",
      "endDate": "'$LAST_MONTH_END'"
    }
  }')

JOB_ID=$(echo $RESPONSE | jq -r '.jobId')

echo "Job created: $JOB_ID"

# Monitor job until completion
while true; do
    STATUS=$(curl -s -H "Authorization: Bearer $TOKEN" \
      https://api.caixaseguradora.com.br/api/v1/batch/jobs/$JOB_ID | \
      jq -r '.status')

    if [ "$STATUS" = "completed" ]; then
        echo "Job completed successfully"
        exit 0
    elif [ "$STATUS" = "failed" ]; then
        echo "Job failed"
        exit 1
    fi

    sleep 30
done
```

### Monitoring Jobs

```bash
# List all jobs
curl -H "Authorization: Bearer $TOKEN" \
  https://api.caixaseguradora.com.br/api/v1/batch/jobs

# Get job details
curl -H "Authorization: Bearer $TOKEN" \
  https://api.caixaseguradora.com.br/api/v1/batch/jobs/{jobId}

# Cancel running job
curl -X POST -H "Authorization: Bearer $TOKEN" \
  https://api.caixaseguradora.com.br/api/v1/batch/jobs/{jobId}/cancel
```

---

## Database Management

### Backup Procedures

#### Daily Automated Backup

```bash
#!/bin/bash
# /usr/local/bin/backup-caixa-db.sh

BACKUP_DIR="/var/backups/caixa-db"
DATE=$(date +%Y%m%d_%H%M%S)
DB_NAME="CaixaSeguradora"

# Create backup
pg_dump -h prod-db -U caixa_admin -F c -b -v -f \
  "$BACKUP_DIR/${DB_NAME}_${DATE}.backup" $DB_NAME

# Compress
gzip "$BACKUP_DIR/${DB_NAME}_${DATE}.backup"

# Delete backups older than 30 days
find $BACKUP_DIR -name "*.backup.gz" -mtime +30 -delete

# Upload to S3 (optional)
aws s3 cp "$BACKUP_DIR/${DB_NAME}_${DATE}.backup.gz" \
  s3://caixa-backups/database/
```

**Cron**: `0 1 * * * /usr/local/bin/backup-caixa-db.sh`

#### Restore from Backup

```bash
# Stop API to prevent writes
sudo systemctl stop caixa-api

# Restore database
gunzip < /var/backups/caixa-db/CaixaSeguradora_20251023_010000.backup.gz | \
  pg_restore -h prod-db -U caixa_admin -d CaixaSeguradora -c -v

# Restart API
sudo systemctl start caixa-api
```

### Database Maintenance

#### Vacuum and Analyze (PostgreSQL)

```bash
# Weekly maintenance
psql -h prod-db -U caixa_admin -d CaixaSeguradora -c "VACUUM ANALYZE;"

# Reindex (monthly)
psql -h prod-db -U caixa_admin -d CaixaSeguradora -c "REINDEX DATABASE CaixaSeguradora;"
```

#### Check Index Usage

```sql
-- Find unused indexes
SELECT schemaname, tablename, indexname
FROM pg_stat_user_indexes
WHERE idx_scan = 0
ORDER BY tablename, indexname;

-- Find missing indexes (slow queries)
SELECT schemaname, tablename, seq_scan, seq_tup_read, idx_scan
FROM pg_stat_user_tables
WHERE seq_scan > 1000 AND idx_scan < 100
ORDER BY seq_tup_read DESC;
```

---

## Backup & Recovery

### Recovery Time Objective (RTO)

**Target**: System restored within 4 hours of failure

### Recovery Point Objective (RPO)

**Target**: Maximum 24 hours of data loss (daily backups)

### Disaster Recovery Steps

1. **Assess Damage**
   - Identify failed components
   - Check backup integrity

2. **Restore Database**
   ```bash
   pg_restore -h new-db -U caixa_admin -d CaixaSeguradora \
     /var/backups/caixa-db/latest.backup
   ```

3. **Redeploy Application**
   ```bash
   cd /var/www/caixa-api
   sudo systemctl start caixa-api
   ```

4. **Verify System Health**
   ```bash
   curl https://api.caixaseguradora.com.br/health
   ```

5. **Run Smoke Tests**
   - Generate test report
   - Verify database queries
   - Check file output format

6. **Notify Stakeholders**
   - Send status update email
   - Update status page

---

## Troubleshooting

### Common Issues

#### 1. API Returns 500 Internal Server Error

**Symptoms**: All API requests fail with 500 status

**Diagnosis**:
```bash
# Check API logs
journalctl -u caixa-api -n 100

# Check database connection
psql -h prod-db -U caixa_admin -d CaixaSeguradora -c "SELECT 1;"
```

**Solutions**:
- Database connection lost: Restart database or update connection string
- Out of memory: Restart API service, increase memory limits
- Unhandled exception: Check logs for stack trace, deploy hotfix

#### 2. Report Generation Hangs

**Symptoms**: Report job stuck in "running" status for >30 minutes

**Diagnosis**:
```bash
# Check for long-running queries
psql -h prod-db -U caixa_admin -d CaixaSeguradora -c "
  SELECT pid, now() - pg_stat_activity.query_start AS duration, query
  FROM pg_stat_activity
  WHERE state = 'active' AND query NOT LIKE '%pg_stat_activity%'
  ORDER BY duration DESC;
"
```

**Solutions**:
- Kill hung query: `SELECT pg_terminate_backend(pid);`
- Cancel job via API: `POST /api/v1/batch/jobs/{jobId}/cancel`
- Check for missing indexes on date columns

#### 3. Byte-for-Byte Validation Fails

**Symptoms**: Generated PREMIT/PREMCED file doesn't match COBOL output

**Diagnosis**:
```bash
# Run comparison test
cd backend/tests/CaixaSeguradora.ComparisonTests
dotnet test --filter "Category=Comparison"
```

**Solutions**:
- Check for incorrect decimal precision (must use `decimal`, not `float`)
- Verify fixed-width formatting (spaces vs zeros padding)
- Compare byte-by-byte using hex editor: `xxd file1.txt > file1.hex`

#### 4. High CPU Usage

**Symptoms**: API server CPU consistently >90%

**Diagnosis**:
```bash
# Find CPU-intensive process
top -u www-data

# Profile .NET application
dotnet-trace collect --process-id $(pgrep -f CaixaSeguradora.Api)
```

**Solutions**:
- Scale horizontally: Add more API instances behind load balancer
- Optimize database queries: Add indexes, use `AsNoTracking()`
- Reduce calculation complexity: Profile with BenchmarkDotNet

---

## Performance Tuning

### Database Optimization

#### Connection Pooling

**appsettings.json**:
```json
{
  "ConnectionStrings": {
    "DefaultConnection": "Server=prod-db;Database=CaixaSeguradora;Max Pool Size=100;Min Pool Size=10;Connection Lifetime=300"
  }
}
```

#### Query Optimization

```csharp
// ❌ Avoid: Loads all records into memory
var premiums = await _context.Premiums.ToListAsync();

// ✅ Correct: Streaming with IAsyncEnumerable
await foreach (var premium in _repository.GetPremiumsAsync(startDate, endDate))
{
    // Process one at a time
}
```

### API Caching

Enable response caching for frequently accessed data:

```csharp
// Program.cs
builder.Services.AddResponseCaching();
app.UseResponseCaching();

// Controller
[HttpGet("products")]
[ResponseCache(Duration = 3600)] // Cache for 1 hour
public async Task<IActionResult> GetProducts()
{
    return Ok(await _productService.GetAllAsync());
}
```

### Frontend Performance

- **Code Splitting**: Lazy load routes
- **Image Optimization**: Use WebP format, compress images
- **Bundle Size**: Target <500 KB initial bundle
- **CDN**: Serve static assets from CDN

---

## Security Operations

### Authentication & Authorization

- **JWT Tokens**: 60-minute expiration
- **Role-Based Access**: Admin, Operator, Viewer roles
- **API Keys**: For batch service accounts

### Rate Limiting

Current limits:
- **Authenticated Users**: 100 requests/minute
- **Anonymous**: 10 requests/minute
- **Batch Jobs**: 5 concurrent jobs

### SSL/TLS Configuration

**Nginx** minimum TLS 1.2:

```nginx
ssl_protocols TLSv1.2 TLSv1.3;
ssl_ciphers 'ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256';
ssl_prefer_server_ciphers on;
```

### Audit Logging

All sensitive operations are logged:
- User authentication attempts
- Report generation requests
- Database modifications
- Configuration changes

**Query Audit Log**:
```bash
grep "AuditLog" /var/log/caixa-api/application.log | jq .
```

---

## Incident Response

### Severity Levels

- **P0 (Critical)**: System down, no workaround. Response: 15 minutes
- **P1 (High)**: Major functionality broken. Response: 1 hour
- **P2 (Medium)**: Minor functionality broken, workaround exists. Response: 4 hours
- **P3 (Low)**: Cosmetic issues. Response: Next business day

### Escalation Path

1. **L1 Support** (24/7 on-call): Initial triage
2. **L2 Engineering** (Business hours): Code-level debugging
3. **L3 Architect** (On-call): System design issues

### Communication Template

**Subject**: [P0/P1/P2/P3] Caixa Seguradora Incident - [Brief Description]

**Body**:
```
INCIDENT SUMMARY
----------------
Severity: P1
Start Time: 2025-10-23 14:30 BRT
Affected Service: Report Generation API
Impact: Users unable to generate PREMIT reports

CURRENT STATUS
--------------
Investigating database connection timeouts. No data loss detected.

NEXT STEPS
----------
1. Restart database connection pool (ETA: 5 minutes)
2. Monitor for recovery
3. Root cause analysis

UPDATES
-------
Will update every 15 minutes until resolved.
```

---

## Maintenance Procedures

### Monthly Maintenance Window

**Schedule**: First Sunday of month, 2am-6am BRT

**Checklist**:

- [ ] Apply security patches to OS
- [ ] Update .NET runtime (if needed)
- [ ] Update npm packages (frontend)
- [ ] Run database maintenance (VACUUM, REINDEX)
- [ ] Review and archive old logs
- [ ] Test backup restoration
- [ ] Review performance metrics
- [ ] Update SSL certificates (if expiring soon)

### Quarterly Reviews

- Review and update this operations manual
- Conduct disaster recovery drill
- Review and optimize slow queries
- Audit user access and permissions
- Review and update monitoring alerts

---

## Contacts

### Support Team

- **Operations**: ops@caixaseguradora.com.br
- **Engineering**: dev@caixaseguradora.com.br
- **Security**: security@caixaseguradora.com.br

### Vendor Support

- **.NET Support**: Microsoft Premier Support
- **Database**: PostgreSQL Community / AWS RDS Support
- **Cloud Provider**: AWS Support (Business tier)

---

## Appendix

### Useful Commands Cheat Sheet

```bash
# Check API status
sudo systemctl status caixa-api

# Restart API
sudo systemctl restart caixa-api

# View real-time logs
journalctl -u caixa-api -f

# Check database connection
psql -h prod-db -U caixa_admin -d CaixaSeguradora -c "SELECT version();"

# Generate on-demand report
curl -X POST https://api.caixaseguradora.com.br/api/v1/reports/generate \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{"reportType":"PREMIT","startDate":"2025-10-01","endDate":"2025-10-31"}'

# Check disk space
df -h /var

# Check memory usage
free -h

# Check open file descriptors
lsof -u www-data | wc -l
```

### References

- System Architecture: `/docs/architecture.md`
- API Documentation: `/docs/api/index.html`
- Deployment Guide: `/docs/deployment.md`
- COBOL Analysis: `/docs/parser/FINAL-ANALYSIS-REPORT.md`
- Feature Specification: `/specs/001-vamos-migrar-sistema/spec.md`

---

**Document Version**: 1.0
**Maintained By**: DevOps Team
**Review Frequency**: Quarterly
