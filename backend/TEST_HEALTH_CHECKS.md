# Health Checks Testing Guide

## Quick Start

1. **Start the API**:
   ```bash
   cd backend/src/CaixaSeguradora.Api
   dotnet run
   ```

2. **Test the health endpoint**:
   ```bash
   # Basic health check
   curl -i http://localhost:5555/health

   # With JSON formatting (if jq is installed)
   curl -s http://localhost:5555/health | jq .
   ```

## Expected Responses

### Healthy Response (HTTP 200)
```json
{
  "status": "Healthy",
  "results": {
    "database": {
      "status": "Healthy",
      "description": "Database respondeu em 45ms",
      "data": {
        "responseTime": "45ms",
        "threshold_healthy": "100ms",
        "threshold_degraded": "500ms"
      }
    },
    "fileSystem": {
      "status": "Healthy",
      "description": "Sistema de arquivos OK - 256.5 GB disponível",
      "data": {
        "outputDirectory": "/path/to/output",
        "minimumDiskSpace": "1 GB",
        "availableDiskSpace": "256.5 GB",
        "driveFormat": "APFS",
        "driveName": "/"
      }
    }
  }
}
```

### Degraded Response (HTTP 200)
Occurs when:
- Database response time is 100-500ms
- Available disk space < 1 GB

```json
{
  "status": "Degraded",
  "results": {
    "database": {
      "status": "Degraded",
      "description": "Database com performance degradada (250ms)",
      "data": { ... }
    },
    "fileSystem": {
      "status": "Degraded",
      "description": "Espaço em disco baixo: 512 MB disponível (mínimo requerido: 1 GB)",
      "data": { ... }
    }
  }
}
```

### Unhealthy Response (HTTP 503)
Occurs when:
- Database connection fails
- Database response time > 500ms
- Output directory not writable
- File system error

```json
{
  "status": "Unhealthy",
  "results": {
    "database": {
      "status": "Unhealthy",
      "description": "Falha ao conectar ao banco de dados: unable to open database file",
      "data": {
        "error": "unable to open database file",
        "errorType": "SqliteException",
        "elapsedTime": "15ms"
      }
    },
    "fileSystem": {
      "status": "Unhealthy",
      "description": "Diretório de output não possui permissão de escrita: /readonly/path",
      "data": { ... }
    }
  }
}
```

## Testing Scenarios

### 1. Test Healthy State (Normal Operation)
```bash
# Ensure database file exists and is accessible
ls -lh backend/src/CaixaSeguradora.Api/premium_reporting.db

# Ensure output directory is writable
mkdir -p backend/src/CaixaSeguradora.Api/output
chmod 755 backend/src/CaixaSeguradora.Api/output

# Test health endpoint
curl http://localhost:5555/health
```

### 2. Test Database Degraded (Simulated Slow Response)
This would require modifying the database or adding intentional delays. In production:
- Monitor response times during high load
- Check for slow queries or connection pool exhaustion

### 3. Test File System Degraded (Low Disk Space)
```bash
# Check current disk space
df -h /

# If testing on a separate partition with low space:
# The health check will automatically report Degraded status
```

### 4. Test Database Unhealthy (No Database)
```bash
# Rename database file to simulate missing database
mv backend/src/CaixaSeguradora.Api/premium_reporting.db \
   backend/src/CaixaSeguradora.Api/premium_reporting.db.bak

# Test health endpoint (should return HTTP 503)
curl -i http://localhost:5555/health

# Restore database
mv backend/src/CaixaSeguradora.Api/premium_reporting.db.bak \
   backend/src/CaixaSeguradora.Api/premium_reporting.db
```

### 5. Test File System Unhealthy (No Write Permission)
```bash
# Remove write permission from output directory
chmod 555 backend/src/CaixaSeguradora.Api/output

# Test health endpoint (should return HTTP 503)
curl -i http://localhost:5555/health

# Restore permissions
chmod 755 backend/src/CaixaSeguradora.Api/output
```

## Integration with Monitoring Tools

### Prometheus
Add health check metrics endpoint (Task T195):
```bash
curl http://localhost:5555/metrics
```

### Kubernetes Liveness Probe
```yaml
livenessProbe:
  httpGet:
    path: /health
    port: 5555
  initialDelaySeconds: 30
  periodSeconds: 10
  timeoutSeconds: 5
  failureThreshold: 3
```

### Kubernetes Readiness Probe
```yaml
readinessProbe:
  httpGet:
    path: /health
    port: 5555
  initialDelaySeconds: 10
  periodSeconds: 5
  timeoutSeconds: 3
  failureThreshold: 3
```

### Docker Healthcheck
```dockerfile
HEALTHCHECK --interval=30s --timeout=3s --start-period=40s --retries=3 \
  CMD curl -f http://localhost:5555/health || exit 1
```

## Automated Testing

### Using curl in scripts
```bash
#!/bin/bash
response=$(curl -s -o /dev/null -w "%{http_code}" http://localhost:5555/health)

if [ "$response" = "200" ]; then
  echo "✅ Health check passed"
  exit 0
elif [ "$response" = "503" ]; then
  echo "❌ Health check failed - service unhealthy"
  exit 1
else
  echo "⚠️  Health check returned unexpected status: $response"
  exit 2
fi
```

### Using PowerShell
```powershell
$response = Invoke-RestMethod -Uri "http://localhost:5555/health"
if ($response.status -eq "Healthy") {
    Write-Host "✅ Health check passed" -ForegroundColor Green
} else {
    Write-Host "❌ Health check failed: $($response.status)" -ForegroundColor Red
    exit 1
}
```

## Troubleshooting

### Health endpoint returns 404
- Verify `app.MapHealthChecks("/health")` is in Program.cs
- Check that the application started successfully
- Confirm the correct port (5555 by default)

### Database health always Unhealthy
- Check database file exists: `ls -lh *.db`
- Verify database permissions
- Check connection string in appsettings.json
- Review logs for detailed error messages

### File system health always Degraded
- Check available disk space: `df -h`
- Verify output directory configuration
- Ensure sufficient space (> 1 GB free)

### High database response times
- Check for database locks
- Review concurrent connections
- Analyze slow queries
- Consider indexing optimization

## Logs to Monitor

When running the application, watch for these log messages:

```
[22:48:15 INF] Database health check: Healthy (response time: 45ms)
[22:48:15 DBG] File system health check: Healthy (available space: 256.5 GB on /)
```

Or for issues:
```
[22:48:15 WRN] Database health check: Degraded (response time: 250ms)
[22:48:15 ERR] Database health check failed after 15ms: unable to open database file
[22:48:15 WRN] Low disk space on /: 512 MB available (minimum required: 1 GB)
```

## Next Steps

1. Test the basic health endpoint
2. Implement T192: HealthController for detailed diagnostics
3. Implement T195: Add Prometheus metrics
4. Configure monitoring/alerting based on health status
5. Add health checks to deployment pipeline (CI/CD)

---

**Last Updated**: October 27, 2025
