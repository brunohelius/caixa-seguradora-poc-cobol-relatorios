# Deployment Guide - Caixa Seguradora Premium Reporting System

**Version**: 1.0
**Last Updated**: October 27, 2025
**Phase 10 Completion**: T187-T204 (Production Readiness)

## Overview

This guide covers deploying the complete COBOL RG1866B migration system (backend + frontend) using Docker containers. The system is production-ready with comprehensive error handling, health checks, structured logging, and correlation ID tracking.

## Architecture

```
┌─────────────────────────────────────────────────┐
│  Frontend (React + nginx)                      │
│  - Port: 5173 (host) → 80 (container)         │
│  - Health: /health                             │
└─────────────────┬───────────────────────────────┘
                  │ HTTP API calls
┌─────────────────▼───────────────────────────────┐
│  Backend (.NET 9 + SQLite)                     │
│  - Port: 5555 (HTTP)                           │
│  - Health: /api/v1/health                      │
│  - Swagger: /swagger                           │
└─────────────────────────────────────────────────┘
```

## Prerequisites

- **Docker 24.0+** - Container runtime
- **Docker Compose 2.0+** - Multi-container orchestration
- **Minimum 2GB RAM** - For backend processing
- **Minimum 10GB Disk** - For database and logs

## Quick Start (Development)

### 1. Clone and navigate to project

```bash
cd "/Users/brunosouza/Development/Caixa Seguradora/POC Cobol"
```

### 2. Set environment variables (optional)

```bash
# Create .env file in project root
cat > .env << EOF
JWT_SECRET_KEY=your-super-secret-key-minimum-32-characters-long
ASPNETCORE_ENVIRONMENT=Development
VITE_API_BASE_URL=http://localhost:5555
EOF
```

### 3. Build and start services

```bash
docker-compose up --build
```

This will:
- Build backend Docker image from `backend/Dockerfile`
- Build frontend Docker image from `frontend/Dockerfile`
- Start both services with health checks
- Create persistent volumes for database and logs

### 4. Verify deployment

Open browser:
- **Frontend**: http://localhost:5173
- **Backend API**: http://localhost:5555
- **Swagger Docs**: http://localhost:5555/swagger
- **Backend Health**: http://localhost:5555/api/v1/health
- **Frontend Health**: http://localhost:5173/health

### 5. View logs

```bash
# All services
docker-compose logs -f

# Backend only
docker-compose logs -f backend

# Frontend only
docker-compose logs -f frontend

# Last 100 lines
docker-compose logs --tail=100
```

### 6. Stop services

```bash
# Stop containers (preserves volumes)
docker-compose stop

# Stop and remove containers
docker-compose down

# Remove containers and volumes
docker-compose down -v
```

## Production Deployment

### 1. Build production images

```bash
# Backend
cd backend
docker build -t caixa-seguradora-backend:1.0 .
docker tag caixa-seguradora-backend:1.0 caixa-seguradora-backend:latest

# Frontend
cd ../frontend
docker build -t caixa-seguradora-frontend:1.0 .
docker tag caixa-seguradora-frontend:1.0 caixa-seguradora-frontend:latest
```

### 2. Push to container registry (optional)

```bash
# Login to registry
docker login your-registry.com

# Tag for registry
docker tag caixa-seguradora-backend:1.0 your-registry.com/caixa-seguradora-backend:1.0
docker tag caixa-seguradora-frontend:1.0 your-registry.com/caixa-seguradora-frontend:1.0

# Push images
docker push your-registry.com/caixa-seguradora-backend:1.0
docker push your-registry.com/caixa-seguradora-frontend:1.0
```

### 3. Create production docker-compose override

Create `docker-compose.prod.yml`:

```yaml
version: '3.8'

services:
  backend:
    environment:
      - ASPNETCORE_ENVIRONMENT=Production
      - Jwt__SecretKey=${JWT_SECRET_KEY}  # From environment
      - ConnectionStrings__DefaultConnection=Data Source=/app/data/premium_reporting.db
      - Cors__AllowedOrigins__0=https://yourdomain.com
    restart: always

  frontend:
    environment:
      - VITE_API_BASE_URL=https://api.yourdomain.com
    restart: always
```

### 4. Deploy to production

```bash
# Set production environment variables
export JWT_SECRET_KEY="your-production-secret-key-min-32-chars"

# Start with production overrides
docker-compose -f docker-compose.yml -f docker-compose.prod.yml up -d

# Verify health
curl http://localhost:5555/api/v1/health
curl http://localhost:5173/health
```

## Health Checks

### Backend Health Endpoints

**1. Comprehensive Health Check**

```bash
curl http://localhost:5555/api/v1/health
```

Response:

```json
{
  "status": "Healthy",
  "timestamp": "2025-10-27T12:00:00Z",
  "version": "1.0.0",
  "databaseStatus": "Healthy",
  "databaseResponseTimeMs": 45,
  "checks": {
    "Database": {
      "status": "Healthy",
      "responseTimeMs": 45,
      "message": "Database connection is healthy"
    },
    "FileSystem": {
      "status": "Healthy",
      "responseTimeMs": 2,
      "message": "Output directory writable. Available space: 150.50 GB"
    }
  }
}
```

**2. Liveness Probe** (for Kubernetes)

```bash
curl http://localhost:5555/api/v1/health/live
```

Returns 200 if API is running.

**3. Readiness Probe** (for Kubernetes)

```bash
curl http://localhost:5555/api/v1/health/ready
```

Returns 200 if API can serve traffic (database accessible).

### Frontend Health Endpoint

```bash
curl http://localhost:5173/health
```

Returns 200 with "healthy" text.

## Monitoring

### View Application Logs

```bash
# Backend logs (Serilog structured logging)
tail -f backend/logs/premiumreporting-*.log

# Docker container logs
docker logs -f caixa-seguradora-backend
docker logs -f caixa-seguradora-frontend
```

### Log Format

Backend logs include correlation IDs for request tracking:

```
2025-10-27 14:30:45.123 -03:00 [INF] CorrelationId=abc123def456 Processing premium calculation for policy 1234567890123
```

### Monitoring Metrics

Key metrics to monitor:

- **Health check status** - Should always return 200
- **Database response time** - Should be < 100ms (healthy), < 500ms (degraded)
- **Disk space** - Should be > 1GB free
- **Memory usage** - Backend should stay < 2GB
- **API response times** - P95 should be < 2 seconds

## Database Management

### Backup Database

```bash
# Backup SQLite database from running container
docker exec caixa-seguradora-backend sh -c "cp /app/data/premium_reporting.db /app/output/backup-$(date +%Y%m%d-%H%M%S).db"

# Copy to host
docker cp caixa-seguradora-backend:/app/output/backup-20251027-120000.db ./backups/
```

### Restore Database

```bash
# Copy backup to container
docker cp ./backups/backup-20251027-120000.db caixa-seguradora-backend:/app/data/premium_reporting.db

# Restart backend
docker-compose restart backend
```

### Apply Migrations

```bash
# Run migrations in container
docker exec -it caixa-seguradora-backend dotnet ef database update
```

## Troubleshooting

### Backend Container Won't Start

```bash
# Check logs
docker logs caixa-seguradora-backend

# Common issues:
# 1. Port 5555 already in use
lsof -ti:5555 | xargs kill -9

# 2. Database locked
docker exec caixa-seguradora-backend rm -f /app/data/premium-reporting-dev.db-shm
docker exec caixa-seguradora-backend rm -f /app/data/premium-reporting-dev.db-wal

# 3. Permission issues
docker-compose down -v  # Remove volumes
docker-compose up --build
```

### Frontend Can't Connect to Backend

```bash
# Check network connectivity
docker exec caixa-seguradora-frontend wget -O- http://backend:5555/api/v1/health

# Verify CORS configuration in backend
# Should allow http://localhost:5173
```

### Health Check Failing

```bash
# Check health endpoint manually
docker exec caixa-seguradora-backend curl http://localhost:5555/api/v1/health/live

# If curl not found, install it
docker exec -u root caixa-seguradora-backend apt-get update
docker exec -u root caixa-seguradora-backend apt-get install -y curl
```

### High Memory Usage

```bash
# Check memory usage
docker stats

# Limit container memory
# Add to docker-compose.yml:
services:
  backend:
    deploy:
      resources:
        limits:
          memory: 2G
```

### Disk Space Issues

```bash
# Check disk usage
docker system df

# Clean up unused resources
docker system prune -a

# Check volume usage
docker volume ls
docker volume inspect backend-data
```

## Security Considerations

### 1. Change Default Secrets

**CRITICAL**: Change these before production:

- JWT secret key (minimum 32 characters)
- Database passwords (if using external DB)
- API keys for external services

### 2. Use HTTPS in Production

Configure reverse proxy (nginx/traefik) for TLS:

```nginx
server {
    listen 443 ssl;
    server_name api.yourdomain.com;

    ssl_certificate /path/to/cert.pem;
    ssl_certificate_key /path/to/key.pem;

    location / {
        proxy_pass http://localhost:5555;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
}
```

### 3. Network Isolation

Use Docker networks to isolate services:

```yaml
networks:
  frontend-network:
  backend-network:

services:
  frontend:
    networks:
      - frontend-network
  backend:
    networks:
      - frontend-network
      - backend-network
  database:
    networks:
      - backend-network
```

### 4. Run as Non-Root

Both containers already run as non-root user (appuser, UID 1001).

## Scaling

### Horizontal Scaling

Backend API is stateless and can be scaled horizontally:

```yaml
services:
  backend:
    deploy:
      replicas: 3
    ports:
      - "5555-5557:5555"  # Multiple ports
```

Add load balancer (nginx/traefik) for distribution.

### Vertical Scaling

Increase container resources:

```yaml
services:
  backend:
    deploy:
      resources:
        limits:
          cpus: '2.0'
          memory: 4G
        reservations:
          cpus: '1.0'
          memory: 2G
```

## Performance Tuning

### Backend

- **Connection pooling**: Already configured (T155)
- **Cursor-based processing**: Implemented for large datasets (T145-T158)
- **Query optimization**: No-tracking queries for read operations

### Frontend

- **Static asset caching**: 1 year for JS/CSS/images
- **Gzip compression**: Enabled in nginx
- **Code splitting**: Implemented by Vite

## Compliance and Validation

### COBOL Parity Tests

Run comparison tests to verify byte-for-byte output compatibility:

```bash
# Inside backend container
docker exec -it caixa-seguradora-backend dotnet test --filter Category=Comparison

# Expected: 100% pass rate, zero byte differences
```

### SUSEP Format Validation

Validate generated PREMIT.TXT and PREMCED.TXT files:

```bash
# Check file structure
docker exec caixa-seguradora-backend sh -c "wc -c /app/output/PREMIT_202510.TXT"
# Should show: 765 * number_of_records bytes

docker exec caixa-seguradora-backend sh -c "wc -c /app/output/PREMCED_202510.TXT"
# Should show: 168 * number_of_records bytes
```

## Support and Maintenance

### Regular Maintenance Tasks

**Daily**:
- Check health endpoints
- Monitor disk space
- Review error logs

**Weekly**:
- Backup database
- Review performance metrics
- Clean old log files

**Monthly**:
- Update Docker images
- Review security vulnerabilities
- Archive old reports

### Getting Help

1. **Check logs**: `docker-compose logs -f`
2. **Health status**: `curl http://localhost:5555/api/v1/health`
3. **API documentation**: http://localhost:5555/swagger
4. **Backend README**: `backend/README.md`
5. **Frontend README**: `frontend/README.md`

---

**Phase 10 Implementation**: ✅ Complete
**Tasks T187-T204**: All implemented and tested
**Production Ready**: Yes
**COBOL Parity**: Validated
**Regulatory Compliance**: SUSEP Circular 360 compliant
