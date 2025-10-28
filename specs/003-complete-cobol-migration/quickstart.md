# Developer Quickstart Guide

**COBOL RG1866B to .NET 9 Migration Project**
Get up and running in **under 30 minutes**.

---

## Prerequisites

### Required Software

Install the following tools before starting:

| Tool | Version | Download | Verify Installation |
|------|---------|----------|---------------------|
| **.NET 9 SDK** | 9.0.0+ | [Download](https://dotnet.microsoft.com/download/dotnet/9.0) | `dotnet --version` |
| **Node.js** | 20.0.0+ LTS | [Download](https://nodejs.org/en/download/) | `node --version` |
| **Git** | 2.40.0+ | [Download](https://git-scm.com/downloads) | `git --version` |
| **Docker Desktop** | 4.0.0+ (optional) | [Download](https://www.docker.com/products/docker-desktop/) | `docker --version` |

### System Requirements

- **RAM**: 8GB minimum (16GB recommended)
- **Disk Space**: 5GB free space
- **OS**: macOS, Windows, or Linux

### Recommended IDEs

- **VS Code** with [C# Dev Kit](https://marketplace.visualstudio.com/items?itemName=ms-dotnettools.csdevkit) extension
- **JetBrains Rider** (commercial license required)

---

## Project Structure Overview

```
POC Cobol/
├── backend/              ← .NET 9 Web API (Clean Architecture)
│   ├── src/
│   │   ├── CaixaSeguradora.Api/          # Controllers, Program.cs
│   │   ├── CaixaSeguradora.Core/         # Domain entities, services
│   │   └── CaixaSeguradora.Infrastructure/ # Data access, repositories
│   └── tests/
│       ├── CaixaSeguradora.UnitTests/
│       ├── CaixaSeguradora.IntegrationTests/
│       └── CaixaSeguradora.ComparisonTests/ # COBOL output validation
│
├── frontend/             ← React 19 + Vite + TailwindCSS
│   ├── src/
│   │   ├── components/   # UI components (dashboard, reports, etc.)
│   │   ├── pages/        # Route components
│   │   └── services/     # Axios API clients
│   └── tests/
│       └── e2e/          # Playwright tests
│
├── docs/                 ← Analysis & documentation
│   ├── legacy-system/    # COBOL analysis reports
│   └── parser/           # COBOL parser output
│
├── specs/                ← Feature specifications (DOCUMENTATION ONLY)
│   ├── 001-vamos-migrar-sistema/
│   ├── 002-migration-analysis-pdf/
│   └── 003-complete-cobol-migration/
│
└── RG1866B.cbl          ← Original COBOL source (5,000 lines)
```

**Key Files**:
- **Backend API**: `backend/src/CaixaSeguradora.Api/Program.cs`
- **Frontend Entry**: `frontend/src/main.tsx`
- **COBOL Source**: `RG1866B.cbl` (root directory)
- **Documentation**: `CLAUDE.md`, `README.md`, `specs/*/spec.md`

---

## Initial Setup

### Step 1: Clone and Checkout

```bash
# Clone the repository
git clone https://github.com/your-org/poc-cobol.git
cd "POC Cobol"

# Checkout the feature branch
git checkout 003-complete-cobol-migration

# Verify current branch
git branch --show-current
```

### Step 2: Backend Setup

```bash
# Navigate to backend directory
cd backend

# Restore NuGet packages
dotnet restore

# Build the solution
dotnet build --configuration Debug

# Apply database migrations (creates SQLite database)
dotnet ef database update --project src/CaixaSeguradora.Infrastructure --startup-project src/CaixaSeguradora.Api

# Expected output:
# ✓ Build succeeded
# ✓ Applying migration '20251022_InitialCreate'
# ✓ Done.
```

**Verify Backend Build**:
```bash
# Should show no errors
dotnet build --configuration Debug
```

### Step 3: Load Sample Data (Optional)

```bash
# Navigate to API project
cd src/CaixaSeguradora.Api

# Run the API briefly to create database
dotnet run --urls "http://localhost:5555;https://localhost:5556" &
sleep 5

# Load sample premium records (using mock data endpoint)
curl -X POST https://localhost:5556/api/v1/mock-data/load \
  -H "Content-Type: application/json" \
  -d '{"recordCount": 100}' \
  --insecure

# Expected response:
# {"success": true, "recordsLoaded": 100, "message": "Dados de exemplo carregados com sucesso"}

# Stop the API
pkill -f "dotnet.*CaixaSeguradora.Api"

# Return to project root
cd ../../../
```

### Step 4: Frontend Setup

```bash
# Navigate to frontend directory
cd frontend

# Install npm dependencies (this may take 2-3 minutes)
npm install

# Build production assets
npm run build

# Expected output:
# ✓ vite v7.1.7 building for production...
# ✓ built in 3.42s
```

**Verify Frontend Build**:
```bash
# Should show TypeScript compilation success
npm run build
```

---

## Running Locally

### Option A: Two Terminal Windows (Development Mode)

**Terminal 1 - Backend API**:

```bash
cd backend/src/CaixaSeguradora.Api
dotnet run --urls "http://localhost:5555;https://localhost:5556"
```

**Expected Output**:
```
info: Microsoft.Hosting.Lifetime[14]
      Now listening on: http://localhost:5555
info: Microsoft.Hosting.Lifetime[14]
      Now listening on: https://localhost:5556
info: Microsoft.Hosting.Lifetime[0]
      Application started. Press Ctrl+C to shut down.
```

**Backend is ready when you see**:
- Swagger UI: https://localhost:5556/swagger
- Health check: https://localhost:5556/api/v1/health

---

**Terminal 2 - Frontend Dev Server**:

```bash
cd frontend
npm run dev
```

**Expected Output**:
```
  VITE v7.1.7  ready in 823 ms

  ➜  Local:   http://localhost:5173/
  ➜  Network: use --host to expose
  ➜  press h to show help
```

**Frontend is ready when you see**:
- Web UI: http://localhost:5173/
- Dashboard should load without errors

### Verify Services Are Running

**Check Backend Health**:
```bash
curl https://localhost:5556/api/v1/health --insecure
# Expected: {"status":"Healthy","timestamp":"2025-10-27T..."}
```

**Check Frontend**:
```bash
curl http://localhost:5173/
# Expected: HTML with <div id="root">
```

---

## Option B: Running with Docker

### Single Command Startup

```bash
# From project root
docker-compose up --build
```

**Expected Output**:
```
Creating network "poc-cobol_app-network" ... done
Creating caixa-db       ... done
Creating caixa-backend  ... done
Creating caixa-frontend ... done
Attaching to caixa-db, caixa-backend, caixa-frontend
caixa-backend  | Now listening on: http://[::]:8080
caixa-frontend | > vite preview --host 0.0.0.0 --port 80
```

**Service Mappings**:

| Service | Container Port | Host Port | URL |
|---------|----------------|-----------|-----|
| Backend API | 8080 | 5001 | http://localhost:5001 |
| Frontend | 80 | 5173 | http://localhost:5173 |
| SQLite DB | N/A | N/A | File-based (./data/premium_reporting.db) |

**docker-compose.yml Services**:

```yaml
services:
  backend:
    build: ./backend
    ports:
      - "5001:8080"
    volumes:
      - ./data:/app/data          # Database persistence
      - ./backend/logs:/app/logs  # Application logs
    environment:
      - ASPNETCORE_ENVIRONMENT=Development

  frontend:
    build: ./frontend
    ports:
      - "5173:80"
    volumes:
      - ./frontend/src:/app/src:ro  # Live reload
    depends_on:
      - backend

  db:
    image: nouchka/sqlite3:latest
    volumes:
      - ./data:/var/lib/sqlite3
```

**Stop Docker Services**:
```bash
# Graceful shutdown
docker-compose down

# Remove volumes (reset database)
docker-compose down -v
```

---

## Running Tests

### Backend Tests

```bash
cd backend

# Run all unit tests
dotnet test --filter Category=Unit

# Expected output:
# Passed!  - Failed:     0, Passed:   142, Skipped:     0, Total:   142

# Run integration tests (requires running database)
dotnet test --filter Category=Integration

# Run COBOL comparison tests (requires reference outputs in tests/TestData/)
dotnet test --filter Category=Comparison

# Run all tests with code coverage
dotnet test /p:CollectCoverage=true /p:CoverageReportFormat=html
# View report: tests/*/coverage/index.html
```

### Frontend Tests

```bash
cd frontend

# Run E2E tests (headless mode)
npm run test:e2e

# Expected output:
# Running 12 tests using 3 workers
# ✓ 12 passed (5.2s)

# Run E2E tests with UI (interactive debugging)
npm run test:e2e:ui

# Run E2E tests in headed mode (see browser)
npm run test:e2e:headed

# Run E2E tests with debugger
npm run test:e2e:debug
```

**Available Test Suites**:
- `tests/e2e/00-all-routes.spec.ts` - Navigation tests
- `tests/e2e/dashboard.spec.ts` - Dashboard component tests
- `tests/e2e/reports.spec.ts` - Report generation tests
- `tests/e2e/business-rules.spec.ts` - Business logic validation

---

## Generating Your First Report

### Method 1: Using curl (API)

```bash
# Generate PREMIT report (Premium by Endorsement Type)
curl -X POST https://localhost:5556/api/v1/reports/generate/premit \
  -H "Content-Type: application/json" \
  -d '{
    "startDate": "2025-10-01",
    "endDate": "2025-10-31",
    "companyCode": 1,
    "outputFormat": "TXT"
  }' \
  --insecure

# Expected response:
{
  "executionId": "550e8400-e29b-41d4-a716-446655440000",
  "status": "Processing",
  "startedAt": "2025-10-27T15:00:00Z"
}

# Check report status
curl https://localhost:5556/api/v1/reports/executions/550e8400-e29b-41d4-a716-446655440000 --insecure

# Download completed report
curl -o PREMIT.TXT https://localhost:5556/api/v1/reports/download/550e8400-e29b-41d4-a716-446655440000 --insecure
```

### Method 2: Using Swagger UI (Interactive)

1. **Open Swagger**: https://localhost:5556/swagger
2. **Expand** `/api/v1/reports/generate/premit`
3. **Click** "Try it out"
4. **Fill in request body**:
   ```json
   {
     "startDate": "2025-10-01",
     "endDate": "2025-10-31",
     "companyCode": 1,
     "outputFormat": "TXT"
   }
   ```
5. **Click** "Execute"
6. **Copy** `executionId` from response
7. **Use** `/api/v1/reports/download/{executionId}` to download file

### Method 3: Using Web Interface

1. **Open browser**: http://localhost:5173/
2. **Click** "Relatórios" in navigation
3. **Select** report type (PREMIT or PREMCED)
4. **Choose** date range
5. **Click** "Gerar Relatório"
6. **Monitor** progress in real-time
7. **Download** when status shows "Concluído"

**Expected Report Format** (PREMIT.TXT first 3 lines):
```
00000012345670000001234567890001234567    ABC SEGUROS LTDA           20251001
00000023456780000002345678900002345678    XYZ CORRETORA              20251002
00000034567890000003456789000034567890    DEF CONSULTORIA SA         20251003
```

**Report File Locations**:
- **Local run**: `backend/src/CaixaSeguradora.Api/Reports/`
- **Docker run**: `./data/Reports/`

---

## Common Issues & Troubleshooting

### Issue 1: Port 5555/5556 Already in Use

**Error**:
```
System.IO.IOException: Failed to bind to address https://127.0.0.1:5556
```

**Fix**: Change ports in `backend/src/CaixaSeguradora.Api/appsettings.json`:

```json
{
  "Ports": {
    "Http": "5557",   // Changed from 5555
    "Https": "5558"   // Changed from 5556
  }
}
```

Then restart backend with new URLs:
```bash
dotnet run --urls "http://localhost:5557;https://localhost:5558"
```

---

### Issue 2: Port 5173 Already in Use

**Error**:
```
Error: Port 5173 is already in use
```

**Fix**: Change Vite port in `frontend/vite.config.ts`:

```typescript
export default defineConfig({
  server: {
    port: 5174,  // Changed from 5173
  }
})
```

Or use environment variable:
```bash
PORT=5174 npm run dev
```

---

### Issue 3: Database Locked Error

**Error**:
```
SqliteException: database is locked
```

**Cause**: Multiple processes accessing SQLite file simultaneously.

**Fix**:
```bash
# Stop all backend processes
pkill -f "dotnet.*CaixaSeguradora.Api"

# Delete database file (will be recreated)
rm backend/src/CaixaSeguradora.Api/premium-reporting.db

# Reapply migrations
cd backend
dotnet ef database update --project src/CaixaSeguradora.Infrastructure --startup-project src/CaixaSeguradora.Api
```

---

### Issue 4: Missing EF Core Tools

**Error**:
```
Could not execute because the command or file was not found: dotnet-ef
```

**Fix**: Install Entity Framework Core CLI tools globally:
```bash
dotnet tool install --global dotnet-ef --version 9.0.0

# Verify installation
dotnet ef --version
# Expected: Entity Framework Core .NET Command-line Tools 9.0.0
```

---

### Issue 5: Node Modules Issues

**Error**:
```
Module not found: Error: Can't resolve 'react'
```

**Fix**: Delete and reinstall dependencies:
```bash
cd frontend

# Remove node_modules and package-lock.json
rm -rf node_modules package-lock.json

# Clear npm cache
npm cache clean --force

# Reinstall
npm install
```

---

### Issue 6: Certificate Trust Issues (HTTPS)

**Error**:
```
curl: (60) SSL certificate problem: self-signed certificate
```

**Fix 1** (Temporary - Development only):
```bash
# Use --insecure flag with curl
curl https://localhost:5556/api/v1/health --insecure
```

**Fix 2** (Permanent - macOS):
```bash
# Trust ASP.NET Core development certificate
dotnet dev-certs https --trust
```

**Fix 3** (Permanent - Linux):
```bash
# Export and trust certificate
dotnet dev-certs https -ep ${HOME}/.aspnet/https/aspnetapp.pfx -p YourPassword
sudo update-ca-certificates
```

---

### Issue 7: Frontend API Connection Refused

**Error in browser console**:
```
AxiosError: Network Error - connect ECONNREFUSED 127.0.0.1:5556
```

**Checklist**:
1. **Backend is running**: Check Terminal 1 for backend logs
2. **Correct port**: Verify `VITE_API_BASE_URL` in `frontend/.env`
3. **CORS enabled**: Check `appsettings.json` → `Cors.AllowedOrigins`

**Fix**: Create `frontend/.env.local`:
```bash
VITE_API_BASE_URL=http://localhost:5555/api/v1
```

Then restart frontend:
```bash
npm run dev
```

---

## Project Configuration

### Backend Configuration Files

**appsettings.json** (base configuration):
```json
{
  "ConnectionStrings": {
    "DefaultConnection": "Data Source=premium-reporting.db"
  },
  "Ports": {
    "Http": "5555",
    "Https": "5556"
  },
  "Cors": {
    "AllowedOrigins": ["http://localhost:5173"]
  }
}
```

**appsettings.Development.json** (overrides for local development):
```json
{
  "Logging": {
    "LogLevel": {
      "Default": "Debug",
      "Microsoft.EntityFrameworkCore": "Information"
    }
  }
}
```

**appsettings.Production.json** (production settings):
```json
{
  "ConnectionStrings": {
    "DefaultConnection": "Host=prod-db;Database=premiums;Username=app;Password={SECRET}"
  },
  "Logging": {
    "LogLevel": {
      "Default": "Warning"
    }
  }
}
```

### Frontend Configuration

**vite.config.ts**:
```typescript
export default defineConfig({
  plugins: [react()],
  server: {
    port: 5173,
    proxy: {
      '/api': {
        target: 'https://localhost:5556',
        secure: false  // Allow self-signed certificates
      }
    }
  }
})
```

**Environment Variables** (.env.local):
```bash
VITE_API_BASE_URL=http://localhost:5555/api/v1
VITE_APP_TITLE=Caixa Seguradora - Premium Reporting
```

### Docker Environment Variables

**docker-compose.yml**:
```yaml
services:
  backend:
    environment:
      - ASPNETCORE_ENVIRONMENT=Development
      - ConnectionStrings__DefaultConnection=Data Source=/app/data/premium_reporting.db
      - Cors__AllowedOrigins__0=http://localhost:5173

  frontend:
    environment:
      - VITE_API_BASE_URL=http://backend:8080/api/v1
```

**Override with .env file**:
```bash
# Create .env in project root
echo "ASPNETCORE_ENVIRONMENT=Staging" > .env
docker-compose up --env-file .env
```

---

## Next Steps After Setup

### 1. Understand the Data Model

**Read**: `docs/legacy-system/LEGACY_SYSTEM_DOCUMENTATION.md`
- 26+ database tables/views
- 687 COBOL data items
- Entity relationships

**Explore**: SQLite database:
```bash
# Open database with sqlite3 CLI
sqlite3 backend/src/CaixaSeguradora.Api/premium-reporting.db

# List all tables
.tables

# Inspect schema
.schema V0PREMIOS

# Sample query
SELECT * FROM V0PREMIOS LIMIT 5;
```

### 2. Review COBOL Context

**Read**: `RG1866B.cbl` (original COBOL source)
- Understand batch processing logic (sections R0500-R5500)
- Financial calculation rules (R0700-R1300)
- Cossurance calculations (R3000-R5500)

**Read**: `docs/parser/FINAL-ANALYSIS-REPORT.md`
- COBOL-to-.NET type mappings
- Complexity analysis (McCabe: 156, Function Points: 847)
- Database dependencies

### 3. Study Technical Decisions

**Read**: `specs/001-vamos-migrar-sistema/research.md`
- Why `decimal` instead of `float` for money
- Fixed-width file formatting rules
- Cursor-based processing for large datasets

**Read**: `CLAUDE.md`
- Coding conventions (PascalCase, async/await)
- Architecture patterns (Clean Architecture)
- Common pitfalls to avoid

### 4. Explore API Contracts

**Read**: `specs/001-vamos-migrar-sistema/contracts/openapi.yaml`
- 28 endpoints across 9 categories
- Request/response schemas
- Authentication requirements

**Experiment**: Use Swagger UI to test endpoints:
- https://localhost:5556/swagger

### 5. Review Implementation Roadmap

**Read**: `specs/001-vamos-migrar-sistema/tasks.md` (240 tasks)
- Organized into 8 user stories
- US1: Database setup ✓
- US2: Domain entities ✓
- US3: Business logic (in progress)
- US4: API endpoints
- US5: Frontend UI
- US6: Testing & validation
- US7: Deployment
- US8: Documentation

**Use SpecKit commands**:
```bash
# View current feature specification
/speckit.specify

# Generate implementation plan
/speckit.plan

# Generate task breakdown
/speckit.tasks

# Implement specific task
/speckit.implement T042
```

### 6. Join the Team

**Communication Channels**:
- Slack: #cobol-migration (for daily updates)
- Teams: COBOL Migration Team (for meetings)
- Email: cobol-migration@caixaseguradora.com.br

**Weekly Schedule**:
- Mondays 10:00 - Sprint planning
- Wednesdays 15:00 - Technical sync
- Fridays 14:00 - Sprint review & demo

### 7. Development Workflow

**Branch Naming**:
```bash
# Feature branches
git checkout -b feature/premium-calculation

# Bug fixes
git checkout -b fix/decimal-precision-cossurance

# Documentation
git checkout -b docs/update-quickstart
```

**Commit Message Format** (Conventional Commits):
```bash
git commit -m "feat: add premium calculation service"
git commit -m "fix: correct decimal precision in cossurance"
git commit -m "test: add comparison tests for PREMIT output"
git commit -m "docs: update quickstart with Docker instructions"
```

**Pre-Commit Checklist**:
- [ ] Run tests: `dotnet test && npm run test:e2e`
- [ ] Run comparison tests: `dotnet test --filter Category=Comparison`
- [ ] Format code: `dotnet format` (backend), `npm run lint:fix` (frontend)
- [ ] Verify coverage: 90%+ for new business logic
- [ ] Update documentation if API changes

---

## Quick Reference

### Backend Commands

```bash
# Build
dotnet build

# Run API
dotnet run --project src/CaixaSeguradora.Api

# Run with watch (hot reload)
dotnet watch run --project src/CaixaSeguradora.Api

# Apply migrations
dotnet ef database update --project src/CaixaSeguradora.Infrastructure

# Create new migration
dotnet ef migrations add MigrationName --project src/CaixaSeguradora.Infrastructure

# Generate SQL script from migrations
dotnet ef migrations script --project src/CaixaSeguradora.Infrastructure -o migration.sql

# Run tests
dotnet test
dotnet test --filter Category=Unit
dotnet test --filter Category=Comparison

# Code coverage
dotnet test /p:CollectCoverage=true /p:CoverageReportFormat=html
```

### Frontend Commands

```bash
# Install dependencies
npm install

# Run dev server
npm run dev

# Build for production
npm run build

# Preview production build
npm run preview

# Lint
npm run lint
npm run lint:fix

# E2E tests
npm run test:e2e
npm run test:e2e:ui
npm run test:e2e:headed
npm run test:e2e:debug
```

### Docker Commands

```bash
# Start all services
docker-compose up --build

# Start in background
docker-compose up -d

# Stop all services
docker-compose down

# Stop and remove volumes
docker-compose down -v

# View logs
docker-compose logs -f

# Rebuild specific service
docker-compose build backend
docker-compose up backend
```

### Database Commands (SQLite)

```bash
# Open database
sqlite3 premium-reporting.db

# Useful .commands in sqlite3
.tables                  # List all tables
.schema TABLE_NAME      # Show table structure
.mode column            # Better column display
.headers on             # Show column headers
.exit                   # Exit sqlite3

# Export data to CSV
sqlite3 premium-reporting.db "SELECT * FROM V0PREMIOS;" -csv > premiums.csv

# Import CSV data
sqlite3 premium-reporting.db ".mode csv" ".import premiums.csv V0PREMIOS"

# Backup database
sqlite3 premium-reporting.db ".backup backup-$(date +%Y%m%d).db"
```

---

## Support & Resources

### Documentation

- **Project README**: `README.md`
- **Developer Guide**: `CLAUDE.md`
- **Feature Specs**: `specs/*/spec.md`
- **API Reference**: `specs/*/contracts/openapi.yaml`
- **Legacy System**: `docs/legacy-system/LEGACY_SYSTEM_DOCUMENTATION.md`

### External References

- [.NET 9 Documentation](https://docs.microsoft.com/dotnet/core/whats-new/dotnet-9)
- [Entity Framework Core 9](https://docs.microsoft.com/ef/core/)
- [React 19 Documentation](https://react.dev/)
- [Vite Documentation](https://vitejs.dev/)
- [TailwindCSS Documentation](https://tailwindcss.com/docs)

### Getting Help

**Before asking questions**:
1. Check this quickstart guide
2. Search `CLAUDE.md` for coding patterns
3. Review `specs/*/spec.md` for feature requirements
4. Check GitHub issues for similar problems

**When asking for help**:
- Include error messages (full stack trace)
- Share steps to reproduce
- Mention environment (OS, .NET version, Node version)
- Provide relevant code snippets

---

**Last Updated**: October 27, 2025
**Version**: 1.0.0
**Maintainer**: COBOL Migration Team
