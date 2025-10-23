# Developer Quickstart Guide: COBOL RG1866B to .NET 9 Migration

**Feature Branch**: `001-vamos-migrar-sistema`
**Created**: October 22, 2025
**Status**: Phase 1.3 - Developer Onboarding
**Version**: 1.0

## Table of Contents

1. [Introduction](#introduction)
2. [Prerequisites](#prerequisites)
3. [Project Structure](#project-structure)
4. [Getting Started](#getting-started)
5. [Development Workflow](#development-workflow)
6. [Running Tests](#running-tests)
7. [Database Management](#database-management)
8. [API Documentation](#api-documentation)
9. [Frontend Development](#frontend-development)
10. [Troubleshooting](#troubleshooting)
11. [Additional Resources](#additional-resources)

---

## Introduction

Welcome to the SUSEP Circular 360 Premium Reporting System migration project! This guide will help you get up and running quickly with the development environment.

### What We're Building

A modern full-stack application that replaces the legacy COBOL RG1866B batch program with:

- **.NET 9 Web API**: RESTful backend with Clean Architecture
- **React Frontend**: Interactive dashboard and report generation UI
- **SQLite Database**: Local development database mimicking DB2 structure
- **Byte-Level Compatibility**: Exact output matching for regulatory compliance

### Project Goals

1. **Functional Parity**: Replicate all COBOL business logic exactly
2. **Modern UX**: Replace batch processing with interactive web interface
3. **Regulatory Compliance**: Maintain SUSEP Circular 360 report format compatibility
4. **Developer Experience**: Clean architecture, comprehensive tests, clear documentation

---

## Prerequisites

### Required Software

| Software | Version | Purpose | Download |
|----------|---------|---------|----------|
| **.NET SDK** | 9.0+ | Backend development | https://dotnet.microsoft.com/download |
| **Node.js** | 20+ LTS | Frontend development | https://nodejs.org/ |
| **Git** | 2.40+ | Version control | https://git-scm.com/ |
| **VS Code** | Latest | Code editor (recommended) | https://code.visualstudio.com/ |

### Optional but Recommended

| Software | Version | Purpose | Download |
|----------|---------|---------|----------|
| **Docker Desktop** | Latest | Containerization | https://www.docker.com/products/docker-desktop |
| **Postman** | Latest | API testing | https://www.postman.com/ |
| **DB Browser for SQLite** | Latest | Database inspection | https://sqlitebrowser.org/ |

### VS Code Extensions

Install these extensions for optimal development experience:

```bash
# C# Development
code --install-extension ms-dotnettools.csharp
code --install-extension ms-dotnettools.csdevkit

# React/TypeScript Development
code --install-extension dbaeumer.vscode-eslint
code --install-extension esbenp.prettier-vscode
code --install-extension bradlc.vscode-tailwindcss

# REST API Testing
code --install-extension humao.rest-client

# Git Integration
code --install-extension eamodio.gitlens
```

### Verify Prerequisites

Run these commands to verify your environment:

```bash
# .NET SDK
dotnet --version
# Expected: 9.0.0 or higher

# Node.js
node --version
# Expected: v20.0.0 or higher

# npm
npm --version
# Expected: 10.0.0 or higher

# Git
git --version
# Expected: 2.40.0 or higher
```

---

## Project Structure

The repository follows Clean Architecture principles with clear separation between backend, frontend, and specifications:

```
POC Cobol/
├── specs/                                  # Feature specifications (SpecKit)
│   └── 001-vamos-migrar-sistema/
│       ├── spec.md                         # Feature specification
│       ├── plan.md                         # Implementation plan
│       ├── research.md                     # Technical research
│       ├── data-model.md                   # Entity definitions
│       ├── quickstart.md                   # This file
│       ├── tasks.md                        # Implementation tasks (generated)
│       ├── contracts/
│       │   ├── openapi.yaml               # API specification
│       │   └── schemas/README.md          # API documentation
│       └── checklists/
│           └── requirements.md            # Quality validation
│
├── backend/                               # .NET 9 Backend
│   ├── CaixaSeguradora.sln               # Solution file
│   ├── src/
│   │   ├── CaixaSeguradora.Api/          # Web API layer
│   │   │   ├── Controllers/              # REST API controllers
│   │   │   ├── Program.cs                # Application entry point
│   │   │   ├── appsettings.json          # Configuration
│   │   │   └── appsettings.Development.json
│   │   │
│   │   ├── CaixaSeguradora.Core/         # Domain layer (Clean Architecture)
│   │   │   ├── Entities/                 # Domain entities
│   │   │   ├── Interfaces/               # Repository & service contracts
│   │   │   ├── Services/                 # Domain services
│   │   │   └── Attributes/               # CobolFieldAttribute, etc.
│   │   │
│   │   └── CaixaSeguradora.Infrastructure/ # Infrastructure layer
│   │       ├── Data/                     # EF Core DbContext
│   │       ├── Repositories/             # Repository implementations
│   │       ├── Services/                 # External service adapters
│   │       └── Formatters/               # FixedWidthFormatter, etc.
│   │
│   └── tests/
│       ├── CaixaSeguradora.UnitTests/    # Unit tests
│       ├── CaixaSeguradora.IntegrationTests/ # Integration tests
│       └── CaixaSeguradora.ComparisonTests/  # COBOL vs .NET validation
│
├── frontend/                              # React Frontend
│   ├── package.json                      # npm dependencies
│   ├── vite.config.ts                    # Vite configuration
│   ├── tailwind.config.js                # TailwindCSS config (Caixa branding)
│   ├── src/
│   │   ├── components/                   # React components
│   │   │   ├── dashboard/               # Dashboard components
│   │   │   ├── reports/                 # Report generation UI
│   │   │   └── query/                   # Data query UI
│   │   ├── pages/                       # Page components
│   │   ├── services/                    # API client layer
│   │   ├── styles/                      # Global styles
│   │   └── App.tsx                      # Root component
│   │
│   └── public/                          # Static assets
│
├── docs/                                 # Documentation
│   └── parser/                          # COBOL parser analysis
│       ├── FINAL-ANALYSIS-REPORT.md     # Complete program analysis
│       ├── INDEX.md                     # Documentation index
│       └── copybooks/
│           └── RG1866B_unix.cbl         # COBOL source code
│
└── CLAUDE.md                            # Project guide for Claude Code

```

### Key Directories

- **`specs/`**: Feature specifications following SpecKit methodology
- **`backend/src/`**: .NET 9 backend with Clean Architecture (Api → Core → Infrastructure)
- **`frontend/src/`**: React 18+ frontend with TypeScript and TailwindCSS
- **`docs/parser/`**: COBOL program analysis and legacy source code
- **`tests/`**: Comprehensive test suite (unit, integration, comparison)

---

## Getting Started

### 1. Clone the Repository

```bash
# Clone the repository
git clone <repository-url>
cd "POC Cobol"

# Checkout the feature branch
git checkout 001-vamos-migrar-sistema
```

### 2. Backend Setup

#### Install Dependencies

```bash
cd backend

# Restore NuGet packages
dotnet restore

# Verify build
dotnet build --configuration Debug
```

#### Configure Database

The application uses SQLite for local development. The database file will be created automatically on first run.

```bash
# Navigate to API project
cd src/CaixaSeguradora.Api

# Apply migrations (creates database schema)
dotnet ef database update

# Load sample mock data (optional)
dotnet run --seed-data
```

#### Run Backend API

```bash
# From backend/src/CaixaSeguradora.Api/
dotnet run

# Or with hot reload
dotnet watch run
```

**Expected Output**:
```
info: Microsoft.Hosting.Lifetime[14]
      Now listening on: https://localhost:5001
info: Microsoft.Hosting.Lifetime[14]
      Now listening on: http://localhost:5000
info: Microsoft.Hosting.Lifetime[0]
      Application started. Press Ctrl+C to shut down.
```

**Swagger UI**: Open https://localhost:5001/swagger in your browser

### 3. Frontend Setup

#### Install Dependencies

```bash
# From project root
cd frontend

# Install npm packages
npm install
```

#### Configure Environment

Create `.env.local` file for local development:

```bash
# frontend/.env.local
VITE_API_BASE_URL=https://localhost:5001/api/v1
VITE_APP_NAME="SUSEP Premium Reporting"
VITE_ENABLE_MOCK_DATA=true
```

#### Run Frontend Development Server

```bash
# From frontend/
npm run dev
```

**Expected Output**:
```
VITE v5.0.0  ready in 423 ms

➜  Local:   http://localhost:5173/
➜  Network: use --host to expose
➜  press h + enter to show help
```

**Dashboard**: Open http://localhost:5173 in your browser

### 4. Verify Setup

#### Test Backend API

```bash
# Health check
curl https://localhost:5001/api/v1/system/health

# Expected: {"status":"healthy","timestamp":"..."}

# Get dashboard metrics
curl https://localhost:5001/api/v1/dashboard/metrics

# Expected: JSON with program info, data structure, complexity metrics
```

#### Test Frontend

1. Navigate to http://localhost:5173
2. Verify dashboard loads with migration metrics
3. Check that all sections display data (Program Info, Data Structure, Complexity)

---

## Development Workflow

### Daily Development Cycle

```bash
# 1. Pull latest changes
git pull origin 001-vamos-migrar-sistema

# 2. Start backend (Terminal 1)
cd backend/src/CaixaSeguradora.Api
dotnet watch run

# 3. Start frontend (Terminal 2)
cd frontend
npm run dev

# 4. Make code changes
# Edit files in VS Code

# 5. Run tests
dotnet test                    # Backend tests
npm run test                   # Frontend tests

# 6. Commit changes
git add .
git commit -m "feat: add premium query filtering"
git push origin 001-vamos-migrar-sistema
```

### Git Workflow

We follow **trunk-based development** with short-lived feature branches:

```bash
# Create feature branch from main feature branch
git checkout 001-vamos-migrar-sistema
git checkout -b feature/premium-statistics

# Make changes and commit
git add .
git commit -m "feat: implement premium statistics endpoint"

# Push and create pull request
git push origin feature/premium-statistics
```

#### Commit Message Convention

Follow [Conventional Commits](https://www.conventionalcommits.org/):

```
feat: add new feature
fix: bug fix
docs: documentation changes
style: code formatting (no logic change)
refactor: code refactoring
test: add or update tests
chore: build/config changes
```

**Examples**:
```bash
git commit -m "feat: add PREMIT report generation endpoint"
git commit -m "fix: correct decimal precision in premium calculations"
git commit -m "test: add comparison tests for COBOL output validation"
git commit -m "docs: update API documentation with cossurance examples"
```

### Code Style

#### Backend (C#)

- **Naming**: PascalCase for classes/methods, camelCase for variables
- **Formatting**: Use `dotnet format` (configured in `.editorconfig`)
- **Architecture**: Follow Clean Architecture - no business logic in controllers
- **Error Handling**: Always log errors with Serilog before throwing

```csharp
// Good: Clean Architecture separation
public class PremiumController : ControllerBase
{
    private readonly IPremiumService _premiumService;

    [HttpGet("{id}")]
    public async Task<ActionResult<PremiumRecord>> GetPremium(long id)
    {
        var premium = await _premiumService.GetByIdAsync(id);
        if (premium == null)
            return NotFound();
        return Ok(premium);
    }
}

// Good: Decimal precision for financial calculations
[CobolField(PicClause = "9(13)V99", DecimalPlaces = 2)]
public decimal TotalPremiumAmount { get; set; }  // Use decimal, not double!

// Bad: Business logic in controller
public ActionResult Calculate()
{
    var result = premium * rate;  // ❌ Move to service layer
    return Ok(result);
}
```

#### Frontend (TypeScript/React)

- **Naming**: PascalCase for components, camelCase for functions/variables
- **Formatting**: Use Prettier (configured in `.prettierrc`)
- **Components**: Functional components with hooks
- **Styling**: TailwindCSS utility classes (avoid inline styles)

```tsx
// Good: Functional component with TypeScript
interface PremiumCardProps {
  premium: PremiumRecord;
  onSelect: (id: number) => void;
}

export const PremiumCard: React.FC<PremiumCardProps> = ({ premium, onSelect }) => {
  return (
    <div className="rounded-lg border border-gray-200 p-4 hover:shadow-md">
      <h3 className="text-lg font-semibold text-caixa-blue">
        Apólice {premium.policyNumber}
      </h3>
      <p className="text-gray-600">
        Prêmio: R$ {premium.totalPremiumAmount.toFixed(2)}
      </p>
      <button
        onClick={() => onSelect(premium.premiumId)}
        className="mt-2 rounded bg-caixa-blue px-4 py-2 text-white hover:bg-caixa-blue-dark"
      >
        Ver Detalhes
      </button>
    </div>
  );
};

// Good: Use TypeScript interfaces for API responses
interface ApiResponse<T> {
  data: T;
  pagination?: PaginationInfo;
}
```

---

## Running Tests

### Backend Tests

```bash
# Run all tests
cd backend
dotnet test

# Run with detailed output
dotnet test --logger "console;verbosity=detailed"

# Run specific test project
dotnet test tests/CaixaSeguradora.UnitTests/

# Run with coverage
dotnet test /p:CollectCoverage=true /p:CoverageReportFormat=html
# Coverage report: tests/*/coverage/index.html
```

#### Test Categories

**Unit Tests**: Test individual components in isolation
```bash
dotnet test --filter Category=Unit
```

**Integration Tests**: Test database and external dependencies
```bash
dotnet test --filter Category=Integration
```

**Comparison Tests**: Validate COBOL vs .NET output
```bash
dotnet test --filter Category=Comparison
```

### Frontend Tests

```bash
cd frontend

# Run unit tests (Vitest)
npm run test

# Run with watch mode
npm run test:watch

# Run with coverage
npm run test:coverage

# Run E2E tests (Playwright)
npm run test:e2e

# Run E2E in UI mode
npm run test:e2e:ui
```

### Test Data

Use mock data for development and testing:

```bash
# Load mock premium data
curl -X POST https://localhost:5001/api/v1/mock-data/load \
  -F "file=@test-data/premiums.csv" \
  -F "entityType=premiums"

# Validate loaded data
curl -X POST https://localhost:5001/api/v1/mock-data/validate

# Reset database to clean state
curl -X POST https://localhost:5001/api/v1/mock-data/reset
```

---

## Database Management

### Entity Framework Core Migrations

#### Create Migration

```bash
cd backend/src/CaixaSeguradora.Api

# Create new migration
dotnet ef migrations add AddPremiumIndexes

# Review generated migration in Migrations/ folder
```

#### Apply Migrations

```bash
# Update database to latest migration
dotnet ef database update

# Update to specific migration
dotnet ef database update AddPremiumIndexes

# Rollback to previous migration
dotnet ef database update PreviousMigrationName
```

#### View Migration SQL

```bash
# Generate SQL script
dotnet ef migrations script > migration.sql

# Generate SQL for specific range
dotnet ef migrations script FromMigration ToMigration > migration.sql
```

### Inspect Database

Use DB Browser for SQLite to inspect the database:

```bash
# Database location
backend/src/CaixaSeguradora.Api/premium_reporting.db
```

**Useful Queries**:

```sql
-- Count premium records
SELECT COUNT(*) FROM V0PREMIOS;

-- Check data distribution by month
SELECT ReferenceYear, ReferenceMonth, COUNT(*) as RecordCount
FROM V0PREMIOS
GROUP BY ReferenceYear, ReferenceMonth
ORDER BY ReferenceYear DESC, ReferenceMonth DESC;

-- Verify foreign key integrity
SELECT COUNT(*) FROM V0PREMIOS p
LEFT JOIN V0APOLICE a ON p.PolicyNumber = a.PolicyNumber
WHERE a.PolicyNumber IS NULL;
```

---

## API Documentation

### Swagger/OpenAPI

When the backend is running, access interactive API documentation at:

**Swagger UI**: https://localhost:5001/swagger

Features:
- Browse all endpoints
- View request/response schemas
- Execute API calls directly from browser
- Download OpenAPI spec

### Testing API with Postman

1. Import OpenAPI spec into Postman:
   - File → Import → `specs/001-vamos-migrar-sistema/contracts/openapi.yaml`

2. Set environment variables:
   - `baseUrl`: `https://localhost:5001/api/v1`
   - `bearerToken`: (JWT token from authentication)

3. Test common workflows:
   - Generate report
   - Query premiums
   - View policy details

### Testing API with curl

```bash
# Generate report
curl -X POST https://localhost:5001/api/v1/reports/generate \
  -H "Content-Type: application/json" \
  -d '{
    "startDate": "2025-10-01",
    "endDate": "2025-10-31",
    "reportTypes": ["PREMIT"],
    "systemId": "GL",
    "processingMode": "MONTHLY"
  }'

# Get report status
curl https://localhost:5001/api/v1/reports/{reportId}

# Query premiums
curl "https://localhost:5001/api/v1/premiums?page=1&pageSize=20&startDate=2025-10-01"

# Get dashboard metrics
curl https://localhost:5001/api/v1/dashboard/metrics
```

---

## Frontend Development

### Project Structure

```
frontend/src/
├── components/           # Reusable components
│   ├── dashboard/       # Dashboard-specific components
│   ├── reports/         # Report generation components
│   ├── query/           # Query builder components
│   └── common/          # Shared UI components
├── pages/               # Page components (routes)
│   ├── DashboardPage.tsx
│   ├── ReportPage.tsx
│   └── QueryPage.tsx
├── services/            # API client layer
│   ├── apiClient.ts     # Axios instance
│   ├── premiumService.ts
│   ├── reportService.ts
│   └── types.ts         # TypeScript interfaces
├── hooks/               # Custom React hooks
├── utils/               # Utility functions
└── App.tsx              # Root component
```

### TailwindCSS with Caixa Branding

Custom color palette configured in `tailwind.config.js`:

```javascript
module.exports = {
  theme: {
    extend: {
      colors: {
        caixa: {
          blue: {
            DEFAULT: '#0047BB',
            dark: '#003380',
            light: '#E6F0FF',
          },
          yellow: {
            DEFAULT: '#FFB81C',
            dark: '#E6A519',
          },
        },
      },
    },
  },
};
```

**Usage in components**:

```tsx
<div className="bg-caixa-blue text-white">Caixa Seguradora</div>
<button className="bg-caixa-yellow hover:bg-caixa-yellow-dark">Gerar Relatório</button>
```

### API Integration

Use generated TypeScript client from OpenAPI spec:

```bash
# Generate client (run once, or after API changes)
cd frontend
npm run generate-api-client
```

**Example usage**:

```tsx
import { premiumService } from '@/services/premiumService';

// Query premiums
const { data } = await premiumService.queryPremiums({
  startDate: '2025-10-01',
  endDate: '2025-10-31',
  page: 1,
  pageSize: 20,
});

console.log(data.premiums);
console.log(data.pagination.totalRecords);
```

### State Management

Use React hooks for local state, React Query for server state:

```tsx
import { useQuery } from '@tanstack/react-query';
import { premiumService } from '@/services/premiumService';

export const PremiumList = () => {
  const { data, isLoading, error } = useQuery({
    queryKey: ['premiums', { page: 1 }],
    queryFn: () => premiumService.queryPremiums({ page: 1, pageSize: 20 }),
  });

  if (isLoading) return <Spinner />;
  if (error) return <ErrorMessage error={error} />;

  return (
    <div>
      {data.premiums.map(premium => (
        <PremiumCard key={premium.premiumId} premium={premium} />
      ))}
    </div>
  );
};
```

---

## Troubleshooting

### Common Issues

#### Backend Won't Start

**Error**: `Unable to bind to https://localhost:5001`

**Solution**: Port already in use
```bash
# Find process using port
lsof -i :5001

# Kill process
kill -9 <PID>

# Or use different port
dotnet run --urls "https://localhost:5002"
```

**Error**: `No such file or directory: premium_reporting.db`

**Solution**: Run migrations
```bash
cd backend/src/CaixaSeguradora.Api
dotnet ef database update
```

#### Frontend Build Errors

**Error**: `Module not found: Can't resolve '@/services/...'`

**Solution**: Path alias issue
```bash
# Verify tsconfig.json has path mappings
{
  "compilerOptions": {
    "paths": {
      "@/*": ["./src/*"]
    }
  }
}

# Restart dev server
npm run dev
```

**Error**: `PostCSS plugin tailwindcss requires PostCSS 8`

**Solution**: Reinstall dependencies
```bash
rm -rf node_modules package-lock.json
npm install
```

#### Database Issues

**Error**: `SqliteException: database is locked`

**Solution**: Close other connections
```bash
# Close DB Browser for SQLite
# Restart backend API
```

**Error**: `Foreign key constraint failed`

**Solution**: Load data in correct order
```bash
# Load in dependency order:
# 1. Products, Clients, Agencies, Producers
# 2. Policies
# 3. Endorsements, Coverages
# 4. Premiums
```

### Debug Mode

#### Backend Debugging (VS Code)

Create `.vscode/launch.json`:

```json
{
  "version": "0.2.0",
  "configurations": [
    {
      "name": ".NET Core Launch (web)",
      "type": "coreclr",
      "request": "launch",
      "preLaunchTask": "build",
      "program": "${workspaceFolder}/backend/src/CaixaSeguradora.Api/bin/Debug/net9.0/CaixaSeguradora.Api.dll",
      "args": [],
      "cwd": "${workspaceFolder}/backend/src/CaixaSeguradora.Api",
      "env": {
        "ASPNETCORE_ENVIRONMENT": "Development"
      }
    }
  ]
}
```

Set breakpoints and press F5 to debug.

#### Frontend Debugging

Use browser DevTools:
- Chrome: F12 → Sources tab
- Set breakpoints in TypeScript files
- Use `debugger;` statement to break

### Logging

#### Backend Logs

Serilog configured in `appsettings.json`:

```json
{
  "Serilog": {
    "MinimumLevel": {
      "Default": "Information",
      "Override": {
        "Microsoft": "Warning",
        "System": "Warning"
      }
    }
  }
}
```

**View logs**:
```bash
# Console output during development
dotnet watch run

# Production logs (if using Seq)
# Open http://localhost:5341
```

#### Frontend Logs

Console logging with levels:

```typescript
console.log('Info message');
console.warn('Warning message');
console.error('Error message');

// Production: Logs sent to monitoring service
```

---

## Additional Resources

### Documentation

- **Feature Specification**: `specs/001-vamos-migrar-sistema/spec.md`
- **Implementation Plan**: `specs/001-vamos-migrar-sistema/plan.md`
- **Technical Research**: `specs/001-vamos-migrar-sistema/research.md`
- **Data Model**: `specs/001-vamos-migrar-sistema/data-model.md`
- **API Documentation**: `specs/001-vamos-migrar-sistema/contracts/schemas/README.md`
- **COBOL Analysis**: `docs/parser/FINAL-ANALYSIS-REPORT.md`

### External Resources

#### .NET 9

- [ASP.NET Core Documentation](https://learn.microsoft.com/en-us/aspnet/core/)
- [Entity Framework Core](https://learn.microsoft.com/en-us/ef/core/)
- [Clean Architecture Guide](https://learn.microsoft.com/en-us/dotnet/architecture/modern-web-apps-azure/)

#### React

- [React Documentation](https://react.dev/)
- [TypeScript Handbook](https://www.typescriptlang.org/docs/)
- [TailwindCSS Documentation](https://tailwindcss.com/docs)
- [React Query (TanStack Query)](https://tanstack.com/query/latest)

#### Tools

- [Postman Learning Center](https://learning.postman.com/)
- [Git Documentation](https://git-scm.com/doc)
- [VS Code Tips](https://code.visualstudio.com/docs/getstarted/tips-and-tricks)

### Team Communication

- **Slack Channel**: #susep-migration
- **Stand-up**: Daily at 9:30 AM
- **Sprint Planning**: Every 2 weeks on Monday
- **Code Review**: All PRs require 1 approval

### Getting Help

1. **Check Documentation**: Start with this quickstart and referenced docs
2. **Search Issues**: Check GitHub issues for similar problems
3. **Ask Team**: Post in #susep-migration Slack channel
4. **Create Issue**: If bug or feature request, create GitHub issue with:
   - Clear description
   - Steps to reproduce (for bugs)
   - Expected vs actual behavior
   - Environment details (OS, .NET version, etc.)

---

## Quick Reference Commands

### Backend

```bash
# Build
dotnet build

# Run
dotnet run
dotnet watch run    # with hot reload

# Test
dotnet test
dotnet test --filter Category=Unit

# Format
dotnet format

# Migrations
dotnet ef migrations add MigrationName
dotnet ef database update
```

### Frontend

```bash
# Install
npm install

# Run
npm run dev

# Test
npm run test
npm run test:e2e

# Build
npm run build
npm run preview     # preview production build

# Lint
npm run lint
npm run lint:fix
```

### Git

```bash
# Status
git status

# Commit
git add .
git commit -m "feat: description"

# Push
git push origin 001-vamos-migrar-sistema

# Pull latest
git pull origin 001-vamos-migrar-sistema

# Create feature branch
git checkout -b feature/branch-name
```

---

## Next Steps

Now that your environment is set up:

1. ✅ **Familiarize with Codebase**: Browse `specs/` documentation
2. ✅ **Run Application**: Start backend and frontend, verify dashboard loads
3. ✅ **Review Tasks**: Check `specs/001-vamos-migrar-sistema/tasks.md` (after Phase 2)
4. ✅ **Pick First Task**: Start with smallest, well-defined task
5. ✅ **Create Feature Branch**: Branch from `001-vamos-migrar-sistema`
6. ✅ **Implement, Test, Commit**: Follow development workflow
7. ✅ **Create Pull Request**: Request code review

**Welcome to the team! Happy coding! 🚀**

---

**Document Version**: 1.0
**Status**: ✅ Complete - Ready for Phase 1.4 (Agent Context Update)
**Created**: October 22, 2025
