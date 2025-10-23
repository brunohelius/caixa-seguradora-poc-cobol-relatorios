# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This repository contains the COBOL RG1866B to .NET 9 migration project for Caixa Seguradora's SUSEP Circular 360 Premium Reporting System. The project migrates a legacy COBOL batch program (~5,000 lines processing 687 data items across 26+ database tables) to a modern full-stack application with .NET 9 backend and React frontend.

**Critical Constraint**: All financial calculations must maintain byte-for-byte compatibility with COBOL output for regulatory compliance (SUSEP).

## Architecture

### Clean Architecture (Three-Layer Backend)

**Dependency Rule**: Dependencies flow inward only. Core has zero external dependencies.

```
┌─────────────────────────────────────────────────────┐
│  CaixaSeguradora.Api (ASP.NET Core Web API)        │
│  - Controllers (HTTP concerns only)                 │
│  - Program.cs (DI, middleware, Swagger)             │
└─────────────────┬───────────────────────────────────┘
                  │ depends on
┌─────────────────▼───────────────────────────────────┐
│  CaixaSeguradora.Core (Domain Layer)               │
│  - Entities (PremiumRecord, Policy, etc.)          │
│  - Interfaces (IRepository, IService)              │
│  - Services (Business logic - calculation rules)   │
│  - DTOs (Data transfer objects)                    │
│  - NO external dependencies                        │
└─────────────────┬───────────────────────────────────┘
                  │ implemented by
┌─────────────────▼───────────────────────────────────┐
│  CaixaSeguradora.Infrastructure                    │
│  - Data/DbContext (EF Core)                        │
│  - Repositories (Data access)                      │
│  - Services (External integrations, file I/O)      │
│  - Formatters (FixedWidthFormatter for COBOL)     │
└─────────────────────────────────────────────────────┘
```

**Key Point**: Never put business logic in controllers or repositories. All business rules belong in `Core/Services/`.

### Frontend Architecture

React SPA with component-based structure:
- **Pages**: Route components (DashboardPage, ReportGenerationPage, etc.)
- **Components**: Reusable UI components organized by feature (dashboard/, reports/, query/)
- **Services**: API client layer (Axios) - all HTTP calls go through services
- **Hooks**: Custom React hooks for shared logic
- **TailwindCSS**: Utility-first styling with Caixa Seguradora brand colors

## Key Technical Decisions

### Financial Calculations (NON-NEGOTIABLE)

```csharp
// ✅ CORRECT: Use decimal for all financial calculations
public decimal CalculatePremium(decimal amount, decimal rate)
{
    return amount * rate; // Exact decimal arithmetic
}

// ❌ WRONG: Never use float or double
public double CalculatePremium(double amount, double rate)
{
    return amount * rate; // Introduces rounding errors - breaks compliance
}
```

**Why**: C# `decimal` type matches COBOL packed decimal (COMP-3) precision. Using `float`/`double` causes rounding errors that fail regulatory byte-level comparison.

### COBOL Type Mapping

All entity properties must preserve COBOL metadata using `[CobolField]` attribute:

```csharp
public class PremiumRecord
{
    // COBOL: PIC 9(15)V99 (implied decimal point)
    [CobolField(PicClause = "9(15)V99", Length = 17, DecimalPlaces = 2)]
    [Column(TypeName = "decimal(17,2)")]
    public decimal TotalPremiumAmount { get; set; }

    // COBOL: PIC X(10) (fixed-width string, right-padded with spaces)
    [CobolField(PicClause = "X(10)", Length = 10)]
    [MaxLength(10)]
    public string PolicyNumber { get; set; }
}
```

**See**: `specs/001-vamos-migrar-sistema/research.md` section R1 for complete type mapping table.

### Fixed-Width File Generation

PREMIT.TXT and PREMCED.TXT files must match COBOL output exactly (byte-for-byte):

```csharp
// Use FixedWidthFormatter (in Infrastructure/Formatters/)
var formatter = new FixedWidthFormatter();

// Numeric fields: left-pad with zeros
string formatted = formatter.FormatNumeric(12345.67m, totalWidth: 15, decimalPlaces: 2);
// Output: "000000001234567" (no decimal point, implied after 2 digits)

// String fields: right-pad with spaces
string formatted = formatter.FormatAlphanumeric("ABC", width: 10);
// Output: "ABC       " (7 spaces)
```

**Critical**: All padding must match COBOL WRITE statement behavior. Any deviation breaks regulatory compliance.

### Cursor-Based Processing

For large datasets (millions of records), use `IAsyncEnumerable<T>` to replicate COBOL cursor behavior:

```csharp
// Repository method with streaming
public async IAsyncEnumerable<PremiumRecord> GetPremiumsAsync(
    DateTime startDate,
    DateTime endDate,
    [EnumeratorCancellation] CancellationToken cancellationToken = default)
{
    var query = _context.Premiums
        .AsNoTracking()  // Read-only optimization
        .Where(p => p.EffectiveDate >= startDate && p.EffectiveDate <= endDate)
        .OrderBy(p => p.PolicyNumber);

    await foreach (var record in query.AsAsyncEnumerable()
        .WithCancellation(cancellationToken))
    {
        yield return record;
    }
}
```

**Why**: Prevents memory overflow when processing 10,000+ records. Matches COBOL FETCH behavior from sections R0500-R0600.

## Development Commands

### Backend (.NET 9)

```bash
# Navigate to backend
cd backend

# Build solution
dotnet build

# Run API (starts on https://localhost:5001)
cd src/CaixaSeguradora.Api
dotnet run

# Run with hot reload
dotnet watch run

# Run all tests
dotnet test

# Run specific test category
dotnet test --filter Category=Unit
dotnet test --filter Category=Comparison

# Run tests with coverage
dotnet test /p:CollectCoverage=true /p:CoverageReportFormat=html

# EF Core migrations
dotnet ef migrations add MigrationName
dotnet ef database update
dotnet ef migrations script > migration.sql

# Code formatting
dotnet format
```

### Frontend (React + Vite)

```bash
# Navigate to frontend
cd frontend

# Install dependencies
npm install

# Run dev server (starts on http://localhost:5173)
npm run dev

# Build for production
npm run build

# Preview production build
npm run preview

# Run tests
npm run test
npm run test:watch
npm run test:coverage

# Run E2E tests
npm run test:e2e
npm run test:e2e:ui

# Lint and format
npm run lint
npm run lint:fix
npm run format
```

### Docker

```bash
# Start all services (backend + frontend + database)
docker-compose up --build

# Start in background
docker-compose up -d

# Stop all services
docker-compose down

# View logs
docker-compose logs -f
```

## Database Management

### SQLite Schema

Database file location: `backend/src/CaixaSeguradora.Api/premium_reporting.db`

**Important**: Schema mirrors 26+ DB2 views/tables from COBOL program:
- V0PREMIOS (premium records)
- V0APOLICE (policies)
- V0ENDOSSO (endorsements)
- V0PRODUTO (products)
- V0CLIENTE (clients)
- V0ENDERECOS (addresses)
- GE399 (cossurance calculations)
- etc.

### Entity Framework Core

All entities are in `CaixaSeguradora.Core/Entities/` with fluent API configurations in `CaixaSeguradora.Infrastructure/Data/Configurations/`.

**Pattern**: One entity class + one configuration class per database view/table.

```csharp
// Entity: CaixaSeguradora.Core/Entities/Policy.cs
public class Policy
{
    public long PolicyNumber { get; set; }
    public int CompanyCode { get; set; }
    // ... other properties
}

// Configuration: CaixaSeguradora.Infrastructure/Data/Configurations/PolicyConfiguration.cs
public class PolicyConfiguration : IEntityTypeConfiguration<Policy>
{
    public void Configure(EntityTypeBuilder<Policy> builder)
    {
        builder.ToView("V0APOLICE");  // Maps to DB2 view
        builder.HasKey(p => p.PolicyNumber);
        // ... indexes, relationships
    }
}
```

### Mock Data Loading

Use `/api/v1/mock-data/load` endpoint or:

```bash
# Load sample CSV data
curl -X POST https://localhost:5001/api/v1/mock-data/load \
  -F "file=@backend/tests/SampleData/premiums.csv" \
  -F "entityType=premiums"

# Validate loaded data
curl -X POST https://localhost:5001/api/v1/mock-data/validate

# Reset database
curl -X POST https://localhost:5001/api/v1/mock-data/reset
```

## Testing Strategy

### Test Pyramid

```
       ┌─────────────────┐
       │  E2E Tests      │  Playwright (frontend user journeys)
       └─────────────────┘
      ┌───────────────────┐
      │ Integration Tests │  API tests, database tests
      └───────────────────┘
    ┌──────────────────────┐
    │    Unit Tests        │  Service logic, calculations
    └──────────────────────┘
  ┌────────────────────────────┐
  │  Comparison Tests          │  COBOL vs .NET output validation
  └────────────────────────────┘
```

### COBOL Comparison Tests (CRITICAL)

**Regulatory Requirement**: 100% byte-for-byte match with COBOL output.

Located in `backend/tests/CaixaSeguradora.ComparisonTests/`:

```csharp
[Fact]
public async Task PremitOutput_MatchesCOBOL_ByteForByte()
{
    // Given: Sample input data
    var startDate = new DateTime(2025, 10, 1);
    var endDate = new DateTime(2025, 10, 31);

    // When: Generate .NET output
    var dotnetOutput = await _reportService.GeneratePremitAsync(startDate, endDate);

    // Then: Compare with COBOL sample
    var cobolOutput = File.ReadAllBytes("TestData/COBOL_PREMIT_Oct2025.TXT");
    var validator = new OutputValidator();
    var result = validator.CompareFiles(cobolOutput, dotnetOutput);

    Assert.True(result.Match, $"Byte mismatch at position {result.FirstDifference}");
}
```

**Run before every commit**: `dotnet test --filter Category=Comparison`

### Unit Test Coverage

**Target**: 90%+ coverage for business logic (constitution requirement).

Focus on:
- `CaixaSeguradora.Core/Services/` (all calculation logic)
- Premium calculations (COBOL sections R0700-R1300)
- Cossurance calculations (COBOL sections R3000-R5500)

```bash
# Check coverage
dotnet test /p:CollectCoverage=true
# View report at tests/*/coverage/index.html
```

## API Documentation

### Swagger UI

When backend is running: **https://localhost:5001/swagger**

### OpenAPI Specification

Complete API contract: `specs/001-vamos-migrar-sistema/contracts/openapi.yaml`

**28 endpoints across 9 categories**:
- Reports (5): Generate, status, download, history, compare
- Premiums (3): Query, details, statistics
- Policies (4): Details, endorsements, coverages, cossurance
- Products (2): List, details
- Clients (2): Details, addresses
- Batch Jobs (4): Create, list, details, executions
- Mock Data (3): Load, validate, reset
- Dashboard (3): Metrics, function points, dependencies
- System (2): Health, configuration

## Code Style & Conventions

### Backend (C#)

**Follow .editorconfig settings** (automatic in VS Code with C# extension).

```csharp
// Class names: PascalCase
public class PremiumCalculationService { }

// Method names: PascalCase
public async Task<decimal> CalculatePremiumAsync() { }

// Private fields: _camelCase
private readonly IPremiumRepository _premiumRepository;

// Properties: PascalCase
public decimal TotalAmount { get; set; }

// Constants: UPPER_SNAKE_CASE
private const int MAX_RETRY_ATTEMPTS = 3;
```

**Async Methods**: Always suffix with `Async` and return `Task<T>`.

### Frontend (TypeScript/React)

**Follow ESLint + Prettier configuration**.

```typescript
// Components: PascalCase
export const DashboardPage: React.FC = () => { };

// Functions: camelCase
export const calculateTotal = (amount: number): number => { };

// Interfaces: PascalCase, prefix with 'I' optional
interface PremiumRecord { }

// Constants: UPPER_SNAKE_CASE
const API_BASE_URL = 'https://localhost:5001/api/v1';
```

**TailwindCSS**: Use utility classes, avoid inline styles.

```tsx
// ✅ Good
<div className="rounded-lg border border-gray-200 p-4 hover:shadow-md">

// ❌ Avoid
<div style={{ borderRadius: '8px', border: '1px solid #e5e7eb' }}>
```

### Caixa Seguradora Branding

Primary colors defined in `frontend/tailwind.config.js`:
- **Blue**: `#0047BB` (primary brand color)
- **Yellow**: `#FFB81C` (accent color)

```tsx
<button className="bg-caixa-blue text-white hover:bg-caixa-blue-dark">
  Gerar Relatório
</button>
```

### Portuguese Language

**All user-facing content must be in Brazilian Portuguese** (FR-020):
- UI labels and buttons
- Error messages
- Validation messages
- API responses (error messages)

```csharp
// ✅ Correct
throw new ValidationException("Data inicial não pode ser maior que data final");

// ❌ Wrong
throw new ValidationException("Start date cannot be greater than end date");
```

## Project Structure Conventions

### Backend File Organization

```
CaixaSeguradora.Core/
├── Entities/           # One file per entity (Policy.cs, Premium.cs)
├── Interfaces/         # One interface per repository/service
├── Services/           # Business logic implementations
├── DTOs/              # Request/response objects
└── Attributes/        # Custom attributes (CobolFieldAttribute.cs)

CaixaSeguradora.Infrastructure/
├── Data/
│   ├── Configurations/  # One EF config per entity
│   └── PremiumReportingDbContext.cs
├── Repositories/       # One repository per entity
├── Services/          # External services, file I/O
└── Formatters/        # FixedWidthFormatter.cs

CaixaSeguradora.Api/
├── Controllers/       # One controller per resource
├── Middleware/        # Custom middleware
└── Program.cs        # DI, CORS, Swagger, logging
```

### Frontend File Organization

```
frontend/src/
├── components/
│   ├── common/       # Reusable across features
│   ├── dashboard/    # Dashboard-specific
│   ├── reports/      # Report generation
│   ├── query/        # Data query
│   └── batch/        # Batch jobs
├── pages/           # One file per route
├── services/        # API clients (dashboardService.ts, etc.)
├── hooks/           # Custom React hooks
├── utils/           # Helper functions
└── App.tsx          # Router configuration
```

## Common Pitfalls

### ❌ Don't: Use float/double for Money

```csharp
public double PremiumAmount { get; set; }  // WRONG - causes rounding errors
```

**Fix**: Use `decimal` type always for financial calculations.

### ❌ Don't: Put Business Logic in Controllers

```csharp
[HttpPost]
public IActionResult Calculate(decimal amount)
{
    var result = amount * 0.15m;  // WRONG - business logic in controller
    return Ok(result);
}
```

**Fix**: Move to service in `Core/Services/`.

### ❌ Don't: Ignore COBOL Metadata

```csharp
public string PolicyNumber { get; set; }  // Missing CobolField attribute
```

**Fix**: Add `[CobolField]` to preserve COBOL PIC information for validation.

### ❌ Don't: Load All Records into Memory

```csharp
var premiums = await _context.Premiums.ToListAsync();  // WRONG - 10K+ records = OutOfMemoryException
```

**Fix**: Use `IAsyncEnumerable<T>` for cursor-based streaming.

## SpecKit Workflow Commands

This project uses SpecKit methodology for feature development:

```bash
# Generate feature specification
/speckit.specify

# Generate implementation plan
/speckit.plan

# Generate task breakdown
/speckit.tasks

# Implement specific task
/speckit.implement T001
```

**All specifications are in**: `specs/001-vamos-migrar-sistema/`

## References

### Documentation

- **Feature Specification**: `specs/001-vamos-migrar-sistema/spec.md`
- **Implementation Plan**: `specs/001-vamos-migrar-sistema/plan.md`
- **Technical Research**: `specs/001-vamos-migrar-sistema/research.md` (type mappings, formatters, etc.)
- **Data Model**: `specs/001-vamos-migrar-sistema/data-model.md` (15 entity definitions)
- **API Contracts**: `specs/001-vamos-migrar-sistema/contracts/openapi.yaml`
- **Quickstart Guide**: `specs/001-vamos-migrar-sistema/quickstart.md`
- **Tasks**: `specs/001-vamos-migrar-sistema/tasks.md` (240 implementation tasks)
- **COBOL Analysis**: `docs/parser/FINAL-ANALYSIS-REPORT.md`

### External Dependencies

**Backend**:
- ASP.NET Core Web API 9.0
- Entity Framework Core 9.0
- Serilog (structured logging)
- Swashbuckle (Swagger/OpenAPI)
- xUnit, FluentAssertions, Moq (testing)

**Frontend**:
- React 18+
- React Router 6+
- Axios (HTTP client)
- Recharts (data visualization)
- TailwindCSS (styling with Caixa branding)
- Vite (build tool)
- Vitest (unit tests)
- Playwright (E2E tests)

## Git Workflow

```bash
# Feature branch naming
git checkout -b feature/premium-calculation

# Commit message format (Conventional Commits)
git commit -m "feat: add premium calculation service"
git commit -m "fix: correct decimal precision in cossurance"
git commit -m "test: add comparison tests for PREMIT output"
```

**Before Committing**:
1. Run tests: `dotnet test && npm run test`
2. Run comparison tests: `dotnet test --filter Category=Comparison`
3. Format code: `dotnet format && npm run format`
4. Verify coverage: 90%+ for new business logic

---

**Last Updated**: October 22, 2025
**Project Status**: Planning complete, ready for implementation (Phase 1: Setup)
