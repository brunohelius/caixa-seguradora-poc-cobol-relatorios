# Implementation Plan: Complete COBOL RG1866B Functional Migration

**Branch**: `003-complete-cobol-migration` | **Date**: October 27, 2025 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/003-complete-cobol-migration/spec.md`

## Summary

Implement complete end-to-end functional migration of COBOL RG1866B batch program to .NET 9 + React, focusing exclusively on actual business functionality (SUSEP Circular 360 premium reporting). Remove non-functional dashboard components from previous iterations and deliver production-ready system capable of replacing mainframe batch job with byte-for-byte output compatibility.

**Primary Requirement**: Generate regulatory PREMIT.TXT and PREMCED.TXT files matching legacy COBOL output exactly for SUSEP compliance.

**Technical Approach**: Clean Architecture with .NET 9 backend (Entity Framework Core for data access, decimal arithmetic for financial calculations, cursor-based processing for large datasets), minimal React frontend for report triggering, SQLite development database mirroring DB2 structure, comprehensive comparison testing against COBOL reference outputs.

## Technical Context

**Language/Version**: .NET 9 (C# 13) backend, React 18+ frontend, Node.js 20+ for build tooling
**Primary Dependencies**:
- Backend: ASP.NET Core 9.0, Entity Framework Core 9.0, Serilog (logging), xUnit (testing)
- Frontend: React 18+, Axios (HTTP), Vite (build)
**Storage**: SQLite 3.x (development/testing), mirrors 26+ DB2 views/tables from mainframe
**Testing**: xUnit for backend unit/integration tests, Vitest for frontend, custom comparison framework for byte-level COBOL output validation
**Target Platform**: Docker containers (Linux), deployable to cloud or on-premises
**Project Type**: Web application (backend API + frontend UI)
**Performance Goals**: Process 10,000+ premium records in under 5 minutes, memory usage under 2GB, support 5 concurrent executions
**Constraints**:
- Byte-for-byte output compatibility with COBOL (regulatory requirement)
- Decimal precision matching COBOL COMP-3 (no floating point)
- Must handle 15,000 record peak loads without memory overflow
- All user-facing text in Brazilian Portuguese
**Scale/Scope**:
- 45 functional requirements (FR-001 to FR-045)
- 7 user stories (4 priority P1, 2 priority P2, 1 priority P3)
- 687 COBOL data items to migrate
- 63 COBOL sections with 65 paragraphs
- 26+ database tables/views to integrate

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

**Status**: No constitution file defined for this project - proceeding with industry standard practices for .NET/React web applications.

**Standard Practices Applied**:
- Clean Architecture with clear separation of concerns (API → Core → Infrastructure)
- Test-Driven Development for business logic (90%+ coverage target per spec SC-017)
- Dependency injection for loose coupling
- Repository pattern for data access abstraction
- Domain-driven design for business entities
- RESTful API design for web interface
- Environment-based configuration (no secrets in code)

**Rationale**: Project constitution will be established as part of this migration to codify learnings and set standards for future COBOL migrations in REGISTROS GERAIS system.

## Project Structure

### Documentation (this feature)

```text
specs/003-complete-cobol-migration/
├── plan.md              # This file
├── research.md          # Phase 0: Technical decisions and COBOL migration patterns
├── data-model.md        # Phase 1: Entity model mapping COBOL to C#
├── quickstart.md        # Phase 1: Developer onboarding guide
├── contracts/           # Phase 1: API contracts (OpenAPI)
│   └── openapi.yaml     # REST API specification
└── tasks.md             # Phase 2: NOT created by /speckit.plan
```

### Source Code (repository root)

```text
backend/
├── src/
│   ├── CaixaSeguradora.Api/          # ASP.NET Core Web API
│   │   ├── Controllers/              # REST endpoints
│   │   ├── Middleware/               # Error handling, logging
│   │   ├── Program.cs                # App entry point, DI config
│   │   └── appsettings.json          # Configuration
│   ├── CaixaSeguradora.Core/         # Domain layer (zero dependencies)
│   │   ├── Entities/                 # Domain models (Premium, Policy, etc.)
│   │   ├── Interfaces/               # Repository/service contracts
│   │   ├── Services/                 # Business logic (calculations)
│   │   ├── DTOs/                     # Data transfer objects
│   │   ├── Attributes/               # CobolFieldAttribute for metadata
│   │   └── Enums/                    # Movement types, status codes
│   └── CaixaSeguradora.Infrastructure/ # External concerns
│       ├── Data/                     # EF Core DbContext, configurations
│       ├── Repositories/             # Data access implementations
│       ├── Services/                 # External service integrations
│       ├── Formatters/               # Fixed-width file formatters
│       └── Migrations/               # EF Core database migrations
└── tests/
    ├── CaixaSeguradora.UnitTests/    # Business logic tests
    ├── CaixaSeguradora.IntegrationTests/ # API + database tests
    └── CaixaSeguradora.ComparisonTests/  # COBOL parity validation

frontend/
├── src/
│   ├── components/                   # React components
│   │   ├── common/                   # Reusable UI (Button, Card, etc.)
│   │   └── reports/                  # Report generation components
│   ├── pages/                        # Route components
│   │   └── ReportGenerationPage.tsx # Main report interface
│   ├── services/                     # API client (Axios)
│   │   └── reportService.ts          # Report generation API calls
│   ├── App.tsx                       # Router configuration
│   └── main.tsx                      # React entry point
└── tests/
    └── unit/                         # Vitest component tests

docs/
├── LEGACY_SYSTEM_DOCUMENTATION.md   # COBOL analysis (existing)
└── migration/                        # Migration artifacts
    ├── cobol-to-csharp-mapping.md   # Type conversions
    ├── business-rules-catalog.md    # All 35+ rule changes
    └── comparison-test-plan.md      # Validation strategy
```

**Structure Decision**: Web application structure (Option 2) selected because feature requires both backend API (report generation, database access, file generation) and minimal frontend UI (report triggering, file download). Backend follows Clean Architecture with three-layer separation: API (HTTP concerns), Core (business logic, zero dependencies), Infrastructure (database, external services). Frontend is deliberately minimal - single page for report generation, avoiding unnecessary dashboard complexity.

**Key Design Decisions**:
1. **Clean Architecture**: Enforces dependency rule (dependencies flow inward), keeps business logic free of framework concerns, enables independent testing
2. **Repository Pattern**: Abstracts database access, allows SQLite (dev) → SQL Server/PostgreSQL (prod) migration without business logic changes
3. **Decimal Type**: C# decimal maps to COBOL COMP-3 with exact precision, critical for regulatory compliance
4. **Cursor Abstraction**: Use IAsyncEnumerable<T> to replicate COBOL cursor behavior, prevents memory overflow with large datasets
5. **Fixed-Width Formatter**: Custom formatter in Infrastructure layer handles COBOL-style padding (zeros for numerics, spaces for strings)

## Complexity Tracking

> No constitution violations - proceeding with standard .NET/React patterns.

*This section intentionally left empty as no complexity violations detected.*

## Phase 0: Research & Technical Decisions

**Objective**: Resolve all technical uncertainties and establish migration patterns before implementation.

### Research Tasks

#### R1: COBOL to C# Type Mapping
**Question**: How to exactly replicate COBOL PIC clauses and COMP-3 arithmetic in C#?

**Approach**:
- Map COBOL PIC X(n) → C# string with [MaxLength(n)]
- Map COBOL PIC 9(p)V99 COMP-3 → C# decimal(p+2, 2)
- Document rounding behavior differences (COBOL vs C# Math.Round)
- Validate precision with test cases for each mapped type

**Deliverable**: Type mapping table in research.md with examples for each COBOL PIC format

#### R2: Cursor-Based Processing Pattern
**Question**: Best way to handle COBOL cursor semantics (FETCH loop) in Entity Framework Core?

**Approach**:
- Evaluate EF Core AsAsyncEnumerable() for streaming query results
- Compare with Dapper for read-only queries (potential performance benefit)
- Test memory usage with 15,000 record dataset
- Benchmark fetch strategies (batch size 100 vs 1000 vs 5000)

**Deliverable**: Recommended pattern with code examples and performance benchmarks

#### R3: Fixed-Width File Generation
**Question**: How to generate exact byte-level output matching COBOL WRITE statements?

**Approach**:
- Research .NET libraries (none exist, must build custom)
- Design FixedWidthFormatter with attributes specifying width, alignment, padding
- Handle special cases: negative numbers, implied decimal points, date formats
- Compare output with COBOL reference using binary diff tools

**Deliverable**: FixedWidthFormatter design with padding rules and examples

#### R4: External Module Integration (RE0001S, GE0009S, GE0010S)
**Question**: How to replicate COBOL CALL functionality for external modules?

**Approach**:
- Reverse-engineer module interfaces from COBOL CALL statements
- Option 1: Implement equivalent C# methods inline
- Option 2: Create service interfaces for future external implementation
- Document known inputs/outputs for each module

**Deliverable**: Service interface designs and mock implementations

#### R5: Transaction Management
**Question**: How to replicate COBOL COMMIT/ROLLBACK behavior?

**Approach**:
- Map COBOL transaction boundaries to EF Core TransactionScope
- Identify explicit COMMIT points in COBOL (after main cursor loop, after PREMCED write)
- Design rollback strategy for mid-processing failures
- Test recovery scenarios (database disconnect, file write failure)

**Deliverable**: Transaction management pattern with error recovery flows

#### R6: SUSEP File Format Validation
**Question**: How to validate generated files meet SUSEP Circular 360 requirements?

**Approach**:
- Research SUSEP validation tools (if publicly available)
- Create byte-level comparison framework against COBOL outputs
- Design field-by-field diff reporting for troubleshooting
- Document all format requirements (record length, field positions, data types)

**Deliverable**: Validation framework design and SUSEP format specification

#### R7: SQLite to DB2 Compatibility
**Question**: What DB2 features must be emulated in SQLite for development?

**Approach**:
- Catalog DB2-specific SQL (WITH UR isolation, DATE functions, DECIMAL types)
- Design abstraction layer for dialect differences
- Document SQLite limitations (no stored procedures, limited concurrency)
- Plan for production database migration (SQL Server or PostgreSQL)

**Deliverable**: Database abstraction strategy and compatibility matrix

#### R8: Portuguese Localization Best Practices
**Question**: How to manage Brazilian Portuguese text throughout application?

**Approach**:
- Evaluate resource files (.resx) vs JSON localization
- Design error message structure (code + Portuguese message)
- Plan for future multi-language support (though out of scope now)
- Create style guide for Portuguese technical writing

**Deliverable**: Localization pattern with example error messages

**Output**: research.md consolidating all findings

## Phase 1: Design & Contracts

**Prerequisites**: research.md complete with all technical decisions resolved

### Design Artifacts

#### D1: Data Model (data-model.md)

**Objective**: Define all entities mapping COBOL data structures to C# classes.

**Content Structure**:
1. **Entity Catalog** (15+ entities):
   - PremiumRecord (V0PREMIOS)
   - Policy (V0APOLICE)
   - Endorsement (V0ENDOSSO)
   - Product (V0PRODUTO)
   - Client (V0CLIENTE)
   - Address (V0ENDERECOS)
   - Coverage (V0COBERAPOL)
   - CossuredPolicy (V0APOLCOSCED)
   - CossuranceCalculation (GE399)
   - [+10 more entities]

2. **For Each Entity**:
   - Entity name and COBOL source table/view
   - Properties with [CobolField] attribute preserving PIC metadata
   - Data type mappings (COBOL → C#)
   - Relationships (foreign keys, navigation properties)
   - Validation rules from functional requirements
   - Example:
     ```csharp
     public class PremiumRecord
     {
         [CobolField(PicClause = "9(13)", Length = 13)]
         public long PolicyNumber { get; set; }

         [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2)]
         [Column(TypeName = "decimal(15,2)")]
         public decimal TotalPremiumAmount { get; set; }

         // ... +80 fields
     }
     ```

3. **Relationships Diagram**: Visual representation of entity relationships mirroring DB2 foreign keys

4. **State Machines** (if applicable):
   - Report execution states (Pending → Running → Completed/Failed)
   - Premium processing states (New → Validated → Calculated → Written)

**Validation**: Cross-reference with COBOL WORKING-STORAGE SECTION (687 data items) to ensure all fields mapped.

#### D2: API Contracts (contracts/openapi.yaml)

**Objective**: Define RESTful API contract for web interface.

**Endpoints** (derived from user stories):

```yaml
openapi: 3.0.0
info:
  title: COBOL RG1866B Migration API
  version: 1.0.0
paths:
  /api/v1/reports/generate:
    post:
      summary: Generate SUSEP premium reports
      requestBody:
        content:
          application/json:
            schema:
              type: object
              properties:
                month:
                  type: string
                  pattern: '^\d{6}$'
                  example: '202510'
                reportType:
                  type: string
                  enum: [PREMIT, PREMCED, BOTH]
                executionMode:
                  type: string
                  enum: [BATCH, INTERACTIVE]
      responses:
        '202':
          description: Report generation started
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/ReportExecution'
        '400':
          description: Invalid parameters
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/ErrorResponse'

  /api/v1/reports/executions/{executionId}:
    get:
      summary: Get execution status
      parameters:
        - name: executionId
          in: path
          required: true
          schema:
            type: string
            format: uuid
      responses:
        '200':
          description: Execution details
          content:
            application/json:
              schema:
                $ref: '#/components/schemas/ReportExecution'

  /api/v1/reports/executions/{executionId}/download/{fileType}:
    get:
      summary: Download generated file
      parameters:
        - name: executionId
          in: path
          required: true
          schema:
            type: string
            format: uuid
        - name: fileType
          in: path
          required: true
          schema:
            type: string
            enum: [PREMIT, PREMCED]
      responses:
        '200':
          description: File download
          content:
            text/plain:
              schema:
                type: string
                format: binary

  /api/v1/reports/executions:
    get:
      summary: List execution history
      parameters:
        - name: page
          in: query
          schema:
            type: integer
            default: 1
        - name: pageSize
          in: query
          schema:
            type: integer
            default: 20
      responses:
        '200':
          description: Paginated execution list
          content:
            application/json:
              schema:
                type: object
                properties:
                  items:
                    type: array
                    items:
                      $ref: '#/components/schemas/ReportExecutionSummary'
                  totalCount:
                    type: integer
                  page:
                    type: integer
                  pageSize:
                    type: integer

components:
  schemas:
    ReportExecution:
      type: object
      properties:
        executionId:
          type: string
          format: uuid
        month:
          type: string
          pattern: '^\d{6}$'
        status:
          type: string
          enum: [PENDING, RUNNING, COMPLETED, FAILED]
        startTime:
          type: string
          format: date-time
        endTime:
          type: string
          format: date-time
        recordsProcessed:
          type: integer
        warnings:
          type: integer
        errors:
          type: integer
        returnCode:
          type: string
          enum: ['0000', '0004', '0008', '0012']
        message:
          type: string

    ReportExecutionSummary:
      type: object
      properties:
        executionId:
          type: string
          format: uuid
        month:
          type: string
        status:
          type: string
        startTime:
          type: string
          format: date-time
        recordsProcessed:
          type: integer

    ErrorResponse:
      type: object
      properties:
        errorCode:
          type: string
        message:
          type: string
        details:
          type: object
```

**Additional Endpoints** (future expansion):
- `/api/v1/health` - Health check
- `/api/v1/mock-data/*` - Mock data management (development only)

#### D3: Quickstart Guide (quickstart.md)

**Objective**: Enable new developers to run the system in under 30 minutes.

**Content Structure**:
1. **Prerequisites**:
   - .NET 9 SDK installation
   - Node.js 20+ installation
   - Docker (optional, for containerized deployment)
   - Git
   - IDE recommendations (VS Code + extensions, Rider)

2. **Initial Setup**:
   ```bash
   # Clone repository
   git clone <repo-url>
   cd "POC Cobol"
   git checkout 003-complete-cobol-migration

   # Backend setup
   cd backend
   dotnet restore
   dotnet ef database update --project src/CaixaSeguradora.Infrastructure

   # Frontend setup
   cd ../frontend
   npm install

   # Load sample data
   cd ../backend/src/CaixaSeguradora.Api
   dotnet run -- load-sample-data
   ```

3. **Running Locally**:
   ```bash
   # Terminal 1: Backend
   cd backend/src/CaixaSeguradora.Api
   dotnet run
   # Listens on https://localhost:5001

   # Terminal 2: Frontend
   cd frontend
   npm run dev
   # Listens on http://localhost:5173
   ```

4. **Running with Docker**:
   ```bash
   docker-compose up --build
   # Backend: http://localhost:5001
   # Frontend: http://localhost:5173
   ```

5. **Running Tests**:
   ```bash
   # Backend unit tests
   cd backend
   dotnet test

   # Backend comparison tests (requires COBOL reference outputs)
   dotnet test --filter Category=Comparison

   # Frontend tests
   cd frontend
   npm run test
   ```

6. **Generating First Report**:
   ```bash
   # Using API directly
   curl -X POST https://localhost:5001/api/v1/reports/generate \
     -H "Content-Type: application/json" \
     -d '{"month": "202510", "reportType": "BOTH"}'

   # Or use web interface
   # Navigate to http://localhost:5173
   ```

7. **Troubleshooting**:
   - Database connection issues
   - Port conflicts
   - Missing dependencies
   - Common errors and solutions

8. **Next Steps**:
   - Review data-model.md for entity structure
   - Read LEGACY_SYSTEM_DOCUMENTATION.md for COBOL context
   - See tasks.md for implementation roadmap (after /speckit.tasks)

#### D4: Agent Context Update

**Action**: Run `.specify/scripts/bash/update-agent-context.sh claude` to update CLAUDE.md with technologies from this plan.

**Technologies to Add**:
- .NET 9 (ASP.NET Core, Entity Framework Core)
- React 18+
- SQLite 3.x (development database)
- xUnit (testing framework)
- Vite (frontend build tool)
- Serilog (logging)

**Manual Additions to Preserve**:
- Existing COBOL context and constraints
- Caixa Seguradora branding guidelines
- Project structure conventions
- Code style preferences

## Phase 2: Task Generation

**Status**: NOT EXECUTED BY /speckit.plan

This phase is handled by the `/speckit.tasks` command, which will:
1. Read this plan.md
2. Read spec.md
3. Read data-model.md, research.md, contracts/
4. Generate tasks.md with sequenced implementation tasks

**Expected Output**:
- tasks.md with 100-200+ tasks organized by:
  - Priority (P1/P2/P3 from user stories)
  - Dependencies (blocking relationships)
  - Effort estimates (T-shirt sizing)
  - Acceptance criteria per task

## Implementation Sequence (High-Level)

After `/speckit.tasks` generates detailed task breakdown, implementation will follow this sequence:

### Iteration 1: Foundation (P1 - Weeks 1-2)
- Project scaffolding (backend + frontend)
- Database schema (SQLite mirroring DB2)
- Core entities (Premium, Policy, Product, Client)
- Repository pattern setup
- Basic API endpoints

### Iteration 2: Premium Calculation Engine (P1 - Weeks 3-4)
- Premium calculation services (sections R0700-R1300)
- Endorsement processing (movement types 101-106)
- Commission calculations
- Business validation rules
- Unit tests for all calculations

### Iteration 3: Cossurance Processing (P1 - Weeks 5-6)
- Cossurance calculation services (sections R3000-R5500)
- GE399 table integration
- Percentage allocation logic
- PREMCED record generation
- Comparison tests

### Iteration 4: File Generation (P1 - Week 7)
- FixedWidthFormatter implementation
- PREMIT.TXT generation (1200 bytes)
- PREMCED.TXT generation (800 bytes)
- Byte-level validation framework
- Comparison with COBOL reference outputs

### Iteration 5: Cursor Processing (P2 - Week 8)
- IAsyncEnumerable implementation for V0PREMIOS
- Nested cursor pattern for secondary queries
- Memory profiling and optimization
- Batch size tuning (1000 records)
- Performance tests with 15,000 records

### Iteration 6: External Services (P2 - Week 9)
- RE0001S reinsurance service mock/implementation
- GE0009S formatting service (CPF/CNPJ, dates)
- GE0010S validation service
- Service integration tests
- Error handling and retry logic

### Iteration 7: Web Interface (P3 - Week 10)
- React report generation form
- Status polling and progress display
- File download functionality
- Execution history page
- Portuguese localization

### Iteration 8: Validation & Hardening (Weeks 11-12)
- Golden dataset comparison tests (100+ scenarios)
- Business user acceptance testing
- Performance optimization
- Error handling improvements
- Documentation completion

## Risk Mitigation

| Risk | Probability | Impact | Mitigation Strategy |
|------|-------------|--------|---------------------|
| Calculation deviations from COBOL | Medium | Critical | Implement comparison framework early (Iteration 1), run continuously, involve business SMEs for validation |
| External module functionality unknown | Medium | High | Create service interfaces with mocks (Iteration 2), defer real implementation, validate assumptions with business users |
| SUSEP format changes during migration | Low | Critical | Monitor SUSEP communications, design format abstraction allowing quick changes, maintain legacy parallel operation |
| Performance below 5-minute SLA | Medium | High | Profile early (Iteration 5), use benchmarking tools, implement cursor-based streaming, optimize query plans |
| Data quality issues in test data | High | Medium | Implement data validation layer (Iteration 3), document quality issues, coordinate with data team for cleanup |
| Scope creep (dashboard features) | Medium | Medium | Reference spec.md "Out of Scope" section, maintain focus on P1/P2 user stories, defer P3 if schedule pressure |

## Success Criteria Mapping

Each success criterion from spec.md maps to specific deliverables:

**Functional Accuracy**:
- SC-001-005: Achieved through Iterations 2-4 (calculations, file generation) + comparison testing

**Performance**:
- SC-006-010: Validated in Iteration 5 (cursor processing) + performance profiling

**Data Integrity**:
- SC-011-015: Enforced through repository pattern (Iteration 1) + transaction management

**Business Rule Coverage**:
- SC-016-020: Implemented in Iteration 2 (business logic) + documented in research.md

**Error Handling**:
- SC-021-025: Implemented across all iterations + consolidated in Iteration 8

**Operational Readiness**:
- SC-026-030: Delivered in Iteration 7 (web interface) + documentation

**Migration Validation**:
- SC-031-035: Executed in Iteration 8 (parallel run) + business sign-off

## Artifacts Summary

Upon completion of `/speckit.plan`, the following artifacts will exist:

**Generated**:
- ✅ plan.md (this file)
- ⏳ research.md (Phase 0 - next step)
- ⏳ data-model.md (Phase 1)
- ⏳ contracts/openapi.yaml (Phase 1)
- ⏳ quickstart.md (Phase 1)
- ⏳ CLAUDE.md updated (Phase 1)

**To Be Generated by /speckit.tasks**:
- ⏳ tasks.md (Phase 2 - separate command)

**Status**: Plan complete. Ready for Phase 0 research.

---

**Next Command**: Continue in this workflow to execute Phase 0 research.
