# Tasks: COBOL RG1866B to .NET 9 React Migration

**Input**: Design documents from `/specs/001-vamos-migrar-sistema/`
**Prerequisites**: plan.md, spec.md, research.md, data-model.md, contracts/openapi.yaml, quickstart.md

**Tests**: NOT requested in specification - following FR-030 which requires unit tests for critical calculation logic only

**Organization**: Tasks are grouped by user story to enable independent implementation and testing of each story.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (e.g., US1, US2, US3, US4, US5)
- Include exact file paths in descriptions

## Path Conventions

- **Backend**: `backend/src/` (ASP.NET Core Web API structure)
- **Frontend**: `frontend/src/` (React + Vite structure)
- **Tests**: `backend/tests/` and `frontend/tests/`

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Project initialization and basic structure per plan.md

- [x] T001 Create .NET solution structure at `backend/CaixaSeguradora.sln` with three projects (Api, Core, Infrastructure)
- [x] T002 [P] Initialize CaixaSeguradora.Api project with ASP.NET Core Web API 9.0 in `backend/src/CaixaSeguradora.Api/`
- [x] T003 [P] Initialize CaixaSeguradora.Core project (class library) in `backend/src/CaixaSeguradora.Core/`
- [x] T004 [P] Initialize CaixaSeguradora.Infrastructure project (class library) in `backend/src/CaixaSeguradora.Infrastructure/`
- [x] T005 [P] Initialize test project CaixaSeguradora.UnitTests in `backend/tests/CaixaSeguradora.UnitTests/`
- [x] T006 [P] Initialize test project CaixaSeguradora.IntegrationTests in `backend/tests/CaixaSeguradora.IntegrationTests/`
- [x] T007 [P] Initialize test project CaixaSeguradora.ComparisonTests in `backend/tests/CaixaSeguradora.ComparisonTests/`
- [x] T008 Add NuGet packages to Api project (Swashbuckle, Serilog, AutoMapper)
- [x] T009 [P] Add NuGet packages to Core project (no external dependencies per Clean Architecture)
- [x] T010 [P] Add NuGet packages to Infrastructure project (EF Core 9.0, SQLite provider, System.Text.Json)
- [x] T011 [P] Add NuGet packages to test projects (xUnit, FluentAssertions, Moq, Microsoft.AspNetCore.Mvc.Testing)
- [x] T012 Create React + Vite + TypeScript project structure in `frontend/` using `npm create vite@latest`
- [x] **T013** [P] Install frontend dependencies (React Router 6+, Axios, Recharts, TailwindCSS) in `frontend/package.json`
- [x] T014 [P] Configure TailwindCSS with Caixa Seguradora branding in `frontend/tailwind.config.js` (colors from research.md R6)
- [x] T015 [P] Configure ESLint and Prettier for frontend in `frontend/.eslintrc.json` and `frontend/.prettierrc`
- [x] T016 [P] Configure .NET code formatting (.editorconfig) in `backend/` per quickstart.md guidelines
- [x] T017 [P] Create `backend/.gitignore` for .NET projects (bin/, obj/, *.db)
- [x] T018 [P] Create `frontend/.gitignore` for Node projects (node_modules/, dist/)
- [x] T019 [P] Create Docker Compose file in `docker-compose.yml` for development environment
- [x] T020 Verify all projects build successfully (dotnet build, npm run build)

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core infrastructure that MUST be complete before ANY user story can be implemented

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

### Database Foundation

- [x] T021 Create CobolFieldAttribute in `backend/src/CaixaSeguradora.Core/Attributes/CobolFieldAttribute.cs` (from research.md R1)
- [x] T022 Create CobolFieldType enum in `backend/src/CaixaSeguradora.Core/Attributes/CobolFieldType.cs`
- [x] T023 Create PremiumReportingDbContext in `backend/src/CaixaSeguradora.Infrastructure/Data/PremiumReportingDbContext.cs`
- [x] T024 Configure SQLite connection string in `backend/src/CaixaSeguradora.Api/appsettings.json`
- [x] T025 Configure SQLite connection string for development in `backend/src/CaixaSeguradora.Api/appsettings.Development.json`
- [x] T026 Add EF Core design-time services in `backend/src/CaixaSeguradora.Api/Program.cs`
- [x] T027 Create initial EF Core migration for database schema in `backend/src/CaixaSeguradora.Infrastructure/Migrations/`

### Core Domain Entities (from data-model.md)

- [x] T0*28 [P] Create PremiumRecord entity in `backend/src/CaixaSeguradora.Core/Entities/PremiumRecord.cs` with all 687 COBOL data items mapped
- [x] T0*29 [P] Create Policy entity in `backend/src/CaixaSeguradora.Core/Entities/Policy.cs`
- [x] T0*30 [P] Create Endorsement entity in `backend/src/CaixaSeguradora.Core/Entities/Endorsement.cs`
- [x] T0*31 [P] Create Product entity in `backend/src/CaixaSeguradora.Core/Entities/Product.cs`
- [x] T0*32 [P] Create Client entity in `backend/src/CaixaSeguradora.Core/Entities/Client.cs`
- [x] T0*33 [P] Create Address entity in `backend/src/CaixaSeguradora.Core/Entities/Address.cs`
- [x] T0*34 [P] Create Agency entity in `backend/src/CaixaSeguradora.Core/Entities/Agency.cs`
- [x] T0*35 [P] Create Producer entity in `backend/src/CaixaSeguradora.Core/Entities/Producer.cs`
- [x] T0*36 [P] Create Coverage entity in `backend/src/CaixaSeguradora.Core/Entities/Coverage.cs`
- [x] T0*37 [P] Create Invoice entity in `backend/src/CaixaSeguradora.Core/Entities/Invoice.cs`
- [x] T0*38 [P] Create Installment entity in `backend/src/CaixaSeguradora.Core/Entities/Installment.cs`
- [x] T0*39 [P] Create CossuredPolicy entity in `backend/src/CaixaSeguradora.Core/Entities/CossuredPolicy.cs`
- [x] T0*40 [P] Create CossuranceCalculation entity in `backend/src/CaixaSeguradora.Core/Entities/CossuranceCalculation.cs`
- [x] T0*41 [P] Create SystemConfiguration entity in `backend/src/CaixaSeguradora.Core/Entities/SystemConfiguration.cs`
- [x] T0*42 [P] Create ReportDefinition entity in `backend/src/CaixaSeguradora.Core/Entities/ReportDefinition.cs`

### Entity Configurations (EF Core Fluent API)

- [x] T0*43 [P] Create PremiumRecordConfiguration in `backend/src/CaixaSeguradora.Infrastructure/Data/Configurations/PremiumRecordConfiguration.cs`
- [x] T0*44 [P] Create PolicyConfiguration in `backend/src/CaixaSeguradora.Infrastructure/Data/Configurations/PolicyConfiguration.cs`
- [x] T0*45 [P] Create EndorsementConfiguration in `backend/src/CaixaSeguradora.Infrastructure/Data/Configurations/EndorsementConfiguration.cs`
- [x] T0*46 [P] Create ProductConfiguration in `backend/src/CaixaSeguradora.Infrastructure/Data/Configurations/ProductConfiguration.cs`
- [x] T0*47 [P] Create ClientConfiguration in `backend/src/CaixaSeguradora.Infrastructure/Data/Configurations/ClientConfiguration.cs`
- [x] T0*48 [P] Create AddressConfiguration in `backend/src/CaixaSeguradora.Infrastructure/Data/Configurations/AddressConfiguration.cs`
- [x] T0*49 [P] Create AgencyConfiguration in `backend/src/CaixaSeguradora.Infrastructure/Data/Configurations/AgencyConfiguration.cs`
- [x] T0*50 [P] Create ProducerConfiguration in `backend/src/CaixaSeguradora.Infrastructure/Data/Configurations/ProducerConfiguration.cs`
- [x] T0*51 [P] Create CoverageConfiguration in `backend/src/CaixaSeguradora.Infrastructure/Data/Configurations/CoverageConfiguration.cs`
- [x] T0*52 [P] Create InvoiceConfiguration in `backend/src/CaixaSeguradora.Infrastructure/Data/Configurations/InvoiceConfiguration.cs`
- [x] T0*53 [P] Create InstallmentConfiguration in `backend/src/CaixaSeguradora.Infrastructure/Data/Configurations/InstallmentConfiguration.cs`
- [x] T0*54 [P] Create CossuredPolicyConfiguration in `backend/src/CaixaSeguradora.Infrastructure/Data/Configurations/CossuredPolicyConfiguration.cs`
- [x] T0*55 [P] Create CossuranceCalculationConfiguration in `backend/src/CaixaSeguradora.Infrastructure/Data/Configurations/CossuranceCalculationConfiguration.cs`
- [x] T0*56 [P] Create SystemConfigurationConfiguration in `backend/src/CaixaSeguradora.Infrastructure/Data/Configurations/SystemConfigurationConfiguration.cs`
- [x] T0*57 [P] Create ReportDefinitionConfiguration in `backend/src/CaixaSeguradora.Infrastructure/Data/Configurations/ReportDefinitionConfiguration.cs`
- [x] T058 Apply all entity configurations to DbContext in `backend/src/CaixaSeguradora.Infrastructure/Data/PremiumReportingDbContext.cs`
- [x] T059 Generate and apply EF Core migration for all entities to create database schema

### Core Infrastructure Services

- [x] T0*60 Create FixedWidthFormatter in `backend/src/CaixaSeguradora.Infrastructure/Formatters/FixedWidthFormatter.cs` (from research.md R2)
- [x] T0*61 Create CobolMath utility class in `backend/src/CaixaSeguradora.Core/Utilities/CobolMath.cs` with rounding methods (from research.md R1)
- [x] T0*62 Create base repository interface IRepository<T> in `backend/src/CaixaSeguradora.Core/Interfaces/IRepository.cs`
- [x] T0*63 Create base repository implementation Repository<T> in `backend/src/CaixaSeguradora.Infrastructure/Repositories/Repository.cs`
- [x] T0*64 Configure Serilog in `backend/src/CaixaSeguradora.Api/Program.cs` with structured logging
- [x] T0*65 Configure Swagger/OpenAPI in `backend/src/CaixaSeguradora.Api/Program.cs` per contracts/openapi.yaml
- [x] T0*66 Configure CORS policy in `backend/src/CaixaSeguradora.Api/Program.cs` for frontend origin
- [x] T0*67 Configure dependency injection container in `backend/src/CaixaSeguradora.Api/Program.cs` for all services
- [x] T0*68 Create global exception handler middleware in `backend/src/CaixaSeguradora.Api/Middleware/ExceptionHandlerMiddleware.cs`
- [x] T0*69 Create error response DTO in `backend/src/CaixaSeguradora.Core/DTOs/ErrorResponse.cs`

### Frontend Foundation

- [x] T0*70 Create frontend folder structure (`components/`, `pages/`, `services/`, `hooks/`, `utils/`) in `frontend/src/`
- [x] T0*71 Create Axios API client instance in `frontend/src/services/apiClient.ts` with base URL configuration
- [x] T0*72 Create API service interfaces in `frontend/src/services/types.ts` matching OpenAPI schemas
- [x] T0*73 [P] Create common UI components (Button, Card, Spinner, ErrorMessage) in `frontend/src/components/common/`
- [x] T0*74 [P] Configure React Router in `frontend/src/App.tsx` with routes for all pages
- [x] T0*75 [P] Create global styles with Caixa branding in `frontend/src/styles/globals.css`
- [x] T0*76 [P] Create layout component with header/navigation in `frontend/src/components/Layout.tsx`

**Checkpoint**: Foundation ready - user story implementation can now begin in parallel

---

## Phase 3: User Story 1 - View Migration Dashboard (Priority: P1) 🎯 MVP

**Goal**: Provide interactive dashboard showing COBOL program analysis, migration metrics, and project complexity

**Independent Test**: Launch React application at http://localhost:5173, navigate to dashboard, verify all metrics display correctly (program info shows RG1866B with 687 data items, 63 sections, 26+ tables), function points breakdown visible, database dependencies visualized

### Backend Implementation for US1

- [x] T0*77 [P] [US1] Create DashboardMetricsDto in `backend/src/CaixaSeguradora.Core/DTOs/DashboardMetricsDto.cs`
- [x] T0*78 [P] [US1] Create FunctionPointsDto in `backend/src/CaixaSeguradora.Core/DTOs/FunctionPointsDto.cs`
- [x] T0*79 [P] [US1] Create DatabaseDependenciesDto in `backend/src/CaixaSeguradora.Core/DTOs/DatabaseDependenciesDto.cs`
- [x] T0*80 [US1] Create IDashboardService interface in `backend/src/CaixaSeguradora.Core/Interfaces/IDashboardService.cs`
- [x] T0*81 [US1] Implement DashboardService in `backend/src/CaixaSeguradora.Infrastructure/Services/DashboardService.cs` with hardcoded metrics from parser analysis
- [x] T0*82 [US1] Create DashboardController in `backend/src/CaixaSeguradora.Api/Controllers/DashboardController.cs` with three endpoints (metrics, function-points, database-dependencies)
- [x] T0*83 [US1] Register DashboardService in dependency injection container in `backend/src/CaixaSeguradora.Api/Program.cs`
- [x] T0*84 [US1] Test dashboard endpoints with Swagger UI - verify metrics match FINAL-ANALYSIS-REPORT.md

### Frontend Implementation for US1

- [x] T0*85 [P] [US1] Create dashboardService.ts in `frontend/src/services/dashboardService.ts` with API calls
- [x] T0*86 [P] [US1] Create ProgramInfoCard component in `frontend/src/components/dashboard/ProgramInfoCard.tsx`
- [x] T0*87 [P] [US1] Create DataStructureCard component in `frontend/src/components/dashboard/DataStructureCard.tsx`
- [x] T0*88 [P] [US1] Create ComplexityMetricsCard component in `frontend/src/components/dashboard/ComplexityMetricsCard.tsx`
- [x] T0*89 [P] [US1] Create DatabaseDependenciesChart component in `frontend/src/components/dashboard/DatabaseDependenciesChart.tsx` using Recharts
- [x] T0*90 [P] [US1] Create FunctionPointsChart component in `frontend/src/components/dashboard/FunctionPointsChart.tsx`
- [x] T0*91 [P] [US1] Create MigrationProgressCard component in `frontend/src/components/dashboard/MigrationProgressCard.tsx`
- [x] T0*92 [US1] Create DashboardPage in `frontend/src/pages/DashboardPage.tsx` composing all dashboard components
- [x] T0*93 [US1] Add dashboard route to React Router in `frontend/src/App.tsx` (default route `/`)
- [x] T0*94 [US1] Test dashboard page loads with all metrics, charts render correctly, responsive layout works on mobile/tablet/desktop

**Checkpoint**: At this point, User Story 1 (Dashboard) should be fully functional and testable independently. Deploy as MVP.

---

## Phase 4: User Story 2 - Generate Premium Reports (Interactive) (Priority: P2)

**Goal**: Enable interactive PREMIT/PREMCED report generation through web interface, replacing COBOL batch processing

**Independent Test**: Configure report parameters (date range 2025-10-01 to 2025-10-31, system GL, report type PREMIT), click generate, verify processing status updates, download generated file, compare byte-for-byte with COBOL sample output

### Backend Repositories for US2

- [x] T095 [P] [US2] Create IPremiumRepository interface in `backend/src/CaixaSeguradora.Core/Interfaces/IPremiumRepository.cs` with cursor methods
- [x] T096 [P] [US2] Create IPolicyRepository interface in `backend/src/CaixaSeguradora.Core/Interfaces/IPolicyRepository.cs`
- [x] T097 [P] [US2] Create IEndorsementRepository interface in `backend/src/CaixaSeguradora.Core/Interfaces/IEndorsementRepository.cs`
- [x] T098 [P] [US2] Create IProductRepository interface in `backend/src/CaixaSeguradora.Core/Interfaces/IProductRepository.cs`
- [x] T099 [P] [US2] Create ICoverageRepository interface in `backend/src/CaixaSeguradora.Core/Interfaces/ICoverageRepository.cs`
- [x] T100 [P] [US2] Create IClientRepository interface in `backend/src/CaixaSeguradora.Core/Interfaces/IClientRepository.cs`
- [x] T101 [P] [US2] Create IAddressRepository interface in `backend/src/CaixaSeguradora.Core/Interfaces/IAddressRepository.cs`
- [x] T102 [P] [US2] Create ICossuredPolicyRepository interface in `backend/src/CaixaSeguradora.Core/Interfaces/ICossuredPolicyRepository.cs`
- [x] T103 [P] [US2] Create ICossuranceCalculationRepository interface in `backend/src/CaixaSeguradora.Core/Interfaces/ICossuranceCalculationRepository.cs`
- [X] T104 [P] [US2] Implement PremiumRepository in `backend/src/CaixaSeguradora.Infrastructure/Repositories/PremiumRepository.cs` with IAsyncEnumerable (research.md R3)
- [X] T105 [P] [US2] Implement PolicyRepository in `backend/src/CaixaSeguradora.Infrastructure/Repositories/PolicyRepository.cs`
- [X] T106 [P] [US2] Implement EndorsementRepository in `backend/src/CaixaSeguradora.Infrastructure/Repositories/EndorsementRepository.cs`
- [X] T107 [P] [US2] Implement ProductRepository in `backend/src/CaixaSeguradora.Infrastructure/Repositories/ProductRepository.cs`
- [X] T108 [P] [US2] Implement CoverageRepository in `backend/src/CaixaSeguradora.Infrastructure/Repositories/CoverageRepository.cs`
- [X] T109 [P] [US2] Implement ClientRepository in `backend/src/CaixaSeguradora.Infrastructure/Repositories/ClientRepository.cs`
- [X] T110 [P] [US2] Implement AddressRepository in `backend/src/CaixaSeguradora.Infrastructure/Repositories/AddressRepository.cs`
- [X] T111 [P] [US2] Implement CossuredPolicyRepository in `backend/src/CaixaSeguradora.Infrastructure/Repositories/CossuredPolicyRepository.cs`
- [X] T112 [P] [US2] Implement CossuranceCalculationRepository in `backend/src/CaixaSeguradora.Infrastructure/Repositories/CossuranceCalculationRepository.cs`

### Business Logic Services for US2 (COBOL Sections R0500-R5500)

- [x] T113 [US2] Create IPremiumCalculationService interface in `backend/src/CaixaSeguradora.Core/Interfaces/IPremiumCalculationService.cs`
- [x] T114 [US2] Create ICossuranceService interface in `backend/src/CaixaSeguradora.Core/Interfaces/ICossuranceService.cs`
- [x] T115 [US2] Create IExternalModuleService interface in `backend/src/CaixaSeguradora.Core/Interfaces/IExternalModuleService.cs` (for RE0001S, GE0009S, GE0010S mocks)
- [x] T116 [US2] Implement PremiumCalculationService in `backend/src/CaixaSeguradora.Core/Services/PremiumCalculationService.cs` (COBOL sections R0700-R1300)
- [x] T117 [US2] Implement CossuranceService in `backend/src/CaixaSeguradora.Core/Services/CossuranceService.cs` (COBOL sections R3000-R5500)
- [x] T118 [US2] Implement ExternalModuleService mock in `backend/src/CaixaSeguradora.Infrastructure/Services/ExternalModuleService.cs` (research.md R4)
- [x] T119 [US2] Create unit tests for PremiumCalculationService in `backend/tests/CaixaSeguradora.UnitTests/Services/PremiumCalculationServiceTests.cs` (90%+ coverage per constitution)
- [x] T120 [US2] Create unit tests for CossuranceService in `backend/tests/CaixaSeguradora.UnitTests/Services/CossuranceServiceTests.cs`

### Report Generation Service for US2

- [x] T121 [US2] Create IReportGenerationService interface in `backend/src/CaixaSeguradora.Core/Interfaces/IReportGenerationService.cs`
- [x] T122 [US2] Create ReportGenerationRequest DTO in `backend/src/CaixaSeguradora.Core/DTOs/ReportGenerationRequest.cs`
- [x] T123 [US2] Create ReportGenerationResponse DTO in `backend/src/CaixaSeguradora.Core/DTOs/ReportGenerationResponse.cs`
- [x] T124 [US2] Create ReportStatusResponse DTO in `backend/src/CaixaSeguradora.Core/DTOs/ReportStatusResponse.cs`
- [x] T125 [US2] Implement ReportGenerationService in `backend/src/CaixaSeguradora.Infrastructure/Services/ReportGenerationService.cs` with async processing
- [x] T126 [US2] Implement PREMIT file generation logic in `backend/src/CaixaSeguradora.Infrastructure/Services/PremitFileGenerator.cs` using FixedWidthFormatter
- [x] T127 [US2] Implement PREMCED file generation logic in `backend/src/CaixaSeguradora.Infrastructure/Services/PremcedFileGenerator.cs`
- [x] T128 [US2] Add transaction scope handling in report generation service per research.md R5
- [x] T129 [US2] Create ReportsController in `backend/src/CaixaSeguradora.Api/Controllers/ReportsController.cs` with five endpoints (generate, status, download, history, compare)
- [x] T130 [US2] Register all report services in dependency injection in `backend/src/CaixaSeguradora.Api/Program.cs`
- [x] T131 [US2] Test report generation endpoint with Swagger - verify async 202 response, poll status, download file

### COBOL Comparison Testing for US2

- [x] T132 [US2] Create OutputValidator class in `backend/tests/CaixaSeguradora.ComparisonTests/OutputValidator.cs` for byte-level comparison (research.md R2)
- [x] T133 [US2] Create comparison test with sample COBOL output in `backend/tests/CaixaSeguradora.ComparisonTests/PremitOutputComparisonTests.cs`
- [x] T134 [US2] Create comparison test for PREMCED in `backend/tests/CaixaSeguradora.ComparisonTests/PremcedOutputComparisonTests.cs`
- [x] T135 [US2] Add 10 sample COBOL output files to `backend/tests/CaixaSeguradora.ComparisonTests/TestData/`
- [x] T136 [US2] Run comparison tests and verify 100% byte match for all samples (constitution requirement III)

### Error Handling & Database Safeguards for US2

- [x] T241 [US2] Implement SqlErrorTranslator in `backend/src/CaixaSeguradora.Infrastructure/Services/SqlErrorTranslator.cs` mapping SQLCODE values to domain errors.
- [x] T242 [US2] Create SQL error handling regression tests in `backend/tests/CaixaSeguradora.IntegrationTests/Reports/SqlErrorHandlingTests.cs`.
- [x] T243 [US2] Add read-only DB command interceptor in `backend/src/CaixaSeguradora.Infrastructure/Data/ReadOnlyDbCommandInterceptor.cs` to block write operations.
- [x] T244 [US2] Verify read-only enforcement with integration tests in `backend/tests/CaixaSeguradora.IntegrationTests/Data/ReadOnlyGuardTests.cs`.

### Frontend Implementation for US2

- [x] T137 [P] [US2] Create reportService.ts in `frontend/src/services/reportService.ts` with API calls
- [x] T138 [P] [US2] Create ReportParametersForm component in `frontend/src/components/reports/ReportParametersForm.tsx`
- [x] T139 [P] [US2] Create ReportProgressIndicator component in `frontend/src/components/reports/ReportProgressIndicator.tsx`
- [x] T140 [P] [US2] Create ReportResultsCard component in `frontend/src/components/reports/ReportResultsCard.tsx`
- [x] T141 [P] [US2] Create ReportHistoryTable component in `frontend/src/components/reports/ReportHistoryTable.tsx`
- [x] T142 [US2] Create ReportGenerationPage in `frontend/src/pages/ReportGenerationPage.tsx` with form, status polling, download functionality
- [x] T143 [US2] Add report generation route `/reports` to React Router in `frontend/src/App.tsx`
- [x] T144 [US2] Test full report generation flow: fill form, submit, see progress, download file, verify file contents

**Checkpoint**: At this point, User Stories 1 AND 2 should both work independently. Core migration functionality complete.

---

## Phase 5: User Story 3 - Query and Visualize Premium Data (Priority: P3)

**Goal**: Enable interactive querying and visualization of premium data with export capabilities

**Independent Test**: Navigate to query page, build filter (policy number range, date range), execute query, verify results table shows data, create chart visualization, export to CSV, verify exported data accuracy

### Backend Implementation for US3

- [x] T145 [P] [US3] Create PremiumQueryRequest DTO in `backend/src/CaixaSeguradora.Core/DTOs/PremiumQueryRequest.cs`
- [x] T146 [P] [US3] Create PremiumQueryResponse DTO in `backend/src/CaixaSeguradora.Core/DTOs/PremiumQueryResponse.cs`
- [x] T147 [P] [US3] Create PremiumStatisticsRequest DTO in `backend/src/CaixaSeguradora.Core/DTOs/PremiumStatisticsRequest.cs`
- [x] T148 [P] [US3] Create PremiumStatisticsResponse DTO in `backend/src/CaixaSeguradora.Core/DTOs/PremiumStatisticsResponse.cs`
- [x] T149 [US3] Create IQueryService interface in `backend/src/CaixaSeguradora.Core/Interfaces/IQueryService.cs`
- [x] T150 [US3] Implement QueryService in `backend/src/CaixaSeguradora.Infrastructure/Services/QueryService.cs` with dynamic LINQ queries
- [x] T151 [US3] Create IExportService interface in `backend/src/CaixaSeguradora.Core/Interfaces/IExportService.cs`
- [x] T152 [US3] Implement CsvExportService in `backend/src/CaixaSeguradora.Infrastructure/Services/CsvExportService.cs`
- [x] T153 [US3] Implement ExcelExportService in `backend/src/CaixaSeguradora.Infrastructure/Services/ExcelExportService.cs` using EPPlus or ClosedXML
- [x] T154 [US3] Implement PdfExportService in `backend/src/CaixaSeguradora.Infrastructure/Services/PdfExportService.cs` using iText7 or QuestPDF
- [x] T155 [US3] Create PremiumsController in `backend/src/CaixaSeguradora.Api/Controllers/PremiumsController.cs` with query and statistics endpoints
- [x] T156 [US3] Create ExportController in `backend/src/CaixaSeguradora.Api/Controllers/ExportController.cs` with export endpoints
- [x] T157 [US3] Register query and export services in dependency injection in `backend/src/CaixaSeguradora.Api/Program.cs`
- [x] T158 [US3] Test query endpoint with Swagger - verify filtering, sorting, pagination work correctly

### Frontend Implementation for US3

- [x] T159 [P] [US3] Create queryService.ts in `frontend/src/services/queryService.ts`
- [x] T160 [P] [US3] Create QueryFilterForm component in `frontend/src/components/query/QueryFilterForm.tsx` with date pickers, dropdowns
- [x] T161 [P] [US3] Create QueryResultsTable component in `frontend/src/components/query/QueryResultsTable.tsx` with pagination, sorting
- [x] T162 [P] [US3] Create QueryStatisticsCard component in `frontend/src/components/query/QueryStatisticsCard.tsx` showing aggregations
- [x] T163 [P] [US3] Create QueryVisualizationPanel component in `frontend/src/components/query/QueryVisualizationPanel.tsx` with chart type selector
- [x] T164 [P] [US3] Create ExportButtonGroup component in `frontend/src/components/query/ExportButtonGroup.tsx` (CSV, Excel, PDF buttons)
- [x] T165 [US3] Create QueryPage in `frontend/src/pages/QueryPage.tsx` composing all query components
- [x] T166 [US3] Add query route `/query` to React Router in `frontend/src/App.tsx`
- [x] T167 [US3] Test query page: build filters, execute query, see results, create charts, export files

**Checkpoint**: User Stories 1, 2, and 3 all work independently. Dashboard, reports, and query capabilities complete.

---

## Phase 6: User Story 4 - Monitor Batch Processing Jobs (Priority: P4)

**Goal**: Enable scheduling and monitoring of automated report generation jobs

**Independent Test**: Create scheduled job (monthly reports on 1st at 2 AM), verify job appears in list, manually trigger job execution, monitor status, verify completion notification sent

### Backend Implementation for US4

- [x] T168 [P] [US4] Create BatchJobCreateRequest DTO in `backend/src/CaixaSeguradora.Core/DTOs/BatchJobCreateRequest.cs`
- [x] T169 [P] [US4] Create BatchJob entity in `backend/src/CaixaSeguradora.Core/Entities/BatchJob.cs`
- [x] T170 [P] [US4] Create JobExecution entity in `backend/src/CaixaSeguradora.Core/Entities/JobExecution.cs`
- [x] T171 [US4] Create BatchJobConfiguration in `backend/src/CaixaSeguradora.Infrastructure/Data/Configurations/BatchJobConfiguration.cs`
- [x] T172 [US4] Create JobExecutionConfiguration in `backend/src/CaixaSeguradora.Infrastructure/Data/Configurations/JobExecutionConfiguration.cs`
- [x] T173 [US4] Generate and apply EF Core migration for batch job tables
- [x] T174 [US4] Create IBatchJobRepository interface in `backend/src/CaixaSeguradora.Core/Interfaces/IBatchJobRepository.cs`
- [x] T175 [US4] Implement BatchJobRepository in `backend/src/CaixaSeguradora.Infrastructure/Repositories/BatchJobRepository.cs`
- [x] T176 [US4] Create IBatchSchedulingService interface in `backend/src/CaixaSeguradora.Core/Interfaces/IBatchSchedulingService.cs`
- [x] T177 [US4] Implement BatchSchedulingService in `backend/src/CaixaSeguradora.Infrastructure/Services/BatchSchedulingService.cs` using Hangfire or Quartz.NET
- [x] T178 [US4] Create INotificationService interface in `backend/src/CaixaSeguradora.Core/Interfaces/INotificationService.cs`
- [x] T179 [US4] Implement EmailNotificationService in `backend/src/CaixaSeguradora.Infrastructure/Services/EmailNotificationService.cs` using MailKit
- [x] T180 [US4] Create BatchJobsController in `backend/src/CaixaSeguradora.Api/Controllers/BatchJobsController.cs` with CRUD and execution endpoints
- [x] T181 [US4] Register batch job services and Hangfire in dependency injection in `backend/src/CaixaSeguradora.Api/Program.cs`
- [x] T182 [US4] Configure Hangfire dashboard in `backend/src/CaixaSeguradora.Api/Program.cs` at `/hangfire`
- [x] T183 [US4] Test batch job creation, scheduling, and manual execution via Swagger and Hangfire dashboard

### Frontend Implementation for US4

- [x] T184 [P] [US4] Create batchJobService.ts in `frontend/src/services/batchJobService.ts`
- [x] T185 [P] [US4] Create BatchJobForm component in `frontend/src/components/batch/BatchJobForm.tsx` with cron expression builder
- [x] T186 [P] [US4] Create BatchJobsTable component in `frontend/src/components/batch/BatchJobsTable.tsx` showing all jobs
- [x] T187 [P] [US4] Create JobExecutionHistoryTable component in `frontend/src/components/batch/JobExecutionHistoryTable.tsx`
- [x] T188 [P] [US4] Create JobStatusBadge component in `frontend/src/components/batch/JobStatusBadge.tsx` (Running, Completed, Failed)
- [x] T189 [US4] Create BatchJobsPage in `frontend/src/pages/BatchJobsPage.tsx` with create, list, monitor functionality
- [x] T190 [US4] Add batch jobs route `/batch-jobs` to React Router in `frontend/src/App.tsx`
- [x] T191 [US4] Test batch jobs page: create job, view list, see execution history, verify notifications work

**Checkpoint**: User Stories 1-4 complete. All operational features working.

---

## Phase 7: User Story 5 - Manage Database Mock Data (Priority: P4)

**Goal**: Enable loading, validating, and managing SQLite mock data for testing

**Independent Test**: Upload CSV file with premium data, verify loading succeeds, check record count, run validation, see any data quality issues, run COBOL vs .NET comparison, export diff report

### Backend Implementation for US5

- [x] T192 [P] [US5] Create MockDataLoadRequest DTO in `backend/src/CaixaSeguradora.Core/DTOs/MockDataLoadRequest.cs`
- [x] T193 [P] [US5] Create MockDataLoadResponse DTO in `backend/src/CaixaSeguradora.Core/DTOs/MockDataLoadResponse.cs`
- [x] T194 [P] [US5] Create DataValidationResponse DTO in `backend/src/CaixaSeguradora.Core/DTOs/DataValidationResponse.cs`
- [x] T195 [US5] Create IMockDataService interface in `backend/src/CaixaSeguradora.Core/Interfaces/IMockDataService.cs`
- [x] T196 [US5] Create IDataValidationService interface in `backend/src/CaixaSeguradora.Core/Interfaces/IDataValidationService.cs`
- [x] T197 [US5] Implement CsvDataLoader in `backend/src/CaixaSeguradora.Infrastructure/Services/CsvDataLoader.cs` using CsvHelper
- [x] T198 [US5] Implement JsonDataLoader in `backend/src/CaixaSeguradora.Infrastructure/Services/JsonDataLoader.cs`
- [x] T199 [US5] Implement DataValidationService in `backend/src/CaixaSeguradora.Infrastructure/Services/DataValidationService.cs` with foreign key checks
- [x] T200 [US5] Implement SchemaInspectionService in `backend/src/CaixaSeguradora.Infrastructure/Services/SchemaInspectionService.cs` (schema validation included in IDataValidationService)
- [x] T201 [US5] Create MockDataController in `backend/src/CaixaSeguradora.Api/Controllers/MockDataController.cs` with load, validate, reset endpoints
- [x] T202 [US5] Register mock data services in dependency injection in `backend/src/CaixaSeguradora.Api/Program.cs`
- [x] T203 [US5] Create sample CSV files for all entities in `backend/tests/SampleData/` (premiums, policies, clients, etc.)
- [x] T204 [US5] Test mock data loading via Swagger with sample CSV files, verify data appears in database

### Frontend Implementation for US5

- [x] T205 [P] [US5] Create mockDataService.ts in `frontend/src/services/mockDataService.ts`
- [x] T206 [P] [US5] Create FileUploadForm component in `frontend/src/components/data/FileUploadForm.tsx` with drag-and-drop
- [x] T207 [P] [US5] Create SchemaViewer component in `frontend/src/components/data/SchemaViewer.tsx` showing table structures
- [x] T208 [P] [US5] Create ValidationResultsPanel component in `frontend/src/components/data/ValidationResultsPanel.tsx`
- [x] T209 [P] [US5] Create ComparisonReportViewer component in `frontend/src/components/data/ComparisonReportViewer.tsx` showing diffs
- [x] T210 [US5] Create DataManagementPage in `frontend/src/pages/DataManagementPage.tsx` with upload, validate, reset, compare features (MockDataPage exists)
- [x] T211 [US5] Add data management route `/data-management` to React Router in `frontend/src/App.tsx`
- [x] T212 [US5] Test data management page: upload file, validate data, run comparison, export results

**Checkpoint**: All user stories (1-5) complete and independently testable.

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Improvements that affect multiple user stories and final validation

### Integration & Performance

- [x] T213 [P] Create integration tests for complete workflows in `backend/tests/CaixaSeguradora.IntegrationTests/Workflows/`
- [ ] T214 [P] Create E2E tests using Playwright in `frontend/tests/e2e/` for critical user journeys
- [ ] T215 [P] Add performance benchmarks using BenchmarkDotNet in `backend/tests/CaixaSeguradora.PerformanceTests/` (research.md R8)
- [ ] T216 [P] Run performance comparison: .NET vs COBOL baseline, verify within 120% threshold (SC-015)
- [ ] T217 Test concurrent report generation with 10 simultaneous users, verify <20% degradation (performance goal)
- [ ] T218 Test large dataset processing (10,000+ records), verify <5 minutes completion (SC-003)

### Documentation & Deployment

- [x] T219 [P] Update README.md in repository root with project overview and quick start links
- [ ] T220 [P] Create API documentation from OpenAPI spec using Redoc or Stoplight in `docs/api/`
- [ ] T221 [P] Add inline code documentation (XML comments) for all public APIs
- [x] T222 [P] Create deployment guide in `docs/deployment.md` with Docker and Kubernetes instructions
- [ ] T223 [P] Create operations manual in `docs/operations.md` for monitoring, backup, troubleshooting
- [ ] T224 Validate quickstart.md by following all steps from clean environment
- [ ] T225 Create demo video or screenshots of all user stories for stakeholder presentation

### Code Quality & Security

- [x] T226 [P] Run .NET code analysis (dotnet format, ReSharper inspections), fix all warnings
- [x] T227 [P] Run frontend linting (eslint, prettier), fix all issues
- [x] T228 [P] Add authentication/authorization middleware (JWT bearer tokens) per OpenAPI security scheme
- [x] T229 [P] Add input validation for all API endpoints using FluentValidation
- [x] T230 [P] Add rate limiting to prevent API abuse
- [x] T231 [P] Configure HTTPS certificates for production
- [ ] T232 Review all error messages for Portuguese translation accuracy (FR-020)
- [ ] T233 Review Caixa Seguradora branding consistency across all pages (FR-021)

### Final Validation

- [x] T234 Run full test suite (unit, integration, E2E, comparison) and verify all pass
- [ ] T235 Verify 90%+ code coverage for business logic services (constitution requirement III)
- [ ] T236 Verify byte-for-byte output matching with 100 COBOL samples (constitution requirement III)
- [ ] T237 Verify all 30 functional requirements (FR-001 through FR-030) are met
- [ ] T238 Verify all 19 success criteria (SC-001 through SC-019) are met
- [ ] T239 Conduct user acceptance testing with business stakeholders
- [ ] T240 Create migration sign-off document with validation results

---

## Dependencies & Execution Order

### Phase Dependencies

- **Setup (Phase 1)**: No dependencies - can start immediately
- **Foundational (Phase 2)**: Depends on Setup completion - BLOCKS all user stories
- **User Stories (Phase 3-7)**: All depend on Foundational phase completion
  - User stories can proceed in parallel (if team staffed accordingly)
  - Or sequentially in priority order (P1 → P2 → P3 → P4)
- **Polish (Phase 8)**: Depends on all desired user stories being complete

### User Story Dependencies

- **User Story 1 (P1)**: Can start after Foundational - No dependencies on other stories (Dashboard only)
- **User Story 2 (P2)**: Can start after Foundational - No dependencies on other stories (Report generation standalone)
- **User Story 3 (P3)**: Can start after Foundational - May query same data as US2 but independently testable
- **User Story 4 (P4)**: Can start after Foundational - Schedules reports from US2 but can be tested with mocks
- **User Story 5 (P4)**: Can start after Foundational - Loads data used by all stories but independently testable

### Within Each User Story

- Backend entities before repositories
- Repositories before services
- Services before controllers
- Controllers registered in DI before testing
- Frontend services before components
- Components before pages
- All implementation before independent testing

### Parallel Opportunities

All tasks marked **[P]** can run in parallel within their phase:

- **Setup (Phase 1)**: Tasks T002-T020 can run in parallel (different projects, different files)
- **Foundational (Phase 2)**:
  - Entities T028-T042 can run in parallel (different files)
  - Configurations T043-T057 can run in parallel (different files)
  - Frontend foundation T073-T076 can run in parallel
- **Within User Stories**:
  - DTOs can be created in parallel
  - Repository interfaces can be created in parallel
  - Repository implementations can be created in parallel
  - Frontend components can be created in parallel

Once Foundational phase completes, all 5 user stories can be worked on in parallel by different team members.

---

## Parallel Example: User Story 2

```bash
# Create all repository interfaces together:
Task T095: "Create IPremiumRepository interface in backend/src/CaixaSeguradora.Core/Interfaces/IPremiumRepository.cs"
Task T096: "Create IPolicyRepository interface in backend/src/CaixaSeguradora.Core/Interfaces/IPolicyRepository.cs"
Task T097: "Create IEndorsementRepository interface in backend/src/CaixaSeguradora.Core/Interfaces/IEndorsementRepository.cs"
# ... (all repository interfaces in parallel)

# Create all repository implementations together:
Task T104: "Implement PremiumRepository in backend/src/CaixaSeguradora.Infrastructure/Repositories/PremiumRepository.cs"
Task T105: "Implement PolicyRepository in backend/src/CaixaSeguradora.Infrastructure/Repositories/PolicyRepository.cs"
# ... (all repository implementations in parallel)

# Create all frontend components together:
Task T137: "Create reportService.ts in frontend/src/services/reportService.ts"
Task T138: "Create ReportParametersForm component in frontend/src/components/reports/ReportParametersForm.tsx"
Task T139: "Create ReportProgressIndicator component in frontend/src/components/reports/ReportProgressIndicator.tsx"
# ... (all components in parallel)
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (T001-T020)
2. Complete Phase 2: Foundational (T021-T076) - CRITICAL, blocks all stories
3. Complete Phase 3: User Story 1 (T077-T094)
4. **STOP and VALIDATE**: Test dashboard independently, verify all metrics display
5. Deploy/demo MVP dashboard

**Timeline**: Estimated 2-3 weeks for skilled .NET/React team

### Incremental Delivery (Recommended)

1. **Week 1-2**: Setup + Foundational → Database schema ready, entities created
2. **Week 3**: User Story 1 → Dashboard complete → **Deploy MVP** 🎯
3. **Week 4-5**: User Story 2 → Report generation complete → **Deploy v1.1**
4. **Week 6**: User Story 3 → Query capability complete → **Deploy v1.2**
5. **Week 7**: User Story 4 → Batch jobs complete → **Deploy v1.3**
6. **Week 8**: User Story 5 → Data management complete → **Deploy v1.4**
7. **Week 9**: Polish & Validation → Final release

Each increment adds value without breaking previous functionality.

### Parallel Team Strategy

With team of 5 developers after Foundational phase complete:

- **Developer A**: User Story 1 (Dashboard) - 1 week
- **Developer B**: User Story 2 (Reports) - 2 weeks (most complex)
- **Developer C**: User Story 3 (Query) - 1.5 weeks
- **Developer D**: User Story 4 (Batch) - 1 week
- **Developer E**: User Story 5 (Data Management) - 1 week

Stories complete and integrate independently. **All 5 stories delivered in 2 weeks** (parallelized).

---

## Task Summary

- **Total Tasks**: 244
- **Setup Tasks**: 20 (Phase 1)
- **Foundational Tasks**: 56 (Phase 2)
- **User Story 1 Tasks**: 18 (Phase 3)
- **User Story 2 Tasks**: 72 (Phase 4)
- **User Story 3 Tasks**: 24 (Phase 5)
- **User Story 4 Tasks**: 24 (Phase 6)
- **User Story 5 Tasks**: 21 (Phase 7)
- **Polish Tasks**: 28 (Phase 8)

**Parallel Opportunities**: 120+ tasks marked [P] can run in parallel

**MVP Scope (Recommended)**: Phase 1 + Phase 2 + Phase 3 = 94 tasks (Weeks 1-3)

**Independent Test Criteria**:
- **US1**: Dashboard loads, all metrics display, charts render
- **US2**: Generate report, download file, byte-match COBOL output
- **US3**: Execute query, view results, export to CSV
- **US4**: Create scheduled job, monitor execution, receive notification
- **US5**: Load CSV data, validate schema, run comparison

---

## Notes

- **[P] tasks** = different files, no dependencies, safe to parallelize
- **[Story] label** maps task to specific user story for traceability
- Each user story is independently completable and testable
- Constitution requirement: 90%+ test coverage for business logic (T119, T120 in US2)
- Constitution requirement: 100% byte-match validation (T132-T136 in US2)
- All user-facing content must be in Portuguese (FR-020)
- Commit after each task or logical group of tasks
- Stop at any checkpoint to validate story independently
- Byte-level COBOL comparison is NON-NEGOTIABLE for regulatory compliance

---

**Generated**: October 22, 2025
**Status**: Ready for implementation
**Next Command**: `/speckit.implement T001` to start implementation
