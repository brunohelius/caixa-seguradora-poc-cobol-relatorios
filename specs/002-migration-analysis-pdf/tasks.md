# Implementation Tasks: Comprehensive Migration Analysis PDF Report

**Feature**: PDF Migration Analysis Document Generator
**Branch**: `002-migration-analysis-pdf`
**Total Tasks**: 87 tasks
**Estimated Duration**: 5 weeks (content aggregation → infrastructure → sections → refinement → polish)

## Task Overview

| Phase | User Story | Task Count | Parallel Tasks | Can Start After |
|-------|------------|------------|----------------|-----------------|
| Phase 1: Setup | N/A | 12 tasks | 5 parallel | Immediate |
| Phase 2: Foundational | N/A | 8 tasks | 4 parallel | Phase 1 complete |
| Phase 3: US1 - PDF Generation | US1 (P1) | 52 tasks | 20 parallel | Phase 2 complete |
| Phase 4: US2 - Progress Updates | US2 (P2) | 10 tasks | 5 parallel | US1 complete |
| Phase 5: US3 - Multi-Language | US3 (P4) | 3 tasks | 2 parallel | US1 complete |
| Phase 6: Polish | N/A | 2 tasks | 0 parallel | All stories complete |

**MVP Scope**: Phase 1 + Phase 2 + Phase 3 (US1) = Complete PDF generation capability

---

## Phase 1: Project Setup & Infrastructure

**Goal**: Initialize .NET project structure, install dependencies, configure tooling

**Duration**: 2-3 days

### Tasks

- [X] T001 Create solution file `backend/tools/PdfGenerator/PdfGenerator.sln`
- [X] T002 [P] Create main project `backend/tools/PdfGenerator/src/PdfGenerator/PdfGenerator.csproj` with .NET 9 target framework
- [X] T003 [P] Create test project `backend/tools/PdfGenerator/tests/PdfGenerator.Tests/PdfGenerator.Tests.csproj` with xUnit
- [X] T004 [P] Add NuGet package QuestPDF 2024.x to `backend/tools/PdfGenerator/src/PdfGenerator/PdfGenerator.csproj`
- [X] T005 [P] Add NuGet package ScottPlot 5.x to `backend/tools/PdfGenerator/src/PdfGenerator/PdfGenerator.csproj`
- [X] T006 [P] Add NuGet package Markdig to `backend/tools/PdfGenerator/src/PdfGenerator/PdfGenerator.csproj`
- [X] T007 Add NuGet packages xUnit, FluentAssertions, Moq to test project `backend/tools/PdfGenerator/tests/PdfGenerator.Tests/PdfGenerator.Tests.csproj`
- [X] T008 Create directory structure `backend/tools/PdfGenerator/src/PdfGenerator/Models/`
- [X] T009 Create directory structure `backend/tools/PdfGenerator/src/PdfGenerator/Services/`
- [X] T010 Create directory structure `backend/tools/PdfGenerator/src/PdfGenerator/PdfGeneration/`
- [X] T011 Create configuration file `backend/tools/PdfGenerator/src/PdfGenerator/appsettings.json` with branding and source file paths
- [X] T012 Create README.md in `backend/tools/PdfGenerator/README.md` with quickstart instructions

**Acceptance Criteria**:
- Solution builds successfully with `dotnet build`
- All NuGet packages restore without errors
- Test project runs with `dotnet test` (no tests yet, should pass with 0 tests)

---

## Phase 2: Foundational Components

**Goal**: Implement core models and services that all user stories depend on

**Duration**: 3-4 days

### Tasks

- [X] T013 [P] Implement DocumentMetadata model in `backend/tools/PdfGenerator/src/PdfGenerator/Models/DocumentMetadata.cs`
- [X] T014 [P] Implement CobolMetrics model in `backend/tools/PdfGenerator/src/PdfGenerator/Models/CobolMetrics.cs`
- [X] T015 [P] Implement MigrationArchitecture model in `backend/tools/PdfGenerator/src/PdfGenerator/Models/MigrationArchitecture.cs`
- [X] T016 [P] Implement FunctionPoint model with CalculatePoints() method in `backend/tools/PdfGenerator/src/PdfGenerator/Models/FunctionPoint.cs`
- [X] T017 Implement IDataExtractor interface in `backend/tools/PdfGenerator/src/PdfGenerator/Services/IDataExtractor.cs`
- [X] T018 Implement DataExtractor service with markdown parsing in `backend/tools/PdfGenerator/src/PdfGenerator/Services/DataExtractor.cs`
- [X] T019 Implement BrazilianFormatter utility class in `backend/tools/PdfGenerator/src/PdfGenerator/Formatting/BrazilianFormatter.cs` with CultureInfo pt-BR
- [X] T020 Create test data files in `backend/tools/PdfGenerator/tests/PdfGenerator.Tests/TestData/` (sample-cobol-metrics.json, sample-function-points.json)

**Acceptance Criteria**:
- All models can be instantiated and validated
- DataExtractor can read markdown files and extract COBOL metrics
- BrazilianFormatter correctly formats currency (R$ 1.234.567,89) and dates (DD/MM/YYYY)
- Test data files load successfully

---

## Phase 3: User Story 1 - Generate Comprehensive Migration Analysis PDF (P1)

**Goal**: Implement complete PDF generation with all 12 required sections

**Independent Test**: Execute PDF generator CLI, verify output contains all sections with correct data, validate PDF/A-1b compliance, check branding and formatting

**Duration**: 2-3 weeks

### 3.1: Chart Generation (Parallel Track)

- [X] T021 [P] [US1] Implement IChartGenerator interface in `backend/tools/PdfGenerator/src/PdfGenerator/Services/IChartGenerator.cs`
- [X] T022 [P] [US1] Implement ChartGenerator service with ScottPlot in `backend/tools/PdfGenerator/src/PdfGenerator/Services/ChartGenerator.cs`
- [X] T023 [P] [US1] Implement GanttChartData model in `backend/tools/PdfGenerator/src/PdfGenerator/Models/ChartModels/GanttChartData.cs`
- [X] T024 [P] [US1] Implement PieChartData model in `backend/tools/PdfGenerator/src/PdfGenerator/Models/ChartModels/PieChartData.cs`
- [X] T025 [P] [US1] Implement BarChartData model in `backend/tools/PdfGenerator/src/PdfGenerator/Models/ChartModels/BarChartData.cs`
- [X] T026 [P] [US1] Add GenerateGanttChart method to ChartGenerator in `backend/tools/PdfGenerator/src/PdfGenerator/Services/ChartGenerator.cs`
- [X] T027 [P] [US1] Add GeneratePieChart method to ChartGenerator in `backend/tools/PdfGenerator/src/PdfGenerator/Services/ChartGenerator.cs`
- [X] T028 [P] [US1] Add GenerateBarChart method to ChartGenerator in `backend/tools/PdfGenerator/src/PdfGenerator/Services/ChartGenerator.cs`
- [X] T029 [US1] Apply Caixa Seguradora branding colors (#0047BB, #FFB81C) to all charts in `backend/tools/PdfGenerator/src/PdfGenerator/Services/ChartGenerator.cs`

### 3.2: Function Point Calculation (Parallel Track)

- [X] T030 [P] [US1] Implement IFunctionPointCalculator interface in `backend/tools/PdfGenerator/src/PdfGenerator/Services/IFunctionPointCalculator.cs`
- [X] T031 [P] [US1] Implement FunctionPointCalculator with IFPUG weighting table in `backend/tools/PdfGenerator/src/PdfGenerator/Services/FunctionPointCalculator.cs`
- [X] T032 [US1] Implement ProjectSchedule model in `backend/tools/PdfGenerator/src/PdfGenerator/Models/ProjectSchedule.cs` with Phase, Task, Milestone classes
- [X] T033 [US1] Implement FinancialAnalysis model in `backend/tools/PdfGenerator/src/PdfGenerator/Models/FinancialAnalysis.cs` with payment milestones
- [X] T034 [US1] Add CalculateFinancialAnalysis method to FunctionPointCalculator in `backend/tools/PdfGenerator/src/PdfGenerator/Services/FunctionPointCalculator.cs`

### 3.3: PDF Rendering Infrastructure (Sequential)

- [X] T035 [US1] Create IPdfRenderer interface in `backend/tools/PdfGenerator/src/PdfGenerator/PdfGeneration/IPdfRenderer.cs`
- [X] T036 [US1] Implement QuestPdfRenderer with PDF/A-1b configuration in `backend/tools/PdfGenerator/src/PdfGenerator/PdfGeneration/QuestPdfRenderer.cs`
- [X] T037 [US1] Create BrandingStyles class with Caixa colors and fonts in `backend/tools/PdfGenerator/src/PdfGenerator/PdfGeneration/Formatting/BrandingStyles.cs`
- [X] T038 [US1] Create TableFormatter class in `backend/tools/PdfGenerator/src/PdfGenerator/PdfGeneration/Formatting/TableFormatter.cs`
- [X] T039 [US1] Implement MarkdownToPdfRenderer in `backend/tools/PdfGenerator/src/PdfGenerator/PdfGeneration/MarkdownToPdfRenderer.cs` using Markdig

### 3.4: PDF Sections (Parallel Track - Each section independent)

- [ ] T040 [P] [US1] Implement CoverPageSection in `backend/tools/PdfGenerator/src/PdfGenerator/PdfGeneration/Sections/CoverPageSection.cs` with logo and title
- [ ] T041 [P] [US1] Implement TableOfContentsSection with hyperlinks in `backend/tools/PdfGenerator/src/PdfGenerator/PdfGeneration/Sections/TableOfContentsSection.cs`
- [ ] T042 [P] [US1] Implement ExecutiveSummarySection (2 pages max) in `backend/tools/PdfGenerator/src/PdfGenerator/PdfGeneration/Sections/ExecutiveSummarySection.cs`
- [ ] T043 [P] [US1] Implement CobolAnalysisSection with 687 data items table in `backend/tools/PdfGenerator/src/PdfGenerator/PdfGeneration/Sections/CobolAnalysisSection.cs`
- [ ] T044 [P] [US1] Implement MigrationArchitectureSection with layer diagrams in `backend/tools/PdfGenerator/src/PdfGenerator/PdfGeneration/Sections/MigrationArchitectureSection.cs`
- [ ] T045 [P] [US1] Implement ComponentSpecsSection with React components list in `backend/tools/PdfGenerator/src/PdfGenerator/PdfGeneration/Sections/ComponentSpecsSection.cs`
- [ ] T046 [P] [US1] Implement FunctionPointsSection with IFPUG calculation tables in `backend/tools/PdfGenerator/src/PdfGenerator/PdfGeneration/Sections/FunctionPointsSection.cs`
- [ ] T047 [P] [US1] Implement FinancialAnalysisSection with cost breakdown in `backend/tools/PdfGenerator/src/PdfGenerator/PdfGeneration/Sections/FinancialAnalysisSection.cs`
- [ ] T048 [P] [US1] Implement TimelineSection with Gantt chart in `backend/tools/PdfGenerator/src/PdfGenerator/PdfGeneration/Sections/TimelineSection.cs`
- [ ] T049 [P] [US1] Implement MethodologySection explaining MIGRAI in `backend/tools/PdfGenerator/src/PdfGenerator/PdfGeneration/Sections/MethodologySection.cs`

### 3.5: Document Service Orchestration

- [ ] T050 [US1] Create IDocumentService interface in `backend/tools/PdfGenerator/src/PdfGenerator/Services/IDocumentService.cs`
- [ ] T051 [US1] Implement DocumentService with section orchestration in `backend/tools/PdfGenerator/src/PdfGenerator/Services/DocumentService.cs`
- [ ] T052 [US1] Add GeneratePdfAsync method to DocumentService in `backend/tools/PdfGenerator/src/PdfGenerator/Services/DocumentService.cs`
- [ ] T053 [US1] Implement file validation in DocumentService.GeneratePdfAsync checking all source files exist
- [ ] T054 [US1] Implement error handling with descriptive messages in DocumentService

### 3.6: CLI Implementation

- [ ] T055 [US1] Implement Program.cs with command-line argument parsing in `backend/tools/PdfGenerator/src/PdfGenerator/Program.cs`
- [ ] T056 [US1] Add dependency injection setup in Program.cs configuring all services
- [ ] T057 [US1] Add Serilog logging configuration in Program.cs
- [ ] T058 [US1] Implement --version, --output, --verbose, --dry-run CLI options in Program.cs
- [ ] T059 [US1] Implement JSON output mode for CI/CD in Program.cs

### 3.7: Data Preparation

- [ ] T060 [US1] Create function-points.json in `data/function-points.json` with sample IFPUG calculations
- [ ] T061 [US1] Create project-schedule.json in `data/project-schedule.json` with 12-week timeline
- [ ] T062 [US1] Validate function point data against IFPUG methodology using FunctionPointCalculator

### 3.8: Testing

- [ ] T063 [US1] Write unit tests for FunctionPointCalculator in `backend/tools/PdfGenerator/tests/PdfGenerator.Tests/Unit/FunctionPointCalculatorTests.cs`
- [ ] T064 [US1] Write unit tests for DataExtractor in `backend/tools/PdfGenerator/tests/PdfGenerator.Tests/Unit/DataExtractorTests.cs`
- [ ] T065 [US1] Write unit tests for ChartGenerator in `backend/tools/PdfGenerator/tests/PdfGenerator.Tests/Unit/ChartGeneratorTests.cs`
- [ ] T066 [US1] Write integration tests for DocumentService in `backend/tools/PdfGenerator/tests/PdfGenerator.Tests/Integration/DocumentServiceTests.cs`
- [ ] T067 [US1] Write integration test for complete PDF generation in `backend/tools/PdfGenerator/tests/PdfGenerator.Tests/Integration/PdfRenderingTests.cs`
- [ ] T068 [US1] Verify PDF/A-1b compliance using VeraPDF validator
- [ ] T069 [US1] Test PDF rendering on macOS, Windows, Linux with different PDF viewers
- [ ] T070 [US1] Validate Brazilian Portuguese formatting in all text content
- [ ] T071 [US1] Test print output (color and grayscale) to verify chart readability
- [ ] T072 [US1] Verify file size < 20MB with 50+ diagrams

**User Story 1 Acceptance Criteria**:
- ✅ PDF generated in < 60 seconds
- ✅ All 12 sections present with correct data
- ✅ COBOL metrics match parser reports exactly
- ✅ Function points calculated per IFPUG 4.3.1
- ✅ Financial calculations accurate (Total = FP × 750 BRL)
- ✅ Gantt chart displays 12-week timeline
- ✅ Caixa Seguradora branding applied correctly
- ✅ PDF/A-1b compliant
- ✅ Brazilian Portuguese formatting throughout
- ✅ File size < 20MB

---

## Phase 4: User Story 2 - Update PDF with Latest Migration Progress (P2)

**Goal**: Enable PDF regeneration with updated metrics while maintaining version history

**Independent Test**: Modify function-points.json and project-schedule.json with updated values, regenerate PDF, verify changes reflected and version incremented

**Prerequisites**: User Story 1 complete

**Duration**: 3-4 days

### Tasks

- [ ] T073 [P] [US2] Implement version comparison logic in DocumentService checking source file hashes
- [ ] T074 [P] [US2] Add auto-increment version logic in DocumentService when source data changes
- [ ] T075 [P] [US2] Create ChangeLogGenerator service in `backend/tools/PdfGenerator/src/PdfGenerator/Services/ChangeLogGenerator.cs`
- [ ] T076 [US2] Implement diff detection for function points in ChangeLogGenerator
- [ ] T077 [US2] Implement diff detection for schedule in ChangeLogGenerator
- [ ] T078 [US2] Add progress indicators to Gantt chart (task completion percentages) in TimelineSection
- [ ] T079 [US2] Add actual vs. planned timeline comparison table in TimelineSection
- [ ] T080 [US2] Store version history in `output/pdf-versions.json` file
- [ ] T081 [US2] Write integration test for version increment logic
- [ ] T082 [US2] Write test for change log generation comparing v1.0.0 to v1.1.0

**User Story 2 Acceptance Criteria**:
- ✅ PDF version auto-increments when source data changes
- ✅ Change log section documents modifications
- ✅ Gantt chart shows updated progress with color coding
- ✅ Version history file tracks all regenerations
- ✅ Baseline comparison preserved in metadata

---

## Phase 5: User Story 3 - Export PDF in Multiple Languages (P4)

**Goal**: Support English translation for international stakeholders

**Independent Test**: Run PDF generator with `--language en` option, verify all text in English while technical terms preserved

**Prerequisites**: User Story 1 complete

**Duration**: 2 days

### Tasks

- [ ] T083 [P] [US3] Create resource files for Portuguese in `backend/tools/PdfGenerator/src/PdfGenerator/Resources/Strings.pt-BR.resx`
- [ ] T084 [P] [US3] Create resource files for English in `backend/tools/PdfGenerator/src/PdfGenerator/Resources/Strings.en.resx`
- [ ] T085 [US3] Implement language selection in Program.cs with --language CLI option

**User Story 3 Acceptance Criteria**:
- ✅ All UI text translates to English when --language en specified
- ✅ Technical terms (COBOL, .NET, React) remain unchanged
- ✅ Numerical data (dates, currency) adapts to locale

---

## Phase 6: Polish & Cross-Cutting Concerns

**Goal**: Final quality improvements and documentation

**Duration**: 1-2 days

### Tasks

- [ ] T086 Add comprehensive error messages for all failure scenarios in DocumentService
- [ ] T087 Update README.md with complete usage examples, troubleshooting, and CI/CD integration

**Acceptance Criteria**:
- ✅ All error messages actionable with remediation steps
- ✅ README complete with examples
- ✅ CI/CD integration documented

---

## Dependency Graph

### Story Completion Order

```text
Phase 1: Setup (T001-T012)
         ↓
Phase 2: Foundational (T013-T020)
         ↓
    ┌────┴────┬────────────────┐
    ↓         ↓                ↓
Phase 3:   Phase 4:        Phase 5:
US1 (P1)   US2 (P2)        US3 (P4)
(T021-T072) (T073-T082)    (T083-T085)
    │         │                │
    └────┬────┴────────────────┘
         ↓
Phase 6: Polish (T086-T087)
```

**Critical Path**: Phase 1 → Phase 2 → Phase 3 (US1) = Minimum viable product

**Parallel Opportunities**:
- US2 and US3 can be implemented concurrently after US1
- Within US1: Chart generation, function point calculation, and PDF sections can be parallelized

---

## Parallel Execution Examples

### Phase 1: Setup (5 tasks in parallel)
```bash
# Can run simultaneously:
T002: Create main project
T003: Create test project
T004: Add QuestPDF package
T005: Add ScottPlot package
T006: Add Markdig package
```

### Phase 2: Foundational (4 models in parallel)
```bash
# Can run simultaneously:
T013: DocumentMetadata model
T014: CobolMetrics model
T015: MigrationArchitecture model
T016: FunctionPoint model
```

### Phase 3.1: Chart Generation (all parallel)
```bash
# Can run simultaneously:
T021-T028: All chart-related tasks (8 tasks)
```

### Phase 3.4: PDF Sections (10 sections in parallel)
```bash
# Can run simultaneously:
T040: CoverPageSection
T041: TableOfContentsSection
T042: ExecutiveSummarySection
T043: CobolAnalysisSection
T044: MigrationArchitectureSection
T045: ComponentSpecsSection
T046: FunctionPointsSection
T047: FinancialAnalysisSection
T048: TimelineSection
T049: MethodologySection
```

---

## Implementation Strategy

### MVP Scope (Week 1-3)
**Goal**: Generate basic PDF with all required sections

**Includes**:
- Phase 1: Setup (T001-T012)
- Phase 2: Foundational (T013-T020)
- Phase 3: User Story 1 (T021-T072)

**Deliverable**: Working PDF generator producing 100-150 page document with all sections

### Enhancement 1 (Week 4)
**Goal**: Add progress tracking and versioning

**Includes**:
- Phase 4: User Story 2 (T073-T082)

**Deliverable**: PDF regeneration with change tracking

### Enhancement 2 (Week 5)
**Goal**: Multi-language support and polish

**Includes**:
- Phase 5: User Story 3 (T083-T085)
- Phase 6: Polish (T086-T087)

**Deliverable**: Production-ready tool with documentation

---

## Independent Test Criteria

### User Story 1 (P1) - Complete PDF Generation
```bash
# Execute from repository root
dotnet run --project backend/tools/PdfGenerator/src/PdfGenerator

# Verify output exists
test -f output/migration-analysis-v1.0.0.pdf

# Validate PDF/A compliance
verapdf output/migration-analysis-v1.0.0.pdf

# Check file size
SIZE=$(stat -f%z output/migration-analysis-v1.0.0.pdf)
[ $SIZE -lt 20971520 ] # < 20MB

# Verify sections present (12 required)
pdftotext output/migration-analysis-v1.0.0.pdf - | grep -c "Executive Summary"
# Should output: 1

# Test manual review checklist:
# □ Caixa branding present
# □ All COBOL metrics match parser report
# □ Function points calculated correctly
# □ Financial totals accurate
# □ Gantt chart displays 12-week timeline
# □ Brazilian Portuguese formatting
# □ Charts legible in grayscale
```

### User Story 2 (P2) - Progress Updates
```bash
# Modify source data
echo '{"type":"EO","name":"New Report","complexity":"High"}' >> data/function-points.json

# Regenerate
dotnet run --project backend/tools/PdfGenerator/src/PdfGenerator

# Verify version incremented
test -f output/migration-analysis-v1.1.0.pdf

# Check change log present
pdftotext output/migration-analysis-v1.1.0.pdf - | grep -c "Change Log"
# Should output: 1
```

### User Story 3 (P4) - Multi-Language
```bash
# Generate in English
dotnet run --project backend/tools/PdfGenerator/src/PdfGenerator -- --language en

# Verify English text
pdftotext output/migration-analysis-v1.0.0-en.pdf - | grep -c "Executive Summary"
# Should output: 1 (English text, not Portuguese)
```

---

## Task Execution Guidelines

### Before Starting
1. Ensure Phase 1 and Phase 2 complete before User Stories
2. Read plan.md and data-model.md for context
3. Review quickstart.md for testing approach

### During Implementation
1. Follow file paths exactly as specified
2. Mark tasks complete only when acceptance criteria met
3. Run tests after each subsection (3.1, 3.2, 3.3, etc.)
4. Commit after each phase completes

### Testing Approach
1. Unit tests for calculators and extractors
2. Integration tests for document generation
3. Manual verification for PDF quality
4. Performance testing with sample data

### Performance Optimization
1. Generate charts in parallel (Task.WhenAll)
2. Lazy load images during rendering
3. Stream PDF output (don't assemble in memory)
4. Profile if generation exceeds 45 seconds

---

## Success Metrics

- [ ] PDF generated in < 60 seconds (NFR-001)
- [ ] PDF file size < 20MB (NFR-002)
- [ ] PDF/A-1b compliant (NFR-003)
- [ ] All COBOL metrics match parser reports (SC-002)
- [ ] Function points independently verified (SC-003)
- [ ] 90%+ unit test coverage for calculators
- [ ] Zero layout issues when printed (SC-004)
- [ ] Stakeholder approval within 5 days (SC-005)

---

**Next Command**: Start with Phase 1 tasks (T001-T012) to initialize project structure.
