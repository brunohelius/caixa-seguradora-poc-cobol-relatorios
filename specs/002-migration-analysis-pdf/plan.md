# Implementation Plan: Comprehensive Migration Analysis PDF Report

**Branch**: `002-migration-analysis-pdf` | **Date**: October 23, 2025 | **Spec**: [spec.md](./spec.md)

**Input**: Feature specification from `/specs/002-migration-analysis-pdf/spec.md`

## Summary

Generate a comprehensive PDF document consolidating the complete COBOL RG1866B system analysis, .NET 9 with React migration plan, function point analysis, financial estimates, and detailed implementation timeline. The document will serve as the primary deliverable for stakeholder approval, funding authorization, and regulatory compliance documentation. The PDF will be professionally formatted with Caixa Seguradora branding, containing 100-150 pages with technical diagrams, Gantt charts, cost breakdowns, and methodology documentation.

**Technical Approach**: Build a .NET 9 console application using QuestPDF library for document generation. The application will read existing analysis artifacts (COBOL parser reports, migration specifications, research documentation) from the file system, perform function point calculations using IFPUG methodology, generate charts using ScottPlot or similar library, and produce a PDF/A-1b compliant document with vector graphics and embedded fonts.

## Technical Context

**Language/Version**: C# / .NET 9.0
**Primary Dependencies**:
- QuestPDF 2024.x (MIT license - PDF generation)
- ScottPlot 5.x (MIT license - chart generation)
- Markdig (BSD license - Markdown to formatted text parsing)
- System.Text.Json (built-in - JSON parsing for parser reports)

**Storage**: File system (read existing documentation, write generated PDF)
**Testing**: xUnit with FluentAssertions for unit tests, comparison tests against expected output
**Target Platform**: Cross-platform (Windows, macOS, Linux) console application
**Project Type**: Single project - CLI tool for document generation
**Performance Goals**:
- Generate complete 150-page PDF in under 60 seconds (NFR-001)
- Keep PDF file size under 20MB with 50+ diagrams (NFR-002)
- Support regeneration with updated data without code changes

**Constraints**:
- PDF must comply with PDF/A-1b archival standard (NFR-003)
- All currency formatting must follow Brazilian conventions (R$ 1.234.567,89) (NFR-007)
- Primary language Brazilian Portuguese with proper UTF-8 encoding for accents (NFR-006)
- Must embed fonts to ensure cross-platform rendering consistency
- Vector graphics only (SVG to PDF) for diagrams to maintain quality

**Scale/Scope**:
- 100-150 page document with 10+ major sections
- 22 functional requirements + 9 non-functional requirements
- 50+ charts and diagrams (architecture, Gantt, cost breakdowns, database schemas)
- 687 COBOL data items to document
- 26+ database tables to visualize
- Function point calculations across 5+ functional areas

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

**Status**: Constitution file is a template with no actual principles defined. Therefore, no gates to enforce. Proceeding with best practices for .NET development:

✅ **Assumed Best Practices Applied**:
- Clean Architecture separation (Models, Services, CLI interface)
- Unit testing with xUnit (>90% code coverage for business logic per project standards)
- PDF generation as library code, CLI as thin wrapper
- Configuration via JSON files (appsettings.json) not hardcoded values
- Structured logging with Serilog for observability
- Semantic versioning (1.0.0 for initial release)

**Post-Design Re-check**: Will verify after Phase 1 that:
- All PDF generation logic is testable without file system dependencies (using abstractions)
- CLI supports both JSON output (machine-readable) and formatted text (human-readable)
- Contract tests exist for QuestPDF document structure
- Data source files are validated before processing

## Project Structure

### Documentation (this feature)

```text
specs/002-migration-analysis-pdf/
├── plan.md              # This file (/speckit.plan output)
├── research.md          # Phase 0 output - PDF library evaluation, chart libraries, IFPUG methodology
├── data-model.md        # Phase 1 output - Document structure models, chart data models
├── quickstart.md        # Phase 1 output - How to run PDF generator, regenerate with new data
├── contracts/           # Phase 1 output - Document schema, expected section structure
│   └── document-schema.json
├── checklists/
│   └── requirements.md  # Already created - specification quality validation
└── tasks.md             # Phase 2 output (/speckit.tasks - NOT created by /speckit.plan)
```

### Source Code (repository root)

```text
backend/
└── tools/
    └── PdfGenerator/
        ├── PdfGenerator.sln
        ├── src/
        │   └── PdfGenerator/
        │       ├── PdfGenerator.csproj
        │       ├── Program.cs                    # CLI entry point
        │       ├── Models/
        │       │   ├── DocumentMetadata.cs       # Version, timestamp, source versions
        │       │   ├── CobolMetrics.cs           # 687 data items, 63 sections, etc.
        │       │   ├── MigrationArchitecture.cs  # Backend/frontend specs
        │       │   ├── FunctionPoint.cs          # IFPUG calculation data
        │       │   ├── ProjectSchedule.cs        # Timeline, tasks, milestones
        │       │   └── FinancialAnalysis.cs      # Cost calculations
        │       ├── Services/
        │       │   ├── IDocumentService.cs
        │       │   ├── DocumentService.cs        # Orchestrates PDF generation
        │       │   ├── IDataExtractor.cs
        │       │   ├── DataExtractor.cs          # Reads source documentation
        │       │   ├── IFunctionPointCalculator.cs
        │       │   ├── FunctionPointCalculator.cs # IFPUG methodology implementation
        │       │   ├── IChartGenerator.cs
        │       │   └── ChartGenerator.cs         # Generates charts (ScottPlot)
        │       ├── PdfGeneration/
        │       │   ├── IPdfRenderer.cs
        │       │   ├── QuestPdfRenderer.cs       # QuestPDF implementation
        │       │   ├── Sections/
        │       │   │   ├── CoverPageSection.cs
        │       │   │   ├── TableOfContentsSection.cs
        │       │   │   ├── ExecutiveSummarySection.cs
        │       │   │   ├── CobolAnalysisSection.cs
        │       │   │   ├── MigrationArchitectureSection.cs
        │       │   │   ├── ComponentSpecsSection.cs
        │       │   │   ├── FunctionPointsSection.cs
        │       │   │   ├── FinancialAnalysisSection.cs
        │       │   │   ├── TimelineSection.cs
        │       │   │   └── MethodologySection.cs
        │       │   └── Formatting/
        │       │       ├── BrandingStyles.cs     # Caixa colors, fonts
        │       │       └── TableFormatter.cs     # Consistent table styling
        │       └── appsettings.json              # File paths, branding config
        └── tests/
            └── PdfGenerator.Tests/
                ├── PdfGenerator.Tests.csproj
                ├── Unit/
                │   ├── DataExtractorTests.cs
                │   ├── FunctionPointCalculatorTests.cs
                │   └── ChartGeneratorTests.cs
                ├── Integration/
                │   ├── DocumentServiceTests.cs
                │   └── PdfRenderingTests.cs
                └── TestData/
                    ├── sample-cobol-metrics.json
                    ├── sample-function-points.json
                    └── expected-output-structure.json
```

**Structure Decision**: Single project structure with CLI tool placed in `backend/tools/PdfGenerator/` directory. This separates the PDF generation tool from the main migration application (`backend/src/CaixaSeguradora.Api/`) since it's a one-time documentation generator, not part of the runtime application. The tool will be a self-contained console application that can be executed manually or as part of CI/CD pipeline.

## Complexity Tracking

> **No constitution violations identified.** This section left empty as constitution is not yet defined for the project.

## Phase 0: Research

**Goal**: Resolve technical unknowns and establish best practices for PDF generation, chart rendering, and function point calculation.

### Research Tasks

#### R1: PDF Generation Library Selection

**Decision**: QuestPDF 2024.x

**Rationale**:
- Modern, actively maintained library (last update October 2024)
- Fluent API with C# idioms (similar to HTML/CSS for document layout)
- Native support for vector graphics, charts, images, and tables
- PDF/A compliance support (required by NFR-003)
- MIT license (no licensing issues for commercial use)
- Excellent documentation and community support
- Performance optimized for large documents (tested with 200+ page documents)

**Alternatives Considered**:
1. **iTextSharp (iText 7)** - Rejected: AGPL license requires commercial license purchase ($$$), more complex API, primarily Java-focused with .NET port
2. **PdfSharp** - Rejected: Lower-level API requiring more boilerplate, limited layout capabilities, last major update 2018 (maintenance concerns)
3. **SelectPdf** - Rejected: Commercial license required, primarily HTML-to-PDF converter (doesn't fit programmatic generation use case)

**Implementation Notes**:
```csharp
// QuestPDF fluent API example
Document.Create(container =>
{
    container.Page(page =>
    {
        page.Size(PageSizes.A4);
        page.Margin(1, Unit.Inch);
        page.Header().Text("Caixa Seguradora").FontSize(20).FontColor("#0047BB");
        page.Content().Column(column =>
        {
            column.Item().Text("Executive Summary");
            column.Item().Table(table => { /* table definition */ });
        });
        page.Footer().AlignCenter().PageNumber();
    });
}).GeneratePdf("output.pdf");
```

**References**:
- QuestPDF Documentation: https://www.questpdf.com/
- PDF/A Compliance Guide: https://www.questpdf.com/documentation/pdf-a-compliance.html

---

#### R2: Chart Generation Library Selection

**Decision**: ScottPlot 5.0

**Rationale**:
- .NET native charting library with excellent performance
- Supports all required chart types: bar charts, pie charts, line charts, Gantt charts
- Export to multiple formats including PNG, SVG (vector), and direct bitmap
- Highly customizable styling to match Caixa Seguradora branding
- No external dependencies beyond System.Drawing
- MIT license (compatible with project)
- Active development (5.0 released in 2024)

**Alternatives Considered**:
1. **LiveCharts2** - Rejected: Primarily designed for WPF/WinForms UI, not optimized for server-side rendering, limited Gantt chart support
2. **OxyPlot** - Rejected: Older library, limited documentation, complex API for advanced customization
3. **ZedGraph** - Rejected: Unmaintained (last update 2014), outdated API

**Implementation Notes**:
```csharp
// ScottPlot example for Gantt chart
var plt = new ScottPlot.Plot();
plt.Add.Palette = new ScottPlot.Palettes.Custom(["#0047BB", "#FFB81C"]);
var gantt = plt.Add.Gantt();
gantt.Add(start: new DateTime(2025, 11, 1), duration: TimeSpan.FromDays(7), label: "Phase 1: Foundation");
gantt.Add(start: new DateTime(2025, 11, 8), duration: TimeSpan.FromDays(14), label: "Phase 2: Core Backend");
plt.SavePng("gantt.png", 1200, 600);
```

**Alternative for Complex Gantt Charts**: If ScottPlot's Gantt support is insufficient, use **Mermaid.js** via headless browser (Playwright) to render Gantt diagrams as SVG, then embed in PDF.

**References**:
- ScottPlot Documentation: https://scottplot.net/
- Gantt Chart Examples: https://scottplot.net/cookbook/5.0/Gantt/

---

#### R3: IFPUG Function Point Calculation Methodology

**Decision**: Implement IFPUG Counting Practices Manual 4.3.1 methodology

**Rationale**:
- Industry standard for software project estimation in Brazil
- Accepted by Brazilian government and insurance regulators (SUSEP)
- Provides objective, repeatable calculation method
- Well-documented with examples and case studies
- Aligns with client expectation (750 BRL per function point)

**IFPUG Function Types** (5 categories):
1. **External Inputs (EI)**: User inputs that add/modify data (e.g., report generation form)
2. **External Outputs (EO)**: Reports, files generated (e.g., PREMIT.TXT, PREMCED.TXT files)
3. **External Queries (EQ)**: Data retrieval without modification (e.g., policy query, premium lookup)
4. **Internal Logical Files (ILF)**: Data maintained by application (e.g., Premium table, Policy table)
5. **External Interface Files (EIF)**: Data referenced from external systems (e.g., DB2 views, external modules)

**Complexity Weighting**:
| Type | Low | Average | High |
|------|-----|---------|------|
| EI   | 3   | 4       | 6    |
| EO   | 4   | 5       | 7    |
| EQ   | 3   | 4       | 6    |
| ILF  | 7   | 10      | 15   |
| EIF  | 5   | 7       | 10   |

**Complexity Determination Factors**:
- Number of Data Element Types (DETs) - fields/attributes
- Number of File Types Referenced (FTRs) - tables accessed
- Number of Record Element Types (RETs) - subgroups within files

**Example Calculation for RG1866B Migration**:

**External Outputs (Reports)**:
- PREMIT.TXT generation (EO): 50+ DETs, 15+ FTRs → **High complexity = 7 FP**
- PREMCED.TXT generation (EO): 40+ DETs, 10+ FTRs → **High complexity = 7 FP**
- Dashboard metrics (EO): 20 DETs, 5 FTRs → **Average complexity = 5 FP**

**Internal Logical Files (Database Tables)**:
- V0PREMIOS (ILF): 30+ DETs, 3 RETs → **Average complexity = 10 FP**
- V0APOLICE (ILF): 25+ DETs, 2 RETs → **Average complexity = 10 FP**
- (Continue for all 26+ tables...)

**Total Estimated Function Points**: 450-550 FP (detailed breakdown in function point analysis section of PDF)

**Implementation Notes**:
```csharp
public class FunctionPointCalculator
{
    public decimal Calculate(FunctionPointInput input)
    {
        decimal totalFP = 0;
        totalFP += CalculateEI(input.ExternalInputs);
        totalFP += CalculateEO(input.ExternalOutputs);
        totalFP += CalculateEQ(input.ExternalQueries);
        totalFP += CalculateILF(input.InternalLogicalFiles);
        totalFP += CalculateEIF(input.ExternalInterfaceFiles);

        // Apply Value Adjustment Factor (VAF) based on 14 General System Characteristics
        decimal vaf = CalculateVAF(input.SystemCharacteristics);
        return totalFP * vaf;
    }
}
```

**References**:
- IFPUG Counting Practices Manual 4.3.1: http://www.ifpug.org/
- Brazilian Software Estimation Standards: SISP guidelines
- Function Point Analysis for Insurance Systems: Industry case studies

---

#### R4: Markdown to PDF Conversion Strategy

**Decision**: Use Markdig parser with QuestPDF rendering

**Rationale**:
- Existing documentation is in Markdown format (spec.md, research.md, etc.)
- Markdig is the de facto standard Markdown parser for .NET (used by Microsoft Docs)
- Extensible architecture allows custom rendering for QuestPDF
- Supports CommonMark + GitHub Flavored Markdown extensions
- Handles code blocks, tables, lists, headings with proper parsing

**Implementation Approach**:
1. Parse Markdown AST (Abstract Syntax Tree) using Markdig
2. Walk AST and convert each node type to QuestPDF fluent API calls
3. Maintain heading hierarchy for table of contents generation
4. Apply Caixa branding styles to rendered elements

**Implementation Notes**:
```csharp
public class MarkdownToPdfRenderer
{
    public void Render(string markdown, IContainer container)
    {
        var document = Markdown.Parse(markdown);
        foreach (var block in document)
        {
            if (block is HeadingBlock heading)
                RenderHeading(heading, container);
            else if (block is ParagraphBlock paragraph)
                RenderParagraph(paragraph, container);
            else if (block is CodeBlock code)
                RenderCodeBlock(code, container);
            else if (block is Table table)
                RenderTable(table, container);
        }
    }
}
```

**Alternatives Considered**:
1. **HTML Conversion (Markdown → HTML → PDF)** - Rejected: Extra conversion step, loses control over formatting, harder to apply branding
2. **Manual Text Parsing** - Rejected: Reinventing the wheel, error-prone, doesn't handle Markdown edge cases

**References**:
- Markdig GitHub: https://github.com/xoofx/markdig
- QuestPDF Custom Components: https://www.questpdf.com/documentation/custom-components.html

---

#### R5: Brazilian Currency and Date Formatting

**Decision**: Use CultureInfo("pt-BR") with custom NumberFormatInfo

**Rationale**:
- .NET has built-in support for Brazilian Portuguese locale
- Automatically handles: thousand separators (.), decimal separators (,), currency symbol (R$)
- Correctly formats dates as DD/MM/YYYY (Brazilian standard)
- Thread-safe and GC-friendly (cached culture objects)

**Implementation Notes**:
```csharp
public static class BrazilianFormatter
{
    private static readonly CultureInfo BrazilianCulture = new CultureInfo("pt-BR");

    public static string FormatCurrency(decimal amount)
    {
        return amount.ToString("C", BrazilianCulture); // R$ 1.234.567,89
    }

    public static string FormatDate(DateTime date)
    {
        return date.ToString("dd/MM/yyyy", BrazilianCulture); // 23/10/2025
    }

    public static string FormatNumber(decimal number)
    {
        return number.ToString("N2", BrazilianCulture); // 1.234,56
    }
}
```

**Validation**: All formatted values will be verified against Brazilian accounting standards before PDF generation.

**References**:
- .NET Globalization: https://learn.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo
- Brazilian Number Formats: Central Bank of Brazil standards

---

#### R6: PDF/A-1b Compliance Implementation

**Decision**: Configure QuestPDF with PDF/A-1b settings

**Rationale**:
- PDF/A-1b required for insurance industry archival compliance (NFR-003)
- QuestPDF has built-in support via `DocumentMetadata.PdfA` property
- Ensures 10+ year readability per SUSEP requirements
- Key requirements: embedded fonts, no encryption, self-contained (no external references)

**Implementation Notes**:
```csharp
Document.Create(container =>
{
    container.DocumentSettings(settings =>
    {
        settings.PdfA = true; // Enable PDF/A-1b compliance
        settings.ImageCompressionQuality = ImageCompressionQuality.High;
        settings.EmbedFonts = true; // Required for PDF/A
    });

    container.Metadata(metadata =>
    {
        metadata.Title = "Análise de Migração COBOL para .NET";
        metadata.Author = "Caixa Seguradora - Equipe de Migração";
        metadata.Creator = "PDF Generator v1.0";
        metadata.CreationDate = DateTime.Now;
    });

    // ... document content
});
```

**Validation**: Use PDF/A validator (e.g., VeraPDF) to verify compliance before delivery.

**References**:
- PDF/A Standard: ISO 19005-1:2005
- QuestPDF PDF/A Documentation: https://www.questpdf.com/documentation/pdf-a-compliance.html

---

### Research Checklist

✅ **R1**: PDF library selected (QuestPDF)
✅ **R2**: Chart library selected (ScottPlot)
✅ **R3**: Function point methodology documented (IFPUG 4.3.1)
✅ **R4**: Markdown conversion strategy defined (Markdig + QuestPDF)
✅ **R5**: Brazilian formatting approach (CultureInfo pt-BR)
✅ **R6**: PDF/A compliance implementation (QuestPDF settings)

**Status**: All research tasks complete. No NEEDS CLARIFICATION items remaining. Ready to proceed to Phase 1.

## Phase 1: Design & Contracts

**Prerequisites**: ✅ research.md complete

### Design Artifacts

#### 1. Data Model (`data-model.md`)

The PDF document will be generated from several data model entities extracted from existing documentation:

**Core Models**:

**DocumentMetadata**
- Purpose: Track PDF version, generation timestamp, source data versions
- Fields:
  - `Version` (string): Semantic version (e.g., "1.0.0")
  - `GeneratedAt` (DateTime): Timestamp of generation
  - `GeneratedBy` (string): User or system that generated document
  - `SourceDataVersions` (Dictionary<string, string>): Hashes of source files
  - `ChangeLog` (List<string>): Summary of changes from previous version
- Validation: Version must follow semver format, GeneratedAt must be UTC

**CobolMetrics**
- Purpose: Quantitative analysis of legacy COBOL system
- Fields:
  - `ProgramId` (string): "RG1866B"
  - `ProgramName` (string): "SUSEP Circular 360 Premium Reporting"
  - `TotalLines` (int): Approximate 5,000 lines
  - `DataItems` (int): 687 data items
  - `Sections` (int): 63 sections
  - `Paragraphs` (int): 65 paragraphs
  - `Cursors` (int): 4 cursors
  - `DatabaseTables` (int): 26+ tables/views
  - `ExternalModules` (List<string>): ["RE0001S", "GE0009S", "GE0010S"]
  - `OutputFiles` (List<string>): ["PREMIT.TXT", "PREMCED.TXT"]
- Source: Extracted from `/docs/parser/FINAL-ANALYSIS-REPORT.md`

**MigrationArchitecture**
- Purpose: Describe target .NET and React architecture
- Fields:
  - `BackendFramework` (string): ".NET 9"
  - `FrontendFramework` (string): "React 18"
  - `ArchitecturePattern` (string): "Clean Architecture"
  - `DatabaseTechnology` (string): "SQLite (dev), SQL Server (prod)"
  - `Layers` (List<ArchitectureLayer>): API, Core, Infrastructure
  - `Components` (List<ComponentSpec>): All React components
  - `ApiEndpoints` (int): 28 endpoints
  - `ApiCategories` (List<string>): Reports, Premiums, Policies, etc.
- Source: Extracted from `/specs/001-vamos-migrar-sistema/spec.md`

**FunctionPoint**
- Purpose: IFPUG function point calculation data
- Fields:
  - `Type` (enum): EI, EO, EQ, ILF, EIF
  - `Name` (string): Description of function
  - `Complexity` (enum): Low, Average, High
  - `DataElements` (int): Number of DETs
  - `FileReferences` (int): Number of FTRs
  - `RecordTypes` (int): Number of RETs
  - `Points` (decimal): Calculated function points
- Methods:
  - `CalculatePoints()`: Apply IFPUG weighting based on type and complexity
- Validation: Complexity must be justified by DET/FTR/RET counts

**ProjectSchedule**
- Purpose: 12-week timeline (8 dev + 4 homologation)
- Fields:
  - `TotalDurationWeeks` (int): 12 weeks
  - `DevelopmentWeeks` (int): 8 weeks
  - `HomologationWeeks` (int): 4 weeks
  - `Phases` (List<Phase>): Foundation, Core Backend, Report Gen, Validation, Homologation
  - `Tasks` (List<Task>): Individual work items
  - `Milestones` (List<Milestone>): Key checkpoints
  - `CriticalPath` (List<Task>): Tasks that cannot be delayed
- Relationships: Task → Phase (many-to-one), Task → Dependencies (many-to-many)

**FinancialAnalysis**
- Purpose: Cost calculations and payment schedule
- Fields:
  - `TotalFunctionPoints` (decimal): Sum of all function points
  - `RatePerFunctionPoint` (decimal): 750 BRL (fixed)
  - `TotalProjectCost` (decimal): TotalFunctionPoints × RatePerFunctionPoint
  - `PaymentMilestones` (List<PaymentMilestone>): 30%, 40%, 20%, 10%
  - `CostBreakdownByPhase` (Dictionary<string, decimal>): Phase → Cost
  - `ROIAnalysis` (string): Comparison of legacy vs. modern platform costs
- Validation: Sum of payment milestones must equal 100%

**Chart Data Models**:

**GanttChartData**
- `Tasks` (List<GanttTask>): Task bars with start/end dates
- `Dependencies` (List<Dependency>): Arrows between tasks
- `Milestones` (List<Milestone>): Diamond markers
- `TodayMarker` (DateTime): Current date indicator

**PieChartData**
- `Segments` (List<PieSegment>): Label, value, color

**BarChartData**
- `Categories` (List<string>): X-axis labels
- `Series` (List<BarSeries>): Y-axis values

Full data model documentation will be created in `data-model.md`.

---

#### 2. API Contracts (`contracts/`)

Since this is a CLI tool (not a web API), the "contract" is the document structure schema:

**Document Schema** (`contracts/document-schema.json`):

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "Migration Analysis PDF Document Structure",
  "type": "object",
  "required": ["metadata", "sections"],
  "properties": {
    "metadata": {
      "type": "object",
      "properties": {
        "version": { "type": "string", "pattern": "^\\d+\\.\\d+\\.\\d+$" },
        "generatedAt": { "type": "string", "format": "date-time" },
        "title": { "type": "string" },
        "author": { "type": "string" }
      }
    },
    "sections": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "id": { "type": "string" },
          "title": { "type": "string" },
          "order": { "type": "integer" },
          "required": { "type": "boolean" },
          "subsections": { "type": "array" }
        }
      },
      "minItems": 10
    }
  }
}
```

**Expected Section Structure** (ordered):
1. Cover Page (Caixa branding, title, date)
2. Table of Contents (auto-generated with page numbers)
3. Executive Summary (2 pages max)
4. COBOL System Analysis (687 data items, 63 sections, etc.)
5. Migration Architecture (.NET 9, React, Clean Arch)
6. Component & Page Specifications (React components, API endpoints)
7. Function Point Analysis (IFPUG calculations with tables)
8. Financial Analysis (cost breakdown, payment schedule)
9. Project Timeline (Gantt chart, 12-week schedule)
10. Migration Methodology (MIGRAI, LLM assistance)
11. Technical Diagrams (architecture, database schema)
12. Appendices (detailed specs, references)

Contract schema will be saved to `contracts/document-schema.json`.

---

#### 3. Quickstart Guide (`quickstart.md`)

Will include:
- How to build the PDF generator tool (`dotnet build`)
- How to run PDF generation (`dotnet run --project PdfGenerator`)
- How to regenerate with updated data (modify source JSON files, re-run)
- How to customize branding (edit `appsettings.json` colors/fonts)
- Troubleshooting common issues (missing source files, font errors)

---

#### 4. Agent Context Update

Run agent context update script to add new technologies to Claude Code context:

```bash
.specify/scripts/bash/update-agent-context.sh claude
```

This will add to CLAUDE.md:
- QuestPDF for PDF generation
- ScottPlot for chart generation
- Markdig for Markdown parsing
- IFPUG function point methodology reference

---

### Phase 1 Deliverables Checklist

- [ ] `data-model.md` created with 6 core models + chart models
- [ ] `contracts/document-schema.json` created with PDF structure schema
- [ ] `quickstart.md` created with build/run instructions
- [ ] Agent context updated (CLAUDE.md modified)

**Next Step**: Execute Phase 1 tasks to generate artifacts, then re-evaluate Constitution Check.

## Post-Phase 1 Constitution Re-check

*To be completed after Phase 1 artifacts are generated.*

**Items to verify**:
- [ ] PDF generation logic is in `Services/` layer (testable without file system)
- [ ] CLI in `Program.cs` is thin wrapper (no business logic)
- [ ] Unit tests exist for FunctionPointCalculator
- [ ] Integration tests exist for DocumentService
- [ ] Configuration is externalized in `appsettings.json`
- [ ] Structured logging with Serilog implemented

---

## Notes

### Critical Implementation Considerations

1. **Function Point Accuracy**: IFPUG calculations must be documented with assumptions. Each function type (EI, EO, EQ, ILF, EIF) requires justification for complexity rating based on DET/FTR/RET counts. Include spreadsheet backup in PDF appendix.

2. **PDF Generation Performance**: Target 60 seconds for 150-page document. Profile performance if generation exceeds 45 seconds. Optimize with:
   - Lazy loading of images (don't load until rendering)
   - Parallel chart generation (use Task.WhenAll)
   - Streaming PDF write (not in-memory assembly)

3. **Branding Consistency**: Caixa Seguradora colors (#0047BB, #FFB81C) must match website exactly. Download brand guidelines PDF and extract exact RGB values. Font embedding required for cross-platform compatibility.

4. **Version Control**: Each PDF regeneration increments version number. Store version history in separate JSON file (`pdf-versions.json`). Include git commit hash of source data in metadata.

5. **Chart Readability**: All charts must be legible in grayscale (for B&W printing). Test with accessibility tools. Avoid red/green color combinations (colorblind friendly).

6. **Error Handling**: If source files are missing, provide actionable error message with file path and remediation steps. Don't fail silently.

7. **Internationalization**: While primary language is Portuguese, structure code for future English translation (externalize all strings to resource files).

### Known Limitations

1. **ScottPlot Gantt Limitations**: ScottPlot's Gantt chart support is basic. If complex dependencies (precedence diagram) are required, may need Mermaid.js via headless browser.

2. **PDF Size Optimization**: With 50+ diagrams, file size may exceed 20MB target. Mitigation: compress images to 150 DPI (sufficient for screen), use vector graphics where possible.

3. **Font Licensing**: Ensure fonts used in PDF have redistribution rights. Stick to open fonts (e.g., Roboto, Open Sans) or Caixa-provided corporate fonts.

4. **Regeneration Complexity**: If source documentation structure changes (e.g., spec.md adds new sections), PDF generator may need code updates. Document parsing assumptions in `DataExtractor` class.

### Development Timeline

Based on Migration Strategy in spec.md:

- **Week 1**: Content Aggregation & Analysis (extract data from existing docs)
- **Week 2**: PDF Generation Infrastructure (QuestPDF setup, branding template)
- **Week 3-4**: Content Section Implementation (all 10+ sections)
- **Week 5**: Refinement & Validation (stakeholder review, corrections)

**Total**: 5 weeks from start to production-ready PDF.

### Success Metrics

- [ ] PDF generated in < 60 seconds (NFR-001)
- [ ] PDF file size < 20MB (NFR-002)
- [ ] PDF/A-1b compliant (verified with VeraPDF)
- [ ] All COBOL metrics match parser reports (SC-002)
- [ ] Function points independently verified (SC-003)
- [ ] Stakeholder approval within 5 days (SC-005)
- [ ] 90%+ unit test coverage for calculators
- [ ] Zero layout issues when printed (SC-004)

---

**Plan Status**: ✅ Complete - Ready for Phase 2 (`/speckit.tasks` command)

**Generated Artifacts Location**: `specs/002-migration-analysis-pdf/`
