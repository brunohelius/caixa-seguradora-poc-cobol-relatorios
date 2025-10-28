# Quickstart Guide: PDF Migration Analysis Generator

**Last Updated**: October 23, 2025
**Tool**: PdfGenerator - CLI tool for generating migration analysis PDF documents

## Prerequisites

- .NET 9 SDK installed
- Git repository cloned
- Source documentation files present (COBOL parser reports, migration specs)

## Quick Start

### 1. Build the PDF Generator

```bash
# Navigate to PDF generator project
cd backend/tools/PdfGenerator

# Restore dependencies
dotnet restore

# Build the project
dotnet build --configuration Release
```

### 2. Prepare Source Data

Ensure all required source files exist:

```bash
# Check COBOL analysis report
ls docs/parser/FINAL-ANALYSIS-REPORT.md

# Check migration specification
ls specs/001-vamos-migrar-sistema/spec.md

# Check research documentation
ls specs/001-vamos-migrar-sistema/research.md

# Check OpenAPI contract
ls specs/001-vamos-migrar-sistema/contracts/openapi.yaml
```

### 3. Generate PDF

```bash
# Run the PDF generator (from project root)
cd /Users/brunosouza/Development/Caixa\ Seguradora/POC\ Cobol
dotnet run --project backend/tools/PdfGenerator/src/PdfGenerator

# Output will be generated at:
# ./output/migration-analysis-v1.0.0.pdf
```

### 4. Verify Output

```bash
# Check file was created
ls -lh output/migration-analysis-*.pdf

# Validate PDF/A compliance (if VeraPDF installed)
verapdf output/migration-analysis-v1.0.0.pdf

# Open PDF for review
open output/migration-analysis-v1.0.0.pdf  # macOS
# or
xdg-open output/migration-analysis-v1.0.0.pdf  # Linux
# or
start output/migration-analysis-v1.0.0.pdf  # Windows
```

## Configuration

### Customize Branding

Edit `backend/tools/PdfGenerator/src/PdfGenerator/appsettings.json`:

```json
{
  "Branding": {
    "PrimaryColor": "#0047BB",
    "AccentColor": "#FFB81C",
    "LogoPath": "assets/caixa-seguradora-logo.png",
    "CompanyName": "Caixa Seguradora",
    "DocumentTitle": "Análise de Migração COBOL para .NET"
  },
  "SourceFiles": {
    "CobolAnalysisReport": "docs/parser/FINAL-ANALYSIS-REPORT.md",
    "MigrationSpec": "specs/001-vamos-migrar-sistema/spec.md",
    "ResearchDoc": "specs/001-vamos-migrar-sistema/research.md",
    "OpenApiContract": "specs/001-vamos-migrar-sistema/contracts/openapi.yaml",
    "FunctionPointsJson": "data/function-points.json",
    "ProjectScheduleJson": "data/project-schedule.json"
  },
  "Output": {
    "Directory": "output",
    "FileNamePattern": "migration-analysis-v{version}.pdf"
  }
}
```

### Update Function Points

Edit `data/function-points.json` to add/modify function point calculations:

```json
[
  {
    "type": "EO",
    "name": "PREMIT.TXT generation",
    "complexity": "High",
    "dataElements": 52,
    "fileReferences": 15,
    "recordTypes": 3,
    "justification": "50+ DETs, 15+ FTRs, complex business rules"
  },
  {
    "type": "EO",
    "name": "PREMCED.TXT generation",
    "complexity": "High",
    "dataElements": 45,
    "fileReferences": 12,
    "recordTypes": 2,
    "justification": "40+ DETs, 10+ FTRs, cossurance calculations"
  }
]
```

### Update Project Schedule

Edit `data/project-schedule.json` to modify timeline:

```json
{
  "totalDurationWeeks": 12,
  "developmentWeeks": 8,
  "homologationWeeks": 4,
  "phases": [
    {
      "name": "Phase 1: Foundation",
      "startWeek": 1,
      "durationWeeks": 2,
      "deliverables": [
        "Project scaffolding",
        "Dashboard with metrics",
        "Data dictionary"
      ]
    }
  ],
  "tasks": [
    {
      "id": "T001",
      "name": "Analyze parser output",
      "durationDays": 3,
      "dependencies": [],
      "phase": "Phase 1: Foundation"
    }
  ]
}
```

## Regenerating PDF with Updated Data

After modifying function points or schedule:

```bash
# Increment version in source data files
# (PDF generator auto-detects changes)

# Regenerate PDF
dotnet run --project backend/tools/PdfGenerator/src/PdfGenerator

# New version will be created:
# output/migration-analysis-v1.1.0.pdf
```

## Command-Line Options

```bash
# Generate with specific version
dotnet run --project backend/tools/PdfGenerator/src/PdfGenerator -- --version 2.0.0

# Generate to custom output directory
dotnet run --project backend/tools/PdfGenerator/src/PdfGenerator -- --output /path/to/output

# Generate with verbose logging
dotnet run --project backend/tools/PdfGenerator/src/PdfGenerator -- --verbose

# Dry run (validate data without generating PDF)
dotnet run --project backend/tools/PdfGenerator/src/PdfGenerator -- --dry-run

# JSON output mode (for CI/CD)
dotnet run --project backend/tools/PdfGenerator/src/PdfGenerator -- --json
```

## Troubleshooting

### Error: Source file not found

```
Error: Could not find file 'docs/parser/FINAL-ANALYSIS-REPORT.md'
```

**Solution**: Ensure you're running from repository root directory, not from PdfGenerator subdirectory.

```bash
# Correct:
cd /Users/brunosouza/Development/Caixa\ Seguradora/POC\ Cobol
dotnet run --project backend/tools/PdfGenerator/src/PdfGenerator

# Wrong:
cd backend/tools/PdfGenerator
dotnet run
```

### Error: Font not found

```
Error: Font 'Roboto' not found or cannot be embedded
```

**Solution**: Install missing fonts or update `appsettings.json` to use system fonts:

```json
{
  "Branding": {
    "FontFamily": "Arial"  // Use system font instead
  }
}
```

### Error: Invalid function point data

```
Error: Function point complexity does not match DET/FTR counts
```

**Solution**: Review `data/function-points.json` and ensure complexity ratings are justified:
- High complexity: DataElements > 19 OR FileReferences > 5
- Average: DataElements 5-19 OR FileReferences 2-5
- Low: DataElements < 5 AND FileReferences < 2

### PDF file size exceeds 20MB

**Solution**: Reduce chart resolution in `appsettings.json`:

```json
{
  "Charts": {
    "DPI": 150,  // Reduce from 300 to 150
    "Format": "PNG",  // Or use "SVG" for vector graphics
    "CompressionQuality": 80  // Add compression
  }
}
```

## Testing

### Run Unit Tests

```bash
cd backend/tools/PdfGenerator
dotnet test --filter Category=Unit
```

### Run Integration Tests

```bash
dotnet test --filter Category=Integration
```

### Validate PDF Output

```bash
# Check PDF structure
dotnet test --filter FullyQualifiedName~PdfRenderingTests

# Compare with expected output
diff output/migration-analysis-v1.0.0.pdf tests/PdfGenerator.Tests/TestData/expected-output.pdf
```

## CI/CD Integration

### GitHub Actions Example

```yaml
name: Generate Migration Analysis PDF

on:
  push:
    paths:
      - 'data/function-points.json'
      - 'data/project-schedule.json'
      - 'specs/**/*.md'

jobs:
  generate-pdf:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Setup .NET
        uses: actions/setup-dotnet@v3
        with:
          dotnet-version: '9.0.x'

      - name: Generate PDF
        run: |
          dotnet run --project backend/tools/PdfGenerator/src/PdfGenerator -- --json

      - name: Upload artifact
        uses: actions/upload-artifact@v3
        with:
          name: migration-analysis-pdf
          path: output/*.pdf
```

## Performance Tips

- **Parallel chart generation**: Charts are generated in parallel automatically
- **Lazy loading**: Images loaded only when needed during rendering
- **Streaming output**: PDF written in chunks, not assembled in memory
- **Expected performance**: 150-page PDF in 30-45 seconds on modern hardware

## Next Steps

1. Review generated PDF for accuracy
2. Verify all COBOL metrics match parser reports
3. Validate function point calculations with third party
4. Share PDF with stakeholders for approval
5. Regenerate with updated data as project progresses

## Support

For issues or questions:
- Check `backend/tools/PdfGenerator/logs/` for detailed error logs
- Review `specs/002-migration-analysis-pdf/plan.md` for implementation details
- Consult `specs/002-migration-analysis-pdf/data-model.md` for data structure reference
