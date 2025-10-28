using Microsoft.Extensions.Logging;
using PdfGenerator.Models;
using PdfGenerator.PdfGeneration;
using PdfGenerator.PdfGeneration.Sections;
using System.Diagnostics;

namespace PdfGenerator.Services;

/// <summary>
/// Service for orchestrating the complete PDF generation process.
/// </summary>
public interface IDocumentGenerationService
{
    /// <summary>
    /// Generates a complete PDF document based on the provided options.
    /// </summary>
    Task<string> GeneratePdfAsync(DocumentGenerationOptions options);

    /// <summary>
    /// Validates that all required source files exist and are accessible.
    /// </summary>
    Task<bool> ValidateSourceFilesAsync();
}

public class DocumentGenerationOptions
{
    public string OutputDirectory { get; set; } = "output";
    public string Version { get; set; } = "1.0.0";
    public bool Verbose { get; set; }
    public bool DryRun { get; set; }
    public bool GenerateCharts { get; set; } = true;
    public string? CustomTitle { get; set; }
}

public class DocumentGenerationService : IDocumentGenerationService
{
    private readonly IDataExtractor _dataExtractor;
    private readonly IChartGenerator _chartGenerator;
    private readonly IFunctionPointCalculator _functionPointCalculator;
    private readonly IPdfRenderer _pdfRenderer;
    private readonly ILogger<DocumentGenerationService> _logger;

    // Define all sections in order
    private readonly List<IPdfSection> _sections;

    public DocumentGenerationService(
        IDataExtractor dataExtractor,
        IChartGenerator chartGenerator,
        IFunctionPointCalculator functionPointCalculator,
        IPdfRenderer pdfRenderer,
        ILogger<DocumentGenerationService> logger)
    {
        _dataExtractor = dataExtractor;
        _chartGenerator = chartGenerator;
        _functionPointCalculator = functionPointCalculator;
        _pdfRenderer = pdfRenderer;
        _logger = logger;

        // Initialize sections in order
        _sections = new List<IPdfSection>
        {
            new CoverPageSection(),
            new TableOfContentsSection(),
            new ExecutiveSummarySection(),
            new CobolAnalysisSection(),
            new MigrationArchitectureSection(),
            new ComponentSpecsSection(),
            new FunctionPointsSection(),
            new FinancialAnalysisSection(),
            new TimelineSection(),
            new MethodologySection()
        };
    }

    public async Task<string> GeneratePdfAsync(DocumentGenerationOptions options)
    {
        var stopwatch = Stopwatch.StartNew();
        _logger.LogInformation("Starting PDF generation with version {Version}", options.Version);

        try
        {
            // Step 1: Validate source files
            _logger.LogInformation("Step 1: Validating source files");
            var isValid = await ValidateSourceFilesAsync();
            if (!isValid)
            {
                throw new InvalidOperationException("Source file validation failed. Please ensure all required files exist.");
            }

            // Step 2: Extract data from sources
            _logger.LogInformation("Step 2: Extracting data from sources");
            var context = await BuildSectionContextAsync(options);

            // Step 3: Generate charts if enabled
            if (options.GenerateCharts)
            {
                _logger.LogInformation("Step 3: Generating charts");
                await GenerateChartsAsync(context, options);
            }

            // Step 4: Calculate function points and financial analysis
            _logger.LogInformation("Step 4: Calculating function points and financial analysis");
            await CalculateMetricsAsync(context);

            // Step 5: Check for dry run
            if (options.DryRun)
            {
                _logger.LogInformation("Dry run mode - skipping actual PDF generation");
                var dryRunPath = Path.Combine(options.OutputDirectory, $"migration-analysis-v{options.Version}-DRYRUN.pdf");
                return dryRunPath;
            }

            // Step 6: Render PDF document
            _logger.LogInformation("Step 5: Rendering PDF document");
            var outputPath = await RenderPdfAsync(context, options);

            stopwatch.Stop();
            _logger.LogInformation("PDF generation completed in {ElapsedSeconds:F2} seconds. Output: {OutputPath}",
                stopwatch.Elapsed.TotalSeconds, outputPath);

            return outputPath;
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error during PDF generation");
            throw;
        }
    }

    public async Task<bool> ValidateSourceFilesAsync()
    {
        var requiredFiles = new[]
        {
            "docs/parser/FINAL-ANALYSIS-REPORT.md",
            "specs/001-vamos-migrar-sistema/spec.md",
            "specs/001-vamos-migrar-sistema/research.md",
            "specs/001-vamos-migrar-sistema/contracts/openapi.yaml"
        };

        var allValid = true;

        foreach (var file in requiredFiles)
        {
            var fullPath = Path.GetFullPath(file);
            if (!File.Exists(fullPath))
            {
                _logger.LogWarning("Required source file not found: {FilePath}", fullPath);
                allValid = false;
            }
            else
            {
                _logger.LogDebug("Source file validated: {FilePath}", fullPath);
            }
        }

        // Check for data directory
        var dataDir = Path.GetFullPath("data");
        if (!Directory.Exists(dataDir))
        {
            _logger.LogInformation("Creating data directory: {DataDir}", dataDir);
            Directory.CreateDirectory(dataDir);
        }

        return await Task.FromResult(allValid);
    }

    private async Task<SectionContext> BuildSectionContextAsync(DocumentGenerationOptions options)
    {
        var context = new SectionContext
        {
            OutputPath = options.OutputDirectory,
            Metadata = new DocumentMetadata
            {
                Version = options.Version,
                GeneratedAt = DateTime.Now,
                Title = options.CustomTitle ?? "Análise de Migração COBOL para .NET",
                Author = "Caixa Seguradora - Equipe de Modernização"
            }
        };

        // Extract COBOL metrics from analysis report
        var cobolAnalysisPath = Path.GetFullPath("docs/parser/FINAL-ANALYSIS-REPORT.md");
        if (File.Exists(cobolAnalysisPath))
        {
            context.CobolMetrics = await _dataExtractor.ExtractCobolMetricsAsync(cobolAnalysisPath);
            _logger.LogInformation("Extracted COBOL metrics: {DataItems} data items, {Sections} sections",
                context.CobolMetrics.TotalDataItems, context.CobolMetrics.TotalSections);
        }

        // Extract migration architecture from spec
        var specPath = Path.GetFullPath("specs/001-vamos-migrar-sistema/spec.md");
        if (File.Exists(specPath))
        {
            context.Architecture = await _dataExtractor.ExtractArchitectureAsync(specPath);
            _logger.LogInformation("Extracted architecture info: {Stack}", context.Architecture.TechnologyStack);
        }

        // Load function points data
        var fpPath = Path.Combine("data", "function-points.json");
        if (File.Exists(fpPath))
        {
            context.FunctionPoints = await _dataExtractor.ExtractFunctionPointsAsync(fpPath);
            _logger.LogInformation("Loaded {Count} function points", context.FunctionPoints.Length);
        }
        else
        {
            // Use default function points if file doesn't exist
            context.FunctionPoints = GetDefaultFunctionPoints();
            _logger.LogInformation("Using default function points: {Count} items", context.FunctionPoints.Length);
        }

        // Load project schedule
        var schedulePath = Path.Combine("data", "project-schedule.json");
        if (File.Exists(schedulePath))
        {
            context.Schedule = await _dataExtractor.ExtractScheduleAsync(schedulePath);
            _logger.LogInformation("Loaded project schedule: {Weeks} weeks", context.Schedule.TotalDurationWeeks);
        }
        else
        {
            context.Schedule = GetDefaultSchedule();
            _logger.LogInformation("Using default project schedule");
        }

        return context;
    }

    private async Task GenerateChartsAsync(SectionContext context, DocumentGenerationOptions options)
    {
        var chartsDir = Path.Combine(options.OutputDirectory, "charts");
        if (!Directory.Exists(chartsDir))
        {
            Directory.CreateDirectory(chartsDir);
        }

        // Generate various charts
        var functionPointChart = await _chartGenerator.GenerateFunctionPointChartAsync(
            context.FunctionPoints,
            Path.Combine(chartsDir, "function-points.png"));
        _logger.LogInformation("Generated function points chart: {Path}", functionPointChart);

        var timelineChart = await _chartGenerator.GenerateGanttChartAsync(
            context.Schedule,
            Path.Combine(chartsDir, "timeline.png"));
        _logger.LogInformation("Generated timeline Gantt chart: {Path}", timelineChart);

        // Store chart paths in context for sections to use
        context.Metadata.ChartPaths = new Dictionary<string, string>
        {
            ["FunctionPoints"] = functionPointChart,
            ["Timeline"] = timelineChart
        };
    }

    private async Task CalculateMetricsAsync(SectionContext context)
    {
        // Calculate total function points
        var totalPoints = _functionPointCalculator.CalculateTotalPoints(context.FunctionPoints);
        _logger.LogInformation("Total function points calculated: {Points}", totalPoints);

        // Calculate financial analysis
        context.Financial = new FinancialAnalysis
        {
            TotalFunctionPoints = totalPoints,
            CostPerPoint = 750m, // BRL per function point
            TotalCost = totalPoints * 750m,
            PaymentMilestones = new[]
            {
                new PaymentMilestone { Phase = "Fase 1: Fundação", Percentage = 30 },
                new PaymentMilestone { Phase = "Fase 2: Backend Core", Percentage = 40 },
                new PaymentMilestone { Phase = "Fase 3: Geração de Relatórios", Percentage = 20 },
                new PaymentMilestone { Phase = "Fase 4: Validação e Homologação", Percentage = 10 }
            }
        };

        _logger.LogInformation("Financial analysis completed: Total cost = {Cost:C}", context.Financial.TotalCost);
        await Task.CompletedTask;
    }

    private async Task<string> RenderPdfAsync(SectionContext context, DocumentGenerationOptions options)
    {
        // Ensure output directory exists
        if (!Directory.Exists(options.OutputDirectory))
        {
            Directory.CreateDirectory(options.OutputDirectory);
        }

        // Generate output filename
        var fileName = $"migration-analysis-v{options.Version}.pdf";
        var outputPath = Path.Combine(options.OutputDirectory, fileName);

        // Create PDF document with all sections
        var document = new MigrationAnalysisDocument(_sections, context);

        // Render using the PDF renderer
        await _pdfRenderer.RenderAsync(document, outputPath);

        _logger.LogInformation("PDF rendered successfully: {OutputPath}", outputPath);
        return outputPath;
    }

    private static FunctionPoint[] GetDefaultFunctionPoints()
    {
        return new[]
        {
            new FunctionPoint
            {
                Type = FunctionPointType.EO,
                Name = "PREMIT.TXT generation",
                Complexity = Complexity.High,
                DataElements = 52,
                FileReferences = 15,
                RecordTypes = 3,
                Justification = "50+ DETs, 15+ FTRs, complex business rules"
            },
            new FunctionPoint
            {
                Type = FunctionPointType.EO,
                Name = "PREMCED.TXT generation",
                Complexity = Complexity.High,
                DataElements = 45,
                FileReferences = 12,
                RecordTypes = 2,
                Justification = "45+ DETs, 12+ FTRs, complex calculations"
            },
            new FunctionPoint
            {
                Type = FunctionPointType.EI,
                Name = "Premium data input processing",
                Complexity = Complexity.Average,
                DataElements = 20,
                FileReferences = 5,
                RecordTypes = 1,
                Justification = "Standard input processing with validations"
            },
            new FunctionPoint
            {
                Type = FunctionPointType.EQ,
                Name = "Premium report query",
                Complexity = Complexity.Low,
                DataElements = 10,
                FileReferences = 2,
                RecordTypes = 1,
                Justification = "Simple query with basic filters"
            },
            new FunctionPoint
            {
                Type = FunctionPointType.ILF,
                Name = "Premium database",
                Complexity = Complexity.High,
                DataElements = 687,
                FileReferences = 26,
                RecordTypes = 26,
                Justification = "687 data items across 26+ tables"
            }
        };
    }

    private static ProjectSchedule GetDefaultSchedule()
    {
        return new ProjectSchedule
        {
            TotalDurationWeeks = 12,
            DevelopmentWeeks = 8,
            HomologationWeeks = 4,
            Phases = new[]
            {
                new ProjectPhase
                {
                    Name = "Fase 1: Fundação",
                    StartWeek = 1,
                    DurationWeeks = 2,
                    Deliverables = new[] { "Scaffolding do projeto", "Dashboard com métricas" }
                },
                new ProjectPhase
                {
                    Name = "Fase 2: Backend Core",
                    StartWeek = 3,
                    DurationWeeks = 3,
                    Deliverables = new[] { "Entidades", "Repositórios", "Serviços de negócio" }
                },
                new ProjectPhase
                {
                    Name = "Fase 3: Geração de Relatórios",
                    StartWeek = 6,
                    DurationWeeks = 3,
                    Deliverables = new[] { "PREMIT.TXT", "PREMCED.TXT", "Validação de formato" }
                },
                new ProjectPhase
                {
                    Name = "Fase 4: Homologação",
                    StartWeek = 9,
                    DurationWeeks = 4,
                    Deliverables = new[] { "Testes comparativos", "Ajustes", "Aprovação SUSEP" }
                }
            }
        };
    }
}