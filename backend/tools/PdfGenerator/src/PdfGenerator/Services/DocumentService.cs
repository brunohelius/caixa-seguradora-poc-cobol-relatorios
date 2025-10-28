using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Security.Cryptography;
using System.Text;
using System.Text.Json;
using System.Threading.Tasks;
using Microsoft.Extensions.Logging;
using PdfGenerator.Models;
using PdfGenerator.PdfGeneration;
using PdfGenerator.PdfGeneration.Sections;
using QuestPDF.Fluent;
using QuestPDF.Helpers;
using QuestPDF.Infrastructure;

namespace PdfGenerator.Services;

/// <summary>
/// Service orchestrating PDF document generation with all sections
/// </summary>
public class DocumentService : IDocumentService
{
    private readonly ILogger<DocumentService> _logger;
    private readonly IDataExtractor _dataExtractor;
    private readonly IFunctionPointCalculator _functionPointCalculator;
    private readonly IChartGenerator _chartGenerator;
    private readonly IPdfRenderer _pdfRenderer;

    public DocumentService(
        ILogger<DocumentService> logger,
        IDataExtractor dataExtractor,
        IFunctionPointCalculator functionPointCalculator,
        IChartGenerator chartGenerator,
        IPdfRenderer pdfRenderer)
    {
        _logger = logger;
        _dataExtractor = dataExtractor;
        _functionPointCalculator = functionPointCalculator;
        _chartGenerator = chartGenerator;
        _pdfRenderer = pdfRenderer;

        // Configure QuestPDF license
        QuestPDF.Settings.License = LicenseType.Community;
    }

    public async Task<string> GeneratePdfAsync(string outputPath, string dataPath)
    {
        try
        {
            _logger.LogInformation("Starting PDF generation...");

            // Validate source files
            var validation = await ValidateSourceFilesAsync(dataPath);
            if (!validation.IsValid)
            {
                throw new InvalidOperationException(
                    $"Source file validation failed. Missing files: {string.Join(", ", validation.MissingFiles)}");
            }

            // Load all data
            _logger.LogInformation("Loading data from source files...");
            var context = await LoadDataAsync(dataPath);
            context.OutputPath = outputPath;

            // Generate charts
            _logger.LogInformation("Generating charts...");
            await GenerateChartsAsync(context, outputPath);

            // Get document version
            var version = await GetDocumentVersionAsync(dataPath);
            context.Metadata.Version = version.Version;
            context.Metadata.CreatedDate = version.GeneratedDate;

            // Create all sections
            var sections = CreateSections();

            // Generate PDF document
            _logger.LogInformation("Rendering PDF document...");
            var pdfPath = Path.Combine(outputPath, $"migration-analysis-v{version.Version}.pdf");

            var document = Document.Create(container =>
            {
                container.Page(page =>
                {
                    page.Size(PageSizes.A4);
                    page.Margin(2, Unit.Centimetre);

                    // Render each section
                    foreach (var section in sections.OrderBy(s => s.Order))
                    {
                        _logger.LogDebug($"Rendering section: {section.Title}");
                        section.Render(page.Content(), context);
                    }
                });
            });

            // Generate PDF file
            document.GeneratePdf(pdfPath);

            _logger.LogInformation($"PDF generated successfully: {pdfPath}");
            return pdfPath;
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error generating PDF");
            throw new ApplicationException("Failed to generate PDF document. See inner exception for details.", ex);
        }
    }

    public async Task<ValidationResult> ValidateSourceFilesAsync(string dataPath)
    {
        var result = new ValidationResult { IsValid = true };

        // Check for required data files
        var requiredFiles = new[]
        {
            "function-points.json",
            "project-schedule.json",
            "cobol-metrics.json",
            "migration-architecture.json"
        };

        foreach (var file in requiredFiles)
        {
            var filePath = Path.Combine(dataPath, file);
            if (!File.Exists(filePath))
            {
                result.MissingFiles.Add(file);
                result.IsValid = false;
            }
        }

        // Check for optional files and add warnings
        var optionalFiles = new[]
        {
            "financial-analysis.json",
            "react-components.json"
        };

        foreach (var file in optionalFiles)
        {
            var filePath = Path.Combine(dataPath, file);
            if (!File.Exists(filePath))
            {
                result.Warnings.Add($"Optional file missing: {file}. Using default values.");
            }
        }

        return await Task.FromResult(result);
    }

    public async Task<DocumentVersion> GetDocumentVersionAsync(string dataPath)
    {
        var version = new DocumentVersion();

        // Calculate hash of source files to detect changes
        var sourceHash = await CalculateSourceHashAsync(dataPath);

        // Check version history file
        var versionFile = Path.Combine(dataPath, "..", "output", "pdf-versions.json");
        List<DocumentVersion>? versionHistory = null;

        if (File.Exists(versionFile))
        {
            var json = await File.ReadAllTextAsync(versionFile);
            versionHistory = JsonSerializer.Deserialize<List<DocumentVersion>>(json);
        }

        if (versionHistory != null && versionHistory.Any())
        {
            var lastVersion = versionHistory.Last();
            if (lastVersion.SourceHash != sourceHash)
            {
                // Source files have changed, increment version
                version.HasChanges = true;
                version.Version = IncrementVersion(lastVersion.Version);
            }
            else
            {
                version.Version = lastVersion.Version;
            }
        }

        version.SourceHash = sourceHash;
        return version;
    }

    private async Task<SectionContext> LoadDataAsync(string dataPath)
    {
        var context = new SectionContext();

        // Load COBOL metrics
        var cobolFile = Path.Combine(dataPath, "cobol-metrics.json");
        if (File.Exists(cobolFile))
        {
            var json = await File.ReadAllTextAsync(cobolFile);
            context.CobolMetrics = JsonSerializer.Deserialize<CobolMetrics>(json) ?? new CobolMetrics();
        }
        else
        {
            // Use default sample data
            context.CobolMetrics = CreateSampleCobolMetrics();
        }

        // Load migration architecture
        var archFile = Path.Combine(dataPath, "migration-architecture.json");
        if (File.Exists(archFile))
        {
            var json = await File.ReadAllTextAsync(archFile);
            context.Architecture = JsonSerializer.Deserialize<MigrationArchitecture>(json) ?? new MigrationArchitecture();
        }
        else
        {
            context.Architecture = CreateSampleArchitecture();
        }

        // Load function points
        var fpFile = Path.Combine(dataPath, "function-points.json");
        if (File.Exists(fpFile))
        {
            var json = await File.ReadAllTextAsync(fpFile);
            context.FunctionPoints = JsonSerializer.Deserialize<FunctionPoint[]>(json) ?? Array.Empty<FunctionPoint>();
        }
        else
        {
            context.FunctionPoints = CreateSampleFunctionPoints();
        }

        // Calculate financial analysis from function points
        context.Financial = _functionPointCalculator.CalculateFinancialAnalysis(
            context.FunctionPoints, 750m); // R$ 750 per function point

        // Load project schedule
        var scheduleFile = Path.Combine(dataPath, "project-schedule.json");
        if (File.Exists(scheduleFile))
        {
            var json = await File.ReadAllTextAsync(scheduleFile);
            context.Schedule = JsonSerializer.Deserialize<ProjectSchedule>(json) ?? new ProjectSchedule();
        }
        else
        {
            context.Schedule = CreateSampleSchedule();
        }

        // Set metadata
        context.Metadata = new DocumentMetadata
        {
            Title = "Análise de Migração - Sistema de Prêmios SUSEP",
            Author = "Caixa Seguradora",
            Subject = "Migração RG1866B COBOL para .NET 9 + React",
            CreatedDate = DateTime.Now
        };

        return context;
    }

    private async Task GenerateChartsAsync(SectionContext context, string outputPath)
    {
        var chartsPath = Path.Combine(outputPath, "charts");
        Directory.CreateDirectory(chartsPath);

        // Generate charts in parallel
        var tasks = new List<Task>();

        // Gantt chart
        tasks.Add(Task.Run(async () =>
        {
            var ganttData = new GanttChartData
            {
                Title = "Cronograma do Projeto",
                Phases = context.Schedule.Phases.Select(p => new GanttPhase
                {
                    Name = p.Name,
                    StartWeek = p.StartWeek,
                    EndWeek = p.EndWeek,
                    Color = GetPhaseColor(p.Name)
                }).ToList()
            };

            var ganttPath = Path.Combine(chartsPath, "gantt-chart.png");
            await _chartGenerator.GenerateGanttChartAsync(ganttData, ganttPath);
        }));

        // Pie chart for function points distribution
        tasks.Add(Task.Run(async () =>
        {
            var pieData = new PieChartData
            {
                Title = "Distribuição de Pontos de Função",
                Segments = context.FunctionPoints
                    .GroupBy(fp => fp.Type)
                    .Select(g => new PieSegment
                    {
                        Label = g.Key,
                        Value = g.Sum(fp => fp.AdjustedPoints),
                        Color = GetTypeColor(g.Key)
                    }).ToList()
            };

            var piePath = Path.Combine(chartsPath, "fp-distribution.png");
            await _chartGenerator.GeneratePieChartAsync(pieData, piePath);
        }));

        // Bar chart for cost breakdown
        tasks.Add(Task.Run(async () =>
        {
            var barData = new BarChartData
            {
                Title = "Distribuição de Custos",
                XAxisLabel = "Categoria",
                YAxisLabel = "Valor (R$)",
                Categories = context.Financial.CostBreakdown
                    .Select(cb => new BarCategory
                    {
                        Name = cb.Category,
                        Value = (double)cb.Amount,
                        Color = GetCostColor(cb.Category)
                    }).ToList()
            };

            var barPath = Path.Combine(chartsPath, "cost-breakdown.png");
            await _chartGenerator.GenerateBarChartAsync(barData, barPath);
        }));

        await Task.WhenAll(tasks);
    }

    private List<IPdfSection> CreateSections()
    {
        var sections = new List<IPdfSection>();

        // Create all sections in order
        sections.Add(new CoverPageSection());

        // TOC needs reference to other sections
        var tocSection = new TableOfContentsSection(sections);
        sections.Add(tocSection);

        sections.Add(new ExecutiveSummarySection());
        sections.Add(new CobolAnalysisSection());
        sections.Add(new MigrationArchitectureSection());
        sections.Add(new ComponentSpecsSection());
        sections.Add(new FunctionPointsSection());
        sections.Add(new FinancialAnalysisSection());
        sections.Add(new TimelineSection());
        sections.Add(new MethodologySection());

        return sections;
    }

    private async Task<string> CalculateSourceHashAsync(string dataPath)
    {
        var files = Directory.GetFiles(dataPath, "*.json")
            .OrderBy(f => f)
            .ToList();

        using var sha256 = SHA256.Create();
        var combinedHash = new StringBuilder();

        foreach (var file in files)
        {
            var content = await File.ReadAllBytesAsync(file);
            var hash = sha256.ComputeHash(content);
            combinedHash.Append(BitConverter.ToString(hash).Replace("-", ""));
        }

        return combinedHash.ToString().Substring(0, 16); // Use first 16 chars
    }

    private string IncrementVersion(string currentVersion)
    {
        var parts = currentVersion.Split('.');
        if (parts.Length != 3) return "1.0.0";

        if (int.TryParse(parts[2], out var patch))
        {
            return $"{parts[0]}.{parts[1]}.{patch + 1}";
        }

        return "1.0.0";
    }

    // Sample data creation methods
    private CobolMetrics CreateSampleCobolMetrics()
    {
        return new CobolMetrics
        {
            ProgramName = "RG1866B",
            TotalLines = 4876,
            TotalSections = 63,
            TotalParagraphs = 145,
            TotalDataItems = 687,
            DatabaseTables = 26,
            SqlCursors = 8,
            ComplexityLevel = "Alta",
            ProcessingType = "Batch",
            ExecutionFrequency = "Mensal"
        };
    }

    private MigrationArchitecture CreateSampleArchitecture()
    {
        return new MigrationArchitecture
        {
            BackendFramework = ".NET 9.0",
            FrontendFramework = "React 18",
            DatabaseType = "Azure SQL",
            ApiEndpoints = new List<ApiEndpoint>
            {
                new() { Category = "Reports", Path = "/api/v1/reports/generate", Method = "POST" },
                new() { Category = "Reports", Path = "/api/v1/reports/{id}", Method = "GET" },
                new() { Category = "Premiums", Path = "/api/v1/premiums/query", Method = "POST" },
                new() { Category = "Policies", Path = "/api/v1/policies/{id}", Method = "GET" }
            },
            ReactComponents = new List<ReactComponent>
            {
                new() { Name = "DashboardPage", Category = "Pages", FilePath = "pages/DashboardPage.tsx",
                        Props = new[] { "user", "metrics" }, LinesOfCode = 245 },
                new() { Name = "ReportGenerator", Category = "Dashboard", FilePath = "components/ReportGenerator.tsx",
                        Props = new[] { "onGenerate", "parameters" }, LinesOfCode = 189 }
            }
        };
    }

    private FunctionPoint[] CreateSampleFunctionPoints()
    {
        return new[]
        {
            new FunctionPoint { Name = "Generate PREMIT Report", Type = "External Output",
                               Complexity = "High", UnadjustedPoints = 7, AdjustedPoints = 10 },
            new FunctionPoint { Name = "Calculate Cossurance", Type = "External Inquiry",
                               Complexity = "High", UnadjustedPoints = 6, AdjustedPoints = 9 },
            new FunctionPoint { Name = "Premium Database", Type = "Internal Logical File",
                               Complexity = "High", UnadjustedPoints = 15, AdjustedPoints = 22 },
            new FunctionPoint { Name = "Policy Query", Type = "External Input",
                               Complexity = "Average", UnadjustedPoints = 4, AdjustedPoints = 6 },
            new FunctionPoint { Name = "SUSEP Interface", Type = "External Interface File",
                               Complexity = "Average", UnadjustedPoints = 7, AdjustedPoints = 10 }
        };
    }

    private ProjectSchedule CreateSampleSchedule()
    {
        return new ProjectSchedule
        {
            StartDate = DateTime.Now,
            EndDate = DateTime.Now.AddDays(84), // 12 weeks
            TotalWeeks = 12,
            Phases = new List<Phase>
            {
                new Phase
                {
                    Name = "Setup & Análise",
                    StartWeek = 1,
                    EndWeek = 2,
                    DurationWeeks = 2,
                    Description = "Configuração do ambiente e análise detalhada",
                    Tasks = new List<ProjectTask>
                    {
                        new() { Name = "Setup ambiente dev", DurationDays = 3, Status = "Completed" },
                        new() { Name = "Análise COBOL", DurationDays = 5, Status = "Completed" }
                    }
                },
                new Phase
                {
                    Name = "Backend Development",
                    StartWeek = 3,
                    EndWeek = 6,
                    DurationWeeks = 4,
                    Description = "Implementação da API .NET",
                    Tasks = new List<ProjectTask>
                    {
                        new() { Name = "Entities & DbContext", DurationDays = 5, Status = "InProgress" },
                        new() { Name = "Business Services", DurationDays = 10, Status = "Pending" }
                    }
                }
            },
            Milestones = new List<Milestone>
            {
                new() { Name = "Kickoff", Date = DateTime.Now,
                        AcceptanceCriteria = "Ambiente configurado e equipe alinhada" },
                new() { Name = "Backend Complete", Date = DateTime.Now.AddDays(42), // 6 weeks
                        AcceptanceCriteria = "API testada e documentada" },
                new() { Name = "Go-Live", Date = DateTime.Now.AddDays(84), // 12 weeks
                        AcceptanceCriteria = "Sistema em produção com suporte 24x7" }
            }
        };
    }

    private string GetPhaseColor(string phaseName)
    {
        return phaseName switch
        {
            var n when n.Contains("Setup") => "#0047BB",
            var n when n.Contains("Backend") => "#28a745",
            var n when n.Contains("Frontend") => "#6f42c1",
            _ => "#6c757d"
        };
    }

    private string GetTypeColor(string type)
    {
        return type switch
        {
            "External Input" => "#007bff",
            "External Output" => "#28a745",
            "External Inquiry" => "#ffc107",
            "Internal Logical File" => "#6f42c1",
            "External Interface File" => "#17a2b8",
            _ => "#6c757d"
        };
    }

    private string GetCostColor(string category)
    {
        return category switch
        {
            var c when c.Contains("Desenvolvimento") => "#0047BB",
            var c when c.Contains("Teste") => "#FFB81C",
            var c when c.Contains("Infraestrutura") => "#28a745",
            _ => "#6c757d"
        };
    }
}