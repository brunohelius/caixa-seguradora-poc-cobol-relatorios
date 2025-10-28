using System;
using System.IO;
using System.Threading.Tasks;
using FluentAssertions;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using PdfGenerator.Services;
using PdfGenerator.PdfGeneration;
using Serilog;
using Xunit;

namespace PdfGenerator.Tests.Integration
{
    public class PdfGenerationIntegrationTests : IDisposable
    {
        private readonly ServiceProvider _serviceProvider;
        private readonly string _testDataPath;
        private readonly string _testOutputPath;
        private readonly IDocumentService _documentService;

        public PdfGenerationIntegrationTests()
        {
            // Setup test directories
            var baseTestPath = Path.Combine(Path.GetTempPath(), "PdfGeneratorIntegrationTests");
            _testDataPath = Path.Combine(baseTestPath, "data");
            _testOutputPath = Path.Combine(baseTestPath, "output");

            Directory.CreateDirectory(_testDataPath);
            Directory.CreateDirectory(_testOutputPath);

            // Configure Serilog for testing
            Log.Logger = new LoggerConfiguration()
                .MinimumLevel.Debug()
                .WriteTo.Console()
                .CreateLogger();

            // Setup DI container
            var services = new ServiceCollection();
            ConfigureServices(services);
            _serviceProvider = services.BuildServiceProvider();

            _documentService = _serviceProvider.GetRequiredService<IDocumentService>();

            // Create test data files
            CreateTestDataFiles();
        }

        private void ConfigureServices(IServiceCollection services)
        {
            services.AddLogging(builder =>
            {
                builder.ClearProviders();
                builder.AddSerilog();
            });

            services.AddSingleton<IDataExtractor, DataExtractor>();
            services.AddSingleton<IFunctionPointCalculator, FunctionPointCalculator>();
            services.AddSingleton<IChartGenerator, ChartGenerator>();
            services.AddSingleton<IPdfRenderer, QuestPdfRenderer>();
            services.AddSingleton<IDocumentService, DocumentService>();
        }

        private void CreateTestDataFiles()
        {
            // Create minimal test data files
            var cobolMetrics = @"{
                ""totalLines"": 5000,
                ""dataItems"": 687,
                ""tablesAccessed"": 26,
                ""programsAnalyzed"": 1,
                ""complexityScore"": 85,
                ""estimatedEffort"": 120,
                ""mainProgram"": ""RG1866B""
            }";
            File.WriteAllText(Path.Combine(_testDataPath, "cobol-metrics.json"), cobolMetrics);

            var functionPoints = @"{
                ""functionPoints"": [
                    {
                        ""type"": ""EO"",
                        ""name"": ""Generate PREMIT Report"",
                        ""dataElements"": 50,
                        ""filesReferenced"": 5,
                        ""complexity"": ""High""
                    },
                    {
                        ""type"": ""EO"",
                        ""name"": ""Generate PREMCED Report"",
                        ""dataElements"": 45,
                        ""filesReferenced"": 4,
                        ""complexity"": ""High""
                    },
                    {
                        ""type"": ""ILF"",
                        ""name"": ""Premium Database"",
                        ""dataElements"": 100,
                        ""recordTypes"": 10,
                        ""complexity"": ""High""
                    }
                ],
                ""generalSystemCharacteristics"": {
                    ""dataCommunications"": 4,
                    ""distributedDataProcessing"": 3,
                    ""performance"": 5,
                    ""heavilyUsedConfiguration"": 4,
                    ""transactionRate"": 4,
                    ""onlineDataEntry"": 3,
                    ""endUserEfficiency"": 4,
                    ""onlineUpdate"": 3,
                    ""complexProcessing"": 5,
                    ""reusability"": 3,
                    ""installationEase"": 3,
                    ""operationalEase"": 4,
                    ""multipleSites"": 2,
                    ""facilitateChange"": 4
                }
            }";
            File.WriteAllText(Path.Combine(_testDataPath, "function-points.json"), functionPoints);

            var architecture = @"{
                ""sourceTechnology"": ""IBM COBOL on z/OS"",
                ""targetTechnology"": "".NET 9.0 + React 18"",
                ""layers"": [
                    {
                        ""name"": ""API Layer"",
                        ""technology"": ""ASP.NET Core Web API"",
                        ""components"": [""Controllers"", ""Middleware"", ""Filters""]
                    },
                    {
                        ""name"": ""Core Layer"",
                        ""technology"": ""C# Domain Models"",
                        ""components"": [""Entities"", ""Interfaces"", ""Services""]
                    },
                    {
                        ""name"": ""Infrastructure Layer"",
                        ""technology"": ""Entity Framework Core + SQLite"",
                        ""components"": [""DbContext"", ""Repositories"", ""Migrations""]
                    }
                ]
            }";
            File.WriteAllText(Path.Combine(_testDataPath, "migration-architecture.json"), architecture);

            var schedule = @"{
                ""startDate"": ""2025-11-01"",
                ""endDate"": ""2026-01-31"",
                ""phases"": [
                    {
                        ""name"": ""Phase 1: Setup and Infrastructure"",
                        ""startDate"": ""2025-11-01"",
                        ""endDate"": ""2025-11-15"",
                        ""tasks"": [""Project setup"", ""Database design"", ""CI/CD pipeline""]
                    },
                    {
                        ""name"": ""Phase 2: Core Development"",
                        ""startDate"": ""2025-11-16"",
                        ""endDate"": ""2025-12-31"",
                        ""tasks"": [""Entity models"", ""Business logic"", ""API endpoints""]
                    },
                    {
                        ""name"": ""Phase 3: Testing and Deployment"",
                        ""startDate"": ""2026-01-01"",
                        ""endDate"": ""2026-01-31"",
                        ""tasks"": [""Integration testing"", ""Performance testing"", ""Production deployment""]
                    }
                ]
            }";
            File.WriteAllText(Path.Combine(_testDataPath, "project-schedule.json"), schedule);
        }

        [Fact]
        public async Task GeneratePdf_WithValidData_ShouldCreatePdfFile()
        {
            // Act
            var pdfPath = await _documentService.GeneratePdfAsync(_testOutputPath, _testDataPath);

            // Assert
            pdfPath.Should().NotBeNullOrEmpty();
            File.Exists(pdfPath).Should().BeTrue();

            var fileInfo = new FileInfo(pdfPath);
            fileInfo.Length.Should().BeGreaterThan(0);
            fileInfo.Length.Should().BeLessThan(20 * 1024 * 1024); // Less than 20MB
        }

        [Fact]
        public async Task ValidateSourceFiles_WithAllFiles_ShouldReturnValid()
        {
            // Act
            var validation = await _documentService.ValidateSourceFilesAsync(_testDataPath);

            // Assert
            validation.Should().NotBeNull();
            validation.IsValid.Should().BeTrue();
            validation.MissingFiles.Should().BeEmpty();
        }

        [Fact]
        public async Task ValidateSourceFiles_WithMissingFiles_ShouldReturnInvalid()
        {
            // Arrange - Delete one required file
            var functionPointsFile = Path.Combine(_testDataPath, "function-points.json");
            if (File.Exists(functionPointsFile))
            {
                File.Delete(functionPointsFile);
            }

            // Act
            var validation = await _documentService.ValidateSourceFilesAsync(_testDataPath);

            // Assert
            validation.Should().NotBeNull();
            validation.IsValid.Should().BeFalse();
            validation.MissingFiles.Should().NotBeEmpty();
            validation.MissingFiles.Should().Contain(file => file.Contains("function-points.json"));

            // Restore the file for other tests
            CreateTestDataFiles();
        }

        [Fact]
        public async Task GeneratePdf_ShouldCompleteWithinTimeLimit()
        {
            // Act
            var startTime = DateTime.Now;
            var pdfPath = await _documentService.GeneratePdfAsync(_testOutputPath, _testDataPath);
            var duration = DateTime.Now - startTime;

            // Assert
            pdfPath.Should().NotBeNullOrEmpty();
            duration.TotalSeconds.Should().BeLessThan(60); // Should complete within 60 seconds
        }

        [Fact]
        public async Task GeneratePdf_MultipleTimes_ShouldCreateUniqueFiles()
        {
            // Act - Generate PDF twice
            var pdfPath1 = await _documentService.GeneratePdfAsync(_testOutputPath, _testDataPath);
            await Task.Delay(1100); // Wait to ensure different timestamp
            var pdfPath2 = await _documentService.GeneratePdfAsync(_testOutputPath, _testDataPath);

            // Assert
            pdfPath1.Should().NotBeNullOrEmpty();
            pdfPath2.Should().NotBeNullOrEmpty();
            pdfPath1.Should().NotBe(pdfPath2);

            File.Exists(pdfPath1).Should().BeTrue();
            File.Exists(pdfPath2).Should().BeTrue();
        }

        [Fact]
        public void DataExtractor_ShouldLoadAllDataFiles()
        {
            // Arrange
            var extractor = _serviceProvider.GetRequiredService<IDataExtractor>();

            // Act
            var cobolMetrics = extractor.ExtractCobolMetrics(Path.Combine(_testDataPath, "cobol-metrics.json"));
            var functionPoints = extractor.ExtractFunctionPoints(Path.Combine(_testDataPath, "function-points.json"));
            var architecture = extractor.ExtractArchitecture(Path.Combine(_testDataPath, "migration-architecture.json"));
            var schedule = extractor.ExtractSchedule(Path.Combine(_testDataPath, "project-schedule.json"));

            // Assert
            cobolMetrics.Should().NotBeNull();
            cobolMetrics.TotalLines.Should().Be(5000);
            cobolMetrics.DataItems.Should().Be(687);

            functionPoints.Should().NotBeNull();
            functionPoints.FunctionPoints.Should().NotBeEmpty();

            architecture.Should().NotBeNull();
            architecture.Layers.Should().NotBeEmpty();

            schedule.Should().NotBeNull();
            schedule.Phases.Should().NotBeEmpty();
        }

        [Fact]
        public void FunctionPointCalculator_ShouldCalculateMetrics()
        {
            // Arrange
            var calculator = _serviceProvider.GetRequiredService<IFunctionPointCalculator>();
            var extractor = _serviceProvider.GetRequiredService<IDataExtractor>();
            var fpData = extractor.ExtractFunctionPoints(Path.Combine(_testDataPath, "function-points.json"));

            // Act
            var ufp = calculator.CalculateUnadjustedFunctionPoints(fpData.FunctionPoints);
            var vaf = calculator.CalculateValueAdjustmentFactor(fpData.GeneralSystemCharacteristics);
            var afp = calculator.CalculateAdjustedFunctionPoints(ufp, vaf);

            // Assert
            ufp.Should().BeGreaterThan(0);
            vaf.Should().BeGreaterThan(0.65).And.BeLessThanOrEqualTo(1.35);
            afp.Should().BeGreaterThan(0);
        }

        public void Dispose()
        {
            _serviceProvider?.Dispose();

            // Cleanup test directories
            var baseTestPath = Path.Combine(Path.GetTempPath(), "PdfGeneratorIntegrationTests");
            if (Directory.Exists(baseTestPath))
            {
                try
                {
                    Directory.Delete(baseTestPath, true);
                }
                catch
                {
                    // Ignore cleanup errors
                }
            }

            Log.CloseAndFlush();
        }
    }
}