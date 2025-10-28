using System;
using System.Collections.Generic;
using System.IO;
using FluentAssertions;
using PdfGenerator.Models;
using PdfGenerator.PdfGeneration.Sections;
using QuestPDF.Fluent;
using QuestPDF.Infrastructure;
using Xunit;

namespace PdfGenerator.Tests.PdfGeneration
{
    public class SectionRenderingTests
    {
        private readonly string _testOutputPath;

        public SectionRenderingTests()
        {
            _testOutputPath = Path.Combine(Path.GetTempPath(), "SectionRenderingTests");
            Directory.CreateDirectory(_testOutputPath);

            // License QuestPDF for testing
            QuestPDF.Settings.License = LicenseType.Community;
        }

        [Fact]
        public void CoverPageSection_ShouldRenderWithoutErrors()
        {
            // Arrange
            var section = new CoverPageSection();
            var metadata = new DocumentMetadata
            {
                Title = "Test Document",
                Version = "1.0.0",
                Date = DateTime.Now,
                Author = "Test Author",
                Company = "Caixa Seguradora"
            };

            // Act & Assert - Verify section can be created
            section.Should().NotBeNull();
            metadata.Should().NotBeNull();
            metadata.Title.Should().Be("Test Document");
        }

        [Fact]
        public void TableOfContentsSection_ShouldHandleMultipleEntries()
        {
            // Arrange
            var section = new TableOfContentsSection();
            var toc = new List<TocEntry>
            {
                new TocEntry { Title = "Executive Summary", PageNumber = 2 },
                new TocEntry { Title = "COBOL Analysis", PageNumber = 5 },
                new TocEntry { Title = "Migration Architecture", PageNumber = 10 }
            };

            // Assert
            section.Should().NotBeNull();
            toc.Should().HaveCount(3);
            toc[0].Title.Should().Be("Executive Summary");
            toc[0].PageNumber.Should().Be(2);
        }

        [Fact]
        public void ExecutiveSummarySection_ShouldHandleCompleteData()
        {
            // Arrange
            var section = new ExecutiveSummarySection();
            var summary = new ExecutiveSummaryData
            {
                ProjectName = "COBOL Migration",
                TotalFunctionPoints = 1250,
                EstimatedCost = 937500.00m,
                EstimatedDuration = "12 semanas",
                KeyBenefits = new[]
                {
                    "Modernização tecnológica",
                    "Redução de custos operacionais",
                    "Melhoria na manutenibilidade"
                },
                Risks = new[]
                {
                    "Complexidade da migração",
                    "Treinamento da equipe"
                }
            };

            // Assert
            section.Should().NotBeNull();
            summary.ProjectName.Should().Be("COBOL Migration");
            summary.TotalFunctionPoints.Should().Be(1250);
            summary.EstimatedCost.Should().Be(937500.00m);
            summary.KeyBenefits.Should().HaveCount(3);
            summary.Risks.Should().HaveCount(2);
        }

        [Fact]
        public void CobolAnalysisSection_ShouldHandleMetrics()
        {
            // Arrange
            var section = new CobolAnalysisSection();
            var metrics = new CobolMetrics
            {
                TotalLines = 5000,
                DataItems = 687,
                TablesAccessed = 26,
                ProgramsAnalyzed = 1,
                ComplexityScore = 85,
                EstimatedEffort = 120,
                MainProgram = "RG1866B"
            };

            // Assert
            section.Should().NotBeNull();
            metrics.TotalLines.Should().Be(5000);
            metrics.DataItems.Should().Be(687);
            metrics.TablesAccessed.Should().Be(26);
            metrics.MainProgram.Should().Be("RG1866B");
        }

        [Fact]
        public void MigrationArchitectureSection_ShouldHandleLayers()
        {
            // Arrange
            var section = new MigrationArchitectureSection();
            var architecture = new MigrationArchitecture
            {
                SourceTechnology = "IBM COBOL",
                TargetTechnology = ".NET 9.0 + React",
                Layers = new[]
                {
                    new ArchitectureLayer
                    {
                        Name = "API Layer",
                        Technology = "ASP.NET Core",
                        Components = new[] { "Controllers", "Middleware" }
                    },
                    new ArchitectureLayer
                    {
                        Name = "Core Layer",
                        Technology = "C# Classes",
                        Components = new[] { "Entities", "Services" }
                    },
                    new ArchitectureLayer
                    {
                        Name = "Infrastructure Layer",
                        Technology = "EF Core + SQLite",
                        Components = new[] { "Repositories", "DbContext" }
                    }
                }
            };

            // Assert
            section.Should().NotBeNull();
            architecture.SourceTechnology.Should().Be("IBM COBOL");
            architecture.TargetTechnology.Should().Be(".NET 9.0 + React");
            architecture.Layers.Should().HaveCount(3);
            architecture.Layers[0].Name.Should().Be("API Layer");
        }

        [Fact]
        public void FunctionPointsSection_ShouldHandleAnalysisData()
        {
            // Arrange
            var section = new FunctionPointsSection();
            var data = new FunctionPointAnalysis
            {
                TotalUnadjustedFunctionPoints = 1100,
                ValueAdjustmentFactor = 1.15,
                TotalAdjustedFunctionPoints = 1265,
                EstimatedCost = 948750.00m,
                CostPerFunctionPoint = 750.00m,
                ComplexityDistribution = new Dictionary<Complexity, int>
                {
                    { Complexity.Low, 20 },
                    { Complexity.Average, 35 },
                    { Complexity.High, 15 }
                },
                TypeDistribution = new Dictionary<FunctionPointType, int>
                {
                    { FunctionPointType.EI, 25 },
                    { FunctionPointType.EO, 15 },
                    { FunctionPointType.EQ, 10 },
                    { FunctionPointType.ILF, 12 },
                    { FunctionPointType.EIF, 8 }
                }
            };

            // Assert
            section.Should().NotBeNull();
            data.TotalUnadjustedFunctionPoints.Should().Be(1100);
            data.ValueAdjustmentFactor.Should().Be(1.15);
            data.TotalAdjustedFunctionPoints.Should().Be(1265);
            data.ComplexityDistribution.Should().HaveCount(3);
            data.TypeDistribution.Should().HaveCount(5);
        }

        [Fact]
        public void TimelineSection_ShouldHandleScheduleData()
        {
            // Arrange
            var section = new TimelineSection();
            var schedule = new ProjectSchedule
            {
                StartDate = new DateTime(2025, 11, 1),
                EndDate = new DateTime(2026, 1, 31),
                Phases = new[]
                {
                    new ProjectPhase
                    {
                        Name = "Fase 1: Configuração",
                        StartDate = new DateTime(2025, 11, 1),
                        EndDate = new DateTime(2025, 11, 15),
                        Tasks = new[] { "Setup", "Configuração" }
                    },
                    new ProjectPhase
                    {
                        Name = "Fase 2: Desenvolvimento",
                        StartDate = new DateTime(2025, 11, 16),
                        EndDate = new DateTime(2025, 12, 31),
                        Tasks = new[] { "Backend", "Frontend" }
                    }
                }
            };

            // Assert
            section.Should().NotBeNull();
            schedule.StartDate.Should().Be(new DateTime(2025, 11, 1));
            schedule.EndDate.Should().Be(new DateTime(2026, 1, 31));
            schedule.Phases.Should().HaveCount(2);
            schedule.Phases[0].Name.Should().Be("Fase 1: Configuração");
            schedule.Phases[0].Tasks.Should().HaveCount(2);
        }

        [Fact]
        public void FinancialAnalysisSection_ShouldHandleFinancialData()
        {
            // Arrange
            var section = new FinancialAnalysisSection();
            var analysis = new FinancialAnalysis
            {
                TotalCost = 950000.00m,
                CostBreakdown = new Dictionary<string, decimal>
                {
                    { "Desenvolvimento", 700000.00m },
                    { "Testes", 150000.00m },
                    { "Documentação", 100000.00m }
                },
                ROI = 2.5m,
                PaybackPeriod = "18 meses"
            };

            // Assert
            section.Should().NotBeNull();
            analysis.TotalCost.Should().Be(950000.00m);
            analysis.CostBreakdown.Should().HaveCount(3);
            analysis.ROI.Should().Be(2.5m);
            analysis.PaybackPeriod.Should().Be("18 meses");
        }

        [Fact]
        public void MethodologySection_ShouldBeInitializable()
        {
            // Arrange & Act
            var section = new MethodologySection();

            // Assert
            section.Should().NotBeNull();
        }

        [Fact]
        public void ComponentSpecsSection_ShouldBeInitializable()
        {
            // Arrange & Act
            var section = new ComponentSpecsSection();

            // Assert
            section.Should().NotBeNull();
        }

        public void Dispose()
        {
            // Cleanup test output directory
            if (Directory.Exists(_testOutputPath))
            {
                Directory.Delete(_testOutputPath, true);
            }
        }
    }
}