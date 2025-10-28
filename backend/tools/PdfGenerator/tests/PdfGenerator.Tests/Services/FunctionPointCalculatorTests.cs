using System.Collections.Generic;
using System.Linq;
using FluentAssertions;
using Microsoft.Extensions.Logging;
using Moq;
using PdfGenerator.Models;
using PdfGenerator.Services;
using Xunit;

namespace PdfGenerator.Tests.Services
{
    public class FunctionPointCalculatorTests
    {
        private readonly FunctionPointCalculator _calculator;
        private readonly Mock<ILogger<FunctionPointCalculator>> _loggerMock;

        public FunctionPointCalculatorTests()
        {
            _loggerMock = new Mock<ILogger<FunctionPointCalculator>>();
            _calculator = new FunctionPointCalculator(_loggerMock.Object);
        }

        [Fact]
        public void CalculateUnadjustedFunctionPoints_WithSingleEI_ShouldCalculateCorrectly()
        {
            // Arrange
            var functionPoints = new List<FunctionPoint>
            {
                new FunctionPoint
                {
                    Type = FunctionPointType.EI,
                    Name = "Create Premium Record",
                    DataElements = 10,
                    FilesReferenced = 2,
                    Complexity = Complexity.Average
                }
            };

            // Act
            var result = _calculator.CalculateUnadjustedFunctionPoints(functionPoints);

            // Assert
            result.Should().Be(4); // EI Average = 4 points
        }

        [Fact]
        public void CalculateUnadjustedFunctionPoints_WithMultipleTypes_ShouldCalculateCorrectly()
        {
            // Arrange
            var functionPoints = new List<FunctionPoint>
            {
                new FunctionPoint
                {
                    Type = FunctionPointType.EI,
                    Name = "Create Premium",
                    Complexity = Complexity.Low
                },
                new FunctionPoint
                {
                    Type = FunctionPointType.EO,
                    Name = "Generate PREMIT Report",
                    Complexity = Complexity.High
                },
                new FunctionPoint
                {
                    Type = FunctionPointType.EQ,
                    Name = "Query Policy",
                    Complexity = Complexity.Average
                },
                new FunctionPoint
                {
                    Type = FunctionPointType.ILF,
                    Name = "Premium Database",
                    Complexity = Complexity.High
                },
                new FunctionPoint
                {
                    Type = FunctionPointType.EIF,
                    Name = "Client Data",
                    Complexity = Complexity.Low
                }
            };

            // Act
            var result = _calculator.CalculateUnadjustedFunctionPoints(functionPoints);

            // Assert
            // EI Low: 3, EO High: 7, EQ Average: 4, ILF High: 15, EIF Low: 5
            result.Should().Be(3 + 7 + 4 + 15 + 5); // = 34
        }

        [Fact]
        public void CalculateValueAdjustmentFactor_WithDefaultFactors_ShouldReturn107()
        {
            // Arrange
            var factors = new GeneralSystemCharacteristics
            {
                DataCommunications = 3,
                DistributedDataProcessing = 3,
                Performance = 3,
                HeavilyUsedConfiguration = 3,
                TransactionRate = 3,
                OnlineDataEntry = 3,
                EndUserEfficiency = 3,
                OnlineUpdate = 3,
                ComplexProcessing = 3,
                Reusability = 3,
                InstallationEase = 3,
                OperationalEase = 3,
                MultipleSites = 3,
                FacilitateChange = 3
            };

            // Act
            var result = _calculator.CalculateValueAdjustmentFactor(factors);

            // Assert
            // Total DI = 14 * 3 = 42
            // VAF = (42 * 0.01) + 0.65 = 0.42 + 0.65 = 1.07
            result.Should().BeApproximately(1.07, 0.001);
        }

        [Fact]
        public void CalculateAdjustedFunctionPoints_ShouldMultiplyUFPByVAF()
        {
            // Arrange
            int ufp = 100;
            double vaf = 1.15;

            // Act
            var result = _calculator.CalculateAdjustedFunctionPoints(ufp, vaf);

            // Assert
            result.Should().Be(115);
        }

        [Fact]
        public void GenerateSummaryReport_ShouldReturnCompleteAnalysis()
        {
            // Arrange
            var functionPoints = new List<FunctionPoint>
            {
                new FunctionPoint { Type = FunctionPointType.EI, Complexity = Complexity.Low },
                new FunctionPoint { Type = FunctionPointType.EI, Complexity = Complexity.Average },
                new FunctionPoint { Type = FunctionPointType.EO, Complexity = Complexity.High },
                new FunctionPoint { Type = FunctionPointType.EQ, Complexity = Complexity.Low },
                new FunctionPoint { Type = FunctionPointType.ILF, Complexity = Complexity.High },
                new FunctionPoint { Type = FunctionPointType.EIF, Complexity = Complexity.Average }
            };

            var factors = new GeneralSystemCharacteristics
            {
                DataCommunications = 4,
                DistributedDataProcessing = 3,
                Performance = 5,
                ComplexProcessing = 4,
                Reusability = 3
            };

            // Act
            var report = _calculator.GenerateSummaryReport(functionPoints, factors, 750.00m);

            // Assert
            report.TotalUnadjustedFunctionPoints.Should().BeGreaterThan(0);
            report.ValueAdjustmentFactor.Should().BeGreaterThan(0);
            report.TotalAdjustedFunctionPoints.Should().BeGreaterThan(0);
            report.EstimatedCost.Should().BeGreaterThan(0);
            report.ComplexityDistribution.Should().NotBeEmpty();
            report.TypeDistribution.Should().NotBeEmpty();
        }

        [Fact]
        public void DetermineComplexity_ForEI_ShouldReturnCorrectComplexity()
        {
            // Act & Assert
            _calculator.DetermineComplexity(FunctionPointType.EI, 4, 1).Should().Be(Complexity.Low);
            _calculator.DetermineComplexity(FunctionPointType.EI, 10, 2).Should().Be(Complexity.Average);
            _calculator.DetermineComplexity(FunctionPointType.EI, 16, 3).Should().Be(Complexity.High);
        }

        [Fact]
        public void AnalyzeCobolMigration_ShouldGenerateRealisticMetrics()
        {
            // Arrange
            var cobolMetrics = new CobolMetrics
            {
                TotalLines = 5000,
                DataItems = 687,
                TablesAccessed = 26,
                ProgramsAnalyzed = 1,
                ComplexityScore = 85
            };

            // Act
            var functionPoints = _calculator.AnalyzeCobolMigration(cobolMetrics);

            // Assert
            functionPoints.Should().NotBeNull();
            functionPoints.Should().NotBeEmpty();

            // Should have various types of function points
            functionPoints.Any(fp => fp.Type == FunctionPointType.EO).Should().BeTrue();
            functionPoints.Any(fp => fp.Type == FunctionPointType.ILF).Should().BeTrue();
            functionPoints.Any(fp => fp.Type == FunctionPointType.EI).Should().BeTrue();

            // Should have PREMIT and PREMCED outputs
            functionPoints.Any(fp => fp.Name.Contains("PREMIT")).Should().BeTrue();
            functionPoints.Any(fp => fp.Name.Contains("PREMCED")).Should().BeTrue();
        }
    }
}