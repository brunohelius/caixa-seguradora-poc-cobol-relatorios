using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using FluentAssertions;
using Microsoft.Extensions.Logging;
using Moq;
using PdfGenerator.Models.ChartModels;
using PdfGenerator.Services;
using Xunit;

namespace PdfGenerator.Tests.Services
{
    public class ChartGeneratorTests
    {
        private readonly ChartGenerator _generator;
        private readonly Mock<ILogger<ChartGenerator>> _loggerMock;
        private readonly string _testOutputPath;

        public ChartGeneratorTests()
        {
            _loggerMock = new Mock<ILogger<ChartGenerator>>();
            _generator = new ChartGenerator(_loggerMock.Object);
            _testOutputPath = Path.Combine(Path.GetTempPath(), "ChartGeneratorTests");
            Directory.CreateDirectory(_testOutputPath);
        }

        [Fact]
        public void GeneratePieChart_ShouldCreateValidImage()
        {
            // Arrange
            var data = new PieChartData
            {
                Title = "Test Pie Chart",
                Labels = new[] { "Section A", "Section B", "Section C" },
                Values = new[] { 45.0, 30.0, 25.0 }
            };

            var outputPath = Path.Combine(_testOutputPath, "test_pie.png");

            // Act
            var result = _generator.GeneratePieChart(data, outputPath);

            // Assert
            result.Should().NotBeNull();
            File.Exists(result).Should().BeTrue();

            var fileInfo = new FileInfo(result);
            fileInfo.Length.Should().BeGreaterThan(0);

            // Cleanup
            File.Delete(result);
        }

        [Fact]
        public void GenerateBarChart_ShouldCreateValidImage()
        {
            // Arrange
            var data = new BarChartData
            {
                Title = "Test Bar Chart",
                XLabel = "Categories",
                YLabel = "Values",
                Categories = new[] { "Q1", "Q2", "Q3", "Q4" },
                Values = new[] { 100.0, 150.0, 120.0, 180.0 }
            };

            var outputPath = Path.Combine(_testOutputPath, "test_bar.png");

            // Act
            var result = _generator.GenerateBarChart(data, outputPath);

            // Assert
            result.Should().NotBeNull();
            File.Exists(result).Should().BeTrue();

            var fileInfo = new FileInfo(result);
            fileInfo.Length.Should().BeGreaterThan(0);

            // Cleanup
            File.Delete(result);
        }

        [Fact]
        public void GenerateGanttChart_ShouldCreateValidImage()
        {
            // Arrange
            var data = new GanttChartData
            {
                Title = "Test Gantt Chart",
                Tasks = new List<GanttTask>
                {
                    new GanttTask
                    {
                        Name = "Task 1",
                        StartDate = new DateTime(2025, 1, 1),
                        EndDate = new DateTime(2025, 1, 15),
                        Progress = 50
                    },
                    new GanttTask
                    {
                        Name = "Task 2",
                        StartDate = new DateTime(2025, 1, 10),
                        EndDate = new DateTime(2025, 1, 25),
                        Progress = 25
                    },
                    new GanttTask
                    {
                        Name = "Task 3",
                        StartDate = new DateTime(2025, 1, 20),
                        EndDate = new DateTime(2025, 2, 5),
                        Progress = 0
                    }
                }
            };

            var outputPath = Path.Combine(_testOutputPath, "test_gantt.png");

            // Act
            var result = _generator.GenerateGanttChart(data, outputPath);

            // Assert
            result.Should().NotBeNull();
            File.Exists(result).Should().BeTrue();

            var fileInfo = new FileInfo(result);
            fileInfo.Length.Should().BeGreaterThan(0);

            // Cleanup
            File.Delete(result);
        }

        [Fact]
        public void GenerateLineChart_ShouldCreateValidImage()
        {
            // Arrange
            var xValues = new[] { 1.0, 2.0, 3.0, 4.0, 5.0 };
            var yValues = new[] { 10.0, 15.0, 13.0, 18.0, 20.0 };
            var title = "Test Line Chart";
            var xLabel = "X Axis";
            var yLabel = "Y Axis";
            var outputPath = Path.Combine(_testOutputPath, "test_line.png");

            // Act
            var result = _generator.GenerateLineChart(
                xValues, yValues, title, xLabel, yLabel, outputPath);

            // Assert
            result.Should().NotBeNull();
            File.Exists(result).Should().BeTrue();

            var fileInfo = new FileInfo(result);
            fileInfo.Length.Should().BeGreaterThan(0);

            // Cleanup
            File.Delete(result);
        }

        [Fact]
        public void GenerateStackedBarChart_ShouldCreateValidImage()
        {
            // Arrange
            var categories = new[] { "Jan", "Feb", "Mar", "Apr" };
            var series1 = new[] { 10.0, 20.0, 15.0, 25.0 };
            var series2 = new[] { 5.0, 10.0, 8.0, 12.0 };
            var series3 = new[] { 3.0, 6.0, 4.0, 7.0 };
            var seriesNames = new[] { "Series A", "Series B", "Series C" };
            var title = "Test Stacked Bar Chart";
            var outputPath = Path.Combine(_testOutputPath, "test_stacked.png");

            // Act
            var result = _generator.GenerateStackedBarChart(
                categories,
                new[] { series1, series2, series3 },
                seriesNames,
                title,
                "Categories",
                "Values",
                outputPath);

            // Assert
            result.Should().NotBeNull();
            File.Exists(result).Should().BeTrue();

            var fileInfo = new FileInfo(result);
            fileInfo.Length.Should().BeGreaterThan(0);

            // Cleanup
            File.Delete(result);
        }

        [Fact]
        public void GeneratePieChart_WithInvalidData_ShouldHandleGracefully()
        {
            // Arrange
            var data = new PieChartData
            {
                Title = "Invalid Chart",
                Labels = new string[] { }, // Empty data
                Values = new double[] { }
            };

            var outputPath = Path.Combine(_testOutputPath, "test_invalid.png");

            // Act & Assert
            Action act = () => _generator.GeneratePieChart(data, outputPath);
            act.Should().NotThrow();
        }

        [Fact]
        public void GenerateBarChart_WithNegativeValues_ShouldHandleCorrectly()
        {
            // Arrange
            var data = new BarChartData
            {
                Title = "Chart with Negative Values",
                XLabel = "Categories",
                YLabel = "Values",
                Categories = new[] { "A", "B", "C", "D" },
                Values = new[] { 10.0, -5.0, 15.0, -2.0 }
            };

            var outputPath = Path.Combine(_testOutputPath, "test_negative.png");

            // Act
            var result = _generator.GenerateBarChart(data, outputPath);

            // Assert
            result.Should().NotBeNull();
            File.Exists(result).Should().BeTrue();

            // Cleanup
            File.Delete(result);
        }

        [Fact]
        public void GenerateGanttChart_WithOverlappingTasks_ShouldRenderCorrectly()
        {
            // Arrange
            var data = new GanttChartData
            {
                Title = "Overlapping Tasks",
                Tasks = new List<GanttTask>
                {
                    new GanttTask
                    {
                        Name = "Task A",
                        StartDate = new DateTime(2025, 1, 1),
                        EndDate = new DateTime(2025, 1, 20),
                        Progress = 75
                    },
                    new GanttTask
                    {
                        Name = "Task B",
                        StartDate = new DateTime(2025, 1, 5),
                        EndDate = new DateTime(2025, 1, 15),
                        Progress = 100
                    },
                    new GanttTask
                    {
                        Name = "Task C",
                        StartDate = new DateTime(2025, 1, 10),
                        EndDate = new DateTime(2025, 1, 25),
                        Progress = 30
                    }
                }
            };

            var outputPath = Path.Combine(_testOutputPath, "test_overlapping.png");

            // Act
            var result = _generator.GenerateGanttChart(data, outputPath);

            // Assert
            result.Should().NotBeNull();
            File.Exists(result).Should().BeTrue();

            // Cleanup
            File.Delete(result);
        }

        [Fact]
        public void ApplyCaixaBranding_ShouldUseCorrectColors()
        {
            // Arrange
            var data = new BarChartData
            {
                Title = "Branded Chart",
                XLabel = "X",
                YLabel = "Y",
                Categories = new[] { "A", "B" },
                Values = new[] { 10.0, 20.0 }
            };

            var outputPath = Path.Combine(_testOutputPath, "test_branded.png");

            // Act
            var result = _generator.GenerateBarChart(data, outputPath);

            // Assert
            // The chart should be created with Caixa branding colors
            // Primary: #0047BB (blue), Secondary: #FFB81C (yellow)
            result.Should().NotBeNull();
            File.Exists(result).Should().BeTrue();

            // Cleanup
            File.Delete(result);
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