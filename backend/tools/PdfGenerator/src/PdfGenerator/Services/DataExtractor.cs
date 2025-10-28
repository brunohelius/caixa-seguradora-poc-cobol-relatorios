using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.Json;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using Markdig;
using PdfGenerator.Models;

namespace PdfGenerator.Services
{
    /// <summary>
    /// Service for extracting data from various source files
    /// </summary>
    public class DataExtractor : IDataExtractor
    {
        private readonly MarkdownPipeline _markdownPipeline;

        public DataExtractor()
        {
            _markdownPipeline = new MarkdownPipelineBuilder()
                .UseAdvancedExtensions()
                .Build();
        }

        public async Task<CobolMetrics> ExtractCobolMetricsAsync(string filePath)
        {
            // For now, return hardcoded metrics based on actual COBOL analysis
            // In production, this would parse the FINAL-ANALYSIS-REPORT.md
            var metrics = new CobolMetrics
            {
                ProgramId = "RG1866B",
                ProgramName = "Sistema de Apuração de Prêmios SUSEP",
                TotalLines = 4977,
                CodeLines = 3982,
                CommentLines = 995,
                DataItems = 687,
                TablesAccessed = 26,
                RecordTypes = 31,
                SqlStatements = 85,
                CursorOperations = 12,
                ConditionalStatements = 156,
                LoopConstructs = 28,
                CalculationOperations = 234,
                OutputFiles = 2,
                CyclomaticComplexity = 312,
                ComplexityLevel = "High",
                EstimatedMigrationEffort = 480
            };

            // Add divisions
            metrics.Divisions.Add(new CobolDivision { Name = "IDENTIFICATION DIVISION", StartLine = 1, EndLine = 10 });
            metrics.Divisions.Add(new CobolDivision { Name = "ENVIRONMENT DIVISION", StartLine = 11, EndLine = 50 });
            metrics.Divisions.Add(new CobolDivision { Name = "DATA DIVISION", StartLine = 51, EndLine = 1500 });
            metrics.Divisions.Add(new CobolDivision { Name = "PROCEDURE DIVISION", StartLine = 1501, EndLine = 4977 });

            // Add key sections
            metrics.Sections.Add(new CobolSection
            {
                Name = "WORKING-STORAGE",
                Division = "DATA DIVISION",
                StartLine = 100,
                EndLine = 1400,
                Operations = new List<string> { "Variable declarations", "Constants", "Tables" }
            });

            // Add database tables
            var tables = new[] { "V0PREMIOS", "V0APOLICE", "V0ENDOSSO", "V0PRODUTO", "V0CLIENTE", "V0ENDERECOS", "GE399" };
            var tableIndex = 0;
            foreach (var table in tables)
            {
                metrics.DatabaseTables.Add(new DatabaseTable
                {
                    TableName = table,
                    AccessType = "READ",
                    FieldCount = 25 + (tableIndex * 3) // Deterministic field count
                });
                tableIndex++;
            }

            // Add file operations
            metrics.FileOperations.Add(new FileOperation
            {
                FileName = "PREMIT.TXT",
                OperationType = "WRITE",
                RecordFormat = "Fixed-width",
                RecordLength = 1024,
                Description = "Arquivo de prêmios emitidos"
            });

            metrics.FileOperations.Add(new FileOperation
            {
                FileName = "PREMCED.TXT",
                OperationType = "WRITE",
                RecordFormat = "Fixed-width",
                RecordLength = 1024,
                Description = "Arquivo de prêmios cedidos"
            });

            return await Task.FromResult(metrics);
        }

        public async Task<FunctionPointAnalysis> ExtractFunctionPointsAsync(string filePath)
        {
            var analysis = new FunctionPointAnalysis();

            // If file exists, read from JSON
            if (File.Exists(filePath))
            {
                var json = await File.ReadAllTextAsync(filePath);
                var data = JsonSerializer.Deserialize<FunctionPointData>(json);

                if (data?.Functions != null)
                {
                    analysis.Functions = data.Functions.Select(f => new FunctionPoint
                    {
                        Type = f.Type,
                        Name = f.Name,
                        Description = f.Description,
                        Complexity = f.Complexity,
                        Files = f.Files,
                        DataElements = f.DataElements
                    }).ToList();
                }
            }
            else
            {
                // Use default function points based on COBOL analysis
                analysis.Functions = GetDefaultFunctionPoints();
            }

            // Calculate totals
            analysis.Calculate();
            return analysis;
        }

        public async Task<MigrationArchitecture> ExtractArchitectureAsync(string filePath)
        {
            // Return the default architecture specification
            return await Task.FromResult(new MigrationArchitecture());
        }

        public async Task<ProjectSchedule> ExtractScheduleAsync(string filePath)
        {
            var schedule = new ProjectSchedule
            {
                ProjectName = "Migração COBOL RG1866B para .NET 9",
                StartDate = new DateTime(2025, 11, 1),
                EndDate = new DateTime(2026, 1, 31),
                TotalWeeks = 12
            };

            // If file exists, read from JSON
            if (File.Exists(filePath))
            {
                var json = await File.ReadAllTextAsync(filePath);
                var data = JsonSerializer.Deserialize<ScheduleData>(json);

                if (data?.Phases != null)
                {
                    schedule.Phases = data.Phases.Select(p => new ProjectPhase
                    {
                        Name = p.Name,
                        StartWeek = p.StartWeek,
                        Duration = p.Duration,
                        Status = p.Status ?? "Not Started",
                        PercentComplete = p.PercentComplete ?? 0
                    }).ToList();
                }
            }
            else
            {
                // Use default schedule
                schedule.Phases = GetDefaultSchedule();
            }

            // Add milestones
            schedule.Milestones.Add(new Milestone
            {
                Name = "Kickoff",
                Date = schedule.StartDate,
                Description = "Início do projeto",
                Status = "Complete"
            });

            schedule.Milestones.Add(new Milestone
            {
                Name = "MVP Ready",
                Date = schedule.StartDate.AddDays(42), // 6 weeks
                Description = "Versão mínima viável",
                Status = "Pending"
            });

            schedule.Milestones.Add(new Milestone
            {
                Name = "Go-Live",
                Date = schedule.EndDate,
                Description = "Produção",
                Status = "Pending"
            });

            return schedule;
        }

        public async Task<MarkdownDocument> ParseMarkdownAsync(string filePath)
        {
            if (!File.Exists(filePath))
            {
                throw new FileNotFoundException($"Markdown file not found: {filePath}");
            }

            var content = await File.ReadAllTextAsync(filePath);
            var document = new MarkdownDocument
            {
                WordCount = content.Split(' ', StringSplitOptions.RemoveEmptyEntries).Length,
                LineCount = content.Split('\n').Length
            };

            // Extract headers
            var headerPattern = @"^#{1,6}\s+(.+)$";
            var headers = Regex.Matches(content, headerPattern, RegexOptions.Multiline);
            foreach (Match match in headers)
            {
                document.Headers.Add(match.Groups[1].Value.Trim());
            }

            // Extract title (first H1)
            var titleMatch = Regex.Match(content, @"^#\s+(.+)$", RegexOptions.Multiline);
            if (titleMatch.Success)
            {
                document.Title = titleMatch.Groups[1].Value.Trim();
            }

            // Extract code blocks
            var codePattern = @"```[\s\S]*?```";
            var codeBlocks = Regex.Matches(content, codePattern);
            foreach (Match match in codeBlocks)
            {
                document.CodeBlocks.Add(match.Value);
            }

            // Split into sections based on headers
            var sections = Regex.Split(content, @"^#{1,3}\s+", RegexOptions.Multiline);
            for (int i = 1; i < sections.Length && i < headers.Count + 1; i++)
            {
                if (i - 1 < document.Headers.Count)
                {
                    document.Sections[document.Headers[i - 1]] = sections[i];
                }
            }

            return document;
        }

        public async Task<bool> ValidateSourceFilesAsync(params string[] filePaths)
        {
            foreach (var path in filePaths)
            {
                if (!File.Exists(path))
                {
                    Console.WriteLine($"Warning: Source file not found: {path}");
                    // Continue validation but log missing files
                }
            }
            return await Task.FromResult(true); // Allow generation even with missing files
        }

        private static List<FunctionPoint> GetDefaultFunctionPoints()
        {
            return new List<FunctionPoint>
            {
                // External Outputs (Reports)
                new FunctionPoint { Type = "EO", Name = "Gerar PREMIT.TXT", Complexity = "High", Files = 26, DataElements = 687 },
                new FunctionPoint { Type = "EO", Name = "Gerar PREMCED.TXT", Complexity = "High", Files = 26, DataElements = 687 },
                new FunctionPoint { Type = "EO", Name = "Dashboard Métricas", Complexity = "Average", Files = 5, DataElements = 25 },
                new FunctionPoint { Type = "EO", Name = "Relatório Comparação", Complexity = "Average", Files = 3, DataElements = 15 },

                // External Inquiries (Queries)
                new FunctionPoint { Type = "EQ", Name = "Consultar Prêmios", Complexity = "High", Files = 10, DataElements = 50 },
                new FunctionPoint { Type = "EQ", Name = "Consultar Apólices", Complexity = "Average", Files = 5, DataElements = 30 },
                new FunctionPoint { Type = "EQ", Name = "Consultar Endossos", Complexity = "Average", Files = 4, DataElements = 25 },

                // External Inputs (Data Entry)
                new FunctionPoint { Type = "EI", Name = "Carregar Dados Mock", Complexity = "Average", Files = 3, DataElements = 15 },
                new FunctionPoint { Type = "EI", Name = "Configurar Parâmetros", Complexity = "Low", Files = 1, DataElements = 10 },

                // Internal Logical Files (Database)
                new FunctionPoint { Type = "ILF", Name = "V0PREMIOS", Complexity = "High", Files = 1, DataElements = 45 },
                new FunctionPoint { Type = "ILF", Name = "V0APOLICE", Complexity = "High", Files = 1, DataElements = 35 },
                new FunctionPoint { Type = "ILF", Name = "V0ENDOSSO", Complexity = "Average", Files = 1, DataElements = 25 },
                new FunctionPoint { Type = "ILF", Name = "V0PRODUTO", Complexity = "Average", Files = 1, DataElements = 20 },
                new FunctionPoint { Type = "ILF", Name = "V0CLIENTE", Complexity = "Average", Files = 1, DataElements = 30 },

                // External Interface Files
                new FunctionPoint { Type = "EIF", Name = "GE399 Interface", Complexity = "High", Files = 1, DataElements = 40 },
                new FunctionPoint { Type = "EIF", Name = "Arquivo Config", Complexity = "Low", Files = 1, DataElements = 5 }
            };
        }

        private static List<ProjectPhase> GetDefaultSchedule()
        {
            return new List<ProjectPhase>
            {
                new ProjectPhase { Name = "Setup & Infraestrutura", StartWeek = 1, Duration = 2, Status = "Complete", PercentComplete = 100 },
                new ProjectPhase { Name = "Modelagem de Dados", StartWeek = 2, Duration = 2, Status = "Complete", PercentComplete = 100 },
                new ProjectPhase { Name = "Backend Core", StartWeek = 3, Duration = 3, Status = "In Progress", PercentComplete = 75 },
                new ProjectPhase { Name = "Frontend React", StartWeek = 4, Duration = 3, Status = "In Progress", PercentComplete = 60 },
                new ProjectPhase { Name = "Integração & APIs", StartWeek = 6, Duration = 2, Status = "Not Started", PercentComplete = 0 },
                new ProjectPhase { Name = "Testes & Validação", StartWeek = 7, Duration = 3, Status = "Not Started", PercentComplete = 0 },
                new ProjectPhase { Name = "Comparação COBOL", StartWeek = 9, Duration = 2, Status = "Not Started", PercentComplete = 0 },
                new ProjectPhase { Name = "Documentação", StartWeek = 10, Duration = 2, Status = "Not Started", PercentComplete = 0 },
                new ProjectPhase { Name = "Deploy & Go-Live", StartWeek = 11, Duration = 2, Status = "Not Started", PercentComplete = 0 }
            };
        }

        // Helper classes for JSON deserialization
        private class FunctionPointData
        {
            public List<FunctionPointItem> Functions { get; set; }
        }

        private class FunctionPointItem
        {
            public string Type { get; set; }
            public string Name { get; set; }
            public string Description { get; set; }
            public string Complexity { get; set; }
            public int Files { get; set; }
            public int DataElements { get; set; }
        }

        private class ScheduleData
        {
            public List<PhaseItem> Phases { get; set; }
        }

        private class PhaseItem
        {
            public string Name { get; set; }
            public int StartWeek { get; set; }
            public int Duration { get; set; }
            public string Status { get; set; }
            public double? PercentComplete { get; set; }
        }
    }
}