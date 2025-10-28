using System;
using System.CommandLine;
using System.CommandLine.Invocation;
using System.IO;
using System.Text.Json;
using System.Threading.Tasks;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using PdfGenerator.Services;
using PdfGenerator.PdfGeneration;
using Serilog;
using Serilog.Events;

namespace PdfGenerator;

class Program
{
    static async Task<int> Main(string[] args)
    {
        // Configure Serilog
        Log.Logger = new LoggerConfiguration()
            .MinimumLevel.Debug()
            .MinimumLevel.Override("Microsoft", LogEventLevel.Information)
            .Enrich.FromLogContext()
            .WriteTo.Console(outputTemplate: "[{Timestamp:HH:mm:ss} {Level:u3}] {Message:lj}{NewLine}{Exception}")
            .CreateLogger();

        try
        {
            // Create root command
            var rootCommand = new RootCommand("Caixa Seguradora COBOL Migration Analysis PDF Generator")
            {
                Description = "Generates comprehensive PDF documentation for COBOL to .NET migration project"
            };

            // Add options
            var outputOption = new Option<string>(
                aliases: new[] { "--output", "-o" },
                description: "Output directory for the generated PDF",
                getDefaultValue: () => "output");

            var dataOption = new Option<string>(
                aliases: new[] { "--data", "-d" },
                description: "Path to data files directory",
                getDefaultValue: () => "data");

            var versionOption = new Option<bool>(
                aliases: new[] { "--version", "-v" },
                description: "Show version information");

            var verboseOption = new Option<bool>(
                aliases: new[] { "--verbose" },
                description: "Enable verbose logging");

            var dryRunOption = new Option<bool>(
                aliases: new[] { "--dry-run" },
                description: "Validate inputs without generating PDF");

            var jsonOption = new Option<bool>(
                aliases: new[] { "--json" },
                description: "Output result in JSON format for CI/CD");

            var languageOption = new Option<string>(
                aliases: new[] { "--language", "-l" },
                description: "Language for PDF generation",
                getDefaultValue: () => "pt-BR")
                .FromAmong("pt-BR", "en");

            // Add options to command
            rootCommand.AddOption(outputOption);
            rootCommand.AddOption(dataOption);
            rootCommand.AddOption(versionOption);
            rootCommand.AddOption(verboseOption);
            rootCommand.AddOption(dryRunOption);
            rootCommand.AddOption(jsonOption);
            rootCommand.AddOption(languageOption);

            // Set handler
            rootCommand.SetHandler(async (context) =>
            {
                var output = context.ParseResult.GetValueForOption(outputOption)!;
                var data = context.ParseResult.GetValueForOption(dataOption)!;
                var showVersion = context.ParseResult.GetValueForOption(versionOption);
                var verbose = context.ParseResult.GetValueForOption(verboseOption);
                var dryRun = context.ParseResult.GetValueForOption(dryRunOption);
                var json = context.ParseResult.GetValueForOption(jsonOption);
                var language = context.ParseResult.GetValueForOption(languageOption)!;

                // Handle version flag
                if (showVersion)
                {
                    ShowVersion(json);
                    context.ExitCode = 0;
                    return;
                }

                // Configure logging level
                if (verbose)
                {
                    Log.Logger = new LoggerConfiguration()
                        .MinimumLevel.Verbose()
                        .WriteTo.Console(outputTemplate: "[{Timestamp:HH:mm:ss} {Level:u3}] {Message:lj}{NewLine}{Exception}")
                        .CreateLogger();
                }

                // Run generation
                context.ExitCode = await RunGenerationAsync(output, data, dryRun, json, language);
            });

            // Execute command
            return await rootCommand.InvokeAsync(args);
        }
        catch (Exception ex)
        {
            Log.Fatal(ex, "Application terminated unexpectedly");
            return 1;
        }
        finally
        {
            Log.CloseAndFlush();
        }
    }

    private static async Task<int> RunGenerationAsync(
        string outputPath,
        string dataPath,
        bool dryRun,
        bool jsonOutput,
        string language)
    {
        try
        {
            // Setup DI container
            var services = new ServiceCollection();
            ConfigureServices(services);

            var serviceProvider = services.BuildServiceProvider();
            var documentService = serviceProvider.GetRequiredService<IDocumentService>();

            // Create output directory
            Directory.CreateDirectory(outputPath);

            // Validate source files
            Log.Information("Validating source files...");
            var validation = await documentService.ValidateSourceFilesAsync(dataPath);

            if (!validation.IsValid)
            {
                if (jsonOutput)
                {
                    var errorResult = new
                    {
                        success = false,
                        error = "Source file validation failed",
                        missingFiles = validation.MissingFiles,
                        warnings = validation.Warnings
                    };
                    Console.WriteLine(JsonSerializer.Serialize(errorResult, new JsonSerializerOptions { WriteIndented = true }));
                }
                else
                {
                    Log.Error("Source file validation failed:");
                    foreach (var file in validation.MissingFiles)
                    {
                        Log.Error($"  - Missing: {file}");
                    }
                    foreach (var warning in validation.Warnings)
                    {
                        Log.Warning($"  - {warning}");
                    }
                }
                return 1;
            }

            // Show warnings if any
            foreach (var warning in validation.Warnings)
            {
                Log.Warning(warning);
            }

            if (dryRun)
            {
                if (jsonOutput)
                {
                    var dryRunResult = new
                    {
                        success = true,
                        message = "Dry run completed successfully",
                        warnings = validation.Warnings
                    };
                    Console.WriteLine(JsonSerializer.Serialize(dryRunResult, new JsonSerializerOptions { WriteIndented = true }));
                }
                else
                {
                    Log.Information("Dry run completed successfully. All validations passed.");
                }
                return 0;
            }

            // Generate PDF
            Log.Information($"Generating PDF document (Language: {language})...");
            var startTime = DateTime.Now;

            var pdfPath = await documentService.GeneratePdfAsync(outputPath, dataPath);

            var duration = DateTime.Now - startTime;

            // Get file info
            var fileInfo = new FileInfo(pdfPath);
            var fileSizeMB = fileInfo.Length / (1024.0 * 1024.0);

            if (jsonOutput)
            {
                var result = new
                {
                    success = true,
                    pdfPath = pdfPath,
                    fileSizeMB = Math.Round(fileSizeMB, 2),
                    generationTimeSeconds = Math.Round(duration.TotalSeconds, 2),
                    language = language,
                    timestamp = DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss")
                };
                Console.WriteLine(JsonSerializer.Serialize(result, new JsonSerializerOptions { WriteIndented = true }));
            }
            else
            {
                Log.Information("PDF generated successfully!");
                Log.Information($"  File: {pdfPath}");
                Log.Information($"  Size: {fileSizeMB:F2} MB");
                Log.Information($"  Time: {duration.TotalSeconds:F2} seconds");

                // Check size constraint
                if (fileSizeMB > 20)
                {
                    Log.Warning($"PDF size ({fileSizeMB:F2} MB) exceeds 20MB limit!");
                }

                // Check generation time
                if (duration.TotalSeconds > 60)
                {
                    Log.Warning($"Generation time ({duration.TotalSeconds:F2}s) exceeds 60 second target!");
                }
            }

            return 0;
        }
        catch (Exception ex)
        {
            if (jsonOutput)
            {
                var errorResult = new
                {
                    success = false,
                    error = ex.Message,
                    type = ex.GetType().Name
                };
                Console.WriteLine(JsonSerializer.Serialize(errorResult, new JsonSerializerOptions { WriteIndented = true }));
            }
            else
            {
                Log.Error(ex, "Failed to generate PDF");
            }
            return 1;
        }
    }

    private static void ConfigureServices(IServiceCollection services)
    {
        // Add logging
        services.AddLogging(builder =>
        {
            builder.ClearProviders();
            builder.AddSerilog();
        });

        // Register services
        services.AddSingleton<IDataExtractor, DataExtractor>();
        services.AddSingleton<IFunctionPointCalculator, FunctionPointCalculator>();
        services.AddSingleton<IChartGenerator, ChartGenerator>();
        services.AddSingleton<IPdfRenderer, QuestPdfRenderer>();
        services.AddSingleton<IDocumentService, DocumentService>();
    }

    private static void ShowVersion(bool json)
    {
        var version = typeof(Program).Assembly.GetName().Version?.ToString() ?? "1.0.0";
        var name = "Caixa Seguradora PDF Generator";
        var description = "COBOL Migration Analysis Document Generator";

        if (json)
        {
            var versionInfo = new
            {
                name = name,
                version = version,
                description = description,
                framework = "NET 9.0",
                build = DateTime.Now.ToString("yyyy-MM-dd")
            };
            Console.WriteLine(JsonSerializer.Serialize(versionInfo, new JsonSerializerOptions { WriteIndented = true }));
        }
        else
        {
            Console.WriteLine($"{name} v{version}");
            Console.WriteLine(description);
            Console.WriteLine();
            Console.WriteLine("Framework: .NET 9.0");
            Console.WriteLine($"Build Date: {DateTime.Now:yyyy-MM-dd}");
        }
    }
}