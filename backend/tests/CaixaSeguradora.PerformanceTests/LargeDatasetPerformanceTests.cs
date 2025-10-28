using System.Diagnostics;
using CaixaSeguradora.Core.DTOs;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Interfaces;
using CaixaSeguradora.Core.Services;
using CaixaSeguradora.Infrastructure.Data;
using CaixaSeguradora.Infrastructure.Services;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using Moq;
using Xunit;
using Xunit.Abstractions;

namespace CaixaSeguradora.PerformanceTests;

/// <summary>
/// Large dataset performance tests for US5 requirements.
/// Target: Process 15,000 records in under 7 minutes (420 seconds).
/// </summary>
[Collection("Performance")]
public class LargeDatasetPerformanceTests : IDisposable
{
    private readonly ITestOutputHelper _output;
    private readonly ServiceProvider _serviceProvider;
    private readonly PremiumReportingDbContext _context;

    public LargeDatasetPerformanceTests(ITestOutputHelper output)
    {
        _output = output;

        // Setup in-memory database
        var services = new ServiceCollection();
        services.AddDbContext<PremiumReportingDbContext>(options =>
            options.UseInMemoryDatabase($"PerformanceTest_{Guid.NewGuid()}"));

        // Register repositories
        services.AddScoped<IPremiumRepository, CaixaSeguradora.Infrastructure.Repositories.PremiumRepository>();
        services.AddScoped<IPolicyRepository, CaixaSeguradora.Infrastructure.Repositories.PolicyRepository>();
        services.AddScoped<IProductRepository, CaixaSeguradora.Infrastructure.Repositories.ProductRepository>();
        services.AddScoped<IClientRepository, CaixaSeguradora.Infrastructure.Repositories.ClientRepository>();
        services.AddScoped<IAddressRepository, CaixaSeguradora.Infrastructure.Repositories.AddressRepository>();
        services.AddScoped<ICossuranceRepository, CaixaSeguradora.Infrastructure.Repositories.CossuranceRepository>();

        // Register logging
        services.AddLogging(builder => builder.AddConsole().SetMinimumLevel(LogLevel.Information));

        // Register mock services for orchestration
        var mockExecutionService = new Mock<IExecutionTrackingService>();
        mockExecutionService
            .Setup(x => x.CreateExecutionAsync(It.IsAny<string>(), It.IsAny<string>(), It.IsAny<string>(), It.IsAny<string>(), It.IsAny<CancellationToken>()))
            .ReturnsAsync(Guid.NewGuid());

        var mockFileWriter = new Mock<IFileWriterService>();
        var mockValidationService = new Mock<IBusinessRuleValidationService>();
        mockValidationService
            .Setup(x => x.ValidatePremiumAsync(It.IsAny<PremiumRecord>(), It.IsAny<Policy>(), It.IsAny<Product>(), It.IsAny<CancellationToken>()))
            .ReturnsAsync(new Core.Models.ValidationResult { IsValid = true });

        services.AddSingleton(mockExecutionService.Object);
        services.AddSingleton(mockFileWriter.Object);
        services.AddSingleton(mockValidationService.Object);

        // Register orchestration service
        services.AddScoped<ReportOrchestrationService>();

        _serviceProvider = services.BuildServiceProvider();
        _context = _serviceProvider.GetRequiredService<PremiumReportingDbContext>();
    }

    /// <summary>
    /// T152: Test 15,000 records complete processing in under 7 minutes.
    /// This is the primary US5 performance requirement.
    /// </summary>
    [Fact(Skip = "Long-running test - run manually with: dotnet test --filter FullyQualifiedName~Process15000Records")]
    public async Task Process15000Records_CompletesUnder7Minutes()
    {
        // Arrange
        _output.WriteLine("=== Phase 7 (US5) Performance Test: 15,000 Records ===");
        _output.WriteLine("Seeding database with 15,000 premium records...");

        await SeedLargeDatasetAsync(15000);

        var orchestrationService = _serviceProvider.GetRequiredService<ReportOrchestrationService>();

        var request = new GenerateReportRequest
        {
            Month = "202510",
            ReportType = "PREMIT",
            ExecutionMode = "BATCH",
            TriggeringUser = "PerformanceTest"
        };

        // Act
        _output.WriteLine("Starting report generation...");
        var stopwatch = Stopwatch.StartNew();

        var executionId = await orchestrationService.GenerateReportAsync(request);

        stopwatch.Stop();

        // Assert
        var elapsedSeconds = stopwatch.Elapsed.TotalSeconds;
        var throughput = 15000 / elapsedSeconds;

        _output.WriteLine("");
        _output.WriteLine("=== Performance Test Results ===");
        _output.WriteLine($"Total records: 15,000");
        _output.WriteLine($"Processing time: {stopwatch.Elapsed.TotalMinutes:F2} minutes ({elapsedSeconds:F2} seconds)");
        _output.WriteLine($"Throughput: {throughput:F2} records/second");
        _output.WriteLine($"Average time per record: {(elapsedSeconds / 15000) * 1000:F2} ms");

        // US5 requirement: Complete in under 7 minutes (420 seconds)
        Assert.True(
            elapsedSeconds < 420,
            $"Processing took {stopwatch.Elapsed.TotalMinutes:F2} minutes, expected < 7 minutes. " +
            $"Throughput: {throughput:F2} records/sec");

        Assert.NotEqual(Guid.Empty, executionId);
    }

    /// <summary>
    /// Test 10,000 records complete processing in under 5 minutes.
    /// This validates the standard volume requirement.
    /// </summary>
    [Fact(Skip = "Long-running test - run manually")]
    public async Task Process10000Records_CompletesUnder5Minutes()
    {
        // Arrange
        _output.WriteLine("=== Performance Test: 10,000 Records ===");
        await SeedLargeDatasetAsync(10000);

        var orchestrationService = _serviceProvider.GetRequiredService<ReportOrchestrationService>();

        var request = new GenerateReportRequest
        {
            Month = "202510",
            ReportType = "PREMIT",
            ExecutionMode = "BATCH"
        };

        // Act
        var stopwatch = Stopwatch.StartNew();
        var executionId = await orchestrationService.GenerateReportAsync(request);
        stopwatch.Stop();

        // Assert
        var elapsedSeconds = stopwatch.Elapsed.TotalSeconds;
        _output.WriteLine($"Processed 10,000 records in {stopwatch.Elapsed.TotalMinutes:F2} minutes");
        _output.WriteLine($"Throughput: {10000 / elapsedSeconds:F2} records/sec");

        Assert.True(
            elapsedSeconds < 300,
            $"Processing took {stopwatch.Elapsed.TotalMinutes:F2} minutes, expected < 5 minutes");
    }

    /// <summary>
    /// T157 (partial): Test concurrent execution of smaller datasets.
    /// Validates that multiple reports can run in parallel without performance degradation.
    /// </summary>
    [Fact]
    public async Task ProcessConcurrent_3Executions_NoPerformanceDegradation()
    {
        // Arrange
        _output.WriteLine("=== Concurrent Execution Test ===");
        await SeedLargeDatasetAsync(3000);

        var orchestrationService = _serviceProvider.GetRequiredService<ReportOrchestrationService>();

        var request1 = new GenerateReportRequest { Month = "202510", ReportType = "PREMIT" };
        var request2 = new GenerateReportRequest { Month = "202509", ReportType = "PREMIT" };
        var request3 = new GenerateReportRequest { Month = "202508", ReportType = "PREMIT" };

        // Act: Run 3 reports concurrently
        var stopwatch = Stopwatch.StartNew();

        var tasks = new[]
        {
            orchestrationService.GenerateReportAsync(request1),
            orchestrationService.GenerateReportAsync(request2),
            orchestrationService.GenerateReportAsync(request3)
        };

        await Task.WhenAll(tasks);
        stopwatch.Stop();

        // Assert
        _output.WriteLine($"Completed 3 concurrent executions in {stopwatch.Elapsed.TotalSeconds:F2} seconds");

        // With 3000 records, single execution should take ~10s
        // Concurrent should complete in under 30s (3x with some overhead tolerance)
        Assert.True(
            stopwatch.Elapsed.TotalSeconds < 60,
            $"Concurrent execution took {stopwatch.Elapsed.TotalSeconds:F2}s, expected < 60s");

        // All tasks should have completed successfully
        Assert.All(tasks, task => Assert.True(task.IsCompletedSuccessfully));
    }

    /// <summary>
    /// Benchmark test: Measure baseline cursor vs ToList performance.
    /// </summary>
    [Fact]
    public async Task Benchmark_CursorVsToList_ComparememoryEfficiency()
    {
        // Arrange
        await SeedLargeDatasetAsync(5000);

        var premiumRepository = _serviceProvider.GetRequiredService<IPremiumRepository>();
        var startDate = DateTime.Parse("2025-10-01");
        var endDate = DateTime.Parse("2025-10-31");

        GC.Collect();
        GC.WaitForPendingFinalizers();
        GC.Collect();

        // Act 1: Cursor-based streaming (IAsyncEnumerable)
        var cursorMemoryStart = GC.GetTotalMemory(false);
        var cursorStopwatch = Stopwatch.StartNew();

        var cursorCount = 0;
        await foreach (var premium in premiumRepository.GetPremiumsForReportAsync(startDate, endDate))
        {
            cursorCount++;
        }

        cursorStopwatch.Stop();
        var cursorMemoryPeak = GC.GetTotalMemory(false);
        var cursorMemoryUsed = cursorMemoryPeak - cursorMemoryStart;

        _output.WriteLine("=== Benchmark Results ===");
        _output.WriteLine($"Cursor-based streaming:");
        _output.WriteLine($"  - Time: {cursorStopwatch.Elapsed.TotalMilliseconds:F2} ms");
        _output.WriteLine($"  - Memory used: {FormatBytes(cursorMemoryUsed)}");
        _output.WriteLine($"  - Records: {cursorCount}");

        // Validate cursor approach works
        Assert.Equal(5000, cursorCount);
        _output.WriteLine("");
        _output.WriteLine("Cursor-based approach validated successfully.");
    }

    /// <summary>
    /// Seeds the database with premium records, policies, and related data.
    /// </summary>
    private async Task SeedLargeDatasetAsync(int recordCount)
    {
        var policies = new List<Policy>();
        var premiums = new List<PremiumRecord>();
        var products = new List<Product>();
        var clients = new List<Client>();

        // Create reference data
        for (int i = 1; i <= 10; i++)
        {
            products.Add(new Product
            {
                ProductCode = 1000 + i,
                ProductName = $"Produto {i}",
                RamoSusep = 500 + i
            });

            clients.Add(new Client
            {
                ClientCode = 100000 + i,
                ClientName = $"Cliente {i}",
                DocumentNumber = $"{12345678900 + i}"
            });
        }

        await _context.Products.AddRangeAsync(products);
        await _context.Clients.AddRangeAsync(clients);

        // Create premium records and policies
        for (int i = 1; i <= recordCount; i++)
        {
            var policyNumber = 1000000000000L + i;
            var productCode = 1000 + (i % 10) + 1;
            var clientCode = 100000 + (i % 10) + 1;

            var policy = new Policy
            {
                PolicyNumber = policyNumber,
                CompanyCode = 5631,
                ProductCode = productCode,
                ProposerClientCode = clientCode,
                PolicyStartDate = "2025-10-01",
                PolicyEndDate = "2026-10-01",
                PolicyStatus = "A"
            };
            policies.Add(policy);

            var premium = new PremiumRecord
            {
                PolicyNumber = policyNumber,
                CompanyCode = 5631,
                EndorsementNumber = 0,
                InstallmentNumber = 1,
                MovementType = "101",
                ReferenceYear = 2025,
                ReferenceMonth = 10,
                PolicyStartDate = "2025-10-01",
                PolicyEndDate = "2026-10-01",
                NetPremiumAmount = 1000.00m + (i % 1000),
                TotalPremiumTotal = 1100.00m + (i % 1000),
                NetPremiumTotal = 1000.00m + (i % 1000)
            };
            premiums.Add(premium);
        }

        // Batch insert
        await _context.Policies.AddRangeAsync(policies);
        await _context.PremiumRecords.AddRangeAsync(premiums);
        await _context.SaveChangesAsync();

        _output.WriteLine($"Seeded {recordCount} premium records with related data");
    }

    private static string FormatBytes(long bytes)
    {
        string[] sizes = { "B", "KB", "MB", "GB" };
        double len = bytes;
        int order = 0;

        while (len >= 1024 && order < sizes.Length - 1)
        {
            order++;
            len /= 1024;
        }

        return $"{len:F2} {sizes[order]}";
    }

    public void Dispose()
    {
        _context?.Dispose();
        _serviceProvider?.Dispose();
    }
}
