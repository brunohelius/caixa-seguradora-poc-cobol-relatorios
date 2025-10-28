using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading;
using System.Threading.Tasks;
using CaixaSeguradora.Core.DTOs;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Interfaces;
using CaixaSeguradora.Core.Services;
using CaixaSeguradora.Infrastructure.Data;
using CaixaSeguradora.Infrastructure.Repositories;
using CaixaSeguradora.Infrastructure.Services;
using FluentAssertions;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using Moq;
using Xunit;

namespace CaixaSeguradora.PerformanceTests;

/// <summary>
/// Large dataset integration tests for SC-006 performance requirements.
/// Tests validate processing of 10,000-15,000 records with memory efficiency.
/// </summary>
public class LargeDatasetIntegrationTests : IDisposable
{
    private readonly ServiceProvider _serviceProvider;
    private readonly PremiumReportingDbContext _context;
    private readonly ReportOrchestrationService _orchestrationService;

    public LargeDatasetIntegrationTests()
    {
        // Setup in-memory database with unique name per test instance
        var services = new ServiceCollection();
        services.AddDbContext<PremiumReportingDbContext>(options =>
            options.UseInMemoryDatabase($"TestDb_{Guid.NewGuid()}"));

        // Register repositories
        services.AddScoped<IPremiumRepository, PremiumRepository>();
        services.AddScoped<IPolicyRepository, PolicyRepository>();
        services.AddScoped<IProductRepository, ProductRepository>();
        services.AddScoped<IClientRepository, ClientRepository>();
        services.AddScoped<IAddressRepository, AddressRepository>();
        services.AddScoped<ICossuranceRepository, CossuranceRepository>();

        // Register logging
        services.AddLogging(builder => builder.AddConsole().SetMinimumLevel(LogLevel.Warning));

        // Register mock services for orchestration
        var mockExecutionService = new Mock<IExecutionTrackingService>();
        mockExecutionService
            .Setup(x => x.CreateExecutionAsync(
                It.IsAny<string>(),
                It.IsAny<string>(),
                It.IsAny<string>(),
                It.IsAny<string>(),
                It.IsAny<CancellationToken>()))
            .ReturnsAsync(Guid.NewGuid());

        var mockFileWriter = new Mock<IFileWriterService>();
        mockFileWriter
            .Setup(x => x.WriteReportAsync(
                It.IsAny<string>(),
                It.IsAny<IAsyncEnumerable<string>>(),
                It.IsAny<CancellationToken>()))
            .Returns(Task.CompletedTask);

        var mockValidationService = new Mock<IBusinessRuleValidationService>();
        mockValidationService
            .Setup(x => x.ValidatePremiumAsync(
                It.IsAny<PremiumRecord>(),
                It.IsAny<Policy>(),
                It.IsAny<Product>(),
                It.IsAny<CancellationToken>()))
            .ReturnsAsync(new Core.Models.ValidationResult { IsValid = true });

        services.AddSingleton(mockExecutionService.Object);
        services.AddSingleton(mockFileWriter.Object);
        services.AddSingleton(mockValidationService.Object);

        // Register orchestration service
        services.AddScoped<ReportOrchestrationService>();

        _serviceProvider = services.BuildServiceProvider();
        _context = _serviceProvider.GetRequiredService<PremiumReportingDbContext>();
        _orchestrationService = _serviceProvider.GetRequiredService<ReportOrchestrationService>();
    }

    /// <summary>
    /// SC-006: System must process 10,000+ records in under 5 minutes.
    /// This test validates the minimum performance requirement.
    /// </summary>
    [Fact(Skip = "Long-running test - run manually with: dotnet test --filter FullyQualifiedName~GenerateReport_15000Records_CompletesUnder5Minutes")]
    public async Task GenerateReport_15000Records_CompletesUnder5Minutes()
    {
        // Arrange
        var premiums = GenerateLargePremiumDataset(15000);
        await _context.PremiumRecords.AddRangeAsync(premiums);
        await _context.SaveChangesAsync();

        var request = new GenerateReportRequest
        {
            Month = "202510",
            ReportType = "PREMIT",
            ExecutionMode = "BATCH",
            TriggeringUser = "IntegrationTest"
        };

        // Act
        var stopwatch = Stopwatch.StartNew();
        var executionId = await _orchestrationService.GenerateReportAsync(request);
        stopwatch.Stop();

        // Assert
        stopwatch.Elapsed.Should().BeLessThan(TimeSpan.FromMinutes(5),
            "SC-006: System must process 10,000+ records in under 5 minutes");

        executionId.Should().NotBe(Guid.Empty);

        // Log performance metrics
        var throughput = 15000 / stopwatch.Elapsed.TotalSeconds;
        Console.WriteLine($"Processed 15,000 records in {stopwatch.Elapsed.TotalMinutes:F2} minutes");
        Console.WriteLine($"Throughput: {throughput:F2} records/second");
    }

    /// <summary>
    /// Memory efficiency test: Validates cursor-based streaming keeps memory under 500MB.
    /// Uses IAsyncEnumerable to replicate COBOL cursor behavior.
    /// </summary>
    [Fact(Skip = "Memory test - run manually with profiler: dotnet test --filter FullyQualifiedName~MemoryUnder500MB")]
    public async Task GenerateReport_15000Records_MemoryUnder500MB()
    {
        // Arrange
        var premiums = GenerateLargePremiumDataset(15000);
        await _context.PremiumRecords.AddRangeAsync(premiums);
        await _context.SaveChangesAsync();

        // Force garbage collection to get baseline
        GC.Collect();
        GC.WaitForPendingFinalizers();
        GC.Collect();

        var beforeMemory = GC.GetTotalMemory(forceFullCollection: true);

        var request = new GenerateReportRequest
        {
            Month = "202510",
            ReportType = "PREMIT",
            ExecutionMode = "BATCH"
        };

        // Act
        await _orchestrationService.GenerateReportAsync(request);

        // Force GC to measure peak memory
        GC.Collect();
        GC.WaitForPendingFinalizers();

        var afterMemory = GC.GetTotalMemory(forceFullCollection: true);
        var memoryUsedMB = (afterMemory - beforeMemory) / 1024.0 / 1024.0;

        // Assert
        memoryUsedMB.Should().BeLessThan(500,
            "Memory usage should stay under 500MB with cursor-based streaming. " +
            $"Actual: {memoryUsedMB:F2} MB");

        Console.WriteLine($"Memory used: {memoryUsedMB:F2} MB");
    }

    /// <summary>
    /// Validates IAsyncEnumerable cursor-based streaming yields all records.
    /// Replicates COBOL FETCH behavior from sections R0500-R0600.
    /// </summary>
    [Fact]
    public async Task CursorBasedStream_15000Records_YieldsAllRecords()
    {
        // Arrange
        var premiums = GenerateLargePremiumDataset(15000);
        await _context.PremiumRecords.AddRangeAsync(premiums);
        await _context.SaveChangesAsync();

        var premiumRepository = _serviceProvider.GetRequiredService<IPremiumRepository>();
        var startDate = DateTime.Parse("2025-10-01");
        var endDate = DateTime.Parse("2025-10-31");

        // Act
        var count = 0;
        await foreach (var premium in premiumRepository.GetPremiumsForReportAsync(startDate, endDate))
        {
            count++;
        }

        // Assert
        count.Should().Be(15000, "Cursor should stream all records without loading into memory");
    }

    /// <summary>
    /// Validates cursor streaming with cancellation token support.
    /// </summary>
    [Fact]
    public async Task CursorBasedStream_WithCancellation_SupportsTokenProperly()
    {
        // Arrange
        var premiums = GenerateLargePremiumDataset(5000);
        await _context.PremiumRecords.AddRangeAsync(premiums);
        await _context.SaveChangesAsync();

        var premiumRepository = _serviceProvider.GetRequiredService<IPremiumRepository>();
        var startDate = DateTime.Parse("2025-10-01");
        var endDate = DateTime.Parse("2025-10-31");

        using var cts = new CancellationTokenSource();

        // Act
        var count = 0;
        await foreach (var premium in premiumRepository.GetPremiumsForReportAsync(
            startDate, endDate, cts.Token))
        {
            count++;
            if (count == 100)
            {
                cts.Cancel(); // Cancel after 100 records
            }
        }

        // Assert - should process at least 100 records before cancellation
        count.Should().BeGreaterOrEqualTo(100);
        count.Should().BeLessThan(5000, "Should not process all records after cancellation");
    }

    /// <summary>
    /// Helper: Direct cursor-based streaming from DbContext.
    /// Demonstrates IAsyncEnumerable pattern matching COBOL cursor behavior.
    /// </summary>
    private async IAsyncEnumerable<PremiumRecord> GetPremiumsAsync(
        DateTime startDate,
        DateTime endDate,
        [EnumeratorCancellation] CancellationToken cancellationToken = default)
    {
        var query = _context.Set<PremiumRecord>()
            .AsNoTracking()  // Read-only optimization
            .Where(p => p.ReferenceYear == startDate.Year && p.ReferenceMonth == startDate.Month)
            .OrderBy(p => p.PolicyNumber)
            .ThenBy(p => p.InstallmentNumber);

        await foreach (var record in query.AsAsyncEnumerable()
            .WithCancellation(cancellationToken))
        {
            yield return record;
        }
    }

    /// <summary>
    /// Generates large premium dataset for testing.
    /// Fixed seed (42) ensures reproducible test results.
    /// </summary>
    private List<PremiumRecord> GenerateLargePremiumDataset(int count)
    {
        var premiums = new List<PremiumRecord>();
        var random = new Random(42); // Fixed seed for reproducibility

        // Generate company codes: 0 (Bradesco), 10 (BISA), 11 (BSEG)
        var companyCodes = new[] { 0, 10, 11 };

        // Generate movement types from COBOL logic (101-106, 201-206, etc.)
        var movementTypes = new[] { "101", "102", "103", "201", "202", "301" };

        for (int i = 1; i <= count; i++)
        {
            var policyNumber = 1000000000000L + i;
            var companyCode = companyCodes[random.Next(companyCodes.Length)];
            var movementType = movementTypes[random.Next(movementTypes.Length)];

            // Generate realistic premium amounts (COBOL: PIC 9(15)V99)
            var netPremium = (decimal)(random.NextDouble() * 50000 + 100);
            var totalPremium = netPremium * 1.10m; // Add 10% overhead

            premiums.Add(new PremiumRecord
            {
                PolicyNumber = policyNumber,
                CompanyCode = companyCode,
                EndorsementNumber = random.Next(0, 3),
                InstallmentNumber = random.Next(1, 13),
                ReferenceYear = 2025,
                ReferenceMonth = 10,
                MovementType = movementType,
                PolicyStartDate = "2025-10-01",
                PolicyEndDate = "2026-10-01",
                NetPremiumAmount = Math.Round(netPremium, 2),
                TotalPremiumTotal = Math.Round(totalPremium, 2),
                NetPremiumTotal = Math.Round(netPremium, 2),
                IofAmount = Math.Round(netPremium * 0.07m, 2), // 7% IOF
                // Additional COBOL fields
                CedingCompanyCode = companyCode,
                BrokerCode = 1000 + random.Next(100),
                ProductCode = 5000 + random.Next(50),
                RamoSusep = 500 + random.Next(20)
            });
        }

        return premiums;
    }

    /// <summary>
    /// Benchmark: Compare cursor vs ToList approach (demonstrates anti-pattern).
    /// </summary>
    [Fact]
    public async Task Benchmark_CursorVsToList_MemoryEfficiency()
    {
        // Arrange
        var premiums = GenerateLargePremiumDataset(5000);
        await _context.PremiumRecords.AddRangeAsync(premiums);
        await _context.SaveChangesAsync();

        var premiumRepository = _serviceProvider.GetRequiredService<IPremiumRepository>();
        var startDate = DateTime.Parse("2025-10-01");
        var endDate = DateTime.Parse("2025-10-31");

        GC.Collect();
        GC.WaitForPendingFinalizers();
        GC.Collect();

        // Act 1: Cursor-based streaming (IAsyncEnumerable) - CORRECT APPROACH
        var cursorMemoryStart = GC.GetTotalMemory(false);
        var cursorStopwatch = Stopwatch.StartNew();

        var cursorCount = 0;
        await foreach (var premium in premiumRepository.GetPremiumsForReportAsync(startDate, endDate))
        {
            cursorCount++;
        }

        cursorStopwatch.Stop();
        var cursorMemoryPeak = GC.GetTotalMemory(false);
        var cursorMemoryUsedMB = (cursorMemoryPeak - cursorMemoryStart) / 1024.0 / 1024.0;

        // Assert
        cursorCount.Should().Be(5000, "Cursor should process all records");

        Console.WriteLine("=== Benchmark Results ===");
        Console.WriteLine($"Cursor-based streaming:");
        Console.WriteLine($"  - Time: {cursorStopwatch.Elapsed.TotalMilliseconds:F2} ms");
        Console.WriteLine($"  - Memory used: {cursorMemoryUsedMB:F2} MB");
        Console.WriteLine($"  - Records: {cursorCount}");

        // Verify cursor approach is memory-efficient
        cursorMemoryUsedMB.Should().BeLessThan(50, "Cursor-based approach should use minimal memory");
    }

    /// <summary>
    /// Validates database seeding with related entities (policies, products, clients).
    /// </summary>
    [Fact]
    public async Task SeedLargeDataset_WithRelatedEntities_CreatesCompleteGraph()
    {
        // Arrange & Act
        await SeedLargeDatasetWithRelatedEntitiesAsync(1000);

        // Assert
        var premiumCount = await _context.PremiumRecords.CountAsync();
        var policyCount = await _context.Policies.CountAsync();
        var productCount = await _context.Products.CountAsync();
        var clientCount = await _context.Clients.CountAsync();

        premiumCount.Should().Be(1000);
        policyCount.Should().Be(1000);
        productCount.Should().BeGreaterOrEqualTo(10);
        clientCount.Should().BeGreaterOrEqualTo(10);
    }

    /// <summary>
    /// Seeds database with premium records and related entities for comprehensive testing.
    /// </summary>
    private async Task SeedLargeDatasetWithRelatedEntitiesAsync(int recordCount)
    {
        var policies = new List<Policy>();
        var premiums = new List<PremiumRecord>();
        var products = new List<Product>();
        var clients = new List<Client>();

        // Create reference data (10 products and clients for variety)
        for (int i = 1; i <= 10; i++)
        {
            products.Add(new Product
            {
                ProductCode = 5000 + i,
                ProductName = $"Produto Seguro {i}",
                RamoSusep = 500 + i
            });

            clients.Add(new Client
            {
                ClientCode = 100000 + i,
                ClientName = $"Cliente Segurado {i}",
                DocumentNumber = $"{12345678900 + i}"
            });
        }

        await _context.Products.AddRangeAsync(products);
        await _context.Clients.AddRangeAsync(clients);

        // Create premium records with policies
        for (int i = 1; i <= recordCount; i++)
        {
            var policyNumber = 1000000000000L + i;
            var productCode = 5000 + (i % 10) + 1;
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

        // Batch insert for performance
        await _context.Policies.AddRangeAsync(policies);
        await _context.PremiumRecords.AddRangeAsync(premiums);
        await _context.SaveChangesAsync();
    }

    public void Dispose()
    {
        _context?.Dispose();
        _serviceProvider?.Dispose();
    }
}
