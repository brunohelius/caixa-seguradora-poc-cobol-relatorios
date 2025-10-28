using System.Diagnostics;
using CaixaSeguradora.Core.DTOs;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Interfaces;
using CaixaSeguradora.Infrastructure.Data;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.DependencyInjection;
using Xunit;
using Xunit.Abstractions;

namespace CaixaSeguradora.IntegrationTests;

/// <summary>
/// Memory profiling tests for large dataset processing (US5 requirement).
/// Target: Process 15,000 records with peak memory under 500MB.
/// </summary>
[Collection("Database")]
public class MemoryProfilingTests : IDisposable
{
    private readonly ITestOutputHelper _output;
    private readonly ServiceProvider _serviceProvider;
    private readonly PremiumReportingDbContext _context;

    public MemoryProfilingTests(ITestOutputHelper output)
    {
        _output = output;

        // Setup in-memory database for testing
        var services = new ServiceCollection();
        services.AddDbContext<PremiumReportingDbContext>(options =>
            options.UseInMemoryDatabase($"MemoryProfiling_{Guid.NewGuid()}"));

        // Register repositories
        services.AddScoped<IPremiumRepository, CaixaSeguradora.Infrastructure.Repositories.PremiumRepository>();
        services.AddScoped<IPolicyRepository, CaixaSeguradora.Infrastructure.Repositories.PolicyRepository>();
        services.AddScoped<IAddressRepository, CaixaSeguradora.Infrastructure.Repositories.AddressRepository>();
        services.AddScoped<ICossuranceRepository, CaixaSeguradora.Infrastructure.Repositories.CossuranceRepository>();

        _serviceProvider = services.BuildServiceProvider();
        _context = _serviceProvider.GetRequiredService<PremiumReportingDbContext>();
    }

    /// <summary>
    /// T151: Test memory usage stays under 500MB when processing 15,000 records.
    /// Validates US5 requirement for memory-efficient cursor-based processing.
    /// </summary>
    [Fact]
    public async Task ProcessLargeDataset_15000Records_MemoryUnder500MB()
    {
        // Arrange: Seed 15,000 premium records
        _output.WriteLine("=== Phase 7 (US5) Memory Profiling Test ===");
        _output.WriteLine("Seeding 15,000 premium records for memory test...");

        await SeedLargeDatasetAsync(15000);

        // Force garbage collection before measurement
        GC.Collect();
        GC.WaitForPendingFinalizers();
        GC.Collect();

        var initialMemory = GC.GetTotalMemory(false);
        _output.WriteLine($"Initial memory: {FormatBytes(initialMemory)}");

        // Act: Stream and process records using cursor pattern
        var premiumRepository = _serviceProvider.GetRequiredService<IPremiumRepository>();
        var policyRepository = _serviceProvider.GetRequiredService<IPolicyRepository>();

        var startDate = DateTime.Parse("2025-10-01");
        var endDate = DateTime.Parse("2025-10-31");

        var stopwatch = Stopwatch.StartNew();
        var processedCount = 0;
        var peakMemory = initialMemory;

        await foreach (var premium in premiumRepository.GetPremiumsForReportAsync(startDate, endDate))
        {
            // Simulate processing (like ReportOrchestrationService)
            var policy = await policyRepository.GetByPolicyNumberAsync(premium.PolicyNumber);

            processedCount++;

            // Measure memory every 1000 records
            if (processedCount % 1000 == 0)
            {
                var currentMemory = GC.GetTotalMemory(false);
                peakMemory = Math.Max(peakMemory, currentMemory);

                _output.WriteLine(
                    $"Processed {processedCount:N0} records | " +
                    $"Current: {FormatBytes(currentMemory)} | " +
                    $"Peak: {FormatBytes(peakMemory)} | " +
                    $"Elapsed: {stopwatch.Elapsed.TotalSeconds:F2}s");
            }
        }

        stopwatch.Stop();

        // Final measurement
        var finalMemory = GC.GetTotalMemory(false);
        peakMemory = Math.Max(peakMemory, finalMemory);

        var memoryIncrease = peakMemory - initialMemory;
        var memoryPerRecord = memoryIncrease / processedCount;

        // Assert: Memory requirements
        _output.WriteLine("");
        _output.WriteLine("=== Memory Profiling Results ===");
        _output.WriteLine($"Total records processed: {processedCount:N0}");
        _output.WriteLine($"Initial memory: {FormatBytes(initialMemory)}");
        _output.WriteLine($"Peak memory: {FormatBytes(peakMemory)}");
        _output.WriteLine($"Final memory: {FormatBytes(finalMemory)}");
        _output.WriteLine($"Memory increase: {FormatBytes(memoryIncrease)}");
        _output.WriteLine($"Memory per record: {FormatBytes((long)memoryPerRecord)}");
        _output.WriteLine($"Processing time: {stopwatch.Elapsed.TotalSeconds:F2} seconds");
        _output.WriteLine($"Throughput: {processedCount / stopwatch.Elapsed.TotalSeconds:F2} records/sec");

        // US5 requirement: Memory should stay under 500MB
        const long maxMemoryBytes = 500L * 1024 * 1024; // 500MB
        Assert.True(
            memoryIncrease < maxMemoryBytes,
            $"Memory increase ({FormatBytes(memoryIncrease)}) exceeded 500MB limit. " +
            $"Cursor-based streaming may not be working correctly.");

        // Additional validation: Processing should complete
        Assert.Equal(15000, processedCount);
    }

    /// <summary>
    /// T152 (partial): Test smaller dataset (1000 records) completes quickly.
    /// This validates the streaming pattern works for moderate volumes.
    /// </summary>
    [Fact]
    public async Task ProcessMediumDataset_1000Records_CompletesUnder30Seconds()
    {
        // Arrange
        _output.WriteLine("=== Medium Dataset Performance Test ===");
        await SeedLargeDatasetAsync(1000);

        var premiumRepository = _serviceProvider.GetRequiredService<IPremiumRepository>();
        var startDate = DateTime.Parse("2025-10-01");
        var endDate = DateTime.Parse("2025-10-31");

        // Act
        var stopwatch = Stopwatch.StartNew();
        var processedCount = 0;

        await foreach (var premium in premiumRepository.GetPremiumsForReportAsync(startDate, endDate))
        {
            processedCount++;
        }

        stopwatch.Stop();

        // Assert
        _output.WriteLine($"Processed {processedCount} records in {stopwatch.Elapsed.TotalSeconds:F2} seconds");
        _output.WriteLine($"Throughput: {processedCount / stopwatch.Elapsed.TotalSeconds:F2} records/sec");

        Assert.Equal(1000, processedCount);
        Assert.True(
            stopwatch.Elapsed.TotalSeconds < 30,
            $"Processing took {stopwatch.Elapsed.TotalSeconds:F2}s, expected < 30s");
    }

    /// <summary>
    /// Seeds the database with a specified number of premium records for testing.
    /// </summary>
    private async Task SeedLargeDatasetAsync(int recordCount)
    {
        var policies = new List<Policy>();
        var premiums = new List<PremiumRecord>();

        for (int i = 1; i <= recordCount; i++)
        {
            var policyNumber = 1000000000000L + i;

            // Create policy
            var policy = new Policy
            {
                PolicyNumber = policyNumber,
                SystemCode = "RG",
                ProductCode = 1001,
                ProposerClientCode = 100000 + (i % 100),
                PolicyStartDate = "2025-10-01",
                PolicyEndDate = "2026-10-01",
                PolicyStatus = "A"
            };
            policies.Add(policy);

            // Create premium record
            var premium = new PremiumRecord
            {
                PolicyNumber = policyNumber,
                CompanyCode = 5631,
                EndorsementNumber = 0,
                InstallmentNumber = 1,
                MovementType = "E", // "101" is invalid
                ReferenceYear = 2025,
                ReferenceMonth = 10,
                ReferenceDay = 1,
                PolicyStartDate = "2025-10-01",
                // PolicyEndDate doesn't exist in PremiumRecord
                NetPremiumTotal = 1000.00m + (i % 1000), // NetPremiumAmount is alias
                TotalPremiumTotal = 1100.00m + (i % 1000)
            };
            premiums.Add(premium);
        }

        // Batch insert for performance
        await _context.Policies.AddRangeAsync(policies);
        await _context.PremiumRecords.AddRangeAsync(premiums);
        await _context.SaveChangesAsync();

        _output.WriteLine($"Seeded {recordCount} premium records and policies");
    }

    /// <summary>
    /// Formats bytes into human-readable format (KB, MB, GB).
    /// </summary>
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
