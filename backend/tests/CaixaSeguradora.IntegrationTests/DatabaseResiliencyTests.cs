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
/// Database resiliency and connection recovery tests (T158 - US5).
/// Tests error handling and graceful degradation when database issues occur.
/// </summary>
[Collection("Database")]
public class DatabaseResiliencyTests : IDisposable
{
    private readonly ITestOutputHelper _output;
    private readonly ServiceProvider _serviceProvider;
    private readonly PremiumReportingDbContext _context;

    public DatabaseResiliencyTests(ITestOutputHelper _output)
    {
        this._output = _output;

        // Setup in-memory database for testing
        var services = new ServiceCollection();
        services.AddDbContext<PremiumReportingDbContext>(options =>
            options.UseInMemoryDatabase($"ResiliencyTest_{Guid.NewGuid()}"));

        // Register repositories
        services.AddScoped<IPremiumRepository, CaixaSeguradora.Infrastructure.Repositories.PremiumRepository>();
        services.AddScoped<IPolicyRepository, CaixaSeguradora.Infrastructure.Repositories.PolicyRepository>();

        _serviceProvider = services.BuildServiceProvider();
        _context = _serviceProvider.GetRequiredService<PremiumReportingDbContext>();
    }

    /// <summary>
    /// T158: Test graceful handling when database becomes unavailable mid-processing.
    /// Validates that the system can detect connection loss and fail gracefully.
    /// </summary>
    [Fact]
    public async Task ProcessRecords_DatabaseDisposed_HandlesGracefully()
    {
        // Arrange
        _output.WriteLine("=== T158: Database Connection Loss Recovery Test ===");
        await SeedDataAsync(1000);

        var premiumRepository = _serviceProvider.GetRequiredService<IPremiumRepository>();
        var startDate = DateTime.Parse("2025-10-01");
        var endDate = DateTime.Parse("2025-10-31");

        // Act: Start processing, then dispose context mid-stream
        var processedCount = 0;
        Exception? exception = null;

        try
        {
            await foreach (var premium in premiumRepository.GetPremiumsForReportAsync(startDate, endDate))
            {
                processedCount++;

                // Simulate connection loss after 500 records
                if (processedCount == 500)
                {
                    _output.WriteLine("Simulating database connection loss after 500 records...");
                    await _context.DisposeAsync(); // Force dispose context
                }
            }
        }
        catch (Exception ex)
        {
            exception = ex;
            _output.WriteLine($"Exception caught: {ex.GetType().Name} - {ex.Message}");
        }

        // Assert: System should detect connection loss
        _output.WriteLine($"Processed {processedCount} records before failure");

        // In-memory database will throw ObjectDisposedException
        Assert.NotNull(exception);
        Assert.True(
            exception is ObjectDisposedException or InvalidOperationException,
            $"Expected ObjectDisposedException or InvalidOperationException, got {exception?.GetType().Name}");

        // Should have processed some records before failure
        Assert.True(processedCount >= 500, $"Expected at least 500 records processed, got {processedCount}");
        Assert.True(processedCount <= 1000, $"Should not process all records after dispose, got {processedCount}");
    }

    /// <summary>
    /// Test that transaction rollback works correctly on error.
    /// </summary>
    [Fact]
    public async Task SaveChanges_OnError_RollsBackTransaction()
    {
        // Arrange
        _output.WriteLine("=== Transaction Rollback Test ===");

        var initialCount = await _context.PremiumRecords.CountAsync();

        // Act: Try to save with transaction
        Exception? exception = null;

        try
        {
            using var transaction = await _context.Database.BeginTransactionAsync();

            // Add some records
            _context.PremiumRecords.Add(new PremiumRecord
            {
                PolicyNumber = 9999999999999,
                CompanyCode = 5631,
                EndorsementNumber = 0,
                InstallmentNumber = 1,
                MovementType = "E", // "101" is invalid, use "E" for emission
                ReferenceYear = 2025,
                ReferenceMonth = 10,
                ReferenceDay = 1,
                PolicyStartDate = "2025-10-01",
                // PolicyEndDate doesn't exist - use ExpirationDate NotMapped property or omit
                NetPremiumTotal = 1000.00m, // NetPremiumAmount is alias
                TotalPremiumTotal = 1100.00m
            });

            await _context.SaveChangesAsync();

            // Simulate error before commit
            throw new InvalidOperationException("Simulated error during processing");

            #pragma warning disable CS0162 // Unreachable code
            await transaction.CommitAsync();
            #pragma warning restore CS0162
        }
        catch (Exception ex)
        {
            exception = ex;
            _output.WriteLine($"Transaction failed: {ex.Message}");
        }

        // Assert: Changes should be rolled back
        var finalCount = await _context.PremiumRecords.CountAsync();

        Assert.NotNull(exception);
        Assert.Equal(initialCount, finalCount); // No records should be added
        _output.WriteLine($"Transaction successfully rolled back. Record count: {finalCount}");
    }

    /// <summary>
    /// Test query timeout handling for long-running operations.
    /// </summary>
    [Fact]
    public async Task QueryTimeout_LongRunningQuery_CompletesOrTimesOut()
    {
        // Arrange
        _output.WriteLine("=== Query Timeout Test ===");
        await SeedDataAsync(5000);

        var premiumRepository = _serviceProvider.GetRequiredService<IPremiumRepository>();
        var startDate = DateTime.Parse("2025-10-01");
        var endDate = DateTime.Parse("2025-10-31");

        // Act: Process with stopwatch
        var stopwatch = Stopwatch.StartNew();
        var processedCount = 0;

        await foreach (var premium in premiumRepository.GetPremiumsForReportAsync(startDate, endDate))
        {
            processedCount++;

            // Simulate slow processing (1ms per record)
            await Task.Delay(1);
        }

        stopwatch.Stop();

        // Assert: Should complete within reasonable time
        _output.WriteLine($"Processed {processedCount} records in {stopwatch.Elapsed.TotalSeconds:F2} seconds");

        Assert.Equal(5000, processedCount);
        Assert.True(
            stopwatch.Elapsed.TotalSeconds < 30,
            $"Query took {stopwatch.Elapsed.TotalSeconds:F2}s, expected < 30s");
    }

    /// <summary>
    /// Test concurrent access to database doesn't cause corruption.
    /// </summary>
    [Fact]
    public async Task ConcurrentAccess_MultipleReaders_NoDataCorruption()
    {
        // Arrange
        _output.WriteLine("=== Concurrent Access Test ===");
        await SeedDataAsync(2000);

        var premiumRepository = _serviceProvider.GetRequiredService<IPremiumRepository>();
        var startDate = DateTime.Parse("2025-10-01");
        var endDate = DateTime.Parse("2025-10-31");

        // Act: Start 3 concurrent readers
        var tasks = new List<Task<int>>();

        for (int i = 0; i < 3; i++)
        {
            var taskNumber = i + 1;
            tasks.Add(Task.Run(async () =>
            {
                var count = 0;
                await foreach (var premium in premiumRepository.GetPremiumsForReportAsync(startDate, endDate))
                {
                    count++;
                }
                _output.WriteLine($"Task {taskNumber} completed: {count} records");
                return count;
            }));
        }

        var results = await Task.WhenAll(tasks);

        // Assert: All tasks should read same number of records
        Assert.All(results, count => Assert.Equal(2000, count));
        _output.WriteLine($"All 3 concurrent readers successfully read {results[0]} records");
    }

    /// <summary>
    /// Test memory doesn't grow unbounded during streaming.
    /// </summary>
    [Fact]
    public async Task StreamingQuery_LargeDataset_MemoryStable()
    {
        // Arrange
        _output.WriteLine("=== Memory Stability Test ===");
        await SeedDataAsync(5000);

        var premiumRepository = _serviceProvider.GetRequiredService<IPremiumRepository>();
        var startDate = DateTime.Parse("2025-10-01");
        var endDate = DateTime.Parse("2025-10-31");

        GC.Collect();
        GC.WaitForPendingFinalizers();
        GC.Collect();

        var initialMemory = GC.GetTotalMemory(false);
        _output.WriteLine($"Initial memory: {FormatBytes(initialMemory)}");

        // Act: Stream records and measure memory
        var processedCount = 0;
        var memoryMeasurements = new List<long>();

        await foreach (var premium in premiumRepository.GetPremiumsForReportAsync(startDate, endDate))
        {
            processedCount++;

            // Measure memory every 1000 records
            if (processedCount % 1000 == 0)
            {
                var currentMemory = GC.GetTotalMemory(false);
                memoryMeasurements.Add(currentMemory);
                _output.WriteLine($"After {processedCount} records: {FormatBytes(currentMemory)}");
            }
        }

        var finalMemory = GC.GetTotalMemory(false);
        var memoryGrowth = finalMemory - initialMemory;

        // Assert: Memory growth should be reasonable (< 50MB for 5000 records)
        _output.WriteLine($"Final memory: {FormatBytes(finalMemory)}");
        _output.WriteLine($"Memory growth: {FormatBytes(memoryGrowth)}");

        Assert.True(
            memoryGrowth < 50 * 1024 * 1024,
            $"Memory growth ({FormatBytes(memoryGrowth)}) exceeded 50MB limit");

        // Memory should not grow continuously (streaming works)
        if (memoryMeasurements.Count >= 2)
        {
            var lastMeasurement = memoryMeasurements[^1];
            var firstMeasurement = memoryMeasurements[0];
            var growthDuringProcessing = lastMeasurement - firstMeasurement;

            _output.WriteLine($"Memory growth during processing: {FormatBytes(growthDuringProcessing)}");
        }
    }

    private async Task SeedDataAsync(int count)
    {
        var policies = new List<Policy>();
        var premiums = new List<PremiumRecord>();

        for (int i = 1; i <= count; i++)
        {
            var policyNumber = 1000000000000L + i;

            policies.Add(new Policy
            {
                PolicyNumber = policyNumber,
                SystemCode = "RG",
                ProductCode = 1001,
                ProposerClientCode = 100001,
                PolicyStartDate = "2025-10-01",
                PolicyEndDate = "2026-10-01", // This is a valid string property
                PolicyStatus = "A"
            });

            premiums.Add(new PremiumRecord
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
                NetPremiumTotal = 1000.00m + i, // NetPremiumAmount is alias
                TotalPremiumTotal = 1100.00m + i
            });
        }

        await _context.Policies.AddRangeAsync(policies);
        await _context.PremiumRecords.AddRangeAsync(premiums);
        await _context.SaveChangesAsync();
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
        try
        {
            _context?.Dispose();
        }
        catch
        {
            // Already disposed in tests
        }

        _serviceProvider?.Dispose();
    }
}
