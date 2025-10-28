# Large Dataset Integration Tests

## Overview

This test suite validates **SC-006 performance requirements** for the COBOL to .NET migration project:
- Process 10,000+ premium records in under 5 minutes
- Memory-efficient cursor-based streaming (IAsyncEnumerable)
- Validate byte-for-byte compatibility with COBOL output

## Test File

**Location**: `LargeDatasetIntegrationTests.cs`

## Test Categories

### 1. Performance Tests (Long-Running)

These tests are marked with `Skip` attribute and must be run manually:

#### Test: GenerateReport_15000Records_CompletesUnder5Minutes
- **SC-006 Requirement**: Process 10,000+ records in under 5 minutes
- **Dataset**: 15,000 premium records
- **Expected**: Complete processing in < 5 minutes (300 seconds)
- **Validates**: End-to-end report generation throughput

**Run Command**:
```bash
cd backend/tests/CaixaSeguradora.PerformanceTests
dotnet test --filter "FullyQualifiedName~GenerateReport_15000Records_CompletesUnder5Minutes"
```

#### Test: GenerateReport_15000Records_MemoryUnder500MB
- **Requirement**: Memory-efficient processing using cursor-based streaming
- **Dataset**: 15,000 premium records
- **Expected**: Memory usage < 500 MB
- **Validates**: IAsyncEnumerable prevents loading all records into memory

**Run Command**:
```bash
dotnet test --filter "FullyQualifiedName~MemoryUnder500MB"
```

**Note**: For accurate memory profiling, use tools like:
- dotMemory (JetBrains)
- PerfView (Microsoft)
- dotnet-counters

### 2. Functional Tests (Fast)

These tests run automatically and validate cursor behavior:

#### Test: CursorBasedStream_15000Records_YieldsAllRecords
- **Validates**: IAsyncEnumerable streams all records correctly
- **Dataset**: 15,000 premium records
- **Expected**: Processes exactly 15,000 records without buffering

#### Test: CursorBasedStream_WithCancellation_SupportsTokenProperly
- **Validates**: Cancellation token support in cursor streaming
- **Dataset**: 5,000 premium records
- **Expected**: Respects cancellation after 100 records

#### Test: Benchmark_CursorVsToList_MemoryEfficiency
- **Validates**: Cursor approach is more memory-efficient than ToList()
- **Dataset**: 5,000 premium records
- **Expected**: Cursor memory usage < 50 MB

#### Test: SeedLargeDataset_WithRelatedEntities_CreatesCompleteGraph
- **Validates**: Database seeding with policies, products, clients
- **Dataset**: 1,000 premium records + related entities
- **Expected**: Complete entity graph created

**Run All Fast Tests**:
```bash
cd backend/tests/CaixaSeguradora.PerformanceTests
dotnet test --filter "FullyQualifiedName~LargeDatasetIntegrationTests&Skip!=Long-running"
```

## Implementation Details

### Cursor-Based Streaming Pattern

The tests validate the IAsyncEnumerable pattern that replicates COBOL cursor behavior:

```csharp
// COBOL: FETCH C1-PREMIOS (sections R0500-R0600)
// .NET: IAsyncEnumerable with yield return
private async IAsyncEnumerable<PremiumRecord> GetPremiumsAsync(
    DateTime startDate,
    DateTime endDate,
    [EnumeratorCancellation] CancellationToken cancellationToken = default)
{
    var query = _context.Set<PremiumRecord>()
        .AsNoTracking()  // Read-only optimization
        .Where(p => p.ReferenceYear == startDate.Year)
        .OrderBy(p => p.PolicyNumber);

    await foreach (var record in query.AsAsyncEnumerable()
        .WithCancellation(cancellationToken))
    {
        yield return record;
    }
}
```

**Benefits**:
1. **Memory Efficient**: Processes one record at a time (no ToList())
2. **Cancellation Support**: Respects CancellationToken for long-running operations
3. **COBOL Parity**: Matches COBOL FETCH behavior byte-for-byte

### Test Data Generation

Tests use **fixed seed (42)** for reproducible results:

```csharp
var random = new Random(42); // Deterministic test data
```

**Dataset Characteristics**:
- Company codes: 0 (Bradesco), 10 (BISA), 11 (BSEG)
- Movement types: 101-106, 201-206, 301 (COBOL section R0700)
- Premium amounts: $100 - $50,100 with realistic distributions
- Related entities: 10 products, 10 clients (reference data)

### Dependency Injection Setup

Tests use in-memory database with complete DI container:

```csharp
services.AddDbContext<PremiumReportingDbContext>(options =>
    options.UseInMemoryDatabase($"TestDb_{Guid.NewGuid()}"));

// Real repositories
services.AddScoped<IPremiumRepository, PremiumRepository>();
services.AddScoped<IPolicyRepository, PolicyRepository>();
// ... etc

// Mock external services
services.AddSingleton<IExecutionTrackingService>(mockExecutionService.Object);
services.AddSingleton<IFileWriterService>(mockFileWriter.Object);
```

## Running Tests

### Run All Fast Tests (< 1 minute)
```bash
cd backend/tests/CaixaSeguradora.PerformanceTests
dotnet test LargeDatasetIntegrationTests.cs
```

### Run Long-Running Performance Tests (5-10 minutes)
```bash
# Run specific test by name
dotnet test --filter "FullyQualifiedName~GenerateReport_15000Records_CompletesUnder5Minutes"

# Or temporarily remove [Skip] attribute and run:
dotnet test --filter "ClassName~LargeDatasetIntegrationTests"
```

### Run with Detailed Output
```bash
dotnet test --logger "console;verbosity=detailed"
```

### Run with Coverage
```bash
dotnet test /p:CollectCoverage=true /p:CoverageReportFormat=html
```

## Performance Benchmarks

Expected performance metrics (based on SC-006):

| Records | Time Target | Throughput Target | Memory Target |
|---------|-------------|-------------------|---------------|
| 10,000  | < 5 min     | > 33 rec/sec      | < 500 MB      |
| 15,000  | < 5 min     | > 50 rec/sec      | < 500 MB      |

**Actual Results** (to be measured):
- Run tests and document results in `specs/002-migration-analysis-pdf/performance-results.md`

## Troubleshooting

### Build Errors
If you see Entity Framework errors:
```bash
# Ensure Core/Infrastructure projects build first
cd backend
dotnet build src/CaixaSeguradora.Core
dotnet build src/CaixaSeguradora.Infrastructure
dotnet build tests/CaixaSeguradora.PerformanceTests
```

### Memory Tests Failing
- Close other applications to free memory
- Run tests individually, not in parallel
- Use memory profiler for accurate measurements

### Timeout Errors
- Increase test timeout in xUnit: `[Fact(Timeout = 600000)]` (10 minutes)
- Check database connection pooling settings
- Verify no other processes consuming CPU

## Integration with CI/CD

### GitHub Actions Example
```yaml
- name: Run Performance Tests
  run: |
    cd backend/tests/CaixaSeguradora.PerformanceTests
    dotnet test --filter "FullyQualifiedName~LargeDatasetIntegrationTests&Skip!=Long-running"
```

### Manual Performance Validation
Run before major releases:
```bash
# Full performance suite (15-20 minutes)
./run-full-performance-suite.sh
```

## References

- **Feature Spec**: `specs/001-vamos-migrar-sistema/spec.md` (SC-006)
- **COBOL Analysis**: `docs/parser/FINAL-ANALYSIS-REPORT.md` (cursor sections R0500-R0600)
- **Performance Benchmarks**: `specs/002-migration-analysis-pdf/performance-benchmarks.md`

## Authors

- Performance tests created: October 27, 2025
- Based on SpecKit methodology and COBOL RG1866B analysis
- SC-006 requirement: Process 10,000+ records in under 5 minutes
