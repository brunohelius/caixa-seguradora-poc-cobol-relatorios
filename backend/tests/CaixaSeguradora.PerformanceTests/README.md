# Performance Benchmarks - CaixaSeguradora

This project contains performance benchmarks for the COBOL to .NET migration, ensuring the .NET implementation meets performance requirements.

## Requirements

**Constitution Requirement**: .NET performance must be within 120% of COBOL baseline execution time.

## Benchmark Categories

### 1. Premium Calculations (`PremiumCalculationBenchmarks.cs`)
Tests COBOL sections R0700-R1300 equivalent calculations.

**COBOL Baseline:**
- 10 records: ~0.5ms
- 100 records: ~5ms
- 1,000 records: ~50ms
- 10,000 records: ~500ms
- 100,000 records: ~5000ms (5 seconds)

**Benchmarks:**
- `Calculate_10_Premiums`: Basic premium calculation for 10 records
- `Calculate_100_Premiums`: 100 records
- `Calculate_1000_Premiums`: 1,000 records
- `Calculate_10K_Premiums`: 10,000 records
- `Calculate_100K_Premiums`: 100,000 records
- `ComplexCalculation_1K_Premiums`: Complex calculations with taxes and fees

### 2. Cossurance Calculations (`CossuranceCalculationBenchmarks.cs`)
Tests COBOL sections R3000-R5500 equivalent calculations.

**COBOL Baseline:**
- 10 records: ~1ms
- 100 records: ~10ms
- 1,000 records: ~100ms
- 10,000 records: ~1000ms (1 second)

**Benchmarks:**
- `Calculate_10_Cossurances`: 10 cossurance calculations
- `Calculate_100_Cossurances`: 100 calculations
- `Calculate_1K_Cossurances`: 1,000 calculations
- `Calculate_10K_Cossurances`: 10,000 calculations

### 3. File Generation (`FileGenerationBenchmarks.cs`)
Tests PREMIT.TXT and PREMCED.TXT fixed-width file generation.

**COBOL Baseline:**
- 10 records: ~2ms
- 100 records: ~20ms
- 1,000 records: ~200ms
- 10,000 records: ~2000ms (2 seconds)
- 100,000 records: ~20000ms (20 seconds)

**Benchmarks:**
- `GeneratePREMIT_10_Records`: Generate file with 10 records
- `GeneratePREMIT_100_Records`: 100 records
- `GeneratePREMIT_1K_Records`: 1,000 records
- `GeneratePREMIT_10K_Records`: 10,000 records
- `GeneratePREMIT_100K_Records`: 100,000 records
- `GeneratePREMIT_10K_Streaming`: Streaming approach for 10K records

## Running Benchmarks

### Run All Benchmarks
```bash
cd backend/tests/CaixaSeguradora.PerformanceTests
dotnet run -c Release
```

### Run Specific Category
```bash
dotnet run -c Release -- --filter *Premium*
dotnet run -c Release -- --filter *Cossurance*
dotnet run -c Release -- --filter *FileGen*
```

### Run Specific Benchmark
```bash
dotnet run -c Release -- --filter *Calculate_1K_Premiums*
```

## Interpreting Results

BenchmarkDotNet will output:

1. **Mean**: Average execution time
2. **Error**: Standard error of the mean
3. **StdDev**: Standard deviation
4. **Median**: Median execution time
5. **Allocated**: Memory allocation
6. **Rank**: Performance ranking

### Example Output
```
| Method                    | Mean      | Error    | StdDev   | Allocated |
|-------------------------- |----------:|---------:|---------:|----------:|
| Calculate_10_Premiums     | 0.500 ms  | 0.010 ms | 0.009 ms |    1.2 KB |
| Calculate_100_Premiums    | 5.200 ms  | 0.050 ms | 0.047 ms |   12.5 KB |
| Calculate_1000_Premiums   | 52.000 ms | 0.800 ms | 0.750 ms |  125.0 KB |
```

### Performance Threshold Validation

For each benchmark, verify:
```
.NET Mean Time ≤ COBOL Baseline × 1.20
```

**Example:**
- COBOL Baseline (1K premiums): 50ms
- .NET Maximum Allowed: 50ms × 1.20 = 60ms
- If .NET Mean = 52ms → ✅ PASS (52ms < 60ms)
- If .NET Mean = 65ms → ❌ FAIL (65ms > 60ms)

## Continuous Performance Monitoring

Run benchmarks:
1. **Before each release**
2. **After significant code changes**
3. **Monthly** to detect performance regression

## Troubleshooting

### Slow Performance
1. Check for missing Release build: `-c Release`
2. Verify `decimal` types are used (not `float`/`double`)
3. Profile memory allocations using MemoryDiagnoser
4. Consider StringBuilder capacity pre-allocation

### Inconsistent Results
1. Run multiple iterations: `--iterationCount 10`
2. Close background applications
3. Use dedicated benchmark machine (no Docker/VMs running)

## References

- BenchmarkDotNet Documentation: https://benchmarkdotnet.org/
- COBOL Performance Baseline: `docs/parser/FINAL-ANALYSIS-REPORT.md`
- Performance Requirements: `specs/001-vamos-migrar-sistema/spec.md` (NFR-001)
