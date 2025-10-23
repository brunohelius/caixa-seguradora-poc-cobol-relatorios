using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Order;
using CaixaSeguradora.Infrastructure.Formatters;
using System.Text;

namespace CaixaSeguradora.PerformanceTests;

/// <summary>
/// Performance benchmarks for fixed-width file generation (PREMIT, PREMCED)
///
/// COBOL Baseline Performance:
/// - 10 records: ~2ms
/// - 100 records: ~20ms
/// - 1,000 records: ~200ms
/// - 10,000 records: ~2000ms (2 seconds)
/// - 100,000 records: ~20000ms (20 seconds)
///
/// Target: .NET must be within 120% of COBOL baseline
/// </summary>
[MemoryDiagnoser]
[Orderer(SummaryOrderPolicy.FastestToSlowest)]
[RankColumn]
public class FileGenerationBenchmarks
{
    private List<TestPremiumRecord> _records10 = null!;
    private List<TestPremiumRecord> _records100 = null!;
    private List<TestPremiumRecord> _records1K = null!;
    private List<TestPremiumRecord> _records10K = null!;
    private List<TestPremiumRecord> _records100K = null!;

    [GlobalSetup]
    public void Setup()
    {

        _records10 = GenerateTestRecords(10);
        _records100 = GenerateTestRecords(100);
        _records1K = GenerateTestRecords(1_000);
        _records10K = GenerateTestRecords(10_000);
        _records100K = GenerateTestRecords(100_000);
    }

    private List<TestPremiumRecord> GenerateTestRecords(int count)
    {
        var random = new Random(42);
        var records = new List<TestPremiumRecord>(count);

        for (int i = 0; i < count; i++)
        {
            records.Add(new TestPremiumRecord
            {
                PolicyNumber = $"POL{i:D10}",
                CompanyCode = 123,
                BranchCode = 456,
                PremiumAmount = (decimal)(random.NextDouble() * 10000 + 100),
                EffectiveDate = DateTime.Now.AddDays(-random.Next(365))
            });
        }

        return records;
    }

    private string FormatRecord(TestPremiumRecord record)
    {
        // PREMIT format: Fixed-width 200 characters per line
        var sb = new StringBuilder(200);

        // Policy number: 10 chars
        sb.Append(FixedWidthFormatter.FormatAlphanumeric(record.PolicyNumber, 10));

        // Company code: 5 chars (numeric, left-padded)
        sb.Append(FixedWidthFormatter.FormatNumeric(record.CompanyCode, 5, 0));

        // Branch code: 5 chars (numeric, left-padded)
        sb.Append(FixedWidthFormatter.FormatNumeric(record.BranchCode, 5, 0));

        // Premium amount: 15 chars (2 decimal places, implied decimal point)
        sb.Append(FixedWidthFormatter.FormatNumeric(record.PremiumAmount, 15, 2));

        // Effective date: 8 chars (YYYYMMDD)
        sb.Append(record.EffectiveDate.ToString("yyyyMMdd"));

        // Padding to 200 chars
        sb.Append(new string(' ', 200 - sb.Length));

        return sb.ToString();
    }

    [Benchmark]
    [BenchmarkCategory("FileGen", "Small")]
    public void GeneratePREMIT_10_Records()
    {
        var sb = new StringBuilder(_records10.Count * 200);

        foreach (var record in _records10)
        {
            sb.AppendLine(FormatRecord(record));
        }

        var output = sb.ToString();
    }

    [Benchmark]
    [BenchmarkCategory("FileGen", "Medium")]
    public void GeneratePREMIT_100_Records()
    {
        var sb = new StringBuilder(_records100.Count * 200);

        foreach (var record in _records100)
        {
            sb.AppendLine(FormatRecord(record));
        }

        var output = sb.ToString();
    }

    [Benchmark]
    [BenchmarkCategory("FileGen", "Large")]
    public void GeneratePREMIT_1K_Records()
    {
        var sb = new StringBuilder(_records1K.Count * 200);

        foreach (var record in _records1K)
        {
            sb.AppendLine(FormatRecord(record));
        }

        var output = sb.ToString();
    }

    [Benchmark]
    [BenchmarkCategory("FileGen", "VeryLarge")]
    public void GeneratePREMIT_10K_Records()
    {
        var sb = new StringBuilder(_records10K.Count * 200);

        foreach (var record in _records10K)
        {
            sb.AppendLine(FormatRecord(record));
        }

        var output = sb.ToString();
    }

    [Benchmark]
    [BenchmarkCategory("FileGen", "Extreme")]
    public void GeneratePREMIT_100K_Records()
    {
        var sb = new StringBuilder(_records100K.Count * 200);

        foreach (var record in _records100K)
        {
            sb.AppendLine(FormatRecord(record));
        }

        var output = sb.ToString();
    }

    // Test streaming approach for large files
    [Benchmark]
    [BenchmarkCategory("FileGen", "Streaming")]
    public async Task GeneratePREMIT_10K_Streaming()
    {
        using var ms = new MemoryStream();
        using var writer = new StreamWriter(ms);

        foreach (var record in _records10K)
        {
            await writer.WriteLineAsync(FormatRecord(record));
        }

        await writer.FlushAsync();
    }

    private class TestPremiumRecord
    {
        public string PolicyNumber { get; set; } = string.Empty;
        public int CompanyCode { get; set; }
        public int BranchCode { get; set; }
        public decimal PremiumAmount { get; set; }
        public DateTime EffectiveDate { get; set; }
    }
}
