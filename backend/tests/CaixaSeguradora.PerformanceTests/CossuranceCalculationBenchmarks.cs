using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Order;

namespace CaixaSeguradora.PerformanceTests;

/// <summary>
/// Performance benchmarks for cossurance calculations (COBOL sections R3000-R5500)
///
/// COBOL Baseline Performance:
/// - 10 records: ~1ms
/// - 100 records: ~10ms
/// - 1,000 records: ~100ms
/// - 10,000 records: ~1000ms (1 second)
///
/// Target: .NET must be within 120% of COBOL baseline
/// </summary>
[MemoryDiagnoser]
[Orderer(SummaryOrderPolicy.FastestToSlowest)]
[RankColumn]
public class CossuranceCalculationBenchmarks
{
    private List<TestCossuranceData> _testData10 = null!;
    private List<TestCossuranceData> _testData100 = null!;
    private List<TestCossuranceData> _testData1K = null!;
    private List<TestCossuranceData> _testData10K = null!;

    [GlobalSetup]
    public void Setup()
    {
        _testData10 = GenerateTestData(10);
        _testData100 = GenerateTestData(100);
        _testData1K = GenerateTestData(1_000);
        _testData10K = GenerateTestData(10_000);
    }

    private List<TestCossuranceData> GenerateTestData(int count)
    {
        var random = new Random(42);
        var data = new List<TestCossuranceData>(count);

        for (int i = 0; i < count; i++)
        {
            data.Add(new TestCossuranceData
            {
                TotalPremium = (decimal)(random.NextDouble() * 50000 + 1000),
                CompanyShare = (decimal)(random.NextDouble() * 0.5 + 0.3), // 30-80%
                ReinsuranceShare = (decimal)(random.NextDouble() * 0.3) // 0-30%
            });
        }

        return data;
    }

    // Simplified cossurance calculation matching COBOL logic
    private CossuranceResult CalculateCossurance(decimal totalPremium, decimal companyShare, decimal reinsuranceShare)
    {
        var companyAmount = Math.Round(totalPremium * companyShare, 2, MidpointRounding.AwayFromZero);
        var reinsuranceAmount = Math.Round(totalPremium * reinsuranceShare, 2, MidpointRounding.AwayFromZero);
        var remainingShare = 1.0m - companyShare - reinsuranceShare;
        var remainingAmount = Math.Round(totalPremium * remainingShare, 2, MidpointRounding.AwayFromZero);

        return new CossuranceResult
        {
            CompanyAmount = companyAmount,
            ReinsuranceAmount = reinsuranceAmount,
            RemainingAmount = remainingAmount
        };
    }

    [Benchmark]
    [BenchmarkCategory("Cossurance", "Small")]
    public void Calculate_10_Cossurances()
    {
        foreach (var data in _testData10)
        {
            var result = CalculateCossurance(
                data.TotalPremium,
                data.CompanyShare,
                data.ReinsuranceShare
            );
        }
    }

    [Benchmark]
    [BenchmarkCategory("Cossurance", "Medium")]
    public void Calculate_100_Cossurances()
    {
        foreach (var data in _testData100)
        {
            var result = CalculateCossurance(
                data.TotalPremium,
                data.CompanyShare,
                data.ReinsuranceShare
            );
        }
    }

    [Benchmark]
    [BenchmarkCategory("Cossurance", "Large")]
    public void Calculate_1K_Cossurances()
    {
        foreach (var data in _testData1K)
        {
            var result = CalculateCossurance(
                data.TotalPremium,
                data.CompanyShare,
                data.ReinsuranceShare
            );
        }
    }

    [Benchmark]
    [BenchmarkCategory("Cossurance", "VeryLarge")]
    public void Calculate_10K_Cossurances()
    {
        foreach (var data in _testData10K)
        {
            var result = CalculateCossurance(
                data.TotalPremium,
                data.CompanyShare,
                data.ReinsuranceShare
            );
        }
    }

    private class TestCossuranceData
    {
        public decimal TotalPremium { get; set; }
        public decimal CompanyShare { get; set; }
        public decimal ReinsuranceShare { get; set; }
    }

    private class CossuranceResult
    {
        public decimal CompanyAmount { get; set; }
        public decimal ReinsuranceAmount { get; set; }
        public decimal RemainingAmount { get; set; }
    }
}
