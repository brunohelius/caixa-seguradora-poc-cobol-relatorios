using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Order;

namespace CaixaSeguradora.PerformanceTests;

/// <summary>
/// Performance benchmarks for premium calculations (COBOL sections R0700-R1300)
///
/// COBOL Baseline Performance:
/// - 10 records: ~0.5ms
/// - 100 records: ~5ms
/// - 1,000 records: ~50ms
/// - 10,000 records: ~500ms
/// - 100,000 records: ~5000ms (5 seconds)
///
/// Target: .NET must be within 120% of COBOL baseline
/// </summary>
[MemoryDiagnoser]
[Orderer(SummaryOrderPolicy.FastestToSlowest)]
[RankColumn]
public class PremiumCalculationBenchmarks
{
    private List<decimal> _baseAmounts10 = null!;
    private List<decimal> _baseAmounts100 = null!;
    private List<decimal> _baseAmounts1K = null!;
    private List<decimal> _baseAmounts10K = null!;
    private List<decimal> _baseAmounts100K = null!;

    [GlobalSetup]
    public void Setup()
    {
        // Generate test data sets
        _baseAmounts10 = GenerateTestData(10);
        _baseAmounts100 = GenerateTestData(100);
        _baseAmounts1K = GenerateTestData(1_000);
        _baseAmounts10K = GenerateTestData(10_000);
        _baseAmounts100K = GenerateTestData(100_000);
    }

    private List<decimal> GenerateTestData(int count)
    {
        var random = new Random(42); // Fixed seed for consistency
        var amounts = new List<decimal>(count);

        for (int i = 0; i < count; i++)
        {
            // Generate realistic premium amounts (R$ 100 to R$ 10,000)
            amounts.Add((decimal)(random.NextDouble() * 9900 + 100));
        }

        return amounts;
    }

    // Simplified calculation matching COBOL logic
    private decimal CalculatePremium(decimal baseAmount, decimal rate)
    {
        // Net premium calculation
        var netPremium = baseAmount * (1 + rate);

        // Round to 2 decimal places (COBOL COMP-3 precision)
        return Math.Round(netPremium, 2, MidpointRounding.AwayFromZero);
    }

    [Benchmark]
    [BenchmarkCategory("Premium", "Small")]
    public void Calculate_10_Premiums()
    {
        decimal rate = 0.15m; // 15% tax rate

        foreach (var amount in _baseAmounts10)
        {
            var premium = CalculatePremium(amount, rate);
        }
    }

    [Benchmark]
    [BenchmarkCategory("Premium", "Medium")]
    public void Calculate_100_Premiums()
    {
        decimal rate = 0.15m;

        foreach (var amount in _baseAmounts100)
        {
            var premium = CalculatePremium(amount, rate);
        }
    }

    [Benchmark]
    [BenchmarkCategory("Premium", "Large")]
    public void Calculate_1000_Premiums()
    {
        decimal rate = 0.15m;

        foreach (var amount in _baseAmounts1K)
        {
            var premium = CalculatePremium(amount, rate);
        }
    }

    [Benchmark]
    [BenchmarkCategory("Premium", "VeryLarge")]
    public void Calculate_10K_Premiums()
    {
        decimal rate = 0.15m;

        foreach (var amount in _baseAmounts10K)
        {
            var premium = CalculatePremium(amount, rate);
        }
    }

    [Benchmark]
    [BenchmarkCategory("Premium", "Extreme")]
    public void Calculate_100K_Premiums()
    {
        decimal rate = 0.15m;

        foreach (var amount in _baseAmounts100K)
        {
            var premium = CalculatePremium(amount, rate);
        }
    }

    // Complex calculation benchmarks (multiple operations per record)
    [Benchmark]
    [BenchmarkCategory("Premium", "Complex")]
    public void ComplexCalculation_1K_Premiums()
    {
        decimal rate = 0.15m;
        decimal iofRate = 0.0738m; // IOF tax

        foreach (var amount in _baseAmounts1K)
        {
            // Simulate full premium calculation with taxes
            var netPremium = CalculatePremium(amount, rate);
            var withIOF = netPremium * (1 + iofRate);
            var totalPremium = Math.Round(withIOF * 1.065m, 2); // Additional 6.5% fee
        }
    }
}
