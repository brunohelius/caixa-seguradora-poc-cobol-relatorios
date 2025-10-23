using BenchmarkDotNet.Running;

namespace CaixaSeguradora.PerformanceTests;

public class Program
{
    public static void Main(string[] args)
    {
        var summary = BenchmarkRunner.Run<PremiumCalculationBenchmarks>();
        var summary2 = BenchmarkRunner.Run<CossuranceCalculationBenchmarks>();
        var summary3 = BenchmarkRunner.Run<FileGenerationBenchmarks>();
    }
}
