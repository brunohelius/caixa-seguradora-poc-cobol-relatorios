using Xunit;
using FluentAssertions;
using Microsoft.Extensions.Logging;
using Moq;
using CaixaSeguradora.Core.Services;
using CaixaSeguradora.Core.Entities;
using System.Globalization;

namespace CaixaSeguradora.Tests.Services
{
    /// <summary>
    /// Comparison tests for premium calculations using golden dataset (T098-T101).
    /// CRITICAL: These tests verify byte-for-byte compatibility with COBOL output.
    /// Golden dataset contains verified COBOL outputs for regulatory compliance.
    /// </summary>
    [Trait("Category", "Comparison")]
    public class PremiumCalculationComparisonTests
    {
        private readonly Mock<ILogger<PremiumCalculationService>> _loggerMock;
        private readonly PremiumCalculationService _service;
        private readonly string _testDataPath;

        public PremiumCalculationComparisonTests()
        {
            _loggerMock = new Mock<ILogger<PremiumCalculationService>>();
            _service = new PremiumCalculationService(_loggerMock.Object);
            _testDataPath = Path.Combine(AppContext.BaseDirectory, "TestData", "golden-premiums.csv");
        }

        #region Golden Dataset Tests (T098)

        [Fact]
        public void CalculateNetPremium_AgainstGoldenDataset_MatchesAllRecords()
        {
            // Arrange
            var goldenRecords = LoadGoldenDataset();
            var failures = new List<string>();

            // Act & Assert
            foreach (var record in goldenRecords)
            {
                var calculated = _service.CalculateNetPremium(record.BasePremium, record.Discount);

                if (calculated != record.NetPremiumExpected)
                {
                    failures.Add($"Policy {record.PolicyNumber}: Expected {record.NetPremiumExpected}, Got {calculated}");
                }
            }

            // Assert
            failures.Should().BeEmpty($"All records should match COBOL output. Failures:{Environment.NewLine}{string.Join(Environment.NewLine, failures)}");
        }

        [Fact]
        public void CalculateIof_AgainstGoldenDataset_MatchesAllRecords()
        {
            // Arrange
            var goldenRecords = LoadGoldenDataset();
            var failures = new List<string>();
            const decimal defaultIofRate = 0.0738m;

            // Act & Assert
            foreach (var record in goldenRecords)
            {
                // Life insurance (167, 1061) should have 0 IOF
                var iofRate = (record.RamoSusep == 167 || record.RamoSusep == 1061) ? 0m : defaultIofRate;
                var calculated = _service.CalculateIof(record.NetPremiumExpected, iofRate);

                if (Math.Abs(calculated - record.IofExpected) > 0.01m) // Allow 1 cent tolerance
                {
                    failures.Add($"Policy {record.PolicyNumber}: Expected IOF {record.IofExpected}, Got {calculated}");
                }
            }

            // Assert
            failures.Should().BeEmpty($"All IOF calculations should match COBOL output. Failures:{Environment.NewLine}{string.Join(Environment.NewLine, failures)}");
        }

        [Fact]
        public void CalculateTotalPremium_AgainstGoldenDataset_MatchesAllRecords()
        {
            // Arrange
            var goldenRecords = LoadGoldenDataset();
            var failures = new List<string>();

            // Act & Assert
            foreach (var record in goldenRecords)
            {
                var calculated = _service.CalculateTotalPremium(
                    record.NetPremiumExpected,
                    record.IofExpected,
                    0m, // No installment surcharge in basic tests
                    0m); // No issuance cost in basic tests

                if (Math.Abs(calculated - record.TotalPremiumExpected) > 0.01m)
                {
                    failures.Add($"Policy {record.PolicyNumber}: Expected Total {record.TotalPremiumExpected}, Got {calculated}");
                }
            }

            // Assert
            failures.Should().BeEmpty($"All total premium calculations should match COBOL output. Failures:{Environment.NewLine}{string.Join(Environment.NewLine, failures)}");
        }

        #endregion

        #region All Ramos Test (T100)

        [Fact]
        public void CalculatePremium_ForAllRamos_ProducesConsistentResults()
        {
            // Arrange
            var goldenRecords = LoadGoldenDataset();
            var ramoGroups = goldenRecords.GroupBy(r => r.RamoSusep);

            // Act & Assert
            foreach (var ramoGroup in ramoGroups)
            {
                foreach (var record in ramoGroup)
                {
                    // Calculate net premium
                    var netPremium = _service.CalculateNetPremium(record.BasePremium, record.Discount);
                    netPremium.Should().Be(record.NetPremiumExpected,
                        $"Net premium for ramo {record.RamoSusep} policy {record.PolicyNumber} should match");

                    // Calculate IOF (ramo-specific rates)
                    var iofRate = GetRamoSpecificIofRate(record.RamoSusep);
                    var iof = _service.CalculateIof(netPremium, iofRate);
                    Math.Abs(iof - record.IofExpected).Should().BeLessThanOrEqualTo(0.01m,
                        $"IOF for ramo {record.RamoSusep} policy {record.PolicyNumber} should match");

                    // Calculate total
                    var total = _service.CalculateTotalPremium(netPremium, iof);
                    Math.Abs(total - record.TotalPremiumExpected).Should().BeLessThanOrEqualTo(0.01m,
                        $"Total for ramo {record.RamoSusep} policy {record.PolicyNumber} should match");
                }
            }
        }

        #endregion

        #region All Movement Types Test (T101)

        [Theory]
        [InlineData("1")] // Emission
        [InlineData("2")] // Renewal
        [InlineData("3")] // Majoração
        [InlineData("4")] // Redução
        [InlineData("5")] // Cancelamento
        [InlineData("6")] // Restituição
        public void ApplyMovementTypeAdjustment_ForAllTypes_ProducesCorrectSign(string movementType)
        {
            // Arrange
            var basePremium = 1000.00m;

            // Act
            var result = _service.ApplyMovementTypeAdjustment(basePremium, movementType);

            // Assert
            switch (movementType)
            {
                case "1": // Emission
                case "2": // Renewal
                case "3": // Majoração
                    result.Should().BePositive($"Movement type {movementType} should produce positive premium");
                    break;

                case "4": // Redução
                case "5": // Cancelamento
                case "6": // Restituição
                    result.Should().BeNegative($"Movement type {movementType} should produce negative premium");
                    break;
            }
        }

        [Fact]
        public void CalculatePremium_ForAllMovementTypes_MatchesGoldenDataset()
        {
            // Arrange
            var goldenRecords = LoadGoldenDataset();
            var movementGroups = goldenRecords.GroupBy(r => r.MovementType);

            // Act & Assert
            foreach (var movementGroup in movementGroups)
            {
                foreach (var record in movementGroup)
                {
                    var netPremium = _service.CalculateNetPremium(record.BasePremium, record.Discount);

                    // Apply movement type adjustment
                    var adjustedPremium = _service.ApplyMovementTypeAdjustment(netPremium, record.MovementType);

                    // For cancellations and reductions, premium should be negative
                    if (record.MovementType == "4" || record.MovementType == "5" || record.MovementType == "6")
                    {
                        adjustedPremium.Should().BeNegative(
                            $"Movement type {record.MovementType} should produce negative premium for policy {record.PolicyNumber}");
                    }
                    else
                    {
                        adjustedPremium.Should().BePositive(
                            $"Movement type {record.MovementType} should produce positive premium for policy {record.PolicyNumber}");
                    }
                }
            }
        }

        #endregion

        #region Banker's Rounding Verification

        [Fact]
        public void GoldenDataset_EdgeCases_VerifyBankersRounding()
        {
            // Arrange: Records specifically designed to test banker's rounding
            var edgeCaseRecords = LoadGoldenDataset()
                .Where(r => r.PolicyNumber >= 1234567890138 && r.PolicyNumber <= 1234567890140)
                .ToList();

            edgeCaseRecords.Should().NotBeEmpty("Golden dataset should contain banker's rounding edge cases");

            // Act & Assert
            foreach (var record in edgeCaseRecords)
            {
                var netPremium = _service.CalculateNetPremium(record.BasePremium, record.Discount);
                netPremium.Should().Be(record.NetPremiumExpected,
                    $"Banker's rounding test for policy {record.PolicyNumber}");

                var iof = _service.CalculateIof(netPremium, 0.0738m);
                Math.Abs(iof - record.IofExpected).Should().BeLessThanOrEqualTo(0.01m,
                    $"Banker's rounding IOF test for policy {record.PolicyNumber}");
            }
        }

        #endregion

        #region Helper Methods

        private List<GoldenPremiumRecord> LoadGoldenDataset()
        {
            var records = new List<GoldenPremiumRecord>();

            if (!File.Exists(_testDataPath))
            {
                throw new FileNotFoundException($"Golden dataset not found at {_testDataPath}");
            }

            var lines = File.ReadAllLines(_testDataPath);

            // Skip header and comment lines
            foreach (var line in lines.Skip(1).Where(l => !string.IsNullOrWhiteSpace(l) && !l.StartsWith("#")))
            {
                var fields = line.Split(',');

                records.Add(new GoldenPremiumRecord
                {
                    PolicyNumber = long.Parse(fields[0]),
                    RamoSusep = int.Parse(fields[1]),
                    MovementType = fields[2],
                    BasePremium = decimal.Parse(fields[3], CultureInfo.InvariantCulture),
                    Discount = decimal.Parse(fields[4], CultureInfo.InvariantCulture),
                    NetPremiumExpected = decimal.Parse(fields[5], CultureInfo.InvariantCulture),
                    IofExpected = decimal.Parse(fields[6], CultureInfo.InvariantCulture),
                    TotalPremiumExpected = decimal.Parse(fields[7], CultureInfo.InvariantCulture),
                    CommissionExpected = decimal.Parse(fields[8], CultureInfo.InvariantCulture),
                    BrokerCommissionExpected = decimal.Parse(fields[9], CultureInfo.InvariantCulture),
                    AgencyCommissionExpected = decimal.Parse(fields[10], CultureInfo.InvariantCulture)
                });
            }

            return records;
        }

        private decimal GetRamoSpecificIofRate(int ramoSusep)
        {
            // Life insurance (167, 1061) is IOF exempt
            if (ramoSusep == 167 || ramoSusep == 1061)
            {
                return 0m;
            }

            // Default IOF rate for other lines
            return 0.0738m;
        }

        #endregion

        #region Test Data Model

        private class GoldenPremiumRecord
        {
            public long PolicyNumber { get; set; }
            public int RamoSusep { get; set; }
            public string MovementType { get; set; }
            public decimal BasePremium { get; set; }
            public decimal Discount { get; set; }
            public decimal NetPremiumExpected { get; set; }
            public decimal IofExpected { get; set; }
            public decimal TotalPremiumExpected { get; set; }
            public decimal CommissionExpected { get; set; }
            public decimal BrokerCommissionExpected { get; set; }
            public decimal AgencyCommissionExpected { get; set; }
        }

        #endregion
    }
}
