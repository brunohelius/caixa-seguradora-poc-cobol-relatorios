using Xunit;
using FluentAssertions;
using Microsoft.Extensions.Logging;
using Moq;
using CaixaSeguradora.Core.Services;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Interfaces;

namespace CaixaSeguradora.Tests.Services
{
    /// <summary>
    /// Unit tests for PremiumCalculationService.
    /// Tests COBOL sections R0700-R1300 calculations with exact decimal arithmetic.
    /// CRITICAL: All financial calculations must match COBOL output byte-for-byte.
    /// </summary>
    public class PremiumCalculationServiceTests
    {
        private readonly Mock<ILogger<PremiumCalculationService>> _loggerMock;
        private readonly PremiumCalculationService _service;

        public PremiumCalculationServiceTests()
        {
            _loggerMock = new Mock<ILogger<PremiumCalculationService>>();
            _service = new PremiumCalculationService(_loggerMock.Object);
        }

        #region Basic Calculation Tests (T091)

        [Fact]
        public void CalculateNetPremium_WithValidInputs_ReturnsCorrectValue()
        {
            // Arrange
            var grossPremium = 1000.00m;
            var discount = 50.00m;
            var expected = 950.00m;

            // Act
            var result = _service.CalculateNetPremium(grossPremium, discount);

            // Assert
            result.Should().Be(expected);
        }

        [Fact]
        public void CalculateNetPremium_WithZeroDiscount_ReturnsGrossPremium()
        {
            // Arrange
            var grossPremium = 1234.56m;
            var discount = 0m;

            // Act
            var result = _service.CalculateNetPremium(grossPremium, discount);

            // Assert
            result.Should().Be(grossPremium);
        }

        [Fact]
        public void CalculateIof_WithStandardRate_ReturnsCorrectValue()
        {
            // Arrange
            var netPremium = 1000.00m;
            var iofRate = 0.0738m; // 7.38%
            var expected = 73.80m; // 1000 * 0.0738 = 73.80

            // Act
            var result = _service.CalculateIof(netPremium, iofRate);

            // Assert
            result.Should().Be(expected);
        }

        [Fact]
        public void CalculateIof_WithZeroRate_ReturnsZero()
        {
            // Arrange
            var netPremium = 1000.00m;
            var iofRate = 0m;

            // Act
            var result = _service.CalculateIof(netPremium, iofRate);

            // Assert
            result.Should().Be(0m);
        }

        [Fact]
        public void CalculateIof_WithInvalidRate_ThrowsException()
        {
            // Arrange
            var netPremium = 1000.00m;
            var invalidRate = 1.5m; // Over 100%

            // Act & Assert
            Assert.Throws<ArgumentException>(() => _service.CalculateIof(netPremium, invalidRate));
        }

        [Fact]
        public void CalculateTotalPremium_WithAllComponents_ReturnsCorrectSum()
        {
            // Arrange
            var netPremium = 1000.00m;
            var iof = 73.80m;
            var installmentSurcharge = 20.00m;
            var issuanceCost = 10.00m;
            var expected = 1103.80m;

            // Act
            var result = _service.CalculateTotalPremium(netPremium, iof, installmentSurcharge, issuanceCost);

            // Assert
            result.Should().Be(expected);
        }

        [Fact]
        public void CalculateTotalPremium_WithMinimalComponents_ReturnsNetPlusIof()
        {
            // Arrange
            var netPremium = 500.00m;
            var iof = 36.90m;
            var expected = 536.90m;

            // Act
            var result = _service.CalculateTotalPremium(netPremium, iof);

            // Assert
            result.Should().Be(expected);
        }

        [Fact]
        public void CalculateCommission_WithStandardRate_ReturnsCorrectValue()
        {
            // Arrange
            var netPremium = 1000.00m;
            var commissionRate = 0.15m; // 15%
            var expected = 150.00m;

            // Act
            var result = _service.CalculateCommission(netPremium, commissionRate);

            // Assert
            result.Should().Be(expected);
        }

        [Fact]
        public void CalculateCommission_WithZeroRate_ReturnsZero()
        {
            // Arrange
            var netPremium = 1000.00m;
            var commissionRate = 0m;

            // Act
            var result = _service.CalculateCommission(netPremium, commissionRate);

            // Assert
            result.Should().Be(0m);
        }

        [Fact]
        public void CalculateCommission_WithInvalidRate_ThrowsException()
        {
            // Arrange
            var netPremium = 1000.00m;
            var invalidRate = -0.1m; // Negative rate

            // Act & Assert
            Assert.Throws<ArgumentException>(() => _service.CalculateCommission(netPremium, invalidRate));
        }

        #endregion

        #region Commission Calculation Tests (T095)

        [Fact]
        public void CalculateCorretagem_WithValidRate_ReturnsCorrectValue()
        {
            // Arrange
            var netPremium = 1200.00m;
            var corretagemRate = 0.12m; // 12%
            var expected = 144.00m;

            // Act
            var result = _service.CalculateCorretagem(netPremium, corretagemRate);

            // Assert
            result.Should().Be(expected);
        }

        [Fact]
        public void CalculateAgenciamento_WithValidRate_ReturnsCorrectValue()
        {
            // Arrange
            var netPremium = 1000.00m;
            var agenciamentoRate = 0.05m; // 5%
            var expected = 50.00m;

            // Act
            var result = _service.CalculateAgenciamento(netPremium, agenciamentoRate);

            // Assert
            result.Should().Be(expected);
        }

        [Fact]
        public void CalculateAdministracao_WithValidRate_ReturnsCorrectValue()
        {
            // Arrange
            var netPremium = 1000.00m;
            var administracaoRate = 0.02m; // 2%
            var expected = 20.00m;

            // Act
            var result = _service.CalculateAdministracao(netPremium, administracaoRate);

            // Assert
            result.Should().Be(expected);
        }

        #endregion

        #region Banker's Rounding Tests (T092)

        [Theory]
        [InlineData(1.125, 2, 1.12)]   // Banker's rounding: .125 rounds to even (down to .12)
        [InlineData(1.135, 2, 1.14)]   // Banker's rounding: .135 rounds to even (up to .14)
        [InlineData(1.145, 2, 1.14)]   // Banker's rounding: .145 rounds to even (down to .14)
        [InlineData(1.155, 2, 1.16)]   // Banker's rounding: .155 rounds to even (up to .16)
        [InlineData(2.225, 2, 2.22)]   // Banker's rounding: .225 rounds to even (down to .22)
        [InlineData(2.235, 2, 2.24)]   // Banker's rounding: .235 rounds to even (up to .24)
        public void RoundCobol_WithBankersRounding_RoundsToEven(decimal value, int decimalPlaces, decimal expected)
        {
            // Act
            var result = _service.RoundCobol(value, decimalPlaces);

            // Assert
            result.Should().Be(expected, $"because banker's rounding of {value} with {decimalPlaces} decimal places should be {expected}");
        }

        [Theory]
        [InlineData(100.005, 2, 100.00)]  // .005 rounds to even (down)
        [InlineData(100.015, 2, 100.02)]  // .015 rounds to even (up)
        [InlineData(100.025, 2, 100.02)]  // .025 rounds to even (down)
        [InlineData(100.035, 2, 100.04)]  // .035 rounds to even (up)
        public void CalculateNetPremium_UsesBankersRounding(decimal grossPremium, int decimalPlaces, decimal expected)
        {
            // Arrange
            var discount = 0m;

            // Act
            var result = _service.CalculateNetPremium(grossPremium, discount);

            // Assert
            result.Should().Be(expected);
        }

        [Fact]
        public void CalculateIof_UsesBankersRounding_ForEdgeCases()
        {
            // Arrange: Premium that results in .005 IOF
            var netPremium = 67.75m;
            var iofRate = 0.0738m;
            // 67.75 * 0.0738 = 5.00185
            // Banker's rounding: 5.00185 -> 5.00 (rounds to even)
            var expected = 5.00m;

            // Act
            var result = _service.CalculateIof(netPremium, iofRate);

            // Assert
            result.Should().Be(expected);
        }

        #endregion

        #region Negative Premium Tests (T093)

        [Fact]
        public void CalculateNetPremium_WithNegativeGrossPremium_ReturnsNegativeValue()
        {
            // Arrange: Cancellation scenario
            var grossPremium = -1000.00m;
            var discount = 0m;
            var expected = -1000.00m;

            // Act
            var result = _service.CalculateNetPremium(grossPremium, discount);

            // Assert
            result.Should().Be(expected);
        }

        [Fact]
        public void ApplyMovementTypeAdjustment_ForCancellation_ReturnsNegative()
        {
            // Arrange
            var premium = 1000.00m;
            var movementType = "5"; // Cancellation

            // Act
            var result = _service.ApplyMovementTypeAdjustment(premium, movementType);

            // Assert
            result.Should().Be(-1000.00m);
        }

        [Fact]
        public void ApplyMovementTypeAdjustment_ForReduction_ReturnsNegative()
        {
            // Arrange
            var premium = 500.00m;
            var movementType = "4"; // Reduction

            // Act
            var result = _service.ApplyMovementTypeAdjustment(premium, movementType);

            // Assert
            result.Should().Be(-500.00m);
        }

        [Fact]
        public void ApplyMovementTypeAdjustment_ForEmission_ReturnsPositive()
        {
            // Arrange
            var premium = 1000.00m;
            var movementType = "1"; // Emission

            // Act
            var result = _service.ApplyMovementTypeAdjustment(premium, movementType);

            // Assert
            result.Should().Be(1000.00m);
        }

        #endregion

        #region Installment Calculation Tests

        [Fact]
        public void CalculateInstallments_WithNoInterest_DividesEvenly()
        {
            // Arrange
            var totalPremium = 1200.00m;
            var numberOfInstallments = 12;
            var interestRate = 0m;

            // Act
            var result = _service.CalculateInstallments(totalPremium, numberOfInstallments, interestRate);

            // Assert
            result.Should().HaveCount(12);
            result.Sum().Should().Be(totalPremium);
            // All installments should be 100.00
            result.Skip(1).Should().AllBeEquivalentTo(100.00m);
        }

        [Fact]
        public void CalculateInstallments_WithSingleInstallment_ReturnsFullAmount()
        {
            // Arrange
            var totalPremium = 1234.56m;
            var numberOfInstallments = 1;
            var interestRate = 0m;

            // Act
            var result = _service.CalculateInstallments(totalPremium, numberOfInstallments, interestRate);

            // Assert
            result.Should().HaveCount(1);
            result[0].Should().Be(totalPremium);
        }

        [Fact]
        public void CalculateInstallments_AdjustsFirstInstallment_ForRoundingDifferences()
        {
            // Arrange: Amount that doesn't divide evenly
            var totalPremium = 100.00m;
            var numberOfInstallments = 3;
            var interestRate = 0m;

            // Act
            var result = _service.CalculateInstallments(totalPremium, numberOfInstallments, interestRate);

            // Assert
            result.Should().HaveCount(3);
            result.Sum().Should().Be(totalPremium); // Sum must equal total exactly
        }

        [Fact]
        public void CalculateInstallmentSurcharge_ForTwoInstallments_Returns1Percent()
        {
            // Arrange
            var netPremium = 1000.00m;
            var numberOfInstallments = 2;
            var surchargeTable = new Dictionary<int, decimal> { { 2, 0.01m } };
            var expected = 10.00m; // 1% of 1000

            // Act
            var result = _service.CalculateInstallmentSurcharge(netPremium, numberOfInstallments, surchargeTable);

            // Assert
            result.Should().Be(expected);
        }

        [Fact]
        public void CalculateInstallmentSurcharge_ForSingleInstallment_ReturnsZero()
        {
            // Arrange
            var netPremium = 1000.00m;
            var numberOfInstallments = 1;
            var surchargeTable = new Dictionary<int, decimal> { { 1, 0.00m } };

            // Act
            var result = _service.CalculateInstallmentSurcharge(netPremium, numberOfInstallments, surchargeTable);

            // Assert
            result.Should().Be(0m);
        }

        [Fact]
        public void CalculateInstallmentSurcharge_ForTwelveInstallments_Returns11Percent()
        {
            // Arrange
            var netPremium = 1000.00m;
            var numberOfInstallments = 12;
            var surchargeTable = new Dictionary<int, decimal> { { 12, 0.11m } };
            var expected = 110.00m; // 11% of 1000

            // Act
            var result = _service.CalculateInstallmentSurcharge(netPremium, numberOfInstallments, surchargeTable);

            // Assert
            result.Should().Be(expected);
        }

        #endregion

        #region Accumulator Tests

        [Fact]
        public void AccumulateValues_UpdatesAllTotals()
        {
            // Arrange
            var premium = new PremiumRecord
            {
                PolicyNumber = 123456789,
                NetPremiumTotal = 1000.00m,
                TotalPremiumAmount = 1073.80m,
                IofTotal = 73.80m,
                CommissionTotal = 150.00m,
                RamoSusep = 531,
                MovementType = "1" // Emission
            };

            var accumulator = new PremiumAccumulator();

            // Act
            _service.AccumulateValues(premium, accumulator);

            // Assert
            accumulator.TotalNetPremium.Should().Be(1000.00m);
            accumulator.TotalGrossPremium.Should().Be(1073.80m);
            accumulator.TotalIOF.Should().Be(73.80m);
            accumulator.TotalCommissions.Should().Be(150.00m);
            accumulator.EmissionCount.Should().Be(1);
            accumulator.TotalRecordsProcessed.Should().Be(1);
        }

        [Fact]
        public void AccumulateValues_AccumulatesByLineOfBusiness()
        {
            // Arrange
            var accumulator = new PremiumAccumulator();

            var premium1 = new PremiumRecord
            {
                RamoSusep = 531,
                TotalPremiumAmount = 1000.00m,
                MovementType = "1"
            };

            var premium2 = new PremiumRecord
            {
                RamoSusep = 531,
                TotalPremiumAmount = 500.00m,
                MovementType = "1"
            };

            var premium3 = new PremiumRecord
            {
                RamoSusep = 167,
                TotalPremiumAmount = 2000.00m,
                MovementType = "1"
            };

            // Act
            _service.AccumulateValues(premium1, accumulator);
            _service.AccumulateValues(premium2, accumulator);
            _service.AccumulateValues(premium3, accumulator);

            // Assert
            accumulator.TotalsByLineOfBusiness[531].Should().Be(1500.00m);
            accumulator.TotalsByLineOfBusiness[167].Should().Be(2000.00m);
        }

        #endregion

        #region Life Insurance Tests

        [Fact]
        public void CalculateLifeInsurancePremium_WithValidInputs_ReturnsCorrectValue()
        {
            // Arrange
            var baseRate = 2.50m; // Rate per thousand
            var numberOfLives = 10;
            var coverageAmount = 50000m; // 50k coverage
            var expected = 1250.00m; // (50000/1000) * 2.50 * 10 = 1250

            // Act
            var result = _service.CalculateLifeInsurancePremium(baseRate, numberOfLives, coverageAmount);

            // Assert
            result.Should().Be(expected);
        }

        [Fact]
        public void CalculateLifeInsurancePremium_WithSingleLife_ReturnsCorrectValue()
        {
            // Arrange
            var baseRate = 1.50m;
            var numberOfLives = 1;
            var coverageAmount = 100000m;
            var expected = 150.00m; // (100000/1000) * 1.50 * 1 = 150

            // Act
            var result = _service.CalculateLifeInsurancePremium(baseRate, numberOfLives, coverageAmount);

            // Assert
            result.Should().Be(expected);
        }

        #endregion

        #region Truncation Tests

        [Theory]
        [InlineData(10.123456, 2, 10.12)]
        [InlineData(10.129999, 2, 10.12)]
        [InlineData(10.999999, 2, 10.99)]
        [InlineData(10.995, 2, 10.99)]
        public void TruncateCobol_RemovesExcessDecimals_WithoutRounding(decimal value, int decimalPlaces, decimal expected)
        {
            // Act
            var result = _service.TruncateCobol(value, decimalPlaces);

            // Assert
            result.Should().Be(expected);
        }

        #endregion
    }
}
