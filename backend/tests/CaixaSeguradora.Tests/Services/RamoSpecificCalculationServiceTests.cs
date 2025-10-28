using Xunit;
using FluentAssertions;
using Microsoft.Extensions.Logging;
using Moq;
using CaixaSeguradora.Core.Services;
using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Tests.Services
{
    /// <summary>
    /// Unit tests for RamoSpecificCalculationService (T097).
    /// Tests ramo-specific business rules and validations.
    /// </summary>
    public class RamoSpecificCalculationServiceTests
    {
        private readonly Mock<ILogger<RamoSpecificCalculationService>> _loggerMock;
        private readonly RamoSpecificCalculationService _service;

        public RamoSpecificCalculationServiceTests()
        {
            _loggerMock = new Mock<ILogger<RamoSpecificCalculationService>>();
            _service = new RamoSpecificCalculationService(_loggerMock.Object);
        }

        #region IOF Rate Tests

        [Theory]
        [InlineData(531, 0.0738)] // Auto insurance - standard rate
        [InlineData(541, 0.0738)] // Transportation - standard rate
        [InlineData(167, 0.00)]   // Life insurance - exempt
        [InlineData(1061, 0.00)]  // Life insurance - exempt
        [InlineData(860, 0.0738)] // Health insurance - standard rate
        public void GetRamoSpecificIofRate_ReturnsCorrectRate(int ramoSusep, decimal expectedRate)
        {
            // Act
            var result = _service.GetRamoSpecificIofRate(ramoSusep);

            // Assert
            result.Should().Be(expectedRate);
        }

        #endregion

        #region Bilhete Validation Tests (FR-017)

        [Fact]
        public void ValidateBilheteRequirement_ForGrupoRamo09_WithBilhete_ReturnsTrue()
        {
            // Arrange: Grupo ramo 09 (9xx)
            var premium = new PremiumRecord
            {
                RamoSusep = 900, // Grupo ramo 09
                BilheteNumber = 123456789
            };

            // Act
            var result = _service.ValidateBilheteRequirement(premium);

            // Assert
            result.Should().BeTrue();
        }

        [Fact]
        public void ValidateBilheteRequirement_ForGrupoRamo09_WithoutBilhete_ReturnsFalse()
        {
            // Arrange
            var premium = new PremiumRecord
            {
                RamoSusep = 910, // Grupo ramo 09
                BilheteNumber = null
            };

            // Act
            var result = _service.ValidateBilheteRequirement(premium);

            // Assert
            result.Should().BeFalse();
        }

        [Fact]
        public void ValidateBilheteRequirement_ForOtherRamos_ReturnsTrue()
        {
            // Arrange: Non-grupo-ramo-09
            var premium = new PremiumRecord
            {
                RamoSusep = 531, // Auto - grupo ramo 05
                BilheteNumber = null // Not required
            };

            // Act
            var result = _service.ValidateBilheteRequirement(premium);

            // Assert
            result.Should().BeTrue();
        }

        #endregion

        #region Proposal Date Validation Tests (FR-016)

        [Theory]
        [InlineData(167)]  // Life insurance
        [InlineData(860)]  // Health insurance
        [InlineData(870)]  // Health insurance
        [InlineData(993)]  // Health insurance
        [InlineData(1061)] // Life insurance
        [InlineData(1065)] // Life insurance
        [InlineData(1068)] // Life insurance
        public void ValidateProposalDate_ForSpecificRamos_WithValidDate_ReturnsTrue(int ramoSusep)
        {
            // Arrange
            var premium = new PremiumRecord
            {
                RamoSusep = ramoSusep,
                EffectiveDate = new DateTime(2025, 11, 1)
            };

            var policy = new Policy
            {
                ProposalDate = new DateTime(2025, 10, 15) // Before effective date
            };

            // Act
            var result = _service.ValidateProposalDate(premium, policy);

            // Assert
            result.Should().BeTrue();
        }

        [Fact]
        public void ValidateProposalDate_WithProposalAfterEffective_ReturnsFalse()
        {
            // Arrange
            var premium = new PremiumRecord
            {
                RamoSusep = 167,
                EffectiveDate = new DateTime(2025, 10, 1)
            };

            var policy = new Policy
            {
                ProposalDate = new DateTime(2025, 11, 1) // After effective date - invalid
            };

            // Act
            var result = _service.ValidateProposalDate(premium, policy);

            // Assert
            result.Should().BeFalse();
        }

        [Fact]
        public void ValidateProposalDate_ForNonRequiredRamo_ReturnsTrue()
        {
            // Arrange
            var premium = new PremiumRecord
            {
                RamoSusep = 531, // Auto - not in required list
                EffectiveDate = new DateTime(2025, 10, 1)
            };

            var policy = new Policy
            {
                ProposalDate = new DateTime(2025, 11, 1) // After effective - but not validated
            };

            // Act
            var result = _service.ValidateProposalDate(premium, policy);

            // Assert
            result.Should().BeTrue();
        }

        #endregion

        #region SUSEP Process Number Validation Tests (FR-019)

        [Theory]
        [InlineData(1803)]
        [InlineData(1804)]
        [InlineData(1805)]
        public void ValidateSusepProcessNumber_ForRequiredProducts_WithNumber_ReturnsTrue(short productCode)
        {
            // Arrange
            var product = new Product
            {
                ProductCode = productCode,
                SusepProcessNumber = "15414.901234/2014-87"
            };

            // Act
            var result = _service.ValidateSusepProcessNumber(product);

            // Assert
            result.Should().BeTrue();
        }

        [Theory]
        [InlineData(1803)]
        [InlineData(1804)]
        [InlineData(1805)]
        public void ValidateSusepProcessNumber_ForRequiredProducts_WithoutNumber_ReturnsFalse(short productCode)
        {
            // Arrange
            var product = new Product
            {
                ProductCode = productCode,
                SusepProcessNumber = null
            };

            // Act
            var result = _service.ValidateSusepProcessNumber(product);

            // Assert
            result.Should().BeFalse();
        }

        [Fact]
        public void ValidateSusepProcessNumber_ForNonRequiredProduct_ReturnsTrue()
        {
            // Arrange
            var product = new Product
            {
                ProductCode = 1865, // Not in required list
                SusepProcessNumber = null
            };

            // Act
            var result = _service.ValidateSusepProcessNumber(product);

            // Assert
            result.Should().BeTrue();
        }

        #endregion

        #region Minimum Insured Validation Tests (FR-018)

        [Fact]
        public void ValidateMinimumInsured_WithValidQuantity_ReturnsTrue()
        {
            // Arrange
            var premium = new PremiumRecord
            {
                NumberOfInsured = 5
            };

            // Act
            var result = _service.ValidateMinimumInsured(premium);

            // Assert
            result.Should().BeTrue();
        }

        [Fact]
        public void ValidateMinimumInsured_WithZero_ReturnsFalse()
        {
            // Arrange
            var premium = new PremiumRecord
            {
                NumberOfInsured = 0
            };

            // Act
            var result = _service.ValidateMinimumInsured(premium);

            // Assert
            result.Should().BeFalse();
        }

        [Fact]
        public void ValidateMinimumInsured_WithNegative_ReturnsFalse()
        {
            // Arrange
            var premium = new PremiumRecord
            {
                NumberOfInsured = -1
            };

            // Act
            var result = _service.ValidateMinimumInsured(premium);

            // Assert
            result.Should().BeFalse();
        }

        [Fact]
        public void ValidateMinimumInsured_WithNull_ReturnsTrue()
        {
            // Arrange
            var premium = new PremiumRecord
            {
                NumberOfInsured = null
            };

            // Act
            var result = _service.ValidateMinimumInsured(premium);

            // Assert
            result.Should().BeTrue(); // Null is valid (not validated)
        }

        #endregion

        #region Ramo Adjustments Tests

        [Fact]
        public void ApplyRamoAdjustments_ForAutoInsurance_ReturnsAdjustedPremium()
        {
            // Arrange
            var premium = new PremiumRecord
            {
                RamoSusep = 531, // Auto insurance
                NetPremiumTotal = 1000.00m
            };

            var policy = new Policy();
            var product = new Product { RamoSusep = 531 };

            // Act
            var result = _service.ApplyRamoAdjustments(premium, policy, product);

            // Assert
            result.Should().BeGreaterOrEqualTo(1000.00m); // May be adjusted
        }

        [Fact]
        public void ApplyRamoAdjustments_ForLifeInsuranceWithLargeGroup_AppliesDiscount()
        {
            // Arrange
            var premium = new PremiumRecord
            {
                RamoSusep = 167, // Life insurance
                NetPremiumTotal = 1000.00m,
                NumberOfInsured = 150 // Large group
            };

            var policy = new Policy();
            var product = new Product { RamoSusep = 167 };

            // Act
            var result = _service.ApplyRamoAdjustments(premium, policy, product);

            // Assert
            result.Should().BeLessThan(1000.00m); // 5% discount applied
            result.Should().Be(950.00m); // 1000 * 0.95 = 950
        }

        [Fact]
        public void ApplyRamoAdjustments_ForLifeInsuranceWithSmallGroup_NoDiscount()
        {
            // Arrange
            var premium = new PremiumRecord
            {
                RamoSusep = 167,
                NetPremiumTotal = 1000.00m,
                NumberOfInsured = 50 // Small group
            };

            var policy = new Policy();
            var product = new Product { RamoSusep = 167 };

            // Act
            var result = _service.ApplyRamoAdjustments(premium, policy, product);

            // Assert
            result.Should().Be(1000.00m); // No adjustment
        }

        #endregion
    }
}
