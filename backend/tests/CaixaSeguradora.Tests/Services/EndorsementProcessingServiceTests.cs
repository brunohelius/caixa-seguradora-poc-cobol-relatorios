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
    /// Unit tests for EndorsementProcessingService (T094).
    /// Tests COBOL sections R0800-R0900 endorsement processing logic.
    /// </summary>
    public class EndorsementProcessingServiceTests
    {
        private readonly Mock<ILogger<EndorsementProcessingService>> _loggerMock;
        private readonly Mock<IPremiumCalculationService> _premiumCalculationServiceMock;
        private readonly EndorsementProcessingService _service;

        public EndorsementProcessingServiceTests()
        {
            _loggerMock = new Mock<ILogger<EndorsementProcessingService>>();
            _premiumCalculationServiceMock = new Mock<IPremiumCalculationService>();
            _service = new EndorsementProcessingService(
                _loggerMock.Object,
                _premiumCalculationServiceMock.Object);
        }

        #region Majoração Tests (Movement Type 103)

        [Fact]
        public void ProcessMajoracao_WithValidEndorsement_IncreasesPremiум()
        {
            // Arrange
            var originalPremium = 1000.00m;
            var endorsement = new Endorsement
            {
                PolicyNumber = 123456789,
                EndorsementNumber = 1,
                EndorsementType = "M",
                PremiumImpact = 200.00m,
                IssueDate = DateTime.Now,
                EffectiveDate = DateTime.Now
            };
            var expected = 1200.00m;

            // Act
            var result = _service.ProcessMajoracao(endorsement, originalPremium);

            // Assert
            result.Should().Be(expected);
        }

        [Fact]
        public void ProcessMajoracao_WithNegativeImpact_ConvertsToPositive()
        {
            // Arrange: Some systems may store negative as positive increase
            var originalPremium = 1000.00m;
            var endorsement = new Endorsement
            {
                PolicyNumber = 123456789,
                EndorsementNumber = 1,
                EndorsementType = "M",
                PremiumImpact = -150.00m, // Negative but should be treated as increase
                IssueDate = DateTime.Now,
                EffectiveDate = DateTime.Now
            };
            var expected = 1150.00m;

            // Act
            var result = _service.ProcessMajoracao(endorsement, originalPremium);

            // Assert
            result.Should().Be(expected);
        }

        [Fact]
        public void ProcessMajoracao_WithWrongEndorsementType_ThrowsException()
        {
            // Arrange
            var endorsement = new Endorsement
            {
                EndorsementType = "R", // Wrong type (should be M)
                PremiumImpact = 100.00m
            };

            // Act & Assert
            Assert.Throws<ArgumentException>(() => _service.ProcessMajoracao(endorsement, 1000.00m));
        }

        #endregion

        #region Redução Tests (Movement Type 104)

        [Fact]
        public void ProcessReducao_WithValidEndorsement_DecreasePremium()
        {
            // Arrange
            var originalPremium = 1000.00m;
            var endorsement = new Endorsement
            {
                PolicyNumber = 123456789,
                EndorsementNumber = 1,
                EndorsementType = "R",
                PremiumImpact = 300.00m,
                IssueDate = DateTime.Now,
                EffectiveDate = DateTime.Now
            };
            var expected = 700.00m;

            // Act
            var result = _service.ProcessReducao(endorsement, originalPremium);

            // Assert
            result.Should().Be(expected);
        }

        [Fact]
        public void ProcessReducao_ExceedingOriginalPremium_ReturnsZero()
        {
            // Arrange
            var originalPremium = 500.00m;
            var endorsement = new Endorsement
            {
                PolicyNumber = 123456789,
                EndorsementNumber = 1,
                EndorsementType = "R",
                PremiumImpact = 800.00m, // Exceeds original
                IssueDate = DateTime.Now,
                EffectiveDate = DateTime.Now
            };

            // Act
            var result = _service.ProcessReducao(endorsement, originalPremium);

            // Assert
            result.Should().Be(0m);
        }

        [Fact]
        public void ProcessReducao_WithWrongEndorsementType_ThrowsException()
        {
            // Arrange
            var endorsement = new Endorsement
            {
                EndorsementType = "M", // Wrong type
                PremiumImpact = 100.00m
            };

            // Act & Assert
            Assert.Throws<ArgumentException>(() => _service.ProcessReducao(endorsement, 1000.00m));
        }

        #endregion

        #region Cancelamento Tests (Movement Type 105)

        [Fact]
        public void ProcessCancelamento_ReturnsNegativePremium()
        {
            // Arrange
            var endorsement = new Endorsement
            {
                PolicyNumber = 123456789,
                EndorsementNumber = 1,
                EndorsementType = "C",
                PremiumImpact = 1000.00m,
                IssueDate = DateTime.Now,
                EffectiveDate = DateTime.Now
            };
            var expected = -1000.00m;

            // Act
            var result = _service.ProcessCancelamento(endorsement);

            // Assert
            result.Should().Be(expected);
        }

        [Fact]
        public void ProcessCancelamento_WithNegativeImpact_ConvertsToNegative()
        {
            // Arrange
            var endorsement = new Endorsement
            {
                PolicyNumber = 123456789,
                EndorsementNumber = 1,
                EndorsementType = "C",
                PremiumImpact = -500.00m, // Already negative
                IssueDate = DateTime.Now,
                EffectiveDate = DateTime.Now
            };
            var expected = -500.00m;

            // Act
            var result = _service.ProcessCancelamento(endorsement);

            // Assert
            result.Should().Be(expected);
        }

        #endregion

        #region Pro-Rata Tests

        [Fact]
        public void ApplyProRata_ForMidTermEndorsement_CalculatesCorrectly()
        {
            // Arrange
            var premium = 365.00m; // $1 per day for simplicity
            var effectiveDate = new DateTime(2025, 7, 1); // Mid-year
            var expirationDate = new DateTime(2025, 12, 31); // End of year
            var remainingDays = 184; // July 1 to Dec 31
            var expected = Math.Round(365.00m * (184m / 365m), 2, MidpointRounding.ToEven);
            // Expected: 184.00

            // Act
            var result = _service.ApplyProRata(premium, effectiveDate, expirationDate);

            // Assert
            result.Should().Be(expected);
        }

        [Fact]
        public void ApplyProRata_ForFullYear_ReturnsFullPremium()
        {
            // Arrange
            var premium = 1200.00m;
            var effectiveDate = new DateTime(2025, 1, 1);
            var expirationDate = new DateTime(2025, 12, 31);

            // Act
            var result = _service.ApplyProRata(premium, effectiveDate, expirationDate);

            // Assert
            result.Should().BeApproximately(premium, 1.00m); // Allow small rounding difference
        }

        [Fact]
        public void ApplyProRata_WithExpiredPolicy_ThrowsException()
        {
            // Arrange
            var premium = 1000.00m;
            var effectiveDate = new DateTime(2025, 12, 31);
            var expirationDate = new DateTime(2025, 1, 1); // Before effective date

            // Act & Assert
            Assert.Throws<ArgumentException>(() => _service.ApplyProRata(premium, effectiveDate, expirationDate));
        }

        #endregion

        #region Restituição Tests (Movement Type 106)

        [Fact]
        public void ProcessRestituicao_ReturnsNegativePremium()
        {
            // Arrange
            var endorsement = new Endorsement
            {
                PolicyNumber = 123456789,
                EndorsementNumber = 1,
                PremiumImpact = 250.00m,
                IssueDate = DateTime.Now,
                EffectiveDate = DateTime.Now
            };
            var expected = -250.00m;

            // Act
            var result = _service.ProcessRestituicao(endorsement);

            // Assert
            result.Should().Be(expected);
        }

        #endregion

        #region Complete Endorsement Impact Tests

        [Fact]
        public void CalculateEndorsementImpact_ForMajoracao_WithoutProRata()
        {
            // Arrange
            var originalPremium = 1000.00m;
            var endorsement = new Endorsement
            {
                PolicyNumber = 123456789,
                EndorsementNumber = 1,
                EndorsementType = "M",
                PremiumImpact = 200.00m,
                IssueDate = DateTime.Now,
                EffectiveDate = DateTime.Now
            };
            var expected = 1200.00m;

            // Act
            var result = _service.CalculateEndorsementImpact(endorsement, originalPremium, applyProRata: false);

            // Assert
            result.Should().Be(expected);
        }

        [Fact]
        public void CalculateEndorsementImpact_ForReducao_WithoutProRata()
        {
            // Arrange
            var originalPremium = 1000.00m;
            var endorsement = new Endorsement
            {
                PolicyNumber = 123456789,
                EndorsementNumber = 1,
                EndorsementType = "R",
                PremiumImpact = 300.00m,
                IssueDate = DateTime.Now,
                EffectiveDate = DateTime.Now
            };
            var expected = 700.00m;

            // Act
            var result = _service.CalculateEndorsementImpact(endorsement, originalPremium, applyProRata: false);

            // Assert
            result.Should().Be(expected);
        }

        [Fact]
        public void CalculateEndorsementImpact_ForCancelamento_ReturnsNegative()
        {
            // Arrange
            var originalPremium = 1000.00m;
            var endorsement = new Endorsement
            {
                PolicyNumber = 123456789,
                EndorsementNumber = 1,
                EndorsementType = "C",
                PremiumImpact = 1000.00m,
                IssueDate = DateTime.Now,
                EffectiveDate = DateTime.Now
            };
            var expected = -1000.00m;

            // Act
            var result = _service.CalculateEndorsementImpact(endorsement, originalPremium, applyProRata: false);

            // Assert
            result.Should().Be(expected);
        }

        [Fact]
        public void CalculateEndorsementImpact_WithProRata_AppliesTimeProration()
        {
            // Arrange
            var originalPremium = 1000.00m;
            var endorsement = new Endorsement
            {
                PolicyNumber = 123456789,
                EndorsementNumber = 1,
                EndorsementType = "M",
                PremiumImpact = 200.00m,
                IssueDate = DateTime.Now,
                EffectiveDate = new DateTime(2025, 7, 1),
                EndDate = new DateTime(2025, 12, 31)
            };

            // Act
            var result = _service.CalculateEndorsementImpact(endorsement, originalPremium, applyProRata: true);

            // Assert
            result.Should().BeLessThan(1200.00m); // Should be less than full increase due to pro-rata
            result.Should().BeGreaterThan(1000.00m); // But more than original
        }

        #endregion
    }
}
