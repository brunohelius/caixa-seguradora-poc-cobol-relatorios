using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Interfaces;
using CaixaSeguradora.Core.Services;
using FluentAssertions;
using Moq;
using Xunit;

namespace CaixaSeguradora.UnitTests.Services;

/// <summary>
/// Unit tests for CossuranceService.
/// Tests cossurance calculations including ceded/retained premiums, distribution logic, and percentage validation.
/// Target: 90%+ code coverage.
/// </summary>
public class CossuranceServiceTests
{
    private readonly Mock<ICossuredPolicyRepository> _mockCossuredPolicyRepo;
    private readonly Mock<ICossuranceCalculationRepository> _mockCossuranceCalcRepo;
    private readonly ICossuranceService _service;

    public CossuranceServiceTests()
    {
        _mockCossuredPolicyRepo = new Mock<ICossuredPolicyRepository>();
        _mockCossuranceCalcRepo = new Mock<ICossuranceCalculationRepository>();
        _service = new CossuranceService(_mockCossuredPolicyRepo.Object, _mockCossuranceCalcRepo.Object);
    }

    #region CalculateCededPremium Tests

    [Fact]
    public void CalculateCededPremium_WithValidInputs_ReturnsCorrectAmount()
    {
        // Arrange
        var totalPremium = 10000.00m;
        var cededPercentage = 0.30m; // 30%

        // Act
        var result = _service.CalculateCededPremium(totalPremium, cededPercentage);

        // Assert
        // 10000 * 0.30 = 3000.00
        result.Should().Be(3000.00m);
    }

    [Fact]
    public void CalculateCededPremium_WithRounding_UsesCobolRounding()
    {
        // Arrange
        var totalPremium = 10000.00m;
        var cededPercentage = 0.333m; // 33.3%

        // Act
        var result = _service.CalculateCededPremium(totalPremium, cededPercentage);

        // Assert
        // 10000 * 0.333 = 3330.00
        result.Should().Be(3330.00m);
    }

    [Fact]
    public void CalculateCededPremium_WithZeroPercentage_ReturnsZero()
    {
        // Arrange
        var totalPremium = 10000.00m;
        var cededPercentage = 0m;

        // Act
        var result = _service.CalculateCededPremium(totalPremium, cededPercentage);

        // Assert
        result.Should().Be(0m);
    }

    [Fact]
    public void CalculateCededPremium_WithFullPercentage_ReturnsTotalPremium()
    {
        // Arrange
        var totalPremium = 10000.00m;
        var cededPercentage = 1.0m; // 100%

        // Act
        var result = _service.CalculateCededPremium(totalPremium, cededPercentage);

        // Assert
        result.Should().Be(10000.00m);
    }

    [Fact]
    public void CalculateCededPremium_WithNegativePremium_ThrowsArgumentException()
    {
        // Arrange
        var totalPremium = -10000.00m;
        var cededPercentage = 0.30m;

        // Act
        Action act = () => _service.CalculateCededPremium(totalPremium, cededPercentage);

        // Assert
        act.Should().Throw<ArgumentException>()
            .WithParameterName("totalPremium")
            .WithMessage("*cannot be negative*");
    }

    [Fact]
    public void CalculateCededPremium_WithNegativePercentage_ThrowsArgumentException()
    {
        // Arrange
        var totalPremium = 10000.00m;
        var cededPercentage = -0.30m;

        // Act
        Action act = () => _service.CalculateCededPremium(totalPremium, cededPercentage);

        // Assert
        act.Should().Throw<ArgumentException>()
            .WithParameterName("cededPercentage")
            .WithMessage("*must be between 0 and 1*");
    }

    [Fact]
    public void CalculateCededPremium_WithPercentageAboveOne_ThrowsArgumentException()
    {
        // Arrange
        var totalPremium = 10000.00m;
        var cededPercentage = 1.5m; // 150% - invalid

        // Act
        Action act = () => _service.CalculateCededPremium(totalPremium, cededPercentage);

        // Assert
        act.Should().Throw<ArgumentException>()
            .WithParameterName("cededPercentage");
    }

    #endregion

    #region CalculateRetainedPremium Tests

    [Fact]
    public void CalculateRetainedPremium_WithValidInputs_ReturnsCorrectAmount()
    {
        // Arrange
        var totalPremium = 10000.00m;
        var cededPremium = 3000.00m;

        // Act
        var result = _service.CalculateRetainedPremium(totalPremium, cededPremium);

        // Assert
        // 10000 - 3000 = 7000
        result.Should().Be(7000.00m);
    }

    [Fact]
    public void CalculateRetainedPremium_WithNoCeded_ReturnsTotalPremium()
    {
        // Arrange
        var totalPremium = 10000.00m;
        var cededPremium = 0m;

        // Act
        var result = _service.CalculateRetainedPremium(totalPremium, cededPremium);

        // Assert
        result.Should().Be(10000.00m);
    }

    [Fact]
    public void CalculateRetainedPremium_WithFullyCeded_ReturnsZero()
    {
        // Arrange
        var totalPremium = 10000.00m;
        var cededPremium = 10000.00m;

        // Act
        var result = _service.CalculateRetainedPremium(totalPremium, cededPremium);

        // Assert
        result.Should().Be(0m);
    }

    [Fact]
    public void CalculateRetainedPremium_WithRounding_UsesCobolRounding()
    {
        // Arrange
        var totalPremium = 10000.00m;
        var cededPremium = 3333.33m;

        // Act
        var result = _service.CalculateRetainedPremium(totalPremium, cededPremium);

        // Assert
        // 10000 - 3333.33 = 6666.67
        result.Should().Be(6666.67m);
    }

    [Fact]
    public void CalculateRetainedPremium_WithNegativeTotalPremium_ThrowsArgumentException()
    {
        // Arrange
        var totalPremium = -10000.00m;
        var cededPremium = 3000.00m;

        // Act
        Action act = () => _service.CalculateRetainedPremium(totalPremium, cededPremium);

        // Assert
        act.Should().Throw<ArgumentException>()
            .WithParameterName("totalPremium");
    }

    [Fact]
    public void CalculateRetainedPremium_WithNegativeCededPremium_ThrowsArgumentException()
    {
        // Arrange
        var totalPremium = 10000.00m;
        var cededPremium = -3000.00m;

        // Act
        Action act = () => _service.CalculateRetainedPremium(totalPremium, cededPremium);

        // Assert
        act.Should().Throw<ArgumentException>()
            .WithParameterName("cededPremium");
    }

    [Fact]
    public void CalculateRetainedPremium_WithCededExceedingTotal_ThrowsArgumentException()
    {
        // Arrange
        var totalPremium = 10000.00m;
        var cededPremium = 15000.00m; // Exceeds total

        // Act
        Action act = () => _service.CalculateRetainedPremium(totalPremium, cededPremium);

        // Assert
        act.Should().Throw<ArgumentException>()
            .WithParameterName("cededPremium")
            .WithMessage("*cannot exceed total premium*");
    }

    #endregion

    #region DistributeCededPremium Tests

    [Fact]
    public void DistributeCededPremium_WithTwoParticipants_DistributesCorrectly()
    {
        // Arrange
        var cededPremium = 3000.00m;
        var participants = new List<CossuranceParticipant>
        {
            new() { CompanyCode = 101, QuotaPercentage = 0.60m }, // 60%
            new() { CompanyCode = 102, QuotaPercentage = 0.40m }  // 40%
        };

        // Act
        Dictionary<int, decimal> result = _service.DistributeCededPremium(cededPremium, participants);

        // Assert
        result.Should().HaveCount(2);
        result[101].Should().Be(1800.00m); // 3000 * 0.60 = 1800
        result[102].Should().Be(1200.00m); // 3000 * 0.40 = 1200
        result.Values.Sum().Should().Be(cededPremium); // Critical: sum must equal total
    }

    [Fact]
    public void DistributeCededPremium_WithThreeParticipants_HandlesRoundingCorrectly()
    {
        // Arrange
        var cededPremium = 10000.00m;
        var participants = new List<CossuranceParticipant>
        {
            new() { CompanyCode = 101, QuotaPercentage = 0.333m }, // 33.3%
            new() { CompanyCode = 102, QuotaPercentage = 0.333m }, // 33.3%
            new() { CompanyCode = 103, QuotaPercentage = 0.334m }  // 33.4%
        };

        // Act
        Dictionary<int, decimal> result = _service.DistributeCededPremium(cededPremium, participants);

        // Assert
        result.Should().HaveCount(3);
        // First two: 10000 * 0.333 = 3330.00 each
        result[101].Should().Be(3330.00m);
        result[102].Should().Be(3330.00m);
        // Last gets remainder to ensure exact total
        result[103].Should().Be(3340.00m); // 10000 - 3330 - 3330 = 3340
        result.Values.Sum().Should().Be(cededPremium); // Critical validation
    }

    [Fact]
    public void DistributeCededPremium_WithSingleParticipant_GivesFullAmount()
    {
        // Arrange
        var cededPremium = 5000.00m;
        var participants = new List<CossuranceParticipant>
        {
            new() { CompanyCode = 101, QuotaPercentage = 1.0m } // 100%
        };

        // Act
        Dictionary<int, decimal> result = _service.DistributeCededPremium(cededPremium, participants);

        // Assert
        result.Should().HaveCount(1);
        result[101].Should().Be(5000.00m);
    }

    [Fact]
    public void DistributeCededPremium_WithInvalidPercentages_ThrowsArgumentException()
    {
        // Arrange
        var cededPremium = 10000.00m;
        var participants = new List<CossuranceParticipant>
        {
            new() { CompanyCode = 101, QuotaPercentage = 0.50m }, // 50%
            new() { CompanyCode = 102, QuotaPercentage = 0.40m }  // 40% - total = 90%, not 100%
        };

        // Act
        Action act = () => _service.DistributeCededPremium(cededPremium, participants);

        // Assert
        act.Should().Throw<ArgumentException>()
            .WithParameterName("participants")
            .WithMessage("*sum must equal 100%*");
    }

    [Fact]
    public void DistributeCededPremium_WithEmptyList_ThrowsArgumentException()
    {
        // Arrange
        var cededPremium = 10000.00m;
        var participants = new List<CossuranceParticipant>();

        // Act
        Action act = () => _service.DistributeCededPremium(cededPremium, participants);

        // Assert
        act.Should().Throw<ArgumentException>()
            .WithParameterName("participants")
            .WithMessage("*at least one participant*");
    }

    [Fact]
    public void DistributeCededPremium_WithNullList_ThrowsArgumentNullException()
    {
        // Arrange
        var cededPremium = 10000.00m;
        List<CossuranceParticipant> participants = null!;

        // Act
        Action act = () => _service.DistributeCededPremium(cededPremium, participants);

        // Assert
        act.Should().Throw<ArgumentNullException>()
            .WithParameterName("participants");
    }

    #endregion

    #region ValidateCossurancePercentages Tests

    [Fact]
    public void ValidateCossurancePercentages_WithValidPercentages_ReturnsTrue()
    {
        // Arrange
        var participants = new List<CossuranceParticipant>
        {
            new() { QuotaPercentage = 0.60m },
            new() { QuotaPercentage = 0.40m }
        };

        // Act
        var result = _service.ValidateCossurancePercentages(participants);

        // Assert
        result.Should().BeTrue();
    }

    [Fact]
    public void ValidateCossurancePercentages_WithRoundingTolerance_ReturnsTrue()
    {
        // Arrange - sum = 0.9999 (within tolerance)
        var participants = new List<CossuranceParticipant>
        {
            new() { QuotaPercentage = 0.3333m },
            new() { QuotaPercentage = 0.3333m },
            new() { QuotaPercentage = 0.3333m }
        };

        // Act
        var result = _service.ValidateCossurancePercentages(participants);

        // Assert
        result.Should().BeTrue(); // Within tolerance
    }

    [Fact]
    public void ValidateCossurancePercentages_WithInvalidSum_ReturnsFalse()
    {
        // Arrange - sum = 0.90 (not 1.0)
        var participants = new List<CossuranceParticipant>
        {
            new() { QuotaPercentage = 0.50m },
            new() { QuotaPercentage = 0.40m }
        };

        // Act
        var result = _service.ValidateCossurancePercentages(participants);

        // Assert
        result.Should().BeFalse();
    }

    [Fact]
    public void ValidateCossurancePercentages_WithNegativePercentage_ReturnsFalse()
    {
        // Arrange
        var participants = new List<CossuranceParticipant>
        {
            new() { QuotaPercentage = 1.20m },
            new() { QuotaPercentage = -0.20m } // Invalid negative
        };

        // Act
        var result = _service.ValidateCossurancePercentages(participants);

        // Assert
        result.Should().BeFalse();
    }

    [Fact]
    public void ValidateCossurancePercentages_WithPercentageAboveOne_ReturnsFalse()
    {
        // Arrange
        var participants = new List<CossuranceParticipant>
        {
            new() { QuotaPercentage = 1.5m } // Invalid > 1
        };

        // Act
        var result = _service.ValidateCossurancePercentages(participants);

        // Assert
        result.Should().BeFalse();
    }

    [Fact]
    public void ValidateCossurancePercentages_WithEmptyList_ReturnsFalse()
    {
        // Arrange
        var participants = new List<CossuranceParticipant>();

        // Act
        var result = _service.ValidateCossurancePercentages(participants);

        // Assert
        result.Should().BeFalse();
    }

    [Fact]
    public void ValidateCossurancePercentages_WithNullList_ThrowsArgumentNullException()
    {
        // Arrange
        List<CossuranceParticipant> participants = null!;

        // Act
        Action act = () => _service.ValidateCossurancePercentages(participants);

        // Assert
        act.Should().Throw<ArgumentNullException>();
    }

    #endregion

    #region CalculateAcquisitionCommission Tests

    [Fact]
    public void CalculateAcquisitionCommission_WithValidInputs_ReturnsCorrectCommission()
    {
        // Arrange
        var acquiredPremium = 5000.00m;
        var commissionRate = 0.10m; // 10%

        // Act
        var result = _service.CalculateAcquisitionCommission(acquiredPremium, commissionRate);

        // Assert
        // 5000 * 0.10 = 500.00
        result.Should().Be(500.00m);
    }

    [Fact]
    public void CalculateAcquisitionCommission_WithZeroRate_ReturnsZero()
    {
        // Arrange
        var acquiredPremium = 5000.00m;
        var commissionRate = 0m;

        // Act
        var result = _service.CalculateAcquisitionCommission(acquiredPremium, commissionRate);

        // Assert
        result.Should().Be(0m);
    }

    [Fact]
    public void CalculateAcquisitionCommission_WithNegativePremium_ThrowsArgumentException()
    {
        // Arrange
        var acquiredPremium = -5000.00m;
        var commissionRate = 0.10m;

        // Act
        Action act = () => _service.CalculateAcquisitionCommission(acquiredPremium, commissionRate);

        // Assert
        act.Should().Throw<ArgumentException>()
            .WithParameterName("acquiredPremium");
    }

    [Fact]
    public void CalculateAcquisitionCommission_WithInvalidRate_ThrowsArgumentException()
    {
        // Arrange
        var acquiredPremium = 5000.00m;
        var commissionRate = 1.5m; // 150% - invalid

        // Act
        Action act = () => _service.CalculateAcquisitionCommission(acquiredPremium, commissionRate);

        // Assert
        act.Should().Throw<ArgumentException>()
            .WithParameterName("commissionRate");
    }

    #endregion

    #region GetCossuranceType Tests

    [Fact]
    public void GetCossuranceType_WhenCeding_ReturnsC()
    {
        // Arrange
        var companyCode = 100;
        var cedingCompanyCode = 100; // Same as company
        var acquiringCompanyCode = 200;

        // Act
        var result = _service.GetCossuranceType(companyCode, cedingCompanyCode, acquiringCompanyCode);

        // Assert
        result.Should().Be('C'); // Ceded
    }

    [Fact]
    public void GetCossuranceType_WhenAcquiring_ReturnsA()
    {
        // Arrange
        var companyCode = 200;
        var cedingCompanyCode = 100;
        var acquiringCompanyCode = 200; // Same as company

        // Act
        var result = _service.GetCossuranceType(companyCode, cedingCompanyCode, acquiringCompanyCode);

        // Assert
        result.Should().Be('A'); // Acquired
    }

    [Fact]
    public void GetCossuranceType_WhenNeither_ReturnsU()
    {
        // Arrange
        var companyCode = 300;
        var cedingCompanyCode = 100;
        var acquiringCompanyCode = 200;

        // Act
        var result = _service.GetCossuranceType(companyCode, cedingCompanyCode, acquiringCompanyCode);

        // Assert
        result.Should().Be('U'); // Unknown
    }

    #endregion

    #region AccumulateCossuranceValues Tests

    [Fact]
    public void AccumulateCossuranceValues_WithValidInputs_UpdatesAccumulator()
    {
        // Arrange
        var cededPremium = 3000.00m;
        var retainedPremium = 7000.00m;
        var accumulator = new CossuranceAccumulator();

        // Act
        _service.AccumulateCossuranceValues(cededPremium, retainedPremium, accumulator);

        // Assert
        accumulator.TotalCededPremium.Should().Be(3000.00m);
        accumulator.TotalRetainedPremium.Should().Be(7000.00m);
        accumulator.CededRecordCount.Should().Be(1);
    }

    [Fact]
    public void AccumulateCossuranceValues_WithZeroCeded_DoesNotIncrementCount()
    {
        // Arrange
        var cededPremium = 0m;
        var retainedPremium = 10000.00m;
        var accumulator = new CossuranceAccumulator();

        // Act
        _service.AccumulateCossuranceValues(cededPremium, retainedPremium, accumulator);

        // Assert
        accumulator.TotalCededPremium.Should().Be(0m);
        accumulator.TotalRetainedPremium.Should().Be(10000.00m);
        accumulator.CededRecordCount.Should().Be(0); // Not incremented
    }

    [Fact]
    public void AccumulateCossuranceValues_MultipleRecords_SumsCorrectly()
    {
        // Arrange
        var accumulator = new CossuranceAccumulator();

        // Act
        _service.AccumulateCossuranceValues(3000.00m, 7000.00m, accumulator);
        _service.AccumulateCossuranceValues(2000.00m, 8000.00m, accumulator);

        // Assert
        accumulator.TotalCededPremium.Should().Be(5000.00m);
        accumulator.TotalRetainedPremium.Should().Be(15000.00m);
        accumulator.CededRecordCount.Should().Be(2);
    }

    [Fact]
    public void AccumulateCossuranceValues_WithNullAccumulator_ThrowsArgumentNullException()
    {
        // Arrange
        CossuranceAccumulator accumulator = null!;

        // Act
        Action act = () => _service.AccumulateCossuranceValues(1000m, 9000m, accumulator);

        // Assert
        act.Should().Throw<ArgumentNullException>()
            .WithParameterName("accumulator");
    }

    #endregion

    #region CalculateQuotaAdjustment Tests

    [Fact]
    public void CalculateQuotaAdjustment_WithIncreasedQuota_ReturnsPositiveAdjustment()
    {
        // Arrange
        var originalPremium = 10000.00m;
        var originalQuota = 0.30m; // 30%
        var newQuota = 0.40m;      // 40%

        // Act
        var result = _service.CalculateQuotaAdjustment(originalPremium, originalQuota, newQuota);

        // Assert
        // Adjustment: 10000 * (0.40 - 0.30) = 10000 * 0.10 = 1000.00
        result.Should().Be(1000.00m);
    }

    [Fact]
    public void CalculateQuotaAdjustment_WithDecreasedQuota_ReturnsNegativeAdjustment()
    {
        // Arrange
        var originalPremium = 10000.00m;
        var originalQuota = 0.40m; // 40%
        var newQuota = 0.30m;      // 30%

        // Act
        var result = _service.CalculateQuotaAdjustment(originalPremium, originalQuota, newQuota);

        // Assert
        // Adjustment: 10000 * (0.30 - 0.40) = 10000 * -0.10 = -1000.00
        result.Should().Be(-1000.00m);
    }

    [Fact]
    public void CalculateQuotaAdjustment_WithNoChange_ReturnsZero()
    {
        // Arrange
        var originalPremium = 10000.00m;
        var originalQuota = 0.30m;
        var newQuota = 0.30m;

        // Act
        var result = _service.CalculateQuotaAdjustment(originalPremium, originalQuota, newQuota);

        // Assert
        result.Should().Be(0m);
    }

    [Fact]
    public void CalculateQuotaAdjustment_WithInvalidQuotas_ThrowsArgumentException()
    {
        // Arrange
        var originalPremium = 10000.00m;

        // Act & Assert - invalid original quota
        Action act1 = () => _service.CalculateQuotaAdjustment(originalPremium, -0.1m, 0.3m);
        act1.Should().Throw<ArgumentException>()
            .WithParameterName("originalQuota");

        // Act & Assert - invalid new quota
        Action act2 = () => _service.CalculateQuotaAdjustment(originalPremium, 0.3m, 1.5m);
        act2.Should().Throw<ArgumentException>()
            .WithParameterName("newQuota");
    }

    #endregion

    #region GenerateCossuranceOutputRecord Tests

    [Fact]
    public void GenerateCossuranceOutputRecord_WithValidInputs_GeneratesFixedWidthRecord()
    {
        // Arrange
        var premium = new PremiumRecord
        {
            SystemCode = "GL",
            PolicyNumber = 123456789012345,
            EndorsementNumber = 1,
            LineOfBusiness = 531,
            ProductCode = 1001
        };
        var cossuredPolicy = new CossuredPolicy
        {
            CossurerCode = 100200,
            CossurerName = "Test Cossurer Company Ltd",
            PercentageShare = 30.50m,
            CossuranceType = "C"
        };
        var calculation = new CossuranceCalculation
        {
            TotalGrossPremium = 10000.00m,
            TotalNetPremium = 9000.00m,
            CededPremium = 2745.00m,
            CededCommission = 274.50m,
            TotalIOF = 738.00m
        };

        // Act
        var result = _service.GenerateCossuranceOutputRecord(premium, cossuredPolicy, calculation);

        // Assert
        result.Should().NotBeNullOrEmpty();
        result.Should().StartWith("GL"); // System code
        result.Length.Should().BeGreaterThan(100); // Fixed-width format should be substantial
    }

    [Fact]
    public void GenerateCossuranceOutputRecord_WithNullPremium_ThrowsArgumentNullException()
    {
        // Arrange
        PremiumRecord premium = null!;
        var cossuredPolicy = new CossuredPolicy();
        var calculation = new CossuranceCalculation();

        // Act
        Action act = () => _service.GenerateCossuranceOutputRecord(premium, cossuredPolicy, calculation);

        // Assert
        act.Should().Throw<ArgumentNullException>()
            .WithParameterName("premium");
    }

    [Fact]
    public void GenerateCossuranceOutputRecord_WithNullCossuredPolicy_ThrowsArgumentNullException()
    {
        // Arrange
        var premium = new PremiumRecord();
        CossuredPolicy cossuredPolicy = null!;
        var calculation = new CossuranceCalculation();

        // Act
        Action act = () => _service.GenerateCossuranceOutputRecord(premium, cossuredPolicy, calculation);

        // Assert
        act.Should().Throw<ArgumentNullException>()
            .WithParameterName("cossuredPolicy");
    }

    [Fact]
    public void GenerateCossuranceOutputRecord_WithNullCalculation_ThrowsArgumentNullException()
    {
        // Arrange
        var premium = new PremiumRecord();
        var cossuredPolicy = new CossuredPolicy();
        CossuranceCalculation calculation = null!;

        // Act
        Action act = () => _service.GenerateCossuranceOutputRecord(premium, cossuredPolicy, calculation);

        // Assert
        act.Should().Throw<ArgumentNullException>()
            .WithParameterName("calculation");
    }

    #endregion
}
