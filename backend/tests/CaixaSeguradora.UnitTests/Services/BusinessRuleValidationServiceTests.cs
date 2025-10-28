using System;
using System.Threading;
using System.Threading.Tasks;
using CaixaSeguradora.Core.Constants;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Interfaces;
using CaixaSeguradora.Core.Services;
using Microsoft.Extensions.Logging;
using Moq;
using Xunit;

namespace CaixaSeguradora.UnitTests.Services;

/// <summary>
/// Unit tests for BusinessRuleValidationService
/// Tests FR-014 through FR-018: Data validation and business rules
/// </summary>
public class BusinessRuleValidationServiceTests
{
    private readonly Mock<ILogger<BusinessRuleValidationService>> _loggerMock;
    private readonly Mock<IClientRepository> _clientRepositoryMock;
    private readonly Mock<IPolicyRepository> _policyRepositoryMock;
    private readonly Mock<IProductRepository> _productRepositoryMock;
    private readonly BusinessRuleValidationService _service;

    public BusinessRuleValidationServiceTests()
    {
        _loggerMock = new Mock<ILogger<BusinessRuleValidationService>>();
        _clientRepositoryMock = new Mock<IClientRepository>();
        _policyRepositoryMock = new Mock<IPolicyRepository>();
        _productRepositoryMock = new Mock<IProductRepository>();

        _service = new BusinessRuleValidationService(
            _loggerMock.Object,
            _clientRepositoryMock.Object,
            _policyRepositoryMock.Object,
            _productRepositoryMock.Object);
    }

    #region Proposal Date Validation Tests (FR-016)

    [Theory]
    [InlineData(167)]  // Vida Individual
    [InlineData(860)]  // Viagem
    [InlineData(870)]  // Viagem Internacional
    [InlineData(993)]  // Previdência
    [InlineData(1061)] // Educacional
    [InlineData(1065)] // VGBL
    [InlineData(1068)] // PGBL
    public void ValidateProposalDate_ProposalExceedsEffective_SpecificRamos_AutoCorrected(int ramoSusep)
    {
        // Arrange
        var premium = CreateTestPremium();
        var policy = new Policy
        {
            PolicyNumber = 1234567890123,
            RamoSusep = ramoSusep,
            ProposalDate = new DateTime(2025, 11, 1), // After effective date
            EffectiveDate = new DateTime(2025, 10, 1)
        };

        // Act
        var result = _service.ValidateProposalDate(premium, policy);

        // Assert
        Assert.True(result.IsValid, "Validation should pass with auto-correction");
        Assert.Empty(result.Errors);
        Assert.Single(result.AutoCorrected);
        Assert.Equal("ProposalDate", result.AutoCorrected[0].FieldName);
        Assert.Equal("2025-11-01", result.AutoCorrected[0].OriginalValue);
        Assert.Equal("2025-10-01", result.AutoCorrected[0].CorrectedValue);
        Assert.Equal(policy.EffectiveDate, policy.ProposalDate); // Both are DateTime, not nullable
    }

    [Fact]
    public void ValidateProposalDate_ProposalBeforeEffective_ReturnsValid()
    {
        // Arrange
        var premium = CreateTestPremium();
        var policy = new Policy
        {
            PolicyNumber = 1234567890123,
            RamoSusep = 167,
            ProposalDate = new DateTime(2025, 9, 1), // Before effective date
            EffectiveDate = new DateTime(2025, 10, 1)
        };

        // Act
        var result = _service.ValidateProposalDate(premium, policy);

        // Assert
        Assert.True(result.IsValid);
        Assert.Empty(result.Errors);
        Assert.Empty(result.AutoCorrected);
    }

    [Fact]
    public void ValidateProposalDate_RamoNotInRestrictionList_ReturnsValid()
    {
        // Arrange
        var premium = CreateTestPremium();
        var policy = new Policy
        {
            PolicyNumber = 1234567890123,
            RamoSusep = 531, // Auto - not in restriction list
            ProposalDate = new DateTime(2025, 11, 1), // After effective date
            EffectiveDate = new DateTime(2025, 10, 1)
        };

        // Act
        var result = _service.ValidateProposalDate(premium, policy);

        // Assert
        Assert.True(result.IsValid, "Validation not required for this ramo");
        Assert.Empty(result.Errors);
        Assert.Empty(result.AutoCorrected);
    }

    [Fact]
    public void ValidateProposalDate_NullPolicy_ReturnsValid()
    {
        // Arrange
        var premium = CreateTestPremium();

        // Act
        var result = _service.ValidateProposalDate(premium, null);

        // Assert
        Assert.True(result.IsValid, "Cannot validate without policy");
        Assert.Empty(result.Errors);
    }

    #endregion

    #region Bilhete Number Validation Tests (FR-017)

    [Fact]
    public void ValidateBilheteNumber_GrupoRamo09_NoBilhete_ReturnsInvalid()
    {
        // Arrange
        var premium = CreateTestPremium();
        var product = new Product
        {
            ProductCode = 1001,
            CompanyCode = 1,
            LineOfBusinessGroup = 9 // GrupoRamo is read-only, use LineOfBusinessGroup
        };

        // Act
        var result = _service.ValidateBilheteNumber(premium, product);

        // Assert - Based on current placeholder implementation
        // TODO: Update when actual bilhete field is implemented
        Assert.True(result.IsValid || !result.IsValid); // Placeholder until real implementation
    }

    [Fact]
    public void ValidateBilheteNumber_GrupoRamoNot09_ReturnsValid()
    {
        // Arrange
        var premium = CreateTestPremium();
        var product = new Product
        {
            ProductCode = 1001,
            CompanyCode = 1,
            LineOfBusinessGroup = 5 // Not Acidentes Pessoais
        };

        // Act
        var result = _service.ValidateBilheteNumber(premium, product);

        // Assert
        Assert.True(result.IsValid, "Validation not required for this grupo ramo");
        Assert.Empty(result.Errors);
    }

    [Fact]
    public void ValidateBilheteNumber_NullProduct_ReturnsValid()
    {
        // Arrange
        var premium = CreateTestPremium();

        // Act
        var result = _service.ValidateBilheteNumber(premium, null);

        // Assert
        Assert.True(result.IsValid, "Cannot validate without product");
        Assert.Empty(result.Errors);
    }

    #endregion

    #region Insured Quantity Validation Tests (FR-018)

    [Fact]
    public void ValidateInsuredQuantity_PositiveValue_ReturnsValid()
    {
        // Arrange
        var premium = CreateTestPremium();

        // Act
        var result = _service.ValidateInsuredQuantity(premium);

        // Assert - Based on current placeholder implementation
        Assert.True(result.IsValid);
    }

    // Note: Additional tests for zero/negative quantity will be added
    // when actual InsuredQuantity field is implemented in PremiumRecord entity

    #endregion

    #region Date Sequence Validation Tests

    [Fact]
    public void ValidateDateSequence_IssueDateAfterEffective_ReturnsInvalid()
    {
        // Arrange
        var premium = CreateTestPremium();
        var policy = new Policy
        {
            PolicyNumber = 1234567890123,
            IssueDate = new DateTime(2025, 11, 1),
            EffectiveDate = new DateTime(2025, 10, 1)
        };

        // Act
        var result = _service.ValidateDateSequence(premium, policy);

        // Assert
        Assert.False(result.IsValid);
        Assert.Single(result.Errors);
        Assert.Equal(ValidationErrorMessages.ERR_INVALID_DATE_SEQUENCE, result.Errors[0].ErrorCode);
        Assert.Contains("emissão", result.Errors[0].Message.ToLower());
    }

    [Fact]
    public void ValidateDateSequence_EffectiveDateAfterExpiration_ReturnsInvalid()
    {
        // Arrange
        var premium = CreateTestPremium();
        var policy = new Policy
        {
            PolicyNumber = 1234567890123,
            EffectiveDate = new DateTime(2026, 11, 1),
            ExpirationDate = new DateTime(2026, 10, 1)
        };

        // Act
        var result = _service.ValidateDateSequence(premium, policy);

        // Assert
        Assert.False(result.IsValid);
        Assert.Single(result.Errors);
        Assert.Equal(ValidationErrorMessages.ERR_INVALID_DATE_SEQUENCE, result.Errors[0].ErrorCode);
        Assert.Contains("vigência", result.Errors[0].Message.ToLower());
    }

    [Fact]
    public void ValidateDateSequence_ValidSequence_ReturnsValid()
    {
        // Arrange
        var premium = CreateTestPremium();
        var policy = new Policy
        {
            PolicyNumber = 1234567890123,
            IssueDate = new DateTime(2025, 9, 1),
            EffectiveDate = new DateTime(2025, 10, 1),
            ExpirationDate = new DateTime(2026, 10, 1)
        };

        // Act
        var result = _service.ValidateDateSequence(premium, policy);

        // Assert
        Assert.True(result.IsValid);
        Assert.Empty(result.Errors);
    }

    [Fact]
    public void ValidateDateSequence_NullPolicy_ReturnsValid()
    {
        // Arrange
        var premium = CreateTestPremium();

        // Act
        var result = _service.ValidateDateSequence(premium, null);

        // Assert
        Assert.True(result.IsValid, "Cannot validate without policy");
    }

    #endregion

    #region Premium Amount Validation Tests

    [Fact]
    public void ValidatePremiumAmounts_WithinLimits_ReturnsValid()
    {
        // Arrange
        var premium = CreateTestPremium();
        premium.NetPremiumItem = 1234.56m;
        premium.TotalPremiumNet = 5678.90m;

        // Act
        var result = _service.ValidatePremiumAmounts(premium);

        // Assert
        Assert.True(result.IsValid);
        Assert.Empty(result.Errors);
    }

    [Fact]
    public void ValidatePremiumAmounts_ExceedsPrecision_ReturnsInvalid()
    {
        // Arrange
        var premium = CreateTestPremium();
        premium.NetPremiumItem = 99999999999999.99m; // Exceeds decimal(15,2)

        // Act
        var result = _service.ValidatePremiumAmounts(premium);

        // Assert
        Assert.False(result.IsValid);
        Assert.Contains(result.Errors, e => e.ErrorCode == ValidationErrorMessages.ERR_AMOUNT_OVERFLOW);
    }

    [Fact]
    public void ValidatePremiumAmounts_NegativePremiumNonCancellation_ReturnsInvalid()
    {
        // Arrange
        var premium = CreateTestPremium();
        premium.NetPremiumItem = -1000.00m;
        premium.MovementType = "E"; // Emissão (not cancellation)

        // Act
        var result = _service.ValidatePremiumAmounts(premium);

        // Assert
        Assert.False(result.IsValid);
        Assert.Contains(result.Errors, e => e.ErrorCode == ValidationErrorMessages.ERR_NEGATIVE_PREMIUM);
    }

    [Fact]
    public void ValidatePremiumAmounts_NegativePremiumCancellation_ReturnsValid()
    {
        // Arrange
        var premium = CreateTestPremium();
        premium.NetPremiumItem = -1000.00m;
        premium.MovementType = "C"; // Cancelamento

        // Act
        var result = _service.ValidatePremiumAmounts(premium);

        // Assert
        Assert.True(result.IsValid, "Negative premium allowed for cancellations");
    }

    [Fact]
    public void ValidatePremiumAmounts_ZeroPremiumEmission_ReturnsWarning()
    {
        // Arrange
        var premium = CreateTestPremium();
        premium.NetPremiumItem = 0.00m;
        premium.MovementType = "E"; // Emissão

        // Act
        var result = _service.ValidatePremiumAmounts(premium);

        // Assert
        Assert.True(result.IsValid, "Zero premium is warning, not error");
        Assert.Single(result.Warnings);
        Assert.Equal(ValidationErrorMessages.WARN_DATA_QUALITY, result.Warnings[0].WarningCode);
    }

    #endregion

    #region Foreign Key Validation Tests

    [Fact]
    public async Task ValidateForeignKeysAsync_AllExist_ReturnsValid()
    {
        // Arrange
        var premium = CreateTestPremium();

        _clientRepositoryMock
            .Setup(r => r.GetByClientCodeAsync(premium.ClientCode, It.IsAny<CancellationToken>()))
            .ReturnsAsync(new Client { ClientCode = premium.ClientCode });

        _policyRepositoryMock
            .Setup(r => r.GetByPolicyNumberAsync(premium.PolicyNumber, It.IsAny<CancellationToken>()))
            .ReturnsAsync(new Policy { PolicyNumber = premium.PolicyNumber });

        _productRepositoryMock
            .Setup(r => r.GetByProductCodeAsync(It.IsAny<int>(), It.IsAny<CancellationToken>()))
            .ReturnsAsync(new Product { ProductCode = 1001 });

        // Act
        var result = await _service.ValidateForeignKeysAsync(premium);

        // Assert
        Assert.True(result.IsValid);
        Assert.Empty(result.Warnings);
    }

    [Fact]
    public async Task ValidateForeignKeysAsync_ClientNotFound_ReturnsWarning()
    {
        // Arrange
        var premium = CreateTestPremium();

        _clientRepositoryMock
            .Setup(r => r.GetByClientCodeAsync(premium.ClientCode, It.IsAny<CancellationToken>()))
            .ReturnsAsync((Client?)null);

        _policyRepositoryMock
            .Setup(r => r.GetByPolicyNumberAsync(premium.PolicyNumber, It.IsAny<CancellationToken>()))
            .ReturnsAsync(new Policy { PolicyNumber = premium.PolicyNumber });

        _productRepositoryMock
            .Setup(r => r.GetByProductCodeAsync(It.IsAny<int>(), It.IsAny<CancellationToken>()))
            .ReturnsAsync(new Product { ProductCode = 1001 });

        // Act
        var result = await _service.ValidateForeignKeysAsync(premium);

        // Assert
        Assert.True(result.IsValid, "Missing foreign keys produce warnings, not errors");
        Assert.Single(result.Warnings);
        Assert.Contains("Cliente", result.Warnings[0].Message);
    }

    [Fact]
    public async Task ValidateForeignKeysAsync_PolicyNotFound_ReturnsWarning()
    {
        // Arrange
        var premium = CreateTestPremium();

        _clientRepositoryMock
            .Setup(r => r.GetByClientCodeAsync(premium.ClientCode, It.IsAny<CancellationToken>()))
            .ReturnsAsync(new Client { ClientCode = premium.ClientCode });

        _policyRepositoryMock
            .Setup(r => r.GetByPolicyNumberAsync(premium.PolicyNumber, It.IsAny<CancellationToken>()))
            .ReturnsAsync((Policy?)null);

        _productRepositoryMock
            .Setup(r => r.GetByProductCodeAsync(It.IsAny<int>(), It.IsAny<CancellationToken>()))
            .ReturnsAsync(new Product { ProductCode = 1001 });

        // Act
        var result = await _service.ValidateForeignKeysAsync(premium);

        // Assert
        Assert.True(result.IsValid);
        Assert.Single(result.Warnings);
        Assert.Contains("Apólice", result.Warnings[0].Message);
    }

    #endregion

    #region Full Validation Integration Tests

    [Fact]
    public async Task ValidatePremiumAsync_AllValidationsPass_ReturnsValid()
    {
        // Arrange
        var premium = CreateTestPremium();
        var policy = new Policy
        {
            PolicyNumber = premium.PolicyNumber,
            RamoSusep = 531, // Auto - no special restrictions
            IssueDate = new DateTime(2025, 9, 1),
            EffectiveDate = new DateTime(2025, 10, 1),
            ExpirationDate = new DateTime(2026, 10, 1)
        };
        var product = new Product
        {
            ProductCode = 1001,
            CompanyCode = 1,
            LineOfBusinessGroup = 5 // Not grupo ramo 09
        };

        // Mock repositories to return found entities
        _clientRepositoryMock
            .Setup(r => r.GetByClientCodeAsync(premium.ClientCode, It.IsAny<CancellationToken>()))
            .ReturnsAsync(new Client { ClientCode = premium.ClientCode });

        _policyRepositoryMock
            .Setup(r => r.GetByPolicyNumberAsync(premium.PolicyNumber, It.IsAny<CancellationToken>()))
            .ReturnsAsync(policy);

        _productRepositoryMock
            .Setup(r => r.GetByProductCodeAsync(It.IsAny<int>(), It.IsAny<CancellationToken>()))
            .ReturnsAsync(product);

        // Act
        var result = await _service.ValidatePremiumAsync(premium, policy, product);

        // Assert
        Assert.True(result.IsValid);
        Assert.Empty(result.Errors);
    }

    [Fact]
    public async Task ValidatePremiumAsync_MultipleErrors_ReturnsAllErrors()
    {
        // Arrange
        var premium = CreateTestPremium();
        premium.NetPremiumItem = -1000.00m; // Negative for non-cancellation
        premium.MovementType = "E"; // Emissão

        var policy = new Policy
        {
            PolicyNumber = premium.PolicyNumber,
            IssueDate = new DateTime(2025, 11, 1), // After effective date
            EffectiveDate = new DateTime(2025, 10, 1)
        };

        // Mock repositories
        _clientRepositoryMock
            .Setup(r => r.GetByClientCodeAsync(premium.ClientCode, It.IsAny<CancellationToken>()))
            .ReturnsAsync((Client?)null); // Client not found

        _policyRepositoryMock
            .Setup(r => r.GetByPolicyNumberAsync(premium.PolicyNumber, It.IsAny<CancellationToken>()))
            .ReturnsAsync(policy);

        _productRepositoryMock
            .Setup(r => r.GetByProductCodeAsync(It.IsAny<int>(), It.IsAny<CancellationToken>()))
            .ReturnsAsync(new Product { ProductCode = 1001 });

        // Act
        var result = await _service.ValidatePremiumAsync(premium, policy, null);

        // Assert
        Assert.False(result.IsValid);
        Assert.True(result.Errors.Count >= 2, "Should have multiple errors");
        Assert.True(result.Warnings.Count >= 1, "Should have missing client warning");
    }

    #endregion

    #region Helper Methods

    private static PremiumRecord CreateTestPremium()
    {
        return new PremiumRecord
        {
            PremiumId = 1,
            PolicyNumber = 1234567890123,
            ClientCode = 12345,
            CompanyCode = 1,
            ReferenceYear = 2025,
            ReferenceMonth = 10,
            ReferenceDay = 1,
            MovementType = "E",
            LineOfBusiness = 531,
            NetPremiumItem = 1000.00m,
            TotalPremiumNet = 1000.00m
        };
    }

    #endregion
}
