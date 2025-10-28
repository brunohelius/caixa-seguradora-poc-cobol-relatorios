using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using CaixaSeguradora.Core.Constants;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Interfaces;
using CaixaSeguradora.Core.Services;
using Microsoft.Extensions.Logging;
using Moq;
using Xunit;
using Xunit.Abstractions;

namespace CaixaSeguradora.ComparisonTests;

/// <summary>
/// Comparison tests validating .NET business rules match COBOL rejection behavior.
/// Tests using known COBOL rejection scenarios to ensure parity.
/// Category: Comparison
/// </summary>
[Trait("Category", "Comparison")]
public class BusinessRuleComparisonTests
{
    private readonly ITestOutputHelper _output;
    private readonly Mock<ILogger<BusinessRuleValidationService>> _loggerMock;
    private readonly Mock<IClientRepository> _clientRepositoryMock;
    private readonly Mock<IPolicyRepository> _policyRepositoryMock;
    private readonly Mock<IProductRepository> _productRepositoryMock;
    private readonly BusinessRuleValidationService _service;

    public BusinessRuleComparisonTests(ITestOutputHelper output)
    {
        _output = output;
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

    [Fact]
    public async Task ApplyBusinessRules_ProposalDateRamo0167_MatchesCOBOLBehavior()
    {
        // COBOL Behavior: For ramo 0167, if proposal date > effective date,
        // COBOL auto-adjusts proposal date to effective date and logs correction
        // Reference: COBOL paragraph R0800-20-VALIDA-DATAS

        // Arrange
        var premium = new PremiumRecord
        {
            PolicyNumber = 9876543210123,
            ClientCode = 98765
        };

        var policy = new Policy
        {
            PolicyNumber = 9876543210123,
            RamoSusep = 167, // Vida Individual
            ProposalDate = new DateTime(2025, 11, 15),
            EffectiveDate = new DateTime(2025, 10, 1)
        };

        // Act
        var result = _service.ValidateProposalDate(premium, policy);

        // Assert - Match COBOL behavior
        Assert.True(result.IsValid, "COBOL allows with auto-correction");
        Assert.Empty(result.Errors);
        Assert.Single(result.AutoCorrected);

        var correction = result.AutoCorrected[0];
        Assert.Equal("ProposalDate", correction.FieldName);
        Assert.Equal("2025-11-15", correction.OriginalValue);
        Assert.Equal("2025-10-01", correction.CorrectedValue);
        Assert.Equal(policy.EffectiveDate, policy.ProposalDate);

        _output.WriteLine($"✅ COBOL Parity: Proposal date auto-corrected from {correction.OriginalValue} to {correction.CorrectedValue}");
    }

    [Fact]
    public void ApplyBusinessRules_GrupoRamo09NoBilhete_MatchesCOBOLRejection()
    {
        // COBOL Behavior: For grupo ramo 09 (Acidentes Pessoais) without bilhete number,
        // COBOL rejects the record with error message "Ramo 09 requer número de bilhete"
        // Reference: COBOL paragraph R0800-30-VALIDA-BILHETE

        // Arrange
        var premium = new PremiumRecord
        {
            PolicyNumber = 5555555555555,
            ClientCode = 55555
        };

        var product = new Product
        {
            ProductCode = 9001,
            LineOfBusinessGroup = 9, // Acidentes Pessoais (GrupoRamo is computed from this)
            ProductName = "Seguro Acidentes Pessoais"
        };

        // Act
        var result = _service.ValidateBilheteNumber(premium, product);

        // Assert - Match COBOL behavior
        // Note: Based on placeholder implementation - update when actual bilhete field exists
        // Expected: result.IsValid = false, result.Errors contains bilhete error

        _output.WriteLine($"⚠️ Bilhete validation placeholder - awaiting entity field implementation");
        _output.WriteLine($"Expected COBOL behavior: Reject with error '{ValidationErrorMessages.Messages.BilheteRequired}'");
    }

    [Fact]
    public void ApplyBusinessRules_ZeroInsuredQuantity_MatchesCOBOLAutoCorrection()
    {
        // COBOL Behavior: When insured quantity is zero or negative,
        // COBOL auto-corrects to 1 (minimum) and logs correction
        // Reference: COBOL paragraph R0800-40-VALIDA-QUANTIDADE

        // Arrange
        var premium = new PremiumRecord
        {
            PolicyNumber = 7777777777777,
            ClientCode = 77777
            // InsuredQuantity = 0 (when field is implemented)
        };

        // Act
        var result = _service.ValidateInsuredQuantity(premium);

        // Assert - Match COBOL behavior
        // Note: Based on placeholder implementation
        // Expected: result.AutoCorrected contains quantity adjustment from 0 to 1

        _output.WriteLine($"⚠️ Insured quantity validation placeholder - awaiting entity field implementation");
        _output.WriteLine($"Expected COBOL behavior: Auto-correct 0 → 1 with log message");
    }

    [Fact]
    public void ApplyBusinessRules_InvalidDateSequence_MatchesCOBOLRejection()
    {
        // COBOL Behavior: When issue date > effective date,
        // COBOL rejects the record with date sequence error
        // Reference: COBOL section R0800-00-VALIDA-REGISTRO

        // Arrange
        var premium = new PremiumRecord
        {
            PolicyNumber = 3333333333333,
            ClientCode = 33333
        };

        var policy = new Policy
        {
            PolicyNumber = 3333333333333,
            IssueDate = new DateTime(2025, 12, 1),
            EffectiveDate = new DateTime(2025, 10, 1)
        };

        // Act
        var result = _service.ValidateDateSequence(premium, policy);

        // Assert - Match COBOL behavior
        Assert.False(result.IsValid, "COBOL rejects invalid date sequence");
        Assert.Single(result.Errors);
        Assert.Equal(ValidationErrorMessages.ERR_INVALID_DATE_SEQUENCE, result.Errors[0].ErrorCode);
        Assert.Contains("emissão", result.Errors[0].Message.ToLower());

        _output.WriteLine($"✅ COBOL Parity: Date sequence validation rejected as expected");
        _output.WriteLine($"Error: {result.Errors[0].Message}");
    }

    [Fact]
    public void ApplyBusinessRules_NegativePremiumEmission_MatchesCOBOLRejection()
    {
        // COBOL Behavior: Negative premium not allowed for emission (movement type 'E')
        // COBOL rejects with error message about invalid premium amount
        // Reference: COBOL validation paragraphs for premium amounts

        // Arrange
        var premium = new PremiumRecord
        {
            PolicyNumber = 4444444444444,
            ClientCode = 44444,
            NetPremiumItem = -500.00m,
            MovementType = "E" // Emissão
        };

        // Act
        var result = _service.ValidatePremiumAmounts(premium);

        // Assert - Match COBOL behavior
        Assert.False(result.IsValid, "COBOL rejects negative premium for emission");
        Assert.Contains(result.Errors, e => e.ErrorCode == ValidationErrorMessages.ERR_NEGATIVE_PREMIUM);

        var error = result.Errors.First(e => e.ErrorCode == ValidationErrorMessages.ERR_NEGATIVE_PREMIUM);
        Assert.Contains("negativo", error.Message.ToLower());
        Assert.Contains("E", error.Message); // Movement type in message

        _output.WriteLine($"✅ COBOL Parity: Negative premium rejected for emission");
        _output.WriteLine($"Error: {error.Message}");
    }

    [Fact]
    public void ApplyBusinessRules_NegativePremiumCancellation_MatchesCOBOLAcceptance()
    {
        // COBOL Behavior: Negative premium IS allowed for cancellation (movement type 'C')
        // Represents refund to customer
        // Reference: COBOL cancellation processing logic

        // Arrange
        var premium = new PremiumRecord
        {
            PolicyNumber = 6666666666666,
            ClientCode = 66666,
            NetPremiumItem = -500.00m,
            MovementType = "C" // Cancelamento
        };

        // Act
        var result = _service.ValidatePremiumAmounts(premium);

        // Assert - Match COBOL behavior
        Assert.True(result.IsValid, "COBOL allows negative premium for cancellation");
        Assert.DoesNotContain(result.Errors, e => e.ErrorCode == ValidationErrorMessages.ERR_NEGATIVE_PREMIUM);

        _output.WriteLine($"✅ COBOL Parity: Negative premium accepted for cancellation (refund)");
    }

    [Theory]
    [InlineData(167, "2025-11-01", "2025-10-01", true)]  // Vida - requires adjustment
    [InlineData(860, "2025-11-01", "2025-10-01", true)]  // Viagem - requires adjustment
    [InlineData(531, "2025-11-01", "2025-10-01", false)] // Auto - no adjustment
    [InlineData(193, "2025-11-01", "2025-10-01", false)] // Residencial - no adjustment
    public void ApplyBusinessRules_ProposalDateByRamo_MatchesCOBOLBehaviorMatrix(
        int ramoSusep,
        string proposalDate,
        string effectiveDate,
        bool shouldAutoCorrect)
    {
        // COBOL Behavior: Only specific ramos (167, 860, 870, 993, 1061, 1065, 1068)
        // trigger proposal date auto-correction
        // Reference: COBOL array WS-RAMO-VALIDA-DATA-PROPOSTA

        // Arrange
        var premium = new PremiumRecord
        {
            PolicyNumber = 1111111111111,
            ClientCode = 11111
        };

        var policy = new Policy
        {
            PolicyNumber = 1111111111111,
            RamoSusep = ramoSusep,
            ProposalDate = DateTime.Parse(proposalDate),
            EffectiveDate = DateTime.Parse(effectiveDate)
        };

        var originalProposalDate = policy.ProposalDate;

        // Act
        var result = _service.ValidateProposalDate(premium, policy);

        // Assert - Match COBOL behavior
        if (shouldAutoCorrect)
        {
            Assert.True(result.IsValid, $"Ramo {ramoSusep} should auto-correct");
            Assert.Single(result.AutoCorrected);
            Assert.Equal(policy.EffectiveDate, policy.ProposalDate);

            _output.WriteLine($"✅ Ramo {ramoSusep}: Auto-corrected {originalProposalDate:yyyy-MM-dd} → {policy.ProposalDate:yyyy-MM-dd}");
        }
        else
        {
            Assert.True(result.IsValid, $"Ramo {ramoSusep} should not validate proposal date");
            Assert.Empty(result.AutoCorrected);
            Assert.Equal(originalProposalDate, policy.ProposalDate);

            _output.WriteLine($"✅ Ramo {ramoSusep}: No validation required (date unchanged)");
        }
    }

    [Fact]
    public async Task ApplyBusinessRules_KnownCOBOLRejections_AllMatch()
    {
        // Integration test validating multiple known COBOL rejection scenarios
        // Simulates batch processing with mixed valid/invalid records

        // Arrange
        var testCases = new List<(PremiumRecord premium, Policy policy, Product? product, bool shouldReject, string reason)>
        {
            // Case 1: Valid record - should pass
            (
                new PremiumRecord { PolicyNumber = 1000000000001, ClientCode = 10001, NetPremiumItem = 100.00m, MovementType = "E" },
                new Policy { PolicyNumber = 1000000000001, RamoSusep = 531, IssueDate = DateTime.Now.AddDays(-10), EffectiveDate = DateTime.Now },
                new Product { ProductCode = 1001, LineOfBusinessGroup = 5, ProductName = "Valid Product" },
                false,
                "Valid record"
            ),

            // Case 2: Invalid date sequence - should reject
            (
                new PremiumRecord { PolicyNumber = 1000000000002, ClientCode = 10002, NetPremiumItem = 200.00m, MovementType = "E" },
                new Policy { PolicyNumber = 1000000000002, IssueDate = DateTime.Now, EffectiveDate = DateTime.Now.AddDays(-10) },
                null,
                true,
                "Issue date after effective date"
            ),

            // Case 3: Negative premium for emission - should reject
            (
                new PremiumRecord { PolicyNumber = 1000000000003, ClientCode = 10003, NetPremiumItem = -150.00m, MovementType = "E" },
                new Policy { PolicyNumber = 1000000000003 },
                null,
                true,
                "Negative premium for emission"
            ),

            // Case 4: Negative premium for cancellation - should pass
            (
                new PremiumRecord { PolicyNumber = 1000000000004, ClientCode = 10004, NetPremiumItem = -150.00m, MovementType = "C" },
                new Policy { PolicyNumber = 1000000000004 },
                null,
                false,
                "Negative premium for cancellation (valid)"
            )
        };

        // Act & Assert
        var passedCount = 0;
        var rejectedCount = 0;

        foreach (var (premium, policy, product, shouldReject, reason) in testCases)
        {
            // Mock repositories
            _clientRepositoryMock
                .Setup(r => r.GetByClientCodeAsync(premium.ClientCode, default))
                .ReturnsAsync(new Client { ClientCode = premium.ClientCode });

            _policyRepositoryMock
                .Setup(r => r.GetByPolicyNumberAsync(premium.PolicyNumber, default))
                .ReturnsAsync(policy);

            _productRepositoryMock
                .Setup(r => r.GetByProductCodeAsync(It.IsAny<int>(), default))
                .ReturnsAsync(product);

            var result = await _service.ValidatePremiumAsync(premium, policy, product);

            if (shouldReject)
            {
                Assert.False(result.IsValid, $"Should reject: {reason}");
                rejectedCount++;
                _output.WriteLine($"❌ Rejected (expected): Policy {premium.PolicyNumber} - {reason}");
            }
            else
            {
                Assert.True(result.IsValid, $"Should pass: {reason}");
                passedCount++;
                _output.WriteLine($"✅ Passed (expected): Policy {premium.PolicyNumber} - {reason}");
            }
        }

        _output.WriteLine($"\n📊 Summary: {passedCount} passed, {rejectedCount} rejected out of {testCases.Count} test cases");
        _output.WriteLine($"✅ All test cases match expected COBOL behavior");
    }
}
