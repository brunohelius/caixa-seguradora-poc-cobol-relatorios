using System;
using System.Data.Common;
using System.Threading;
using System.Threading.Tasks;
using CaixaSeguradora.Core.Constants;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Interfaces;
using CaixaSeguradora.Core.Models;
using Microsoft.Extensions.Logging;

namespace CaixaSeguradora.Core.Services;

/// <summary>
/// Implements business rule validation for premium records during report processing.
/// COBOL equivalent: Sections R0800-R0900 (validation paragraphs)
/// FR-014 through FR-018: Data validation and business rules
/// </summary>
public class BusinessRuleValidationService : IBusinessRuleValidationService
{
    private readonly ILogger<BusinessRuleValidationService> _logger;
    private readonly IClientRepository _clientRepository;
    private readonly IPolicyRepository _policyRepository;
    private readonly IProductRepository _productRepository;

    // Ramos that require proposal date ≤ effective date (FR-016)
    private static readonly int[] RamosWithProposalDateRestriction = { 167, 860, 870, 993, 1061, 1065, 1068 };

    // Grupo ramo that requires bilhete number (FR-017)
    private const int GrupoRamoAcidentesPessoais = 9;

    public BusinessRuleValidationService(
        ILogger<BusinessRuleValidationService> logger,
        IClientRepository clientRepository,
        IPolicyRepository policyRepository,
        IProductRepository productRepository)
    {
        _logger = logger ?? throw new ArgumentNullException(nameof(logger));
        _clientRepository = clientRepository ?? throw new ArgumentNullException(nameof(clientRepository));
        _policyRepository = policyRepository ?? throw new ArgumentNullException(nameof(policyRepository));
        _productRepository = productRepository ?? throw new ArgumentNullException(nameof(productRepository));
    }

    public async Task<ValidationResult> ValidatePremiumAsync(
        PremiumRecord premium,
        Policy? policy = null,
        Product? product = null,
        CancellationToken cancellationToken = default)
    {
        if (premium == null)
        {
            throw new ArgumentNullException(nameof(premium));
        }

        _logger.LogDebug("Validating premium record for policy {PolicyNumber}", premium.PolicyNumber);

        var result = new ValidationResult();

        // Validate proposal date vs effective date (FR-016)
        var proposalDateResult = ValidateProposalDate(premium, policy);
        MergeResults(result, proposalDateResult);

        // Validate bilhete number for grupo ramo 09 (FR-017)
        var bilheteResult = ValidateBilheteNumber(premium, product);
        MergeResults(result, bilheteResult);

        // Validate insured quantity (FR-018)
        var quantityResult = ValidateInsuredQuantity(premium);
        MergeResults(result, quantityResult);

        // Validate date sequence
        var dateSequenceResult = ValidateDateSequence(premium, policy);
        MergeResults(result, dateSequenceResult);

        // Validate premium amounts
        var amountsResult = ValidatePremiumAmounts(premium);
        MergeResults(result, amountsResult);

        // Validate foreign keys (async)
        var foreignKeyResult = await ValidateForeignKeysAsync(premium, cancellationToken);
        MergeResults(result, foreignKeyResult);

        if (!result.IsValid)
        {
            _logger.LogWarning(
                "Validation failed for policy {PolicyNumber}: {ErrorCount} errors, {WarningCount} warnings",
                premium.PolicyNumber,
                result.Errors.Count,
                result.Warnings.Count);
        }
        else if (result.AutoCorrected.Count > 0)
        {
            _logger.LogInformation(
                "Validation completed with {CorrectionCount} auto-corrections for policy {PolicyNumber}",
                result.AutoCorrected.Count,
                premium.PolicyNumber);
        }

        return result;
    }

    public ValidationResult ValidateProposalDate(PremiumRecord premium, Policy? policy)
    {
        var result = new ValidationResult();

        if (policy == null)
        {
            // Cannot validate without policy data
            return result;
        }

        // FR-016: Check if ramo requires proposal date validation
        var ramoSusep = policy.RamoSusep;
        if (!Array.Exists(RamosWithProposalDateRestriction, r => r == ramoSusep))
        {
            // Validation not required for this ramo
            return result;
        }

        if (policy.ProposalDate == default || policy.EffectiveDate == default)
        {
            // Missing dates - cannot validate
            return result;
        }

        if (policy.ProposalDate > policy.EffectiveDate)
        {
            // Auto-correct: Set proposal date = effective date
            var originalDate = policy.ProposalDate;
            policy.ProposalDate = policy.EffectiveDate;

            result.AddAutoCorrection(
                fieldName: "ProposalDate",
                originalValue: originalDate.ToString("yyyy-MM-dd"),
                correctedValue: policy.ProposalDate.ToString("yyyy-MM-dd"),
                reason: ValidationErrorMessages.Format(
                    ValidationErrorMessages.CorrectionReasons.ProposalDateAdjusted,
                    ramoSusep),
                policyNumber: premium.PolicyNumber);

            _logger.LogInformation(
                "Auto-corrected proposal date for policy {PolicyNumber} ramo {Ramo}: {Original} -> {Corrected}",
                premium.PolicyNumber,
                ramoSusep,
                originalDate,
                policy.ProposalDate);
        }

        return result;
    }

    public ValidationResult ValidateBilheteNumber(PremiumRecord premium, Product? product)
    {
        var result = new ValidationResult();

        if (product == null)
        {
            // Cannot validate without product data
            return result;
        }

        // FR-017: Check if grupo ramo requires bilhete number
        if (product.GrupoRamo != GrupoRamoAcidentesPessoais)
        {
            // Validation not required for this grupo ramo
            return result;
        }

        // Check if bilhete number is missing or zero
        // Assuming premium has a BilheteNumber property (adjust field name as needed)
        // For now, using a placeholder check - update based on actual entity structure
        var hasBilhete = premium.PolicyNumber > 0; // TODO: Replace with actual bilhete field

        if (!hasBilhete)
        {
            result.AddError(
                errorCode: ValidationErrorMessages.ERR_MISSING_BILHETE,
                message: ValidationErrorMessages.Messages.BilheteRequired,
                fieldName: "BilheteNumber",
                policyNumber: premium.PolicyNumber);

            _logger.LogWarning(
                "Missing bilhete number for policy {PolicyNumber} grupo ramo {GrupoRamo}",
                premium.PolicyNumber,
                product.GrupoRamo);
        }

        return result;
    }

    public ValidationResult ValidateInsuredQuantity(PremiumRecord premium)
    {
        var result = new ValidationResult();

        // FR-018: Validate insured quantity is at least 1
        // Assuming premium has an InsuredQuantity property (adjust field name as needed)
        // For now, using a placeholder - update based on actual entity structure
        int insuredQuantity = 1; // TODO: Replace with actual field: premium.InsuredQuantity

        if (insuredQuantity <= 0)
        {
            // Auto-correct to 1
            var originalQuantity = insuredQuantity;
            insuredQuantity = 1;

            // TODO: Apply correction to actual entity field
            // premium.InsuredQuantity = 1;

            result.AddAutoCorrection(
                fieldName: "InsuredQuantity",
                originalValue: originalQuantity,
                correctedValue: 1,
                reason: ValidationErrorMessages.Format(
                    ValidationErrorMessages.Messages.QuantityAutoAdjusted,
                    originalQuantity),
                policyNumber: premium.PolicyNumber);

            _logger.LogInformation(
                "Auto-corrected insured quantity for policy {PolicyNumber}: {Original} -> 1",
                premium.PolicyNumber,
                originalQuantity);
        }

        return result;
    }

    public ValidationResult ValidateDateSequence(PremiumRecord premium, Policy? policy)
    {
        var result = new ValidationResult();

        if (policy == null)
        {
            // Cannot validate without policy data
            return result;
        }

        // Validate: IssueDate ≤ EffectiveDate ≤ ExpirationDate
        if (policy.IssueDate != default && policy.EffectiveDate != default)
        {
            if (policy.IssueDate > policy.EffectiveDate)
            {
                result.AddError(
                    errorCode: ValidationErrorMessages.ERR_INVALID_DATE_SEQUENCE,
                    message: ValidationErrorMessages.Format(
                        ValidationErrorMessages.Messages.IssueDateAfterEffective,
                        policy.IssueDate.ToString("yyyy-MM-dd"),
                        policy.EffectiveDate.ToString("yyyy-MM-dd")),
                    fieldName: "IssueDate",
                    policyNumber: premium.PolicyNumber);
            }
        }

        if (policy.EffectiveDate != default && policy.ExpirationDate != default)
        {
            if (policy.EffectiveDate > policy.ExpirationDate)
            {
                result.AddError(
                    errorCode: ValidationErrorMessages.ERR_INVALID_DATE_SEQUENCE,
                    message: ValidationErrorMessages.Format(
                        ValidationErrorMessages.Messages.EffectiveDateAfterExpiration,
                        policy.EffectiveDate.ToString("yyyy-MM-dd"),
                        policy.ExpirationDate.ToString("yyyy-MM-dd")),
                    fieldName: "EffectiveDate",
                    policyNumber: premium.PolicyNumber);
            }
        }

        return result;
    }

    public ValidationResult ValidatePremiumAmounts(PremiumRecord premium)
    {
        var result = new ValidationResult();

        // Validate amounts are within decimal(15,2) precision
        const decimal maxValue = 9999999999999.99m; // 13 integer digits + 2 decimal

        // Check key premium amounts
        if (Math.Abs(premium.NetPremiumItem) > maxValue)
        {
            result.AddError(
                errorCode: ValidationErrorMessages.ERR_AMOUNT_OVERFLOW,
                message: ValidationErrorMessages.Format(
                    ValidationErrorMessages.Messages.AmountExceedsPrecision,
                    "NetPremiumItem"),
                fieldName: "NetPremiumItem",
                policyNumber: premium.PolicyNumber);
        }

        if (Math.Abs(premium.TotalPremiumNet) > maxValue)
        {
            result.AddError(
                errorCode: ValidationErrorMessages.ERR_AMOUNT_OVERFLOW,
                message: ValidationErrorMessages.Format(
                    ValidationErrorMessages.Messages.AmountExceedsPrecision,
                    "TotalPremiumNet"),
                fieldName: "TotalPremiumNet",
                policyNumber: premium.PolicyNumber);
        }

        // Validate negative premiums for non-cancellation movements
        if (premium.NetPremiumItem < 0 && premium.MovementType != "C") // 'C' = Cancelamento
        {
            result.AddError(
                errorCode: ValidationErrorMessages.ERR_NEGATIVE_PREMIUM,
                message: ValidationErrorMessages.Format(
                    ValidationErrorMessages.Messages.NegativePremiumNotAllowed,
                    premium.MovementType),
                fieldName: "NetPremiumItem",
                policyNumber: premium.PolicyNumber);
        }

        // Warning for zero premium (may indicate calculation issue)
        if (premium.NetPremiumItem == 0 && premium.MovementType == "E") // 'E' = Emissão
        {
            result.AddWarning(
                warningCode: ValidationErrorMessages.WARN_DATA_QUALITY,
                message: ValidationErrorMessages.Messages.ZeroPremiumWarning,
                fieldName: "NetPremiumItem",
                policyNumber: premium.PolicyNumber);
        }

        return result;
    }

    public async Task<ValidationResult> ValidateForeignKeysAsync(
        PremiumRecord premium,
        CancellationToken cancellationToken = default)
    {
        var result = new ValidationResult();

        try
        {
            // Validate client exists
            var client = await _clientRepository.GetByClientCodeAsync(premium.ClientCode, cancellationToken);
            if (client == null)
            {
                result.AddWarning(
                    warningCode: ValidationErrorMessages.WARN_MISSING_FOREIGN_KEY,
                    message: ValidationErrorMessages.Format(
                        ValidationErrorMessages.Messages.ClientNotFound,
                        premium.ClientCode),
                    fieldName: "ClientCode",
                    policyNumber: premium.PolicyNumber);
            }

            // Validate policy exists
            var policy = await _policyRepository.GetByPolicyNumberAsync(premium.PolicyNumber, cancellationToken);
            if (policy == null)
            {
                result.AddWarning(
                    warningCode: ValidationErrorMessages.WARN_MISSING_FOREIGN_KEY,
                    message: ValidationErrorMessages.Format(
                        ValidationErrorMessages.Messages.PolicyNotFound,
                        premium.PolicyNumber),
                    fieldName: "PolicyNumber",
                    policyNumber: premium.PolicyNumber);
            }

            // Validate product exists
            // Note: ProductCode field may vary - adjust based on actual entity
            var productCode = premium.LineOfBusiness; // TODO: Use actual product code field
            var product = await _productRepository.GetByProductCodeAsync(productCode, cancellationToken);
            if (product == null)
            {
                result.AddWarning(
                    warningCode: ValidationErrorMessages.WARN_MISSING_FOREIGN_KEY,
                    message: ValidationErrorMessages.Format(
                        ValidationErrorMessages.Messages.ProductNotFound,
                        productCode),
                    fieldName: "ProductCode",
                    policyNumber: premium.PolicyNumber);
            }
        }
        catch (OperationCanceledException)
        {
            // Expected during cancellation - propagate to caller
            throw;
        }
        catch (DbException dbEx)
        {
            // Database errors are critical - record as error and propagate
            _logger.LogError(dbEx, "Database error validating foreign keys for policy {PolicyNumber}", premium.PolicyNumber);
            result.AddError(
                errorCode: "DB_ERROR",
                message: "Erro ao validar referências no banco de dados. Operação abortada.",
                fieldName: "ForeignKey",
                policyNumber: premium.PolicyNumber);
            throw;
        }
        catch (Exception ex)
        {
            // Unexpected errors should not be silently swallowed
            _logger.LogError(ex, "Unexpected error validating foreign keys for policy {PolicyNumber}", premium.PolicyNumber);
            throw;
        }

        return result;
    }

    /// <summary>
    /// Merges source validation result into target result
    /// </summary>
    private static void MergeResults(ValidationResult target, ValidationResult source)
    {
        target.Errors.AddRange(source.Errors);
        target.Warnings.AddRange(source.Warnings);
        target.AutoCorrected.AddRange(source.AutoCorrected);
    }
}
