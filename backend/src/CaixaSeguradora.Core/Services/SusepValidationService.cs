using System;
using System.Threading;
using System.Threading.Tasks;
using CaixaSeguradora.Core.Constants;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Interfaces;
using CaixaSeguradora.Core.Models;
using Microsoft.Extensions.Logging;

namespace CaixaSeguradora.Core.Services;

/// <summary>
/// Implements SUSEP-specific validation rules.
/// FR-019: SUSEP process number validation for specific products/ramos
/// </summary>
public class SusepValidationService
{
    private readonly ILogger<SusepValidationService> _logger;
    private readonly IProductRepository _productRepository;

    // Products that require SUSEP process number for specific ramos (FR-019)
    private static readonly int[] ProductsRequiringSusepProcess = { 1803, 1804, 1805 };

    public SusepValidationService(
        ILogger<SusepValidationService> logger,
        IProductRepository productRepository)
    {
        _logger = logger ?? throw new ArgumentNullException(nameof(logger));
        _productRepository = productRepository ?? throw new ArgumentNullException(nameof(productRepository));
    }

    /// <summary>
    /// Validates SUSEP process number requirement for specific products
    /// FR-019: Products 1803/1804/1805 require SUSEP process number for certain ramos
    /// COBOL: Paragraph R0850-00-VALIDA-PROCESSO-SUSEP
    /// </summary>
    public async Task<ValidationResult> ValidateSusepProcessNumberAsync(
        PremiumRecord premium,
        Product? product,
        Policy? policy,
        CancellationToken cancellationToken = default)
    {
        var result = new ValidationResult();

        if (product == null || policy == null)
        {
            // Cannot validate without product and policy data
            return result;
        }

        // Check if product requires SUSEP process number
        var productCode = product.ProductCode;
        if (!Array.Exists(ProductsRequiringSusepProcess, p => p == productCode))
        {
            // Validation not required for this product
            return result;
        }

        _logger.LogDebug(
            "Validating SUSEP process number for product {ProductCode} ramo {Ramo} policy {PolicyNumber}",
            productCode,
            policy.RamoSusep,
            premium.PolicyNumber);

        try
        {
            // Retrieve SUSEP process number for product/ramo combination
            // Note: This assumes a method exists on product repository
            // Adjust based on actual implementation
            var susepProcessNumber = await GetSusepProcessNumberAsync(
                productCode,
                policy.RamoSusep,
                cancellationToken);

            if (string.IsNullOrWhiteSpace(susepProcessNumber))
            {
                result.AddError(
                    errorCode: ValidationErrorMessages.ERR_MISSING_SUSEP_PROCESS,
                    message: ValidationErrorMessages.Format(
                        ValidationErrorMessages.Messages.SusepProcessNumberRequired,
                        productCode,
                        policy.RamoSusep),
                    fieldName: "SusepProcessNumber",
                    policyNumber: premium.PolicyNumber);

                _logger.LogWarning(
                    "SUSEP process number not found for product {ProductCode} ramo {Ramo}",
                    productCode,
                    policy.RamoSusep);
            }
            else
            {
                _logger.LogDebug(
                    "SUSEP process number validated: {ProcessNumber} for product {ProductCode} ramo {Ramo}",
                    susepProcessNumber,
                    productCode,
                    policy.RamoSusep);
            }
        }
        catch (Exception ex)
        {
            _logger.LogError(
                ex,
                "Error validating SUSEP process number for product {ProductCode} ramo {Ramo}",
                productCode,
                policy.RamoSusep);

            // Add warning instead of error to allow processing to continue
            result.AddWarning(
                warningCode: ValidationErrorMessages.WARN_DATA_QUALITY,
                message: ValidationErrorMessages.Format(
                    ValidationErrorMessages.Messages.SusepProcessNumberNotFound,
                    productCode,
                    policy.RamoSusep),
                fieldName: "SusepProcessNumber",
                policyNumber: premium.PolicyNumber);
        }

        return result;
    }

    /// <summary>
    /// Retrieves SUSEP process number for a product/ramo combination
    /// COBOL equivalent: Query to SUSEP process table or product configuration
    /// </summary>
    private async Task<string?> GetSusepProcessNumberAsync(
        int productCode,
        int ramoSusep,
        CancellationToken cancellationToken)
    {
        // Retrieve product details
        var product = await _productRepository.GetByProductCodeAsync(productCode, cancellationToken);

        if (product == null)
        {
            return null;
        }

        // Return SUSEP process number from product entity
        // Note: Actual field name may vary based on entity structure
        // TODO: Update with actual field name when entity structure is confirmed
        return product.SusepProcessNumber;
    }

    /// <summary>
    /// Checks if a product/ramo combination requires SUSEP process number validation
    /// </summary>
    public static bool RequiresSusepProcessNumber(int productCode, int ramoSusep)
    {
        // FR-019: Products 1803/1804/1805 require SUSEP process number
        return Array.Exists(ProductsRequiringSusepProcess, p => p == productCode);
    }

    /// <summary>
    /// Validates SUSEP Circular 360 format compliance for output records
    /// Ensures generated records meet SUSEP regulatory requirements
    /// </summary>
    public ValidationResult ValidateSusepFormatCompliance(PremiumRecord premium)
    {
        var result = new ValidationResult();

        // Validate mandatory fields per SUSEP Circular 360
        if (premium.PolicyNumber <= 0)
        {
            result.AddError(
                errorCode: ValidationErrorMessages.ERR_INVALID_RAMO,
                message: ValidationErrorMessages.Format(
                    ValidationErrorMessages.Messages.MandatoryFieldMissing,
                    "PolicyNumber"),
                fieldName: "PolicyNumber",
                policyNumber: premium.PolicyNumber);
        }

        if (premium.ReferenceYear < 2014 || premium.ReferenceYear > DateTime.Now.Year)
        {
            result.AddError(
                errorCode: ValidationErrorMessages.ERR_INVALID_RAMO,
                message: ValidationErrorMessages.Format(
                    ValidationErrorMessages.Messages.DataOutOfRange,
                    "ReferenceYear",
                    2014,
                    DateTime.Now.Year),
                fieldName: "ReferenceYear",
                policyNumber: premium.PolicyNumber);
        }

        if (premium.ReferenceMonth < 1 || premium.ReferenceMonth > 12)
        {
            result.AddError(
                errorCode: ValidationErrorMessages.ERR_INVALID_RAMO,
                message: ValidationErrorMessages.Format(
                    ValidationErrorMessages.Messages.DataOutOfRange,
                    "ReferenceMonth",
                    1,
                    12),
                fieldName: "ReferenceMonth",
                policyNumber: premium.PolicyNumber);
        }

        // Validate movement type is valid per SUSEP
        if (string.IsNullOrWhiteSpace(premium.MovementType))
        {
            result.AddError(
                errorCode: ValidationErrorMessages.ERR_INVALID_MOVEMENT_TYPE,
                message: ValidationErrorMessages.Format(
                    ValidationErrorMessages.Messages.MandatoryFieldMissing,
                    "MovementType"),
                fieldName: "MovementType",
                policyNumber: premium.PolicyNumber);
        }

        return result;
    }
}
