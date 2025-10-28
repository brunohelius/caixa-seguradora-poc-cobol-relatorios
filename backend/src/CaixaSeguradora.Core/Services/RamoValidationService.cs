using System;
using CaixaSeguradora.Core.Constants;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Models;
using Microsoft.Extensions.Logging;

namespace CaixaSeguradora.Core.Services;

/// <summary>
/// Implements ramo-specific (line of business) validation rules.
/// Different insurance product lines have specific validation requirements.
/// Referenced by BusinessRuleValidationService for product-specific validations.
/// </summary>
public class RamoValidationService
{
    private readonly ILogger<RamoValidationService> _logger;

    // Ramo SUSEP codes for common insurance lines
    private const int RamoVidaIndividual = 167;
    private const int RamoAuto = 531;
    private const int RamoResidencial = 193;
    private const int RamoViagem = 860;
    private const int RamoPrevidencia = 993;

    public RamoValidationService(ILogger<RamoValidationService> logger)
    {
        _logger = logger ?? throw new ArgumentNullException(nameof(logger));
    }

    /// <summary>
    /// Validates grupo ramo 09 (Acidentes Pessoais) specific rules
    /// FR-017: Requires bilhete number
    /// </summary>
    public ValidationResult ValidateGrupoRamo09(PremiumRecord premium, Product? product)
    {
        var result = new ValidationResult();

        if (product == null || product.GrupoRamo != 9)
        {
            return result;
        }

        // Bilhete number validation is handled by BusinessRuleValidationService.ValidateBilheteNumber
        // This method can add additional grupo ramo 09 specific validations

        _logger.LogDebug("Validating grupo ramo 09 rules for policy {PolicyNumber}", premium.PolicyNumber);

        return result;
    }

    /// <summary>
    /// Validates ramo 0167 (Vida Individual) specific rules
    /// Requires: proposal date validation, insured age information
    /// </summary>
    public ValidationResult ValidateRamo0167(PremiumRecord premium, Policy? policy, Client? client)
    {
        var result = new ValidationResult();

        if (policy == null || policy.RamoSusep != RamoVidaIndividual)
        {
            return result;
        }

        _logger.LogDebug("Validating ramo 0167 (Vida Individual) for policy {PolicyNumber}", premium.PolicyNumber);

        // Proposal date validation is handled by BusinessRuleValidationService.ValidateProposalDate

        // Additional vida individual validations can be added here
        // Example: insured age requirements, sum insured limits, etc.

        return result;
    }

    /// <summary>
    /// Validates ramo 0531 (Auto) specific rules
    /// Requires: vehicle identification, license plate, etc.
    /// </summary>
    public ValidationResult ValidateRamo0531(PremiumRecord premium, Policy? policy)
    {
        var result = new ValidationResult();

        if (policy == null || policy.RamoSusep != RamoAuto)
        {
            return result;
        }

        _logger.LogDebug("Validating ramo 0531 (Auto) for policy {PolicyNumber}", premium.PolicyNumber);

        // Vehicle identification validation
        // Note: Actual field names depend on entity structure
        // TODO: Implement when vehicle entity/fields are available

        // Example validation (commented out until entity structure is confirmed):
        /*
        if (string.IsNullOrWhiteSpace(policy.VehiclePlate))
        {
            result.AddError(
                errorCode: ValidationErrorMessages.ERR_INVALID_RAMO,
                message: ValidationErrorMessages.Messages.VehicleIdentificationRequired,
                fieldName: "VehiclePlate",
                policyNumber: premium.PolicyNumber);
        }
        */

        return result;
    }

    /// <summary>
    /// Validates ramo 0193 (Residencial) specific rules
    /// Requires: property address, construction type, etc.
    /// </summary>
    public ValidationResult ValidateRamo0193(PremiumRecord premium, Policy? policy)
    {
        var result = new ValidationResult();

        if (policy == null || policy.RamoSusep != RamoResidencial)
        {
            return result;
        }

        _logger.LogDebug("Validating ramo 0193 (Residencial) for policy {PolicyNumber}", premium.PolicyNumber);

        // Property address validation
        // Note: Address is typically a separate entity linked to policy
        // TODO: Implement when address validation integration is available

        // Example validation (commented out until entity structure is confirmed):
        /*
        if (policy.PropertyAddressId == null)
        {
            result.AddWarning(
                warningCode: ValidationErrorMessages.WARN_DATA_QUALITY,
                message: ValidationErrorMessages.Messages.PropertyAddressRequired,
                fieldName: "PropertyAddressId",
                policyNumber: premium.PolicyNumber);
        }
        */

        return result;
    }

    /// <summary>
    /// Validates ramo-specific business rules based on product ramo
    /// Routes to appropriate ramo-specific validation method
    /// </summary>
    public ValidationResult ValidateByRamo(PremiumRecord premium, Policy? policy, Product? product, Client? client = null)
    {
        if (policy == null || product == null)
        {
            return new ValidationResult();
        }

        var ramoSusep = policy.RamoSusep;

        _logger.LogDebug("Routing ramo-specific validation for ramo {Ramo} policy {PolicyNumber}",
            ramoSusep, premium.PolicyNumber);

        return ramoSusep switch
        {
            RamoVidaIndividual => ValidateRamo0167(premium, policy, client),
            RamoAuto => ValidateRamo0531(premium, policy),
            RamoResidencial => ValidateRamo0193(premium, policy),
            RamoViagem => ValidateRamoViagem(premium, policy),
            RamoPrevidencia => ValidateRamoPrevidencia(premium, policy),
            _ => new ValidationResult() // No specific validation for this ramo
        };
    }

    /// <summary>
    /// Validates ramo 0860 (Viagem) specific rules
    /// </summary>
    private ValidationResult ValidateRamoViagem(PremiumRecord premium, Policy? policy)
    {
        var result = new ValidationResult();

        if (policy == null)
        {
            return result;
        }

        _logger.LogDebug("Validating ramo 0860 (Viagem) for policy {PolicyNumber}", premium.PolicyNumber);

        // Proposal date validation is handled by BusinessRuleValidationService.ValidateProposalDate
        // Additional viagem-specific validations can be added here

        return result;
    }

    /// <summary>
    /// Validates ramo 0993 (Previdência) specific rules
    /// </summary>
    private ValidationResult ValidateRamoPrevidencia(PremiumRecord premium, Policy? policy)
    {
        var result = new ValidationResult();

        if (policy == null)
        {
            return result;
        }

        _logger.LogDebug("Validating ramo 0993 (Previdência) for policy {PolicyNumber}", premium.PolicyNumber);

        // Proposal date validation is handled by BusinessRuleValidationService.ValidateProposalDate
        // Additional previdência-specific validations can be added here

        return result;
    }

    /// <summary>
    /// Checks if a ramo requires proposal date validation (FR-016)
    /// </summary>
    public static bool RequiresProposalDateValidation(int ramoSusep)
    {
        return ramoSusep == 167 || ramoSusep == 860 || ramoSusep == 870 ||
               ramoSusep == 993 || ramoSusep == 1061 || ramoSusep == 1065 || ramoSusep == 1068;
    }
}
