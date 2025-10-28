using System.Threading;
using System.Threading.Tasks;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Models;

namespace CaixaSeguradora.Core.Interfaces;

/// <summary>
/// Service interface for validating business rules during report processing.
/// Implements COBOL validation logic from sections R0800-R0900.
/// FR-014 through FR-018: Data validation and business rules
/// </summary>
public interface IBusinessRuleValidationService
{
    /// <summary>
    /// Validates a premium record against all applicable business rules.
    /// COBOL equivalent: Section R0800-00-VALIDA-REGISTRO
    /// </summary>
    /// <param name="premium">Premium record to validate</param>
    /// <param name="policy">Related policy (may be null if not found)</param>
    /// <param name="product">Related product (may be null if not found)</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Validation result with errors, warnings, and auto-corrections</returns>
    Task<ValidationResult> ValidatePremiumAsync(
        PremiumRecord premium,
        Policy? policy = null,
        Product? product = null,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Validates proposal date vs effective date for specific ramos.
    /// FR-016: Proposal date must not exceed effective date for ramos 0167, 0860, 0870, 0993, 1061, 1065, 1068
    /// COBOL: Paragraph R0800-20-VALIDA-DATAS
    /// </summary>
    ValidationResult ValidateProposalDate(PremiumRecord premium, Policy? policy);

    /// <summary>
    /// Validates bilhete number requirement for grupo ramo 09.
    /// FR-017: Reject grupo ramo 09 without bilhete number
    /// COBOL: Paragraph R0800-30-VALIDA-BILHETE
    /// </summary>
    ValidationResult ValidateBilheteNumber(PremiumRecord premium, Product? product);

    /// <summary>
    /// Validates insured quantity (minimum 1).
    /// FR-018: Auto-correct zero/negative quantity to 1
    /// COBOL: Paragraph R0800-40-VALIDA-QUANTIDADE
    /// </summary>
    ValidationResult ValidateInsuredQuantity(PremiumRecord premium);

    /// <summary>
    /// Validates date sequence (issue ≤ effective ≤ expiration).
    /// Ensures logical date ordering across premium lifecycle.
    /// </summary>
    ValidationResult ValidateDateSequence(PremiumRecord premium, Policy? policy);

    /// <summary>
    /// Validates premium amounts are within COMP-3 precision limits.
    /// Ensures no overflow in financial calculations.
    /// </summary>
    ValidationResult ValidatePremiumAmounts(PremiumRecord premium);

    /// <summary>
    /// Validates foreign key relationships exist.
    /// Checks ClientCode, PolicyNumber, ProductCode exist in respective tables.
    /// Returns warnings (not errors) for missing relationships per COBOL behavior.
    /// </summary>
    Task<ValidationResult> ValidateForeignKeysAsync(
        PremiumRecord premium,
        CancellationToken cancellationToken = default);
}
