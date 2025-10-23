using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Core.Interfaces;

/// <summary>
/// Service interface for cossurance and ceded premium calculations.
/// Implements cossurance business logic from COBOL sections R3000-R5500.
/// Handles distribution of premiums among cossurance participants (ceding and acquiring companies).
/// </summary>
/// <remarks>
/// COBOL Section Mapping:
/// - R3000-00-GRAVA-COSSEG-CED: Save cossurance/ceded data to output file
/// - R3100-00-PROCESSA-COSG-CED: Process cossurance/ceded records
/// - R4600-00-SELECT-GE397: Access GE397 table (cossurance master data)
/// - R4700-00-PROCESSA-APOL-COSG: Process cossurance policies
/// - R4800-00-SELECT-V0APOLCOSCED: Select cossurance/ceded policy records
/// - R4900-00-DECLARE-V0APOLCOSCED: Declare cursor for cossurance policies
/// - R5000-00-FETCH-V0APOLCOSCED: Fetch cossurance policy records
/// - R5100-00-PROCESSA-COSG-COBT: Process acquired cossurance (cosseguro obtido)
/// - R5200-00-SELECT-GE399: Access GE399 table (cossurance calculation data)
/// - R5300-00-DECLARE-GE399: Declare cursor for GE399
/// - R5400-00-FETCH-GE399: Fetch GE399 calculation records
/// - R5500-00-CALCULA-COSG-CED: Calculate cossurance/ceded amounts
/// </remarks>
public interface ICossuranceService
{
    /// <summary>
    /// Calculates ceded premium amount based on cossurance agreement.
    /// Maps to COBOL section R5500-00-CALCULA-COSG-CED.
    /// </summary>
    /// <param name="totalPremium">Total premium amount from policy</param>
    /// <param name="cededPercentage">Percentage ceded to cossurers (e.g., 0.30 for 30%)</param>
    /// <returns>Ceded premium amount</returns>
    /// <remarks>
    /// Formula: CededPremium = TotalPremium * CededPercentage
    /// Uses COBOL rounding for regulatory compliance
    /// </remarks>
    decimal CalculateCededPremium(decimal totalPremium, decimal cededPercentage);

    /// <summary>
    /// Calculates retained premium (premium kept by ceding company).
    /// </summary>
    /// <param name="totalPremium">Total premium amount</param>
    /// <param name="cededPremium">Premium ceded to cossurers</param>
    /// <returns>Retained premium amount</returns>
    /// <remarks>
    /// Formula: RetainedPremium = TotalPremium - CededPremium
    /// </remarks>
    decimal CalculateRetainedPremium(decimal totalPremium, decimal cededPremium);

    /// <summary>
    /// Distributes ceded premium among multiple cossurance participants.
    /// Maps to COBOL section R5100-00-PROCESSA-COSG-COBT.
    /// </summary>
    /// <param name="cededPremium">Total premium to be distributed</param>
    /// <param name="participants">List of cossurance participants with their quota percentages</param>
    /// <returns>Dictionary mapping participant company code to their premium share</returns>
    /// <remarks>
    /// Each participant receives: ParticipantPremium = CededPremium * ParticipantQuota
    /// Sum of all participant quotas must equal 1.0 (100%)
    /// Handles rounding so sum of distributed amounts equals ceded premium exactly
    /// </remarks>
    Dictionary<int, decimal> DistributeCededPremium(
        decimal cededPremium,
        List<CossuranceParticipant> participants);

    /// <summary>
    /// Calculates commission for cossurance acquired (cosseguro obtido).
    /// When company acquires portion of another company's policy.
    /// </summary>
    /// <param name="acquiredPremium">Premium amount acquired from ceding company</param>
    /// <param name="commissionRate">Commission rate for acquisition</param>
    /// <returns>Commission amount</returns>
    decimal CalculateAcquisitionCommission(decimal acquiredPremium, decimal commissionRate);

    /// <summary>
    /// Processes cossurance policy to generate PREMCED file output record.
    /// Maps to COBOL section R3000-00-GRAVA-COSSEG-CED.
    /// </summary>
    /// <param name="premium">Original premium record</param>
    /// <param name="cossuredPolicy">Cossurance policy arrangement</param>
    /// <param name="calculation">Cossurance calculation from GE399</param>
    /// <returns>Formatted cossurance output record for PREMCED.TXT file</returns>
    /// <remarks>
    /// Output record must match COBOL fixed-width format exactly
    /// Uses FixedWidthFormatter for byte-level compatibility
    /// </remarks>
    string GenerateCossuranceOutputRecord(
        PremiumRecord premium,
        CossuredPolicy cossuredPolicy,
        CossuranceCalculation calculation);

    /// <summary>
    /// Validates cossurance arrangement percentages.
    /// Ensures all participant quotas sum to 100%.
    /// </summary>
    /// <param name="participants">List of cossurance participants</param>
    /// <returns>True if valid, false otherwise</returns>
    /// <remarks>
    /// Validation rules:
    /// - Sum of all participant percentages must equal 1.0 (allowing small rounding tolerance)
    /// - Each individual percentage must be between 0 and 1
    /// - At least one participant must exist
    /// </remarks>
    bool ValidateCossurancePercentages(List<CossuranceParticipant> participants);

    /// <summary>
    /// Gets cossurance type code based on arrangement.
    /// Determines if ceded (COB - cosseguro cedido) or acquired (OBT - cosseguro obtido).
    /// </summary>
    /// <param name="companyCode">Current company code</param>
    /// <param name="cedingCompanyCode">Code of company ceding the risk</param>
    /// <param name="acquiringCompanyCode">Code of company acquiring the risk</param>
    /// <returns>Cossurance type code ('C' for ceded, 'A' for acquired)</returns>
    char GetCossuranceType(int companyCode, int cedingCompanyCode, int acquiringCompanyCode);

    /// <summary>
    /// Accumulates cossurance totals for reporting.
    /// Maps to section R3100-00-PROCESSA-COSG-CED.
    /// </summary>
    /// <param name="cededPremium">Ceded premium amount</param>
    /// <param name="retainedPremium">Retained premium amount</param>
    /// <param name="accumulator">Cossurance accumulator structure</param>
    void AccumulateCossuranceValues(
        decimal cededPremium,
        decimal retainedPremium,
        CossuranceAccumulator accumulator);

    /// <summary>
    /// Processes quota adjustment for proportional cossurance.
    /// Handles cases where quota percentages change during policy period.
    /// </summary>
    /// <param name="originalPremium">Original premium amount</param>
    /// <param name="originalQuota">Original quota percentage</param>
    /// <param name="newQuota">New quota percentage after adjustment</param>
    /// <returns>Adjustment amount (positive or negative)</returns>
    decimal CalculateQuotaAdjustment(decimal originalPremium, decimal originalQuota, decimal newQuota);

    /// <summary>
    /// Determines if policy participates in cossurance arrangement.
    /// Maps to COBOL logic in R4700-00-PROCESSA-APOL-COSG.
    /// </summary>
    /// <param name="policy">Policy to check</param>
    /// <param name="premium">Related premium record</param>
    /// <returns>True if policy has cossurance arrangement</returns>
    /// <remarks>
    /// Checks:
    /// - Policy has cossurance flag set
    /// - V0APOLCOSCED records exist for policy
    /// - GE399 calculation records exist
    /// </remarks>
    Task<bool> HasCossuranceArrangementAsync(
        Policy policy,
        PremiumRecord premium,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Retrieves all cossurance participants for a policy.
    /// Maps to COBOL cursor operations in R4900/R5000-00-DECLARE/FETCH-V0APOLCOSCED.
    /// </summary>
    /// <param name="policyNumber">Policy number</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>List of cossurance participants</returns>
    Task<List<CossuranceParticipant>> GetCossuranceParticipantsAsync(
        long policyNumber,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Retrieves cossurance calculation data from GE399 table.
    /// Maps to COBOL sections R5200/R5300/R5400-00 for GE399 access.
    /// </summary>
    /// <param name="policyNumber">Policy number</param>
    /// <param name="endorsementNumber">Endorsement number</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Cossurance calculation record</returns>
    Task<CossuranceCalculation?> GetCossuranceCalculationAsync(
        long policyNumber,
        int endorsementNumber,
        CancellationToken cancellationToken = default);
}

/// <summary>
/// Represents a cossurance participant (ceding or acquiring company).
/// Maps to V0APOLCOSCED table structure.
/// </summary>
public class CossuranceParticipant
{
    /// <summary>
    /// Company code of participant.
    /// </summary>
    public int CompanyCode { get; set; }

    /// <summary>
    /// Company name.
    /// </summary>
    public string CompanyName { get; set; } = string.Empty;

    /// <summary>
    /// Participant's quota percentage (0.0 to 1.0).
    /// Example: 0.30 represents 30% participation.
    /// </summary>
    public decimal QuotaPercentage { get; set; }

    /// <summary>
    /// Type of participation: 'C' (ceding) or 'A' (acquiring).
    /// </summary>
    public char ParticipationType { get; set; }

    /// <summary>
    /// Commission rate for this participant.
    /// </summary>
    public decimal CommissionRate { get; set; }
}

/// <summary>
/// Accumulator for cossurance totals.
/// Matches COBOL working storage accumulator structure.
/// </summary>
public class CossuranceAccumulator
{
    /// <summary>
    /// Total premium ceded to cossurers.
    /// </summary>
    public decimal TotalCededPremium { get; set; }

    /// <summary>
    /// Total premium retained by ceding company.
    /// </summary>
    public decimal TotalRetainedPremium { get; set; }

    /// <summary>
    /// Total premium acquired from other companies (cosseguro obtido).
    /// </summary>
    public decimal TotalAcquiredPremium { get; set; }

    /// <summary>
    /// Total commission on acquired cossurance.
    /// </summary>
    public decimal TotalAcquisitionCommission { get; set; }

    /// <summary>
    /// Count of cossured policies processed.
    /// </summary>
    public int CossuredPolicyCount { get; set; }

    /// <summary>
    /// Count of records with ceded premium.
    /// </summary>
    public int CededRecordCount { get; set; }

    /// <summary>
    /// Count of records with acquired premium.
    /// </summary>
    public int AcquiredRecordCount { get; set; }

    /// <summary>
    /// Totals by ceding/acquiring company (indexed by company code).
    /// </summary>
    public Dictionary<int, decimal> TotalsByCompany { get; set; } = new();
}
