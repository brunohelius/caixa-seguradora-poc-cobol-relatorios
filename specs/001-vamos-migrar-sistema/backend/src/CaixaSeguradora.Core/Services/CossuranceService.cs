using System.Globalization;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Interfaces;
using CaixaSeguradora.Core.Utilities;

namespace CaixaSeguradora.Core.Services;

/// <summary>
/// Implementation of cossurance and ceded premium calculations.
/// Migrates COBOL sections R3000-R5500 to C# with exact arithmetic precision.
/// </summary>
/// <remarks>
/// CRITICAL: All decimal operations use COBOL-compatible rounding and truncation.
/// Output must match legacy system byte-for-byte for regulatory compliance.
///
/// COBOL Section Implementation:
/// - R3000-00-GRAVA-COSSEG-CED: GenerateCossuranceOutputRecord
/// - R3100-00-PROCESSA-COSG-CED: AccumulateCossuranceValues
/// - R4600-00-SELECT-GE397: GetCossuranceReferenceData (via repository)
/// - R4700-00-PROCESSA-APOL-COSG: HasCossuranceArrangementAsync
/// - R4800-00-SELECT-V0APOLCOSCED: GetByPolicyAndCompany (via repository)
/// - R4900-00-DECLARE-V0APOLCOSCED: Cursor declaration (via IAsyncEnumerable)
/// - R5000-00-FETCH-V0APOLCOSCED: Fetch operation (via repository streaming)
/// - R5100-00-PROCESSA-COSG-COBT: DistributeCededPremium
/// - R5200-00-SELECT-GE399: GetCalculationParametersAsync (via repository)
/// - R5300-00-DECLARE-GE399: Cursor declaration for GE399
/// - R5400-00-FETCH-GE399: Fetch GE399 records
/// - R5500-00-CALCULA-COSG-CED: CalculateCededPremium, CalculateRetainedPremium
/// </remarks>
public class CossuranceService : ICossuranceService
{
    private readonly ICossuredPolicyRepository _cossuredPolicyRepository;
    private readonly ICossuranceCalculationRepository _cossuranceCalculationRepository;

    // Tolerance for percentage validation (0.01% = 0.0001)
    private const decimal PercentageTolerance = 0.0001m;

    public CossuranceService(
        ICossuredPolicyRepository cossuredPolicyRepository,
        ICossuranceCalculationRepository cossuranceCalculationRepository)
    {
        _cossuredPolicyRepository = cossuredPolicyRepository ?? throw new ArgumentNullException(nameof(cossuredPolicyRepository));
        _cossuranceCalculationRepository = cossuranceCalculationRepository ?? throw new ArgumentNullException(nameof(cossuranceCalculationRepository));
    }

    /// <inheritdoc />
    public decimal CalculateCededPremium(decimal totalPremium, decimal cededPercentage)
    {
        if (totalPremium < 0)
            throw new ArgumentException("Total premium cannot be negative", nameof(totalPremium));

        if (cededPercentage < 0 || cededPercentage > 1)
            throw new ArgumentException("Ceded percentage must be between 0 and 1", nameof(cededPercentage));

        // COBOL: COMPUTE WS-PREMIO-CEDIDO = WS-PREMIO-TOTAL * WS-PERC-CESSAO ROUNDED
        // Maps to section R5500-00-CALCULA-COSG-CED
        var cededPremium = totalPremium * cededPercentage;

        // Use COBOL ROUNDED semantics (MidpointRounding.AwayFromZero)
        return CobolMath.RoundHalfUp(cededPremium, 2);
    }

    /// <inheritdoc />
    public decimal CalculateRetainedPremium(decimal totalPremium, decimal cededPremium)
    {
        if (totalPremium < 0)
            throw new ArgumentException("Total premium cannot be negative", nameof(totalPremium));

        if (cededPremium < 0)
            throw new ArgumentException("Ceded premium cannot be negative", nameof(cededPremium));

        if (cededPremium > totalPremium)
            throw new ArgumentException("Ceded premium cannot exceed total premium", nameof(cededPremium));

        // COBOL: COMPUTE WS-PREMIO-RETIDO = WS-PREMIO-TOTAL - WS-PREMIO-CEDIDO ROUNDED
        var retainedPremium = totalPremium - cededPremium;

        return CobolMath.RoundHalfUp(retainedPremium, 2);
    }

    /// <inheritdoc />
    public Dictionary<int, decimal> DistributeCededPremium(
        decimal cededPremium,
        List<CossuranceParticipant> participants)
    {
        ArgumentNullException.ThrowIfNull(participants);

        if (participants.Count == 0)
            throw new ArgumentException("At least one participant is required", nameof(participants));

        if (!ValidateCossurancePercentages(participants))
            throw new ArgumentException("Invalid cossurance percentages: sum must equal 100%", nameof(participants));

        // Maps to COBOL section R5100-00-PROCESSA-COSG-COBT
        var distribution = new Dictionary<int, decimal>();
        decimal totalDistributed = 0m;

        // Distribute to all participants except the last one
        for (int i = 0; i < participants.Count - 1; i++)
        {
            var participant = participants[i];
            var participantShare = cededPremium * participant.QuotaPercentage;
            participantShare = CobolMath.RoundHalfUp(participantShare, 2);

            distribution[participant.CompanyCode] = participantShare;
            totalDistributed += participantShare;
        }

        // Last participant gets the remainder to ensure exact total
        // This handles rounding differences - CRITICAL for regulatory compliance
        var lastParticipant = participants[^1];
        var lastParticipantShare = cededPremium - totalDistributed;
        distribution[lastParticipant.CompanyCode] = lastParticipantShare;

        return distribution;
    }

    /// <inheritdoc />
    public decimal CalculateAcquisitionCommission(decimal acquiredPremium, decimal commissionRate)
    {
        if (acquiredPremium < 0)
            throw new ArgumentException("Acquired premium cannot be negative", nameof(acquiredPremium));

        if (commissionRate < 0 || commissionRate > 1)
            throw new ArgumentException("Commission rate must be between 0 and 1", nameof(commissionRate));

        // COBOL: COMPUTE WS-COMISSAO-COSG = WS-PREMIO-OBTIDO * WS-TAXA-COMISSAO ROUNDED
        var commission = acquiredPremium * commissionRate;

        return CobolMath.RoundHalfUp(commission, 2);
    }

    /// <inheritdoc />
    public string GenerateCossuranceOutputRecord(
        PremiumRecord premium,
        CossuredPolicy cossuredPolicy,
        CossuranceCalculation calculation)
    {
        ArgumentNullException.ThrowIfNull(premium);
        ArgumentNullException.ThrowIfNull(cossuredPolicy);
        ArgumentNullException.ThrowIfNull(calculation);

        // Maps to COBOL section R3000-00-GRAVA-COSSEG-CED
        // Generates fixed-width output record for PREMCED.TXT file
        // Must match COBOL byte-for-byte for regulatory compliance

        var recordParts = new List<string>();

        // NOTE: Using available PremiumRecord properties
        // Some fields may need to be added to entity or derived from related entities

        // Field 1: System Code (2 characters)
        recordParts.Add(FormatAlphanumeric(premium.SystemCode, 2));

        // Field 2: Policy Number (15 digits)
        recordParts.Add(FormatNumeric((int)premium.PolicyNumber, 15));

        // Field 3: Endorsement Number (10 digits - using actual field size)
        recordParts.Add(FormatNumeric(premium.EndorsementNumber, 10));

        // Field 4: Cossurer Company Code (6 digits)
        recordParts.Add(FormatNumeric(cossuredPolicy.CossurerCode, 6));

        // Field 5: Cossurer Name (60 characters, right-padded with spaces)
        recordParts.Add(FormatAlphanumeric(cossuredPolicy.CossurerName, 60));

        // Field 6: Participation Percentage (5 digits: 999V99 format)
        // Example: 30.50% stored as 03050
        recordParts.Add(FormatNumeric(cossuredPolicy.ParticipationPercentage, 5, 2));

        // Field 7: Cossurance Type (1 character: 'A'=Aceito, 'C'=Cedido)
        recordParts.Add(FormatAlphanumeric(cossuredPolicy.CossuranceType, 1));

        // Field 8: Total Gross Premium (15 digits: 9(13)V99 format)
        recordParts.Add(FormatNumeric(calculation.TotalGrossPremium, 15, 2));

        // Field 9: Total Net Premium (15 digits: 9(13)V99 format)
        recordParts.Add(FormatNumeric(calculation.TotalNetPremium, 15, 2));

        // Field 10: Cossurer Premium (15 digits: 9(13)V99 format)
        recordParts.Add(FormatNumeric(calculation.CossurerPremium, 15, 2));

        // Field 11: Cossurer Commission (15 digits: 9(13)V99 format)
        recordParts.Add(FormatNumeric(calculation.CossurerCommission, 15, 2));

        // Field 12: Total IOF (15 digits: 9(13)V99 format)
        recordParts.Add(FormatNumeric(calculation.TotalIOF, 15, 2));

        // Field 13: Line of Business Code (4 digits)
        recordParts.Add(FormatNumeric(premium.LineOfBusinessCode, 4));

        // Field 14: Product Code (4 digits)
        recordParts.Add(FormatNumeric(premium.ProductCode, 4));

        // Concatenate all parts into single fixed-width record
        return string.Join("", recordParts);
    }

    /// <inheritdoc />
    public bool ValidateCossurancePercentages(List<CossuranceParticipant> participants)
    {
        ArgumentNullException.ThrowIfNull(participants);

        if (participants.Count == 0)
            return false;

        // Validate individual percentages
        foreach (var participant in participants)
        {
            if (participant.QuotaPercentage < 0 || participant.QuotaPercentage > 1)
                return false;
        }

        // Validate sum equals 1.0 (100%) within tolerance
        var totalPercentage = participants.Sum(p => p.QuotaPercentage);
        return Math.Abs(totalPercentage - 1.0m) <= PercentageTolerance;
    }

    /// <inheritdoc />
    public char GetCossuranceType(int companyCode, int cedingCompanyCode, int acquiringCompanyCode)
    {
        // Determine cossurance type based on company roles
        // 'C' = Ceded (Cedido) - company is ceding risk to others
        // 'A' = Acquired (Aceito/Obtido) - company is acquiring risk from others

        if (companyCode == cedingCompanyCode)
            return 'C'; // This company is ceding the risk

        if (companyCode == acquiringCompanyCode)
            return 'A'; // This company is acquiring the risk

        return 'U'; // Unknown - company is neither ceding nor acquiring
    }

    /// <inheritdoc />
    public void AccumulateCossuranceValues(
        decimal cededPremium,
        decimal retainedPremium,
        CossuranceAccumulator accumulator)
    {
        ArgumentNullException.ThrowIfNull(accumulator);

        // Maps to COBOL section R3100-00-PROCESSA-COSG-CED
        // Accumulates totals for cossurance processing

        accumulator.TotalCededPremium += cededPremium;
        accumulator.TotalRetainedPremium += retainedPremium;

        if (cededPremium > 0)
            accumulator.CededRecordCount++;
    }

    /// <inheritdoc />
    public decimal CalculateQuotaAdjustment(decimal originalPremium, decimal originalQuota, decimal newQuota)
    {
        if (originalPremium < 0)
            throw new ArgumentException("Original premium cannot be negative", nameof(originalPremium));

        if (originalQuota < 0 || originalQuota > 1)
            throw new ArgumentException("Original quota must be between 0 and 1", nameof(originalQuota));

        if (newQuota < 0 || newQuota > 1)
            throw new ArgumentException("New quota must be between 0 and 1", nameof(newQuota));

        // Calculate adjustment amount when quota changes mid-policy
        // COBOL: COMPUTE WS-AJUSTE-QUOTA = WS-PREMIO * (WS-NOVA-QUOTA - WS-QUOTA-ORIG) ROUNDED

        var quotaDifference = newQuota - originalQuota;
        var adjustmentAmount = originalPremium * quotaDifference;

        return CobolMath.RoundHalfUp(adjustmentAmount, 2);
    }

    /// <inheritdoc />
    public async Task<bool> HasCossuranceArrangementAsync(
        Policy policy,
        PremiumRecord premium,
        CancellationToken cancellationToken = default)
    {
        ArgumentNullException.ThrowIfNull(policy);
        ArgumentNullException.ThrowIfNull(premium);

        // Maps to COBOL section R4700-00-PROCESSA-APOL-COSG
        // Determines if a policy participates in cossurance

        // Check if policy has any cossurance records
        await foreach (var cossuredPolicy in _cossuredPolicyRepository
            .GetCossuredPoliciesAsync(premium.PolicyNumber, cancellationToken))
        {
            // If we get at least one record, check for calculation data
            // Note: CompanyCode is not in PremiumRecord, using 0 as placeholder
            var calculationData = await _cossuranceCalculationRepository
                .GetCalculationParametersAsync(premium.PolicyNumber, 0, cancellationToken);

            return calculationData != null;
        }

        return false;
    }

    /// <inheritdoc />
    public async Task<List<CossuranceParticipant>> GetCossuranceParticipantsAsync(
        long policyNumber,
        CancellationToken cancellationToken = default)
    {
        // Maps to COBOL cursor operations in R4900/R5000-00-DECLARE/FETCH-V0APOLCOSCED

        var participants = new List<CossuranceParticipant>();

        await foreach (var cossuredPolicy in _cossuredPolicyRepository
            .GetCossuredPoliciesAsync(policyNumber, cancellationToken))
        {
            var participant = new CossuranceParticipant
            {
                CompanyCode = cossuredPolicy.CossurerCode,
                CompanyName = cossuredPolicy.CossurerName,
                QuotaPercentage = cossuredPolicy.ParticipationPercentage / 100m, // Convert from percentage to decimal (30.5 -> 0.305)
                ParticipationType = cossuredPolicy.CossuranceType.Length > 0 ? cossuredPolicy.CossuranceType[0] : 'U',
                CommissionRate = 0m // Would need to be looked up from commission table
            };

            participants.Add(participant);
        }

        return participants;
    }

    /// <inheritdoc />
    public async Task<CossuranceCalculation?> GetCossuranceCalculationAsync(
        long policyNumber,
        int endorsementNumber,
        CancellationToken cancellationToken = default)
    {
        // Maps to COBOL sections R5200/R5300/R5400-00 for GE399 access
        // Note: The repository method doesn't currently support endorsement number filtering
        // This is a simplified implementation - production code would need the full query

        return await _cossuranceCalculationRepository
            .GetCalculationParametersAsync(policyNumber, 0, cancellationToken);
    }

    // Private helper methods for fixed-width formatting (COBOL PIC clause compatibility)

    private static string FormatNumeric(int value, int totalWidth)
    {
        var absoluteValue = Math.Abs(value);
        return absoluteValue.ToString(CultureInfo.InvariantCulture).PadLeft(totalWidth, '0');
    }

    private static string FormatNumeric(decimal value, int totalWidth, int decimalPlaces = 0)
    {
        var absoluteValue = Math.Abs(value);

        // COBOL style: implied decimal point (e.g., "0012345" for 123.45 with V99)
        // Multiply by 10^decimalPlaces to shift decimal
        var multiplier = (decimal)Math.Pow(10, decimalPlaces);
        var shiftedValue = Math.Round(absoluteValue * multiplier, 0, MidpointRounding.AwayFromZero);

        // Convert to integer string and pad
        var intValue = (long)shiftedValue;
        return intValue.ToString(CultureInfo.InvariantCulture).PadLeft(totalWidth, '0');
    }

    private static string FormatAlphanumeric(string? value, int totalWidth, bool truncate = true)
    {
        // Handle null or empty strings
        if (string.IsNullOrEmpty(value))
            return new string(' ', totalWidth);

        // Truncate if needed and allowed
        if (value.Length > totalWidth)
        {
            if (truncate)
                return value.Substring(0, totalWidth);
            else
                throw new ArgumentException($"Value length ({value.Length}) exceeds total width ({totalWidth})", nameof(value));
        }

        // Pad with spaces on the right (COBOL PIC X behavior)
        return value.PadRight(totalWidth, ' ');
    }
}
