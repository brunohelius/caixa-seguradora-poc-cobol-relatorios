using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Core.Interfaces;

/// <summary>
/// Repository interface for premium emission records (V0PREMIOS view).
/// Implements cursor-based batch processing using IAsyncEnumerable for memory efficiency.
/// </summary>
public interface IPremiumRepository : IRepository<PremiumRecord>
{
    /// <summary>
    /// Streams premium records for a given reference period using cursor pattern.
    /// Maps to COBOL cursor: CUR-V0PREMIOS declared at R0500-00-DECLARE-V0PREMIOS.
    /// </summary>
    /// <param name="companyCode">Company code filter</param>
    /// <param name="referenceYear">Reference year (YYYY)</param>
    /// <param name="referenceMonth">Reference month (MM)</param>
    /// <param name="systemCode">System code (e.g., "GL", "VC")</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Async enumerable stream of premium records</returns>
    IAsyncEnumerable<PremiumRecord> GetPremiumsForPeriodAsync(
        int companyCode,
        int referenceYear,
        int referenceMonth,
        string systemCode,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets premium records by policy number and endorsement.
    /// Used for cross-referencing and validation during report generation.
    /// </summary>
    /// <param name="policyNumber">Policy number</param>
    /// <param name="endorsementNumber">Endorsement number (0 for original policy)</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>List of matching premium records</returns>
    Task<List<PremiumRecord>> GetByPolicyAndEndorsementAsync(
        long policyNumber,
        int endorsementNumber,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Gets premium summary statistics for a reference period.
    /// Used for dashboard and reporting validation.
    /// </summary>
    /// <param name="referenceYear">Reference year</param>
    /// <param name="referenceMonth">Reference month</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Summary statistics (count, totals, etc.)</returns>
    Task<PremiumSummaryDto> GetPeriodSummaryAsync(
        int referenceYear,
        int referenceMonth,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Streams premium records for a date range for report generation.
    /// Used for generating reports that span multiple periods.
    /// </summary>
    /// <param name="startDate">Start date of the range</param>
    /// <param name="endDate">End date of the range</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Async enumerable stream of premium records</returns>
    IAsyncEnumerable<PremiumRecord> GetPremiumsForReportAsync(
        DateTime startDate,
        DateTime endDate,
        CancellationToken cancellationToken = default);
}

/// <summary>
/// Premium summary statistics DTO.
/// </summary>
public class PremiumSummaryDto
{
    public int TotalRecords { get; set; }
    public decimal TotalPremiumAmount { get; set; }
    public decimal TotalNetPremium { get; set; }
    public int UniquePolichyCount { get; set; }
    public Dictionary<string, int> MovementTypeCounts { get; set; } = new();
}
