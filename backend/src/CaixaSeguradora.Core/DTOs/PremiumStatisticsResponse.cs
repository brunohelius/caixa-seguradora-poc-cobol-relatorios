namespace CaixaSeguradora.Core.DTOs;

/// <summary>
/// Response DTO for premium statistics queries.
/// Provides aggregated metrics and insights.
/// </summary>
public class PremiumStatisticsResponse
{
    /// <summary>
    /// Total number of premium records analyzed.
    /// </summary>
    public int TotalRecords { get; set; }

    /// <summary>
    /// Sum of all premium amounts.
    /// </summary>
    public decimal TotalPremiumAmount { get; set; }

    /// <summary>
    /// Average premium amount.
    /// </summary>
    public decimal AveragePremiumAmount { get; set; }

    /// <summary>
    /// Minimum premium amount found.
    /// </summary>
    public decimal MinPremiumAmount { get; set; }

    /// <summary>
    /// Maximum premium amount found.
    /// </summary>
    public decimal MaxPremiumAmount { get; set; }

    /// <summary>
    /// Number of unique policies.
    /// </summary>
    public int UniquePolicyCount { get; set; }

    /// <summary>
    /// Number of unique products.
    /// </summary>
    public int UniqueProductCount { get; set; }

    /// <summary>
    /// Statistics by movement type (E, C, R, etc.).
    /// </summary>
    public Dictionary<string, MovementTypeStatistics> ByMovementType { get; set; } = new();

    /// <summary>
    /// Statistics by product.
    /// </summary>
    public List<ProductStatistics> ByProduct { get; set; } = new();

    /// <summary>
    /// Time series data (by date).
    /// </summary>
    public List<TimeSeriesDataPoint> TimeSeries { get; set; } = new();
}

/// <summary>
/// Time series data point for trend analysis.
/// </summary>
public class TimeSeriesDataPoint
{
    /// <summary>
    /// Date of the data point.
    /// </summary>
    public DateTime Date { get; set; }

    /// <summary>
    /// Value at this date.
    /// </summary>
    public decimal Value { get; set; }

    /// <summary>
    /// Record count at this date.
    /// </summary>
    public int RecordCount { get; set; }
}

