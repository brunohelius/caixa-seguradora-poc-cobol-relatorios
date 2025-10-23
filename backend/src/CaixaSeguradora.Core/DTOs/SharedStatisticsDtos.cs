namespace CaixaSeguradora.Core.DTOs;

/// <summary>
/// Statistics for a specific movement type.
/// Shared across multiple statistic response types.
/// </summary>
public class MovementTypeStatistics
{
    /// <summary>
    /// Movement type code (E, C, R, etc.).
    /// </summary>
    public string MovementType { get; set; } = string.Empty;

    /// <summary>
    /// Number of records with this movement type.
    /// </summary>
    public long RecordCount { get; set; }

    /// <summary>
    /// Sum of net premiums for this movement type.
    /// </summary>
    public decimal TotalNetPremium { get; set; }

    /// <summary>
    /// Percentage of total records.
    /// </summary>
    public decimal PercentageOfTotal { get; set; }
}

/// <summary>
/// Statistics for a specific product.
/// Shared across multiple statistic response types.
/// </summary>
public class ProductStatistics
{
    /// <summary>
    /// Product code.
    /// </summary>
    public int ProductCode { get; set; }

    /// <summary>
    /// Product description.
    /// </summary>
    public string? ProductDescription { get; set; }

    /// <summary>
    /// Number of records for this product.
    /// </summary>
    public long RecordCount { get; set; }

    /// <summary>
    /// Sum of net premiums for this product.
    /// </summary>
    public decimal TotalNetPremium { get; set; }

    /// <summary>
    /// Percentage of total records.
    /// </summary>
    public decimal PercentageOfTotal { get; set; }
}
