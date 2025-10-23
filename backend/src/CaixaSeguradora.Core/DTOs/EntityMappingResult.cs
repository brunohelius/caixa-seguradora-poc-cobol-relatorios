namespace CaixaSeguradora.Core.DTOs;

/// <summary>
/// Result of mapping CSV row to entity.
/// </summary>
/// <typeparam name="T">Entity type</typeparam>
public class EntityMappingResult<T> where T : class
{
    /// <summary>
    /// Whether mapping succeeded.
    /// </summary>
    public bool Success { get; set; }

    /// <summary>
    /// Mapped entity instance (null if mapping failed).
    /// </summary>
    public T? Entity { get; set; }

    /// <summary>
    /// Row number in CSV.
    /// </summary>
    public int RowNumber { get; set; }

    /// <summary>
    /// Errors encountered during mapping.
    /// </summary>
    public List<string> Errors { get; set; } = new();

    /// <summary>
    /// Warnings encountered during mapping (non-fatal).
    /// </summary>
    public List<string> Warnings { get; set; } = new();
}
