using System.ComponentModel.DataAnnotations;

namespace CaixaSeguradora.Core.DTOs;

/// <summary>
/// Request DTO for loading mock data from external sources (CSV, JSON).
/// Supports User Story 5 - Manage Database Mock Data.
/// </summary>
public class MockDataLoadRequest
{
    /// <summary>
    /// Entity type to load data for (e.g., "premiums", "policies", "clients").
    /// Must match entity name in database schema.
    /// </summary>
    [Required(ErrorMessage = "Tipo de entidade é obrigatório")]
    [MaxLength(100)]
    public string EntityType { get; set; } = string.Empty;

    /// <summary>
    /// Data format (CSV or JSON).
    /// </summary>
    [Required(ErrorMessage = "Formato de dados é obrigatório")]
    public DataFormat Format { get; set; }

    /// <summary>
    /// File content as base64-encoded string (for CSV/JSON files).
    /// Alternative to FileStream for API requests.
    /// </summary>
    public string? FileContentBase64 { get; set; }

    /// <summary>
    /// Raw data content as string (for direct JSON/CSV input).
    /// </summary>
    public string? RawDataContent { get; set; }

    /// <summary>
    /// Whether to clear existing data before loading.
    /// Default: false (append mode).
    /// </summary>
    public bool ClearExistingData { get; set; } = false;

    /// <summary>
    /// Whether to validate foreign key relationships after loading.
    /// Default: true (recommended).
    /// </summary>
    public bool ValidateForeignKeys { get; set; } = true;

    /// <summary>
    /// CSV delimiter character (default: comma).
    /// Only applicable for CSV format.
    /// </summary>
    public char CsvDelimiter { get; set; } = ',';

    /// <summary>
    /// Whether CSV file has header row.
    /// Default: true.
    /// </summary>
    public bool CsvHasHeaders { get; set; } = true;

    /// <summary>
    /// Maximum number of records to import (0 = unlimited).
    /// Useful for testing with large files.
    /// </summary>
    public int MaxRecords { get; set; } = 0;
}

/// <summary>
/// Supported data formats for mock data loading.
/// </summary>
public enum DataFormat
{
    /// <summary>
    /// Comma-separated values format.
    /// </summary>
    Csv,

    /// <summary>
    /// JavaScript Object Notation format.
    /// </summary>
    Json
}
