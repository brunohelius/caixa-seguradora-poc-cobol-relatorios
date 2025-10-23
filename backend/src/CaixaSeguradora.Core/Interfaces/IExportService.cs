namespace CaixaSeguradora.Core.Interfaces;

/// <summary>
/// Export service interface for generating files in various formats.
/// Supports CSV, Excel, and PDF export for premium data, policies, and reports.
/// </summary>
/// <typeparam name="T">Entity type to export</typeparam>
public interface IExportService<T> where T : class
{
    /// <summary>
    /// Exports data to CSV format.
    /// </summary>
    /// <param name="data">Data to export</param>
    /// <param name="includeHeaders">Include column headers</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>CSV content as byte array</returns>
    Task<byte[]> ExportToCsvAsync(
        IEnumerable<T> data,
        bool includeHeaders = true,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Exports data to Excel format (.xlsx).
    /// </summary>
    /// <param name="data">Data to export</param>
    /// <param name="sheetName">Excel sheet name</param>
    /// <param name="includeHeaders">Include column headers</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Excel file content as byte array</returns>
    Task<byte[]> ExportToExcelAsync(
        IEnumerable<T> data,
        string sheetName = "Data",
        bool includeHeaders = true,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Exports data to PDF format.
    /// </summary>
    /// <param name="data">Data to export</param>
    /// <param name="title">Document title</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>PDF file content as byte array</returns>
    Task<byte[]> ExportToPdfAsync(
        IEnumerable<T> data,
        string title = "Data Export",
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Streams large datasets to CSV format without loading all into memory.
    /// Recommended for datasets with 10,000+ records.
    /// </summary>
    /// <param name="data">Async enumerable data stream</param>
    /// <param name="includeHeaders">Include column headers</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>CSV content stream</returns>
    Task<Stream> StreamToCsvAsync(
        IAsyncEnumerable<T> data,
        bool includeHeaders = true,
        CancellationToken cancellationToken = default);
}

/// <summary>
/// Specialized export service for premium records with additional formatting options.
/// </summary>
public interface IPremiumExportService
{
    /// <summary>
    /// Exports premium records to CSV with customizable column selection.
    /// </summary>
    /// <param name="data">Premium records to export</param>
    /// <param name="columns">Columns to include (null = all columns)</param>
    /// <param name="includeHeaders">Include column headers</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>CSV content as byte array</returns>
    Task<byte[]> ExportPremiumsToCsvAsync(
        IEnumerable<object> data,
        string[]? columns = null,
        bool includeHeaders = true,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Exports premium records to Excel with formatting and totals.
    /// </summary>
    /// <param name="data">Premium records to export</param>
    /// <param name="sheetName">Excel sheet name</param>
    /// <param name="includeTotals">Include summary totals row</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Excel file content as byte array</returns>
    Task<byte[]> ExportPremiumsToExcelAsync(
        IEnumerable<object> data,
        string sheetName = "Prêmios",
        bool includeTotals = true,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Exports premium records to PDF with charts and statistics.
    /// </summary>
    /// <param name="data">Premium records to export</param>
    /// <param name="title">Document title</param>
    /// <param name="includeCharts">Include visualization charts</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>PDF file content as byte array</returns>
    Task<byte[]> ExportPremiumsToPdfAsync(
        IEnumerable<object> data,
        string title = "Relatório de Prêmios",
        bool includeCharts = false,
        CancellationToken cancellationToken = default);
}
