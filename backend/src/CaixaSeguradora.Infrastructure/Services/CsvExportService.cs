using System.Globalization;
using System.Runtime.CompilerServices;
using System.Text;
using CaixaSeguradora.Core.Interfaces;
using Microsoft.Extensions.Logging;

namespace CaixaSeguradora.Infrastructure.Services;

/// <summary>
/// CSV export service implementation.
/// Generates CSV files with proper escaping, encoding (UTF-8 with BOM for Excel compatibility),
/// and configurable delimiters.
/// </summary>
/// <typeparam name="T">Entity type to export</typeparam>
public class CsvExportService<T> : IExportService<T> where T : class
{
    private readonly ILogger<CsvExportService<T>> _logger;
    private const char Delimiter = ',';
    private const char Quote = '"';

    public CsvExportService(ILogger<CsvExportService<T>> logger)
    {
        _logger = logger ?? throw new ArgumentNullException(nameof(logger));
    }

    /// <inheritdoc />
    public async Task<byte[]> ExportToCsvAsync(
        IEnumerable<T> data,
        bool includeHeaders = true,
        CancellationToken cancellationToken = default)
    {
        _logger.LogInformation("Starting CSV export for {TypeName}", typeof(T).Name);

        var dataList = data.ToList();
        if (!dataList.Any())
        {
            _logger.LogWarning("No data to export for {TypeName}", typeof(T).Name);
            return Array.Empty<byte>();
        }

        var csvContent = new StringBuilder();

        // Get properties via reflection
        var properties = typeof(T).GetProperties()
            .Where(p => p.CanRead && IsExportableType(p.PropertyType))
            .ToList();

        // Write headers
        if (includeHeaders)
        {
            IEnumerable<string> headers = properties.Select(p => EscapeCsvField(p.Name));
            csvContent.AppendLine(string.Join(Delimiter, headers));
        }

        // Write data rows
        foreach (T? item in dataList)
        {
            cancellationToken.ThrowIfCancellationRequested();

            IEnumerable<string> values = properties.Select(p =>
            {
                var value = p.GetValue(item);
                return FormatCsvValue(value);
            });

            csvContent.AppendLine(string.Join(Delimiter, values));
        }

        // Convert to bytes with UTF-8 BOM for Excel compatibility
        var bytes = Encoding.UTF8.GetPreamble()
            .Concat(Encoding.UTF8.GetBytes(csvContent.ToString()))
            .ToArray();

        _logger.LogInformation("CSV export completed. Rows: {Count}, Size: {Size} bytes",
            dataList.Count, bytes.Length);

        return await Task.FromResult(bytes);
    }

    /// <inheritdoc />
    public async Task<byte[]> ExportToExcelAsync(
        IEnumerable<T> data,
        string sheetName = "Data",
        bool includeHeaders = true,
        CancellationToken cancellationToken = default)
    {
        // CSV export works with Excel - this is a simpler alternative to full Excel generation
        // For true .xlsx format, use ExcelExportService instead
        _logger.LogInformation("Exporting to CSV format (Excel compatible) for {TypeName}", typeof(T).Name);
        return await ExportToCsvAsync(data, includeHeaders, cancellationToken);
    }

    /// <inheritdoc />
    public Task<byte[]> ExportToPdfAsync(
        IEnumerable<T> data,
        string title = "Data Export",
        CancellationToken cancellationToken = default)
    {
        throw new NotImplementedException(
            "PDF export not supported by CsvExportService. Use PdfExportService instead.");
    }

    /// <inheritdoc />
    public async Task<Stream> StreamToCsvAsync(
        IAsyncEnumerable<T> data,
        bool includeHeaders = true,
        CancellationToken cancellationToken = default)
    {
        _logger.LogInformation("Starting streaming CSV export for {TypeName}", typeof(T).Name);

        var stream = new MemoryStream();
        var writer = new StreamWriter(stream, Encoding.UTF8, leaveOpen: true);

        // Write UTF-8 BOM
        await stream.WriteAsync(Encoding.UTF8.GetPreamble(), cancellationToken);

        // Get properties via reflection
        var properties = typeof(T).GetProperties()
            .Where(p => p.CanRead && IsExportableType(p.PropertyType))
            .ToList();

        // Write headers
        if (includeHeaders)
        {
            IEnumerable<string> headers = properties.Select(p => EscapeCsvField(p.Name));
            await writer.WriteLineAsync(string.Join(Delimiter, headers));
        }

        // Stream data rows
        var rowCount = 0;
        await foreach (T? item in data.WithCancellation(cancellationToken))
        {
            IEnumerable<string> values = properties.Select(p =>
            {
                var value = p.GetValue(item);
                return FormatCsvValue(value);
            });

            await writer.WriteLineAsync(string.Join(Delimiter, values));
            rowCount++;

            // Flush periodically to avoid memory buildup
            if (rowCount % 1000 == 0)
            {
                await writer.FlushAsync();
            }
        }

        await writer.FlushAsync();
        stream.Position = 0;

        _logger.LogInformation("Streaming CSV export completed. Rows: {Count}", rowCount);

        return stream;
    }

    /// <summary>
    /// Formats a value for CSV output with proper escaping and null handling.
    /// </summary>
    private string FormatCsvValue(object? value)
    {
        if (value == null)
        {
            return string.Empty;
        }

        // Handle different types
        var formattedValue = value switch
        {
            DateTime dt => dt.ToString("yyyy-MM-dd HH:mm:ss", CultureInfo.InvariantCulture),
            DateTimeOffset dto => dto.ToString("yyyy-MM-dd HH:mm:ss", CultureInfo.InvariantCulture),
            decimal d => d.ToString("F2", CultureInfo.InvariantCulture),
            double dbl => dbl.ToString("F2", CultureInfo.InvariantCulture),
            float flt => flt.ToString("F2", CultureInfo.InvariantCulture),
            bool b => b ? "Sim" : "Não",
            _ => value.ToString() ?? string.Empty
        };

        return EscapeCsvField(formattedValue);
    }

    /// <summary>
    /// Escapes a CSV field by wrapping in quotes if necessary.
    /// Fields are quoted if they contain delimiter, quotes, or newlines.
    /// </summary>
    private string EscapeCsvField(string field)
    {
        if (string.IsNullOrEmpty(field))
        {
            return string.Empty;
        }

        // Check if quoting is needed
        if (field.Contains(Delimiter) || field.Contains(Quote) || field.Contains('\n') || field.Contains('\r'))
        {
            // Escape existing quotes by doubling them
            var escaped = field.Replace(Quote.ToString(), $"{Quote}{Quote}");
            return $"{Quote}{escaped}{Quote}";
        }

        return field;
    }

    /// <summary>
    /// Checks if a type is exportable to CSV (primitive, string, DateTime, decimal, etc.).
    /// Excludes complex objects and collections.
    /// </summary>
    private bool IsExportableType(Type type)
    {
        // Unwrap nullable types
        Type underlyingType = Nullable.GetUnderlyingType(type) ?? type;

        return underlyingType.IsPrimitive
            || underlyingType == typeof(string)
            || underlyingType == typeof(DateTime)
            || underlyingType == typeof(DateTimeOffset)
            || underlyingType == typeof(decimal)
            || underlyingType == typeof(Guid)
            || underlyingType.IsEnum;
    }
}

/// <summary>
/// Specialized CSV export service for premium records with financial formatting.
/// </summary>
public class PremiumCsvExportService : IPremiumExportService
{
    private readonly ILogger<PremiumCsvExportService> _logger;

    public PremiumCsvExportService(ILogger<PremiumCsvExportService> logger)
    {
        _logger = logger ?? throw new ArgumentNullException(nameof(logger));
    }

    /// <inheritdoc />
    public async Task<byte[]> ExportPremiumsToCsvAsync(
        IEnumerable<object> data,
        string[]? columns = null,
        bool includeHeaders = true,
        CancellationToken cancellationToken = default)
    {
        _logger.LogInformation("Starting premium CSV export");

        var dataList = data.ToList();
        if (!dataList.Any())
        {
            _logger.LogWarning("No premium data to export");
            return Array.Empty<byte>();
        }

        var csvContent = new StringBuilder();

        // Default columns for premium export
        var defaultColumns = new[]
        {
            "PolicyNumber", "EndorsementNumber", "ReferenceDate", "ProductCode",
            "LineOfBusiness", "ClientCode", "TotalPremiumNet", "MovementType", "CompanyCode"
        };

        var selectedColumns = columns ?? defaultColumns;

        // Write headers
        if (includeHeaders)
        {
            csvContent.AppendLine(string.Join(",", selectedColumns));
        }

        // Write data rows using dynamic property access
        foreach (var item in dataList)
        {
            cancellationToken.ThrowIfCancellationRequested();

            Type type = item.GetType();
            IEnumerable<string> values = selectedColumns.Select(col =>
            {
                System.Reflection.PropertyInfo? property = type.GetProperty(col);
                if (property == null)
                {
                    return string.Empty;
                }

                var value = property.GetValue(item);
                return FormatPremiumValue(value, property.PropertyType);
            });

            csvContent.AppendLine(string.Join(",", values));
        }

        var bytes = Encoding.UTF8.GetPreamble()
            .Concat(Encoding.UTF8.GetBytes(csvContent.ToString()))
            .ToArray();

        _logger.LogInformation("Premium CSV export completed. Rows: {Count}", dataList.Count);

        return await Task.FromResult(bytes);
    }

    /// <inheritdoc />
    public Task<byte[]> ExportPremiumsToExcelAsync(
        IEnumerable<object> data,
        string sheetName = "Prêmios",
        bool includeTotals = true,
        CancellationToken cancellationToken = default)
    {
        throw new NotImplementedException(
            "Excel export for premiums requires ExcelExportService. Use ExportPremiumsToCsvAsync for CSV format.");
    }

    /// <inheritdoc />
    public Task<byte[]> ExportPremiumsToPdfAsync(
        IEnumerable<object> data,
        string title = "Relatório de Prêmios",
        bool includeCharts = false,
        CancellationToken cancellationToken = default)
    {
        throw new NotImplementedException(
            "PDF export for premiums requires PdfExportService.");
    }

    private string FormatPremiumValue(object? value, Type propertyType)
    {
        if (value == null)
        {
            return string.Empty;
        }

        // Financial values with 2 decimal places
        if (propertyType == typeof(decimal) || propertyType == typeof(decimal?))
        {
            return ((decimal)value).ToString("F2", CultureInfo.InvariantCulture);
        }

        // Dates in ISO format
        if (propertyType == typeof(DateTime) || propertyType == typeof(DateTime?))
        {
            return ((DateTime)value).ToString("yyyy-MM-dd", CultureInfo.InvariantCulture);
        }

        return value.ToString() ?? string.Empty;
    }
}
