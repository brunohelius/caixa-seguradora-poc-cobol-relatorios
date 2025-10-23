using System.Globalization;
using ClosedXML.Excel;
using CaixaSeguradora.Core.Interfaces;
using Microsoft.Extensions.Logging;

namespace CaixaSeguradora.Infrastructure.Services;

/// <summary>
/// Excel export service implementation using ClosedXML (.xlsx format).
/// Generates professional Excel files with formatting, formulas, and styling.
/// </summary>
/// <typeparam name="T">Entity type to export</typeparam>
public class ExcelExportService<T> : IExportService<T> where T : class
{
    private readonly ILogger<ExcelExportService<T>> _logger;

    public ExcelExportService(ILogger<ExcelExportService<T>> logger)
    {
        _logger = logger ?? throw new ArgumentNullException(nameof(logger));
    }

    /// <inheritdoc />
    public Task<byte[]> ExportToCsvAsync(
        IEnumerable<T> data,
        bool includeHeaders = true,
        CancellationToken cancellationToken = default)
    {
        throw new NotImplementedException(
            "CSV export not supported by ExcelExportService. Use CsvExportService instead.");
    }

    /// <inheritdoc />
    public async Task<byte[]> ExportToExcelAsync(
        IEnumerable<T> data,
        string sheetName = "Data",
        bool includeHeaders = true,
        CancellationToken cancellationToken = default)
    {
        _logger.LogInformation("Starting Excel export for {TypeName}", typeof(T).Name);

        var dataList = data.ToList();
        if (!dataList.Any())
        {
            _logger.LogWarning("No data to export for {TypeName}", typeof(T).Name);
            return Array.Empty<byte>();
        }

        using var workbook = new XLWorkbook();
        IXLWorksheet worksheet = workbook.Worksheets.Add(sheetName);

        // Get properties via reflection
        var properties = typeof(T).GetProperties()
            .Where(p => p.CanRead && IsExportableType(p.PropertyType))
            .ToList();

        var currentRow = 1;

        // Add headers
        if (includeHeaders)
        {
            for (var col = 0; col < properties.Count; col++)
            {
                IXLCell cell = worksheet.Cell(currentRow, col + 1);
                cell.Value = properties[col].Name;
                cell.Style.Font.Bold = true;
                cell.Style.Fill.BackgroundColor = XLColor.FromArgb(0, 71, 187); // Caixa blue
                cell.Style.Font.FontColor = XLColor.White;
                cell.Style.Alignment.Horizontal = XLAlignmentHorizontalValues.Center;
            }
            currentRow++;
        }

        // Add data rows
        foreach (T? item in dataList)
        {
            cancellationToken.ThrowIfCancellationRequested();

            for (var col = 0; col < properties.Count; col++)
            {
                IXLCell cell = worksheet.Cell(currentRow, col + 1);
                var value = properties[col].GetValue(item);
                SetCellValue(cell, value, properties[col].PropertyType);
            }

            currentRow++;
        }

        // Auto-fit columns
        worksheet.Columns().AdjustToContents();

        // Freeze header row
        if (includeHeaders)
        {
            worksheet.SheetView.FreezeRows(1);
        }

        // Convert to byte array
        using var stream = new MemoryStream();
        workbook.SaveAs(stream);
        var bytes = stream.ToArray();

        _logger.LogInformation("Excel export completed. Rows: {Count}, Size: {Size} bytes",
            dataList.Count, bytes.Length);

        return await Task.FromResult(bytes);
    }

    /// <inheritdoc />
    public Task<byte[]> ExportToPdfAsync(
        IEnumerable<T> data,
        string title = "Data Export",
        CancellationToken cancellationToken = default)
    {
        throw new NotImplementedException(
            "PDF export not supported by ExcelExportService. Use PdfExportService instead.");
    }

    /// <inheritdoc />
    public Task<Stream> StreamToCsvAsync(
        IAsyncEnumerable<T> data,
        bool includeHeaders = true,
        CancellationToken cancellationToken = default)
    {
        throw new NotImplementedException(
            "CSV streaming not supported by ExcelExportService. Use CsvExportService instead.");
    }

    /// <summary>
    /// Sets cell value with appropriate formatting based on type.
    /// </summary>
    private void SetCellValue(IXLCell cell, object? value, Type propertyType)
    {
        if (value == null)
        {
            cell.Value = "";
            return;
        }

        // Handle different types with appropriate formatting
        if (propertyType == typeof(DateTime) || propertyType == typeof(DateTime?))
        {
            cell.Value = (DateTime)value;
            cell.Style.DateFormat.Format = "dd/MM/yyyy";
        }
        else if (propertyType == typeof(decimal) || propertyType == typeof(decimal?))
        {
            cell.Value = (decimal)value;
            cell.Style.NumberFormat.Format = "#,##0.00";
        }
        else if (propertyType == typeof(double) || propertyType == typeof(double?))
        {
            cell.Value = (double)value;
            cell.Style.NumberFormat.Format = "#,##0.00";
        }
        else if (propertyType == typeof(int) || propertyType == typeof(int?) ||
                 propertyType == typeof(long) || propertyType == typeof(long?))
        {
            cell.Value = Convert.ToInt64(value);
            cell.Style.NumberFormat.Format = "#,##0";
        }
        else if (propertyType == typeof(bool) || propertyType == typeof(bool?))
        {
            cell.Value = (bool)value ? "Sim" : "Não";
        }
        else
        {
            cell.Value = value.ToString() ?? "";
        }
    }

    /// <summary>
    /// Checks if a type is exportable to Excel.
    /// </summary>
    private bool IsExportableType(Type type)
    {
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
/// Specialized Excel export service for premium records with financial formatting.
/// </summary>
public class PremiumExcelExportService
{
    private readonly ILogger<PremiumExcelExportService> _logger;

    public PremiumExcelExportService(ILogger<PremiumExcelExportService> logger)
    {
        _logger = logger ?? throw new ArgumentNullException(nameof(logger));
    }

    /// <summary>
    /// Exports premium records to Excel with formatting, formulas, and totals.
    /// </summary>
    public async Task<byte[]> ExportPremiumsToExcelAsync(
        IEnumerable<object> data,
        string sheetName = "Prêmios",
        bool includeTotals = true,
        CancellationToken cancellationToken = default)
    {
        _logger.LogInformation("Starting premium Excel export");

        var dataList = data.ToList();
        if (!dataList.Any())
        {
            _logger.LogWarning("No premium data to export");
            return Array.Empty<byte>();
        }

        using var workbook = new XLWorkbook();
        IXLWorksheet worksheet = workbook.Worksheets.Add(sheetName);

        // Define columns
        (string, string)[] columns = new[]
        {
            ("PolicyNumber", "Número da Apólice"),
            ("EndorsementNumber", "Número do Endosso"),
            ("ReferenceDate", "Data de Referência"),
            ("ProductCode", "Código do Produto"),
            ("LineOfBusiness", "Ramo"),
            ("ClientCode", "Código do Cliente"),
            ("TotalPremiumNet", "Prêmio Total Líquido"),
            ("MovementType", "Tipo de Movimento"),
            ("CompanyCode", "Código da Empresa")
        };

        var currentRow = 1;

        // Add headers
        for (var col = 0; col < columns.Length; col++)
        {
            IXLCell cell = worksheet.Cell(currentRow, col + 1);
            cell.Value = columns[col].Item2; // Portuguese label
            cell.Style.Font.Bold = true;
            cell.Style.Fill.BackgroundColor = XLColor.FromArgb(0, 71, 187); // Caixa blue
            cell.Style.Font.FontColor = XLColor.White;
            cell.Style.Alignment.Horizontal = XLAlignmentHorizontalValues.Center;
        }
        currentRow++;

        // Add data rows
        var firstDataRow = currentRow;
        foreach (var item in dataList)
        {
            cancellationToken.ThrowIfCancellationRequested();

            Type type = item.GetType();

            for (var col = 0; col < columns.Length; col++)
            {
                System.Reflection.PropertyInfo? property = type.GetProperty(columns[col].Item1);
                if (property == null)
                {
                    continue;
                }

                IXLCell cell = worksheet.Cell(currentRow, col + 1);
                var value = property.GetValue(item);

                SetPremiumCellValue(cell, value, property.PropertyType, columns[col].Item1);
            }

            currentRow++;
        }

        var lastDataRow = currentRow - 1;

        // Add totals row
        if (includeTotals && lastDataRow >= firstDataRow)
        {
            currentRow++; // Blank row before totals

            var totalRow = currentRow;
            IXLCell totalCell = worksheet.Cell(totalRow, 1);
            totalCell.Value = "TOTAL";
            totalCell.Style.Font.Bold = true;
            totalCell.Style.Fill.BackgroundColor = XLColor.LightGray;

            // Sum for TotalPremiumNet column (column 7)
            IXLCell sumCell = worksheet.Cell(totalRow, 7);
            sumCell.FormulaA1 = $"SUM(G{firstDataRow}:G{lastDataRow})";
            sumCell.Style.Font.Bold = true;
            sumCell.Style.Fill.BackgroundColor = XLColor.LightGray;
            sumCell.Style.NumberFormat.Format = "R$ #,##0.00";
        }

        // Auto-fit columns
        worksheet.Columns().AdjustToContents();

        // Freeze header row
        worksheet.SheetView.FreezeRows(1);

        // Add alternating row colors
        for (var row = firstDataRow; row <= lastDataRow; row++)
        {
            if ((row - firstDataRow) % 2 == 1)
            {
                worksheet.Row(row).Style.Fill.BackgroundColor = XLColor.FromArgb(240, 240, 240);
            }
        }

        // Convert to byte array
        using var stream = new MemoryStream();
        workbook.SaveAs(stream);
        var bytes = stream.ToArray();

        _logger.LogInformation("Premium Excel export completed. Rows: {Count}, Size: {Size} bytes",
            dataList.Count, bytes.Length);

        return await Task.FromResult(bytes);
    }

    private void SetPremiumCellValue(IXLCell cell, object? value, Type propertyType, string propertyName)
    {
        if (value == null)
        {
            cell.Value = "";
            return;
        }

        // Financial columns with currency formatting
        if (propertyName.Contains("Premium") || propertyName.Contains("Amount"))
        {
            if (propertyType == typeof(decimal) || propertyType == typeof(decimal?))
            {
                cell.Value = (decimal)value;
                cell.Style.NumberFormat.Format = "R$ #,##0.00";
            }
        }
        // Date columns
        else if (propertyType == typeof(DateTime) || propertyType == typeof(DateTime?))
        {
            cell.Value = (DateTime)value;
            cell.Style.DateFormat.Format = "dd/MM/yyyy";
        }
        // Integer columns
        else if (propertyType == typeof(int) || propertyType == typeof(int?) ||
                 propertyType == typeof(long) || propertyType == typeof(long?))
        {
            cell.Value = Convert.ToInt64(value);
            cell.Style.NumberFormat.Format = "#,##0";
        }
        else
        {
            cell.Value = value.ToString() ?? "";
        }
    }
}
