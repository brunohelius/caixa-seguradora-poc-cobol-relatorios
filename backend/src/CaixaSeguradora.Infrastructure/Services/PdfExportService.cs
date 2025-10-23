using System.Globalization;
using QuestPDF.Fluent;
using QuestPDF.Helpers;
using QuestPDF.Infrastructure;
using CaixaSeguradora.Core.Interfaces;
using Microsoft.Extensions.Logging;

namespace CaixaSeguradora.Infrastructure.Services;

/// <summary>
/// PDF export service implementation using QuestPDF.
/// Generates professional PDF documents with tables, formatting, and branding.
/// </summary>
/// <typeparam name="T">Entity type to export</typeparam>
public class PdfExportService<T> : IExportService<T> where T : class
{
    private readonly ILogger<PdfExportService<T>> _logger;

    // Caixa Seguradora brand colors
    private static readonly string CaixaBlue = "#0047BB";
    private static readonly string CaixaYellow = "#FFB81C";

    public PdfExportService(ILogger<PdfExportService<T>> logger)
    {
        _logger = logger ?? throw new ArgumentNullException(nameof(logger));

        // Configure QuestPDF license for community/open-source use
        QuestPDF.Settings.License = LicenseType.Community;
    }

    /// <inheritdoc />
    public Task<byte[]> ExportToCsvAsync(
        IEnumerable<T> data,
        bool includeHeaders = true,
        CancellationToken cancellationToken = default)
    {
        throw new NotImplementedException(
            "CSV export not supported by PdfExportService. Use CsvExportService instead.");
    }

    /// <inheritdoc />
    public Task<byte[]> ExportToExcelAsync(
        IEnumerable<T> data,
        string sheetName = "Data",
        bool includeHeaders = true,
        CancellationToken cancellationToken = default)
    {
        throw new NotImplementedException(
            "Excel export not supported by PdfExportService. Use ExcelExportService instead.");
    }

    /// <inheritdoc />
    public async Task<byte[]> ExportToPdfAsync(
        IEnumerable<T> data,
        string title = "Data Export",
        CancellationToken cancellationToken = default)
    {
        _logger.LogInformation("Starting PDF export for {TypeName}", typeof(T).Name);

        var dataList = data.ToList();
        if (!dataList.Any())
        {
            _logger.LogWarning("No data to export for {TypeName}", typeof(T).Name);
            return Array.Empty<byte>();
        }

        // Get properties via reflection
        var properties = typeof(T).GetProperties()
            .Where(p => p.CanRead && IsExportableType(p.PropertyType))
            .Take(10) // Limit columns for PDF width
            .ToList();

        var document = Document.Create(container =>
        {
            container.Page(page =>
            {
                page.Size(PageSizes.A4.Landscape());
                page.Margin(1.5f, Unit.Centimetre);

                // Header
                page.Header().Element(c => ComposeHeader(c, title));

                // Content
                page.Content().PaddingVertical(0.5f, Unit.Centimetre).Element(c =>
                {
                    ComposeTable(c, dataList, properties);
                });

                // Footer
                page.Footer().Element(ComposeFooter);
            });
        });

        var pdfBytes = document.GeneratePdf();

        _logger.LogInformation("PDF export completed. Rows: {Count}, Size: {Size} bytes",
            dataList.Count, pdfBytes.Length);

        return await Task.FromResult(pdfBytes);
    }

    /// <inheritdoc />
    public Task<Stream> StreamToCsvAsync(
        IAsyncEnumerable<T> data,
        bool includeHeaders = true,
        CancellationToken cancellationToken = default)
    {
        throw new NotImplementedException(
            "CSV streaming not supported by PdfExportService. Use CsvExportService instead.");
    }

    private void ComposeHeader(IContainer container, string title)
    {
        container.Row(row =>
        {
            row.RelativeItem().Column(column =>
            {
                column.Item().Text(title)
                    .FontSize(20)
                    .SemiBold()
                    .FontColor(CaixaBlue);

                column.Item().Text($"Gerado em: {DateTime.Now:dd/MM/yyyy HH:mm:ss}")
                    .FontSize(10)
                    .FontColor(Colors.Grey.Darken2);
            });

            row.ConstantItem(100).AlignRight().Text("Caixa Seguradora")
                .FontSize(12)
                .SemiBold()
                .FontColor(CaixaBlue);
        });
    }

    private void ComposeTable(IContainer container, List<T> data, List<System.Reflection.PropertyInfo> properties)
    {
        container.Table(table =>
        {
            // Define columns
            table.ColumnsDefinition(columns =>
            {
                foreach (System.Reflection.PropertyInfo _ in properties)
                {
                    columns.RelativeColumn();
                }
            });

            // Header row
            table.Header(header =>
            {
                foreach (System.Reflection.PropertyInfo property in properties)
                {
                    header.Cell().Element(CellStyle).Background(CaixaBlue).Text(property.Name)
                        .FontColor(Colors.White).FontSize(9).SemiBold();
                }
            });

            // Data rows
            foreach (T item in data)
            {
                foreach (System.Reflection.PropertyInfo property in properties)
                {
                    var value = property.GetValue(item);
                    var formattedValue = FormatValue(value, property.PropertyType);

                    table.Cell().Element(CellStyle).Text(formattedValue)
                        .FontSize(8);
                }
            }
        });
    }

    private void ComposeFooter(IContainer container)
    {
        container.AlignCenter().DefaultTextStyle(x => x.FontSize(8).FontColor(Colors.Grey.Medium)).Text(text =>
        {
            text.Span("Página ");
            text.CurrentPageNumber();
            text.Span(" de ");
            text.TotalPages();
            text.Span(" - Gerado automaticamente por Caixa Seguradora Premium Reporting System");
        });
    }

    private IContainer CellStyle(IContainer container)
    {
        return container
            .Border(0.5f)
            .BorderColor(Colors.Grey.Lighten2)
            .Padding(5)
            .AlignLeft()
            .AlignMiddle();
    }

    private string FormatValue(object? value, Type propertyType)
    {
        if (value == null)
        {
            return "";
        }

        return value switch
        {
            DateTime dt => dt.ToString("dd/MM/yyyy", CultureInfo.InvariantCulture),
            decimal d => d.ToString("N2", new CultureInfo("pt-BR")),
            double dbl => dbl.ToString("N2", new CultureInfo("pt-BR")),
            bool b => b ? "Sim" : "Não",
            _ => value.ToString() ?? ""
        };
    }

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
/// Specialized PDF export service for premium records with charts and professional formatting.
/// </summary>
public class PremiumPdfExportService
{
    private readonly ILogger<PremiumPdfExportService> _logger;
    private static readonly string CaixaBlue = "#0047BB";
    private static readonly string CaixaYellow = "#FFB81C";

    public PremiumPdfExportService(ILogger<PremiumPdfExportService> logger)
    {
        _logger = logger ?? throw new ArgumentNullException(nameof(logger));
        QuestPDF.Settings.License = LicenseType.Community;
    }

    /// <summary>
    /// Exports premium records to PDF with professional formatting and statistics.
    /// </summary>
    public async Task<byte[]> ExportPremiumsToPdfAsync(
        IEnumerable<object> data,
        string title = "Relatório de Prêmios",
        bool includeCharts = false,
        CancellationToken cancellationToken = default)
    {
        _logger.LogInformation("Starting premium PDF export. Include charts: {IncludeCharts}", includeCharts);

        var dataList = data.ToList();
        if (!dataList.Any())
        {
            _logger.LogWarning("No premium data to export");
            return Array.Empty<byte>();
        }

        var document = Document.Create(container =>
        {
            container.Page(page =>
            {
                page.Size(PageSizes.A4.Landscape());
                page.Margin(1.5f, Unit.Centimetre);

                // Header
                page.Header().Element(c => ComposeHeader(c, title, dataList.Count));

                // Content
                page.Content().PaddingVertical(0.5f, Unit.Centimetre).Column(column =>
                {
                    // Summary statistics
                    column.Item().Element(c => ComposeSummary(c, dataList));

                    column.Item().PaddingTop(10);

                    // Data table
                    column.Item().Element(c => ComposePremiumTable(c, dataList));
                });

                // Footer
                page.Footer().Element(ComposeFooter);
            });
        });

        var pdfBytes = document.GeneratePdf();

        _logger.LogInformation("Premium PDF export completed. Rows: {Count}, Size: {Size} bytes",
            dataList.Count, pdfBytes.Length);

        return await Task.FromResult(pdfBytes);
    }

    private void ComposeHeader(IContainer container, string title, int recordCount)
    {
        container.Column(column =>
        {
            column.Item().Row(row =>
            {
                row.RelativeItem().Column(col =>
                {
                    col.Item().Text(title)
                        .FontSize(20)
                        .SemiBold()
                        .FontColor(CaixaBlue);

                    col.Item().Text($"Total de registros: {recordCount:N0}")
                        .FontSize(12)
                        .FontColor(Colors.Grey.Darken2);
                });

                row.ConstantItem(120).AlignRight().Column(col =>
                {
                    col.Item().Text("Caixa Seguradora")
                        .FontSize(14)
                        .SemiBold()
                        .FontColor(CaixaBlue);

                    col.Item().Text($"{DateTime.Now:dd/MM/yyyy HH:mm}")
                        .FontSize(10)
                        .FontColor(Colors.Grey.Medium);
                });
            });

            column.Item().PaddingTop(10).LineHorizontal(2).LineColor(CaixaYellow);
        });
    }

    private void ComposeSummary(IContainer container, List<object> data)
    {
        // Calculate summary statistics
        Type type = data.First().GetType();
        System.Reflection.PropertyInfo? premiumProperty = type.GetProperty("TotalPremiumNet");

        if (premiumProperty != null)
        {
            var premiums = data
                .Select(d => premiumProperty.GetValue(d))
                .Where(v => v != null)
                .Cast<decimal>()
                .ToList();

            var totalPremium = premiums.Sum();
            var avgPremium = premiums.Average();
            var maxPremium = premiums.Max();
            var minPremium = premiums.Min();

            container.Background(Colors.Grey.Lighten4).Padding(10).Row(row =>
            {
                row.RelativeItem().Column(col =>
                {
                    col.Item().Text("Prêmio Total").FontSize(10).FontColor(Colors.Grey.Darken2);
                    col.Item().Text($"R$ {totalPremium:N2}").FontSize(14).SemiBold().FontColor(CaixaBlue);
                });

                row.RelativeItem().Column(col =>
                {
                    col.Item().Text("Prêmio Médio").FontSize(10).FontColor(Colors.Grey.Darken2);
                    col.Item().Text($"R$ {avgPremium:N2}").FontSize(14).SemiBold();
                });

                row.RelativeItem().Column(col =>
                {
                    col.Item().Text("Maior Prêmio").FontSize(10).FontColor(Colors.Grey.Darken2);
                    col.Item().Text($"R$ {maxPremium:N2}").FontSize(14).SemiBold();
                });

                row.RelativeItem().Column(col =>
                {
                    col.Item().Text("Menor Prêmio").FontSize(10).FontColor(Colors.Grey.Darken2);
                    col.Item().Text($"R$ {minPremium:N2}").FontSize(14).SemiBold();
                });
            });
        }
    }

    private void ComposePremiumTable(IContainer container, List<object> data)
    {
        (string, string)[] columns = new[]
        {
            ("PolicyNumber", "Apólice"),
            ("ReferenceDate", "Data Ref."),
            ("ProductCode", "Produto"),
            ("LineOfBusiness", "Ramo"),
            ("ClientCode", "Cliente"),
            ("TotalPremiumNet", "Prêmio Líquido"),
            ("MovementType", "Tipo Mov.")
        };

        container.Table(table =>
        {
            // Define columns
            table.ColumnsDefinition(columns =>
            {
                columns.RelativeColumn(1.5f); // PolicyNumber
                columns.RelativeColumn(1.2f); // ReferenceDate
                columns.RelativeColumn(1f);   // ProductCode
                columns.RelativeColumn(0.8f); // LineOfBusiness
                columns.RelativeColumn(1f);   // ClientCode
                columns.RelativeColumn(1.5f); // TotalPremiumNet
                columns.RelativeColumn(0.8f); // MovementType
            });

            // Header row
            table.Header(header =>
            {
                foreach ((string _, string label) in columns)
                {
                    header.Cell().Element(CellStyle).Background(CaixaBlue)
                        .Text(label).FontColor(Colors.White).FontSize(9).SemiBold();
                }
            });

            // Data rows with alternating colors
            var rowIndex = 0;
            foreach (var item in data)
            {
                Type type = item.GetType();
                var isAlternate = rowIndex % 2 == 1;

                foreach ((string propertyName, string _) in columns)
                {
                    System.Reflection.PropertyInfo? property = type.GetProperty(propertyName);
                    var value = property?.GetValue(item);
                    var formattedValue = FormatPremiumValue(value, property?.PropertyType, propertyName);

                    IContainer cell = table.Cell().Element(CellStyle);
                    if (isAlternate)
                    {
                        cell.Background(Colors.Grey.Lighten4);
                    }

                    cell.Text(formattedValue).FontSize(8);
                }

                rowIndex++;
            }
        });
    }

    private void ComposeFooter(IContainer container)
    {
        container.AlignCenter().DefaultTextStyle(x => x.FontSize(8).FontColor(Colors.Grey.Medium)).Text(text =>
        {
            text.Span("Página ");
            text.CurrentPageNumber();
            text.Span(" de ");
            text.TotalPages();
            text.Span(" | Caixa Seguradora - Sistema de Relatórios de Prêmios | ");
            text.Span($"Gerado em {DateTime.Now:dd/MM/yyyy HH:mm:ss}");
        });
    }

    private IContainer CellStyle(IContainer container)
    {
        return container
            .Border(0.5f)
            .BorderColor(Colors.Grey.Lighten2)
            .Padding(5)
            .AlignLeft()
            .AlignMiddle();
    }

    private string FormatPremiumValue(object? value, Type? propertyType, string propertyName)
    {
        if (value == null)
        {
            return "";
        }

        // Financial columns
        if (propertyName.Contains("Premium") || propertyName.Contains("Amount"))
        {
            if (value is decimal d)
            {
                return $"R$ {d:N2}";
            }
        }

        // Date columns
        if (propertyType == typeof(DateTime) || propertyType == typeof(DateTime?))
        {
            return ((DateTime)value).ToString("dd/MM/yyyy");
        }

        // Numbers
        if (value is int || value is long)
        {
            return ((long)value).ToString("N0");
        }

        return value.ToString() ?? "";
    }
}
