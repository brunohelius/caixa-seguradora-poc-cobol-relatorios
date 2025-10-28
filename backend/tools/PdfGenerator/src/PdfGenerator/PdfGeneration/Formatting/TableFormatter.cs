using System;
using System.Collections.Generic;
using System.Linq;
using QuestPDF.Fluent;
using QuestPDF.Helpers;
using QuestPDF.Infrastructure;

namespace PdfGenerator.PdfGeneration.Formatting
{
    /// <summary>
    /// Formats tables for PDF generation with Caixa Seguradora branding.
    /// </summary>
    public class TableFormatter
    {
        private readonly BrandingStyles _brandingStyles;

        public TableFormatter()
        {
            _brandingStyles = new BrandingStyles();
        }

        /// <summary>
        /// Creates a formatted table with headers and data.
        /// </summary>
        public void CreateTable(IContainer container, TableData tableData)
        {
            container.Table(table =>
            {
                // Define columns
                foreach (var column in tableData.Columns)
                {
                    if (column.Width.HasValue)
                    {
                        table.ColumnsDefinition(columns =>
                        {
                            columns.ConstantColumn(column.Width.Value, Unit.Point);
                        });
                    }
                    else
                    {
                        table.ColumnsDefinition(columns =>
                        {
                            columns.RelativeColumn();
                        });
                    }
                }

                // Header
                table.Header(header =>
                {
                    foreach (var column in tableData.Columns)
                    {
                        header.Cell()
                            .Background(_brandingStyles.TableHeaderBackground)
                            .BorderBottom(1)
                            .BorderColor(_brandingStyles.TableBorderColor)
                            .Padding(5)
                            .Text(column.Title)
                            .FontSize(_brandingStyles.BodyFontSize)
                            .Bold()
                            .FontColor(_brandingStyles.TableHeaderTextColor);
                    }
                });

                // Data rows
                var rowIndex = 0;
                foreach (var row in tableData.Rows)
                {
                    var backgroundColor = _brandingStyles.GetRowBackground(rowIndex);
                    var columnIndex = 0;

                    foreach (var cell in row.Cells)
                    {
                        var column = tableData.Columns[columnIndex];

                        table.Cell()
                            .Background(backgroundColor)
                            .BorderBottom(0.5f)
                            .BorderColor(_brandingStyles.TableBorderColor)
                            .Padding(5)
                            .Element(c => FormatCell(c, cell, column));

                        columnIndex++;
                    }

                    rowIndex++;
                }

                // Footer (if provided)
                if (tableData.Footer != null && tableData.Footer.Any())
                {
                    foreach (var footerCell in tableData.Footer)
                    {
                        table.Cell()
                            .Background(_brandingStyles.HeaderBackground)
                            .BorderTop(1)
                            .BorderColor(_brandingStyles.TableBorderColor)
                            .Padding(5)
                            .Text(footerCell)
                            .FontSize(_brandingStyles.SmallFontSize)
                            .Bold()
                            .FontColor(_brandingStyles.DarkBlue);
                    }
                }
            });
        }

        /// <summary>
        /// Creates a simple key-value table.
        /// </summary>
        public void CreateKeyValueTable(IContainer container, Dictionary<string, string> data, string title = null)
        {
            container.Column(column =>
            {
                // Title if provided
                if (!string.IsNullOrEmpty(title))
                {
                    column.Item()
                        .PaddingBottom(10)
                        .Text(title)
                        .FontSize(_brandingStyles.SubheadingFontSize)
                        .Bold()
                        .FontColor(_brandingStyles.PrimaryColor);
                }

                // Table
                column.Item().Table(table =>
                {
                    table.ColumnsDefinition(columns =>
                    {
                        columns.RelativeColumn(2); // Key column
                        columns.RelativeColumn(3); // Value column
                    });

                    var rowIndex = 0;
                    foreach (var kvp in data)
                    {
                        var backgroundColor = _brandingStyles.GetRowBackground(rowIndex);

                        // Key cell
                        table.Cell()
                            .Background(backgroundColor)
                            .BorderBottom(0.5f)
                            .BorderColor(_brandingStyles.TableBorderColor)
                            .Padding(8)
                            .Text(kvp.Key)
                            .FontSize(_brandingStyles.BodyFontSize)
                            .Bold()
                            .FontColor(_brandingStyles.DarkBlue);

                        // Value cell
                        table.Cell()
                            .Background(backgroundColor)
                            .BorderBottom(0.5f)
                            .BorderColor(_brandingStyles.TableBorderColor)
                            .Padding(8)
                            .Text(kvp.Value)
                            .FontSize(_brandingStyles.BodyFontSize)
                            .FontColor(_brandingStyles.TextColor);

                        rowIndex++;
                    }
                });
            });
        }

        /// <summary>
        /// Creates a comparison table (before/after, COBOL/.NET, etc.).
        /// </summary>
        public void CreateComparisonTable(IContainer container, ComparisonTableData data)
        {
            container.Column(column =>
            {
                // Title
                if (!string.IsNullOrEmpty(data.Title))
                {
                    column.Item()
                        .PaddingBottom(10)
                        .Text(data.Title)
                        .FontSize(_brandingStyles.SubheadingFontSize)
                        .Bold()
                        .FontColor(_brandingStyles.PrimaryColor);
                }

                // Table
                column.Item().Table(table =>
                {
                    table.ColumnsDefinition(columns =>
                    {
                        columns.RelativeColumn(2); // Category
                        columns.RelativeColumn(3); // Before
                        columns.RelativeColumn(3); // After
                        columns.RelativeColumn(2); // Difference
                    });

                    // Header
                    table.Header(header =>
                    {
                        header.Cell()
                            .Background(_brandingStyles.TableHeaderBackground)
                            .Padding(5)
                            .Text(data.CategoryHeader)
                            .FontSize(_brandingStyles.BodyFontSize)
                            .Bold()
                            .FontColor(_brandingStyles.TableHeaderTextColor);

                        header.Cell()
                            .Background(_brandingStyles.TableHeaderBackground)
                            .Padding(5)
                            .Text(data.BeforeHeader)
                            .FontSize(_brandingStyles.BodyFontSize)
                            .Bold()
                            .FontColor(_brandingStyles.TableHeaderTextColor);

                        header.Cell()
                            .Background(_brandingStyles.TableHeaderBackground)
                            .Padding(5)
                            .Text(data.AfterHeader)
                            .FontSize(_brandingStyles.BodyFontSize)
                            .Bold()
                            .FontColor(_brandingStyles.TableHeaderTextColor);

                        header.Cell()
                            .Background(_brandingStyles.TableHeaderBackground)
                            .Padding(5)
                            .Text(data.DifferenceHeader)
                            .FontSize(_brandingStyles.BodyFontSize)
                            .Bold()
                            .FontColor(_brandingStyles.TableHeaderTextColor);
                    });

                    // Data rows
                    var rowIndex = 0;
                    foreach (var row in data.Rows)
                    {
                        var backgroundColor = _brandingStyles.GetRowBackground(rowIndex);
                        var differenceColor = GetDifferenceColor(row.DifferenceType);

                        table.Cell()
                            .Background(backgroundColor)
                            .BorderBottom(0.5f)
                            .BorderColor(_brandingStyles.TableBorderColor)
                            .Padding(5)
                            .Text(row.Category)
                            .FontSize(_brandingStyles.BodyFontSize)
                            .Bold()
                            .FontColor(_brandingStyles.DarkBlue);

                        table.Cell()
                            .Background(backgroundColor)
                            .BorderBottom(0.5f)
                            .BorderColor(_brandingStyles.TableBorderColor)
                            .Padding(5)
                            .Text(row.BeforeValue)
                            .FontSize(_brandingStyles.BodyFontSize)
                            .FontColor(_brandingStyles.TextColor);

                        table.Cell()
                            .Background(backgroundColor)
                            .BorderBottom(0.5f)
                            .BorderColor(_brandingStyles.TableBorderColor)
                            .Padding(5)
                            .Text(row.AfterValue)
                            .FontSize(_brandingStyles.BodyFontSize)
                            .FontColor(_brandingStyles.TextColor);

                        table.Cell()
                            .Background(backgroundColor)
                            .BorderBottom(0.5f)
                            .BorderColor(_brandingStyles.TableBorderColor)
                            .Padding(5)
                            .Text(row.Difference)
                            .FontSize(_brandingStyles.BodyFontSize)
                            .Bold()
                            .FontColor(differenceColor);

                        rowIndex++;
                    }
                });
            });
        }

        /// <summary>
        /// Format individual cell based on column configuration.
        /// </summary>
        private void FormatCell(IContainer container, TableCell cell, TableColumn column)
        {
            var text = container.Text(cell.Value);

            // Apply alignment
            switch (column.Alignment)
            {
                case TextAlignment.Center:
                    text.AlignCenter();
                    break;
                case TextAlignment.Right:
                    text.AlignRight();
                    break;
                default:
                    text.AlignLeft();
                    break;
            }

            // Apply formatting
            text.FontSize(_brandingStyles.BodyFontSize);

            if (cell.IsBold)
                text.Bold();

            if (!string.IsNullOrEmpty(cell.Color))
                text.FontColor(cell.Color);
            else
                text.FontColor(_brandingStyles.TextColor);

            // Apply data type formatting
            if (column.DataType == ColumnDataType.Currency && decimal.TryParse(cell.Value, out var amount))
            {
                text.Text($"R$ {amount:N2}");
            }
            else if (column.DataType == ColumnDataType.Percentage && decimal.TryParse(cell.Value, out var percentage))
            {
                text.Text($"{percentage:N1}%");
                var color = _brandingStyles.GetStatusColor(percentage);
                text.FontColor(color);
            }
        }

        /// <summary>
        /// Get color for difference value based on type.
        /// </summary>
        private string GetDifferenceColor(DifferenceType type)
        {
            return type switch
            {
                DifferenceType.Improvement => _brandingStyles.SuccessColor,
                DifferenceType.Degradation => _brandingStyles.ErrorColor,
                DifferenceType.Neutral => _brandingStyles.InfoColor,
                _ => _brandingStyles.TextColor
            };
        }
    }

    /// <summary>
    /// Represents table data for rendering.
    /// </summary>
    public class TableData
    {
        public List<TableColumn> Columns { get; set; } = new();
        public List<TableRow> Rows { get; set; } = new();
        public List<string> Footer { get; set; }
    }

    /// <summary>
    /// Represents a table column.
    /// </summary>
    public class TableColumn
    {
        public string Title { get; set; }
        public float? Width { get; set; }
        public TextAlignment Alignment { get; set; } = TextAlignment.Left;
        public ColumnDataType DataType { get; set; } = ColumnDataType.Text;
    }

    /// <summary>
    /// Represents a table row.
    /// </summary>
    public class TableRow
    {
        public List<TableCell> Cells { get; set; } = new();
    }

    /// <summary>
    /// Represents a table cell.
    /// </summary>
    public class TableCell
    {
        public string Value { get; set; }
        public bool IsBold { get; set; }
        public string Color { get; set; }
    }

    /// <summary>
    /// Column data types for formatting.
    /// </summary>
    public enum ColumnDataType
    {
        Text,
        Number,
        Currency,
        Percentage,
        Date
    }

    /// <summary>
    /// Text alignment options.
    /// </summary>
    public enum TextAlignment
    {
        Left,
        Center,
        Right
    }

    /// <summary>
    /// Comparison table data.
    /// </summary>
    public class ComparisonTableData
    {
        public string Title { get; set; }
        public string CategoryHeader { get; set; } = "Categoria";
        public string BeforeHeader { get; set; } = "COBOL";
        public string AfterHeader { get; set; } = ".NET";
        public string DifferenceHeader { get; set; } = "Diferença";
        public List<ComparisonRow> Rows { get; set; } = new();
    }

    /// <summary>
    /// Comparison table row.
    /// </summary>
    public class ComparisonRow
    {
        public string Category { get; set; }
        public string BeforeValue { get; set; }
        public string AfterValue { get; set; }
        public string Difference { get; set; }
        public DifferenceType DifferenceType { get; set; } = DifferenceType.Neutral;
    }

    /// <summary>
    /// Type of difference for color coding.
    /// </summary>
    public enum DifferenceType
    {
        Improvement,
        Degradation,
        Neutral
    }
}