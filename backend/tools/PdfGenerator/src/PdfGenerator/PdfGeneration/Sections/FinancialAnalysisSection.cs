using QuestPDF.Fluent;
using QuestPDF.Infrastructure;
using QuestPDF.Helpers;
using PdfGenerator.PdfGeneration.Formatting;
using PdfGenerator.Formatting;

namespace PdfGenerator.PdfGeneration.Sections;

/// <summary>
/// Financial Analysis section with cost breakdown and ROI
/// </summary>
public class FinancialAnalysisSection : IPdfSection
{

    public string SectionId => "financial-analysis";
    public string Title => "Análise Financeira";
    public int Order => 7;
    public bool IncludeInToc => true;

    public void Render(IContainer container, SectionContext context)
    {
        container.Column(column =>
        {
            column.Item().PageBreak();

            // Section title
            column.Item()
                .Text("6. ANÁLISE FINANCEIRA")
                .FontColor(BrandingStyles.PrimaryBlue)
                .Bold()
                .FontSize(18);

            column.Item().Height(10, Unit.Millimetre);

            // Cost summary
            column.Item()
                .Text("6.1 Resumo de Investimento")
                .FontColor(BrandingStyles.TextDark)
                .Bold()
                .FontSize(14);

            column.Item().Height(5, Unit.Millimetre);

            // Key financial metrics
            column.Item().Row(row =>
            {
                row.RelativeItem().Element(c => RenderFinancialCard(c,
                    "Investimento Total",
                    _formatter.FormatCurrency(context.Financial.TotalCost),
                    "12 semanas",
                    BrandingStyles.PrimaryBlue));

                row.ConstantItem(10, Unit.Point);

                row.RelativeItem().Element(c => RenderFinancialCard(c,
                    "Custo por PF",
                    _formatter.FormatCurrency(context.Financial.CostPerFunctionPoint),
                    "Mercado: R$ 800",
                    BrandingStyles.AccentYellow));

                row.ConstantItem(10, Unit.Point);

                row.RelativeItem().Element(c => RenderFinancialCard(c,
                    "ROI Esperado",
                    $"{context.Financial.EstimatedROI:P0}",
                    "18 meses",
                    Colors.Green.Medium));
            });

            column.Item().Height(10, Unit.Millimetre);

            // Cost breakdown
            column.Item()
                .Text("6.2 Distribuição de Custos")
                .FontColor(BrandingStyles.TextDark)
                .Bold()
                .FontSize(14);

            column.Item().Height(5, Unit.Millimetre);

            column.Item().Table(table =>
            {
                table.ColumnsDefinition(columns =>
                {
                    columns.RelativeColumn(3);
                    columns.ConstantColumn(100, Unit.Point);
                    columns.ConstantColumn(80, Unit.Point);
                    columns.RelativeColumn(2);
                });

                table.Header(header =>
                {
                    header.Cell().Background(BrandingStyles.PrimaryBlue).Padding(5)
                        .Text("Categoria").FontColor(Colors.White).Bold().FontSize(10);
                    header.Cell().Background(BrandingStyles.PrimaryBlue).Padding(5)
                        .Text("Valor").FontColor(Colors.White).Bold().FontSize(10);
                    header.Cell().Background(BrandingStyles.PrimaryBlue).Padding(5)
                        .Text("% Total").FontColor(Colors.White).Bold().FontSize(10);
                    header.Cell().Background(BrandingStyles.PrimaryBlue).Padding(5)
                        .Text("Observações").FontColor(Colors.White).Bold().FontSize(10);
                });

                foreach (var item in context.Financial.CostBreakdown)
                {
                    var percentage = (item.Amount / context.Financial.TotalCost) * 100;

                    table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(5)
                        .Text(item.Category).FontColor(BrandingStyles.TextDark).FontSize(10);
                    table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(5)
                        .AlignRight().Text(_formatter.FormatCurrency(item.Amount))
                        .FontColor(BrandingStyles.TextMedium).FontSize(10);
                    table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(5)
                        .AlignRight().Text($"{percentage:F1}%")
                        .FontColor(GetPercentageColor(percentage)).Bold().FontSize(10);
                    table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(5)
                        .Text(item.Description).FontColor(BrandingStyles.TextLight).FontSize(9);
                }

                // Total row
                table.Cell().Background(Colors.Grey.Lighten4).Padding(5)
                    .Text("TOTAL").FontColor(BrandingStyles.TextDark).Bold().FontSize(11);
                table.Cell().Background(Colors.Grey.Lighten4).Padding(5)
                    .AlignRight().Text(_formatter.FormatCurrency(context.Financial.TotalCost))
                    .FontColor(BrandingStyles.PrimaryBlue).Bold().FontSize(11);
                table.Cell().Background(Colors.Grey.Lighten4).Padding(5)
                    .AlignRight().Text("100%")
                    .FontColor(BrandingStyles.TextDark).Bold().FontSize(11);
                table.Cell().Background(Colors.Grey.Lighten4).Padding(5)
                    .Text("").FontColor(BrandingStyles.TextDark).FontSize(11);
            });

            column.Item().Height(10, Unit.Millimetre);

            // Payment milestones
            column.Item()
                .Text("6.3 Cronograma de Pagamentos")
                .FontColor(BrandingStyles.TextDark)
                .Bold()
                .FontSize(14);

            column.Item().Height(5, Unit.Millimetre);

            column.Item().Table(table =>
            {
                table.ColumnsDefinition(columns =>
                {
                    columns.ConstantColumn(40, Unit.Point);
                    columns.RelativeColumn(3);
                    columns.ConstantColumn(100, Unit.Point);
                    columns.ConstantColumn(80, Unit.Point);
                    columns.RelativeColumn(2);
                });

                table.Header(header =>
                {
                    header.Cell().Background(BrandingStyles.AccentYellow).Padding(5)
                        .Text("#").FontColor(BrandingStyles.TextDark).Bold().FontSize(10);
                    header.Cell().Background(BrandingStyles.AccentYellow).Padding(5)
                        .Text("Marco").FontColor(BrandingStyles.TextDark).Bold().FontSize(10);
                    header.Cell().Background(BrandingStyles.AccentYellow).Padding(5)
                        .Text("Valor").FontColor(BrandingStyles.TextDark).Bold().FontSize(10);
                    header.Cell().Background(BrandingStyles.AccentYellow).Padding(5)
                        .Text("Semana").FontColor(BrandingStyles.TextDark).Bold().FontSize(10);
                    header.Cell().Background(BrandingStyles.AccentYellow).Padding(5)
                        .Text("Entregáveis").FontColor(BrandingStyles.TextDark).Bold().FontSize(10);
                });

                var milestoneNumber = 1;
                foreach (var milestone in context.Financial.PaymentMilestones)
                {
                    table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(5)
                        .AlignCenter().Text(milestoneNumber.ToString())
                        .FontColor(BrandingStyles.TextDark).Bold().FontSize(10);
                    table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(5)
                        .Text(milestone.Name).FontColor(BrandingStyles.TextDark).FontSize(10);
                    table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(5)
                        .AlignRight().Text(_formatter.FormatCurrency(milestone.Amount))
                        .FontColor(BrandingStyles.TextMedium).FontSize(10);
                    table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(5)
                        .AlignCenter().Text($"Semana {milestone.WeekNumber}")
                        .FontColor(BrandingStyles.TextMedium).FontSize(10);
                    table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(5)
                        .Text(milestone.Deliverables).FontColor(BrandingStyles.TextLight).FontSize(9);

                    milestoneNumber++;
                }
            });

            column.Item().Height(10, Unit.Millimetre);

            // ROI Analysis
            column.Item()
                .Text("6.4 Análise de Retorno (ROI)")
                .FontColor(BrandingStyles.TextDark)
                .Bold()
                .FontSize(14);

            column.Item().Height(5, Unit.Millimetre);

            column.Item()
                .Background(Colors.Green.Lighten5)
                .Padding(10)
                .Column(roiCol =>
                {
                    roiCol.Item()
                        .Text("Economia Anual Projetada")
                        .FontColor(Colors.Green.Darken2)
                        .Bold()
                        .FontSize(11);

                    roiCol.Item().Height(5, Unit.Millimetre);

                    var savings = new[]
                    {
                        ("Redução de custos com mainframe (MIPS)", 1_200_000m),
                        ("Eliminação de licenças COBOL/DB2", 480_000m),
                        ("Redução de equipe especializada", 360_000m),
                        ("Aumento de produtividade (50%)", 240_000m),
                        ("Menor tempo de manutenção", 120_000m)
                    };

                    var totalSavings = savings.Sum(s => s.Item2);

                    foreach (var (description, amount) in savings)
                    {
                        roiCol.Item().Row(row =>
                        {
                            row.RelativeItem()
                                .Text($"• {description}")
                                .FontColor(BrandingStyles.TextMedium)
                                .FontSize(10);

                            row.ConstantItem(120, Unit.Point)
                                .AlignRight()
                                .Text(_formatter.FormatCurrency(amount))
                                .FontColor(Colors.Green.Darken1)
                                .Bold()
                                .FontSize(10);
                        });

                        roiCol.Item().Height(3, Unit.Millimetre);
                    }

                    roiCol.Item()
                        .BorderTop(1)
                        .BorderColor(Colors.Green.Medium)
                        .PaddingTop(5);

                    roiCol.Item().Row(row =>
                    {
                        row.RelativeItem()
                            .Text("Total de Economia Anual")
                            .FontColor(Colors.Green.Darken2)
                            .Bold()
                            .FontSize(11);

                        row.ConstantItem(120, Unit.Point)
                            .AlignRight()
                            .Text(_formatter.FormatCurrency(totalSavings))
                            .FontColor(Colors.Green.Darken2)
                            .Bold()
                            .FontSize(12);
                    });

                    roiCol.Item().Height(5, Unit.Millimetre);

                    var paybackMonths = (context.Financial.TotalCost / totalSavings) * 12;
                    roiCol.Item()
                        .Text($"Período de Payback: {paybackMonths:F1} meses")
                        .FontColor(BrandingStyles.PrimaryBlue)
                        .Bold()
                        .FontSize(11);
                });

            column.Item().Height(10, Unit.Millimetre);

            // Risk considerations
            column.Item()
                .Background(Colors.Orange.Lighten5)
                .Padding(10)
                .Column(riskCol =>
                {
                    riskCol.Item()
                        .Text("Considerações de Risco")
                        .FontColor(Colors.Orange.Darken2)
                        .Bold()
                        .FontSize(11);

                    riskCol.Item().Height(3, Unit.Millimetre);

                    riskCol.Item()
                        .Text("• Contingência de 15% incluída no orçamento\n" +
                              "• Pagamentos vinculados a entregáveis aprovados\n" +
                              "• Garantia de 6 meses após go-live\n" +
                              "• Suporte paralelo durante transição")
                        .FontColor(BrandingStyles.TextMedium)
                        .FontSize(10)
                        .LineHeight(1.4f);
                });
        });
    }

    private void RenderFinancialCard(IContainer container, string title, string value, string subtitle, string color)
    {
        container
            .Border(2)
            .BorderColor(color)
            .Padding(10)
            .Column(col =>
            {
                col.Item()
                    .Text(title)
                    .FontColor(BrandingStyles.TextMedium)
                    .FontSize(10);

                col.Item().Height(3, Unit.Millimetre);

                col.Item()
                    .Text(value)
                    .FontColor(color)
                    .Bold()
                    .FontSize(16);

                col.Item().Height(2, Unit.Millimetre);

                col.Item()
                    .Text(subtitle)
                    .FontColor(BrandingStyles.TextLight)
                    .Italic()
                    .FontSize(9);
            });
    }

    private string GetPercentageColor(decimal percentage)
    {
        return percentage switch
        {
            > 30 => Colors.Red.Medium,
            > 20 => Colors.Orange.Medium,
            > 10 => BrandingStyles.AccentYellow,
            _ => Colors.Green.Medium
        };
    }
}