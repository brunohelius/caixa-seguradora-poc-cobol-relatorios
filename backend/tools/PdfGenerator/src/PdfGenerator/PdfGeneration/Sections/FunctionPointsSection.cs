using QuestPDF.Fluent;
using QuestPDF.Infrastructure;
using QuestPDF.Helpers;
using PdfGenerator.PdfGeneration.Formatting;

namespace PdfGenerator.PdfGeneration.Sections;

/// <summary>
/// Function Points section with IFPUG calculation tables
/// </summary>
public class FunctionPointsSection : IPdfSection
{
    public string SectionId => "function-points";
    public string Title => "Análise de Pontos de Função";
    public int Order => 6;
    public bool IncludeInToc => true;

    public void Render(IContainer container, SectionContext context)
    {
        container.Column(column =>
        {
            column.Item().PageBreak();

            // Section title
            column.Item()
                .Text("5. ANÁLISE DE PONTOS DE FUNÇÃO")
                .FontColor(BrandingStyles.PrimaryBlue)
                .Bold()
                .FontSize(18);

            column.Item().Height(10, Unit.Millimetre);

            // IFPUG methodology
            column.Item()
                .Text("5.1 Metodologia IFPUG 4.3.1")
                .FontColor(BrandingStyles.TextDark)
                .Bold()
                .FontSize(14);

            column.Item().Height(5, Unit.Millimetre);

            column.Item()
                .Text("Análise baseada no International Function Point Users Group (IFPUG) versão 4.3.1, " +
                      "considerando complexidade funcional e características técnicas do sistema.")
                .FontColor(BrandingStyles.TextMedium)
                .FontSize(11);

            column.Item().Height(8, Unit.Millimetre);

            // Summary metrics
            var totalUnadjusted = context.FunctionPoints.Sum(fp => fp.UnadjustedPoints);
            var totalAdjusted = context.FunctionPoints.Sum(fp => fp.AdjustedPoints);
            var adjustmentFactor = totalUnadjusted > 0 ? (decimal)totalAdjusted / totalUnadjusted : 1;

            column.Item().Row(row =>
            {
                row.RelativeItem().Element(c => RenderMetricCard(c,
                    "PF Não Ajustados", totalUnadjusted.ToString("N0"),
                    BrandingStyles.PrimaryBlue));

                row.ConstantItem(10, Unit.Point);

                row.RelativeItem().Element(c => RenderMetricCard(c,
                    "Fator de Ajuste", $"{adjustmentFactor:P0}",
                    BrandingStyles.AccentYellow));

                row.ConstantItem(10, Unit.Point);

                row.RelativeItem().Element(c => RenderMetricCard(c,
                    "PF Ajustados", totalAdjusted.ToString("N0"),
                    Colors.Green.Medium));
            });

            column.Item().Height(10, Unit.Millimetre);

            // Function types breakdown
            column.Item()
                .Text("5.2 Análise por Tipo de Função")
                .FontColor(BrandingStyles.TextDark)
                .Bold()
                .FontSize(14);

            column.Item().Height(5, Unit.Millimetre);

            // Function points table
            column.Item().Table(table =>
            {
                table.ColumnsDefinition(columns =>
                {
                    columns.RelativeColumn(3);
                    columns.ConstantColumn(80, Unit.Point);
                    columns.ConstantColumn(60, Unit.Point);
                    columns.ConstantColumn(60, Unit.Point);
                    columns.ConstantColumn(80, Unit.Point);
                });

                table.Header(header =>
                {
                    header.Cell().Background(BrandingStyles.PrimaryBlue).Padding(5)
                        .Text("Função").FontColor(Colors.White).Bold().FontSize(10);
                    header.Cell().Background(BrandingStyles.PrimaryBlue).Padding(5)
                        .Text("Tipo").FontColor(Colors.White).Bold().FontSize(10);
                    header.Cell().Background(BrandingStyles.PrimaryBlue).Padding(5)
                        .Text("Complexidade").FontColor(Colors.White).Bold().FontSize(10);
                    header.Cell().Background(BrandingStyles.PrimaryBlue).Padding(5)
                        .Text("PF Base").FontColor(Colors.White).Bold().FontSize(10);
                    header.Cell().Background(BrandingStyles.PrimaryBlue).Padding(5)
                        .Text("PF Ajustado").FontColor(Colors.White).Bold().FontSize(10);
                });

                foreach (var fp in context.FunctionPoints.OrderBy(f => f.Type).Take(20))
                {
                    table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(5)
                        .Text(fp.Name).FontColor(BrandingStyles.TextDark).FontSize(9);
                    table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(5)
                        .Text(GetTypeAbbreviation(fp.Type)).FontColor(GetTypeColor(fp.Type)).Bold().FontSize(9);
                    table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(5)
                        .AlignCenter().Text(fp.Complexity).FontColor(GetComplexityColor(fp.Complexity)).FontSize(9);
                    table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(5)
                        .AlignRight().Text(fp.UnadjustedPoints.ToString()).FontColor(BrandingStyles.TextMedium).FontSize(9);
                    table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(5)
                        .AlignRight().Text(fp.AdjustedPoints.ToString()).FontColor(BrandingStyles.TextDark).Bold().FontSize(9);
                }

                // Totals row
                table.Cell().Background(Colors.Grey.Lighten4).Padding(5)
                    .Text("TOTAL").FontColor(BrandingStyles.TextDark).Bold().FontSize(10);
                table.Cell().Background(Colors.Grey.Lighten4).Padding(5)
                    .Text("").FontColor(BrandingStyles.TextDark).FontSize(10);
                table.Cell().Background(Colors.Grey.Lighten4).Padding(5)
                    .Text("").FontColor(BrandingStyles.TextDark).FontSize(10);
                table.Cell().Background(Colors.Grey.Lighten4).Padding(5)
                    .AlignRight().Text(totalUnadjusted.ToString("N0"))
                    .FontColor(BrandingStyles.TextDark).Bold().FontSize(10);
                table.Cell().Background(Colors.Grey.Lighten4).Padding(5)
                    .AlignRight().Text(totalAdjusted.ToString("N0"))
                    .FontColor(BrandingStyles.PrimaryBlue).Bold().FontSize(10);
            });

            column.Item().Height(10, Unit.Millimetre);

            // Complexity distribution
            column.Item()
                .Text("5.3 Distribuição por Complexidade")
                .FontColor(BrandingStyles.TextDark)
                .Bold()
                .FontSize(14);

            column.Item().Height(5, Unit.Millimetre);

            var complexityGroups = context.FunctionPoints
                .GroupBy(fp => fp.Complexity)
                .Select(g => new { Complexity = g.Key, Count = g.Count(), Points = g.Sum(fp => fp.AdjustedPoints) })
                .OrderBy(g => GetComplexityOrder(g.Complexity));

            column.Item().Row(row =>
            {
                foreach (var group in complexityGroups)
                {
                    row.RelativeItem()
                        .Border(1)
                        .BorderColor(GetComplexityColor(group.Complexity))
                        .Padding(10)
                        .Column(complexCol =>
                        {
                            complexCol.Item()
                                .Text(group.Complexity)
                                .FontColor(GetComplexityColor(group.Complexity))
                                .Bold()
                                .FontSize(11);

                            complexCol.Item().Height(3, Unit.Millimetre);

                            complexCol.Item()
                                .Text($"{group.Count} funções")
                                .FontColor(BrandingStyles.TextMedium)
                                .FontSize(10);

                            complexCol.Item()
                                .Text($"{group.Points} PF")
                                .FontColor(BrandingStyles.TextDark)
                                .Bold()
                                .FontSize(10);
                        });

                    row.ConstantItem(10, Unit.Point);
                }
            });

            column.Item().Height(10, Unit.Millimetre);

            // Technical factors
            column.Item()
                .Text("5.4 Fatores de Ajuste Técnico")
                .FontColor(BrandingStyles.TextDark)
                .Bold()
                .FontSize(14);

            column.Item().Height(5, Unit.Millimetre);

            column.Item()
                .Background(Colors.Blue.Lighten5)
                .Padding(10)
                .Column(factorsCol =>
                {
                    var factors = new[]
                    {
                        ("Comunicação de Dados", 4),
                        ("Processamento Distribuído", 3),
                        ("Performance", 5),
                        ("Configuração Utilizada", 4),
                        ("Volume de Transações", 5),
                        ("Entrada de Dados Online", 2),
                        ("Eficiência do Usuário Final", 4),
                        ("Atualização Online", 3),
                        ("Complexidade de Processamento", 5),
                        ("Reusabilidade", 3),
                        ("Facilidade de Instalação", 4),
                        ("Facilidade Operacional", 4),
                        ("Múltiplos Locais", 2),
                        ("Facilidade de Mudanças", 4)
                    };

                    var totalInfluence = factors.Sum(f => f.Item2);
                    var vaf = 0.65m + (0.01m * totalInfluence);

                    factorsCol.Item()
                        .Text($"Total de Influência (TDI): {totalInfluence}")
                        .FontColor(BrandingStyles.TextDark)
                        .Bold()
                        .FontSize(11);

                    factorsCol.Item()
                        .Text($"Fator de Ajuste de Valor (VAF): {vaf:F2}")
                        .FontColor(BrandingStyles.PrimaryBlue)
                        .Bold()
                        .FontSize(11);

                    factorsCol.Item().Height(5, Unit.Millimetre);

                    factorsCol.Item().Row(row =>
                    {
                        var col1 = factors.Take(7);
                        var col2 = factors.Skip(7);

                        row.RelativeItem().Column(c1 =>
                        {
                            foreach (var (factor, value) in col1)
                            {
                                c1.Item().Row(r =>
                                {
                                    r.RelativeItem().Text(factor).FontColor(BrandingStyles.TextMedium).FontSize(9);
                                    r.ConstantItem(30, Unit.Point).AlignRight()
                                        .Text(value.ToString()).FontColor(BrandingStyles.TextDark).Bold().FontSize(9);
                                });
                                c1.Item().Height(2, Unit.Millimetre);
                            }
                        });

                        row.ConstantItem(20, Unit.Point);

                        row.RelativeItem().Column(c2 =>
                        {
                            foreach (var (factor, value) in col2)
                            {
                                c2.Item().Row(r =>
                                {
                                    r.RelativeItem().Text(factor).FontColor(BrandingStyles.TextMedium).FontSize(9);
                                    r.ConstantItem(30, Unit.Point).AlignRight()
                                        .Text(value.ToString()).FontColor(BrandingStyles.TextDark).Bold().FontSize(9);
                                });
                                c2.Item().Height(2, Unit.Millimetre);
                            }
                        });
                    });
                });
        });
    }

    private void RenderMetricCard(IContainer container, string label, string value, string color)
    {
        container
            .Height(60, Unit.Point)
            .Background(Colors.White)
            .Border(2)
            .BorderColor(color)
            .Padding(10)
            .Column(col =>
            {
                col.Item()
                    .Text(label)
                    .FontColor(BrandingStyles.TextMedium)
                    .FontSize(10);

                col.Item()
                    .AlignCenter()
                    .Text(value)
                    .FontColor(color)
                    .Bold()
                    .FontSize(18);
            });
    }

    private string GetTypeAbbreviation(string type) => type switch
    {
        "External Input" => "EI",
        "External Output" => "EO",
        "External Inquiry" => "EQ",
        "Internal Logical File" => "ILF",
        "External Interface File" => "EIF",
        _ => type
    };

    private string GetTypeColor(string type) => type switch
    {
        "External Input" => Colors.Blue.Medium,
        "External Output" => Colors.Green.Medium,
        "External Inquiry" => Colors.Orange.Medium,
        "Internal Logical File" => Colors.Purple.Medium,
        "External Interface File" => Colors.Teal.Medium,
        _ => BrandingStyles.TextMedium
    };

    private string GetComplexityColor(string complexity) => complexity switch
    {
        "Low" => Colors.Green.Medium,
        "Average" => Colors.Orange.Medium,
        "High" => Colors.Red.Medium,
        _ => BrandingStyles.TextMedium
    };

    private int GetComplexityOrder(string complexity) => complexity switch
    {
        "Low" => 1,
        "Average" => 2,
        "High" => 3,
        _ => 4
    };
}