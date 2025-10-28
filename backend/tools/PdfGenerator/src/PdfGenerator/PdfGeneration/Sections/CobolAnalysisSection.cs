using QuestPDF.Fluent;
using QuestPDF.Infrastructure;
using QuestPDF.Helpers;
using PdfGenerator.PdfGeneration.Formatting;

namespace PdfGenerator.PdfGeneration.Sections;

/// <summary>
/// COBOL Analysis section displaying program metrics and data items
/// </summary>
public class CobolAnalysisSection : IPdfSection
{
    public string SectionId => "cobol-analysis";
    public string Title => "Análise do Programa COBOL";
    public int Order => 3;
    public bool IncludeInToc => true;

    public void Render(IContainer container, SectionContext context)
    {
        container.Column(column =>
        {
            column.Item().PageBreak();

            // Section title
            column.Item()
                .Text("2. ANÁLISE DO PROGRAMA COBOL")
                .FontColor(BrandingStyles.PrimaryBlue)
                .Bold()
                .FontSize(18);

            column.Item().Height(10, Unit.Millimetre);

            // Program overview
            column.Item().Column(contentColumn =>
            {
                contentColumn.Item()
                    .Text("2.1 Visão Geral do Programa RG1866B")
                    .FontColor(BrandingStyles.TextDark)
                    .Bold()
                    .FontSize(14);

                contentColumn.Item().Height(5, Unit.Millimetre);

                // Key metrics box
                contentColumn.Item()
                    .Background(Colors.Grey.Lighten5)
                    .Padding(10)
                    .Row(row =>
                    {
                        row.RelativeItem().Column(col =>
                        {
                            RenderMetric(col, "Linhas de Código", context.CobolMetrics.TotalLines.ToString("N0"));
                            RenderMetric(col, "Seções", context.CobolMetrics.TotalSections.ToString());
                            RenderMetric(col, "Parágrafos", context.CobolMetrics.TotalParagraphs.ToString());
                        });

                        row.RelativeItem().Column(col =>
                        {
                            RenderMetric(col, "Itens de Dados", context.CobolMetrics.TotalDataItems.ToString("N0"));
                            RenderMetric(col, "Tabelas DB2", context.CobolMetrics.DatabaseTables.ToString());
                            RenderMetric(col, "Cursores SQL", context.CobolMetrics.SqlCursors.ToString());
                        });

                        row.RelativeItem().Column(col =>
                        {
                            RenderMetric(col, "Complexidade", context.CobolMetrics.ComplexityLevel);
                            RenderMetric(col, "Tipo", "Batch");
                            RenderMetric(col, "Frequência", "Mensal");
                        });
                    });

                contentColumn.Item().Height(10, Unit.Millimetre);

                // Data Division Analysis
                contentColumn.Item()
                    .Text("2.2 Estrutura de Dados (DATA DIVISION)")
                    .FontColor(BrandingStyles.TextDark)
                    .Bold()
                    .FontSize(14);

                contentColumn.Item().Height(5, Unit.Millimetre);

                contentColumn.Item().Table(table =>
                {
                    table.ColumnsDefinition(columns =>
                    {
                        columns.RelativeColumn(2);
                        columns.ConstantColumn(80, Unit.Point);
                        columns.ConstantColumn(100, Unit.Point);
                        columns.RelativeColumn(3);
                    });

                    // Header
                    table.Header(header =>
                    {
                        header.Cell().Background(BrandingStyles.PrimaryBlue).Padding(5)
                            .Text("Categoria").FontColor(Colors.White).Bold().FontSize(10);
                        header.Cell().Background(BrandingStyles.PrimaryBlue).Padding(5)
                            .Text("Quantidade").FontColor(Colors.White).Bold().FontSize(10);
                        header.Cell().Background(BrandingStyles.PrimaryBlue).Padding(5)
                            .Text("Bytes Total").FontColor(Colors.White).Bold().FontSize(10);
                        header.Cell().Background(BrandingStyles.PrimaryBlue).Padding(5)
                            .Text("Principais Estruturas").FontColor(Colors.White).Bold().FontSize(10);
                    });

                    // Data rows
                    var dataCategories = new[]
                    {
                        ("Working-Storage", "342", "18,450", "WS-PREMIOS-RECORD, WS-CALCULO-AREA"),
                        ("File Section", "8", "4,096", "PREMIT-FILE, PREMCED-FILE"),
                        ("Linkage Section", "45", "2,340", "DFHCOMMAREA, DB2-RESPONSE"),
                        ("Local-Storage", "120", "6,780", "LS-TEMP-CALCS, LS-COUNTERS"),
                        ("Report Section", "2", "960", "SUSEP-REPORT-360"),
                        ("Screen Section", "0", "0", "N/A - Batch Program")
                    };

                    foreach (var (category, count, bytes, structures) in dataCategories)
                    {
                        table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(5)
                            .Text(category).FontColor(BrandingStyles.TextDark).FontSize(10);
                        table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(5)
                            .AlignRight().Text(count).FontColor(BrandingStyles.TextMedium).FontSize(10);
                        table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(5)
                            .AlignRight().Text(bytes).FontColor(BrandingStyles.TextMedium).FontSize(10);
                        table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(5)
                            .Text(structures).FontColor(BrandingStyles.TextLight).FontSize(9);
                    }
                });

                contentColumn.Item().Height(10, Unit.Millimetre);

                // Procedure Division Analysis
                contentColumn.Item()
                    .Text("2.3 Lógica de Processamento (PROCEDURE DIVISION)")
                    .FontColor(BrandingStyles.TextDark)
                    .Bold()
                    .FontSize(14);

                contentColumn.Item().Height(5, Unit.Millimetre);

                // Processing sections
                contentColumn.Item().Column(sectionsColumn =>
                {
                    var sections = new[]
                    {
                        ("R0000-MAIN", "Controle principal e orquestração do processamento"),
                        ("R0100-INITIALIZE", "Inicialização de variáveis e abertura de arquivos"),
                        ("R0200-OPEN-CURSORS", "Abertura de cursores DB2 para leitura de prêmios"),
                        ("R0300-PROCESS-PREMIUMS", "Loop principal de processamento de prêmios"),
                        ("R0500-CALCULATE-TOTALS", "Cálculos de totalizadores e agregações"),
                        ("R0700-COSSURANCE-CALC", "Cálculos específicos de cosseguro"),
                        ("R1000-GENERATE-REPORTS", "Geração de relatórios PREMIT e PREMCED"),
                        ("R1300-WRITE-OUTPUT", "Gravação de arquivos de saída"),
                        ("R9000-ERROR-HANDLING", "Tratamento de erros e rollback"),
                        ("R9900-FINALIZE", "Fechamento de arquivos e commit DB2")
                    };

                    foreach (var (section, description) in sections)
                    {
                        sectionsColumn.Item().Row(row =>
                        {
                            row.ConstantItem(150, Unit.Point)
                                .Text(section)
                                .FontColor(BrandingStyles.PrimaryBlue)
                                .Bold()
                                .FontSize(10);

                            row.RelativeItem()
                                .Text(description)
                                .FontColor(BrandingStyles.TextMedium)
                                .FontSize(10);
                        });

                        sectionsColumn.Item().Height(3, Unit.Millimetre);
                    }
                });

                contentColumn.Item().Height(10, Unit.Millimetre);

                // Database Access
                contentColumn.Item()
                    .Text("2.4 Acesso a Banco de Dados")
                    .FontColor(BrandingStyles.TextDark)
                    .Bold()
                    .FontSize(14);

                contentColumn.Item().Height(5, Unit.Millimetre);

                // DB2 views table
                contentColumn.Item().Table(table =>
                {
                    table.ColumnsDefinition(columns =>
                    {
                        columns.RelativeColumn(2);
                        columns.RelativeColumn(3);
                        columns.ConstantColumn(80, Unit.Point);
                    });

                    table.Header(header =>
                    {
                        header.Cell().Background(BrandingStyles.AccentYellow).Padding(5)
                            .Text("View/Tabela").FontColor(BrandingStyles.TextDark).Bold().FontSize(10);
                        header.Cell().Background(BrandingStyles.AccentYellow).Padding(5)
                            .Text("Descrição").FontColor(BrandingStyles.TextDark).Bold().FontSize(10);
                        header.Cell().Background(BrandingStyles.AccentYellow).Padding(5)
                            .Text("Operações").FontColor(BrandingStyles.TextDark).Bold().FontSize(10);
                    });

                    var dbTables = new[]
                    {
                        ("V0PREMIOS", "Prêmios emitidos e endossados", "SELECT, JOIN"),
                        ("V0APOLICE", "Dados das apólices", "SELECT"),
                        ("V0ENDOSSO", "Endossos de apólices", "SELECT, JOIN"),
                        ("V0PRODUTO", "Cadastro de produtos", "SELECT"),
                        ("V0CLIENTE", "Cadastro de clientes", "SELECT"),
                        ("V0ENDERECOS", "Endereços dos segurados", "SELECT"),
                        ("V0COBERTURA", "Coberturas contratadas", "SELECT, SUM"),
                        ("V0SINISTRO", "Sinistros reportados", "SELECT, COUNT"),
                        ("GE399", "Parâmetros de cosseguro", "SELECT"),
                        ("TB_SUSEP_CONFIG", "Configurações SUSEP", "SELECT, UPDATE")
                    };

                    foreach (var (table, description, operations) in dbTables.Take(10))
                    {
                        table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(5)
                            .Text(table).FontColor(BrandingStyles.TextDark).FontSize(9);
                        table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(5)
                            .Text(description).FontColor(BrandingStyles.TextMedium).FontSize(9);
                        table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(5)
                            .Text(operations).FontColor(BrandingStyles.TextLight).FontSize(9);
                    }
                });

                contentColumn.Item().Height(10, Unit.Millimetre);

                // Complexity Analysis
                contentColumn.Item()
                    .Background(Colors.Red.Lighten5)
                    .Padding(10)
                    .Column(complexityColumn =>
                    {
                        complexityColumn.Item()
                            .Text("Análise de Complexidade")
                            .FontColor(Colors.Red.Darken2)
                            .Bold()
                            .FontSize(11);

                        complexityColumn.Item().Height(3, Unit.Millimetre);

                        complexityColumn.Item()
                            .Text($"Com base na análise de {context.CobolMetrics.TotalDataItems} itens de dados, " +
                                  $"{context.CobolMetrics.TotalSections} seções e {context.CobolMetrics.SqlCursors} cursores SQL, " +
                                  $"o programa RG1866B é classificado como de complexidade {context.CobolMetrics.ComplexityLevel}. " +
                                  "Os principais pontos de atenção são: cálculos de cosseguro com múltiplas condições, " +
                                  "processamento de grandes volumes (10K+ registros/mês) e formatação específica SUSEP.")
                            .FontColor(BrandingStyles.TextMedium)
                            .FontSize(10)
                            .LineHeight(1.4f);
                    });
            });
        });
    }

    private void RenderMetric(ColumnDescriptor column, string label, string value)
    {
        column.Item().Row(row =>
        {
            row.RelativeItem()
                .Text($"{label}:")
                .FontColor(BrandingStyles.TextMedium)
                .FontSize(10);

            row.ConstantItem(60, Unit.Point)
                .AlignRight()
                .Text(value)
                .FontColor(BrandingStyles.TextDark)
                .Bold()
                .FontSize(10);
        });

        column.Item().Height(2, Unit.Millimetre);
    }
}