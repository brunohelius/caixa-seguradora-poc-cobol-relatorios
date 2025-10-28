using QuestPDF.Fluent;
using QuestPDF.Infrastructure;
using QuestPDF.Helpers;
using PdfGenerator.PdfGeneration.Formatting;
using PdfGenerator.Formatting;

namespace PdfGenerator.PdfGeneration.Sections;

/// <summary>
/// Executive Summary section providing 2-page overview of the migration project
/// </summary>
public class ExecutiveSummarySection : IPdfSection
{
    public string SectionId => "executive-summary";
    public string Title => "Resumo Executivo";
    public int Order => 2;
    public bool IncludeInToc => true;

    public void Render(IContainer container, SectionContext context)
    {
        container.Column(column =>
        {
            // Page 1: Project Overview
            column.Item().PageBreak();
            column.Item().Element(container => RenderPage1(container, context));

            // Page 2: Key Metrics and Benefits
            column.Item().PageBreak();
            column.Item().Element(container => RenderPage2(container, context));
        });
    }

    private void RenderPage1(IContainer container, SectionContext context)
    {
        container.Column(column =>
        {
            // Section title
            column.Item()
                .Text("1. RESUMO EXECUTIVO")
                .FontColor(BrandingStyles.PrimaryBlue)
                .Bold()
                .FontSize(18);

            column.Item().Height(10, Unit.Millimetre);

            // Project context
            column.Item().Column(contentColumn =>
            {
                contentColumn.Item()
                    .Text("1.1 Contexto do Projeto")
                    .FontColor(BrandingStyles.TextDark)
                    .Bold()
                    .FontSize(14);

                contentColumn.Item().Height(5, Unit.Millimetre);

                contentColumn.Item()
                    .Text("A Caixa Seguradora possui um sistema crítico de processamento de prêmios SUSEP (RG1866B) " +
                          "desenvolvido em COBOL, executando em mainframe IBM z/OS. Este sistema processa mensalmente " +
                          $"cerca de {context.CobolMetrics.TotalDataItems:N0} itens de dados distribuídos em " +
                          $"{context.CobolMetrics.DatabaseTables} tabelas DB2, gerando relatórios regulatórios obrigatórios " +
                          "conforme Circular SUSEP 360.")
                    .FontColor(BrandingStyles.TextMedium)
                    .FontSize(11)
                    .LineHeight(1.5f);

                contentColumn.Item().Height(8, Unit.Millimetre);

                // Business challenges
                contentColumn.Item()
                    .Text("1.2 Desafios de Negócio")
                    .FontColor(BrandingStyles.TextDark)
                    .Bold()
                    .FontSize(14);

                contentColumn.Item().Height(5, Unit.Millimetre);

                contentColumn.Item().Column(challengesColumn =>
                {
                    var challenges = new[]
                    {
                        "Custos operacionais elevados com mainframe (MIPS)",
                        "Escassez de profissionais COBOL no mercado",
                        "Dificuldade de integração com sistemas modernos",
                        "Limitações para implementar novos requisitos regulatórios",
                        "Tempo de processamento batch superior a 4 horas"
                    };

                    foreach (var challenge in challenges)
                    {
                        challengesColumn.Item().Row(row =>
                        {
                            row.ConstantItem(15, Unit.Point)
                                .AlignCenter()
                                .Text("•")
                                .FontColor(BrandingStyles.AccentYellow)
                                .Bold()
                                .FontSize(12);

                            row.RelativeItem()
                                .Text(challenge)
                                .FontColor(BrandingStyles.TextMedium)
                                .FontSize(11);
                        });

                        challengesColumn.Item().Height(3, Unit.Millimetre);
                    }
                });

                contentColumn.Item().Height(8, Unit.Millimetre);

                // Proposed solution
                contentColumn.Item()
                    .Text("1.3 Solução Proposta")
                    .FontColor(BrandingStyles.TextDark)
                    .Bold()
                    .FontSize(14);

                contentColumn.Item().Height(5, Unit.Millimetre);

                contentColumn.Item()
                    .Text("Migração completa do sistema RG1866B para arquitetura moderna baseada em:")
                    .FontColor(BrandingStyles.TextMedium)
                    .FontSize(11);

                contentColumn.Item().Height(5, Unit.Millimetre);

                // Architecture highlights
                contentColumn.Item()
                    .Background(Colors.Blue.Lighten5)
                    .Padding(10)
                    .Column(archColumn =>
                    {
                        archColumn.Item().Row(row =>
                        {
                            row.RelativeItem().Column(backendCol =>
                            {
                                backendCol.Item()
                                    .Text("Backend")
                                    .FontColor(BrandingStyles.PrimaryBlue)
                                    .Bold()
                                    .FontSize(12);

                                backendCol.Item().Height(3, Unit.Millimetre);

                                backendCol.Item()
                                    .Text("• .NET 9.0 com C#")
                                    .FontColor(BrandingStyles.TextMedium)
                                    .FontSize(10);

                                backendCol.Item()
                                    .Text("• Clean Architecture")
                                    .FontColor(BrandingStyles.TextMedium)
                                    .FontSize(10);

                                backendCol.Item()
                                    .Text("• Entity Framework Core")
                                    .FontColor(BrandingStyles.TextMedium)
                                    .FontSize(10);

                                backendCol.Item()
                                    .Text("• API REST + SOAP")
                                    .FontColor(BrandingStyles.TextMedium)
                                    .FontSize(10);
                            });

                            row.RelativeItem().Column(frontendCol =>
                            {
                                frontendCol.Item()
                                    .Text("Frontend")
                                    .FontColor(BrandingStyles.PrimaryBlue)
                                    .Bold()
                                    .FontSize(12);

                                frontendCol.Item().Height(3, Unit.Millimetre);

                                frontendCol.Item()
                                    .Text("• React 18")
                                    .FontColor(BrandingStyles.TextMedium)
                                    .FontSize(10);

                                frontendCol.Item()
                                    .Text("• TypeScript")
                                    .FontColor(BrandingStyles.TextMedium)
                                    .FontSize(10);

                                frontendCol.Item()
                                    .Text("• TailwindCSS")
                                    .FontColor(BrandingStyles.TextMedium)
                                    .FontSize(10);

                                frontendCol.Item()
                                    .Text("• Recharts")
                                    .FontColor(BrandingStyles.TextMedium)
                                    .FontSize(10);
                            });

                            row.RelativeItem().Column(infraCol =>
                            {
                                infraCol.Item()
                                    .Text("Infraestrutura")
                                    .FontColor(BrandingStyles.PrimaryBlue)
                                    .Bold()
                                    .FontSize(12);

                                infraCol.Item().Height(3, Unit.Millimetre);

                                infraCol.Item()
                                    .Text("• Azure Cloud")
                                    .FontColor(BrandingStyles.TextMedium)
                                    .FontSize(10);

                                infraCol.Item()
                                    .Text("• Docker/K8s")
                                    .FontColor(BrandingStyles.TextMedium)
                                    .FontSize(10);

                                infraCol.Item()
                                    .Text("• Azure SQL")
                                    .FontColor(BrandingStyles.TextMedium)
                                    .FontSize(10);

                                infraCol.Item()
                                    .Text("• CI/CD Pipeline")
                                    .FontColor(BrandingStyles.TextMedium)
                                    .FontSize(10);
                            });
                        });
                    });
            });
        });
    }

    private void RenderPage2(IContainer container, SectionContext context)
    {
        container.Column(column =>
        {
            // Key metrics section
            column.Item()
                .Text("1.4 Métricas-Chave do Projeto")
                .FontColor(BrandingStyles.TextDark)
                .Bold()
                .FontSize(14);

            column.Item().Height(8, Unit.Millimetre);

            // Metrics grid
            column.Item().Row(row =>
            {
                // Function Points
                row.RelativeItem().Element(container => RenderMetricBox(
                    container,
                    "Pontos de Função",
                    context.FunctionPoints.Sum(fp => fp.AdjustedPoints).ToString("N0"),
                    "IFPUG 4.3.1",
                    BrandingStyles.PrimaryBlue
                ));

                row.ConstantItem(10, Unit.Point);

                // Investment
                row.RelativeItem().Element(container => RenderMetricBox(
                    container,
                    "Investimento Total",
                    _formatter.FormatCurrency(context.Financial.TotalCost),
                    "12 semanas",
                    BrandingStyles.AccentYellow
                ));

                row.ConstantItem(10, Unit.Point);

                // ROI
                row.RelativeItem().Element(container => RenderMetricBox(
                    container,
                    "ROI Estimado",
                    $"{context.Financial.EstimatedROI:P0}",
                    "18 meses",
                    Colors.Green.Medium
                ));
            });

            column.Item().Height(10, Unit.Millimetre);

            // Benefits section
            column.Item()
                .Text("1.5 Benefícios Esperados")
                .FontColor(BrandingStyles.TextDark)
                .Bold()
                .FontSize(14);

            column.Item().Height(5, Unit.Millimetre);

            // Benefits table
            column.Item().Table(table =>
            {
                table.ColumnsDefinition(columns =>
                {
                    columns.RelativeColumn(3);
                    columns.RelativeColumn(2);
                    columns.RelativeColumn(2);
                });

                // Header
                table.Header(header =>
                {
                    header.Cell().Background(BrandingStyles.PrimaryBlue)
                        .Padding(5)
                        .Text("Benefício")
                        .FontColor(Colors.White)
                        .Bold()
                        .FontSize(11);

                    header.Cell().Background(BrandingStyles.PrimaryBlue)
                        .Padding(5)
                        .Text("Situação Atual")
                        .FontColor(Colors.White)
                        .Bold()
                        .FontSize(11);

                    header.Cell().Background(BrandingStyles.PrimaryBlue)
                        .Padding(5)
                        .Text("Após Migração")
                        .FontColor(Colors.White)
                        .Bold()
                        .FontSize(11);
                });

                // Benefits rows
                var benefits = new[]
                {
                    ("Tempo de Processamento", "4-6 horas", "< 30 minutos"),
                    ("Custo Operacional Anual", "R$ 2.4M", "R$ 480K"),
                    ("Disponibilidade de Talentos", "Escasso", "Abundante"),
                    ("Capacidade de Integração", "Limitada", "Total (REST/gRPC)"),
                    ("Tempo para Mudanças", "3-6 meses", "1-2 sprints"),
                    ("Observabilidade", "Logs básicos", "Full APM + Traces")
                };

                foreach (var (benefit, current, future) in benefits)
                {
                    table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3)
                        .Padding(5)
                        .Text(benefit)
                        .FontColor(BrandingStyles.TextDark)
                        .FontSize(10);

                    table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3)
                        .Padding(5)
                        .Text(current)
                        .FontColor(Colors.Red.Medium)
                        .FontSize(10);

                    table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3)
                        .Padding(5)
                        .Text(future)
                        .FontColor(Colors.Green.Medium)
                        .Bold()
                        .FontSize(10);
                }
            });

            column.Item().Height(10, Unit.Millimetre);

            // Timeline summary
            column.Item()
                .Text("1.6 Cronograma de Execução")
                .FontColor(BrandingStyles.TextDark)
                .Bold()
                .FontSize(14);

            column.Item().Height(5, Unit.Millimetre);

            column.Item().Row(row =>
            {
                foreach (var phase in context.Schedule.Phases.Take(4))
                {
                    row.RelativeItem()
                        .Border(1)
                        .BorderColor(BrandingStyles.BorderGray)
                        .Padding(8)
                        .Column(phaseColumn =>
                        {
                            phaseColumn.Item()
                                .Text(phase.Name)
                                .FontColor(BrandingStyles.PrimaryBlue)
                                .Bold()
                                .FontSize(10);

                            phaseColumn.Item().Height(2, Unit.Millimetre);

                            phaseColumn.Item()
                                .Text($"{phase.DurationWeeks} semanas")
                                .FontColor(BrandingStyles.TextMedium)
                                .FontSize(9);

                            phaseColumn.Item()
                                .Text($"{phase.Tasks.Count} tarefas")
                                .FontColor(BrandingStyles.TextLight)
                                .FontSize(8);
                        });

                    if (phase != context.Schedule.Phases.Take(4).Last())
                    {
                        row.ConstantItem(5, Unit.Point);
                    }
                }
            });

            column.Item().Height(10, Unit.Millimetre);

            // Risk mitigation note
            column.Item()
                .Background(Colors.Orange.Lighten5)
                .Padding(10)
                .Column(riskColumn =>
                {
                    riskColumn.Item()
                        .Text("Mitigação de Riscos")
                        .FontColor(Colors.Orange.Darken3)
                        .Bold()
                        .FontSize(11);

                    riskColumn.Item().Height(3, Unit.Millimetre);

                    riskColumn.Item()
                        .Text("• Validação byte-a-byte com output COBOL garantindo conformidade SUSEP\n" +
                              "• Execução paralela por 3 meses antes de descomissionamento\n" +
                              "• Rollback automático em caso de discrepâncias")
                        .FontColor(BrandingStyles.TextMedium)
                        .FontSize(10)
                        .LineHeight(1.4f);
                });
        });
    }

    private void RenderMetricBox(IContainer container, string label, string value, string subtext, string color)
    {
        container
            .Border(1)
            .BorderColor(color)
            .Padding(10)
            .Column(column =>
            {
                column.Item()
                    .Text(label)
                    .FontColor(BrandingStyles.TextMedium)
                    .FontSize(10);

                column.Item().Height(3, Unit.Millimetre);

                column.Item()
                    .Text(value)
                    .FontColor(color)
                    .Bold()
                    .FontSize(20);

                column.Item().Height(2, Unit.Millimetre);

                column.Item()
                    .Text(subtext)
                    .FontColor(BrandingStyles.TextLight)
                    .Italic()
                    .FontSize(9);
            });
    }
}