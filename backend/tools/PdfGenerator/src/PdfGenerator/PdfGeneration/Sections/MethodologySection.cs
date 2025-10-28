using QuestPDF.Fluent;
using QuestPDF.Infrastructure;
using QuestPDF.Helpers;
using PdfGenerator.PdfGeneration.Formatting;

namespace PdfGenerator.PdfGeneration.Sections;

/// <summary>
/// Methodology section explaining MIGRAI approach
/// </summary>
public class MethodologySection : IPdfSection
{
    public string SectionId => "methodology";
    public string Title => "Metodologia MIGRAI";
    public int Order => 9;
    public bool IncludeInToc => true;

    public void Render(IContainer container, SectionContext context)
    {
        container.Column(column =>
        {
            column.Item().PageBreak();

            // Section title
            column.Item()
                .Text("8. METODOLOGIA MIGRAI")
                .FontColor(BrandingStyles.PrimaryBlue)
                .Bold()
                .FontSize(18);

            column.Item().Height(10, Unit.Millimetre);

            // MIGRAI Introduction
            column.Item()
                .Background(Colors.Blue.Lighten5)
                .Padding(10)
                .Column(introCol =>
                {
                    introCol.Item()
                        .Text("Migration Intelligence with Generative AI Assistance")
                        .FontColor(BrandingStyles.PrimaryBlue)
                        .Bold()
                        .FontSize(14);

                    introCol.Item().Height(3, Unit.Millimetre);

                    introCol.Item()
                        .Text("MIGRAI é uma metodologia inovadora que combina análise automatizada de código legado " +
                              "com assistência de IA generativa para acelerar e garantir a qualidade de migrações de sistemas.")
                        .FontColor(BrandingStyles.TextMedium)
                        .FontSize(11)
                        .LineHeight(1.4f);
                });

            column.Item().Height(10, Unit.Millimetre);

            // Core principles
            column.Item()
                .Text("8.1 Princípios Fundamentais")
                .FontColor(BrandingStyles.TextDark)
                .Bold()
                .FontSize(14);

            column.Item().Height(5, Unit.Millimetre);

            var principles = new[]
            {
                ("Preservação Semântica",
                 "Todo comportamento do sistema legado deve ser preservado na nova arquitetura",
                 BrandingStyles.PrimaryBlue),
                ("Validação Contínua",
                 "Comparação byte-a-byte entre outputs do sistema legado e novo",
                 Colors.Green.Medium),
                ("Modernização Incremental",
                 "Migração modular permitindo rollback granular se necessário",
                 BrandingStyles.AccentYellow),
                ("Automação Máxima",
                 "IA analisa padrões e gera código inicial para acelerar desenvolvimento",
                 Colors.Purple.Medium)
            };

            foreach (var (principle, description, color) in principles)
            {
                column.Item()
                    .Border(1)
                    .BorderColor(color)
                    .BorderLeft(4)
                    .Padding(10)
                    .Row(row =>
                    {
                        row.ConstantItem(150, Unit.Point)
                            .Text(principle)
                            .FontColor(color)
                            .Bold()
                            .FontSize(11);

                        row.RelativeItem()
                            .Text(description)
                            .FontColor(BrandingStyles.TextMedium)
                            .FontSize(10);
                    });

                column.Item().Height(5, Unit.Millimetre);
            }

            column.Item().Height(10, Unit.Millimetre);

            // MIGRAI phases
            column.Item()
                .Text("8.2 Fases da Metodologia")
                .FontColor(BrandingStyles.TextDark)
                .Bold()
                .FontSize(14);

            column.Item().Height(5, Unit.Millimetre);

            // Phase 1: Analysis
            RenderPhase(column, "1", "ANÁLISE",
                "Parsing e compreensão profunda do código COBOL",
                new[]
                {
                    "Parser COBOL identifica 687 data items e 63 seções",
                    "Mapeamento de fluxo de dados e dependências",
                    "Identificação de regras de negócio críticas",
                    "Análise de complexidade ciclomática"
                },
                BrandingStyles.PrimaryBlue);

            column.Item().Height(8, Unit.Millimetre);

            // Phase 2: Design
            RenderPhase(column, "2", "DESIGN",
                "Arquitetura alvo com padrões modernos",
                new[]
                {
                    "Clean Architecture com separação de camadas",
                    "API-first design com OpenAPI 3.0",
                    "Mapeamento COBOL → C# para tipos de dados",
                    "Design de componentes React reutilizáveis"
                },
                Colors.Green.Medium);

            column.Item().Height(8, Unit.Millimetre);

            // Phase 3: Generation
            RenderPhase(column, "3", "GERAÇÃO",
                "IA generativa produz código inicial",
                new[]
                {
                    "Claude/GPT-4 gera entities e repositories",
                    "Scaffolding automático de controllers",
                    "Geração de testes unitários base",
                    "Documentação inline automática"
                },
                BrandingStyles.AccentYellow);

            column.Item().Height(8, Unit.Millimetre);

            // Phase 4: Refinement
            RenderPhase(column, "4", "REFINAMENTO",
                "Ajuste manual e otimização",
                new[]
                {
                    "Revisão humana do código gerado",
                    "Otimização de queries e performance",
                    "Implementação de casos edge",
                    "Refatoração para padrões da empresa"
                },
                Colors.Orange.Medium);

            column.Item().Height(8, Unit.Millimetre);

            // Phase 5: Validation
            RenderPhase(column, "5", "VALIDAÇÃO",
                "Garantia de equivalência funcional",
                new[]
                {
                    "Testes de comparação com output COBOL",
                    "Validação de cálculos financeiros",
                    "Teste de carga com volumes reais",
                    "Certificação SUSEP compliance"
                },
                Colors.Red.Medium);

            column.Item().Height(10, Unit.Millimetre);

            // AI assistance details
            column.Item()
                .Text("8.3 Assistência de IA Generativa")
                .FontColor(BrandingStyles.TextDark)
                .Bold()
                .FontSize(14);

            column.Item().Height(5, Unit.Millimetre);

            column.Item().Table(table =>
            {
                table.ColumnsDefinition(columns =>
                {
                    columns.RelativeColumn(2);
                    columns.RelativeColumn(3);
                    columns.ConstantColumn(80, Unit.Point);
                });

                table.Header(header =>
                {
                    header.Cell().Background(BrandingStyles.PrimaryBlue).Padding(5)
                        .Text("Tarefa").FontColor(Colors.White).Bold().FontSize(10);
                    header.Cell().Background(BrandingStyles.PrimaryBlue).Padding(5)
                        .Text("Assistência IA").FontColor(Colors.White).Bold().FontSize(10);
                    header.Cell().Background(BrandingStyles.PrimaryBlue).Padding(5)
                        .Text("Economia").FontColor(Colors.White).Bold().FontSize(10);
                });

                var aiTasks = new[]
                {
                    ("Análise COBOL", "Parser + interpretação semântica", "80%"),
                    ("Mapeamento de Tipos", "Conversão automática COBOL→C#", "95%"),
                    ("Geração de Entities", "Classes C# com atributos corretos", "90%"),
                    ("Criação de DTOs", "Request/Response objects", "85%"),
                    ("Testes Unitários", "Casos de teste base", "70%"),
                    ("Documentação", "XML comments e README", "75%"),
                    ("SQL Queries", "LINQ queries equivalentes", "60%"),
                    ("React Components", "Componentes UI base", "65%")
                };

                foreach (var (task, assistance, savings) in aiTasks)
                {
                    table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(5)
                        .Text(task).FontColor(BrandingStyles.TextDark).FontSize(9);
                    table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(5)
                        .Text(assistance).FontColor(BrandingStyles.TextMedium).FontSize(9);
                    table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(5)
                        .AlignCenter().Text(savings)
                        .FontColor(GetSavingsColor(savings)).Bold().FontSize(9);
                }
            });

            column.Item().Height(10, Unit.Millimetre);

            // Success metrics
            column.Item()
                .Background(Colors.Green.Lighten5)
                .Padding(10)
                .Column(successCol =>
                {
                    successCol.Item()
                        .Text("Métricas de Sucesso MIGRAI")
                        .FontColor(Colors.Green.Darken2)
                        .Bold()
                        .FontSize(11);

                    successCol.Item().Height(3, Unit.Millimetre);

                    var metrics = new[]
                    {
                        "✓ Redução de 65% no tempo de migração vs. abordagem manual",
                        "✓ 100% de cobertura de funcionalidades do sistema legado",
                        "✓ Zero discrepâncias em cálculos financeiros críticos",
                        "✓ Redução de 40% em bugs pós-migração",
                        "✓ Documentação completa gerada automaticamente"
                    };

                    foreach (var metric in metrics)
                    {
                        successCol.Item()
                            .Text(metric)
                            .FontColor(BrandingStyles.TextMedium)
                            .FontSize(10);

                        successCol.Item().Height(2, Unit.Millimetre);
                    }
                });
        });
    }

    private void RenderPhase(ColumnDescriptor column, string number, string name,
        string description, string[] activities, string color)
    {
        column.Item().Row(row =>
        {
            row.ConstantItem(40, Unit.Point)
                .Height(40, Unit.Point)
                .Background(color)
                .AlignCenter()
                .AlignMiddle()
                .Text(number)
                .FontColor(Colors.White)
                .Bold()
                .FontSize(18);

            row.ConstantItem(10, Unit.Point);

            row.RelativeItem()
                .Border(1)
                .BorderColor(color)
                .Padding(10)
                .Column(phaseCol =>
                {
                    phaseCol.Item()
                        .Text(name)
                        .FontColor(color)
                        .Bold()
                        .FontSize(12);

                    phaseCol.Item()
                        .Text(description)
                        .FontColor(BrandingStyles.TextMedium)
                        .Italic()
                        .FontSize(10);

                    phaseCol.Item().Height(3, Unit.Millimetre);

                    foreach (var activity in activities)
                    {
                        phaseCol.Item().Row(actRow =>
                        {
                            actRow.ConstantItem(15, Unit.Point)
                                .Text("•")
                                .FontColor(color)
                                .FontSize(10);

                            actRow.RelativeItem()
                                .Text(activity)
                                .FontColor(BrandingStyles.TextLight)
                                .FontSize(9);
                        });

                        phaseCol.Item().Height(1, Unit.Millimetre);
                    }
                });
        });
    }

    private string GetSavingsColor(string savings)
    {
        var value = int.Parse(savings.Replace("%", ""));
        return value switch
        {
            >= 80 => Colors.Green.Darken1,
            >= 60 => Colors.Green.Medium,
            >= 40 => BrandingStyles.AccentYellow,
            _ => Colors.Orange.Medium
        };
    }
}