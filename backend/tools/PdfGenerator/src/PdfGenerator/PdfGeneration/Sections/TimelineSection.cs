using QuestPDF.Fluent;
using QuestPDF.Infrastructure;
using QuestPDF.Helpers;
using PdfGenerator.PdfGeneration.Formatting;
using System.IO;

namespace PdfGenerator.PdfGeneration.Sections;

/// <summary>
/// Timeline section with Gantt chart showing project schedule
/// </summary>
public class TimelineSection : IPdfSection
{
    public string SectionId => "timeline";
    public string Title => "Cronograma do Projeto";
    public int Order => 8;
    public bool IncludeInToc => true;

    public void Render(IContainer container, SectionContext context)
    {
        container.Column(column =>
        {
            column.Item().PageBreak();

            // Section title
            column.Item()
                .Text("7. CRONOGRAMA DO PROJETO")
                .FontColor(BrandingStyles.PrimaryBlue)
                .Bold()
                .FontSize(18);

            column.Item().Height(10, Unit.Millimetre);

            // Timeline overview
            column.Item()
                .Text("7.1 Visão Geral")
                .FontColor(BrandingStyles.TextDark)
                .Bold()
                .FontSize(14);

            column.Item().Height(5, Unit.Millimetre);

            // Timeline summary
            column.Item().Row(row =>
            {
                row.RelativeItem().Element(c => RenderTimelineCard(c,
                    "Duração Total", "12 semanas",
                    "8 desenvolvimento + 4 homologação",
                    BrandingStyles.PrimaryBlue));

                row.ConstantItem(10, Unit.Point);

                row.RelativeItem().Element(c => RenderTimelineCard(c,
                    "Início", context.Schedule.StartDate.ToString("dd/MM/yyyy"),
                    "Kickoff + Setup",
                    BrandingStyles.AccentYellow));

                row.ConstantItem(10, Unit.Point);

                row.RelativeItem().Element(c => RenderTimelineCard(c,
                    "Go-Live", context.Schedule.EndDate.ToString("dd/MM/yyyy"),
                    "Produção + Suporte",
                    Colors.Green.Medium));
            });

            column.Item().Height(10, Unit.Millimetre);

            // Gantt chart placeholder (would be actual chart in production)
            column.Item()
                .Text("7.2 Gráfico de Gantt")
                .FontColor(BrandingStyles.TextDark)
                .Bold()
                .FontSize(14);

            column.Item().Height(5, Unit.Millimetre);

            // Check if Gantt chart image exists
            var ganttImagePath = Path.Combine(context.OutputPath, "charts", "gantt-chart.png");
            if (File.Exists(ganttImagePath))
            {
                column.Item()
                    .Height(200, Unit.Point)
                    .Image(ganttImagePath);
            }
            else
            {
                // Render a simple text-based Gantt chart
                column.Item()
                    .Border(1)
                    .BorderColor(BrandingStyles.BorderGray)
                    .Padding(10)
                    .Height(200, Unit.Point)
                    .Column(ganttCol => RenderTextGanttChart(ganttCol, context));
            }

            column.Item().Height(10, Unit.Millimetre);

            // Phases breakdown
            column.Item()
                .Text("7.3 Detalhamento das Fases")
                .FontColor(BrandingStyles.TextDark)
                .Bold()
                .FontSize(14);

            column.Item().Height(5, Unit.Millimetre);

            foreach (var phase in context.Schedule.Phases)
            {
                column.Item()
                    .Border(1)
                    .BorderColor(BrandingStyles.BorderGray)
                    .Padding(10)
                    .Column(phaseCol =>
                    {
                        phaseCol.Item().Row(row =>
                        {
                            row.RelativeItem()
                                .Text(phase.Name)
                                .FontColor(BrandingStyles.PrimaryBlue)
                                .Bold()
                                .FontSize(12);

                            row.ConstantItem(100, Unit.Point)
                                .AlignRight()
                                .Text($"Semanas {phase.StartWeek}-{phase.EndWeek}")
                                .FontColor(BrandingStyles.TextMedium)
                                .FontSize(10);
                        });

                        phaseCol.Item().Height(3, Unit.Millimetre);

                        phaseCol.Item()
                            .Text(phase.Description)
                            .FontColor(BrandingStyles.TextMedium)
                            .FontSize(10);

                        phaseCol.Item().Height(5, Unit.Millimetre);

                        // Tasks table
                        phaseCol.Item().Table(table =>
                        {
                            table.ColumnsDefinition(columns =>
                            {
                                columns.RelativeColumn(3);
                                columns.ConstantColumn(80, Unit.Point);
                                columns.ConstantColumn(80, Unit.Point);
                                columns.ConstantColumn(60, Unit.Point);
                            });

                            table.Header(header =>
                            {
                                header.Cell().Background(Colors.Grey.Lighten4).Padding(3)
                                    .Text("Tarefa").FontColor(BrandingStyles.TextDark).Bold().FontSize(9);
                                header.Cell().Background(Colors.Grey.Lighten4).Padding(3)
                                    .Text("Duração").FontColor(BrandingStyles.TextDark).Bold().FontSize(9);
                                header.Cell().Background(Colors.Grey.Lighten4).Padding(3)
                                    .Text("Dependências").FontColor(BrandingStyles.TextDark).Bold().FontSize(9);
                                header.Cell().Background(Colors.Grey.Lighten4).Padding(3)
                                    .Text("Status").FontColor(BrandingStyles.TextDark).Bold().FontSize(9);
                            });

                            foreach (var task in phase.Tasks.Take(5))
                            {
                                table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(3)
                                    .Text(task.Name).FontColor(BrandingStyles.TextDark).FontSize(9);
                                table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(3)
                                    .Text($"{task.DurationDays}d").FontColor(BrandingStyles.TextMedium).FontSize(9);
                                table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(3)
                                    .Text(string.Join(", ", task.Dependencies))
                                    .FontColor(BrandingStyles.TextLight).FontSize(8);
                                table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(3)
                                    .AlignCenter()
                                    .Text(GetStatusIcon(task.Status))
                                    .FontColor(GetStatusColor(task.Status))
                                    .FontSize(9);
                            }

                            if (phase.Tasks.Count > 5)
                            {
                                table.Cell().ColumnSpan(4).Padding(3)
                                    .AlignCenter()
                                    .Text($"... e mais {phase.Tasks.Count - 5} tarefas")
                                    .FontColor(BrandingStyles.TextLight)
                                    .Italic()
                                    .FontSize(8);
                            }
                        });
                    });

                column.Item().Height(8, Unit.Millimetre);
            }

            // Milestones
            column.Item()
                .Text("7.4 Marcos Principais")
                .FontColor(BrandingStyles.TextDark)
                .Bold()
                .FontSize(14);

            column.Item().Height(5, Unit.Millimetre);

            column.Item().Table(table =>
            {
                table.ColumnsDefinition(columns =>
                {
                    columns.ConstantColumn(30, Unit.Point);
                    columns.ConstantColumn(80, Unit.Point);
                    columns.RelativeColumn(2);
                    columns.RelativeColumn(3);
                });

                table.Header(header =>
                {
                    header.Cell().Background(BrandingStyles.AccentYellow).Padding(5)
                        .Text("").FontColor(BrandingStyles.TextDark).FontSize(10);
                    header.Cell().Background(BrandingStyles.AccentYellow).Padding(5)
                        .Text("Data").FontColor(BrandingStyles.TextDark).Bold().FontSize(10);
                    header.Cell().Background(BrandingStyles.AccentYellow).Padding(5)
                        .Text("Marco").FontColor(BrandingStyles.TextDark).Bold().FontSize(10);
                    header.Cell().Background(BrandingStyles.AccentYellow).Padding(5)
                        .Text("Critério de Aceite").FontColor(BrandingStyles.TextDark).Bold().FontSize(10);
                });

                var milestoneIcon = "◆";
                foreach (var milestone in context.Schedule.Milestones)
                {
                    table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(5)
                        .AlignCenter().Text(milestoneIcon)
                        .FontColor(BrandingStyles.AccentYellow).FontSize(12);
                    table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(5)
                        .Text(milestone.Date.ToString("dd/MM/yyyy"))
                        .FontColor(BrandingStyles.TextMedium).FontSize(9);
                    table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(5)
                        .Text(milestone.Name).FontColor(BrandingStyles.TextDark).Bold().FontSize(9);
                    table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(5)
                        .Text(milestone.AcceptanceCriteria)
                        .FontColor(BrandingStyles.TextLight).FontSize(9);
                }
            });
        });
    }

    private void RenderTimelineCard(IContainer container, string title, string value, string subtitle, string color)
    {
        container
            .Border(1)
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
                    .FontSize(14);

                col.Item().Height(2, Unit.Millimetre);

                col.Item()
                    .Text(subtitle)
                    .FontColor(BrandingStyles.TextLight)
                    .Italic()
                    .FontSize(9);
            });
    }

    private void RenderTextGanttChart(ColumnDescriptor column, SectionContext context)
    {
        column.Item()
            .Text("Cronograma Visual")
            .FontColor(BrandingStyles.TextDark)
            .Bold()
            .FontSize(10);

        column.Item().Height(5, Unit.Millimetre);

        // Week headers
        column.Item().Row(row =>
        {
            row.ConstantItem(100, Unit.Point)
                .Text("Fase")
                .FontColor(BrandingStyles.TextMedium)
                .FontSize(8);

            for (int week = 1; week <= 12; week++)
            {
                row.RelativeItem()
                    .AlignCenter()
                    .Text($"S{week}")
                    .FontColor(BrandingStyles.TextLight)
                    .FontSize(8);
            }
        });

        column.Item().Height(3, Unit.Millimetre);

        // Phase bars
        foreach (var phase in context.Schedule.Phases)
        {
            column.Item().Row(row =>
            {
                row.ConstantItem(100, Unit.Point)
                    .Text(phase.Name)
                    .FontColor(BrandingStyles.TextDark)
                    .FontSize(8);

                for (int week = 1; week <= 12; week++)
                {
                    var isActive = week >= phase.StartWeek && week <= phase.EndWeek;
                    var color = isActive ? GetPhaseColor(phase.Name) : Colors.Grey.Lighten4;

                    row.RelativeItem()
                        .Height(15, Unit.Point)
                        .Background(color)
                        .Border(0.5f)
                        .BorderColor(Colors.White);
                }
            });

            column.Item().Height(2, Unit.Millimetre);
        }
    }

    private string GetPhaseColor(string phaseName)
    {
        return phaseName switch
        {
            var name when name.Contains("Setup") => BrandingStyles.PrimaryBlue,
            var name when name.Contains("Backend") => Colors.Green.Medium,
            var name when name.Contains("Frontend") => Colors.Purple.Medium,
            var name when name.Contains("Integração") => Colors.Orange.Medium,
            var name when name.Contains("Teste") => Colors.Red.Medium,
            var name when name.Contains("Homologação") => BrandingStyles.AccentYellow,
            _ => Colors.Grey.Medium
        };
    }

    private string GetStatusIcon(string status) => status switch
    {
        "Completed" => "✓",
        "InProgress" => "→",
        "Pending" => "○",
        "Blocked" => "✗",
        _ => "?"
    };

    private string GetStatusColor(string status) => status switch
    {
        "Completed" => Colors.Green.Medium,
        "InProgress" => BrandingStyles.AccentYellow,
        "Pending" => BrandingStyles.TextLight,
        "Blocked" => Colors.Red.Medium,
        _ => BrandingStyles.TextMedium
    };
}