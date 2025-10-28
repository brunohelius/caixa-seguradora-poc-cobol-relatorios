using QuestPDF.Fluent;
using QuestPDF.Infrastructure;
using QuestPDF.Helpers;
using PdfGenerator.PdfGeneration.Formatting;

namespace PdfGenerator.PdfGeneration.Sections;

/// <summary>
/// Cover page section for the migration analysis PDF
/// </summary>
public class CoverPageSection : IPdfSection
{
    public string SectionId => "cover";
    public string Title => "Capa";
    public int Order => 0;
    public bool IncludeInToc => false;

    public void Render(IContainer container, SectionContext context)
    {
        container.Page(page =>
        {
            page.Size(PageSizes.A4);
            page.Margin(2, Unit.Centimetre);

            page.Content().Column(column =>
            {
                // Top spacing
                column.Item().Height(4, Unit.Centimetre);

                // Caixa Seguradora Logo placeholder (would be actual logo in production)
                column.Item().AlignCenter().Column(logoColumn =>
                {
                    logoColumn.Item()
                        .Width(200, Unit.Point)
                        .Height(80, Unit.Point)
                        .Background(BrandingStyles.PrimaryBlue)
                        .AlignCenter()
                        .AlignMiddle()
                        .Text("CAIXA SEGURADORA")
                        .FontColor(Colors.White)
                        .Bold()
                        .FontSize(18);

                    logoColumn.Item().Height(5, Unit.Millimetre);
                });

                // Spacing
                column.Item().Height(3, Unit.Centimetre);

                // Main title
                column.Item().AlignCenter().Column(titleColumn =>
                {
                    titleColumn.Item()
                        .Text("ANÁLISE DE MIGRAÇÃO")
                        .FontColor(BrandingStyles.PrimaryBlue)
                        .Bold()
                        .FontSize(32);

                    titleColumn.Item().Height(5, Unit.Millimetre);

                    titleColumn.Item()
                        .Text("Sistema de Prêmios SUSEP")
                        .FontColor(BrandingStyles.TextDark)
                        .FontSize(24);

                    titleColumn.Item().Height(3, Unit.Millimetre);

                    titleColumn.Item()
                        .Text("RG1866B → .NET 9.0 + React")
                        .FontColor(BrandingStyles.TextMedium)
                        .FontSize(18);
                });

                // Spacing
                column.Item().Height(2, Unit.Centimetre);

                // Document info box
                column.Item().AlignCenter().Width(400, Unit.Point)
                    .Border(1)
                    .BorderColor(BrandingStyles.BorderGray)
                    .Padding(20)
                    .Column(infoColumn =>
                    {
                        infoColumn.Item().Row(row =>
                        {
                            row.RelativeItem()
                                .Text("Versão:")
                                .FontColor(BrandingStyles.TextMedium)
                                .FontSize(12);

                            row.RelativeItem()
                                .AlignRight()
                                .Text(context.Metadata.Version)
                                .FontColor(BrandingStyles.TextDark)
                                .Bold()
                                .FontSize(12);
                        });

                        infoColumn.Item().Height(5, Unit.Millimetre);

                        infoColumn.Item().Row(row =>
                        {
                            row.RelativeItem()
                                .Text("Data de Geração:")
                                .FontColor(BrandingStyles.TextMedium)
                                .FontSize(12);

                            row.RelativeItem()
                                .AlignRight()
                                .Text(context.Metadata.CreatedDate.ToString("dd/MM/yyyy HH:mm"))
                                .FontColor(BrandingStyles.TextDark)
                                .Bold()
                                .FontSize(12);
                        });

                        infoColumn.Item().Height(5, Unit.Millimetre);

                        infoColumn.Item().Row(row =>
                        {
                            row.RelativeItem()
                                .Text("Pontos de Função:")
                                .FontColor(BrandingStyles.TextMedium)
                                .FontSize(12);

                            row.RelativeItem()
                                .AlignRight()
                                .Text(context.FunctionPoints.Sum(fp => fp.AdjustedPoints).ToString("N0"))
                                .FontColor(BrandingStyles.TextDark)
                                .Bold()
                                .FontSize(12);
                        });

                        infoColumn.Item().Height(5, Unit.Millimetre);

                        infoColumn.Item().Row(row =>
                        {
                            row.RelativeItem()
                                .Text("Custo Estimado:")
                                .FontColor(BrandingStyles.TextMedium)
                                .FontSize(12);

                            row.RelativeItem()
                                .AlignRight()
                                .Text($"R$ {context.Financial.TotalCost:N2}")
                                .FontColor(BrandingStyles.AccentYellow)
                                .Bold()
                                .FontSize(12);
                        });
                    });

                // Spacing
                column.Item().Height(3, Unit.Centimetre);

                // Bottom section with methodology
                column.Item().AlignCenter().Column(bottomColumn =>
                {
                    bottomColumn.Item()
                        .Width(450, Unit.Point)
                        .Background(Colors.Grey.Lighten4)
                        .Padding(15)
                        .Column(methodColumn =>
                        {
                            methodColumn.Item()
                                .Text("Metodologia MIGRAI")
                                .FontColor(BrandingStyles.PrimaryBlue)
                                .Bold()
                                .FontSize(14)
                                .AlignCenter();

                            methodColumn.Item().Height(3, Unit.Millimetre);

                            methodColumn.Item()
                                .Text("Migration Intelligence with Generative AI Assistance")
                                .FontColor(BrandingStyles.TextMedium)
                                .Italic()
                                .FontSize(10)
                                .AlignCenter();
                        });
                });

                // Footer with confidentiality notice
                column.Item().AlignBottom().AlignCenter()
                    .Text("DOCUMENTO CONFIDENCIAL - USO INTERNO")
                    .FontColor(Colors.Red.Medium)
                    .Bold()
                    .FontSize(10);
            });
        });
    }
}