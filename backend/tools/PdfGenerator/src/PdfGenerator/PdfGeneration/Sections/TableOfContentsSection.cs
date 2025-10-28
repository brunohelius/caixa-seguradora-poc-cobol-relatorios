using QuestPDF.Fluent;
using QuestPDF.Infrastructure;
using QuestPDF.Helpers;
using PdfGenerator.PdfGeneration.Formatting;

namespace PdfGenerator.PdfGeneration.Sections;

/// <summary>
/// Table of Contents section with hyperlinks to all document sections
/// </summary>
public class TableOfContentsSection : IPdfSection
{
    private readonly List<IPdfSection> _sections;

    public string SectionId => "toc";
    public string Title => "Sumário";
    public int Order => 1;
    public bool IncludeInToc => false;

    public TableOfContentsSection(List<IPdfSection> sections)
    {
        _sections = sections;
    }

    public void Render(IContainer container, SectionContext context)
    {
        container.Page(page =>
        {
            page.Size(PageSizes.A4);
            page.Margin(2, Unit.Centimetre);

            page.Header()
                .Height(50, Unit.Point)
                .AlignCenter()
                .Text("SUMÁRIO")
                .FontColor(BrandingStyles.PrimaryBlue)
                .Bold()
                .FontSize(24);

            page.Content().Column(column =>
            {
                column.Item().Height(10, Unit.Millimetre);

                // Get all sections that should appear in TOC
                var tocSections = _sections
                    .Where(s => s.IncludeInToc)
                    .OrderBy(s => s.Order);

                var sectionNumber = 1;
                foreach (var section in tocSections)
                {
                    column.Item().Element(container => RenderTocEntry(
                        container,
                        sectionNumber,
                        section.Title,
                        section.SectionId,
                        GetPageNumber(section.Order)
                    ));

                    column.Item().Height(8, Unit.Millimetre);
                    sectionNumber++;
                }

                // Add appendices if any
                column.Item().Height(15, Unit.Millimetre);
                column.Item()
                    .BorderTop(1)
                    .BorderColor(BrandingStyles.BorderGray)
                    .PaddingTop(10);

                column.Item().Element(container => RenderTocEntry(
                    container,
                    0,
                    "Anexos",
                    "appendices",
                    GetPageNumber(20),
                    isAppendix: true
                ));

                column.Item().Height(5, Unit.Millimetre);

                column.Item().PaddingLeft(20).Column(appendixColumn =>
                {
                    appendixColumn.Item().Element(container => RenderSubEntry(
                        container,
                        "A",
                        "Glossário de Termos Técnicos",
                        "glossary",
                        GetPageNumber(21)
                    ));

                    appendixColumn.Item().Height(5, Unit.Millimetre);

                    appendixColumn.Item().Element(container => RenderSubEntry(
                        container,
                        "B",
                        "Tabela de Complexidade IFPUG",
                        "ifpug-table",
                        GetPageNumber(22)
                    ));

                    appendixColumn.Item().Height(5, Unit.Millimetre);

                    appendixColumn.Item().Element(container => RenderSubEntry(
                        container,
                        "C",
                        "Referências e Bibliografia",
                        "references",
                        GetPageNumber(23)
                    ));
                });
            });

            page.Footer()
                .AlignCenter()
                .Text("ii")
                .FontColor(BrandingStyles.TextMedium)
                .FontSize(10);
        });
    }

    private void RenderTocEntry(
        IContainer container,
        int sectionNumber,
        string title,
        string sectionId,
        int pageNumber,
        bool isAppendix = false)
    {
        container.Row(row =>
        {
            // Section number
            if (!isAppendix && sectionNumber > 0)
            {
                row.ConstantItem(40, Unit.Point)
                    .AlignLeft()
                    .Text($"{sectionNumber}.")
                    .FontColor(BrandingStyles.TextDark)
                    .Bold()
                    .FontSize(12);
            }
            else if (isAppendix)
            {
                row.ConstantItem(40, Unit.Point);
            }

            // Title with hyperlink
            row.RelativeItem()
                .Hyperlink(sectionId)
                .Text(title)
                .FontColor(BrandingStyles.PrimaryBlue)
                .FontSize(12)
                .Underline(false);

            // Dotted line
            row.RelativeItem()
                .AlignBottom()
                .BorderBottom(1)
                .BorderColor(BrandingStyles.BorderGray)
                .Height(10, Unit.Point);

            // Page number
            row.ConstantItem(50, Unit.Point)
                .AlignRight()
                .Text(pageNumber.ToString())
                .FontColor(BrandingStyles.TextMedium)
                .FontSize(12);
        });
    }

    private void RenderSubEntry(
        IContainer container,
        string letter,
        string title,
        string sectionId,
        int pageNumber)
    {
        container.Row(row =>
        {
            // Letter
            row.ConstantItem(30, Unit.Point)
                .AlignLeft()
                .Text(letter)
                .FontColor(BrandingStyles.TextMedium)
                .FontSize(11);

            // Title with hyperlink
            row.RelativeItem()
                .Hyperlink(sectionId)
                .Text(title)
                .FontColor(BrandingStyles.TextMedium)
                .FontSize(11)
                .Underline(false);

            // Dotted line
            row.RelativeItem()
                .AlignBottom()
                .BorderBottom(1)
                .BorderColor(Colors.Grey.Lighten3)
                .Height(10, Unit.Point);

            // Page number
            row.ConstantItem(50, Unit.Point)
                .AlignRight()
                .Text(pageNumber.ToString())
                .FontColor(BrandingStyles.TextLight)
                .FontSize(11);
        });
    }

    private int GetPageNumber(int order)
    {
        // Simple page number calculation based on order
        // In a real implementation, this would be determined during PDF generation
        return order switch
        {
            0 => 1,   // Cover
            1 => 2,   // TOC
            2 => 3,   // Executive Summary
            3 => 5,   // COBOL Analysis
            4 => 12,  // Migration Architecture
            5 => 20,  // Component Specs
            6 => 28,  // Function Points
            7 => 35,  // Financial Analysis
            8 => 40,  // Timeline
            9 => 45,  // Methodology
            20 => 50, // Appendices
            21 => 51, // Glossary
            22 => 55, // IFPUG Table
            23 => 58, // References
            _ => order + 2
        };
    }
}