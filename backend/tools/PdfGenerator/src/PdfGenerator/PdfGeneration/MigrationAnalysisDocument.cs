using QuestPDF.Fluent;
using QuestPDF.Helpers;
using QuestPDF.Infrastructure;
using PdfGenerator.PdfGeneration.Sections;
using PdfGenerator.PdfGeneration.Formatting;

namespace PdfGenerator.PdfGeneration;

/// <summary>
/// Main PDF document class that orchestrates all sections.
/// </summary>
public class MigrationAnalysisDocument : IDocument
{
    private readonly List<IPdfSection> _sections;
    private readonly SectionContext _context;

    public MigrationAnalysisDocument(List<IPdfSection> sections, SectionContext context)
    {
        _sections = sections.OrderBy(s => s.Order).ToList();
        _context = context;
    }

    public DocumentMetadata GetMetadata()
    {
        return new DocumentMetadata
        {
            Title = _context.Metadata.Title,
            Author = _context.Metadata.Author,
            Subject = "Análise de Migração COBOL para .NET - Sistema RG1866B",
            Keywords = "COBOL, .NET, Migration, SUSEP, RG1866B, Caixa Seguradora",
            Creator = "Caixa Seguradora PDF Generator v" + _context.Metadata.Version,
            Producer = "QuestPDF"
        };
    }

    public void Compose(IDocumentContainer container)
    {
        container.Page(page =>
        {
            // Default page setup
            page.Size(PageSizes.A4);
            page.Margin(2, Unit.Centimetre);
            page.PageColor(Colors.White);

            // Header for all pages except cover
            page.Header().Element(ComposeHeader);

            // Footer for all pages
            page.Footer().Element(ComposeFooter);

            // Main content
            page.Content().Element(ComposeContent);
        });
    }

    private void ComposeHeader(IContainer container)
    {
        container.Row(row =>
        {
            row.RelativeItem().Column(column =>
            {
                column.Item().Text("Análise de Migração COBOL")
                    .FontSize(10)
                    .FontColor(BrandingStyles.PrimaryColor);
            });

            row.ConstantItem(100).AlignRight().Text(text =>
            {
                text.Span("Caixa Seguradora")
                    .FontSize(10)
                    .Bold()
                    .FontColor(BrandingStyles.PrimaryColor);
            });
        });
    }

    private void ComposeFooter(IContainer container)
    {
        container.Row(row =>
        {
            row.RelativeItem().Text(text =>
            {
                text.Span("Documento Confidencial - ")
                    .FontSize(8)
                    .FontColor(Colors.Grey.Medium);

                text.Span(_context.Metadata.GeneratedAt.ToString("dd/MM/yyyy"))
                    .FontSize(8)
                    .FontColor(Colors.Grey.Medium);
            });

            row.ConstantItem(50).AlignRight().Text(text =>
            {
                text.CurrentPageNumber()
                    .FontSize(8)
                    .FontColor(Colors.Grey.Medium);

                text.Span(" / ")
                    .FontSize(8)
                    .FontColor(Colors.Grey.Medium);

                text.TotalPages()
                    .FontSize(8)
                    .FontColor(Colors.Grey.Medium);
            });
        });
    }

    private void ComposeContent(IContainer container)
    {
        container.Column(column =>
        {
            // Render each section in order
            foreach (var section in _sections)
            {
                column.Item().Element(sectionContainer =>
                {
                    section.Render(sectionContainer, _context);
                });

                // Add page break after each section except the last
                if (section != _sections.Last())
                {
                    column.Item().PageBreak();
                }
            }
        });
    }
}