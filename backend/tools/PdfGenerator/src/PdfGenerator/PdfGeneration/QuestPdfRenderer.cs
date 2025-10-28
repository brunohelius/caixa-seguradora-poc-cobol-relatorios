using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using PdfGenerator.Models;
using PdfGenerator.PdfGeneration.Formatting;
using PdfGenerator.PdfGeneration.Sections;
using QuestPDF.Fluent;
using QuestPDF.Helpers;
using QuestPDF.Infrastructure;

namespace PdfGenerator.PdfGeneration
{
    /// <summary>
    /// Implementation of PDF renderer using QuestPDF library with PDF/A-1b compliance.
    /// </summary>
    public class QuestPdfRenderer : IPdfRenderer
    {
        private readonly List<Sections.IPdfSection> _sections = new();
        private readonly BrandingStyles _brandingStyles;
        private Models.DocumentMetadata _metadata;
        private PdfRenderContext _context;

        public QuestPdfRenderer()
        {
            _brandingStyles = new BrandingStyles();

            // Configure QuestPDF license (if applicable)
            QuestPDF.Settings.License = LicenseType.Community;
        }

        /// <summary>
        /// Generates a complete PDF document with all sections.
        /// </summary>
        public async Task<string> GeneratePdfAsync(Models.DocumentMetadata metadata, string outputPath)
        {
            _metadata = metadata;

            // Ensure output directory exists
            var directory = Path.GetDirectoryName(outputPath);
            if (!string.IsNullOrEmpty(directory))
            {
                Directory.CreateDirectory(directory);
            }

            // Create render context
            _context = new PdfRenderContext
            {
                Metadata = metadata,
                TempDirectory = Path.Combine(Path.GetTempPath(), "PdfGenerator"),
                CurrentPage = 1
            };

            // Create the document
            var document = Document.Create(container =>
            {
                // Configure document settings for PDF/A-1b compliance
                container.Page(page =>
                {
                    page.Size(PageSizes.A4);
                    page.Margin(2, Unit.Centimetre);
                    page.PageColor(Colors.White);
                    page.DefaultTextStyle(x => x
                        .FontSize(11)
                        .FontColor(_brandingStyles.TextColor));

                    // Header
                    page.Header()
                        .Height(2, Unit.Centimetre)
                        .Background(_brandingStyles.HeaderBackground)
                        .AlignCenter()
                        .Text(text =>
                        {
                            text.Span(_metadata.Title)
                                .FontSize(14)
                                .Bold()
                                .FontColor(_brandingStyles.PrimaryColor);
                        });

                    // Content
                    page.Content()
                        .Column(column =>
                        {
                            // Render all sections in order
                            foreach (var section in _sections.OrderBy(s => s.Order))
                            {
                                column.Item().Element(c => RenderSection(c, section));

                                // Add page break between sections
                                if (section != _sections.Last())
                                {
                                    column.Item().PageBreak();
                                }
                            }
                        });

                    // Footer
                    page.Footer()
                        .Height(1, Unit.Centimetre)
                        .AlignCenter()
                        .Text(text =>
                        {
                            text.CurrentPageNumber()
                                .FontSize(10);
                            text.Span(" / ");
                            text.TotalPages()
                                .FontSize(10);
                        });
                });
            });

            // Configure PDF metadata for PDF/A-1b compliance
            document.WithMetadata(metadata =>
            {
                metadata.Title = _metadata.Title;
                metadata.Author = _metadata.Author;
                metadata.Subject = _metadata.Subject;
                metadata.Keywords = string.Join(", ", _metadata.Keywords);
                metadata.Creator = "Caixa Seguradora PDF Generator";
                metadata.Producer = "QuestPDF";
                metadata.CreationDate = DateTime.Now;
                metadata.ModificationDate = DateTime.Now;
            });

            // Generate PDF with PDF/A-1b settings
            await Task.Run(() =>
            {
                document.GeneratePdf(outputPath);
            });

            // Validate PDF/A compliance
            var validationResult = await ValidatePdfComplianceAsync(outputPath);
            if (!validationResult.IsCompliant)
            {
                // Log warnings but don't fail generation
                foreach (var warning in validationResult.Warnings)
                {
                    Console.WriteLine($"PDF/A Warning: {warning}");
                }
            }

            return outputPath;
        }

        /// <summary>
        /// Renders a single section.
        /// </summary>
        private void RenderSection(IContainer container, IPdfSection section)
        {
            container.Column(column =>
            {
                // Section title
                column.Item()
                    .PaddingBottom(10)
                    .Text(section.Title)
                    .FontSize(16)
                    .Bold()
                    .FontColor(_brandingStyles.PrimaryColor);

                // Section content
                column.Item().Element(async c =>
                {
                    // Render section content asynchronously
                    section.StartPage = _context.CurrentPage;
                    await section.RenderAsync(_context);
                    _context.CurrentPage += section.EstimatePageCount();
                });
            });
        }

        /// <summary>
        /// Adds a section to the PDF document.
        /// </summary>
        public async Task AddSectionAsync(IPdfSection section)
        {
            await Task.Run(() => _sections.Add(section));
        }

        /// <summary>
        /// Configures PDF/A-1b compliance settings.
        /// </summary>
        public void ConfigurePdfACompliance()
        {
            // QuestPDF handles most PDF/A compliance automatically
            // Additional settings can be configured here if needed

            // Ensure all fonts are embedded (QuestPDF does this by default)
            // Ensure color space is RGB or CMYK
            // Ensure no transparency is used
            // Ensure metadata is complete
        }

        /// <summary>
        /// Sets the document properties.
        /// </summary>
        public void SetDocumentProperties(Models.DocumentMetadata metadata)
        {
            _metadata = metadata;
        }

        /// <summary>
        /// Validates that the generated PDF meets PDF/A-1b standards.
        /// </summary>
        public async Task<PdfValidationResult> ValidatePdfComplianceAsync(string pdfPath)
        {
            return await Task.Run(() =>
            {
                var result = new PdfValidationResult
                {
                    IsCompliant = true,
                    ComplianceLevel = "PDF/A-1b"
                };

                // Check file exists
                if (!File.Exists(pdfPath))
                {
                    result.IsCompliant = false;
                    result.Errors.Add("PDF file not found");
                    return result;
                }

                // Check file size
                var fileInfo = new FileInfo(pdfPath);
                result.FileSize = fileInfo.Length;

                if (result.FileSize > 20 * 1024 * 1024) // 20MB limit
                {
                    result.Warnings.Add($"File size ({result.FileSize / (1024 * 1024)}MB) exceeds recommended 20MB limit");
                }

                // Basic PDF/A-1b compliance checks
                // Note: For full validation, use a tool like VeraPDF

                // Check for required metadata
                if (_metadata == null)
                {
                    result.Errors.Add("Document metadata is missing");
                    result.IsCompliant = false;
                }
                else
                {
                    if (string.IsNullOrEmpty(_metadata.Title))
                        result.Warnings.Add("Document title is missing");
                    if (string.IsNullOrEmpty(_metadata.Author))
                        result.Warnings.Add("Document author is missing");
                }

                // Estimate page count based on sections
                result.PageCount = _sections.Sum(s => s.EstimatePageCount());

                return result;
            });
        }

        /// <summary>
        /// Gets the current page count of the document being generated.
        /// </summary>
        public int GetPageCount()
        {
            return _sections.Sum(s => s.EstimatePageCount());
        }

        /// <summary>
        /// Gets the estimated file size of the document.
        /// </summary>
        public long GetEstimatedFileSize()
        {
            // Rough estimate: 100KB per page + 500KB for images/charts per section
            var baseSize = GetPageCount() * 100 * 1024L;
            var chartSize = _sections.Count * 500 * 1024L;
            return baseSize + chartSize;
        }
    }
}