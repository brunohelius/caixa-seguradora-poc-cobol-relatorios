using System.IO;
using System.Threading.Tasks;
using PdfGenerator.Models;
using PdfGenerator.PdfGeneration.Sections;

namespace PdfGenerator.PdfGeneration
{
    /// <summary>
    /// Interface for PDF rendering with PDF/A-1b compliance.
    /// </summary>
    public interface IPdfRenderer
    {
        /// <summary>
        /// Generates a complete PDF document with all sections.
        /// </summary>
        /// <param name="metadata">Document metadata including version, title, etc.</param>
        /// <param name="outputPath">Path where the PDF should be saved</param>
        /// <returns>Path to the generated PDF file</returns>
        System.Threading.Tasks.Task<string> GeneratePdfAsync(DocumentMetadata metadata, string outputPath);

        /// <summary>
        /// Adds a section to the PDF document.
        /// </summary>
        /// <param name="section">The section to add</param>
        System.Threading.Tasks.Task AddSectionAsync(Sections.IPdfSection section);

        /// <summary>
        /// Configures PDF/A-1b compliance settings.
        /// </summary>
        void ConfigurePdfACompliance();

        /// <summary>
        /// Sets the document properties (title, author, subject, keywords).
        /// </summary>
        /// <param name="metadata">Document metadata</param>
        void SetDocumentProperties(DocumentMetadata metadata);

        /// <summary>
        /// Validates that the generated PDF meets PDF/A-1b standards.
        /// </summary>
        /// <param name="pdfPath">Path to the PDF file to validate</param>
        /// <returns>Validation result with any compliance issues</returns>
        Task<PdfValidationResult> ValidatePdfComplianceAsync(string pdfPath);

        /// <summary>
        /// Gets the current page count of the document being generated.
        /// </summary>
        int GetPageCount();

        /// <summary>
        /// Gets the estimated file size of the document.
        /// </summary>
        long GetEstimatedFileSize();
    }

    /// <summary>
    /// Context for PDF rendering operations.
    /// </summary>
    public class PdfRenderContext
    {
        public DocumentMetadata Metadata { get; set; }
        public CobolMetrics CobolMetrics { get; set; }
        public MigrationArchitecture Architecture { get; set; }
        public FunctionPointResult FunctionPoints { get; set; }
        public FinancialAnalysis FinancialAnalysis { get; set; }
        public ProjectSchedule ProjectSchedule { get; set; }
        public string TempDirectory { get; set; }
        public int CurrentPage { get; set; }
        public bool IsDebugMode { get; set; }
    }

    /// <summary>
    /// Result of PDF/A compliance validation.
    /// </summary>
    public class PdfValidationResult
    {
        public bool IsCompliant { get; set; }
        public string ComplianceLevel { get; set; } = "PDF/A-1b";
        public List<string> Errors { get; set; } = new();
        public List<string> Warnings { get; set; } = new();
        public long FileSize { get; set; }
        public int PageCount { get; set; }

        /// <summary>
        /// Checks if the PDF meets all requirements.
        /// </summary>
        public bool MeetsAllRequirements()
        {
            return IsCompliant &&
                   FileSize < 20 * 1024 * 1024 && // Less than 20MB
                   !Errors.Any();
        }
    }
}