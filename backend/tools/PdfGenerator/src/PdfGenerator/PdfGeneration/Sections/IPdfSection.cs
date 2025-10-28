using QuestPDF.Fluent;
using QuestPDF.Infrastructure;
using PdfGenerator.Models;

namespace PdfGenerator.PdfGeneration.Sections;

/// <summary>
/// Interface for all PDF document sections
/// </summary>
public interface IPdfSection
{
    /// <summary>
    /// Section identifier for navigation and TOC
    /// </summary>
    string SectionId { get; }

    /// <summary>
    /// Section title for display in TOC
    /// </summary>
    string Title { get; }

    /// <summary>
    /// Order number for section sequencing
    /// </summary>
    int Order { get; }

    /// <summary>
    /// Whether this section should appear in the table of contents
    /// </summary>
    bool IncludeInToc { get; }

    /// <summary>
    /// Renders the section content using QuestPDF
    /// </summary>
    /// <param name="container">QuestPDF container to render into</param>
    /// <param name="context">Rendering context with all required data</param>
    void Render(IContainer container, SectionContext context);
}

/// <summary>
/// Context object containing all data needed by sections
/// </summary>
public class SectionContext
{
    public CobolMetrics CobolMetrics { get; set; } = new();
    public MigrationArchitecture Architecture { get; set; } = new();
    public FunctionPoint[] FunctionPoints { get; set; } = Array.Empty<FunctionPoint>();
    public ProjectSchedule Schedule { get; set; } = new();
    public FinancialAnalysis Financial { get; set; } = new();
    public Models.DocumentMetadata Metadata { get; set; } = new();
    public string OutputPath { get; set; } = "output";
}