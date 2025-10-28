using System.Threading.Tasks;
using PdfGenerator.Models;

namespace PdfGenerator.Services
{
    /// <summary>
    /// Interface for extracting data from source files
    /// </summary>
    public interface IDataExtractor
    {
        /// <summary>
        /// Extract COBOL metrics from analysis report
        /// </summary>
        Task<CobolMetrics> ExtractCobolMetricsAsync(string filePath);

        /// <summary>
        /// Extract function points from JSON file
        /// </summary>
        Task<FunctionPointAnalysis> ExtractFunctionPointsAsync(string filePath);

        /// <summary>
        /// Extract migration architecture from specification
        /// </summary>
        Task<MigrationArchitecture> ExtractArchitectureAsync(string filePath);

        /// <summary>
        /// Extract project schedule from JSON file
        /// </summary>
        Task<ProjectSchedule> ExtractScheduleAsync(string filePath);

        /// <summary>
        /// Parse markdown content and extract sections
        /// </summary>
        Task<MarkdownDocument> ParseMarkdownAsync(string filePath);

        /// <summary>
        /// Validate if all required source files exist
        /// </summary>
        Task<bool> ValidateSourceFilesAsync(params string[] filePaths);
    }

    /// <summary>
    /// Represents a parsed markdown document
    /// </summary>
    public class MarkdownDocument
    {
        public string Title { get; set; }
        public Dictionary<string, string> Sections { get; set; } = new Dictionary<string, string>();
        public List<string> Headers { get; set; } = new List<string>();
        public Dictionary<string, List<string>> Tables { get; set; } = new Dictionary<string, List<string>>();
        public List<string> CodeBlocks { get; set; } = new List<string>();
        public int WordCount { get; set; }
        public int LineCount { get; set; }
    }

    /// <summary>
    /// Project schedule for Gantt chart generation
    /// </summary>
    public class ProjectSchedule
    {
        public string ProjectName { get; set; }
        public DateTime StartDate { get; set; }
        public DateTime EndDate { get; set; }
        public int TotalWeeks { get; set; }
        public List<ProjectPhase> Phases { get; set; } = new List<ProjectPhase>();
        public List<Milestone> Milestones { get; set; } = new List<Milestone>();
    }

    public class ProjectPhase
    {
        public string Name { get; set; }
        public int StartWeek { get; set; }
        public int Duration { get; set; }
        public int EndWeek => StartWeek + Duration - 1;
        public List<ProjectTask> Tasks { get; set; } = new List<ProjectTask>();
        public string Status { get; set; } // Not Started, In Progress, Complete
        public double PercentComplete { get; set; }
        public string Color { get; set; } // For Gantt chart
    }

    public class ProjectTask
    {
        public string Name { get; set; }
        public string Phase { get; set; }
        public int StartDay { get; set; }
        public int Duration { get; set; }
        public List<string> Dependencies { get; set; } = new List<string>();
        public string Assignee { get; set; }
        public string Status { get; set; }
        public double PercentComplete { get; set; }
    }

    public class Milestone
    {
        public string Name { get; set; }
        public DateTime Date { get; set; }
        public string Description { get; set; }
        public string Status { get; set; }
        public string Icon { get; set; } // For visual representation
    }
}