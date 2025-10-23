namespace CaixaSeguradora.Core.DTOs;

/// <summary>
/// Dashboard metrics displaying COBOL program analysis and migration progress.
/// Data sourced from FINAL-ANALYSIS-REPORT.md and migration tracking.
/// </summary>
public class DashboardMetricsDto
{
    /// <summary>
    /// Program information section
    /// </summary>
    public ProgramInfoDto ProgramInfo { get; set; } = new();

    /// <summary>
    /// Data structure metrics
    /// </summary>
    public DataStructureMetricsDto DataStructure { get; set; } = new();

    /// <summary>
    /// Complexity metrics
    /// </summary>
    public ComplexityMetricsDto Complexity { get; set; } = new();

    /// <summary>
    /// Migration progress tracking
    /// </summary>
    public MigrationProgressDto MigrationProgress { get; set; } = new();
}

/// <summary>
/// COBOL program identification and basic information
/// </summary>
public class ProgramInfoDto
{
    /// <summary>
    /// Program name (RG1866B)
    /// </summary>
    public string ProgramName { get; set; } = "RG1866B";

    /// <summary>
    /// Program description
    /// </summary>
    public string Description { get; set; } = "SUSEP Circular 360 Premium Reporting System";

    /// <summary>
    /// Program type (Batch, Online, etc.)
    /// </summary>
    public string ProgramType { get; set; } = "Batch";

    /// <summary>
    /// Primary output files
    /// </summary>
    public List<string> OutputFiles { get; set; } = new() { "PREMIT.TXT", "PREMCED.TXT" };

    /// <summary>
    /// Total lines of COBOL code
    /// </summary>
    public int TotalLinesOfCode { get; set; }

    /// <summary>
    /// Last analysis date
    /// </summary>
    public DateTime LastAnalyzed { get; set; } = DateTime.UtcNow;
}

/// <summary>
/// Data structure analysis metrics from COBOL copybooks
/// </summary>
public class DataStructureMetricsDto
{
    /// <summary>
    /// Total number of COBOL data items (687 per analysis)
    /// </summary>
    public int TotalDataItems { get; set; } = 687;

    /// <summary>
    /// Number of working storage sections
    /// </summary>
    public int WorkingStorageSections { get; set; }

    /// <summary>
    /// Number of file sections
    /// </summary>
    public int FileSections { get; set; }

    /// <summary>
    /// Number of linkage sections
    /// </summary>
    public int LinkageSections { get; set; }

    /// <summary>
    /// Total database tables/views accessed (26+ per spec)
    /// </summary>
    public int DatabaseTables { get; set; } = 26;

    /// <summary>
    /// Number of cursor declarations (4 per spec)
    /// </summary>
    public int CursorDeclarations { get; set; } = 4;
}

/// <summary>
/// Program complexity metrics
/// </summary>
public class ComplexityMetricsDto
{
    /// <summary>
    /// Total number of sections (63 per analysis)
    /// </summary>
    public int TotalSections { get; set; } = 63;

    /// <summary>
    /// Total number of paragraphs (65 per analysis)
    /// </summary>
    public int TotalParagraphs { get; set; } = 65;

    /// <summary>
    /// Number of decision points (IF, EVALUATE, etc.)
    /// </summary>
    public int DecisionPoints { get; set; }

    /// <summary>
    /// Cyclomatic complexity estimate
    /// </summary>
    public int CyclomaticComplexity { get; set; }

    /// <summary>
    /// Number of external module calls (3 - RE0001S, GE0009S, GE0010S)
    /// </summary>
    public int ExternalCalls { get; set; } = 3;

    /// <summary>
    /// Number of SQL statements
    /// </summary>
    public int SqlStatements { get; set; }

    /// <summary>
    /// Number of file I/O operations
    /// </summary>
    public int FileOperations { get; set; }
}

/// <summary>
/// Migration progress tracking
/// </summary>
public class MigrationProgressDto
{
    /// <summary>
    /// Overall migration completion percentage (0-100)
    /// </summary>
    public decimal CompletionPercentage { get; set; }

    /// <summary>
    /// Number of tasks completed
    /// </summary>
    public int TasksCompleted { get; set; }

    /// <summary>
    /// Total number of tasks
    /// </summary>
    public int TotalTasks { get; set; } = 244;

    /// <summary>
    /// Migration status (Not Started, In Progress, Testing, Complete)
    /// </summary>
    public string Status { get; set; } = "In Progress";

    /// <summary>
    /// Current migration phase
    /// </summary>
    public string CurrentPhase { get; set; } = "Development";

    /// <summary>
    /// Last updated timestamp
    /// </summary>
    public DateTime LastUpdated { get; set; } = DateTime.UtcNow;

    /// <summary>
    /// Byte-match validation status (percentage of output files matching COBOL)
    /// </summary>
    public decimal ValidationMatchPercentage { get; set; }
}
