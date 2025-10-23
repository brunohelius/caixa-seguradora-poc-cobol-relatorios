namespace CaixaSeguradora.Core.DTOs;

/// <summary>
/// Function points analysis for project estimation and complexity assessment.
/// Based on IFPUG (International Function Point Users Group) counting practices.
/// </summary>
public class FunctionPointsDto
{
    /// <summary>
    /// External Inputs (EI) - data entry, control inputs from user
    /// </summary>
    public FunctionPointCategoryDto ExternalInputs { get; set; } = new();

    /// <summary>
    /// External Outputs (EO) - reports, files, graphs generated
    /// </summary>
    public FunctionPointCategoryDto ExternalOutputs { get; set; } = new();

    /// <summary>
    /// External Inquiries (EQ) - queries with input and output
    /// </summary>
    public FunctionPointCategoryDto ExternalInquiries { get; set; } = new();

    /// <summary>
    /// Internal Logical Files (ILF) - data maintained by application
    /// </summary>
    public FunctionPointCategoryDto InternalLogicalFiles { get; set; } = new();

    /// <summary>
    /// External Interface Files (EIF) - data referenced but not maintained
    /// </summary>
    public FunctionPointCategoryDto ExternalInterfaceFiles { get; set; } = new();

    /// <summary>
    /// Total unadjusted function points
    /// </summary>
    public int TotalUnadjustedFunctionPoints { get; set; }

    /// <summary>
    /// Value adjustment factor (0.65 to 1.35 based on 14 general characteristics)
    /// </summary>
    public decimal ValueAdjustmentFactor { get; set; } = 1.0m;

    /// <summary>
    /// Total adjusted function points (UFP * VAF)
    /// </summary>
    public decimal TotalAdjustedFunctionPoints { get; set; }

    /// <summary>
    /// Estimated development effort in person-months
    /// </summary>
    public decimal EstimatedEffortMonths { get; set; }

    /// <summary>
    /// Project complexity rating
    /// </summary>
    public string ComplexityRating { get; set; } = "Medium";

    /// <summary>
    /// Breakdown by module/section
    /// </summary>
    public List<ModuleFunctionPointsDto> ModuleBreakdown { get; set; } = new();
}

/// <summary>
/// Function point category details with complexity breakdown
/// </summary>
public class FunctionPointCategoryDto
{
    /// <summary>
    /// Category name
    /// </summary>
    public string CategoryName { get; set; } = string.Empty;

    /// <summary>
    /// Number of low complexity items
    /// </summary>
    public int LowComplexityCount { get; set; }

    /// <summary>
    /// Number of average complexity items
    /// </summary>
    public int AverageComplexityCount { get; set; }

    /// <summary>
    /// Number of high complexity items
    /// </summary>
    public int HighComplexityCount { get; set; }

    /// <summary>
    /// Total count for this category
    /// </summary>
    public int TotalCount => LowComplexityCount + AverageComplexityCount + HighComplexityCount;

    /// <summary>
    /// Function points for low complexity items
    /// </summary>
    public int LowComplexityPoints { get; set; }

    /// <summary>
    /// Function points for average complexity items
    /// </summary>
    public int AverageComplexityPoints { get; set; }

    /// <summary>
    /// Function points for high complexity items
    /// </summary>
    public int HighComplexityPoints { get; set; }

    /// <summary>
    /// Total function points for this category
    /// </summary>
    public int TotalPoints => LowComplexityPoints + AverageComplexityPoints + HighComplexityPoints;
}

/// <summary>
/// Function points breakdown by module or section
/// </summary>
public class ModuleFunctionPointsDto
{
    /// <summary>
    /// Module/section name
    /// </summary>
    public string ModuleName { get; set; } = string.Empty;

    /// <summary>
    /// Function points for this module
    /// </summary>
    public int FunctionPoints { get; set; }

    /// <summary>
    /// Module complexity (Low, Medium, High)
    /// </summary>
    public string Complexity { get; set; } = "Medium";

    /// <summary>
    /// Estimated effort in person-hours
    /// </summary>
    public decimal EstimatedHours { get; set; }

    /// <summary>
    /// Migration status (Not Started, In Progress, Complete)
    /// </summary>
    public string Status { get; set; } = "Not Started";
}
