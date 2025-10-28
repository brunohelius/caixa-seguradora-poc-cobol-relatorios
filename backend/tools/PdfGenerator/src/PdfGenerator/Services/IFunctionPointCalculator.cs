using System.Collections.Generic;
using System.Threading.Tasks;
using PdfGenerator.Models;

namespace PdfGenerator.Services
{
    /// <summary>
    /// Interface for calculating function points using IFPUG methodology.
    /// Implements IFPUG 4.3.1 standard for function point analysis.
    /// </summary>
    public interface IFunctionPointCalculator
    {
        /// <summary>
        /// Calculates the total function points for a system based on IFPUG methodology.
        /// </summary>
        /// <param name="functionPoints">Collection of function point items to calculate</param>
        /// <returns>Total adjusted function points</returns>
        Task<FunctionPointResult> CalculateFunctionPointsAsync(IEnumerable<FunctionPoint> functionPoints);

        /// <summary>
        /// Calculates the financial analysis based on function points.
        /// Uses standard rate of R$ 750 per function point.
        /// </summary>
        /// <param name="totalFunctionPoints">Total calculated function points</param>
        /// <param name="schedule">Project schedule for payment milestone distribution</param>
        /// <returns>Complete financial analysis with payment schedule</returns>
        Task<FinancialAnalysis> CalculateFinancialAnalysisAsync(decimal totalFunctionPoints, ProjectSchedule schedule);

        /// <summary>
        /// Validates function points against IFPUG standards.
        /// </summary>
        /// <param name="functionPoint">Function point to validate</param>
        /// <returns>Validation result with any warnings or errors</returns>
        ValidationResult ValidateFunctionPoint(FunctionPoint functionPoint);

        /// <summary>
        /// Gets the IFPUG complexity weight for a given function type and complexity.
        /// </summary>
        /// <param name="functionType">Type of function (ILF, EIF, EI, EO, EQ)</param>
        /// <param name="complexity">Complexity level (Low, Average, High)</param>
        /// <returns>IFPUG weight value</returns>
        int GetIFPUGWeight(FunctionType functionType, ComplexityLevel complexity);

        /// <summary>
        /// Calculates the Value Adjustment Factor (VAF) based on 14 General System Characteristics.
        /// </summary>
        /// <param name="characteristics">Dictionary of GSC ratings (0-5)</param>
        /// <returns>VAF value (typically between 0.65 and 1.35)</returns>
        decimal CalculateValueAdjustmentFactor(Dictionary<string, int> characteristics);
    }

    /// <summary>
    /// Result of function point calculation
    /// </summary>
    public class FunctionPointResult
    {
        public decimal UnadjustedFunctionPoints { get; set; }
        public decimal ValueAdjustmentFactor { get; set; }
        public decimal AdjustedFunctionPoints { get; set; }
        public Dictionary<FunctionType, FunctionTypeBreakdown> Breakdown { get; set; } = new();
        public List<string> Warnings { get; set; } = new();
    }

    /// <summary>
    /// Breakdown of function points by type
    /// </summary>
    public class FunctionTypeBreakdown
    {
        public FunctionType Type { get; set; }
        public int Count { get; set; }
        public decimal TotalPoints { get; set; }
        public Dictionary<ComplexityLevel, int> ComplexityDistribution { get; set; } = new();
    }

    /// <summary>
    /// Function point validation result
    /// </summary>
    public class FunctionPointValidationResult
    {
        public bool IsValid { get; set; }
        public List<string> Errors { get; set; } = new();
        public List<string> Warnings { get; set; } = new();
    }

    /// <summary>
    /// Function types according to IFPUG
    /// </summary>
    public enum FunctionType
    {
        ILF,  // Internal Logical File
        EIF,  // External Interface File
        EI,   // External Input
        EO,   // External Output
        EQ    // External Inquiry
    }

    /// <summary>
    /// Complexity levels
    /// </summary>
    public enum ComplexityLevel
    {
        Low,
        Average,
        High
    }
}