using System;
using System.Collections.Generic;
using System.Linq;

namespace PdfGenerator.Models
{
    /// <summary>
    /// Function Point calculation model based on IFPUG 4.3.1
    /// </summary>
    public class FunctionPoint
    {
        public string Type { get; set; } // EI, EO, EQ, ILF, EIF
        public string Name { get; set; }
        public string Description { get; set; }
        public string Complexity { get; set; } // Low, Average, High
        public int Files { get; set; }
        public int DataElements { get; set; }
        public int Points { get; set; }

        /// <summary>
        /// Calculate function points based on IFPUG weights
        /// </summary>
        public int CalculatePoints()
        {
            var weights = GetIFPUGWeights();
            var key = $"{Type}-{Complexity}";

            if (weights.ContainsKey(key))
            {
                Points = weights[key];
                return Points;
            }

            // Default to average complexity if not found
            key = $"{Type}-Average";
            Points = weights.ContainsKey(key) ? weights[key] : 4;
            return Points;
        }

        /// <summary>
        /// IFPUG 4.3.1 complexity weights
        /// </summary>
        private static Dictionary<string, int> GetIFPUGWeights()
        {
            return new Dictionary<string, int>
            {
                // External Input (EI)
                { "EI-Low", 3 },
                { "EI-Average", 4 },
                { "EI-High", 6 },

                // External Output (EO)
                { "EO-Low", 4 },
                { "EO-Average", 5 },
                { "EO-High", 7 },

                // External Inquiry (EQ)
                { "EQ-Low", 3 },
                { "EQ-Average", 4 },
                { "EQ-High", 6 },

                // Internal Logical File (ILF)
                { "ILF-Low", 7 },
                { "ILF-Average", 10 },
                { "ILF-High", 15 },

                // External Interface File (EIF)
                { "EIF-Low", 5 },
                { "EIF-Average", 7 },
                { "EIF-High", 10 }
            };
        }

        /// <summary>
        /// Determine complexity based on data elements and files
        /// </summary>
        public string DetermineComplexity()
        {
            // Simplified complexity determination
            // Real implementation would use IFPUG matrices
            if (Type == "ILF" || Type == "EIF")
            {
                // For files: based on record types and data elements
                if (DataElements < 20 && Files < 2) return "Low";
                if (DataElements < 50 && Files < 5) return "Average";
                return "High";
            }
            else
            {
                // For transactions: based on files referenced and data elements
                if (DataElements < 5 && Files < 2) return "Low";
                if (DataElements < 15 && Files < 3) return "Average";
                return "High";
            }
        }
    }

    /// <summary>
    /// Complete function point analysis for the project
    /// </summary>
    public class FunctionPointAnalysis
    {
        public List<FunctionPoint> Functions { get; set; } = new List<FunctionPoint>();
        public int UnadjustedFunctionPoints { get; set; }
        public double ValueAdjustmentFactor { get; set; } = 1.0;
        public int AdjustedFunctionPoints { get; set; }

        // General System Characteristics (GSCs)
        public Dictionary<string, int> SystemCharacteristics { get; set; } = new Dictionary<string, int>
        {
            { "Data Communications", 5 },
            { "Distributed Data Processing", 3 },
            { "Performance", 5 },
            { "Heavily Used Configuration", 4 },
            { "Transaction Rate", 5 },
            { "Online Data Entry", 5 },
            { "End User Efficiency", 5 },
            { "Online Update", 4 },
            { "Complex Processing", 5 },
            { "Reusability", 4 },
            { "Installation Ease", 3 },
            { "Operational Ease", 4 },
            { "Multiple Sites", 2 },
            { "Facilitate Change", 4 }
        };

        /// <summary>
        /// Calculate total function points
        /// </summary>
        public void Calculate()
        {
            // Calculate unadjusted FPs
            UnadjustedFunctionPoints = Functions.Sum(f => f.CalculatePoints());

            // Calculate VAF (Value Adjustment Factor)
            int totalDI = SystemCharacteristics.Values.Sum();
            ValueAdjustmentFactor = 0.65 + (0.01 * totalDI);

            // Calculate adjusted FPs
            AdjustedFunctionPoints = (int)Math.Round(UnadjustedFunctionPoints * ValueAdjustmentFactor);
        }

        /// <summary>
        /// Get function point breakdown by type
        /// </summary>
        public Dictionary<string, FunctionPointSummary> GetBreakdownByType()
        {
            var breakdown = new Dictionary<string, FunctionPointSummary>();
            var types = new[] { "EI", "EO", "EQ", "ILF", "EIF" };

            foreach (var type in types)
            {
                var functions = Functions.Where(f => f.Type == type).ToList();
                breakdown[type] = new FunctionPointSummary
                {
                    Type = type,
                    Count = functions.Count,
                    Points = functions.Sum(f => f.Points),
                    Percentage = functions.Count > 0 ?
                        (functions.Sum(f => f.Points) * 100.0 / UnadjustedFunctionPoints) : 0
                };
            }

            return breakdown;
        }
    }

    public class FunctionPointSummary
    {
        public string Type { get; set; }
        public int Count { get; set; }
        public int Points { get; set; }
        public double Percentage { get; set; }

        public string GetTypeName()
        {
            return Type switch
            {
                "EI" => "External Input",
                "EO" => "External Output",
                "EQ" => "External Inquiry",
                "ILF" => "Internal Logical File",
                "EIF" => "External Interface File",
                _ => Type
            };
        }
    }

    /// <summary>
    /// Result of function point calculation
    /// </summary>
    public class FunctionPointResult
    {
        public int TotalFunctionPoints { get; set; }
        public int UnadjustedFunctionPoints { get; set; }
        public double ValueAdjustmentFactor { get; set; }
        public List<FunctionPoint> Functions { get; set; } = new List<FunctionPoint>();
        public Dictionary<string, FunctionPointSummary> BreakdownByType { get; set; } = new Dictionary<string, FunctionPointSummary>();
        public decimal EstimatedCost { get; set; }
        public decimal CostPerFunctionPoint { get; set; } = 750m; // BRL
    }
}