using System.Collections.Generic;
using System.Linq;

namespace PdfGenerator.Models.ChartModels
{
    /// <summary>
    /// Represents data for generating a pie chart.
    /// </summary>
    public class PieChartData
    {
        public string Title { get; set; } = string.Empty;
        public string Subtitle { get; set; } = string.Empty;
        public List<PieSegment> Segments { get; set; } = new();
        public bool ShowPercentages { get; set; } = true;
        public bool ShowLegend { get; set; } = true;
        public bool ShowValues { get; set; } = false;
        public string ValueFormat { get; set; } = "N0"; // Number format
        public PieChartType ChartType { get; set; } = PieChartType.Standard;

        /// <summary>
        /// Calculate the total value across all segments
        /// </summary>
        public decimal GetTotalValue()
        {
            return Segments?.Sum(s => s.Value) ?? 0;
        }

        /// <summary>
        /// Get segment percentage of total
        /// </summary>
        public decimal GetSegmentPercentage(PieSegment segment)
        {
            var total = GetTotalValue();
            if (total == 0) return 0;
            return (segment.Value / total) * 100;
        }

        /// <summary>
        /// Sort segments by value (descending)
        /// </summary>
        public void SortByValue()
        {
            if (Segments != null)
            {
                Segments = Segments.OrderByDescending(s => s.Value).ToList();
            }
        }

        /// <summary>
        /// Apply Caixa Seguradora color palette to segments
        /// </summary>
        public void ApplyCaixaBranding()
        {
            var caixaColors = new[]
            {
                "#0047BB", // Primary blue
                "#FFB81C", // Secondary yellow
                "#003D7A", // Dark blue
                "#FF9500", // Dark yellow
                "#002654", // Darker blue
                "#CC7700", // Darker yellow
                "#4B7BCD", // Light blue
                "#FFC94D"  // Light yellow
            };

            if (Segments != null)
            {
                for (int i = 0; i < Segments.Count; i++)
                {
                    Segments[i].Color = caixaColors[i % caixaColors.Length];
                }
            }
        }
    }

    /// <summary>
    /// Represents a single segment in the pie chart
    /// </summary>
    public class PieSegment
    {
        public string Label { get; set; } = string.Empty;
        public decimal Value { get; set; }
        public string Color { get; set; } = "#0047BB";
        public string Description { get; set; } = string.Empty;
        public bool IsHighlighted { get; set; }
        public decimal ExplodeOffset { get; set; } = 0; // For exploded pie charts

        /// <summary>
        /// Format the value for display based on the specified format
        /// </summary>
        public string GetFormattedValue(string format = "N0")
        {
            return Value.ToString(format);
        }

        /// <summary>
        /// Get formatted percentage string
        /// </summary>
        public string GetFormattedPercentage(decimal totalValue)
        {
            if (totalValue == 0) return "0%";
            var percentage = (Value / totalValue) * 100;
            return $"{percentage:N1}%";
        }
    }

    /// <summary>
    /// Type of pie chart
    /// </summary>
    public enum PieChartType
    {
        Standard,       // Regular pie chart
        Donut,         // Donut chart with hollow center
        Exploded,      // Pie chart with separated segments
        ThreeD         // 3D perspective pie chart
    }
}