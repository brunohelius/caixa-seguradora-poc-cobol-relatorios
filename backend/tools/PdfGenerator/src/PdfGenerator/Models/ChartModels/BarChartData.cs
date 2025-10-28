using System.Collections.Generic;
using System.Linq;

namespace PdfGenerator.Models.ChartModels
{
    /// <summary>
    /// Represents data for generating a bar chart.
    /// </summary>
    public class BarChartData
    {
        public string Title { get; set; } = string.Empty;
        public string Subtitle { get; set; } = string.Empty;
        public string XAxisLabel { get; set; } = string.Empty;
        public string YAxisLabel { get; set; } = string.Empty;
        public List<BarSeries> Series { get; set; } = new();
        public BarChartOrientation Orientation { get; set; } = BarChartOrientation.Vertical;
        public bool ShowLegend { get; set; } = true;
        public bool ShowValues { get; set; } = true;
        public bool ShowGridLines { get; set; } = true;
        public string ValueFormat { get; set; } = "N0";
        public decimal? MaxValue { get; set; }
        public decimal? MinValue { get; set; }

        /// <summary>
        /// Get all unique categories across all series
        /// </summary>
        public List<string> GetCategories()
        {
            return Series?.SelectMany(s => s.DataPoints)
                         .Select(dp => dp.Category)
                         .Distinct()
                         .ToList() ?? new List<string>();
        }

        /// <summary>
        /// Get the maximum value across all series
        /// </summary>
        public decimal GetMaxValue()
        {
            if (MaxValue.HasValue) return MaxValue.Value;
            return Series?.SelectMany(s => s.DataPoints)
                         .Max(dp => dp.Value) ?? 0;
        }

        /// <summary>
        /// Get the minimum value across all series
        /// </summary>
        public decimal GetMinValue()
        {
            if (MinValue.HasValue) return MinValue.Value;
            return Series?.SelectMany(s => s.DataPoints)
                         .Min(dp => dp.Value) ?? 0;
        }

        /// <summary>
        /// Apply Caixa Seguradora color palette to series
        /// </summary>
        public void ApplyCaixaBranding()
        {
            var caixaColors = new[]
            {
                "#0047BB", // Primary blue
                "#FFB81C", // Secondary yellow
                "#003D7A", // Dark blue
                "#FF9500", // Dark yellow
                "#4B7BCD", // Light blue
                "#FFC94D"  // Light yellow
            };

            if (Series != null)
            {
                for (int i = 0; i < Series.Count; i++)
                {
                    Series[i].Color = caixaColors[i % caixaColors.Length];
                }
            }
        }
    }

    /// <summary>
    /// Represents a data series in the bar chart
    /// </summary>
    public class BarSeries
    {
        public string Name { get; set; } = string.Empty;
        public string Color { get; set; } = "#0047BB";
        public List<BarDataPoint> DataPoints { get; set; } = new();
        public bool IsVisible { get; set; } = true;
        public BarStyle Style { get; set; } = BarStyle.Solid;

        /// <summary>
        /// Get the total sum of all data points in this series
        /// </summary>
        public decimal GetTotal()
        {
            return DataPoints?.Sum(dp => dp.Value) ?? 0;
        }

        /// <summary>
        /// Get the average value of all data points in this series
        /// </summary>
        public decimal GetAverage()
        {
            if (DataPoints == null || DataPoints.Count == 0) return 0;
            return DataPoints.Average(dp => dp.Value);
        }
    }

    /// <summary>
    /// Represents a single data point in a bar chart series
    /// </summary>
    public class BarDataPoint
    {
        public string Category { get; set; } = string.Empty;
        public decimal Value { get; set; }
        public string Label { get; set; } = string.Empty;
        public string Tooltip { get; set; } = string.Empty;
        public bool IsHighlighted { get; set; }

        /// <summary>
        /// Format the value for display
        /// </summary>
        public string GetFormattedValue(string format = "N0")
        {
            return Value.ToString(format);
        }
    }

    /// <summary>
    /// Bar chart orientation
    /// </summary>
    public enum BarChartOrientation
    {
        Vertical,   // Traditional column chart
        Horizontal  // Horizontal bar chart
    }

    /// <summary>
    /// Bar style
    /// </summary>
    public enum BarStyle
    {
        Solid,      // Solid color fill
        Gradient,   // Gradient fill
        Pattern     // Pattern fill
    }
}