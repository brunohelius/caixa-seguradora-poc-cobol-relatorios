using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using PdfGenerator.Models.ChartModels;
using ScottPlot;
using ScottPlot.Plottables;

namespace PdfGenerator.Services
{
    /// <summary>
    /// Implementation of chart generation using ScottPlot with Caixa Seguradora branding.
    /// </summary>
    public class ChartGenerator : IChartGenerator
    {
        private readonly string _tempPath;

        // Caixa Seguradora brand colors
        private readonly System.Drawing.Color _caixaBlue = System.Drawing.ColorTranslator.FromHtml("#0047BB");
        private readonly System.Drawing.Color _caixaYellow = System.Drawing.ColorTranslator.FromHtml("#FFB81C");
        private readonly System.Drawing.Color _caixaDarkBlue = System.Drawing.ColorTranslator.FromHtml("#003D7A");
        private readonly System.Drawing.Color _caixaDarkYellow = System.Drawing.ColorTranslator.FromHtml("#FF9500");
        private readonly System.Drawing.Color _caixaLightBlue = System.Drawing.ColorTranslator.FromHtml("#4B7BCD");
        private readonly System.Drawing.Color _caixaLightYellow = System.Drawing.ColorTranslator.FromHtml("#FFC94D");

        public ChartGenerator()
        {
            _tempPath = Path.Combine(Path.GetTempPath(), "PdfGenerator", "Charts");
            Directory.CreateDirectory(_tempPath);
        }

        /// <summary>
        /// Generates a Gantt chart showing project timeline and milestones.
        /// </summary>
        public async Task<string> GenerateGanttChartAsync(GanttChartData data, string outputPath)
        {
            return await Task.Run(() =>
            {
                var plot = new Plot();
                plot.Title(data.Title, size: 20);

                // Configure axes
                plot.Axes.Left.Label.Text = "Fases do Projeto";
                plot.Axes.Bottom.Label.Text = "Semanas";

                // Calculate date range
                var startDate = data.ProjectStartDate;
                var endDate = data.ProjectEndDate;
                var totalWeeks = data.GetDurationInWeeks();

                // Create bars for each phase
                var bars = new List<Bar>();
                double yPosition = 0;
                var phaseLabels = new List<string>();
                var yTicks = new List<double>();

                foreach (var phase in data.Phases.OrderBy(p => p.StartDate))
                {
                    var phaseStartWeek = (phase.StartDate - startDate).Days / 7.0;
                    var phaseDurationWeeks = phase.GetDurationInDays() / 7.0;

                    // Create filled bar for completed portion
                    if (phase.PercentComplete > 0)
                    {
                        var completedDuration = phaseDurationWeeks * (double)(phase.PercentComplete / 100m);
                        bars.Add(new Bar()
                        {
                            Position = phaseStartWeek + completedDuration / 2,
                            Value = yPosition,
                            Error = 0,
                            IsHorizontal = true,
                            FillColor = _caixaBlue,
                            Size = completedDuration
                        });
                    }

                    // Create outline bar for remaining portion
                    if (phase.PercentComplete < 100)
                    {
                        var remainingStart = phaseStartWeek + phaseDurationWeeks * (double)(phase.PercentComplete / 100m);
                        var remainingDuration = phaseDurationWeeks * (double)((100 - phase.PercentComplete) / 100m);

                        if (remainingDuration > 0)
                        {
                            bars.Add(new Bar()
                            {
                                Position = remainingStart + remainingDuration / 2,
                                Value = yPosition,
                                Error = 0,
                                IsHorizontal = true,
                                FillColor = Color.FromArgb(100, _caixaLightBlue),
                                Size = remainingDuration
                            });
                        }
                    }

                    phaseLabels.Add(phase.Name);
                    yTicks.Add(yPosition);
                    yPosition++;
                }

                // Add bars to plot
                var barPlot = plot.Add.Bars(bars);
                barPlot.Horizontal = true;

                // Add milestones as scatter points
                foreach (var milestone in data.Milestones)
                {
                    var milestoneWeek = (milestone.Date - startDate).Days / 7.0;
                    var scatter = plot.Add.Scatter(
                        new double[] { milestoneWeek },
                        new double[] { yPosition + 0.5 }
                    );
                    scatter.MarkerStyle.Shape = MarkerShape.Diamond;
                    scatter.MarkerStyle.Size = 12;
                    scatter.MarkerStyle.FillColor = milestone.IsCompleted ? _caixaYellow : _caixaDarkYellow;
                    scatter.Label = milestone.Name;
                    scatter.LegendText = milestone.Name;
                }

                // Configure Y axis with phase labels
                plot.Axes.Left.SetTicks(yTicks.ToArray(), phaseLabels.ToArray());

                // Configure X axis with week numbers
                var weekTicks = Enumerable.Range(0, totalWeeks + 1).Select(w => (double)w).ToArray();
                var weekLabels = weekTicks.Select(w => $"Sem {w:F0}").ToArray();
                plot.Axes.Bottom.SetTicks(weekTicks, weekLabels);

                // Apply branding
                ApplyBrandingToPlot(plot);

                // Save the chart
                var fileName = Path.GetFileName(outputPath) ?? "gantt-chart.png";
                var fullPath = Path.Combine(_tempPath, fileName);
                plot.SavePng(fullPath, 1200, 600);

                return fullPath;
            });
        }

        /// <summary>
        /// Generates a pie chart for displaying proportional data.
        /// </summary>
        public async Task<string> GeneratePieChartAsync(PieChartData data, string outputPath)
        {
            return await Task.Run(() =>
            {
                var plot = new Plot();
                plot.Title(data.Title, size: 20);

                if (!string.IsNullOrEmpty(data.Subtitle))
                {
                    plot.Axes.Title.Label.Text = data.Subtitle;
                }

                // Prepare pie slices
                var values = data.Segments.Select(s => (double)s.Value).ToArray();
                var labels = data.Segments.Select(s => s.Label).ToArray();

                // Apply Caixa branding colors
                data.ApplyCaixaBranding();
                var colors = data.Segments.Select(s => ColorTranslator.FromHtml(s.Color)).ToArray();

                // Create pie chart
                var pie = plot.Add.Pie(values);
                pie.SliceLabels = labels;
                pie.SliceFillColors = colors;
                pie.ShowSliceLabels = true;
                pie.ShowSliceValues = data.ShowValues;
                pie.ShowPercentages = data.ShowPercentages;

                // Handle exploded segments
                if (data.ChartType == PieChartType.Exploded)
                {
                    pie.ExplodeFraction = 0.1;
                }

                // Apply branding
                ApplyBrandingToPlot(plot);

                // Configure legend
                if (data.ShowLegend)
                {
                    plot.ShowLegend(Alignment.UpperRight);
                }

                // Save the chart
                var fileName = Path.GetFileName(outputPath) ?? "pie-chart.png";
                var fullPath = Path.Combine(_tempPath, fileName);
                plot.SavePng(fullPath, 800, 600);

                return fullPath;
            });
        }

        /// <summary>
        /// Generates a bar chart for comparative data visualization.
        /// </summary>
        public async Task<string> GenerateBarChartAsync(BarChartData data, string outputPath)
        {
            return await Task.Run(() =>
            {
                var plot = new Plot();
                plot.Title(data.Title, size: 20);

                // Configure axes labels
                plot.Axes.Left.Label.Text = data.YAxisLabel;
                plot.Axes.Bottom.Label.Text = data.XAxisLabel;

                // Apply Caixa branding colors
                data.ApplyCaixaBranding();

                // Get categories
                var categories = data.GetCategories();
                var categoryPositions = Enumerable.Range(0, categories.Count).Select(i => (double)i).ToArray();

                // Add each series
                double barWidth = 0.8 / data.Series.Count;
                double offset = -barWidth * (data.Series.Count - 1) / 2;

                for (int seriesIndex = 0; seriesIndex < data.Series.Count; seriesIndex++)
                {
                    var series = data.Series[seriesIndex];
                    var bars = new List<Bar>();

                    for (int i = 0; i < categories.Count; i++)
                    {
                        var category = categories[i];
                        var dataPoint = series.DataPoints.FirstOrDefault(dp => dp.Category == category);

                        if (dataPoint != null)
                        {
                            bars.Add(new Bar()
                            {
                                Position = i + offset + seriesIndex * barWidth,
                                Value = (double)dataPoint.Value,
                                Error = 0,
                                FillColor = ColorTranslator.FromHtml(series.Color),
                                Label = dataPoint.Label,
                                IsHorizontal = data.Orientation == BarChartOrientation.Horizontal
                            });
                        }
                    }

                    if (bars.Any())
                    {
                        var barPlot = plot.Add.Bars(bars);
                        barPlot.Label = series.Name;
                        barPlot.LegendText = series.Name;
                    }
                }

                // Set category labels
                plot.Axes.Bottom.SetTicks(categoryPositions, categories.ToArray());

                // Configure grid lines
                plot.Grid.MajorLineColor = Color.FromArgb(50, Color.Gray);
                plot.Grid.Enable = data.ShowGridLines;

                // Apply branding
                ApplyBrandingToPlot(plot);

                // Show legend if requested
                if (data.ShowLegend && data.Series.Count > 1)
                {
                    plot.ShowLegend(Alignment.UpperRight);
                }

                // Save the chart
                var fileName = Path.GetFileName(outputPath) ?? "bar-chart.png";
                var fullPath = Path.Combine(_tempPath, fileName);
                plot.SavePng(fullPath, 1000, 600);

                return fullPath;
            });
        }

        /// <summary>
        /// Applies Caixa Seguradora branding colors to all generated charts.
        /// </summary>
        public void ApplyBranding()
        {
            // This method is called by individual chart generation methods
            // Branding is applied automatically
        }

        /// <summary>
        /// Apply consistent branding to a plot
        /// </summary>
        private void ApplyBrandingToPlot(Plot plot)
        {
            // Set background colors
            plot.Style.FigureBackground.Color = Color.White;
            plot.Style.DataBackground.Color = Color.FromArgb(250, 250, 250);

            // Set font styles
            plot.Axes.Title.Label.FontSize = 18;
            plot.Axes.Title.Label.Bold = true;
            plot.Axes.Title.Label.ForeColor = _caixaDarkBlue;

            plot.Axes.Left.Label.ForeColor = _caixaDarkBlue;
            plot.Axes.Left.Label.FontSize = 12;

            plot.Axes.Bottom.Label.ForeColor = _caixaDarkBlue;
            plot.Axes.Bottom.Label.FontSize = 12;

            // Set grid style
            plot.Grid.MajorLineColor = Color.FromArgb(30, _caixaBlue);
            plot.Grid.MinorLineColor = Color.FromArgb(15, _caixaBlue);

            // Set axis colors
            plot.Axes.Color(_caixaDarkBlue);
        }
    }
}