using System.Threading.Tasks;

namespace PdfGenerator.Services
{
    /// <summary>
    /// Interface for generating various types of charts for the PDF report.
    /// All charts must follow Caixa Seguradora branding guidelines.
    /// </summary>
    public interface IChartGenerator
    {
        /// <summary>
        /// Generates a Gantt chart showing project timeline and milestones.
        /// </summary>
        /// <param name="data">The Gantt chart data containing phases, tasks, and milestones</param>
        /// <param name="outputPath">Path to save the generated chart image</param>
        /// <returns>The path to the generated chart image file</returns>
        Task<string> GenerateGanttChartAsync(Models.ChartModels.GanttChartData data, string outputPath);

        /// <summary>
        /// Generates a pie chart for displaying proportional data.
        /// Typically used for function point distribution or cost breakdown.
        /// </summary>
        /// <param name="data">The pie chart data containing segments and values</param>
        /// <param name="outputPath">Path to save the generated chart image</param>
        /// <returns>The path to the generated chart image file</returns>
        Task<string> GeneratePieChartAsync(Models.ChartModels.PieChartData data, string outputPath);

        /// <summary>
        /// Generates a bar chart for comparative data visualization.
        /// Used for comparing metrics across different components or phases.
        /// </summary>
        /// <param name="data">The bar chart data containing categories and values</param>
        /// <param name="outputPath">Path to save the generated chart image</param>
        /// <returns>The path to the generated chart image file</returns>
        Task<string> GenerateBarChartAsync(Models.ChartModels.BarChartData data, string outputPath);

        /// <summary>
        /// Applies Caixa Seguradora branding colors to all generated charts.
        /// Primary: #0047BB (blue), Secondary: #FFB81C (yellow)
        /// </summary>
        void ApplyBranding();
    }
}