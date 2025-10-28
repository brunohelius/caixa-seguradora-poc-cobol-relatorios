using System.ComponentModel.DataAnnotations;

namespace CaixaSeguradora.Core.DTOs
{
    /// <summary>
    /// Simplified request DTO for Phase 3 report generation.
    /// Accepts month in YYYYMM format for monthly report generation.
    /// </summary>
    public class GenerateReportRequest
    {
        /// <summary>
        /// Reference month in YYYYMM format (e.g., "202510" for October 2025).
        /// Must be a valid month and cannot be a future month.
        /// </summary>
        /// <example>202510</example>
        [Required(ErrorMessage = "Mês de referência é obrigatório")]
        [RegularExpression(@"^\d{6}$", ErrorMessage = "Mês deve estar no formato YYYYMM")]
        public string Month { get; set; } = string.Empty;

        /// <summary>
        /// Report type to generate.
        /// Valid values: PREMIT, PREMCED, Both
        /// Default: Both
        /// </summary>
        [Required(ErrorMessage = "Tipo de relatório é obrigatório")]
        public string ReportType { get; set; } = "Both";

        /// <summary>
        /// Execution mode for processing.
        /// Valid values: Monthly, Weekly
        /// Default: Monthly
        /// </summary>
        public string ExecutionMode { get; set; } = "Monthly";

        /// <summary>
        /// User initiating the report generation.
        /// </summary>
        public string? TriggeringUser { get; set; }
    }
}
