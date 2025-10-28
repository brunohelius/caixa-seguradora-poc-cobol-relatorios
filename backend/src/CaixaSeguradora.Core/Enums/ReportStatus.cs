using System.ComponentModel;

namespace CaixaSeguradora.Core.Enums
{
    /// <summary>
    /// Report execution status values.
    /// Tracks the state of a report generation execution.
    /// </summary>
    public enum ReportStatus
    {
        /// <summary>
        /// Report execution has been created but not started.
        /// Initial state when generation request is received.
        /// </summary>
        [Description("Pendente")]
        Pending = 0,

        /// <summary>
        /// Report generation is currently in progress.
        /// Processing premiums, applying calculations, validating business rules.
        /// </summary>
        [Description("Processando")]
        Running = 1,

        /// <summary>
        /// Report generation completed successfully.
        /// Output files (PREMIT.TXT, PREMCED.TXT) have been generated.
        /// Return code: 0000 (success) or 0004 (warnings).
        /// </summary>
        [Description("Concluído")]
        Completed = 2,

        /// <summary>
        /// Report generation failed with errors.
        /// No output files generated or files are incomplete.
        /// Return code: 0008 (partial error) or 0012 (fatal error).
        /// </summary>
        [Description("Falha")]
        Failed = 3,

        /// <summary>
        /// Report execution was cancelled by user.
        /// Processing interrupted before completion.
        /// </summary>
        [Description("Cancelado")]
        Cancelled = 4
    }

    /// <summary>
    /// Extension methods for ReportStatus enum.
    /// </summary>
    public static class ReportStatusExtensions
    {
        /// <summary>
        /// Gets the Portuguese description for the status.
        /// </summary>
        public static string GetDescription(this ReportStatus status)
        {
            var fieldInfo = status.GetType().GetField(status.ToString());
            var attributes = (DescriptionAttribute[])fieldInfo.GetCustomAttributes(typeof(DescriptionAttribute), false);
            return attributes.Length > 0 ? attributes[0].Description : status.ToString();
        }

        /// <summary>
        /// Determines if the status represents a terminal state (execution finished).
        /// </summary>
        public static bool IsTerminal(this ReportStatus status)
        {
            return status == ReportStatus.Completed || status == ReportStatus.Failed || status == ReportStatus.Cancelled;
        }

        /// <summary>
        /// Determines if the status represents an active processing state.
        /// </summary>
        public static bool IsActive(this ReportStatus status)
        {
            return status == ReportStatus.Running;
        }

        /// <summary>
        /// Determines if the status allows file download.
        /// </summary>
        public static bool AllowsDownload(this ReportStatus status)
        {
            return status == ReportStatus.Completed;
        }

        /// <summary>
        /// Gets the CSS color class for UI display.
        /// </summary>
        public static string GetColorClass(this ReportStatus status)
        {
            return status switch
            {
                ReportStatus.Pending => "text-gray-500",
                ReportStatus.Running => "text-blue-500",
                ReportStatus.Completed => "text-green-500",
                ReportStatus.Failed => "text-red-500",
                ReportStatus.Cancelled => "text-yellow-500",
                _ => "text-gray-500"
            };
        }
    }
}
