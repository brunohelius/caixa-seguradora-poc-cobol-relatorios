using System.ComponentModel;

namespace CaixaSeguradora.Core.Enums
{
    /// <summary>
    /// Return codes matching COBOL program exit codes.
    /// Used to signal execution outcome to calling processes or schedulers.
    /// Follows mainframe convention for batch program return codes.
    /// </summary>
    public enum ReturnCode
    {
        /// <summary>
        /// Success - No errors or warnings.
        /// All premiums processed successfully, all validations passed.
        /// Output files generated without issues.
        /// COBOL: RETURN-CODE = 0000
        /// </summary>
        [Description("Success")]
        RC_0000 = 0,

        /// <summary>
        /// Warning - Minor issues detected, less than 5% rejections.
        /// Processing completed but some records were rejected or corrected.
        /// Output files generated, but review logs for warnings.
        /// Examples: Auto-corrected proposal dates, missing optional fields.
        /// COBOL: RETURN-CODE = 0004
        /// </summary>
        [Description("Warning - minor issues, < 5% rejections")]
        RC_0004 = 4,

        /// <summary>
        /// Partial Error - More than 5% rejections.
        /// Significant data quality issues detected during processing.
        /// Output files generated but may be incomplete or contain errors.
        /// Requires investigation before SUSEP submission.
        /// Examples: Missing bilhete numbers, invalid date sequences, exceeded precision.
        /// COBOL: RETURN-CODE = 0008
        /// </summary>
        [Description("Partial error - > 5% rejections")]
        RC_0008 = 8,

        /// <summary>
        /// Fatal Error - Database unavailable or critical system failure.
        /// Processing could not complete due to infrastructure issues.
        /// No output files generated or files are corrupted.
        /// Requires immediate technical intervention.
        /// Examples: DB2 connection failure, file system full, SQLCODE -911 (deadlock).
        /// COBOL: RETURN-CODE = 0012
        /// </summary>
        [Description("Fatal error - database unavailable")]
        RC_0012 = 12,

        /// <summary>
        /// Fatal Error - Data inconsistency or integrity violation.
        /// Processing aborted due to severe data quality issues.
        /// Examples: Missing foreign keys, constraint violations, data corruption.
        /// COBOL: RETURN-CODE = 0016
        /// </summary>
        [Description("Fatal error - data inconsistency")]
        RC_0016 = 16,

        /// <summary>
        /// Configuration Error - Invalid parameters or missing configuration.
        /// Examples: Invalid reference month, missing connection string.
        /// </summary>
        [Description("Configuration error")]
        RC_0020 = 20
    }

    /// <summary>
    /// Extension methods for ReturnCode enum.
    /// </summary>
    public static class ReturnCodeExtensions
    {
        /// <summary>
        /// Gets the COBOL return code value.
        /// </summary>
        public static int ToCobolCode(this ReturnCode returnCode)
        {
            return (int)returnCode;
        }

        /// <summary>
        /// Gets the 4-character return code string (e.g., "0000", "0004").
        /// </summary>
        public static string ToCodeString(this ReturnCode returnCode)
        {
            return ((int)returnCode).ToString("D4");
        }

        /// <summary>
        /// Gets the description for the return code.
        /// </summary>
        public static string GetDescription(this ReturnCode returnCode)
        {
            var fieldInfo = returnCode.GetType().GetField(returnCode.ToString());
            var attributes = (DescriptionAttribute[])fieldInfo.GetCustomAttributes(typeof(DescriptionAttribute), false);
            return attributes.Length > 0 ? attributes[0].Description : returnCode.ToString();
        }

        /// <summary>
        /// Determines if the return code indicates success.
        /// </summary>
        public static bool IsSuccess(this ReturnCode returnCode)
        {
            return returnCode == ReturnCode.RC_0000;
        }

        /// <summary>
        /// Determines if the return code indicates a warning (processing completed with minor issues).
        /// </summary>
        public static bool IsWarning(this ReturnCode returnCode)
        {
            return returnCode == ReturnCode.RC_0004;
        }

        /// <summary>
        /// Determines if the return code indicates an error (processing failed or incomplete).
        /// </summary>
        public static bool IsError(this ReturnCode returnCode)
        {
            return (int)returnCode >= 8;
        }

        /// <summary>
        /// Determines if the return code indicates a fatal error (requires intervention).
        /// </summary>
        public static bool IsFatal(this ReturnCode returnCode)
        {
            return (int)returnCode >= 12;
        }

        /// <summary>
        /// Determines if the return code allows SUSEP file submission.
        /// </summary>
        public static bool AllowsSusepSubmission(this ReturnCode returnCode)
        {
            return returnCode == ReturnCode.RC_0000 || returnCode == ReturnCode.RC_0004;
        }

        /// <summary>
        /// Maps return code to ReportStatus.
        /// </summary>
        public static ReportStatus ToReportStatus(this ReturnCode returnCode)
        {
            return returnCode switch
            {
                ReturnCode.RC_0000 => ReportStatus.Completed,
                ReturnCode.RC_0004 => ReportStatus.Completed,
                _ => ReportStatus.Failed
            };
        }

        /// <summary>
        /// Parses a string code (e.g., "0004") to ReturnCode enum.
        /// </summary>
        public static ReturnCode FromCodeString(string code)
        {
            if (string.IsNullOrEmpty(code) || code.Length != 4)
                throw new ArgumentException("Return code must be a 4-character string", nameof(code));

            if (!int.TryParse(code, out int value))
                throw new ArgumentException($"Invalid return code format: {code}", nameof(code));

            return value switch
            {
                0 => ReturnCode.RC_0000,
                4 => ReturnCode.RC_0004,
                8 => ReturnCode.RC_0008,
                12 => ReturnCode.RC_0012,
                16 => ReturnCode.RC_0016,
                20 => ReturnCode.RC_0020,
                _ => throw new ArgumentException($"Unknown return code: {code}", nameof(code))
            };
        }

        /// <summary>
        /// Determines the appropriate return code based on error statistics.
        /// </summary>
        /// <param name="totalRecords">Total number of records processed</param>
        /// <param name="errorCount">Number of records with errors</param>
        /// <param name="warningCount">Number of records with warnings</param>
        /// <param name="fatalError">Whether a fatal error occurred</param>
        /// <returns>Appropriate return code</returns>
        public static ReturnCode DetermineReturnCode(int totalRecords, int errorCount, int warningCount, bool fatalError = false)
        {
            if (fatalError)
                return ReturnCode.RC_0012;

            if (totalRecords == 0)
                return ReturnCode.RC_0000;

            double errorRate = (double)errorCount / totalRecords;
            double warningRate = (double)warningCount / totalRecords;

            if (errorRate > 0.05) // More than 5% errors
                return ReturnCode.RC_0008;

            if (errorRate > 0 || warningRate > 0)
                return ReturnCode.RC_0004;

            return ReturnCode.RC_0000;
        }
    }
}
