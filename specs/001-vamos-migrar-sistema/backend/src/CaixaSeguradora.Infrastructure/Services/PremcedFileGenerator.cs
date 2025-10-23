using System.Text;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Infrastructure.Formatters;

namespace CaixaSeguradora.Infrastructure.Services;

/// <summary>
/// Generates PREMCED.TXT file records for cossured/ceded premium data.
/// Implements SUSEP Circular 360 cossurance premium report format.
/// </summary>
/// <remarks>
/// Record Layout: Fixed-width format for cossurance/ceded premium reporting
/// - Focuses on cossurance-specific fields (IC suffix in COBOL)
/// - Encoding: ISO-8859-1 (COBOL-compatible)
/// - Line Terminator: CRLF (\r\n)
///
/// Purpose: Reports premiums ceded to cossurers or reinsurers
/// Only includes records where cossurance/reinsurance amounts are non-zero
/// </remarks>
public static class PremcedFileGenerator
{
    /// <summary>
    /// Generates a single PREMCED record line from a premium record.
    /// Only generates records for policies with cossurance or reinsurance data.
    /// </summary>
    /// <param name="premium">The premium record to format</param>
    /// <returns>Fixed-width PREMCED record string, or null if no cossurance data</returns>
    /// <exception cref="ArgumentNullException">If premium is null</exception>
    public static string? GenerateRecord(PremiumRecord premium)
    {
        if (premium == null)
            throw new ArgumentNullException(nameof(premium));

        // Only generate PREMCED record if there is cossurance or reinsurance data
        // This matches COBOL logic that checks for non-zero cossurance amounts
        bool hasCossuranceData = premium.InsuredAmountCossurance != 0 ||
                                  premium.NetPremiumCossurance != 0 ||
                                  premium.InsuredAmountReinsurance != 0 ||
                                  premium.NetPremiumReinsurance != 0;

        if (!hasCossuranceData)
            return null; // Skip record - no cossurance/reinsurance to report

        var record = new StringBuilder(400); // Pre-allocate estimated size

        // Business Identifiers (same as PREMIT for record correlation)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.CompanyCode, 9));         // PIC 9(9)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.ReferenceYear, 4));       // PIC 9(4)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.ReferenceMonth, 4));      // PIC 9(4)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.ReferenceDay, 4));        // PIC 9(4)
        record.Append(FixedWidthFormatter.FormatAlphanumeric(premium.MovementType, 1));   // PIC X(1)
        record.Append(FixedWidthFormatter.FormatNumeric((long)premium.PolicyNumber, 13)); // PIC 9(13)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.EndorsementNumber, 9));   // PIC 9(9)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.InstallmentNumber, 4));   // PIC 9(4)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.OccurrenceNumber, 4));    // PIC 9(4)

        // Product Classification (for linking to master policy data)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.LineOfBusiness, 4));      // PIC 9(4)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.ProductModality, 4));     // PIC 9(4)

        // Cossurance Components (IC suffix) - Primary data for PREMCED
        record.Append(FixedWidthFormatter.FormatNumeric(premium.InsuredAmountCossurance, 16, 5));           // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.BasePremiumCossurance, 16, 5));             // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.FixedPremiumCossurance, 16, 5));            // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.TariffPremiumCossurance, 16, 5));           // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.DiscountCossurance, 16, 5));                // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.NetPremiumCossurance, 16, 5));              // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.AdditionalFractionalCossurance, 16, 5));    // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.CommissionCossurance, 16, 5));              // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.AdministrationFeeCossurance, 16, 5));       // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.AgencyCommissionCossurance, 16, 5));        // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.PreferentialCommissionCossurance, 16, 5));  // PIC 9(10)V9(5)

        // Reinsurance Components (IR suffix) - Secondary data for PREMCED
        record.Append(FixedWidthFormatter.FormatNumeric(premium.InsuredAmountReinsurance, 16, 5));        // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.TariffPremiumReinsurance, 16, 5));        // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.DiscountReinsurance, 16, 5));             // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.NetPremiumReinsurance, 16, 5));           // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.AdditionalFractionalReinsurance, 16, 5)); // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.CommissionReinsurance, 16, 5));           // PIC 9(10)V9(5)

        // Total Premium Net (for verification against sum of cossurance + reinsurance)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.NetPremiumTotal, 15, 2));  // PIC 9(13)V99

        // Total Premium Item (original gross premium for percentage calculations)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.TotalPremiumItem, 16, 5)); // PIC 9(10)V9(5)

        return record.ToString();
    }

    /// <summary>
    /// Generates complete PREMCED file content from multiple premium records.
    /// Automatically filters to only include records with cossurance/reinsurance data.
    /// </summary>
    /// <param name="premiums">Collection of premium records to format</param>
    /// <param name="encoding">File encoding (default: ISO-8859-1 for COBOL compatibility)</param>
    /// <returns>Complete file content as byte array</returns>
    public static byte[] GenerateFile(IEnumerable<PremiumRecord> premiums, Encoding? encoding = null)
    {
        if (premiums == null)
            throw new ArgumentNullException(nameof(premiums));

        encoding ??= Encoding.GetEncoding("ISO-8859-1"); // COBOL default encoding

        var fileContent = new StringBuilder();
        int recordCount = 0;

        foreach (var premium in premiums)
        {
            var recordLine = GenerateRecord(premium);

            // Only include non-null records (those with cossurance data)
            if (recordLine != null)
            {
                fileContent.AppendLine(recordLine); // Adds CRLF line terminator
                recordCount++;
            }
        }

        // If no records with cossurance data, return empty file (valid scenario)
        return encoding.GetBytes(fileContent.ToString());
    }

    /// <summary>
    /// Writes PREMCED file to disk with COBOL-compatible encoding.
    /// </summary>
    /// <param name="filePath">Full path to output file</param>
    /// <param name="premiums">Collection of premium records</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Number of records written (may be less than input if records have no cossurance data)</returns>
    public static async Task<int> WriteFileAsync(
        string filePath,
        IEnumerable<PremiumRecord> premiums,
        CancellationToken cancellationToken = default)
    {
        if (string.IsNullOrWhiteSpace(filePath))
            throw new ArgumentException("File path cannot be null or empty", nameof(filePath));

        var encoding = Encoding.GetEncoding("ISO-8859-1");
        var fileContent = new StringBuilder();
        int recordCount = 0;

        foreach (var premium in premiums)
        {
            var recordLine = GenerateRecord(premium);
            if (recordLine != null)
            {
                fileContent.AppendLine(recordLine);
                recordCount++;
            }
        }

        var fileBytes = encoding.GetBytes(fileContent.ToString());
        await File.WriteAllBytesAsync(filePath, fileBytes, cancellationToken);

        return recordCount;
    }

    /// <summary>
    /// Calculates expected record length based on COBOL field definitions.
    /// </summary>
    /// <returns>Total fixed-width record length in characters</returns>
    public static int GetExpectedRecordLength()
    {
        // Business Identifiers: 9+4+4+4+1+13+9+4+4 = 52
        // Product Classification: 4+4 = 8
        // Cossurance (11 fields × 16): 176
        // Reinsurance (6 fields × 16): 96
        // Total Premium Net: 15
        // Total Premium Item: 16
        // Total: 52+8+176+96+15+16 = 363 characters
        return 363;
    }

    /// <summary>
    /// Validates a generated record matches expected length.
    /// </summary>
    /// <param name="record">Generated record string</param>
    /// <exception cref="InvalidOperationException">If record length doesn't match expected</exception>
    public static void ValidateRecordLength(string record)
    {
        var expectedLength = GetExpectedRecordLength();
        if (record.Length != expectedLength)
        {
            throw new InvalidOperationException(
                $"PREMCED record length mismatch: expected {expectedLength}, got {record.Length}");
        }
    }

    /// <summary>
    /// Determines if a premium record should be included in PREMCED file.
    /// </summary>
    /// <param name="premium">Premium record to check</param>
    /// <returns>True if record has cossurance/reinsurance data</returns>
    public static bool ShouldIncludeInPremced(PremiumRecord premium)
    {
        if (premium == null)
            return false;

        return premium.InsuredAmountCossurance != 0 ||
               premium.NetPremiumCossurance != 0 ||
               premium.InsuredAmountReinsurance != 0 ||
               premium.NetPremiumReinsurance != 0;
    }
}
