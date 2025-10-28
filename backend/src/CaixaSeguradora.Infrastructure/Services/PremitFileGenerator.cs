using System.Text;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Infrastructure.Formatters;

namespace CaixaSeguradora.Infrastructure.Services;

/// <summary>
/// Generates PREMIT.TXT file records with byte-for-byte COBOL compatibility.
/// Implements SUSEP Circular 360 premium emission report format.
/// </summary>
/// <remarks>
/// Record Layout: Fixed-width format matching COBOL RG1866B output
/// - Total Record Length: Calculated from all field widths
/// - Encoding: ISO-8859-1 (COBOL-compatible)
/// - Line Terminator: CRLF (\r\n) to match COBOL WRITE behavior
///
/// Field Order: Matches COBOL WRITE statement order in section R0700-00-PROCESSA-REGISTRO
/// </remarks>
public static class PremitFileGenerator
{
    /// <summary>
    /// Generates a single PREMIT record line from a premium record.
    /// </summary>
    /// <param name="premium">The premium record to format</param>
    /// <returns>Fixed-width PREMIT record string</returns>
    /// <exception cref="ArgumentNullException">If premium is null</exception>
    public static string GenerateRecord(PremiumRecord premium)
    {
        if (premium == null)
        {
            throw new ArgumentNullException(nameof(premium));
        }

        var record = new StringBuilder(500); // Pre-allocate estimated size for performance

        // Business Identifiers (COBOL: IDENTIFICATION DIVISION equivalent fields)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.CompanyCode, 9));         // PIC 9(9)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.ReferenceYear, 4));       // PIC 9(4)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.ReferenceMonth, 4));      // PIC 9(4)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.ReferenceDay, 4));        // PIC 9(4)
        record.Append(FixedWidthFormatter.FormatAlphanumeric(premium.MovementType, 1));   // PIC X(1)
        record.Append(FixedWidthFormatter.FormatNumeric((long)premium.PolicyNumber, 13)); // PIC 9(13)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.EndorsementNumber, 9));   // PIC 9(9)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.InstallmentNumber, 4));   // PIC 9(4)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.OccurrenceNumber, 4));    // PIC 9(4)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.HistoricalOccurrence, 4)); // PIC 9(4)

        // Product Classification
        record.Append(FixedWidthFormatter.FormatNumeric(premium.LineOfBusiness, 4));      // PIC 9(4)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.ProductModality, 4));     // PIC 9(4)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.OperationType, 4));       // PIC 9(4)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.BusinessOperationType, 4)); // PIC 9(4)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.ClientCode, 9));          // PIC 9(9)

        // Currency & Exchange Rate
        record.Append(FixedWidthFormatter.FormatNumeric(premium.ExchangeRate, 16, 9));    // PIC 9(6)V9(9)

        // Premium Components - Installment (Item) - "IT" suffix in COBOL
        record.Append(FixedWidthFormatter.FormatNumeric(premium.InsuredAmountItem, 16, 5));        // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.BasePremiumItem, 16, 5));          // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.FixedPremiumItem, 16, 5));         // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.TariffPremiumItem, 16, 5));        // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.DiscountItem, 16, 5));             // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.NetPremiumItem, 16, 5));           // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.AdditionalFractionalItem, 16, 5)); // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.IssuanceCostItem, 16, 5));         // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.IofItem, 16, 5));                  // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.TotalPremiumItem, 16, 5));         // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.CommissionItem, 16, 5));           // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.AdministrationFeeItem, 16, 5));    // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.AgencyCommissionItem, 16, 5));     // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.PreferentialCommissionItem, 16, 5)); // PIC 9(10)V9(5)

        // Premium Components - Net (Liquido) - "IL" suffix in COBOL
        record.Append(FixedWidthFormatter.FormatNumeric(premium.InsuredAmountNet, 16, 5));        // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.BasePremiumNet, 16, 5));          // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.FixedPremiumNet, 16, 5));         // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.TariffPremiumNet, 16, 5));        // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.DiscountNet, 16, 5));             // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.NetPremiumNet, 16, 5));           // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.AdditionalFractionalNet, 16, 5)); // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.IssuanceCostNet, 16, 5));         // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.IofNet, 16, 5));                  // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.TotalPremiumNet, 16, 5));         // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.CommissionNet, 16, 5));           // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.AdministrationFeeNet, 16, 5));    // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.AgencyCommissionNet, 16, 5));     // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.PreferentialCommissionNet, 16, 5)); // PIC 9(10)V9(5)

        // Premium Components - Cossurance (Cosseguro) - "IC" suffix in COBOL
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

        // Premium Components - Reinsurance - "IR" suffix in COBOL
        record.Append(FixedWidthFormatter.FormatNumeric(premium.InsuredAmountReinsurance, 16, 5));        // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.TariffPremiumReinsurance, 16, 5));        // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.DiscountReinsurance, 16, 5));             // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.NetPremiumReinsurance, 16, 5));           // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.AdditionalFractionalReinsurance, 16, 5)); // PIC 9(10)V9(5)
        record.Append(FixedWidthFormatter.FormatNumeric(premium.CommissionReinsurance, 16, 5));           // PIC 9(10)V9(5)

        // Premium Totals - "T" suffix in COBOL
        record.Append(FixedWidthFormatter.FormatNumeric(premium.InsuredAmountTotal, 15, 2));           // PIC 9(13)V99
        record.Append(FixedWidthFormatter.FormatNumeric(premium.BasePremiumTotal, 15, 2));             // PIC 9(13)V99
        record.Append(FixedWidthFormatter.FormatNumeric(premium.FixedPremiumTotal, 15, 2));            // PIC 9(13)V99
        record.Append(FixedWidthFormatter.FormatNumeric(premium.TariffPremiumTotal, 15, 2));           // PIC 9(13)V99
        record.Append(FixedWidthFormatter.FormatNumeric(premium.DiscountTotal, 15, 2));                // PIC 9(13)V99
        record.Append(FixedWidthFormatter.FormatNumeric(premium.NetPremiumTotal, 15, 2));              // PIC 9(13)V99
        record.Append(FixedWidthFormatter.FormatNumeric(premium.AdditionalFractionalTotal, 15, 2));    // PIC 9(13)V99
        record.Append(FixedWidthFormatter.FormatNumeric(premium.IssuanceCostTotal, 15, 2));            // PIC 9(13)V99
        record.Append(FixedWidthFormatter.FormatNumeric(premium.IofTotal, 15, 2));                     // PIC 9(13)V99
        record.Append(FixedWidthFormatter.FormatNumeric(premium.TotalPremiumTotal, 15, 2));            // PIC 9(13)V99
        record.Append(FixedWidthFormatter.FormatNumeric(premium.CommissionTotal, 15, 2));              // PIC 9(13)V99
        record.Append(FixedWidthFormatter.FormatNumeric(premium.AdministrationFeeTotal, 15, 2));       // PIC 9(13)V99
        record.Append(FixedWidthFormatter.FormatNumeric(premium.AgencyCommissionTotal, 15, 2));        // PIC 9(13)V99
        record.Append(FixedWidthFormatter.FormatNumeric(premium.PreferentialCommissionTotal, 15, 2));  // PIC 9(13)V99

        // Net Local Currency Totals - "L" suffix in COBOL
        record.Append(FixedWidthFormatter.FormatNumeric(premium.InsuredAmountLocalTotal, 15, 2));    // PIC 9(13)V99
        record.Append(FixedWidthFormatter.FormatNumeric(premium.BasePremiumLocalTotal, 15, 2));      // PIC 9(13)V99
        record.Append(FixedWidthFormatter.FormatNumeric(premium.FixedPremiumLocalTotal, 15, 2));     // PIC 9(13)V99

        return record.ToString();
    }

    /// <summary>
    /// Generates complete PREMIT file content from multiple premium records.
    /// </summary>
    /// <param name="premiums">Collection of premium records to format</param>
    /// <param name="encoding">File encoding (default: ISO-8859-1 for COBOL compatibility)</param>
    /// <returns>Complete file content as byte array</returns>
    public static byte[] GenerateFile(IEnumerable<PremiumRecord> premiums, Encoding? encoding = null)
    {
        if (premiums == null)
        {
            throw new ArgumentNullException(nameof(premiums));
        }

        encoding ??= Encoding.GetEncoding("ISO-8859-1"); // COBOL default encoding

        var fileContent = new StringBuilder();

        foreach (PremiumRecord premium in premiums)
        {
            var recordLine = GenerateRecord(premium);
            fileContent.AppendLine(recordLine); // Adds CRLF line terminator matching COBOL WRITE
        }

        return encoding.GetBytes(fileContent.ToString());
    }

    /// <summary>
    /// Writes PREMIT file to disk with COBOL-compatible encoding.
    /// </summary>
    /// <param name="filePath">Full path to output file</param>
    /// <param name="premiums">Collection of premium records</param>
    /// <param name="cancellationToken">Cancellation token</param>
    public static async Task WriteFileAsync(
        string filePath,
        IEnumerable<PremiumRecord> premiums,
        CancellationToken cancellationToken = default)
    {
        if (string.IsNullOrWhiteSpace(filePath))
        {
            throw new ArgumentException("File path cannot be null or empty", nameof(filePath));
        }

        var premiumsList = premiums.ToList(); // Materialize for count
        var fileBytes = GenerateFile(premiumsList);
        var expectedSize = fileBytes.Length;

        try
        {
            // Ensure directory exists
            var directory = Path.GetDirectoryName(filePath);
            if (!string.IsNullOrEmpty(directory))
            {
                Directory.CreateDirectory(directory);
            }

            // Write to temp file first (atomic operation)
            var tempFile = filePath + ".tmp";
            await File.WriteAllBytesAsync(tempFile, fileBytes, cancellationToken);

            // Verify temp file size
            var fileInfo = new FileInfo(tempFile);
            if (!fileInfo.Exists)
            {
                throw new IOException($"Arquivo temporário não foi criado: {tempFile}");
            }

            if (fileInfo.Length != expectedSize)
            {
                try { File.Delete(tempFile); } catch { }
                throw new IOException(
                    $"Arquivo truncado: esperado {expectedSize} bytes, escrito {fileInfo.Length} bytes. Disco cheio?");
            }

            // Atomic move (replaces existing file)
            File.Move(tempFile, filePath, overwrite: true);

            // Final verification
            fileInfo = new FileInfo(filePath);
            if (fileInfo.Length != expectedSize)
            {
                throw new IOException($"Verificação final falhou: arquivo {filePath} tem tamanho incorreto");
            }
        }
        catch (IOException ioEx)
        {
            throw new IOException($"Erro ao escrever arquivo PREMIT: {ioEx.Message}. Verifique espaço em disco e permissões.", ioEx);
        }
    }

    /// <summary>
    /// Calculates expected record length based on COBOL field definitions.
    /// Useful for validation and testing.
    /// </summary>
    /// <returns>Total fixed-width record length in characters</returns>
    public static int GetExpectedRecordLength()
    {
        // Business Identifiers: 9+4+4+4+1+13+9+4+4+4 = 56
        // Product Classification: 4+4+4+4+9 = 25
        // Currency: 16 = 16
        // Item (14 fields × 16): 224
        // Net (14 fields × 16): 224
        // Cossurance (11 fields × 16): 176
        // Reinsurance (6 fields × 16): 96
        // Totals (14 fields × 15): 210
        // Local Totals (3 fields × 15): 45
        // Total: 56+25+16+224+224+176+96+210+45 = 1072 characters
        return 1072;
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
                $"PREMIT record length mismatch: expected {expectedLength}, got {record.Length}");
        }
    }
}
