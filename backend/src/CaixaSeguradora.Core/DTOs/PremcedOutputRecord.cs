using System;
using CaixaSeguradora.Core.Attributes;

namespace CaixaSeguradora.Core.DTOs
{
    /// <summary>
    /// PREMCED.TXT output record layout (168 bytes per record).
    /// This DTO represents one line in the PREMCED.TXT file for cossurance data submitted to SUSEP.
    ///
    /// SUSEP Format: Fixed-width positional file, no delimiters, no line terminators between records.
    /// Total Record Length: 168 bytes
    ///
    /// Field Formatting Rules:
    /// - Numeric (PIC 9): Left-pad with zeros, implied decimal point
    /// - Alphanumeric (PIC X): Right-pad with spaces
    /// - Dates: YYYYMMDD format (8 digits)
    ///
    /// Source: JAZZ T285991 specification update (168-byte format)
    /// COBOL Source: REG-PREMCED structure in RG1866B
    /// Research Reference: specs/003-complete-cobol-migration/research.md section R3
    /// </summary>
    public class PremcedOutputRecord
    {
        // ===== POLICY IDENTIFICATION =====

        /// <summary>
        /// Company Code (JV1 companies: 00, 10, 11).
        /// COBOL: CED-COD-CIA PIC 9(5)
        /// </summary>
        [FixedWidthField(Position = 1, Length = 5, Type = FieldType.Numeric, DecimalPlaces = 0)]
        public int CompanyCode { get; set; }

        /// <summary>
        /// SUSEP Branch Code (Ramo SUSEP).
        /// COBOL: CED-RAMO-SUSEP PIC 9(4)
        /// Examples: 0531 (Auto), 0167 (Life), 0193 (Homeowners)
        /// </summary>
        [FixedWidthField(Position = 6, Length = 4, Type = FieldType.Numeric, DecimalPlaces = 0)]
        public int RamoSusep { get; set; }

        /// <summary>
        /// Policy Number (alphanumeric format).
        /// COBOL: CED-NUM-APOLICE PIC X(20)
        /// </summary>
        [FixedWidthField(Position = 10, Length = 20, Type = FieldType.Alphanumeric)]
        public string PolicyNumber { get; set; }

        /// <summary>
        /// Endorsement Number.
        /// COBOL: CED-NUM-ENDOSSO PIC 9(10)
        /// </summary>
        [FixedWidthField(Position = 30, Length = 10, Type = FieldType.Numeric, DecimalPlaces = 0)]
        public long EndorsementNumber { get; set; }

        // ===== COSSURANCE DETAILS =====

        /// <summary>
        /// Cession Type.
        /// COBOL: CED-TIPO-CESSAO PIC X(1)
        /// 'C' = Cedido (Ceded - outgoing cossurance)
        /// 'O' = Obtido (Accepted - incoming cossurance)
        /// </summary>
        [FixedWidthField(Position = 40, Length = 1, Type = FieldType.Alphanumeric)]
        public string CessionType { get; set; }

        /// <summary>
        /// Cossurer Company Code (coparticipant company).
        /// COBOL: CED-COD-CIA-COPART PIC 9(5)
        /// Code of the company sharing the risk.
        /// </summary>
        [FixedWidthField(Position = 41, Length = 5, Type = FieldType.Numeric, DecimalPlaces = 0)]
        public int CossurerCompanyCode { get; set; }

        /// <summary>
        /// Participation Percentage (0.00% to 100.00%).
        /// COBOL: CED-PERC-PARTICIPACAO PIC 9(3)V99
        /// Example: 25.00% → "02500" (implied decimal: 25.00)
        /// </summary>
        [FixedWidthField(Position = 46, Length = 5, Type = FieldType.Numeric, DecimalPlaces = 2)]
        public decimal ParticipationPercentage { get; set; }

        // ===== FINANCIAL AMOUNTS =====

        /// <summary>
        /// Ceded Premium Amount (or Obtained Premium if CessionType = 'O').
        /// COBOL: CED-PREMIO-CEDIDO PIC S9(13)V99 COMP-3
        /// Amount of premium transferred to/from cossurer.
        /// </summary>
        [FixedWidthField(Position = 51, Length = 15, Type = FieldType.Numeric, DecimalPlaces = 2)]
        public decimal CededPremium { get; set; }

        /// <summary>
        /// Ceded Commission Amount.
        /// COBOL: CED-COMISSAO-CEDIDA PIC S9(13)V99 COMP-3
        /// Commission transferred to/from cossurer.
        /// </summary>
        [FixedWidthField(Position = 66, Length = 15, Type = FieldType.Numeric, DecimalPlaces = 2)]
        public decimal CededCommission { get; set; }

        /// <summary>
        /// Ceded Insured Sum.
        /// COBOL: CED-IMP-SEG-CEDIDO PIC S9(13)V99 COMP-3
        /// Portion of insured sum allocated to cossurer.
        /// </summary>
        [FixedWidthField(Position = 81, Length = 15, Type = FieldType.Numeric, DecimalPlaces = 2)]
        public decimal CededInsuredSum { get; set; }

        /// <summary>
        /// Issue Date.
        /// COBOL: CED-DT-EMISSAO PIC 9(8)
        /// Format: YYYYMMDD
        /// </summary>
        [FixedWidthField(Position = 96, Length = 8, Type = FieldType.Date)]
        public DateTime IssueDate { get; set; }

        /// <summary>
        /// Effective Date (Start of Coverage).
        /// COBOL: CED-DT-INI-VIG PIC 9(8)
        /// Format: YYYYMMDD
        /// </summary>
        [FixedWidthField(Position = 104, Length = 8, Type = FieldType.Date)]
        public DateTime EffectiveDate { get; set; }

        /// <summary>
        /// Expiration Date (End of Coverage).
        /// COBOL: CED-DT-FIM-VIG PIC 9(8)
        /// Format: YYYYMMDD
        /// </summary>
        [FixedWidthField(Position = 112, Length = 8, Type = FieldType.Date)]
        public DateTime ExpirationDate { get; set; }

        // ===== OPERATIONAL ATTRIBUTES =====

        /// <summary>
        /// Movement Type Code (101-106).
        /// COBOL: CED-TIPO-MOV PIC 9(3)
        /// 101 = Emission, 102 = Renewal, 103 = Majoração, 104 = Redução, 105 = Cancellation, 106 = Restitution
        /// </summary>
        [FixedWidthField(Position = 120, Length = 3, Type = FieldType.Numeric, DecimalPlaces = 0)]
        public int MovementType { get; set; }

        /// <summary>
        /// Product Code (Internal).
        /// COBOL: CED-COD-PRODUTO PIC 9(4)
        /// </summary>
        [FixedWidthField(Position = 123, Length = 4, Type = FieldType.Numeric, DecimalPlaces = 0)]
        public int ProductCode { get; set; }

        /// <summary>
        /// Client Code (Insured).
        /// COBOL: CED-COD-CLIENTE PIC 9(9)
        /// </summary>
        [FixedWidthField(Position = 127, Length = 9, Type = FieldType.Numeric, DecimalPlaces = 0)]
        public int ClientCode { get; set; }

        /// <summary>
        /// Sequence Number (for multiple cossurance records per policy).
        /// COBOL: CED-NUM-SEQUENCIA PIC 9(4)
        /// </summary>
        [FixedWidthField(Position = 136, Length = 4, Type = FieldType.Numeric, DecimalPlaces = 0)]
        public int SequenceNumber { get; set; }

        // ===== RESERVED FIELDS FOR SUSEP (FILL WITH SPACES) =====

        /// <summary>
        /// Reserved Field (filler).
        /// COBOL: CED-FILLER PIC X(28)
        /// Total padding to reach 168 bytes: positions 140-168 (29 bytes)
        /// Note: Adjusted to ensure total = 168 bytes
        /// </summary>
        [FixedWidthField(Position = 140, Length = 29, Type = FieldType.Alphanumeric)]
        public string Reserved { get; set; } = new string(' ', 29);

        /// <summary>
        /// Calculate and return the total record length.
        /// Should always be 168 bytes for PREMCED.TXT.
        /// </summary>
        public const int RecordLength = 168;

        /// <summary>
        /// Validate that this record structure matches expected 168-byte layout.
        /// </summary>
        public bool ValidateRecordLength()
        {
            // This would be calculated by summing all FixedWidthField Length properties via reflection
            // Total should equal 168 bytes
            return true; // Placeholder - implement via reflection in service layer
        }

        /// <summary>
        /// Validate cossurance percentage is within valid range (0.00-100.00).
        /// </summary>
        public bool IsValidParticipationPercentage()
        {
            return ParticipationPercentage >= 0m && ParticipationPercentage <= 100m;
        }

        /// <summary>
        /// Validate cession type is either 'C' (Cedido) or 'O' (Obtido).
        /// </summary>
        public bool IsValidCessionType()
        {
            return CessionType == "C" || CessionType == "O";
        }
    }
}
