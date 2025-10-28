using System;
using CaixaSeguradora.Core.Attributes;

namespace CaixaSeguradora.Core.DTOs
{
    /// <summary>
    /// PREMIT.TXT output record layout (765 bytes per record).
    /// This DTO represents one line in the PREMIT.TXT file submitted to SUSEP.
    ///
    /// SUSEP Format: Fixed-width positional file, no delimiters, no line terminators between records.
    /// Total Record Length: 765 bytes
    ///
    /// Field Formatting Rules:
    /// - Numeric (PIC 9): Left-pad with zeros, implied decimal point
    /// - Alphanumeric (PIC X): Right-pad with spaces
    /// - Dates: YYYYMMDD format (8 digits)
    ///
    /// Source: JAZZ T285991 specification update (765-byte format)
    /// COBOL Source: REG-PREMIT structure in RG1866B
    /// Research Reference: specs/003-complete-cobol-migration/research.md section R3
    /// </summary>
    public class PremitOutputRecord
    {
        // ===== POLICY IDENTIFICATION =====

        /// <summary>
        /// Company Code (JV1 companies: 00, 10, 11).
        /// COBOL: EMI-COD-CIA PIC 9(5)
        /// </summary>
        [FixedWidthField(Position = 1, Length = 5, Type = FieldType.Numeric, DecimalPlaces = 0)]
        public int CompanyCode { get; set; }

        /// <summary>
        /// SUSEP Branch Code (Ramo SUSEP).
        /// COBOL: EMI-RAMO-SUSEP PIC 9(4)
        /// Examples: 0531 (Auto), 0167 (Life), 0193 (Homeowners)
        /// </summary>
        [FixedWidthField(Position = 6, Length = 4, Type = FieldType.Numeric, DecimalPlaces = 0)]
        public int RamoSusep { get; set; }

        /// <summary>
        /// Policy Number (left-padded with zeros for numeric representation).
        /// COBOL: EMI-NUM-APOLICE PIC X(20)
        /// </summary>
        [FixedWidthField(Position = 10, Length = 20, Type = FieldType.Alphanumeric)]
        public string PolicyNumber { get; set; }

        /// <summary>
        /// Endorsement Number (CA endorsement number).
        /// COBOL: EMI-NUM-ENDOSSO-CA PIC 9(10)
        /// </summary>
        [FixedWidthField(Position = 30, Length = 10, Type = FieldType.Numeric, DecimalPlaces = 0)]
        public long EndorsementNumberCA { get; set; }

        /// <summary>
        /// Endorsement Number (general endorsement).
        /// COBOL: EMI-NUM-ENDOSSO PIC 9(10)
        /// </summary>
        [FixedWidthField(Position = 40, Length = 10, Type = FieldType.Numeric, DecimalPlaces = 0)]
        public long EndorsementNumber { get; set; }

        // ===== DATES =====

        /// <summary>
        /// Issue Date (Emission Date).
        /// COBOL: EMI-DT-EMISSAO PIC 9(8)
        /// Format: YYYYMMDD
        /// </summary>
        [FixedWidthField(Position = 50, Length = 8, Type = FieldType.Date)]
        public DateTime IssueDate { get; set; }

        /// <summary>
        /// Effective Date (Start of Coverage).
        /// COBOL: EMI-DT-INI-VIG PIC 9(8)
        /// Format: YYYYMMDD
        /// </summary>
        [FixedWidthField(Position = 58, Length = 8, Type = FieldType.Date)]
        public DateTime EffectiveDate { get; set; }

        /// <summary>
        /// Expiration Date (End of Coverage).
        /// COBOL: EMI-DT-FIM-VIG PIC 9(8)
        /// Format: YYYYMMDD
        /// </summary>
        [FixedWidthField(Position = 66, Length = 8, Type = FieldType.Date)]
        public DateTime ExpirationDate { get; set; }

        /// <summary>
        /// Proposal Date.
        /// COBOL: EMI-DT-PROPOSTA PIC 9(8)
        /// Format: YYYYMMDD (00000000 if null)
        /// </summary>
        [FixedWidthField(Position = 74, Length = 8, Type = FieldType.Date)]
        public DateTime? ProposalDate { get; set; }

        // ===== MOVEMENT TYPE =====

        /// <summary>
        /// Movement Type Code (101-106).
        /// COBOL: EMI-TIPO-MOV PIC 9(3)
        /// 101 = Emission, 102 = Renewal, 103 = Majoração, 104 = Redução, 105 = Cancellation, 106 = Restitution
        /// </summary>
        [FixedWidthField(Position = 82, Length = 3, Type = FieldType.Numeric, DecimalPlaces = 0)]
        public int MovementType { get; set; }

        // ===== FINANCIAL AMOUNTS (ITEM LEVEL - 5 DECIMAL PLACES) =====

        /// <summary>
        /// Insured Sum - Items (Importo Segurado Itens).
        /// COBOL: EMI-IMP-SEG-IT PIC S9(10)V9(5) COMP-3
        /// </summary>
        [FixedWidthField(Position = 85, Length = 15, Type = FieldType.Numeric, DecimalPlaces = 5)]
        public decimal InsuredSumItems { get; set; }

        /// <summary>
        /// Base Premium - Items.
        /// COBOL: EMI-VLPRMBAS-IT PIC S9(10)V9(5) COMP-3
        /// </summary>
        [FixedWidthField(Position = 100, Length = 15, Type = FieldType.Numeric, DecimalPlaces = 5)]
        public decimal BasePremiumItems { get; set; }

        /// <summary>
        /// Fixed Premium - Items.
        /// COBOL: EMI-VLPREFIX-IT PIC S9(10)V9(5) COMP-3
        /// </summary>
        [FixedWidthField(Position = 115, Length = 15, Type = FieldType.Numeric, DecimalPlaces = 5)]
        public decimal FixedPremiumItems { get; set; }

        /// <summary>
        /// Tariff Premium - Items.
        /// COBOL: EMI-VLPRMTAR-IT PIC S9(10)V9(5) COMP-3
        /// </summary>
        [FixedWidthField(Position = 130, Length = 15, Type = FieldType.Numeric, DecimalPlaces = 5)]
        public decimal TariffPremiumItems { get; set; }

        /// <summary>
        /// Discount - Items.
        /// COBOL: EMI-VLDESCON-IT PIC S9(10)V9(5) COMP-3
        /// </summary>
        [FixedWidthField(Position = 145, Length = 15, Type = FieldType.Numeric, DecimalPlaces = 5)]
        public decimal DiscountItems { get; set; }

        /// <summary>
        /// Net Premium - Items.
        /// COBOL: EMI-VLPRMLIQ-IT PIC S9(10)V9(5) COMP-3
        /// </summary>
        [FixedWidthField(Position = 160, Length = 15, Type = FieldType.Numeric, DecimalPlaces = 5)]
        public decimal NetPremiumItems { get; set; }

        /// <summary>
        /// Installment Surcharge - Items.
        /// COBOL: EMI-VLADIFRA-IT PIC S9(10)V9(5) COMP-3
        /// </summary>
        [FixedWidthField(Position = 175, Length = 15, Type = FieldType.Numeric, DecimalPlaces = 5)]
        public decimal InstallmentSurchargeItems { get; set; }

        /// <summary>
        /// Issuance Cost - Items.
        /// COBOL: EMI-VLCUSEMI-IT PIC S9(10)V9(5) COMP-3
        /// </summary>
        [FixedWidthField(Position = 190, Length = 15, Type = FieldType.Numeric, DecimalPlaces = 5)]
        public decimal IssuanceCostItems { get; set; }

        /// <summary>
        /// IOF - Items.
        /// COBOL: EMI-VLIOCC-IT PIC S9(10)V9(5) COMP-3
        /// </summary>
        [FixedWidthField(Position = 205, Length = 15, Type = FieldType.Numeric, DecimalPlaces = 5)]
        public decimal IofItems { get; set; }

        /// <summary>
        /// Total Premium - Items.
        /// COBOL: EMI-VLPRMTOT-IT PIC S9(10)V9(5) COMP-3
        /// </summary>
        [FixedWidthField(Position = 220, Length = 15, Type = FieldType.Numeric, DecimalPlaces = 5)]
        public decimal TotalPremiumItems { get; set; }

        /// <summary>
        /// Commission - Items.
        /// COBOL: EMI-VLCOMIS-IT PIC S9(10)V9(5) COMP-3
        /// </summary>
        [FixedWidthField(Position = 235, Length = 15, Type = FieldType.Numeric, DecimalPlaces = 5)]
        public decimal CommissionItems { get; set; }

        /// <summary>
        /// Administration Fee - Items.
        /// COBOL: EMI-VLADMN-IT PIC S9(10)V9(5) COMP-3
        /// </summary>
        [FixedWidthField(Position = 250, Length = 15, Type = FieldType.Numeric, DecimalPlaces = 5)]
        public decimal AdministrationFeeItems { get; set; }

        /// <summary>
        /// Agency Fee - Items.
        /// COBOL: EMI-VLAGENC-IT PIC S9(10)V9(5) COMP-3
        /// </summary>
        [FixedWidthField(Position = 265, Length = 15, Type = FieldType.Numeric, DecimalPlaces = 5)]
        public decimal AgencyFeeItems { get; set; }

        // ===== FINANCIAL AMOUNTS (TOTAL LEVEL - 2 DECIMAL PLACES) =====

        /// <summary>
        /// Insured Sum - Total.
        /// COBOL: EMI-IMP-SEG-T PIC S9(13)V99 COMP-3
        /// </summary>
        [FixedWidthField(Position = 280, Length = 15, Type = FieldType.Numeric, DecimalPlaces = 2)]
        public decimal InsuredSumTotal { get; set; }

        /// <summary>
        /// Base Premium - Total.
        /// COBOL: EMI-VLPRMBAS-T PIC S9(13)V99 COMP-3
        /// </summary>
        [FixedWidthField(Position = 295, Length = 15, Type = FieldType.Numeric, DecimalPlaces = 2)]
        public decimal BasePremiumTotal { get; set; }

        /// <summary>
        /// Net Premium - Total.
        /// COBOL: EMI-VLPRMLIQ-T PIC S9(13)V99 COMP-3
        /// </summary>
        [FixedWidthField(Position = 310, Length = 15, Type = FieldType.Numeric, DecimalPlaces = 2)]
        public decimal NetPremiumTotal { get; set; }

        /// <summary>
        /// Total Premium Amount - Total.
        /// COBOL: EMI-VLPRMTOT-T PIC S9(13)V99 COMP-3
        /// </summary>
        [FixedWidthField(Position = 325, Length = 15, Type = FieldType.Numeric, DecimalPlaces = 2)]
        public decimal TotalPremiumAmount { get; set; }

        /// <summary>
        /// IOF - Total.
        /// COBOL: EMI-VLIOCC-T PIC S9(13)V99 COMP-3
        /// </summary>
        [FixedWidthField(Position = 340, Length = 15, Type = FieldType.Numeric, DecimalPlaces = 2)]
        public decimal IofTotal { get; set; }

        /// <summary>
        /// Commission - Total.
        /// COBOL: EMI-VLCOMIS-T PIC S9(13)V99 COMP-3
        /// </summary>
        [FixedWidthField(Position = 355, Length = 15, Type = FieldType.Numeric, DecimalPlaces = 2)]
        public decimal CommissionTotal { get; set; }

        // ===== PARTY IDENTIFIERS =====

        /// <summary>
        /// Client Code (Insured).
        /// COBOL: EMI-COD-CLIENTE PIC 9(9)
        /// </summary>
        [FixedWidthField(Position = 370, Length = 9, Type = FieldType.Numeric, DecimalPlaces = 0)]
        public int ClientCode { get; set; }

        /// <summary>
        /// Policyholder Code (Estipulante).
        /// COBOL: EMI-COD-ESTIPULANTE PIC 9(9)
        /// </summary>
        [FixedWidthField(Position = 379, Length = 9, Type = FieldType.Numeric, DecimalPlaces = 0)]
        public int? EstipulanteCode { get; set; }

        /// <summary>
        /// Premium Payer Code (Tomador).
        /// COBOL: EMI-COD-TOMADOR PIC 9(9)
        /// </summary>
        [FixedWidthField(Position = 388, Length = 9, Type = FieldType.Numeric, DecimalPlaces = 0)]
        public int? TomadorCode { get; set; }

        /// <summary>
        /// Agency Code.
        /// COBOL: EMI-COD-AGENCIA PIC 9(4)
        /// </summary>
        [FixedWidthField(Position = 397, Length = 4, Type = FieldType.Numeric, DecimalPlaces = 0)]
        public int? AgencyCode { get; set; }

        /// <summary>
        /// Producer Code (Broker).
        /// COBOL: EMI-COD-PRODUTOR PIC 9(4)
        /// </summary>
        [FixedWidthField(Position = 401, Length = 4, Type = FieldType.Numeric, DecimalPlaces = 0)]
        public int? ProducerCode { get; set; }

        /// <summary>
        /// Sales Channel Code.
        /// COBOL: EMI-CANAL-VENDA PIC 9(4)
        /// </summary>
        [FixedWidthField(Position = 405, Length = 4, Type = FieldType.Numeric, DecimalPlaces = 0)]
        public int? SalesChannelCode { get; set; }

        // ===== OPERATIONAL ATTRIBUTES =====

        /// <summary>
        /// Bilhete Number (required for grupo ramo 09).
        /// COBOL: EMI-NUM-BILHETE PIC 9(15)
        /// </summary>
        [FixedWidthField(Position = 409, Length = 15, Type = FieldType.Numeric, DecimalPlaces = 0)]
        public long? BilheteNumber { get; set; }

        /// <summary>
        /// Number of Insured Lives.
        /// COBOL: EMI-QTD-SEGURADOS PIC 9(4)
        /// </summary>
        [FixedWidthField(Position = 424, Length = 4, Type = FieldType.Numeric, DecimalPlaces = 0)]
        public int NumberOfInsured { get; set; }

        /// <summary>
        /// Number of Installments.
        /// COBOL: EMI-NUM-PARCELAS PIC 9(4)
        /// </summary>
        [FixedWidthField(Position = 428, Length = 4, Type = FieldType.Numeric, DecimalPlaces = 0)]
        public int NumberOfInstallments { get; set; }

        /// <summary>
        /// Product Code (Internal).
        /// COBOL: EMI-COD-PRODUTO PIC 9(4)
        /// </summary>
        [FixedWidthField(Position = 432, Length = 4, Type = FieldType.Numeric, DecimalPlaces = 0)]
        public int ProductCode { get; set; }

        /// <summary>
        /// Operation Type.
        /// COBOL: EMI-TIPO-OPER PIC 9(4)
        /// </summary>
        [FixedWidthField(Position = 436, Length = 4, Type = FieldType.Numeric, DecimalPlaces = 0)]
        public int? OperationType { get; set; }

        /// <summary>
        /// State Code (UF - 2 letters).
        /// COBOL: EMI-UF PIC X(2)
        /// Examples: "SP", "RJ", "MG"
        /// </summary>
        [FixedWidthField(Position = 440, Length = 2, Type = FieldType.Alphanumeric)]
        public string StateCode { get; set; }

        /// <summary>
        /// City Code (IBGE Code).
        /// COBOL: EMI-COD-CIDADE PIC 9(7)
        /// </summary>
        [FixedWidthField(Position = 442, Length = 7, Type = FieldType.Numeric, DecimalPlaces = 0)]
        public int? CityCode { get; set; }

        /// <summary>
        /// SUSEP Process Number (for specific products).
        /// COBOL: EMI-NUM-PROCESSO-SUSEP PIC X(25)
        /// </summary>
        [FixedWidthField(Position = 449, Length = 25, Type = FieldType.Alphanumeric)]
        public string SusepProcessNumber { get; set; }

        /// <summary>
        /// Cossurance Flag.
        /// COBOL: EMI-IND-COSSEGURO PIC X(1)
        /// 'S' = Yes, 'N' = No
        /// </summary>
        [FixedWidthField(Position = 474, Length = 1, Type = FieldType.Alphanumeric)]
        public string HasCossurance { get; set; }

        /// <summary>
        /// Renewal Flag.
        /// COBOL: EMI-IND-RENOVACAO PIC X(1)
        /// 'S' = Yes, 'N' = No
        /// </summary>
        [FixedWidthField(Position = 475, Length = 1, Type = FieldType.Alphanumeric)]
        public string IsRenewal { get; set; }

        // ===== RESERVED FIELDS FOR SUSEP (FILL WITH SPACES) =====

        /// <summary>
        /// Reserved Field 1 (filler).
        /// COBOL: EMI-FILLER-1 PIC X(290)
        /// Total padding to reach 765 bytes: positions 476-765
        /// </summary>
        [FixedWidthField(Position = 476, Length = 290, Type = FieldType.Alphanumeric)]
        public string Reserved { get; set; } = new string(' ', 290);

        /// <summary>
        /// Calculate and return the total record length.
        /// Should always be 765 bytes for PREMIT.TXT.
        /// </summary>
        public const int RecordLength = 765;

        /// <summary>
        /// Validate that this record structure matches expected 765-byte layout.
        /// </summary>
        public bool ValidateRecordLength()
        {
            // This would be calculated by summing all FixedWidthField Length properties via reflection
            // Total should equal 765 bytes
            return true; // Placeholder - implement via reflection in service layer
        }
    }
}
