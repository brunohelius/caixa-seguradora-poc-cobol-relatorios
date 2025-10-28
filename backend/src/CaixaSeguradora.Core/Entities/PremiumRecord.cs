using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using CaixaSeguradora.Core.Attributes;

namespace CaixaSeguradora.Core.Entities
{
    /// <summary>
    /// Represents premium emission records - the core entity for report generation
    /// COBOL Source: V0PREMIOS view, processed via cursor at section R0500-00-DECLARE-V0PREMIOS
    /// </summary>
    public class PremiumRecord
    {
        // Primary Key
        [Key]
        public long PremiumId { get; set; }

        // Business Identifiers
        [CobolField(PicClause = "9(9)", Length = 9, FieldType = CobolFieldType.Numeric)]
        public int CompanyCode { get; set; }  // V0PREM-COD-EMP (COMP)

        [CobolField(PicClause = "9(4)", Length = 4)]
        public int ReferenceYear { get; set; }  // V0PREM-ANO-REFER

        [CobolField(PicClause = "9(4)", Length = 4)]
        public int ReferenceMonth { get; set; }  // V0PREM-MES-REFER

        [CobolField(PicClause = "9(4)", Length = 4)]
        public int ReferenceDay { get; set; }  // V0PREM-DIA-REFER

        [CobolField(PicClause = "X(1)", Length = 1)]
        [MaxLength(1)]
        public string MovementType { get; set; } = string.Empty;  // V0PREM-TIPO-MOVT ('E', 'C', 'R', etc.)

        [CobolField(PicClause = "9(13)", Length = 13, FieldType = CobolFieldType.PackedDecimal)]
        public long PolicyNumber { get; set; }  // V0PREM-NUM-APOL (COMP-3)

        [CobolField(PicClause = "9(9)", Length = 9)]
        public int EndorsementNumber { get; set; }  // V0PREM-NRENDOS

        [CobolField(PicClause = "9(4)", Length = 4)]
        public int InstallmentNumber { get; set; }  // V0PREM-NRPARCEL

        [CobolField(PicClause = "9(4)", Length = 4)]
        public int OccurrenceNumber { get; set; }  // V0PREM-NUM-OCORR

        [CobolField(PicClause = "9(4)", Length = 4)]
        public int HistoricalOccurrence { get; set; }  // V0PREM-OCORHIST

        // Product Classification
        [CobolField(PicClause = "9(4)", Length = 4)]
        public int LineOfBusiness { get; set; }  // V0PREM-RAMOFR (Ramo)

        [CobolField(PicClause = "9(4)", Length = 4)]
        public int ProductModality { get; set; }  // V0PREM-MODALIFR

        [CobolField(PicClause = "9(4)", Length = 4)]
        public int OperationType { get; set; }  // V0PREM-OPERACAO

        [CobolField(PicClause = "9(4)", Length = 4)]
        public int BusinessOperationType { get; set; }  // V0PREM-TIPO-OPER

        [CobolField(PicClause = "9(9)", Length = 9)]
        public int ClientCode { get; set; }  // V0PREM-CODCLIEN

        // Currency & Exchange
        [CobolField(PicClause = "9(6)V9(9)", Length = 16, DecimalPlaces = 9, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,9)")]
        public decimal ExchangeRate { get; set; }  // V0PREM-VALOR-COT

        // Premium Components - Installment (Item)
        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal InsuredAmountItem { get; set; }  // V0PREM-IMP-SEG-IT

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal BasePremiumItem { get; set; }  // V0PREM-VLPRMBAS-IT

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal FixedPremiumItem { get; set; }  // V0PREM-VLPREFIX-IT

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal TariffPremiumItem { get; set; }  // V0PREM-VLPRMTAR-IT

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal DiscountItem { get; set; }  // V0PREM-VLDESCON-IT

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal NetPremiumItem { get; set; }  // V0PREM-VLPRMLIQ-IT

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal AdditionalFractionalItem { get; set; }  // V0PREM-VLADIFRA-IT

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal IssuanceCostItem { get; set; }  // V0PREM-VLCUSEMI-IT

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal IofItem { get; set; }  // V0PREM-VLIOCC-IT

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal TotalPremiumItem { get; set; }  // V0PREM-VLPRMTOT-IT

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal CommissionItem { get; set; }  // V0PREM-VLCOMIS-IT

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal AdministrationFeeItem { get; set; }  // V0PREM-VLADMN-IT

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal AgencyCommissionItem { get; set; }  // V0PREM-VLAGENC-IT

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal PreferentialCommissionItem { get; set; }  // V0PREM-VLPREFCM-IT

        // Premium Components - Net (Liquido)
        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal InsuredAmountNet { get; set; }  // V0PREM-IMP-SEG-IL

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal BasePremiumNet { get; set; }  // V0PREM-VLPRMBAS-IL

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal FixedPremiumNet { get; set; }  // V0PREM-VLPREFIX-IL

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal TariffPremiumNet { get; set; }  // V0PREM-VLPRMTAR-IL

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal DiscountNet { get; set; }  // V0PREM-VLDESCON-IL

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal NetPremiumNet { get; set; }  // V0PREM-VLPRMLIQ-IL

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal AdditionalFractionalNet { get; set; }  // V0PREM-VLADIFRA-IL

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal IssuanceCostNet { get; set; }  // V0PREM-VLCUSEMI-IL

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal IofNet { get; set; }  // V0PREM-VLIOCC-IL

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal TotalPremiumNet { get; set; }  // V0PREM-VLPRMTOT-IL

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal CommissionNet { get; set; }  // V0PREM-VLCOMIS-IL

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal AdministrationFeeNet { get; set; }  // V0PREM-VLADMN-IL

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal AgencyCommissionNet { get; set; }  // V0PREM-VLAGENC-IL

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal PreferentialCommissionNet { get; set; }  // V0PREM-VLPREFCM-IL

        // Premium Components - Cossurance (Cosseguro)
        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal InsuredAmountCossurance { get; set; }  // V0PREM-IMP-SEG-IC

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal BasePremiumCossurance { get; set; }  // V0PREM-VLPRMBAS-IC

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal FixedPremiumCossurance { get; set; }  // V0PREM-VLPREFIX-IC

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal TariffPremiumCossurance { get; set; }  // V0PREM-VLPRMTAR-IC

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal DiscountCossurance { get; set; }  // V0PREM-VLDESCON-IC

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal NetPremiumCossurance { get; set; }  // V0PREM-VLPRMLIQ-IC

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal AdditionalFractionalCossurance { get; set; }  // V0PREM-VLADIFRA-IC

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal CommissionCossurance { get; set; }  // V0PREM-VLCOMIS-IC

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal AdministrationFeeCossurance { get; set; }  // V0PREM-VLADMN-IC

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal AgencyCommissionCossurance { get; set; }  // V0PREM-VLAGENC-IC

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal PreferentialCommissionCossurance { get; set; }  // V0PREM-VLPREFCM-IC

        // Premium Components - Reinsurance
        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal InsuredAmountReinsurance { get; set; }  // V0PREM-IMP-SEG-IR

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal TariffPremiumReinsurance { get; set; }  // V0PREM-VLPRMTAR-IR

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal DiscountReinsurance { get; set; }  // V0PREM-VLDESCON-IR

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal NetPremiumReinsurance { get; set; }  // V0PREM-VLPRMLIQ-IR

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal AdditionalFractionalReinsurance { get; set; }  // V0PREM-VLADIFRA-IR

        [CobolField(PicClause = "9(10)V9(5)", Length = 16, DecimalPlaces = 5, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,5)")]
        public decimal CommissionReinsurance { get; set; }  // V0PREM-VLCOMIS-IR

        // Premium Totals
        [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,2)")]
        public decimal InsuredAmountTotal { get; set; }  // V0PREM-IMP-SEG-T

        [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,2)")]
        public decimal BasePremiumTotal { get; set; }  // V0PREM-VLPRMBAS-T

        [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,2)")]
        public decimal FixedPremiumTotal { get; set; }  // V0PREM-VLPREFIX-T

        [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,2)")]
        public decimal TariffPremiumTotal { get; set; }  // V0PREM-VLPRMTAR-T

        [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,2)")]
        public decimal DiscountTotal { get; set; }  // V0PREM-VLDESCON-T

        [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,2)")]
        public decimal NetPremiumTotal { get; set; }  // V0PREM-VLPRMLIQ-T

        [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,2)")]
        public decimal AdditionalFractionalTotal { get; set; }  // V0PREM-VLADIFRA-T

        [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,2)")]
        public decimal IssuanceCostTotal { get; set; }  // V0PREM-VLCUSEMI-T

        [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,2)")]
        public decimal IofTotal { get; set; }  // V0PREM-VLIOCC-T

        [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,2)")]
        public decimal TotalPremiumTotal { get; set; }  // V0PREM-VLPRMTOT-T

        [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,2)")]
        public decimal CommissionTotal { get; set; }  // V0PREM-VLCOMIS-T

        [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,2)")]
        public decimal AdministrationFeeTotal { get; set; }  // V0PREM-VLADMN-T

        [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,2)")]
        public decimal AgencyCommissionTotal { get; set; }  // V0PREM-VLAGENC-T

        [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,2)")]
        public decimal PreferentialCommissionTotal { get; set; }  // V0PREM-VLPREFCM-T

        // Net Local Currency Totals
        [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,2)")]
        public decimal InsuredAmountLocalTotal { get; set; }  // V0PREM-IMP-SEG-L

        [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,2)")]
        public decimal BasePremiumLocalTotal { get; set; }  // V0PREM-VLPRMBAS-L

        [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,2)")]
        public decimal FixedPremiumLocalTotal { get; set; }  // V0PREM-VLPREFIX-L

        // Calculated/Derived Properties (for backward compatibility with services)
        [NotMapped]
        public decimal NetPremium => NetPremiumTotal;

        [NotMapped]
        public decimal GrossPremium => TotalPremiumTotal;

        [NotMapped]
        public decimal IOFAmount => IofTotal;

        [NotMapped]
        public decimal CommissionAmount => CommissionTotal;

        [NotMapped]
        public int LineOfBusinessCode => LineOfBusiness;

        // Additional business identifiers (from data-model.md analysis)
        [CobolField(PicClause = "9(4)", Length = 4)]
        public int ProductCode { get; set; }  // Derived from Product relationship

        [CobolField(PicClause = "X(2)", Length = 2)]
        [MaxLength(2)]
        public string SystemCode { get; set; } = string.Empty;  // System identifier (e.g., 'RG', 'GL')

        // Extended Properties for Repository/Service compatibility
        public long Id { get; set; }  // Alias for PremiumId

        [NotMapped]
        public long PolicyId => PolicyNumber;

        [CobolField(PicClause = "9(4)", Length = 4)]
        public int AgencyCode { get; set; }

        [CobolField(PicClause = "X(100)", Length = 100)]
        [MaxLength(100)]
        public string AgencyName { get; set; } = string.Empty;

        [CobolField(PicClause = "9(9)", Length = 9)]
        public int ProducerCode { get; set; }

        [CobolField(PicClause = "X(100)", Length = 100)]
        [MaxLength(100)]
        public string ProducerName { get; set; } = string.Empty;

        [CobolField(PicClause = "9(4)V99", Length = 6, DecimalPlaces = 2)]
        [Column(TypeName = "decimal(6,2)")]
        public decimal ProducerCommissionPercentage { get; set; }

        [CobolField(PicClause = "9(9)", Length = 9)]
        public int InsuredCode { get; set; }

        [CobolField(PicClause = "X(100)", Length = 100)]
        [MaxLength(100)]
        public string InsuredName { get; set; } = string.Empty;

        [CobolField(PicClause = "X(14)", Length = 14)]
        [MaxLength(14)]
        public string InsuredTaxId { get; set; } = string.Empty;

        [CobolField(PicClause = "X(1)", Length = 1)]
        [MaxLength(1)]
        public string InsuredPersonType { get; set; } = string.Empty;

        [CobolField(PicClause = "X(100)", Length = 100)]
        [MaxLength(100)]
        public string ProductName { get; set; } = string.Empty;

        [CobolField(PicClause = "X(10)", Length = 10)]
        [MaxLength(10)]
        public string PolicyStartDate { get; set; } = string.Empty;

        [CobolField(PicClause = "X(1)", Length = 1)]
        [MaxLength(1)]
        public string PolicyStatus { get; set; } = string.Empty;

        [CobolField(PicClause = "X(3)", Length = 3)]
        [MaxLength(3)]
        public string CurrencyCode { get; set; } = "BRL";

        [CobolField(PicClause = "X(1)", Length = 1)]
        [MaxLength(1)]
        public string CossuranceIndicator { get; set; } = "N";

        [CobolField(PicClause = "9(4)V9(9)", Length = 14, DecimalPlaces = 9)]
        [Column(TypeName = "decimal(13,9)")]
        public decimal CossurancePercentage { get; set; }

        [Column(TypeName = "decimal(15,2)")]
        public decimal CossurancePremium { get; set; }

        [CobolField(PicClause = "X(100)", Length = 100)]
        [MaxLength(100)]
        public string Street { get; set; } = string.Empty;

        [CobolField(PicClause = "X(10)", Length = 10)]
        [MaxLength(10)]
        public string AddressNumber { get; set; } = string.Empty;

        [CobolField(PicClause = "X(50)", Length = 50)]
        [MaxLength(50)]
        public string City { get; set; } = string.Empty;

        [CobolField(PicClause = "X(2)", Length = 2)]
        [MaxLength(2)]
        public string State { get; set; } = string.Empty;

        [CobolField(PicClause = "X(8)", Length = 8)]
        [MaxLength(8)]
        public string PostalCode { get; set; } = string.Empty;

        [CobolField(PicClause = "X(1)", Length = 1)]
        [MaxLength(1)]
        public string CalculationType { get; set; } = string.Empty;

        // Audit fields
        [CobolField(PicClause = "X(8)", Length = 8)]
        [MaxLength(8)]
        public string CreatedBy { get; set; } = string.Empty;

        [CobolField(PicClause = "X(8)", Length = 8)]
        [MaxLength(8)]
        public string UpdatedBy { get; set; } = string.Empty;

        [CobolField(PicClause = "X(32)", Length = 32)]
        [MaxLength(32)]
        public string RecordChecksum { get; set; } = string.Empty;

        // Derived/calculated fields for service compatibility
        [NotMapped]
        public decimal InsuredAmount => InsuredAmountTotal;

        [NotMapped]
        public decimal GrossPremiumAmount => TotalPremiumTotal;

        [NotMapped]
        public decimal NetPremiumAmount => NetPremiumTotal;

        [NotMapped]
        public decimal IofTaxValue => IofTotal;

        [NotMapped]
        public decimal CommissionValue => CommissionTotal;

        [NotMapped]
        public decimal InstallmentAmount => TotalPremiumItem;

        [NotMapped]
        public decimal InstallmentFee => IssuanceCostItem;

        [NotMapped]
        public decimal PolicyCost => IssuanceCostTotal;

        // Additional computed properties for OutputRecordMappingService compatibility
        [NotMapped]
        public int RamoSusep => LineOfBusiness;

        [NotMapped]
        public int EndorsementNumberCA => EndorsementNumber;

        [NotMapped]
        public DateTime IssueDate { get; set; }

        [NotMapped]
        public DateTime EffectiveDate { get; set; }

        [NotMapped]
        public DateTime ExpirationDate { get; set; }

        [NotMapped]
        public DateTime ProposalDate { get; set; }

        // Plural aliases for Item properties (as collections for OutputRecordMappingService)
        [NotMapped]
        public IEnumerable<decimal>? InsuredSumItems => new[] { InsuredAmountItem };

        [NotMapped]
        public IEnumerable<decimal>? BasePremiumItems => new[] { BasePremiumItem };

        [NotMapped]
        public IEnumerable<decimal>? FixedPremiumItems => new[] { FixedPremiumItem };

        [NotMapped]
        public IEnumerable<decimal>? TariffPremiumItems => new[] { TariffPremiumItem };

        [NotMapped]
        public IEnumerable<decimal>? DiscountItems => new[] { DiscountItem };

        [NotMapped]
        public IEnumerable<decimal>? NetPremiumItems => new[] { NetPremiumItem };

        [NotMapped]
        public IEnumerable<decimal>? InstallmentSurchargeItems => new[] { AdditionalFractionalItem };

        [NotMapped]
        public IEnumerable<decimal>? IssuanceCostItems => new[] { IssuanceCostItem };

        [NotMapped]
        public IEnumerable<decimal>? IofItems => new[] { IofItem };

        [NotMapped]
        public IEnumerable<decimal>? TotalPremiumItems => new[] { TotalPremiumItem };

        [NotMapped]
        public IEnumerable<decimal>? CommissionItems => new[] { CommissionItem };

        [NotMapped]
        public IEnumerable<decimal>? AdministrationFeeItems => new[] { AdministrationFeeItem };

        [NotMapped]
        public IEnumerable<decimal>? AgencyFeeItems => new[] { AgencyCommissionItem };

        // Total aliases
        [NotMapped]
        public decimal InsuredSumTotal => InsuredAmountTotal;

        [NotMapped]
        public decimal TotalPremiumAmount => TotalPremiumTotal;

        // Additional business properties
        [NotMapped]
        public int EstipulanteCode { get; set; }

        [NotMapped]
        public int TomadorCode { get; set; }

        [NotMapped]
        public int SalesChannelCode { get; set; }

        [NotMapped]
        public long BilheteNumber { get; set; }

        [NotMapped]
        public int NumberOfInsured { get; set; }

        [NotMapped]
        public int NumberOfInstallments { get; set; }

        [NotMapped]
        public bool HasCossurance => CossuranceIndicator == "S";

        [NotMapped]
        public bool IsRenewal { get; set; }

        // Navigation Properties
        public Policy? Policy { get; set; }
        public Product? Product { get; set; }
        public Client? Client { get; set; }
        public Endorsement? Endorsement { get; set; }
    }
}
