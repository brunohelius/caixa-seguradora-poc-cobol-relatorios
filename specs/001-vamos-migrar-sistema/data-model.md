# Data Model Design: COBOL RG1866B to .NET 9 Migration

**Feature Branch**: `001-vamos-migrar-sistema`
**Created**: October 22, 2025
**Status**: Phase 1.1 - Data Model Design
**Version**: 1.0

## Overview

This document defines the C# entity models for the SUSEP Circular 360 Premium Reporting System migration. All entities map to DB2 views/tables used by the legacy COBOL RG1866B program and follow Clean Architecture principles with precise type mappings to preserve COBOL data semantics.

## Design Principles

### Type Mapping Strategy
- **Financial Fields**: Use C# `decimal` type exclusively (never `float`/`double`) to match COBOL packed decimal precision
- **Fixed-Length Strings**: Preserve COBOL `PIC X(n)` semantics with `[MaxLength(n)]` attributes
- **Date Fields**: Use `DateTime` with explicit format handling for COBOL date representations (YYYYMMDD, DDMMYYYY)
- **COBOL Metadata**: Apply `[CobolField]` custom attributes to preserve PIC clause information for validation and file generation

### Entity Framework Configuration
- **Fluent API**: Configure complex relationships, indexes, and constraints
- **No Tracking**: Read-only queries use `AsNoTracking()` for performance
- **View Mapping**: Use `ToView()` for DB2 views, `ToTable()` for tables
- **Concurrency**: Not required (read-only operations)

### Relationship Patterns
- **Navigation Properties**: Defined for foreign key relationships
- **Lazy Loading**: Disabled (use explicit `Include()` for performance control)
- **Cascade Delete**: Not applicable (read-only system)

---

## Core Domain Entities

### 1. Premium Record (V0PREMIOS)

**Purpose**: Represents premium emission records - the core entity for report generation, aggregating policy, product, and financial data.

**COBOL Source**: `V0PREMIOS` view, processed via cursor at section R0500-00-DECLARE-V0PREMIOS

#### Properties

```csharp
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
    public string MovementType { get; set; }  // V0PREM-TIPO-MOVT ('E', 'C', 'R', etc.)

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

    // Navigation Properties
    public Policy Policy { get; set; }
    public Product Product { get; set; }
    public Client Client { get; set; }
    public Endorsement Endorsement { get; set; }
}
```

#### Validation Rules
1. `PolicyNumber` must exist in `Policy` entity
2. `EndorsementNumber` > 0 indicates endorsement-related premium
3. `ReferenceYear` must be >= 2000
4. `ReferenceMonth` must be 1-12
5. `ReferenceDay` must be 1-31
6. `MovementType` must be in `['E', 'C', 'R', 'S', 'A']` (Emission, Cancellation, Reversal, Supplement, Adjustment)
7. All financial totals must equal sum of corresponding item/net/cossurance/reinsurance components

#### EF Core Configuration

```csharp
public class PremiumRecordConfiguration : IEntityTypeConfiguration<PremiumRecord>
{
    public void Configure(EntityTypeBuilder<PremiumRecord> builder)
    {
        builder.ToView("V0PREMIOS");
        builder.HasKey(p => p.PremiumId);

        // Indexes for cursor processing (COBOL WHERE clause equivalents)
        builder.HasIndex(p => new { p.CompanyCode, p.ReferenceYear, p.ReferenceMonth, p.ReferenceDay })
            .HasDatabaseName("IX_V0PREMIOS_DateRange");

        builder.HasIndex(p => p.PolicyNumber)
            .HasDatabaseName("IX_V0PREMIOS_PolicyNumber");

        // Relationships
        builder.HasOne(p => p.Policy)
            .WithMany()
            .HasForeignKey(p => p.PolicyNumber)
            .OnDelete(DeleteBehavior.Restrict);

        builder.HasOne(p => p.Client)
            .WithMany()
            .HasForeignKey(p => p.ClientCode)
            .OnDelete(DeleteBehavior.Restrict);
    }
}
```

---

### 2. Policy (V0APOLICE)

**Purpose**: Insurance policy master data containing contract information, effective dates, and status.

**COBOL Source**: `V0APOLICE` view, accessed at sections R0980-00-SELECT-V0APOLICE, R0990-00-SELECT-EF-APOLICE

#### Properties

```csharp
public class Policy
{
    [Key]
    [CobolField(PicClause = "9(13)", Length = 13, FieldType = CobolFieldType.PackedDecimal)]
    public long PolicyNumber { get; set; }  // NUM_APOLICE

    [CobolField(PicClause = "9(9)", Length = 9)]
    public int CompanyCode { get; set; }  // COD_EMP

    [CobolField(PicClause = "9(4)", Length = 4)]
    public int ProductCode { get; set; }  // COD_PROD

    [CobolField(PicClause = "9(9)", Length = 9)]
    public int ClientCode { get; set; }  // COD_CLIEN (Policyholder)

    [CobolField(PicClause = "X(10)", Length = 10)]
    [MaxLength(10)]
    public string EffectiveDate { get; set; }  // DT_INIVIG (YYYY-MM-DD)

    public DateTime EffectiveDateParsed => DateTime.ParseExact(EffectiveDate, "yyyy-MM-dd", CultureInfo.InvariantCulture);

    [CobolField(PicClause = "X(10)", Length = 10)]
    [MaxLength(10)]
    public string ExpirationDate { get; set; }  // DT_FIMVIG

    public DateTime ExpirationDateParsed => DateTime.ParseExact(ExpirationDate, "yyyy-MM-dd", CultureInfo.InvariantCulture);

    [CobolField(PicClause = "X(10)", Length = 10)]
    [MaxLength(10)]
    public string IssuanceDate { get; set; }  // DT_EMIS

    public DateTime IssuanceDateParsed => DateTime.ParseExact(IssuanceDate, "yyyy-MM-dd", CultureInfo.InvariantCulture);

    [CobolField(PicClause = "X(10)", Length = 10)]
    [MaxLength(10)]
    public string ProposalDate { get; set; }  // DT_PROPT

    [CobolField(PicClause = "X(1)", Length = 1)]
    [MaxLength(1)]
    public string PolicyStatus { get; set; }  // IND_SITUACAO ('A'=Active, 'C'=Cancelled, etc.)

    [CobolField(PicClause = "9(4)", Length = 4)]
    public int AgencyCode { get; set; }  // COD_AGENC

    [CobolField(PicClause = "9(9)", Length = 9)]
    public int ProducerCode { get; set; }  // COD_PRODU

    [CobolField(PicClause = "9(13)", Length = 13)]
    public long ProposalNumber { get; set; }  // NUM_PROPT

    [CobolField(PicClause = "9(4)", Length = 4)]
    public int LineOfBusiness { get; set; }  // RAMO_SUSEP

    [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
    [Column(TypeName = "decimal(15,2)")]
    public decimal InsuredAmount { get; set; }  // IMP_SEG

    [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
    [Column(TypeName = "decimal(15,2)")]
    public decimal TotalPremium { get; set; }  // VL_PRM_TOT

    [CobolField(PicClause = "9(4)", Length = 4)]
    public int RenewalType { get; set; }  // TIP_RENOV

    // Navigation Properties
    public Product Product { get; set; }
    public Client Policyholder { get; set; }
    public Agency Agency { get; set; }
    public Producer Producer { get; set; }
    public ICollection<Endorsement> Endorsements { get; set; }
    public ICollection<Coverage> Coverages { get; set; }
}
```

#### Validation Rules
1. `EffectiveDate` must be <= `ExpirationDate`
2. `IssuanceDate` should be <= `EffectiveDate` (with exceptions for specific products)
3. `PolicyStatus` must be in `['A', 'C', 'S', 'E']` (Active, Cancelled, Suspended, Expired)
4. `PolicyNumber` must be unique per `CompanyCode`

#### EF Core Configuration

```csharp
public class PolicyConfiguration : IEntityTypeConfiguration<Policy>
{
    public void Configure(EntityTypeBuilder<Policy> builder)
    {
        builder.ToView("V0APOLICE");
        builder.HasKey(p => p.PolicyNumber);

        builder.HasIndex(p => new { p.CompanyCode, p.ProductCode })
            .HasDatabaseName("IX_V0APOLICE_Product");

        builder.HasIndex(p => p.ClientCode)
            .HasDatabaseName("IX_V0APOLICE_Client");

        builder.HasOne(p => p.Product)
            .WithMany()
            .HasForeignKey(p => p.ProductCode)
            .OnDelete(DeleteBehavior.Restrict);

        builder.HasOne(p => p.Policyholder)
            .WithMany()
            .HasForeignKey(p => p.ClientCode)
            .OnDelete(DeleteBehavior.Restrict);

        builder.HasOne(p => p.Agency)
            .WithMany()
            .HasForeignKey(p => p.AgencyCode)
            .OnDelete(DeleteBehavior.Restrict);

        builder.HasOne(p => p.Producer)
            .WithMany()
            .HasForeignKey(p => p.ProducerCode)
            .OnDelete(DeleteBehavior.Restrict);
    }
}
```

---

### 3. Endorsement (V0ENDOSSO)

**Purpose**: Policy modifications (endorsements) that alter coverage, premium, or other policy terms.

**COBOL Source**: `V0ENDOSSO` view, accessed at sections R0760-00-SELECT-V0ENDOSSO, R0780-00-SELECT-ENDOS-CANCLM

#### Properties

```csharp
public class Endorsement
{
    [Key]
    public long EndorsementId { get; set; }

    [CobolField(PicClause = "9(13)", Length = 13, FieldType = CobolFieldType.PackedDecimal)]
    public long PolicyNumber { get; set; }  // NUM_APOLICE

    [CobolField(PicClause = "9(9)", Length = 9)]
    public int EndorsementNumber { get; set; }  // NUM_ENDOS

    [CobolField(PicClause = "X(10)", Length = 10)]
    [MaxLength(10)]
    public string EndorsementDate { get; set; }  // DT_ENDOS

    public DateTime EndorsementDateParsed => DateTime.ParseExact(EndorsementDate, "yyyy-MM-dd", CultureInfo.InvariantCulture);

    [CobolField(PicClause = "9(4)", Length = 4)]
    public int EndorsementType { get; set; }  // TIP_ENDOS

    [CobolField(PicClause = "X(1)", Length = 1)]
    [MaxLength(1)]
    public string EndorsementStatus { get; set; }  // IND_SITUACAO

    [CobolField(PicClause = "X(1)", Length = 1)]
    [MaxLength(1)]
    public string CancellationFlag { get; set; }  // IND_CANCELM ('S'/'N')

    [CobolField(PicClause = "X(10)", Length = 10)]
    [MaxLength(10)]
    public string CancellationDate { get; set; }  // DT_CANCELM

    [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
    [Column(TypeName = "decimal(15,2)")]
    public decimal PremiumImpact { get; set; }  // VL_PRM_ENDOS (can be negative for cancellations)

    [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
    [Column(TypeName = "decimal(15,2)")]
    public decimal InsuredAmountChange { get; set; }  // IMP_SEG_ENDOS

    [CobolField(PicClause = "9(4)", Length = 4)]
    public int CancellationReason { get; set; }  // COD_MOTIVO_CANCEL

    [CobolField(PicClause = "X(200)", Length = 200)]
    [MaxLength(200)]
    public string EndorsementDescription { get; set; }  // DES_ENDOS

    // Navigation Properties
    public Policy Policy { get; set; }
}
```

#### Validation Rules
1. `PolicyNumber` must exist in `Policy` entity
2. `EndorsementNumber` must be > 0
3. `CancellationFlag` must be 'S' or 'N'
4. If `CancellationFlag` = 'S', `CancellationDate` must not be null
5. `EndorsementDate` must be >= Policy `EffectiveDate`

#### EF Core Configuration

```csharp
public class EndorsementConfiguration : IEntityTypeConfiguration<Endorsement>
{
    public void Configure(EntityTypeBuilder<Endorsement> builder)
    {
        builder.ToView("V0ENDOSSO");
        builder.HasKey(e => e.EndorsementId);

        builder.HasIndex(e => new { e.PolicyNumber, e.EndorsementNumber })
            .IsUnique()
            .HasDatabaseName("IX_V0ENDOSSO_PolicyEndorsement");

        builder.HasIndex(e => e.CancellationFlag)
            .HasDatabaseName("IX_V0ENDOSSO_Cancellation");

        builder.HasOne(e => e.Policy)
            .WithMany(p => p.Endorsements)
            .HasForeignKey(e => e.PolicyNumber)
            .OnDelete(DeleteBehavior.Restrict);
    }
}
```

---

### 4. Product (V0PRODUTO, V0PRODUTOSVG)

**Purpose**: Insurance product definitions including SUSEP codes, line of business classification, and product metadata.

**COBOL Source**: `V0PRODUTO` view (R0740-00-SELECT-V0PRODUTO), `V0PRODUTOSVG` view (R1020-00-SELECT-V0PRODUTOSVG)

#### Properties

```csharp
public class Product
{
    [Key]
    [CobolField(PicClause = "9(4)", Length = 4)]
    public int ProductCode { get; set; }  // COD_PROD

    [CobolField(PicClause = "9(9)", Length = 9)]
    public int CompanyCode { get; set; }  // COD_EMP

    [CobolField(PicClause = "X(100)", Length = 100)]
    [MaxLength(100)]
    public string ProductName { get; set; }  // NOM_PROD

    [CobolField(PicClause = "9(4)", Length = 4)]
    public int LineOfBusiness { get; set; }  // RAMO_SUSEP

    [CobolField(PicClause = "9(4)", Length = 4)]
    public int LineOfBusinessGroup { get; set; }  // GRUPO_RAMO_SUSEP

    [CobolField(PicClause = "X(20)", Length = 20)]
    [MaxLength(20)]
    public string SusepProcessNumber { get; set; }  // NUM_PROC_SUSEP

    [CobolField(PicClause = "X(1)", Length = 1)]
    [MaxLength(1)]
    public string ProductType { get; set; }  // TIP_PROD ('A'=Auto, 'V'=Vida, 'R'=Residencial, etc.)

    [CobolField(PicClause = "X(1)", Length = 1)]
    [MaxLength(1)]
    public string ProductStatus { get; set; }  // IND_SITUACAO ('A'=Active, 'I'=Inactive)

    [CobolField(PicClause = "9(4)", Length = 4)]
    public int ProductModality { get; set; }  // MODALIFR

    [CobolField(PicClause = "X(1)", Length = 1)]
    [MaxLength(1)]
    public string IsLifeInsurance { get; set; }  // IND_VIDA_GRUPO ('S'/'N')

    // Navigation Properties
    public ICollection<Policy> Policies { get; set; }
}
```

#### Validation Rules
1. `ProductCode` must be unique per `CompanyCode`
2. `LineOfBusiness` must be valid SUSEP code (e.g., 0118 = Auto, 0969 = Vida Individual)
3. `ProductStatus` must be in `['A', 'I', 'S']` (Active, Inactive, Suspended)
4. `SusepProcessNumber` format must match SUSEP standards (e.g., "15414.900XXX/XXXX-XX")

#### EF Core Configuration

```csharp
public class ProductConfiguration : IEntityTypeConfiguration<Product>
{
    public void Configure(EntityTypeBuilder<Product> builder)
    {
        builder.ToView("V0PRODUTO");
        builder.HasKey(p => p.ProductCode);

        builder.HasIndex(p => new { p.CompanyCode, p.LineOfBusiness })
            .HasDatabaseName("IX_V0PRODUTO_LineOfBusiness");

        builder.HasIndex(p => p.ProductStatus)
            .HasDatabaseName("IX_V0PRODUTO_Status");
    }
}
```

---

### 5. Client (V0CLIENTE, V0TOMADOR)

**Purpose**: Customer/party information for policyholders, insured parties, and beneficiaries.

**COBOL Source**: `V0CLIENTE` view (R0960-00-SELECT-V0CLIENTE), `V0TOMADOR` view (R1140-00-SELECT-V0TOMADOR)

#### Properties

```csharp
public class Client
{
    [Key]
    [CobolField(PicClause = "9(9)", Length = 9)]
    public int ClientCode { get; set; }  // COD_CLIEN

    [CobolField(PicClause = "9(9)", Length = 9)]
    public int CompanyCode { get; set; }  // COD_EMP

    [CobolField(PicClause = "X(100)", Length = 100)]
    [MaxLength(100)]
    public string ClientName { get; set; }  // NOM_CLIEN

    [CobolField(PicClause = "X(1)", Length = 1)]
    [MaxLength(1)]
    public string ClientType { get; set; }  // TIP_PESSOA ('F'=Fisica/Person, 'J'=Juridica/Company)

    [CobolField(PicClause = "X(14)", Length = 14)]
    [MaxLength(14)]
    public string DocumentNumber { get; set; }  // NUM_CPF_CNPJ (CPF or CNPJ)

    [CobolField(PicClause = "X(20)", Length = 20)]
    [MaxLength(20)]
    public string IdentityDocument { get; set; }  // NUM_RG_IE

    [CobolField(PicClause = "X(10)", Length = 10)]
    [MaxLength(10)]
    public string BirthDate { get; set; }  // DT_NASC (YYYY-MM-DD)

    public DateTime? BirthDateParsed => string.IsNullOrWhiteSpace(BirthDate)
        ? null
        : DateTime.ParseExact(BirthDate, "yyyy-MM-dd", CultureInfo.InvariantCulture);

    [CobolField(PicClause = "X(1)", Length = 1)]
    [MaxLength(1)]
    public string Gender { get; set; }  // IND_SEXO ('M', 'F')

    [CobolField(PicClause = "X(50)", Length = 50)]
    [MaxLength(50)]
    public string Email { get; set; }  // DES_EMAIL

    [CobolField(PicClause = "X(20)", Length = 20)]
    [MaxLength(20)]
    public string PhoneNumber { get; set; }  // NUM_FONE

    [CobolField(PicClause = "X(1)", Length = 1)]
    [MaxLength(1)]
    public string ClientStatus { get; set; }  // IND_SITUACAO ('A'=Active, 'I'=Inactive)

    // Navigation Properties
    public ICollection<Address> Addresses { get; set; }
}
```

#### Validation Rules
1. `ClientCode` must be unique per `CompanyCode`
2. `ClientType` must be 'F' or 'J'
3. If `ClientType` = 'F', `DocumentNumber` must be valid CPF (11 digits)
4. If `ClientType` = 'J', `DocumentNumber` must be valid CNPJ (14 digits)
5. `Email` must match email format regex if not null
6. `Gender` must be in `['M', 'F', 'O']` (Male, Female, Other)

#### EF Core Configuration

```csharp
public class ClientConfiguration : IEntityTypeConfiguration<Client>
{
    public void Configure(EntityTypeBuilder<Client> builder)
    {
        builder.ToView("V0CLIENTE");
        builder.HasKey(c => c.ClientCode);

        builder.HasIndex(c => c.DocumentNumber)
            .IsUnique()
            .HasDatabaseName("IX_V0CLIENTE_DocumentNumber");

        builder.HasIndex(c => c.ClientType)
            .HasDatabaseName("IX_V0CLIENTE_Type");
    }
}
```

---

### 6. Address (V0ENDERECOS)

**Purpose**: Address information for clients, agencies, and other parties. COBOL uses cursor processing for multiple addresses per client.

**COBOL Source**: `V0ENDERECOS` view, cursor declared at R1230-00-DECLARE-V0ENDERECOS, fetched at R1240-00-FETCH-V0ENDERECOS

#### Properties

```csharp
public class Address
{
    [Key]
    public long AddressId { get; set; }

    [CobolField(PicClause = "9(9)", Length = 9)]
    public int ClientCode { get; set; }  // COD_CLIEN

    [CobolField(PicClause = "9(4)", Length = 4)]
    public int AddressSequence { get; set; }  // SEQ_ENDER

    [CobolField(PicClause = "X(1)", Length = 1)]
    [MaxLength(1)]
    public string AddressType { get; set; }  // TIP_ENDER ('R'=Residential, 'C'=Commercial, etc.)

    [CobolField(PicClause = "X(100)", Length = 100)]
    [MaxLength(100)]
    public string StreetAddress { get; set; }  // DES_LOGRADOURO

    [CobolField(PicClause = "X(10)", Length = 10)]
    [MaxLength(10)]
    public string Number { get; set; }  // NUM_LOGRA

    [CobolField(PicClause = "X(50)", Length = 50)]
    [MaxLength(50)]
    public string Complement { get; set; }  // DES_COMPL

    [CobolField(PicClause = "X(50)", Length = 50)]
    [MaxLength(50)]
    public string Neighborhood { get; set; }  // DES_BAIRRO

    [CobolField(PicClause = "X(50)", Length = 50)]
    [MaxLength(50)]
    public string City { get; set; }  // NOM_MUNIC

    [CobolField(PicClause = "X(2)", Length = 2)]
    [MaxLength(2)]
    public string State { get; set; }  // COD_UF (e.g., 'SP', 'RJ')

    [CobolField(PicClause = "X(8)", Length = 8)]
    [MaxLength(8)]
    public string PostalCode { get; set; }  // NUM_CEP (Brazilian CEP format XXXXX-XXX)

    [CobolField(PicClause = "9(4)", Length = 4)]
    public int CountryCode { get; set; }  // COD_PAIS (default 1058 = Brazil)

    // Navigation Properties
    public Client Client { get; set; }
}
```

#### Validation Rules
1. `ClientCode` must exist in `Client` entity
2. `AddressSequence` must be > 0
3. Combination of `ClientCode` + `AddressSequence` must be unique
4. `State` must be valid Brazilian UF code (e.g., 'SP', 'RJ', 'MG')
5. `PostalCode` must match Brazilian CEP format (NNNNN-NNN)
6. `AddressType` must be in `['R', 'C', 'P', 'O']` (Residential, Commercial, Postal, Other)

#### EF Core Configuration

```csharp
public class AddressConfiguration : IEntityTypeConfiguration<Address>
{
    public void Configure(EntityTypeBuilder<Address> builder)
    {
        builder.ToView("V0ENDERECOS");
        builder.HasKey(a => a.AddressId);

        builder.HasIndex(a => new { a.ClientCode, a.AddressSequence })
            .IsUnique()
            .HasDatabaseName("IX_V0ENDERECOS_ClientSequence");

        builder.HasIndex(a => a.State)
            .HasDatabaseName("IX_V0ENDERECOS_State");

        builder.HasOne(a => a.Client)
            .WithMany(c => c.Addresses)
            .HasForeignKey(a => a.ClientCode)
            .OnDelete(DeleteBehavior.Restrict);
    }
}
```

---

### 7. Agency (V0AGENCIAS)

**Purpose**: Sales agency/branch information for distribution channel tracking.

**COBOL Source**: `V0AGENCIAS` view, accessed at R1180-00-SELECT-V0AGENCIAS

#### Properties

```csharp
public class Agency
{
    [Key]
    [CobolField(PicClause = "9(4)", Length = 4)]
    public int AgencyCode { get; set; }  // COD_AGENC

    [CobolField(PicClause = "9(9)", Length = 9)]
    public int CompanyCode { get; set; }  // COD_EMP

    [CobolField(PicClause = "X(100)", Length = 100)]
    [MaxLength(100)]
    public string AgencyName { get; set; }  // NOM_AGENC

    [CobolField(PicClause = "9(4)", Length = 4)]
    public int RegionCode { get; set; }  // COD_REGIAO

    [CobolField(PicClause = "X(2)", Length = 2)]
    [MaxLength(2)]
    public string State { get; set; }  // COD_UF

    [CobolField(PicClause = "X(1)", Length = 1)]
    [MaxLength(1)]
    public string ChannelType { get; set; }  // TIP_CANAL ('A'=Agency, 'C'=Call Center, 'W'=Web, etc.)

    [CobolField(PicClause = "X(1)", Length = 1)]
    [MaxLength(1)]
    public string AgencyStatus { get; set; }  // IND_SITUACAO ('A'=Active, 'I'=Inactive)

    // Navigation Properties
    public ICollection<Policy> Policies { get; set; }
}
```

#### Validation Rules
1. `AgencyCode` must be unique per `CompanyCode`
2. `State` must be valid Brazilian UF code
3. `ChannelType` must be in `['A', 'C', 'W', 'P']` (Agency, Call Center, Web, Partner)
4. `AgencyStatus` must be in `['A', 'I']`

#### EF Core Configuration

```csharp
public class AgencyConfiguration : IEntityTypeConfiguration<Agency>
{
    public void Configure(EntityTypeBuilder<Agency> builder)
    {
        builder.ToView("V0AGENCIAS");
        builder.HasKey(a => a.AgencyCode);

        builder.HasIndex(a => new { a.CompanyCode, a.RegionCode })
            .HasDatabaseName("IX_V0AGENCIAS_Region");
    }
}
```

---

### 8. Producer (V0PRODUTOR)

**Purpose**: Insurance broker/producer information for commission tracking and distribution management.

**COBOL Source**: `V0PRODUTOR` view, accessed at R1200-00-SELECT-V0PRODUTOR

#### Properties

```csharp
public class Producer
{
    [Key]
    [CobolField(PicClause = "9(9)", Length = 9)]
    public int ProducerCode { get; set; }  // COD_PRODU

    [CobolField(PicClause = "9(9)", Length = 9)]
    public int CompanyCode { get; set; }  // COD_EMP

    [CobolField(PicClause = "X(100)", Length = 100)]
    [MaxLength(100)]
    public string ProducerName { get; set; }  // NOM_PRODU

    [CobolField(PicClause = "X(14)", Length = 14)]
    [MaxLength(14)]
    public string DocumentNumber { get; set; }  // NUM_CPF_CNPJ

    [CobolField(PicClause = "X(20)", Length = 20)]
    [MaxLength(20)]
    public string SusepRegistration { get; set; }  // NUM_REG_SUSEP

    [CobolField(PicClause = "9(4)V99", Length = 6, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
    [Column(TypeName = "decimal(6,2)")]
    public decimal CommissionRate { get; set; }  // PCT_COMIS (percentage)

    [CobolField(PicClause = "X(1)", Length = 1)]
    [MaxLength(1)]
    public string ProducerType { get; set; }  // TIP_PRODU ('C'=Corretor, 'A'=Agenciador, 'I'=Indicador)

    [CobolField(PicClause = "X(1)", Length = 1)]
    [MaxLength(1)]
    public string ProducerStatus { get; set; }  // IND_SITUACAO ('A'=Active, 'I'=Inactive)

    // Navigation Properties
    public ICollection<Policy> Policies { get; set; }
}
```

#### Validation Rules
1. `ProducerCode` must be unique per `CompanyCode`
2. `DocumentNumber` must be valid CPF or CNPJ
3. `SusepRegistration` format must match SUSEP broker registration standards
4. `CommissionRate` must be between 0.00 and 100.00
5. `ProducerType` must be in `['C', 'A', 'I']` (Corretor, Agenciador, Indicador)

#### EF Core Configuration

```csharp
public class ProducerConfiguration : IEntityTypeConfiguration<Producer>
{
    public void Configure(EntityTypeBuilder<Producer> builder)
    {
        builder.ToView("V0PRODUTOR");
        builder.HasKey(p => p.ProducerCode);

        builder.HasIndex(p => p.SusepRegistration)
            .IsUnique()
            .HasDatabaseName("IX_V0PRODUTOR_SusepReg");
    }
}
```

---

### 9. Coverage (V0COBERAPOL)

**Purpose**: Policy coverage details including insured amounts, rates, and coverage-specific premiums.

**COBOL Source**: `V0COBERAPOL` view, accessed at sections R0850-00-SELECT-V0COBERAPOL, R1250-00-SELECT-V0COBERAPOL

#### Properties

```csharp
public class Coverage
{
    [Key]
    public long CoverageId { get; set; }

    [CobolField(PicClause = "9(13)", Length = 13, FieldType = CobolFieldType.PackedDecimal)]
    public long PolicyNumber { get; set; }  // NUM_APOLICE

    [CobolField(PicClause = "9(4)", Length = 4)]
    public int CoverageCode { get; set; }  // COD_COBER

    [CobolField(PicClause = "X(100)", Length = 100)]
    [MaxLength(100)]
    public string CoverageName { get; set; }  // NOM_COBER

    [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
    [Column(TypeName = "decimal(15,2)")]
    public decimal InsuredAmount { get; set; }  // IMP_SEG

    [CobolField(PicClause = "9(4)V9999", Length = 8, DecimalPlaces = 4, FieldType = CobolFieldType.PackedDecimal)]
    [Column(TypeName = "decimal(8,4)")]
    public decimal Rate { get; set; }  // PCT_TAXA (per thousand or percentage)

    [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
    [Column(TypeName = "decimal(15,2)")]
    public decimal TariffPremium { get; set; }  // VL_PRM_TAR

    [CobolField(PicClause = "X(1)", Length = 1)]
    [MaxLength(1)]
    public string CoverageType { get; set; }  // TIP_COBER ('B'=Basica, 'A'=Adicional)

    [CobolField(PicClause = "X(1)", Length = 1)]
    [MaxLength(1)]
    public string IsMandatory { get; set; }  // IND_OBRIGAT ('S'/'N')

    // Navigation Properties
    public Policy Policy { get; set; }
}
```

#### Validation Rules
1. `PolicyNumber` must exist in `Policy` entity
2. `InsuredAmount` must be > 0 for active coverages
3. `Rate` must be >= 0
4. `CoverageType` must be in `['B', 'A']` (Basica, Adicional)
5. `IsMandatory` must be 'S' or 'N'

#### EF Core Configuration

```csharp
public class CoverageConfiguration : IEntityTypeConfiguration<Coverage>
{
    public void Configure(EntityTypeBuilder<Coverage> builder)
    {
        builder.ToView("V0COBERAPOL");
        builder.HasKey(c => c.CoverageId);

        builder.HasIndex(c => new { c.PolicyNumber, c.CoverageCode })
            .IsUnique()
            .HasDatabaseName("IX_V0COBERAPOL_PolicyCoverage");

        builder.HasOne(c => c.Policy)
            .WithMany(p => p.Coverages)
            .HasForeignKey(c => c.PolicyNumber)
            .OnDelete(DeleteBehavior.Restrict);
    }
}
```

---

### 10. Invoice (V0FATURAS)

**Purpose**: Billing invoice information for premium collection tracking.

**COBOL Source**: `V0FATURAS` view, accessed at R1060-00-SELECT-V0FATURAS

#### Properties

```csharp
public class Invoice
{
    [Key]
    [CobolField(PicClause = "9(13)", Length = 13)]
    public long InvoiceNumber { get; set; }  // NUM_FATURA

    [CobolField(PicClause = "9(13)", Length = 13, FieldType = CobolFieldType.PackedDecimal)]
    public long PolicyNumber { get; set; }  // NUM_APOLICE

    [CobolField(PicClause = "9(9)", Length = 9)]
    public int EndorsementNumber { get; set; }  // NUM_ENDOS

    [CobolField(PicClause = "X(10)", Length = 10)]
    [MaxLength(10)]
    public string InvoiceDate { get; set; }  // DT_FATURA

    public DateTime InvoiceDateParsed => DateTime.ParseExact(InvoiceDate, "yyyy-MM-dd", CultureInfo.InvariantCulture);

    [CobolField(PicClause = "X(10)", Length = 10)]
    [MaxLength(10)]
    public string DueDate { get; set; }  // DT_VENCTO

    public DateTime DueDateParsed => DateTime.ParseExact(DueDate, "yyyy-MM-dd", CultureInfo.InvariantCulture);

    [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
    [Column(TypeName = "decimal(15,2)")]
    public decimal InvoiceAmount { get; set; }  // VL_FATURA

    [CobolField(PicClause = "9(4)", Length = 4)]
    public int NumberOfInstallments { get; set; }  // QTD_PARCELAS

    [CobolField(PicClause = "X(1)", Length = 1)]
    [MaxLength(1)]
    public string PaymentStatus { get; set; }  // IND_SITUACAO ('A'=Aguardando, 'P'=Pago, 'V'=Vencido)

    // Navigation Properties
    public Policy Policy { get; set; }
    public ICollection<Installment> Installments { get; set; }
}
```

#### Validation Rules
1. `PolicyNumber` must exist in `Policy` entity
2. `DueDate` must be >= `InvoiceDate`
3. `InvoiceAmount` must be > 0
4. `NumberOfInstallments` must be >= 1
5. `PaymentStatus` must be in `['A', 'P', 'V', 'C']` (Aguardando, Pago, Vencido, Cancelado)

#### EF Core Configuration

```csharp
public class InvoiceConfiguration : IEntityTypeConfiguration<Invoice>
{
    public void Configure(EntityTypeBuilder<Invoice> builder)
    {
        builder.ToView("V0FATURAS");
        builder.HasKey(i => i.InvoiceNumber);

        builder.HasIndex(i => new { i.PolicyNumber, i.EndorsementNumber })
            .HasDatabaseName("IX_V0FATURAS_PolicyEndorsement");

        builder.HasOne(i => i.Policy)
            .WithMany()
            .HasForeignKey(i => i.PolicyNumber)
            .OnDelete(DeleteBehavior.Restrict);
    }
}
```

---

### 11. Installment (V0HISTOPARC)

**Purpose**: Premium installment payment history for tracking collection status.

**COBOL Source**: `V0HISTOPARC` view, accessed at R0800-00-SELECT-V0HISTOPARC

#### Properties

```csharp
public class Installment
{
    [Key]
    public long InstallmentId { get; set; }

    [CobolField(PicClause = "9(13)", Length = 13)]
    public long InvoiceNumber { get; set; }  // NUM_FATURA

    [CobolField(PicClause = "9(4)", Length = 4)]
    public int InstallmentNumber { get; set; }  // NUM_PARCELA

    [CobolField(PicClause = "X(10)", Length = 10)]
    [MaxLength(10)]
    public string DueDate { get; set; }  // DT_VENCTO

    public DateTime DueDateParsed => DateTime.ParseExact(DueDate, "yyyy-MM-dd", CultureInfo.InvariantCulture);

    [CobolField(PicClause = "X(10)", Length = 10)]
    [MaxLength(10)]
    public string PaymentDate { get; set; }  // DT_PAGTO

    public DateTime? PaymentDateParsed => string.IsNullOrWhiteSpace(PaymentDate)
        ? null
        : DateTime.ParseExact(PaymentDate, "yyyy-MM-dd", CultureInfo.InvariantCulture);

    [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
    [Column(TypeName = "decimal(15,2)")]
    public decimal InstallmentAmount { get; set; }  // VL_PARCELA

    [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
    [Column(TypeName = "decimal(15,2)")]
    public decimal AmountPaid { get; set; }  // VL_PAGO

    [CobolField(PicClause = "X(1)", Length = 1)]
    [MaxLength(1)]
    public string PaymentStatus { get; set; }  // IND_SITUACAO ('P'=Pago, 'A'=Aguardando, 'V'=Vencido)

    // Navigation Properties
    public Invoice Invoice { get; set; }
}
```

#### Validation Rules
1. `InvoiceNumber` must exist in `Invoice` entity
2. `InstallmentNumber` must be > 0 and <= `Invoice.NumberOfInstallments`
3. If `PaymentStatus` = 'P', `PaymentDate` must not be null
4. `AmountPaid` must be <= `InstallmentAmount`
5. `PaymentStatus` must be in `['P', 'A', 'V', 'C']`

#### EF Core Configuration

```csharp
public class InstallmentConfiguration : IEntityTypeConfiguration<Installment>
{
    public void Configure(EntityTypeBuilder<Installment> builder)
    {
        builder.ToView("V0HISTOPARC");
        builder.HasKey(i => i.InstallmentId);

        builder.HasIndex(i => new { i.InvoiceNumber, i.InstallmentNumber })
            .IsUnique()
            .HasDatabaseName("IX_V0HISTOPARC_InvoiceInstallment");

        builder.HasOne(i => i.Invoice)
            .WithMany(inv => inv.Installments)
            .HasForeignKey(i => i.InvoiceNumber)
            .OnDelete(DeleteBehavior.Restrict);
    }
}
```

---

### 12. CossuredPolicy (V0APOLCOSCED)

**Purpose**: Cossurance and ceded reinsurance policy arrangements where risk is shared among multiple insurers.

**COBOL Source**: `V0APOLCOSCED` view, cursor declared at R4900-00-DECLARE-V0APOLCOSCED, fetched at R5000-00-FETCH-V0APOLCOSCED

#### Properties

```csharp
public class CossuredPolicy
{
    [Key]
    public long CossuranceId { get; set; }

    [CobolField(PicClause = "9(13)", Length = 13, FieldType = CobolFieldType.PackedDecimal)]
    public long PolicyNumber { get; set; }  // NUM_APOLICE

    [CobolField(PicClause = "9(4)", Length = 4)]
    public int CossuranceCode { get; set; }  // COD_COSSG

    [CobolField(PicClause = "X(1)", Length = 1)]
    [MaxLength(1)]
    public string CossuranceType { get; set; }  // TIP_COSSG ('C'=Cosseguro, 'R'=Resseguro, 'E'=Retrocessao)

    [CobolField(PicClause = "9(9)", Length = 9)]
    public int CedingCompanyCode { get; set; }  // COD_CIA_CEDENTE

    [CobolField(PicClause = "9(9)", Length = 9)]
    public int AcquiringCompanyCode { get; set; }  // COD_CIA_CESSIONARIA

    [CobolField(PicClause = "9(4)V9(9)", Length = 14, DecimalPlaces = 9, FieldType = CobolFieldType.PackedDecimal)]
    [Column(TypeName = "decimal(13,9)")]
    public decimal PercentageShare { get; set; }  // PCT_PARTICIPACAO (0.000000001 to 1.000000000)

    [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
    [Column(TypeName = "decimal(15,2)")]
    public decimal CededInsuredAmount { get; set; }  // IMP_SEG_CEDIDO

    [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
    [Column(TypeName = "decimal(15,2)")]
    public decimal CededPremium { get; set; }  // VL_PRM_CEDIDO

    [CobolField(PicClause = "X(1)", Length = 1)]
    [MaxLength(1)]
    public string IsLeader { get; set; }  // IND_LIDER ('S'/'N' - for cossurance)

    // Navigation Properties
    public Policy Policy { get; set; }
}
```

#### Validation Rules
1. `PolicyNumber` must exist in `Policy` entity
2. `CossuranceType` must be in `['C', 'R', 'E']` (Cosseguro, Resseguro, Retrocessao)
3. `PercentageShare` must be > 0 and <= 1.0
4. `CededPremium` must be >= 0
5. `IsLeader` must be 'S' or 'N'

#### EF Core Configuration

```csharp
public class CossuredPolicyConfiguration : IEntityTypeConfiguration<CossuredPolicy>
{
    public void Configure(EntityTypeBuilder<CossuredPolicy> builder)
    {
        builder.ToView("V0APOLCOSCED");
        builder.HasKey(c => c.CossuranceId);

        builder.HasIndex(c => new { c.PolicyNumber, c.CossuranceCode })
            .HasDatabaseName("IX_V0APOLCOSCED_PolicyCossurance");

        builder.HasIndex(c => c.CossuranceType)
            .HasDatabaseName("IX_V0APOLCOSCED_Type");

        builder.HasOne(c => c.Policy)
            .WithMany()
            .HasForeignKey(c => c.PolicyNumber)
            .OnDelete(DeleteBehavior.Restrict);
    }
}
```

---

### 13. CossuranceCalculation (GE399)

**Purpose**: Detailed cossurance calculation data for quota distribution among cossurers.

**COBOL Source**: `GE399` table, cursor declared at R5300-00-DECLARE-GE399, fetched at R5400-00-FETCH-GE399

#### Properties

```csharp
public class CossuranceCalculation
{
    [Key]
    public long CalculationId { get; set; }

    [CobolField(PicClause = "9(13)", Length = 13, FieldType = CobolFieldType.PackedDecimal)]
    public long PolicyNumber { get; set; }  // NUM_APOLICE

    [CobolField(PicClause = "9(4)", Length = 4)]
    public int CossuranceCode { get; set; }  // COD_COSSG

    [CobolField(PicClause = "9(4)V9(9)", Length = 14, DecimalPlaces = 9, FieldType = CobolFieldType.PackedDecimal)]
    [Column(TypeName = "decimal(13,9)")]
    public decimal QuotaPercentage { get; set; }  // PCT_QUOTA

    [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
    [Column(TypeName = "decimal(15,2)")]
    public decimal RetainedPremium { get; set; }  // VL_PRM_RETIDO

    [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
    [Column(TypeName = "decimal(15,2)")]
    public decimal CededPremium { get; set; }  // VL_PRM_CEDIDO

    [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
    [Column(TypeName = "decimal(15,2)")]
    public decimal CededCommission { get; set; }  // VL_COMIS_CEDIDA

    // Navigation Properties
    public Policy Policy { get; set; }
}
```

#### Validation Rules
1. `PolicyNumber` must exist in `Policy` entity
2. `QuotaPercentage` must be > 0 and <= 1.0
3. `RetainedPremium` + `CededPremium` should equal total premium (within tolerance)
4. All financial values must be >= 0

#### EF Core Configuration

```csharp
public class CossuranceCalculationConfiguration : IEntityTypeConfiguration<CossuranceCalculation>
{
    public void Configure(EntityTypeBuilder<CossuranceCalculation> builder)
    {
        builder.ToTable("GE399");
        builder.HasKey(c => c.CalculationId);

        builder.HasIndex(c => new { c.PolicyNumber, c.CossuranceCode })
            .HasDatabaseName("IX_GE399_PolicyCossurance");

        builder.HasOne(c => c.Policy)
            .WithMany()
            .HasForeignKey(c => c.PolicyNumber)
            .OnDelete(DeleteBehavior.Restrict);
    }
}
```

---

### 14. SystemConfiguration (V0SISTEMA)

**Purpose**: System configuration parameters including processing dates and system identifiers.

**COBOL Source**: `V0SISTEMA` view, accessed at R0100-00-SELECT-SISTEMAS

#### Properties

```csharp
public class SystemConfiguration
{
    [Key]
    [CobolField(PicClause = "X(2)", Length = 2)]
    [MaxLength(2)]
    public string SystemId { get; set; }  // IDE_SISTEMA (e.g., 'GL', 'RG')

    [CobolField(PicClause = "X(10)", Length = 10)]
    [MaxLength(10)]
    public string ProcessingDate { get; set; }  // DT_MOVABE (YYYY-MM-DD)

    public DateTime ProcessingDateParsed => DateTime.ParseExact(ProcessingDate, "yyyy-MM-dd", CultureInfo.InvariantCulture);

    [CobolField(PicClause = "X(100)", Length = 100)]
    [MaxLength(100)]
    public string SystemName { get; set; }  // NOM_SISTEMA

    [CobolField(PicClause = "X(1)", Length = 1)]
    [MaxLength(1)]
    public string SystemStatus { get; set; }  // IND_SITUACAO ('A'=Active, 'I'=Inactive)

    [CobolField(PicClause = "9(9)", Length = 9)]
    public int CompanyCode { get; set; }  // COD_EMP
}
```

#### Validation Rules
1. `SystemId` must be unique
2. `ProcessingDate` must be valid date
3. `SystemStatus` must be in `['A', 'I']`

#### EF Core Configuration

```csharp
public class SystemConfigurationConfiguration : IEntityTypeConfiguration<SystemConfiguration>
{
    public void Configure(EntityTypeBuilder<SystemConfiguration> builder)
    {
        builder.ToView("V0SISTEMA");
        builder.HasKey(s => s.SystemId);
    }
}
```

---

### 15. ReportDefinition (V0RELATORIOS)

**Purpose**: Report execution metadata including user, date range, and status tracking.

**COBOL Source**: `V0RELATORIOS` view, accessed at R0200-00-SELECT-V0RELATORIO, R0300-00-DELETE-V0RELATORIO

#### Properties

```csharp
public class ReportDefinition
{
    [Key]
    public long ReportId { get; set; }

    [CobolField(PicClause = "X(8)", Length = 8)]
    [MaxLength(8)]
    public string UserCode { get; set; }  // COD_USUARIO

    [CobolField(PicClause = "X(10)", Length = 10)]
    [MaxLength(10)]
    public string RequestDate { get; set; }  // DTA_SOLICTA

    public DateTime RequestDateParsed => DateTime.ParseExact(RequestDate, "yyyy-MM-dd", CultureInfo.InvariantCulture);

    [CobolField(PicClause = "X(2)", Length = 2)]
    [MaxLength(2)]
    public string SystemId { get; set; }  // IDE_SISTEMA

    [CobolField(PicClause = "X(8)", Length = 8)]
    [MaxLength(8)]
    public string ReportCode { get; set; }  // COD_RELAT

    [CobolField(PicClause = "X(10)", Length = 10)]
    [MaxLength(10)]
    public string PeriodStart { get; set; }  // PERI_INICIAL

    [CobolField(PicClause = "X(10)", Length = 10)]
    [MaxLength(10)]
    public string PeriodEnd { get; set; }  // PERI_FINAL

    [CobolField(PicClause = "9(4)", Length = 4)]
    public int ReferenceYear { get; set; }  // ANO_REFER

    [CobolField(PicClause = "9(4)", Length = 4)]
    public int ReferenceMonth { get; set; }  // MES_REFER

    [CobolField(PicClause = "X(10)", Length = 10)]
    [MaxLength(10)]
    public string ReferenceDate { get; set; }  // DATA_REFR
}
```

#### Validation Rules
1. `RequestDate` must be valid date
2. `PeriodStart` must be <= `PeriodEnd`
3. `ReferenceMonth` must be 1-12
4. `ReferenceYear` must be >= 2000

#### EF Core Configuration

```csharp
public class ReportDefinitionConfiguration : IEntityTypeConfiguration<ReportDefinition>
{
    public void Configure(EntityTypeBuilder<ReportDefinition> builder)
    {
        builder.ToView("V0RELATORIOS");
        builder.HasKey(r => r.ReportId);

        builder.HasIndex(r => new { r.SystemId, r.ReportCode, r.RequestDate })
            .HasDatabaseName("IX_V0RELATORIOS_Request");
    }
}
```

---

## Supporting Infrastructure

### Custom Attribute for COBOL Field Metadata

```csharp
[AttributeUsage(AttributeTargets.Property)]
public class CobolFieldAttribute : Attribute
{
    public string PicClause { get; set; }
    public int Length { get; set; }
    public int DecimalPlaces { get; set; }
    public CobolFieldType FieldType { get; set; } = CobolFieldType.Display;
}

public enum CobolFieldType
{
    Display,          // PIC X(n) or PIC 9(n) - standard display
    Numeric,          // PIC 9(n) COMP - binary integer
    PackedDecimal,    // PIC 9(n) COMP-3 - packed decimal
    SignedNumeric     // PIC S9(n) COMP - signed binary
}
```

### DbContext Configuration

```csharp
public class PremiumReportingDbContext : DbContext
{
    public DbSet<PremiumRecord> PremiumRecords { get; set; }
    public DbSet<Policy> Policies { get; set; }
    public DbSet<Endorsement> Endorsements { get; set; }
    public DbSet<Product> Products { get; set; }
    public DbSet<Client> Clients { get; set; }
    public DbSet<Address> Addresses { get; set; }
    public DbSet<Agency> Agencies { get; set; }
    public DbSet<Producer> Producers { get; set; }
    public DbSet<Coverage> Coverages { get; set; }
    public DbSet<Invoice> Invoices { get; set; }
    public DbSet<Installment> Installments { get; set; }
    public DbSet<CossuredPolicy> CossuredPolicies { get; set; }
    public DbSet<CossuranceCalculation> CossuranceCalculations { get; set; }
    public DbSet<SystemConfiguration> SystemConfigurations { get; set; }
    public DbSet<ReportDefinition> ReportDefinitions { get; set; }

    protected override void OnModelCreating(ModelBuilder modelBuilder)
    {
        modelBuilder.ApplyConfiguration(new PremiumRecordConfiguration());
        modelBuilder.ApplyConfiguration(new PolicyConfiguration());
        modelBuilder.ApplyConfiguration(new EndorsementConfiguration());
        modelBuilder.ApplyConfiguration(new ProductConfiguration());
        modelBuilder.ApplyConfiguration(new ClientConfiguration());
        modelBuilder.ApplyConfiguration(new AddressConfiguration());
        modelBuilder.ApplyConfiguration(new AgencyConfiguration());
        modelBuilder.ApplyConfiguration(new ProducerConfiguration());
        modelBuilder.ApplyConfiguration(new CoverageConfiguration());
        modelBuilder.ApplyConfiguration(new InvoiceConfiguration());
        modelBuilder.ApplyConfiguration(new InstallmentConfiguration());
        modelBuilder.ApplyConfiguration(new CossuredPolicyConfiguration());
        modelBuilder.ApplyConfiguration(new CossuranceCalculationConfiguration());
        modelBuilder.ApplyConfiguration(new SystemConfigurationConfiguration());
        modelBuilder.ApplyConfiguration(new ReportDefinitionConfiguration());
    }

    protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder)
    {
        if (!optionsBuilder.IsConfigured)
        {
            optionsBuilder.UseSqlite("Data Source=premium_reporting.db");
        }
    }
}
```

---

## Data Model Traceability Matrix

| Entity | COBOL View/Table | Cursor Processing | Primary COBOL Section | Key Relationships |
|--------|------------------|-------------------|----------------------|-------------------|
| PremiumRecord | V0PREMIOS | Yes (R0500, R0600) | R0700-00-PROCESSA-REGISTRO | Policy, Product, Client, Endorsement |
| Policy | V0APOLICE | No | R0980-00-SELECT-V0APOLICE | Product, Client, Agency, Producer |
| Endorsement | V0ENDOSSO | No | R0760-00-SELECT-V0ENDOSSO | Policy |
| Product | V0PRODUTO | No | R0740-00-SELECT-V0PRODUTO | None |
| Client | V0CLIENTE | No | R0960-00-SELECT-V0CLIENTE | Address |
| Address | V0ENDERECOS | Yes (R1230, R1240) | R1220-00-PROCESSA-UF-VIDA | Client |
| Agency | V0AGENCIAS | No | R1180-00-SELECT-V0AGENCIAS | None |
| Producer | V0PRODUTOR | No | R1200-00-SELECT-V0PRODUTOR | None |
| Coverage | V0COBERAPOL | No | R0850-00-SELECT-V0COBERAPOL | Policy |
| Invoice | V0FATURAS | No | R1060-00-SELECT-V0FATURAS | Policy, Installment |
| Installment | V0HISTOPARC | No | R0800-00-SELECT-V0HISTOPARC | Invoice |
| CossuredPolicy | V0APOLCOSCED | Yes (R4900, R5000) | R4700-00-PROCESSA-APOL-COSG | Policy |
| CossuranceCalculation | GE399 | Yes (R5300, R5400) | R5500-00-CALCULA-COSG-CED | Policy |
| SystemConfiguration | V0SISTEMA | No | R0100-00-SELECT-SISTEMAS | None |
| ReportDefinition | V0RELATORIOS | No | R0200-00-SELECT-V0RELATORIO | None |

---

## Next Steps

With the data model design complete, the next phase involves:

1. **Phase 1.2**: Generate API contracts (`/contracts/openapi.yaml`) with RESTful endpoints for report generation, query, and data management
2. **Phase 1.3**: Create `quickstart.md` developer onboarding guide
3. **Phase 1.4**: Update agent context with data model documentation

**Document Version**: 1.0
**Status**: ✅ Complete - Ready for Phase 1.2 (API Contract Design)
**Created**: October 22, 2025
