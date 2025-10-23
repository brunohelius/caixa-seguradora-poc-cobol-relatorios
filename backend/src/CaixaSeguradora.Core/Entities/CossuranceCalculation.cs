using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using CaixaSeguradora.Core.Attributes;

namespace CaixaSeguradora.Core.Entities
{
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

        // Additional properties for service compatibility
        [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,2)")]
        public decimal TotalGrossPremium { get; set; }  // Total gross premium for calculation

        [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,2)")]
        public decimal TotalNetPremium { get; set; }  // Total net premium

        [NotMapped]
        public decimal CossurerPremium => CededPremium;  // Alias

        [NotMapped]
        public decimal CossurerCommission => CededCommission;  // Alias

        [CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
        [Column(TypeName = "decimal(15,2)")]
        public decimal TotalIOF { get; set; }  // Total IOF tax

        [NotMapped]
        public long Id => CalculationId;  // Alias

        [NotMapped]
        public decimal TotalCommission => CededCommission;  // Alias

        public long? CossuredPolicyId { get; set; }  // Foreign key to CossuredPolicy

        public CossuredPolicy? CossuredPolicy { get; set; }  // Navigation to Cossured Policy

        // Navigation properties
        public Policy Policy { get; set; } = null!;
    }
}
