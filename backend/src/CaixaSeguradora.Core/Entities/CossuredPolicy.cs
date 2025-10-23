using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using CaixaSeguradora.Core.Attributes;

namespace CaixaSeguradora.Core.Entities
{
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
        public string CossuranceType { get; set; } = string.Empty;  // TIP_COSSG ('C'=Cosseguro, 'R'=Resseguro, 'E'=Retrocessao)

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
        public string IsLeader { get; set; } = "N";  // IND_LIDER ('S'/'N' - for cossurance)

        // Additional properties for service compatibility
        [CobolField(PicClause = "9(9)", Length = 9)]
        public int CossurerCode { get; set; }  // Alias for AcquiringCompanyCode

        [CobolField(PicClause = "X(100)", Length = 100)]
        [MaxLength(100)]
        public string CossurerName { get; set; } = string.Empty;  // Cossurer company name

        [NotMapped]
        public decimal ParticipationPercentage => PercentageShare;  // Alias

        [NotMapped]
        public long Id => CossuranceId;  // Alias

        [NotMapped]
        public long CossuredPolicyId => CossuranceId;  // Alias

        [NotMapped]
        public long PolicyId => PolicyNumber;  // Alias

        [CobolField(PicClause = "X(1)", Length = 1)]
        [MaxLength(1)]
        public string Status { get; set; } = "A";  // A=Active, I=Inactive

        // Navigation properties
        public Policy Policy { get; set; } = null!;
    }
}
