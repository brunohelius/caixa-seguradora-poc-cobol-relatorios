using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using CaixaSeguradora.Core.Attributes;

namespace CaixaSeguradora.Core.Entities
{
    public class Product
    {
        [Key]
        [CobolField(PicClause = "9(4)", Length = 4)]
        public int ProductCode { get; set; }  // COD_PROD

        [CobolField(PicClause = "9(9)", Length = 9)]
        public int CompanyCode { get; set; }  // COD_EMP

        [CobolField(PicClause = "X(100)", Length = 100)]
        [MaxLength(100)]
        public string ProductName { get; set; } = string.Empty;  // NOM_PROD

        [CobolField(PicClause = "9(4)", Length = 4)]
        public int LineOfBusiness { get; set; }  // RAMO_SUSEP

        [CobolField(PicClause = "9(4)", Length = 4)]
        public int LineOfBusinessGroup { get; set; }  // GRUPO_RAMO_SUSEP

        [CobolField(PicClause = "X(20)", Length = 20)]
        [MaxLength(20)]
        public string SusepProcessNumber { get; set; } = string.Empty;  // NUM_PROC_SUSEP

        [CobolField(PicClause = "X(1)", Length = 1)]
        [MaxLength(1)]
        public string ProductType { get; set; } = string.Empty;  // TIP_PROD ('A'=Auto, 'V'=Vida, 'R'=Residencial, etc.)

        [CobolField(PicClause = "X(1)", Length = 1)]
        [MaxLength(1)]
        public string ProductStatus { get; set; } = "A";  // IND_SITUACAO ('A'=Active, 'I'=Inactive)

        [CobolField(PicClause = "9(4)", Length = 4)]
        public int ProductModality { get; set; }  // MODALIFR

        [CobolField(PicClause = "X(1)", Length = 1)]
        [MaxLength(1)]
        public string IsLifeInsurance { get; set; } = "N";  // IND_VIDA_GRUPO ('S'/'N')

        // Additional properties for repository compatibility
        [NotMapped]
        public int Id => ProductCode;  // Alias

        [NotMapped]
        public string Description => ProductName;  // Alias

        [NotMapped]
        public string Status => ProductStatus;  // Alias

        [CobolField(PicClause = "9(4)V99", Length = 6, DecimalPlaces = 2)]
        [Column(TypeName = "decimal(6,2)")]
        public decimal CommissionPercentage { get; set; }

        [NotMapped]
        public string ProductDescription => ProductName;  // Alias

        [NotMapped]
        public int LineOfBusinessCode => LineOfBusiness;  // Alias for controller compatibility

        [NotMapped]
        public string LineOfBusinessDescription => $"Ramo {LineOfBusiness}";  // Alias for controller compatibility

        [NotMapped]
        public bool IsActive => ProductStatus == "A";  // Alias for controller compatibility

        [NotMapped]
        public int GrupoRamo => LineOfBusinessGroup;  // Alias for RamoValidationService compatibility

        // Navigation properties
        public ICollection<Policy> Policies { get; set; } = new List<Policy>();
        public ICollection<Coverage> Coverages { get; set; } = new List<Coverage>();
    }
}
