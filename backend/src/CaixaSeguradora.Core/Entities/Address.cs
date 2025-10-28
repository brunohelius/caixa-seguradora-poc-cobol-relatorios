using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using CaixaSeguradora.Core.Attributes;

namespace CaixaSeguradora.Core.Entities
{
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
        public string AddressType { get; set; } = string.Empty;  // TIP_ENDER ('R'=Residential, 'C'=Commercial, etc.)

        [CobolField(PicClause = "X(100)", Length = 100)]
        [MaxLength(100)]
        public string StreetAddress { get; set; } = string.Empty;  // DES_LOGRADOURO

        [CobolField(PicClause = "X(10)", Length = 10)]
        [MaxLength(10)]
        public string Number { get; set; } = string.Empty;  // NUM_LOGRA

        [CobolField(PicClause = "X(50)", Length = 50)]
        [MaxLength(50)]
        public string Complement { get; set; } = string.Empty;  // DES_COMPL

        [CobolField(PicClause = "X(50)", Length = 50)]
        [MaxLength(50)]
        public string Neighborhood { get; set; } = string.Empty;  // DES_BAIRRO

        [CobolField(PicClause = "X(50)", Length = 50)]
        [MaxLength(50)]
        public string City { get; set; } = string.Empty;  // NOM_MUNIC

        [CobolField(PicClause = "X(2)", Length = 2)]
        [MaxLength(2)]
        public string State { get; set; } = string.Empty;  // COD_UF (e.g., 'SP', 'RJ')

        [CobolField(PicClause = "X(8)", Length = 8)]
        [MaxLength(8)]
        public string PostalCode { get; set; } = string.Empty;  // NUM_CEP (Brazilian CEP format XXXXX-XXX)

        [CobolField(PicClause = "9(4)", Length = 4)]
        public int CountryCode { get; set; } = 1058;  // COD_PAIS (default 1058 = Brazil)

        // Additional properties for repository compatibility
        [NotMapped]
        public long Id => AddressId;  // Alias

        [NotMapped]
        public string Street => StreetAddress;  // Alias

        [CobolField(PicClause = "X(50)", Length = 50)]
        [MaxLength(50)]
        public string Country { get; set; } = "Brazil";  // Country name

        [NotMapped]
        public int ClientId => ClientCode;  // Alias

        [NotMapped]
        public string StateCode => State;  // Alias for ExternalValidationService compatibility

        // Navigation properties
        public Client Client { get; set; } = null!;
    }
}
