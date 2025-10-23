using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Globalization;
using CaixaSeguradora.Core.Attributes;

namespace CaixaSeguradora.Core.Entities
{
    public class Client
    {
        [Key]
        [CobolField(PicClause = "9(9)", Length = 9)]
        public int ClientCode { get; set; }  // COD_CLIEN

        [CobolField(PicClause = "9(9)", Length = 9)]
        public int CompanyCode { get; set; }  // COD_EMP

        [CobolField(PicClause = "X(100)", Length = 100)]
        [MaxLength(100)]
        public string ClientName { get; set; } = string.Empty;  // NOM_CLIEN

        [CobolField(PicClause = "X(1)", Length = 1)]
        [MaxLength(1)]
        public string ClientType { get; set; } = string.Empty;  // TIP_PESSOA ('F'=Fisica/Person, 'J'=Juridica/Company)

        [CobolField(PicClause = "X(14)", Length = 14)]
        [MaxLength(14)]
        public string DocumentNumber { get; set; } = string.Empty;  // NUM_CPF_CNPJ (CPF or CNPJ)

        [CobolField(PicClause = "X(20)", Length = 20)]
        [MaxLength(20)]
        public string IdentityDocument { get; set; } = string.Empty;  // NUM_RG_IE

        [CobolField(PicClause = "X(10)", Length = 10)]
        [MaxLength(10)]
        public string BirthDate { get; set; } = string.Empty;  // DT_NASC (YYYY-MM-DD)

        public DateTime? BirthDateParsed => string.IsNullOrWhiteSpace(BirthDate)
            ? null
            : DateTime.ParseExact(BirthDate, "yyyy-MM-dd", CultureInfo.InvariantCulture);

        [CobolField(PicClause = "X(1)", Length = 1)]
        [MaxLength(1)]
        public string Gender { get; set; } = string.Empty;  // IND_SEXO ('M', 'F')

        [CobolField(PicClause = "X(50)", Length = 50)]
        [MaxLength(50)]
        public string Email { get; set; } = string.Empty;  // DES_EMAIL

        [CobolField(PicClause = "X(20)", Length = 20)]
        [MaxLength(20)]
        public string PhoneNumber { get; set; } = string.Empty;  // NUM_FONE

        [CobolField(PicClause = "X(1)", Length = 1)]
        [MaxLength(1)]
        public string ClientStatus { get; set; } = "A";  // IND_SITUACAO ('A'=Active, 'I'=Inactive)

        // Additional properties for repository compatibility
        [NotMapped]
        public int Id => ClientCode;  // Alias

        [NotMapped]
        public string Name => ClientName;  // Alias

        [NotMapped]
        public string PersonType => ClientType;  // Alias

        [NotMapped]
        public string TaxId => DocumentNumber;  // Alias

        [NotMapped]
        public string DocumentType => ClientType == "F" ? "CPF" : "CNPJ";  // Alias for controller compatibility

        // Navigation properties
        public ICollection<Address> Addresses { get; set; } = new List<Address>();
        public ICollection<Policy> Policies { get; set; } = new List<Policy>();
    }
}
