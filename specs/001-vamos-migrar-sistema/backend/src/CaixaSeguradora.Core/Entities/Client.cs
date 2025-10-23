using System;
using System.Collections.Generic;
using CaixaSeguradora.Core.Attributes;

namespace CaixaSeguradora.Core.Entities
{
    public class Client
    {
        public int Id { get; set; }

        [CobolField("WS-NUM-CPF-CNPJ", CobolFieldType.Numeric, 1, 14)]
        public string TaxId { get; set; } = string.Empty;

        [CobolField("WS-NOM-CLIENTE", CobolFieldType.Alphanumeric, 15, 60)]
        public string Name { get; set; } = string.Empty;

        [CobolField("WS-TIP-PESSOA", CobolFieldType.Alphanumeric, 75, 1)]
        public string PersonType { get; set; } = string.Empty; // F=Física, J=Jurídica

        [CobolField("WS-DAT-NASCIMENTO", CobolFieldType.Date, 76, 8)]
        public DateTime? BirthDate { get; set; }

        [CobolField("WS-EMAIL", CobolFieldType.Alphanumeric, 84, 100)]
        public string Email { get; set; } = string.Empty;

        [CobolField("WS-TEL-CONTATO", CobolFieldType.Alphanumeric, 184, 20)]
        public string PhoneNumber { get; set; } = string.Empty;

        public DateTime CreatedAt { get; set; } = DateTime.UtcNow;
        public DateTime? UpdatedAt { get; set; }

        // Navigation properties
        public ICollection<Address> Addresses { get; set; } = new List<Address>();
        public ICollection<Policy> Policies { get; set; } = new List<Policy>();
    }
}
