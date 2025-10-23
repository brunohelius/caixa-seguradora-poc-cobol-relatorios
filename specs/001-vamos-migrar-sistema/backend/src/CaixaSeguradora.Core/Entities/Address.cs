using CaixaSeguradora.Core.Attributes;

namespace CaixaSeguradora.Core.Entities
{
    public class Address
    {
        public int Id { get; set; }

        [CobolField("WS-COD-CEP", CobolFieldType.Numeric, 1, 8)]
        public string PostalCode { get; set; } = string.Empty;

        [CobolField("WS-NOM-LOGRADOURO", CobolFieldType.Alphanumeric, 9, 100)]
        public string Street { get; set; } = string.Empty;

        [CobolField("WS-NUM-ENDERECO", CobolFieldType.Alphanumeric, 109, 10)]
        public string Number { get; set; } = string.Empty;

        [CobolField("WS-DES-COMPLEMENTO", CobolFieldType.Alphanumeric, 119, 50)]
        public string Complement { get; set; } = string.Empty;

        [CobolField("WS-NOM-BAIRRO", CobolFieldType.Alphanumeric, 169, 50)]
        public string Neighborhood { get; set; } = string.Empty;

        [CobolField("WS-NOM-CIDADE", CobolFieldType.Alphanumeric, 219, 50)]
        public string City { get; set; } = string.Empty;

        [CobolField("WS-COD-UF", CobolFieldType.Alphanumeric, 269, 2)]
        public string State { get; set; } = string.Empty;

        [CobolField("WS-NOM-PAIS", CobolFieldType.Alphanumeric, 271, 50)]
        public string Country { get; set; } = "Brasil";

        public int ClientId { get; set; }

        // Navigation properties
        public Client Client { get; set; } = null!;
    }
}
