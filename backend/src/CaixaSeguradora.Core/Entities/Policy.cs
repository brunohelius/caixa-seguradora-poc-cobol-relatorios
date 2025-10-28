using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using CaixaSeguradora.Core.Attributes;

namespace CaixaSeguradora.Core.Entities
{
    public class Policy
    {
        [Key]
        [CobolField("WS-NUM-APOLICE", CobolFieldType.Numeric, 1, 15)]
        public long PolicyNumber { get; set; }

        [CobolField("WS-NUM-ENDOSSO", CobolFieldType.Numeric, 16, 10)]
        public int EndorsementNumber { get; set; }

        [CobolField("WS-COD-SISTEMA", CobolFieldType.Alphanumeric, 26, 2)]
        public string SystemCode { get; set; } = string.Empty;

        [CobolField("WS-COD-PRODUTO", CobolFieldType.Numeric, 28, 4)]
        public int ProductCode { get; set; }

        [CobolField("WS-RAMO-SUSEP", CobolFieldType.Numeric, 32, 4)]
        public int RamoSusep { get; set; }

        [CobolField("WS-DAT-EMISSAO", CobolFieldType.Date, 36, 8)]
        public DateTime IssueDate { get; set; }

        [CobolField("WS-DAT-PROPOSTA", CobolFieldType.Date, 44, 8)]
        public DateTime ProposalDate { get; set; }

        [CobolField("WS-DAT-INICIO-VIGENCIA", CobolFieldType.Date, 52, 8)]
        public DateTime EffectiveDate { get; set; }

        [CobolField("WS-DAT-FIM-VIGENCIA", CobolFieldType.Date, 40, 8)]
        public DateTime ExpirationDate { get; set; }

        [CobolField("WS-VAL-PREMIO-TOTAL", CobolFieldType.PackedDecimal, 48, 15, 2, "S9(13)V99")]
        public decimal TotalPremium { get; set; }

        [CobolField("WS-VAL-PREMIO-LIQUIDO", CobolFieldType.PackedDecimal, 63, 15, 2, "S9(13)V99")]
        public decimal NetPremium { get; set; }

        [CobolField("WS-STAT-APOLICE", CobolFieldType.Alphanumeric, 78, 1)]
        public string PolicyStatus { get; set; } = string.Empty;

        [CobolField("WS-COD-CLIENTE", CobolFieldType.Numeric, 80, 9)]
        public int ClientCode { get; set; }  // Main client/policyholder code

        [CobolField("WS-COD-AGENCIA", CobolFieldType.Numeric, 89, 4)]
        public int AgencyCode { get; set; }  // Agency/Branch code

        [CobolField("WS-COD-PRODUTOR", CobolFieldType.Numeric, 93, 9)]
        public int ProducerCode { get; set; }  // Producer/Broker code

        [CobolField("WS-COD-SEGURADO", CobolFieldType.Numeric, 102, 9)]
        public int InsuredClientCode { get; set; }  // Insured party code

        [CobolField("WS-COD-SEGURADO-ALIAS", CobolFieldType.Numeric, 98, 9)]
        public int InsuredCode { get; set; }  // Alias for InsuredClientCode

        [CobolField("WS-DAT-INICIO-VIGENCIA-STR", CobolFieldType.Alphanumeric, 107, 10)]
        public string PolicyStartDate { get; set; } = string.Empty;  // String format YYYY-MM-DD

        [CobolField("WS-DAT-FIM-VIGENCIA-STR", CobolFieldType.Alphanumeric, 117, 10)]
        public string PolicyEndDate { get; set; } = string.Empty;  // String format YYYY-MM-DD

        [CobolField("WS-COD-STATUS", CobolFieldType.Alphanumeric, 127, 1)]
        public string PolicyStatusCode { get; set; } = string.Empty;  // Alias for PolicyStatus

        [CobolField("WS-NUM-PROPOSTA", CobolFieldType.Numeric, 128, 13)]
        public long ProposalNumber { get; set; }  // Proposal/quote number

        [CobolField("WS-COD-UF", CobolFieldType.Alphanumeric, 141, 2)]
        public string StateCode { get; set; } = string.Empty;  // State code (UF)

        [CobolField("WS-COD-PROPONENTE", CobolFieldType.Numeric, 143, 9)]
        public int ProposerClientCode { get; set; }  // Proposer/applicant client code

        // Navigation properties
        public Client Client { get; set; } = null!;
        public Agency Agency { get; set; } = null!;
        public Producer Producer { get; set; } = null!;
        public Product Product { get; set; } = null!;
        public ICollection<Endorsement> Endorsements { get; set; } = new List<Endorsement>();
        public ICollection<Coverage> Coverages { get; set; } = new List<Coverage>();
        public ICollection<Invoice> Invoices { get; set; } = new List<Invoice>();
        public ICollection<CossuredPolicy> CossuredPolicies { get; set; } = new List<CossuredPolicy>();
    }
}
