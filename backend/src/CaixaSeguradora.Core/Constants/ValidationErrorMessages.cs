namespace CaixaSeguradora.Core.Constants;

/// <summary>
/// Catalog of validation error messages in Brazilian Portuguese.
/// All user-facing validation messages for COBOL business rules.
/// FR-020: All error messages must be in Brazilian Portuguese
/// </summary>
public static class ValidationErrorMessages
{
    // Error Codes
    public const string ERR_INVALID_PROPOSAL_DATE = "ERR_001";
    public const string ERR_MISSING_BILHETE = "ERR_002";
    public const string ERR_INVALID_QUANTITY = "ERR_003";
    public const string ERR_INVALID_DATE_SEQUENCE = "ERR_004";
    public const string ERR_AMOUNT_OVERFLOW = "ERR_005";
    public const string ERR_MISSING_CLIENT = "ERR_006";
    public const string ERR_MISSING_POLICY = "ERR_007";
    public const string ERR_MISSING_PRODUCT = "ERR_008";
    public const string ERR_INVALID_RAMO = "ERR_009";
    public const string ERR_MISSING_SUSEP_PROCESS = "ERR_010";
    public const string ERR_NEGATIVE_PREMIUM = "ERR_011";
    public const string ERR_INVALID_MOVEMENT_TYPE = "ERR_012";

    // Warning Codes
    public const string WARN_AUTO_CORRECTED = "WARN_001";
    public const string WARN_MISSING_FOREIGN_KEY = "WARN_002";
    public const string WARN_DATA_QUALITY = "WARN_003";

    // Error Messages (Portuguese)
    public static class Messages
    {
        // FR-016: Proposal Date Validation
        public const string InvalidProposalDate =
            "Data de proposta não pode ser maior que data de vigência inicial para ramo {0}";

        public const string ProposalDateAutoAdjusted =
            "Data de proposta ajustada automaticamente para data de vigência inicial";

        // FR-017: Bilhete Number Validation
        public const string MissingBilheteNumber =
            "Ramo 09 requer número de bilhete";

        public const string BilheteRequired =
            "Grupo ramo 09 (Acidentes Pessoais) exige número de bilhete válido";

        // FR-018: Insured Quantity Validation
        public const string InvalidInsuredQuantity =
            "Quantidade de segurados não pode ser zero ou negativa";

        public const string QuantityAutoAdjusted =
            "Quantidade de segurados ajustada de {0} para 1 (mínimo obrigatório)";

        // Date Sequence Validation
        public const string InvalidDateSequence =
            "Sequência de datas inválida: emissão ≤ vigência inicial ≤ vigência final";

        public const string IssueDateAfterEffective =
            "Data de emissão ({0}) não pode ser posterior à data de vigência inicial ({1})";

        public const string EffectiveDateAfterExpiration =
            "Data de vigência inicial ({0}) não pode ser posterior à data de vigência final ({1})";

        // Amount Validation
        public const string AmountExceedsPrecision =
            "Valor do campo {0} excede precisão COMP-3: máximo decimal(15,2)";

        public const string NegativePremiumNotAllowed =
            "Prêmio líquido negativo não permitido para movimentação tipo {0}";

        public const string ZeroPremiumWarning =
            "Prêmio líquido zero pode indicar problema de cálculo";

        // Foreign Key Validation
        public const string ClientNotFound =
            "Cliente código {0} não encontrado na base de dados";

        public const string PolicyNotFound =
            "Apólice {0} não encontrada na base de dados";

        public const string ProductNotFound =
            "Produto código {0} não encontrado na base de dados";

        public const string CoverageNotFound =
            "Cobertura código {0} não encontrada para apólice {1}";

        // Ramo-Specific Validation
        public const string InvalidRamoForProduct =
            "Ramo {0} inválido para produto {1}";

        public const string VehicleIdentificationRequired =
            "Ramo 0531 (Auto) requer identificação do veículo";

        public const string PropertyAddressRequired =
            "Ramo 0193 (Residencial) requer endereço do imóvel";

        public const string InsuredAgeRequired =
            "Ramo 0167 (Vida Individual) requer idade do segurado";

        // FR-019: SUSEP Process Number Validation
        public const string SusepProcessNumberRequired =
            "Produto {0} com ramo {1} requer número de processo SUSEP";

        public const string SusepProcessNumberNotFound =
            "Número de processo SUSEP não encontrado para produto {0} ramo {1}";

        // Movement Type Validation
        public const string InvalidMovementType =
            "Tipo de movimentação '{0}' inválido. Valores aceitos: E (Emissão), C (Cancelamento), R (Restituição), etc.";

        public const string MovementTypeNotSupported =
            "Tipo de movimentação '{0}' não suportado nesta versão";

        // Data Quality
        public const string MandatoryFieldMissing =
            "Campo obrigatório '{0}' não foi preenchido";

        public const string InvalidDataFormat =
            "Formato de dado inválido no campo '{0}': esperado {1}";

        public const string DataOutOfRange =
            "Valor do campo '{0}' fora da faixa permitida: {1} a {2}";

        // General Validation
        public const string ValidationFailed =
            "Validação de regras de negócio falhou para apólice {0}";

        public const string MultipleErrorsFound =
            "Múltiplos erros de validação encontrados. Verifique o log para detalhes.";

        public const string RecordSkipped =
            "Registro da apólice {0} ignorado devido a erros de validação";
    }

    // Auto-Correction Reasons (Portuguese)
    public static class CorrectionReasons
    {
        public const string ProposalDateAdjusted =
            "Data de proposta ajustada para atender regra SUSEP para ramo {0}";

        public const string QuantitySetToMinimum =
            "Quantidade ajustada para mínimo de 1 segurado conforme especificação";

        public const string NullValueReplaced =
            "Valor nulo substituído por valor padrão: {0}";

        public const string InvalidValueCorrected =
            "Valor inválido corrigido automaticamente";
    }

    /// <summary>
    /// Formats an error message with parameters
    /// </summary>
    public static string Format(string message, params object[] args)
    {
        return string.Format(message, args);
    }
}
