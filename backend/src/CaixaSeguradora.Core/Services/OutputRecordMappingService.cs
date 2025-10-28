using System;
using System.Linq;
using CaixaSeguradora.Core.DTOs;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Interfaces;
using Microsoft.Extensions.Logging;

namespace CaixaSeguradora.Core.Services
{
    /// <summary>
    /// Maps domain entities to fixed-width output record DTOs for SUSEP file generation.
    ///
    /// Responsibilities:
    /// - Convert PremiumRecord + related entities → PremitOutputRecord (765 bytes)
    /// - Convert CossuredPolicy + premium → PremcedOutputRecord (168 bytes)
    /// - Apply movement type code mapping (101-106)
    /// - Format dates according to SUSEP requirements (YYYYMMDD)
    /// - Handle null values appropriately (zeros for numeric, spaces for alphanumeric)
    ///
    /// COBOL Sections Replicated:
    /// - R0900-R1300: Premium data population logic
    /// - R5000-R5600: Cossurance data extraction
    ///
    /// Research Reference: specs/003-complete-cobol-migration/research.md section R3
    /// Tasks: T128, T129
    /// </summary>
    public class OutputRecordMappingService : IOutputRecordMappingService
    {
        private readonly ILogger<OutputRecordMappingService> _logger;

        public OutputRecordMappingService(ILogger<OutputRecordMappingService> logger)
        {
            _logger = logger ?? throw new ArgumentNullException(nameof(logger));
        }

        /// <summary>
        /// Map PremiumRecord with related entities to PREMIT output format.
        ///
        /// COBOL equivalent: Sections R0900-R1300 MOVE statements into REG-PREMIT.
        /// </summary>
        /// <param name="premium">Premium record from V0PREMIOS</param>
        /// <param name="policy">Related policy from V0APOLICE (optional)</param>
        /// <param name="client">Related client from V0CLIENTE (optional)</param>
        /// <param name="product">Related product from V0PRODUTO (optional)</param>
        /// <returns>PREMIT output record (765 bytes when formatted)</returns>
        public PremitOutputRecord MapToPremitRecord(
            PremiumRecord premium,
            Policy policy = null,
            Client client = null,
            Product product = null)
        {
            if (premium == null)
                throw new ArgumentNullException(nameof(premium));

            try
            {
                var output = new PremitOutputRecord
                {
                    // ===== POLICY IDENTIFICATION =====
                    CompanyCode = premium.CompanyCode,
                    RamoSusep = premium.RamoSusep,
                    PolicyNumber = FormatPolicyNumber(premium.PolicyNumber),
                    EndorsementNumberCA = premium.EndorsementNumberCA,
                    EndorsementNumber = premium.EndorsementNumber,

                    // ===== DATES =====
                    IssueDate = premium.IssueDate != default ? premium.IssueDate : DateTime.Now,
                    EffectiveDate = premium.EffectiveDate != default ? premium.EffectiveDate : DateTime.Now,
                    ExpirationDate = premium.ExpirationDate != default ? premium.ExpirationDate : DateTime.Now.AddYears(1),
                    ProposalDate = premium.ProposalDate,

                    // ===== MOVEMENT TYPE =====
                    MovementType = MapMovementTypeCode(premium.MovementType),

                    // ===== FINANCIAL AMOUNTS (ITEM LEVEL - 5 DECIMALS) =====
                    InsuredSumItems = premium.InsuredSumItems?.FirstOrDefault() ?? 0m,
                    BasePremiumItems = premium.BasePremiumItems?.FirstOrDefault() ?? 0m,
                    FixedPremiumItems = premium.FixedPremiumItems?.FirstOrDefault() ?? 0m,
                    TariffPremiumItems = premium.TariffPremiumItems?.FirstOrDefault() ?? 0m,
                    DiscountItems = premium.DiscountItems?.FirstOrDefault() ?? 0m,
                    NetPremiumItems = premium.NetPremiumItems?.FirstOrDefault() ?? 0m,
                    InstallmentSurchargeItems = premium.InstallmentSurchargeItems?.FirstOrDefault() ?? 0m,
                    IssuanceCostItems = premium.IssuanceCostItems?.FirstOrDefault() ?? 0m,
                    IofItems = premium.IofItems?.FirstOrDefault() ?? 0m,
                    TotalPremiumItems = premium.TotalPremiumItems?.FirstOrDefault() ?? 0m,
                    CommissionItems = premium.CommissionItems?.FirstOrDefault() ?? 0m,
                    AdministrationFeeItems = premium.AdministrationFeeItems?.FirstOrDefault() ?? 0m,
                    AgencyFeeItems = premium.AgencyFeeItems?.FirstOrDefault() ?? 0m,

                    // ===== FINANCIAL AMOUNTS (TOTAL LEVEL - 2 DECIMALS) =====
                    InsuredSumTotal = premium.InsuredSumTotal,
                    BasePremiumTotal = premium.BasePremiumTotal,
                    NetPremiumTotal = premium.NetPremiumTotal,
                    TotalPremiumAmount = premium.TotalPremiumAmount,
                    IofTotal = premium.IofTotal,
                    CommissionTotal = premium.CommissionTotal,

                    // ===== PARTY IDENTIFIERS =====
                    ClientCode = premium.ClientCode,
                    EstipulanteCode = premium.EstipulanteCode > 0 ? (int?)premium.EstipulanteCode : null,
                    TomadorCode = premium.TomadorCode > 0 ? (int?)premium.TomadorCode : null,
                    AgencyCode = premium.AgencyCode,
                    ProducerCode = premium.ProducerCode,
                    SalesChannelCode = premium.SalesChannelCode,

                    // ===== OPERATIONAL ATTRIBUTES =====
                    BilheteNumber = premium.BilheteNumber,
                    NumberOfInsured = premium.NumberOfInsured > 0 ? premium.NumberOfInsured : 1,
                    NumberOfInstallments = premium.NumberOfInstallments > 0 ? premium.NumberOfInstallments : 1,
                    ProductCode = premium.ProductCode,
                    OperationType = premium.OperationType,
                    StateCode = FormatStateCode(policy?.StateCode),
                    SusepProcessNumber = FormatSusepProcessNumber(product?.SusepProcessNumber),
                    HasCossurance = FormatBooleanFlag(premium.HasCossurance),
                    IsRenewal = FormatBooleanFlag(premium.IsRenewal),

                    // Reserved field (already initialized with spaces in DTO)
                    Reserved = new string(' ', 290)
                };

                _logger.LogDebug("Mapped premium {PolicyNumber} endoso {EndorsementNumber} to PREMIT record",
                    premium.PolicyNumber, premium.EndorsementNumber);

                return output;
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error mapping premium {PolicyNumber} to PREMIT record", premium.PolicyNumber);
                throw;
            }
        }

        /// <summary>
        /// Map cossurance policy with premium context to PREMCED output format.
        ///
        /// COBOL equivalent: Sections R5000-R5600 MOVE statements into REG-PREMCED.
        /// </summary>
        /// <param name="cossurance">Cossurance record from V0APOLCOSCED</param>
        /// <param name="premium">Related premium record for context</param>
        /// <param name="policy">Related policy for dates and identification</param>
        /// <param name="sequenceNumber">Sequence number for multiple cossurance records per policy</param>
        /// <returns>PREMCED output record (168 bytes when formatted)</returns>
        public PremcedOutputRecord MapToPremcedRecord(
            CossuredPolicy cossurance,
            PremiumRecord premium,
            Policy policy = null,
            int sequenceNumber = 1)
        {
            if (cossurance == null)
                throw new ArgumentNullException(nameof(cossurance));
            if (premium == null)
                throw new ArgumentNullException(nameof(premium));

            try
            {
                var output = new PremcedOutputRecord
                {
                    // ===== POLICY IDENTIFICATION =====
                    CompanyCode = premium.CompanyCode,
                    RamoSusep = premium.RamoSusep,
                    PolicyNumber = FormatPolicyNumber(cossurance.PolicyNumber),
                    EndorsementNumber = premium.EndorsementNumber,

                    // ===== COSSURANCE DETAILS =====
                    CessionType = cossurance.CessionType.ToString(),
                    CossurerCompanyCode = cossurance.CossurerCompanyCode,
                    ParticipationPercentage = cossurance.ParticipationPercentage,

                    // ===== FINANCIAL AMOUNTS =====
                    CededPremium = cossurance.CededPremium,
                    CededCommission = CalculateCededCommission(cossurance.ParticipationPercentage, premium.CommissionTotal),
                    CededInsuredSum = CalculateCededInsuredSum(cossurance.ParticipationPercentage, premium.InsuredSumTotal),

                    // ===== DATES =====
                    IssueDate = premium.IssueDate != default ? premium.IssueDate : DateTime.Now,
                    EffectiveDate = premium.EffectiveDate != default ? premium.EffectiveDate : DateTime.Now,
                    ExpirationDate = premium.ExpirationDate != default ? premium.ExpirationDate : DateTime.Now.AddYears(1),

                    // ===== OPERATIONAL ATTRIBUTES =====
                    MovementType = MapMovementTypeCode(premium.MovementType),
                    ProductCode = premium.ProductCode,
                    ClientCode = premium.ClientCode,
                    SequenceNumber = sequenceNumber,

                    // Reserved field (already initialized with spaces in DTO)
                    Reserved = new string(' ', 29)
                };

                _logger.LogDebug("Mapped cossurance for policy {PolicyNumber} with cossurer {CossurerCode} to PREMCED record",
                    cossurance.PolicyNumber, cossurance.CossurerCompanyCode);

                return output;
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error mapping cossurance for policy {PolicyNumber} to PREMCED record",
                    cossurance.PolicyNumber);
                throw;
            }
        }

        // ===== PRIVATE HELPER METHODS =====

        /// <summary>
        /// Map movement type character to SUSEP numeric code.
        ///
        /// COBOL: EMI-TIPO-MOV mapping logic (various sections).
        ///
        /// Mapping Table:
        /// '1' or 'E' → 101 (Emission)
        /// '2' or 'R' → 102 (Renewal)
        /// '3' or 'M' → 103 (Majoração/Increase)
        /// '4' or 'D' → 104 (Redução/Decrease)
        /// '5' or 'C' → 105 (Cancelamento/Cancellation)
        /// '6' or 'S' → 106 (Restituição/Restitution)
        /// </summary>
        private int MapMovementTypeCode(string movementType)
        {
            if (string.IsNullOrWhiteSpace(movementType))
                return 101; // Default to Emission

            return movementType.ToUpperInvariant() switch
            {
                "1" => 101,
                "E" => 101, // Emissão
                "2" => 102,
                "R" => 102, // Renovação
                "3" => 103,
                "M" => 103, // Majoração
                "4" => 104,
                "D" => 104, // Redução (Diminuição)
                "5" => 105,
                "C" => 105, // Cancelamento
                "6" => 106,
                "S" => 106, // Restituição (Substituição)
                _ => 101    // Unknown → default to Emission
            };
        }

        /// <summary>
        /// Format policy number to 20-character alphanumeric field.
        /// Converts long to string, left-pads with zeros.
        /// </summary>
        private string FormatPolicyNumber(long policyNumber)
        {
            return policyNumber.ToString().PadLeft(20, '0');
        }

        /// <summary>
        /// Format state code to 2-character field.
        /// Ensures uppercase and pads with spaces if needed.
        /// </summary>
        private string FormatStateCode(string stateCode)
        {
            if (string.IsNullOrWhiteSpace(stateCode))
                return "  "; // Two spaces for null

            var formatted = stateCode.ToUpperInvariant().Trim();
            return formatted.Length >= 2 ? formatted.Substring(0, 2) : formatted.PadRight(2);
        }

        /// <summary>
        /// Format SUSEP process number to 25-character field.
        /// Right-pads with spaces if shorter, truncates if longer.
        /// </summary>
        private string FormatSusepProcessNumber(string processNumber)
        {
            if (string.IsNullOrWhiteSpace(processNumber))
                return new string(' ', 25);

            return processNumber.Length > 25
                ? processNumber.Substring(0, 25)
                : processNumber.PadRight(25);
        }

        /// <summary>
        /// Format boolean value as single-character flag.
        /// 'S' (Sim/Yes) for true, 'N' (Não/No) for false.
        /// </summary>
        private string FormatBooleanFlag(bool value)
        {
            return value ? "S" : "N";
        }

        /// <summary>
        /// Calculate ceded commission based on participation percentage.
        ///
        /// COBOL: Calculation in section R5400-R5500.
        /// Formula: TotalCommission * (ParticipationPercentage / 100)
        /// </summary>
        private decimal CalculateCededCommission(decimal participationPercentage, decimal totalCommission)
        {
            if (participationPercentage < 0 || participationPercentage > 100)
            {
                _logger.LogWarning("Invalid participation percentage {Percentage}. Using 0.", participationPercentage);
                return 0m;
            }

            return Math.Round(totalCommission * (participationPercentage / 100m), 2, MidpointRounding.ToEven);
        }

        /// <summary>
        /// Calculate ceded insured sum based on participation percentage.
        ///
        /// COBOL: Calculation in section R5500.
        /// Formula: TotalInsuredSum * (ParticipationPercentage / 100)
        /// </summary>
        private decimal CalculateCededInsuredSum(decimal participationPercentage, decimal totalInsuredSum)
        {
            if (participationPercentage < 0 || participationPercentage > 100)
            {
                _logger.LogWarning("Invalid participation percentage {Percentage}. Using 0.", participationPercentage);
                return 0m;
            }

            return Math.Round(totalInsuredSum * (participationPercentage / 100m), 2, MidpointRounding.ToEven);
        }
    }
}
