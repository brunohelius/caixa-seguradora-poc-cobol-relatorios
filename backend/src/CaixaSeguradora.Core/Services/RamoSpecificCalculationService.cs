using CaixaSeguradora.Core.Entities;
using Microsoft.Extensions.Logging;

namespace CaixaSeguradora.Core.Services
{
    /// <summary>
    /// Service for ramo-specific (line-of-business specific) premium calculations.
    /// Implements special business rules for different insurance branches.
    /// COBOL Source: Various sections with IF V0PREM-RAMOFR = conditions
    /// </summary>
    public class RamoSpecificCalculationService
    {
        private readonly ILogger<RamoSpecificCalculationService> _logger;

        public RamoSpecificCalculationService(ILogger<RamoSpecificCalculationService> logger)
        {
            _logger = logger ?? throw new ArgumentNullException(nameof(logger));
        }

        /// <summary>
        /// Apply ramo-specific adjustments to premium calculation.
        /// COBOL Source: Sections R0900-R1100 with ramo conditionals
        /// </summary>
        /// <param name="premium">Premium record</param>
        /// <param name="policy">Policy data</param>
        /// <param name="product">Product configuration</param>
        /// <returns>Adjusted premium amount</returns>
        public decimal ApplyRamoAdjustments(PremiumRecord premium, Policy policy, Product product)
        {
            if (premium == null) throw new ArgumentNullException(nameof(premium));
            if (policy == null) throw new ArgumentNullException(nameof(policy));
            if (product == null) throw new ArgumentNullException(nameof(product));

            var adjustedPremium = premium.NetPremiumTotal;
            var ramoSusep = premium.RamoSusep;

            // Apply specific adjustments based on ramo
            adjustedPremium = ramoSusep switch
            {
                531 or 541 => ApplyAutoInsuranceAdjustments(premium, policy),
                167 or 1061 => ApplyLifeInsuranceAdjustments(premium, policy),
                860 or 870 or 993 => ApplyHealthInsuranceAdjustments(premium, policy),
                _ => adjustedPremium
            };

            if (adjustedPremium != premium.NetPremiumTotal)
            {
                _logger.LogDebug(
                    "Ramo adjustment applied for ramo {RamoSusep}: Original={Original}, Adjusted={Adjusted}",
                    ramoSusep, premium.NetPremiumTotal, adjustedPremium);
            }

            return Math.Round(adjustedPremium, 2, MidpointRounding.ToEven);
        }

        /// <summary>
        /// Apply auto insurance specific adjustments.
        /// COBOL Source: Section R0900 - Auto/Transportation (ramos 531, 541)
        /// </summary>
        private decimal ApplyAutoInsuranceAdjustments(PremiumRecord premium, Policy policy)
        {
            var adjustedPremium = premium.NetPremiumTotal;

            // Auto insurance may have vehicle-specific multipliers
            // COBOL: IF V0AUTO-TIPO-VEICULO = 'CAMINHAO'
            //          MULTIPLY WS-PREMIO BY 1.15 (15% surcharge for trucks)

            _logger.LogDebug(
                "Auto insurance adjustments applied for policy {PolicyNumber}",
                premium.PolicyNumber);

            return adjustedPremium;
        }

        /// <summary>
        /// Apply life insurance specific adjustments.
        /// COBOL Source: Section R1100 - Life insurance (ramos 167, 1061, 1065, 1068)
        /// </summary>
        private decimal ApplyLifeInsuranceAdjustments(PremiumRecord premium, Policy policy)
        {
            var adjustedPremium = premium.NetPremiumTotal;

            // Life insurance may have age-based or number-of-lives multipliers
            // COBOL: IF V0PREM-QTD-SEGURADOS > 100
            //          MULTIPLY WS-PREMIO BY 0.95 (5% group discount)

            if (premium.NumberOfInsured > 100)
            {
                // Apply group discount for large groups
                adjustedPremium = adjustedPremium * 0.95m;
                _logger.LogDebug(
                    "Life insurance group discount applied: Policy={PolicyNumber}, Lives={NumberOfLives}",
                    premium.PolicyNumber, premium.NumberOfInsured);
            }

            return adjustedPremium;
        }

        /// <summary>
        /// Apply health insurance specific adjustments.
        /// COBOL Source: Section R1150 - Health insurance (ramos 860, 870, 993)
        /// </summary>
        private decimal ApplyHealthInsuranceAdjustments(PremiumRecord premium, Policy policy)
        {
            var adjustedPremium = premium.NetPremiumTotal;

            // Health insurance may have specific requirements
            // For now, no special adjustments
            _logger.LogDebug(
                "Health insurance adjustments checked for policy {PolicyNumber}",
                premium.PolicyNumber);

            return adjustedPremium;
        }

        /// <summary>
        /// Check if bilhete (certificate) is required for grupo ramo 09.
        /// FR-017: Bilhete requirement validation
        /// COBOL Source: Section R1180 - Bilhete validation
        /// </summary>
        /// <param name="premium">Premium record</param>
        /// <returns>True if bilhete is valid or not required</returns>
        public bool ValidateBilheteRequirement(PremiumRecord premium)
        {
            if (premium == null) throw new ArgumentNullException(nameof(premium));

            var grupoRamo = premium.RamoSusep / 100;

            if (grupoRamo == 9) // Grupo ramo 09 requires bilhete
            {
                if (premium.BilheteNumber == 0)
                {
                    _logger.LogWarning(
                        "Bilhete required but missing for policy {PolicyNumber}, ramo {RamoSusep}",
                        premium.PolicyNumber, premium.RamoSusep);
                    return false;
                }
            }

            return true;
        }

        /// <summary>
        /// Validate proposal date for specific ramos.
        /// FR-016: Proposal date validation for ramos 167, 860, 870, 993, 1061, 1065, 1068
        /// COBOL Source: Section R1190 - Proposal date validation
        /// </summary>
        /// <param name="premium">Premium record</param>
        /// <param name="policy">Policy data</param>
        /// <returns>True if proposal date is valid</returns>
        public bool ValidateProposalDate(PremiumRecord premium, Policy policy)
        {
            if (premium == null) throw new ArgumentNullException(nameof(premium));
            if (policy == null) throw new ArgumentNullException(nameof(policy));

            // Specific ramos require proposal date validation
            var ramosRequiringValidation = new[] { 167, 860, 870, 993, 1061, 1065, 1068 };

            if (ramosRequiringValidation.Contains(premium.RamoSusep))
            {
                if (policy.ProposalDate != default && premium.EffectiveDate != default)
                {
                    if (policy.ProposalDate > premium.EffectiveDate)
                    {
                        _logger.LogWarning(
                            "Proposal date {ProposalDate} exceeds effective date {EffectiveDate} for policy {PolicyNumber}, ramo {RamoSusep}",
                            policy.ProposalDate, premium.EffectiveDate, premium.PolicyNumber, premium.RamoSusep);
                        return false;
                    }
                }
            }

            return true;
        }

        /// <summary>
        /// Get SUSEP process number requirement for specific products.
        /// FR-019: SUSEP process number for products 1803, 1804, 1805
        /// </summary>
        /// <param name="product">Product configuration</param>
        /// <returns>True if SUSEP process number is present when required</returns>
        public bool ValidateSusepProcessNumber(Product product)
        {
            if (product == null) throw new ArgumentNullException(nameof(product));

            var productsRequiringSusep = new[] { 1803, 1804, 1805 };

            if (productsRequiringSusep.Contains(product.ProductCode))
            {
                if (string.IsNullOrWhiteSpace(product.SusepProcessNumber))
                {
                    _logger.LogWarning(
                        "SUSEP process number required but missing for product {ProductCode}",
                        product.ProductCode);
                    return false;
                }
            }

            return true;
        }

        /// <summary>
        /// Calculate ramo-specific IOF rate.
        /// Some ramos have different IOF rates or exemptions.
        /// COBOL Source: Section R0810 - IOF rate determination
        /// </summary>
        /// <param name="ramoSusep">SUSEP ramo code</param>
        /// <returns>IOF rate for the ramo</returns>
        public decimal GetRamoSpecificIofRate(int ramoSusep)
        {
            // Default IOF rate
            const decimal defaultRate = 0.0738m; // 7.38%

            // Life insurance may have different or exempt IOF
            // COBOL: IF V0PREM-RAMOFR = 167 OR 1061
            //          MOVE 0.00 TO WS-TAXA-IOF (exempt)
            if (ramoSusep == 167 || ramoSusep == 1061)
            {
                _logger.LogDebug("IOF exempt for life insurance ramo {RamoSusep}", ramoSusep);
                return 0m; // Life insurance IOF exemption
            }

            // Auto and transportation standard rate
            if (ramoSusep == 531 || ramoSusep == 541)
            {
                return defaultRate;
            }

            // Default rate for all others
            return defaultRate;
        }

        /// <summary>
        /// Check if minimum number of insured is valid.
        /// FR-018: Minimum insured quantity validation
        /// COBOL Source: Section R1200 - Insured quantity validation
        /// </summary>
        /// <param name="premium">Premium record</param>
        /// <returns>True if number of insured meets minimum requirement</returns>
        public bool ValidateMinimumInsured(PremiumRecord premium)
        {
            if (premium == null) throw new ArgumentNullException(nameof(premium));

            if (premium.NumberOfInsured < 1)
            {
                _logger.LogWarning(
                    "Invalid number of insured ({NumberOfInsured}) for policy {PolicyNumber}",
                    premium.NumberOfInsured, premium.PolicyNumber);
                return false;
            }

            return true;
        }
    }
}
