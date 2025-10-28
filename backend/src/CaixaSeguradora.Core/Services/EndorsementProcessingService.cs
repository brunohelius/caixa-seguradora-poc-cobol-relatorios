using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Interfaces;
using Microsoft.Extensions.Logging;

namespace CaixaSeguradora.Core.Services
{
    /// <summary>
    /// Service for processing endorsements (policy modifications).
    /// Implements endorsement logic for movement types 103-106 (majoração, redução, cancelamento, restituição).
    /// COBOL Source: Sections R0800-R0900
    /// </summary>
    public class EndorsementProcessingService
    {
        private readonly ILogger<EndorsementProcessingService> _logger;
        private readonly IPremiumCalculationService _premiumCalculationService;

        public EndorsementProcessingService(
            ILogger<EndorsementProcessingService> logger,
            IPremiumCalculationService premiumCalculationService)
        {
            _logger = logger ?? throw new ArgumentNullException(nameof(logger));
            _premiumCalculationService = premiumCalculationService ?? throw new ArgumentNullException(nameof(premiumCalculationService));
        }

        /// <summary>
        /// Process majoração (premium increase) endorsement.
        /// COBOL Source: Section R0830 - Movement type 103
        /// </summary>
        /// <param name="endorsement">Endorsement data</param>
        /// <param name="originalPremium">Original premium before endorsement</param>
        /// <returns>New premium amount (positive increase)</returns>
        public decimal ProcessMajoracao(Endorsement endorsement, decimal originalPremium)
        {
            if (endorsement == null) throw new ArgumentNullException(nameof(endorsement));
            if (endorsement.EndorsementType != "M")
            {
                throw new ArgumentException(
                    $"Endorsement type must be 'M' for majoração, got '{endorsement.EndorsementType}'",
                    nameof(endorsement));
            }

            _logger.LogDebug(
                "Processing majoração for policy {PolicyNumber}, endorsement {EndorsementNumber}: Original={OriginalPremium}, Impact={PremiumImpact}",
                endorsement.PolicyNumber, endorsement.EndorsementNumber, originalPremium, endorsement.PremiumImpact);

            // Majoração adds to original premium
            var newPremium = originalPremium + Math.Abs(endorsement.PremiumImpact);
            newPremium = Math.Round(newPremium, 2, MidpointRounding.ToEven);

            _logger.LogInformation(
                "Majoração processed: Policy={PolicyNumber}, Original={OriginalPremium}, New={NewPremium}, Increase={Increase}",
                endorsement.PolicyNumber, originalPremium, newPremium, endorsement.PremiumImpact);

            return newPremium;
        }

        /// <summary>
        /// Process redução (premium decrease) endorsement.
        /// COBOL Source: Section R0840 - Movement type 104
        /// </summary>
        /// <param name="endorsement">Endorsement data</param>
        /// <param name="originalPremium">Original premium before endorsement</param>
        /// <returns>New premium amount (reduced, but >= 0)</returns>
        public decimal ProcessReducao(Endorsement endorsement, decimal originalPremium)
        {
            if (endorsement == null) throw new ArgumentNullException(nameof(endorsement));
            if (endorsement.EndorsementType != "R")
            {
                throw new ArgumentException(
                    $"Endorsement type must be 'R' for redução, got '{endorsement.EndorsementType}'",
                    nameof(endorsement));
            }

            _logger.LogDebug(
                "Processing redução for policy {PolicyNumber}, endorsement {EndorsementNumber}: Original={OriginalPremium}, Impact={PremiumImpact}",
                endorsement.PolicyNumber, endorsement.EndorsementNumber, originalPremium, endorsement.PremiumImpact);

            // Redução subtracts from original premium
            var reduction = Math.Abs(endorsement.PremiumImpact);
            var newPremium = Math.Max(0m, originalPremium - reduction);
            newPremium = Math.Round(newPremium, 2, MidpointRounding.ToEven);

            if (newPremium == 0 && originalPremium > 0)
            {
                _logger.LogWarning(
                    "Redução resulted in zero premium for policy {PolicyNumber}, original was {OriginalPremium}",
                    endorsement.PolicyNumber, originalPremium);
            }

            _logger.LogInformation(
                "Redução processed: Policy={PolicyNumber}, Original={OriginalPremium}, New={NewPremium}, Decrease={Decrease}",
                endorsement.PolicyNumber, originalPremium, newPremium, reduction);

            return newPremium;
        }

        /// <summary>
        /// Process cancelamento (cancellation) endorsement.
        /// COBOL Source: Section R0850 - Movement type 105
        /// </summary>
        /// <param name="endorsement">Endorsement data</param>
        /// <returns>Negative premium for cancellation refund</returns>
        public decimal ProcessCancelamento(Endorsement endorsement)
        {
            if (endorsement == null) throw new ArgumentNullException(nameof(endorsement));
            if (endorsement.EndorsementType != "C")
            {
                throw new ArgumentException(
                    $"Endorsement type must be 'C' for cancelamento, got '{endorsement.EndorsementType}'",
                    nameof(endorsement));
            }

            _logger.LogDebug(
                "Processing cancelamento for policy {PolicyNumber}, endorsement {EndorsementNumber}: Impact={PremiumImpact}",
                endorsement.PolicyNumber, endorsement.EndorsementNumber, endorsement.PremiumImpact);

            // Cancellation always generates negative premium (refund)
            var cancellationPremium = -Math.Abs(endorsement.PremiumImpact);
            cancellationPremium = Math.Round(cancellationPremium, 2, MidpointRounding.ToEven);

            _logger.LogInformation(
                "Cancelamento processed: Policy={PolicyNumber}, Refund={Refund}",
                endorsement.PolicyNumber, cancellationPremium);

            return cancellationPremium;
        }

        /// <summary>
        /// Apply pro-rata calculation for mid-term endorsements.
        /// COBOL Source: Section R0870 - Pro-rata calculation
        /// Formula: ProRata = Premium * (RemainingDays / TotalDays)
        /// </summary>
        /// <param name="premium">Full-term premium amount</param>
        /// <param name="effectiveDate">Endorsement effective date</param>
        /// <param name="expirationDate">Policy expiration date</param>
        /// <returns>Pro-rated premium for remaining term</returns>
        public decimal ApplyProRata(decimal premium, DateTime effectiveDate, DateTime expirationDate)
        {
            if (effectiveDate >= expirationDate)
            {
                throw new ArgumentException(
                    $"Effective date {effectiveDate:yyyy-MM-dd} must be before expiration date {expirationDate:yyyy-MM-dd}");
            }

            var remainingDays = (expirationDate - effectiveDate).Days;
            var totalDays = 365; // Standard annual policy

            if (remainingDays <= 0)
            {
                _logger.LogWarning(
                    "Pro-rata calculation has zero or negative remaining days: Effective={EffectiveDate}, Expiration={ExpirationDate}",
                    effectiveDate, expirationDate);
                return 0m;
            }

            var proRataFactor = (decimal)remainingDays / totalDays;
            var proRatedPremium = premium * proRataFactor;
            proRatedPremium = Math.Round(proRatedPremium, 2, MidpointRounding.ToEven);

            _logger.LogDebug(
                "Pro-rata calculated: Premium={Premium}, RemainingDays={RemainingDays}, TotalDays={TotalDays}, ProRated={ProRatedPremium}",
                premium, remainingDays, totalDays, proRatedPremium);

            return proRatedPremium;
        }

        /// <summary>
        /// Process restituição (restitution) endorsement.
        /// COBOL Source: Section R0860 - Movement type 106
        /// </summary>
        /// <param name="endorsement">Endorsement data</param>
        /// <returns>Negative premium for restitution</returns>
        public decimal ProcessRestituicao(Endorsement endorsement)
        {
            if (endorsement == null) throw new ArgumentNullException(nameof(endorsement));

            _logger.LogDebug(
                "Processing restituição for policy {PolicyNumber}, endorsement {EndorsementNumber}: Impact={PremiumImpact}",
                endorsement.PolicyNumber, endorsement.EndorsementNumber, endorsement.PremiumImpact);

            // Restitution generates negative premium (return to insured)
            var restitutionPremium = -Math.Abs(endorsement.PremiumImpact);
            restitutionPremium = Math.Round(restitutionPremium, 2, MidpointRounding.ToEven);

            _logger.LogInformation(
                "Restituição processed: Policy={PolicyNumber}, Restitution={Restitution}",
                endorsement.PolicyNumber, restitutionPremium);

            return restitutionPremium;
        }

        /// <summary>
        /// Calculate endorsement impact on premium considering all factors.
        /// COBOL Source: Section R0880 - Complete endorsement processing
        /// </summary>
        /// <param name="endorsement">Endorsement to process</param>
        /// <param name="originalPremium">Original premium before endorsement</param>
        /// <param name="applyProRata">Whether to apply pro-rata calculation</param>
        /// <returns>Final premium amount after endorsement</returns>
        public decimal CalculateEndorsementImpact(
            Endorsement endorsement,
            decimal originalPremium,
            bool applyProRata = false)
        {
            if (endorsement == null) throw new ArgumentNullException(nameof(endorsement));

            decimal finalPremium;

            // Process based on endorsement type
            switch (endorsement.EndorsementType)
            {
                case "M": // Majoração (increase)
                    finalPremium = ProcessMajoracao(endorsement, originalPremium);
                    break;

                case "R": // Redução (decrease)
                    finalPremium = ProcessReducao(endorsement, originalPremium);
                    break;

                case "C": // Cancelamento (cancellation)
                    finalPremium = ProcessCancelamento(endorsement);
                    break;

                default:
                    _logger.LogWarning(
                        "Unknown endorsement type '{EndorsementType}' for policy {PolicyNumber}, using original premium",
                        endorsement.EndorsementType, endorsement.PolicyNumber);
                    finalPremium = originalPremium;
                    break;
            }

            // Apply pro-rata if requested and applicable
            if (applyProRata && endorsement.EffectiveDate < endorsement.EndDate)
            {
                finalPremium = ApplyProRata(
                    finalPremium,
                    endorsement.EffectiveDate,
                    endorsement.EndDate);
            }

            return finalPremium;
        }
    }
}
