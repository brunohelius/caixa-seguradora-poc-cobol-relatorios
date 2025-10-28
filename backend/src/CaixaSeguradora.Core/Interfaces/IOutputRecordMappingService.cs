using CaixaSeguradora.Core.DTOs;
using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Core.Interfaces
{
    /// <summary>
    /// Service interface for mapping domain entities to SUSEP output record DTOs.
    /// </summary>
    public interface IOutputRecordMappingService
    {
        /// <summary>
        /// Map PremiumRecord with related entities to PREMIT output format (765 bytes).
        /// </summary>
        PremitOutputRecord MapToPremitRecord(
            PremiumRecord premium,
            Policy policy = null,
            Client client = null,
            Product product = null);

        /// <summary>
        /// Map cossurance policy with premium context to PREMCED output format (168 bytes).
        /// </summary>
        PremcedOutputRecord MapToPremcedRecord(
            CossuredPolicy cossurance,
            PremiumRecord premium,
            Policy policy = null,
            int sequenceNumber = 1);
    }
}
