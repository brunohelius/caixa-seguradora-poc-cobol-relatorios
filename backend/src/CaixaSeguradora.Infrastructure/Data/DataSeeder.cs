using CaixaSeguradora.Core.Entities;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Logging;

namespace CaixaSeguradora.Infrastructure.Data
{
    /// <summary>
    /// Simple data seeder for Phase 3 testing (T073-T074).
    /// Creates minimal sample data for report generation testing.
    /// </summary>
    public class DataSeeder
    {
        private readonly PremiumReportingDbContext _context;
        private readonly ILogger<DataSeeder> _logger;

        public DataSeeder(PremiumReportingDbContext context, ILogger<DataSeeder> logger)
        {
            _context = context;
            _logger = logger;
        }

        /// <summary>
        /// Seeds minimal sample data for testing Phase 3 functionality.
        /// </summary>
        public async Task SeedSampleDataAsync()
        {
            _logger.LogInformation("Starting data seeding process");

            try
            {
                // Check if data already exists
                var existingPremiums = await _context.PremiumRecords.AnyAsync();
                if (existingPremiums)
                {
                    _logger.LogWarning("Database already contains premium records. Skipping seed.");
                    return;
                }

                _logger.LogInformation("Creating minimal sample data for Phase 3 testing");

                // Create 10 simple premium records for October 2025
                for (int i = 1; i <= 10; i++)
                {
                    var premium = new PremiumRecord
                    {
                        PremiumId = 1000000 + i,
                        PolicyNumber = 1000000 + i,
                        ProductCode = 1001,
                        MovementType = "1", // Emission
                        NetPremiumTotal = 1000.00m * i,
                        IofTotal = 73.80m * i,
                        TotalPremiumTotal = 1073.80m * i
                    };

                    _context.PremiumRecords.Add(premium);
                }

                await _context.SaveChangesAsync();

                _logger.LogInformation("Data seeding completed: 10 sample premium records created");
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error during data seeding");
                throw;
            }
        }
    }
}
