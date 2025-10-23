using Microsoft.EntityFrameworkCore;
using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Infrastructure.Data
{
    public class PremiumReportingDbContext : DbContext
    {
        public PremiumReportingDbContext(DbContextOptions<PremiumReportingDbContext> options)
            : base(options)
        {
        }

        // DbSets for all entities
        public DbSet<PremiumRecord> PremiumRecords => Set<PremiumRecord>();
        public DbSet<Policy> Policies => Set<Policy>();
        public DbSet<Endorsement> Endorsements => Set<Endorsement>();
        public DbSet<Product> Products => Set<Product>();
        public DbSet<Client> Clients => Set<Client>();
        public DbSet<Address> Addresses => Set<Address>();
        public DbSet<Agency> Agencies => Set<Agency>();
        public DbSet<Producer> Producers => Set<Producer>();
        public DbSet<Coverage> Coverages => Set<Coverage>();
        public DbSet<Invoice> Invoices => Set<Invoice>();
        public DbSet<Installment> Installments => Set<Installment>();
        public DbSet<CossuredPolicy> CossuredPolicies => Set<CossuredPolicy>();
        public DbSet<CossuranceCalculation> CossuranceCalculations => Set<CossuranceCalculation>();
        public DbSet<SystemConfiguration> SystemConfigurations => Set<SystemConfiguration>();
        public DbSet<ReportDefinition> ReportDefinitions => Set<ReportDefinition>();

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            base.OnModelCreating(modelBuilder);

            // Apply all entity configurations
            modelBuilder.ApplyConfigurationsFromAssembly(typeof(PremiumReportingDbContext).Assembly);
        }
    }
}
