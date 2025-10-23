using Microsoft.EntityFrameworkCore;
using CaixaSeguradora.Core.Entities;

namespace CaixaSeguradora.Infrastructure.Data
{
    public class PremiumReportingDbContext : DbContext
    {
        public PremiumReportingDbContext(DbContextOptions<PremiumReportingDbContext> options) : base(options)
        {
        }

        public DbSet<PremiumRecord> PremiumRecords { get; set; }
        public DbSet<Policy> Policies { get; set; }
        public DbSet<Endorsement> Endorsements { get; set; }
        public DbSet<Product> Products { get; set; }
        public DbSet<Client> Clients { get; set; }
        public DbSet<Address> Addresses { get; set; }
        public DbSet<Agency> Agencies { get; set; }
        public DbSet<Producer> Producers { get; set; }
        public DbSet<Coverage> Coverages { get; set; }
        public DbSet<Invoice> Invoices { get; set; }
        public DbSet<Installment> Installments { get; set; }
        public DbSet<CossuredPolicy> CossuredPolicies { get; set; }
        public DbSet<CossuranceCalculation> CossuranceCalculations { get; set; }
        public DbSet<SystemConfiguration> SystemConfigurations { get; set; }
        public DbSet<ReportDefinition> ReportDefinitions { get; set; }
        public DbSet<BatchJob> BatchJobs { get; set; }
        public DbSet<BatchJobExecution> BatchJobExecutions { get; set; }

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            // Configure all entity configurations
            modelBuilder.ApplyConfigurationsFromAssembly(typeof(PremiumReportingDbContext).Assembly);
            
            base.OnModelCreating(modelBuilder);
        }
    }
}