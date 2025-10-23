using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Design;
using CaixaSeguradora.Infrastructure.Data;

namespace CaixaSeguradora.Api
{
    public class DesignTimeDbContextFactory : IDesignTimeDbContextFactory<PremiumReportingDbContext>
    {
        public PremiumReportingDbContext CreateDbContext(string[] args)
        {
            IConfigurationRoot configuration = new ConfigurationBuilder()
                .SetBasePath(Directory.GetCurrentDirectory())
                .AddJsonFile("appsettings.json", optional: false, reloadOnChange: true)
                .AddJsonFile($"appsettings.Development.json", optional: true)
                .AddEnvironmentVariables()
                .Build();

            var connectionString = configuration.GetConnectionString("DefaultConnection")
                ?? "Data Source=premium_reporting.db";

            var optionsBuilder = new DbContextOptionsBuilder<PremiumReportingDbContext>();
            optionsBuilder.UseSqlite(connectionString);

            return new PremiumReportingDbContext(optionsBuilder.Options);
        }
    }
}
