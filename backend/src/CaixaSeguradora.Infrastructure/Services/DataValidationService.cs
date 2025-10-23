using System.Diagnostics;
using CaixaSeguradora.Core.DTOs;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Core.Interfaces;
using CaixaSeguradora.Infrastructure.Data;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.Logging;

namespace CaixaSeguradora.Infrastructure.Services;

/// <summary>
/// Service for validating data integrity and foreign key relationships.
/// </summary>
public class DataValidationService : IDataValidationService
{
    private readonly PremiumReportingDbContext _context;
    private readonly ILogger<DataValidationService> _logger;

    public DataValidationService(
        PremiumReportingDbContext context,
        ILogger<DataValidationService> logger)
    {
        _context = context;
        _logger = logger;
    }

    public async Task<DataValidationReport> ValidateForeignKeysAsync(
        CancellationToken cancellationToken = default
    )
    {
        var stopwatch = Stopwatch.StartNew();
        var report = new DataValidationReport();

        _logger.LogInformation("Starting foreign key validation");

        try
        {
            // Validate Premium -> Policy references
            await ValidatePremiumPolicyReferences(report, cancellationToken);

            // Validate Premium -> Client references
            await ValidatePremiumClientReferences(report, cancellationToken);

            // Validate Endorsement -> Policy references
            await ValidateEndorsementPolicyReferences(report, cancellationToken);

            // Validate Policy -> Product references
            await ValidatePolicyProductReferences(report, cancellationToken);

            // Validate Policy -> Client references
            await ValidatePolicyClientReferences(report, cancellationToken);

            // Validate Address -> Client references
            await ValidateAddressClientReferences(report, cancellationToken);

            // Validate CossuredPolicy -> Policy references
            await ValidateCossuredPolicyReferences(report, cancellationToken);

            stopwatch.Stop();
            report.ElapsedTime = stopwatch.Elapsed;

            report.IsValid = report.FailedValidations == 0;
            report.Summary = report.IsValid
                ? $"All foreign key validations passed ({report.PassedValidations} checks)."
                : $"Foreign key validation failed: {report.FailedValidations} issues found.";

            _logger.LogInformation(
                "Foreign key validation completed: {Passed} passed, {Failed} failed, elapsed: {Elapsed}ms",
                report.PassedValidations, report.FailedValidations, report.ElapsedTime.TotalMilliseconds);

            return report;
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error during foreign key validation");
            report.Issues.Add(new ValidationIssue
            {
                EntityType = "System",
                ValidationRule = "ForeignKeyValidation",
                Description = $"Validation error: {ex.Message}",
                Severity = ValidationSeverity.Error
            });
            report.IsValid = false;
            return report;
        }
    }

    public async Task<DataValidationReport> ValidateDataIntegrityAsync(
        CancellationToken cancellationToken = default
    )
    {
        var stopwatch = Stopwatch.StartNew();
        var report = new DataValidationReport();

        _logger.LogInformation("Starting data integrity validation");

        try
        {
            // Validate required fields
            await ValidateRequiredFields(report, cancellationToken);

            // Validate data ranges
            await ValidateDataRanges(report, cancellationToken);

            // Validate business rules
            await ValidateBusinessRules(report, cancellationToken);

            stopwatch.Stop();
            report.ElapsedTime = stopwatch.Elapsed;

            report.IsValid = report.FailedValidations == 0;
            report.Summary = report.IsValid
                ? $"All data integrity validations passed ({report.PassedValidations} checks)."
                : $"Data integrity validation failed: {report.FailedValidations} issues found.";

            _logger.LogInformation(
                "Data integrity validation completed: {Passed} passed, {Failed} failed, elapsed: {Elapsed}ms",
                report.PassedValidations, report.FailedValidations, report.ElapsedTime.TotalMilliseconds);

            return report;
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error during data integrity validation");
            report.Issues.Add(new ValidationIssue
            {
                EntityType = "System",
                ValidationRule = "DataIntegrityValidation",
                Description = $"Validation error: {ex.Message}",
                Severity = ValidationSeverity.Error
            });
            report.IsValid = false;
            return report;
        }
    }

    public async Task<DataValidationReport> GetValidationReportAsync(
        CancellationToken cancellationToken = default
    )
    {
        var stopwatch = Stopwatch.StartNew();
        var report = new DataValidationReport();

        _logger.LogInformation("Generating comprehensive validation report");

        try
        {
            // Run foreign key validation
            var fkReport = await ValidateForeignKeysAsync(cancellationToken);
            MergeReports(report, fkReport);

            // Run data integrity validation
            var integrityReport = await ValidateDataIntegrityAsync(cancellationToken);
            MergeReports(report, integrityReport);

            // Collect entity statistics
            await CollectEntityStatistics(report, cancellationToken);

            stopwatch.Stop();
            report.ElapsedTime = stopwatch.Elapsed;

            report.IsValid = report.FailedValidations == 0;
            report.Summary = GenerateSummary(report);

            _logger.LogInformation(
                "Comprehensive validation completed: {Total} validations, {Passed} passed, {Failed} failed",
                report.TotalValidations, report.PassedValidations, report.FailedValidations);

            return report;
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error generating validation report");
            report.Issues.Add(new ValidationIssue
            {
                EntityType = "System",
                ValidationRule = "ValidationReport",
                Description = $"Error generating report: {ex.Message}",
                Severity = ValidationSeverity.Error
            });
            report.IsValid = false;
            return report;
        }
    }

    public async Task<DataValidationReport> ValidateEntityTypeAsync(
        string entityType,
        CancellationToken cancellationToken = default
    )
    {
        var report = new DataValidationReport();

        _logger.LogInformation("Validating entity type: {EntityType}", entityType);

        try
        {
            switch (entityType.ToLower())
            {
                case "premium":
                    await ValidatePremiumRecords(report, cancellationToken);
                    break;
                case "policy":
                    await ValidatePolicyRecords(report, cancellationToken);
                    break;
                case "client":
                    await ValidateClientRecords(report, cancellationToken);
                    break;
                default:
                    report.Issues.Add(new ValidationIssue
                    {
                        EntityType = entityType,
                        ValidationRule = "EntityTypeValidation",
                        Description = $"Entity type '{entityType}' is not supported for validation.",
                        Severity = ValidationSeverity.Error
                    });
                    report.FailedValidations++;
                    break;
            }

            report.IsValid = report.FailedValidations == 0;
            report.Summary = $"Validated {entityType}: {report.PassedValidations} passed, {report.FailedValidations} failed.";

            return report;
        }
        catch (Exception ex)
        {
            _logger.LogError(ex, "Error validating entity type: {EntityType}", entityType);
            report.Issues.Add(new ValidationIssue
            {
                EntityType = entityType,
                ValidationRule = "EntityValidation",
                Description = $"Validation error: {ex.Message}",
                Severity = ValidationSeverity.Error
            });
            report.IsValid = false;
            return report;
        }
    }

    // Private helper methods

    private async Task ValidatePremiumPolicyReferences(
        DataValidationReport report,
        CancellationToken cancellationToken)
    {
        report.TotalValidations++;

        var orphanedPremiums = await _context.PremiumRecords
            .Where(p => !_context.Policies.Any(pol => pol.PolicyNumber == p.PolicyNumber))
            .Take(100) // Limit to first 100 issues
            .ToListAsync(cancellationToken);

        if (orphanedPremiums.Any())
        {
            report.FailedValidations++;
            foreach (var premium in orphanedPremiums)
            {
                report.Issues.Add(new ValidationIssue
                {
                    EntityType = "PremiumRecord",
                    EntityId = premium.PremiumId.ToString(),
                    FieldName = "PolicyNumber",
                    ValidationRule = "ForeignKeyConstraint",
                    Description = $"Premium references non-existent policy: {premium.PolicyNumber}",
                    Severity = ValidationSeverity.Error,
                    ActualValue = premium.PolicyNumber.ToString()
                });
            }
        }
        else
        {
            report.PassedValidations++;
        }
    }

    private async Task ValidatePremiumClientReferences(
        DataValidationReport report,
        CancellationToken cancellationToken)
    {
        report.TotalValidations++;

        var orphanedPremiums = await _context.PremiumRecords
            .Where(p => !_context.Clients.Any(c => c.ClientCode == p.ClientCode))
            .Take(100)
            .ToListAsync(cancellationToken);

        if (orphanedPremiums.Any())
        {
            report.FailedValidations++;
            foreach (var premium in orphanedPremiums)
            {
                report.Issues.Add(new ValidationIssue
                {
                    EntityType = "PremiumRecord",
                    EntityId = premium.PremiumId.ToString(),
                    FieldName = "ClientCode",
                    ValidationRule = "ForeignKeyConstraint",
                    Description = $"Premium references non-existent client: {premium.ClientCode}",
                    Severity = ValidationSeverity.Error,
                    ActualValue = premium.ClientCode.ToString()
                });
            }
        }
        else
        {
            report.PassedValidations++;
        }
    }

    private async Task ValidateEndorsementPolicyReferences(
        DataValidationReport report,
        CancellationToken cancellationToken)
    {
        report.TotalValidations++;

        var orphanedEndorsements = await _context.Endorsements
            .Where(e => !_context.Policies.Any(p => p.PolicyNumber == e.PolicyNumber))
            .Take(100)
            .ToListAsync(cancellationToken);

        if (orphanedEndorsements.Any())
        {
            report.FailedValidations++;
            foreach (var endorsement in orphanedEndorsements)
            {
                report.Issues.Add(new ValidationIssue
                {
                    EntityType = "Endorsement",
                    EntityId = endorsement.EndorsementId.ToString(),
                    FieldName = "PolicyNumber",
                    ValidationRule = "ForeignKeyConstraint",
                    Description = $"Endorsement references non-existent policy: {endorsement.PolicyNumber}",
                    Severity = ValidationSeverity.Error,
                    ActualValue = endorsement.PolicyNumber.ToString()
                });
            }
        }
        else
        {
            report.PassedValidations++;
        }
    }

    private async Task ValidatePolicyProductReferences(
        DataValidationReport report,
        CancellationToken cancellationToken)
    {
        report.TotalValidations++;

        var policiesWithInvalidProduct = await _context.Policies
            .Where(p => !_context.Products.Any(pr => pr.ProductCode == p.ProductCode))
            .Take(100)
            .ToListAsync(cancellationToken);

        if (policiesWithInvalidProduct.Any())
        {
            report.FailedValidations++;
            foreach (var policy in policiesWithInvalidProduct)
            {
                report.Issues.Add(new ValidationIssue
                {
                    EntityType = "Policy",
                    EntityId = policy.PolicyId.ToString(),
                    FieldName = "ProductCode",
                    ValidationRule = "ForeignKeyConstraint",
                    Description = $"Policy references non-existent product: {policy.ProductCode}",
                    Severity = ValidationSeverity.Error,
                    ActualValue = policy.ProductCode.ToString()
                });
            }
        }
        else
        {
            report.PassedValidations++;
        }
    }

    private async Task ValidatePolicyClientReferences(
        DataValidationReport report,
        CancellationToken cancellationToken)
    {
        report.TotalValidations++;

        var policiesWithInvalidClient = await _context.Policies
            .Where(p => !_context.Clients.Any(c => c.ClientCode == p.InsuredClientCode))
            .Take(100)
            .ToListAsync(cancellationToken);

        if (policiesWithInvalidClient.Any())
        {
            report.FailedValidations++;
            foreach (var policy in policiesWithInvalidClient)
            {
                report.Issues.Add(new ValidationIssue
                {
                    EntityType = "Policy",
                    EntityId = policy.PolicyId.ToString(),
                    FieldName = "InsuredClientCode",
                    ValidationRule = "ForeignKeyConstraint",
                    Description = $"Policy references non-existent client: {policy.InsuredClientCode}",
                    Severity = ValidationSeverity.Error,
                    ActualValue = policy.InsuredClientCode.ToString()
                });
            }
        }
        else
        {
            report.PassedValidations++;
        }
    }

    private async Task ValidateAddressClientReferences(
        DataValidationReport report,
        CancellationToken cancellationToken)
    {
        report.TotalValidations++;

        var addressesWithInvalidClient = await _context.Addresses
            .Where(a => !_context.Clients.Any(c => c.ClientCode == a.ClientCode))
            .Take(100)
            .ToListAsync(cancellationToken);

        if (addressesWithInvalidClient.Any())
        {
            report.FailedValidations++;
            foreach (var address in addressesWithInvalidClient)
            {
                report.Issues.Add(new ValidationIssue
                {
                    EntityType = "Address",
                    EntityId = address.AddressId.ToString(),
                    FieldName = "ClientCode",
                    ValidationRule = "ForeignKeyConstraint",
                    Description = $"Address references non-existent client: {address.ClientCode}",
                    Severity = ValidationSeverity.Error,
                    ActualValue = address.ClientCode.ToString()
                });
            }
        }
        else
        {
            report.PassedValidations++;
        }
    }

    private async Task ValidateCossuredPolicyReferences(
        DataValidationReport report,
        CancellationToken cancellationToken)
    {
        report.TotalValidations++;

        var invalidCossured = await _context.CossuredPolicies
            .Where(cp => !_context.Policies.Any(p => p.PolicyNumber == cp.PolicyNumber))
            .Take(100)
            .ToListAsync(cancellationToken);

        if (invalidCossured.Any())
        {
            report.FailedValidations++;
            foreach (var cossured in invalidCossured)
            {
                report.Issues.Add(new ValidationIssue
                {
                    EntityType = "CossuredPolicy",
                    EntityId = cossured.CossuredPolicyId.ToString(),
                    FieldName = "PolicyNumber",
                    ValidationRule = "ForeignKeyConstraint",
                    Description = $"Cossured policy references non-existent policy: {cossured.PolicyNumber}",
                    Severity = ValidationSeverity.Error,
                    ActualValue = cossured.PolicyNumber.ToString()
                });
            }
        }
        else
        {
            report.PassedValidations++;
        }
    }

    private async Task ValidateRequiredFields(
        DataValidationReport report,
        CancellationToken cancellationToken)
    {
        report.TotalValidations++;

        // Validate policies have required fields
        var invalidPolicies = await _context.Policies
            .Where(p => string.IsNullOrEmpty(p.PolicyStatusCode) || p.PolicyNumber == 0)
            .Take(100)
            .ToListAsync(cancellationToken);

        if (invalidPolicies.Any())
        {
            report.FailedValidations++;
            foreach (var policy in invalidPolicies)
            {
                report.Issues.Add(new ValidationIssue
                {
                    EntityType = "Policy",
                    EntityId = policy.PolicyId.ToString(),
                    ValidationRule = "RequiredField",
                    Description = "Policy missing required fields (PolicyStatusCode or PolicyNumber)",
                    Severity = ValidationSeverity.Error
                });
            }
        }
        else
        {
            report.PassedValidations++;
        }
    }

    private async Task ValidateDataRanges(
        DataValidationReport report,
        CancellationToken cancellationToken)
    {
        report.TotalValidations++;

        // Validate premium amounts are positive
        var invalidPremiums = await _context.PremiumRecords
            .Where(p => p.NetPremiumAmount < 0 || p.GrossPremiumAmount < 0)
            .Take(100)
            .ToListAsync(cancellationToken);

        if (invalidPremiums.Any())
        {
            report.FailedValidations++;
            foreach (var premium in invalidPremiums)
            {
                report.Issues.Add(new ValidationIssue
                {
                    EntityType = "PremiumRecord",
                    EntityId = premium.PremiumId.ToString(),
                    ValidationRule = "DataRange",
                    Description = "Premium amounts cannot be negative",
                    Severity = ValidationSeverity.Error,
                    ActualValue = $"Net: {premium.NetPremiumAmount}, Gross: {premium.GrossPremiumAmount}"
                });
            }
        }
        else
        {
            report.PassedValidations++;
        }
    }

    private async Task ValidateBusinessRules(
        DataValidationReport report,
        CancellationToken cancellationToken)
    {
        report.TotalValidations++;

        // Validate policy end date is after start date
        // Note: PolicyStartDate and PolicyEndDate are strings in format "yyyy-MM-dd"
        // String comparison works for ISO date format
        var invalidDatePolicies = await _context.Policies
            .Where(p => string.Compare(p.PolicyEndDate, p.PolicyStartDate) < 0)
            .Take(100)
            .ToListAsync(cancellationToken);

        if (invalidDatePolicies.Any())
        {
            report.FailedValidations++;
            foreach (var policy in invalidDatePolicies)
            {
                report.Issues.Add(new ValidationIssue
                {
                    EntityType = "Policy",
                    EntityId = policy.PolicyId.ToString(),
                    ValidationRule = "BusinessRule",
                    Description = "Policy end date must be after start date",
                    Severity = ValidationSeverity.Error,
                    ActualValue = $"Start: {policy.PolicyStartDate}, End: {policy.PolicyEndDate}"
                });
            }
        }
        else
        {
            report.PassedValidations++;
        }
    }

    private async Task ValidatePremiumRecords(
        DataValidationReport report,
        CancellationToken cancellationToken)
    {
        var premiumCount = await _context.PremiumRecords.CountAsync(cancellationToken);
        var stats = new EntityValidationStats
        {
            EntityType = "PremiumRecord",
            TotalRecords = premiumCount
        };

        // Add specific validations for premiums
        await ValidatePremiumPolicyReferences(report, cancellationToken);
        await ValidatePremiumClientReferences(report, cancellationToken);

        report.EntityStats["PremiumRecord"] = stats;
    }

    private async Task ValidatePolicyRecords(
        DataValidationReport report,
        CancellationToken cancellationToken)
    {
        var policyCount = await _context.Policies.CountAsync(cancellationToken);
        var stats = new EntityValidationStats
        {
            EntityType = "Policy",
            TotalRecords = policyCount
        };

        // Add specific validations for policies
        await ValidatePolicyProductReferences(report, cancellationToken);
        await ValidatePolicyClientReferences(report, cancellationToken);

        report.EntityStats["Policy"] = stats;
    }

    private async Task ValidateClientRecords(
        DataValidationReport report,
        CancellationToken cancellationToken)
    {
        var clientCount = await _context.Clients.CountAsync(cancellationToken);
        var stats = new EntityValidationStats
        {
            EntityType = "Client",
            TotalRecords = clientCount
        };

        report.EntityStats["Client"] = stats;
        report.PassedValidations++;
        report.TotalValidations++;
    }

    private async Task CollectEntityStatistics(
        DataValidationReport report,
        CancellationToken cancellationToken)
    {
        var entityTypes = new[]
        {
            ("PremiumRecord", await _context.PremiumRecords.CountAsync(cancellationToken)),
            ("Policy", await _context.Policies.CountAsync(cancellationToken)),
            ("Client", await _context.Clients.CountAsync(cancellationToken)),
            ("Product", await _context.Products.CountAsync(cancellationToken)),
            ("Endorsement", await _context.Endorsements.CountAsync(cancellationToken)),
            ("Coverage", await _context.Coverages.CountAsync(cancellationToken)),
            ("Address", await _context.Addresses.CountAsync(cancellationToken)),
            ("Agency", await _context.Agencies.CountAsync(cancellationToken)),
            ("Producer", await _context.Producers.CountAsync(cancellationToken)),
            ("Invoice", await _context.Invoices.CountAsync(cancellationToken)),
            ("Installment", await _context.Installments.CountAsync(cancellationToken)),
            ("CossuredPolicy", await _context.CossuredPolicies.CountAsync(cancellationToken)),
            ("CossuranceCalculation", await _context.CossuranceCalculations.CountAsync(cancellationToken))
        };

        foreach (var (entityType, count) in entityTypes)
        {
            if (!report.EntityStats.ContainsKey(entityType))
            {
                report.EntityStats[entityType] = new EntityValidationStats
                {
                    EntityType = entityType,
                    TotalRecords = count
                };
            }
        }

        // Count issues per entity type
        foreach (var issue in report.Issues)
        {
            if (report.EntityStats.TryGetValue(issue.EntityType, out var stats))
            {
                if (issue.Severity == ValidationSeverity.Error)
                {
                    stats.ErrorCount++;
                }
                else if (issue.Severity == ValidationSeverity.Warning)
                {
                    stats.WarningCount++;
                }
                stats.RecordsWithIssues++;
            }
        }
    }

    private void MergeReports(DataValidationReport target, DataValidationReport source)
    {
        target.TotalValidations += source.TotalValidations;
        target.PassedValidations += source.PassedValidations;
        target.FailedValidations += source.FailedValidations;
        target.WarningCount += source.WarningCount;
        target.Issues.AddRange(source.Issues);

        foreach (var (key, value) in source.EntityStats)
        {
            if (target.EntityStats.ContainsKey(key))
            {
                var targetStats = target.EntityStats[key];
                targetStats.ErrorCount += value.ErrorCount;
                targetStats.WarningCount += value.WarningCount;
                targetStats.RecordsWithIssues += value.RecordsWithIssues;
            }
            else
            {
                target.EntityStats[key] = value;
            }
        }
    }

    private string GenerateSummary(DataValidationReport report)
    {
        var sb = new System.Text.StringBuilder();
        sb.AppendLine($"Validation completed with {report.TotalValidations} total checks.");
        sb.AppendLine($"Passed: {report.PassedValidations}, Failed: {report.FailedValidations}, Warnings: {report.WarningCount}");

        if (report.EntityStats.Any())
        {
            sb.AppendLine($"Validated {report.EntityStats.Count} entity types:");
            foreach (var (entityType, stats) in report.EntityStats)
            {
                sb.AppendLine($"  - {entityType}: {stats.TotalRecords} records, {stats.ErrorCount} errors, {stats.WarningCount} warnings");
            }
        }

        return sb.ToString();
    }
}
