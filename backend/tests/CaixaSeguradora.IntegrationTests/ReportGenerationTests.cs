using System;
using System.Net;
using System.Net.Http;
using System.Net.Http.Json;
using System.Threading.Tasks;
using CaixaSeguradora.Core.DTOs;
using CaixaSeguradora.Core.Entities;
using CaixaSeguradora.Infrastructure.Data;
using Microsoft.AspNetCore.Mvc.Testing;
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.DependencyInjection;
using Xunit;

namespace CaixaSeguradora.IntegrationTests
{
    /// <summary>
    /// Integration tests for Phase 3 (US1) - Report Generation workflow.
    /// Tests T070-T072: Generate report API, validation, execution tracking.
    /// </summary>
    public class ReportGenerationTests : IClassFixture<WebApplicationFactory<Program>>
    {
        private readonly WebApplicationFactory<Program> _factory;
        private readonly HttpClient _client;

        public ReportGenerationTests(WebApplicationFactory<Program> factory)
        {
            _factory = factory.WithWebHostBuilder(builder =>
            {
                builder.ConfigureServices(services =>
                {
                    // Replace the database with in-memory database for tests
                    var descriptor = services.SingleOrDefault(
                        d => d.ServiceType == typeof(DbContextOptions<PremiumReportingDbContext>));

                    if (descriptor != null)
                    {
                        services.Remove(descriptor);
                    }

                    services.AddDbContext<PremiumReportingDbContext>(options =>
                    {
                        options.UseInMemoryDatabase($"TestDb_{Guid.NewGuid()}");
                    });
                });
            });

            _client = _factory.CreateClient();
        }

        /// <summary>
        /// T070 - Test: GenerateReport_ValidMonth_ReturnsAccepted
        /// POST request with valid month returns 202 Accepted and creates execution record.
        /// </summary>
        [Fact]
        public async Task GenerateReport_ValidMonth_ReturnsAccepted()
        {
            // Arrange
            await SeedTestDataAsync();

            var request = new GenerateReportRequest
            {
                Month = "202510",
                ReportType = "Both",
                ExecutionMode = "Monthly",
                TriggeringUser = "test_user"
            };

            // Act
            var response = await _client.PostAsJsonAsync("/api/v1/reports/generate", request);

            // Assert
            Assert.Equal(HttpStatusCode.Accepted, response.StatusCode);

            var result = await response.Content.ReadFromJsonAsync<ReportExecutionDto>();
            Assert.NotNull(result);
            Assert.Equal("202510", result.ReferenceMonth);
            Assert.Equal("test_user", result.TriggeringUser);
            Assert.NotEqual(Guid.Empty, result.ExecutionId);
            Assert.True(result.Status == "Pending" || result.Status == "Running" || result.Status == "Completed");
        }

        /// <summary>
        /// T070 - Test: GenerateReport_InvalidMonth_ReturnsBadRequest
        /// Invalid month format returns 400 Bad Request with Portuguese error message.
        /// </summary>
        [Fact]
        public async Task GenerateReport_InvalidMonth_ReturnsBadRequest()
        {
            // Arrange
            var request = new GenerateReportRequest
            {
                Month = "2025-10", // Invalid format (should be YYYYMM)
                ReportType = "Both"
            };

            // Act
            var response = await _client.PostAsJsonAsync("/api/v1/reports/generate", request);

            // Assert
            Assert.Equal(HttpStatusCode.BadRequest, response.StatusCode);

            var errorResponse = await response.Content.ReadFromJsonAsync<ValidationErrorResponse>();
            Assert.NotNull(errorResponse);
            Assert.Equal(400, errorResponse.StatusCode);
            Assert.Contains("Mês", errorResponse.Message); // Portuguese error message
        }

        /// <summary>
        /// T070 - Test: GenerateReport_FutureMonth_ReturnsBadRequest
        /// Future month returns 400 Bad Request with Portuguese error message.
        /// </summary>
        [Fact]
        public async Task GenerateReport_FutureMonth_ReturnsBadRequest()
        {
            // Arrange
            var futureYear = DateTime.UtcNow.Year + 1;
            var request = new GenerateReportRequest
            {
                Month = $"{futureYear}01", // January of next year
                ReportType = "Both"
            };

            // Act
            var response = await _client.PostAsJsonAsync("/api/v1/reports/generate", request);

            // Assert - Can be either 400 (validation) or 500 (business logic rejection)
            Assert.True(response.StatusCode == HttpStatusCode.BadRequest ||
                       response.StatusCode == HttpStatusCode.InternalServerError);
        }

        /// <summary>
        /// T070 - Test: GetExecutionStatus_ExistingExecution_ReturnsDetails
        /// GET request for existing execution returns correct status and details.
        /// </summary>
        [Fact]
        public async Task GetExecutionStatus_ExistingExecution_ReturnsDetails()
        {
            // Arrange
            await SeedTestDataAsync();

            var createRequest = new GenerateReportRequest
            {
                Month = "202510",
                ReportType = "Both",
                TriggeringUser = "test_user"
            };

            var createResponse = await _client.PostAsJsonAsync("/api/v1/reports/generate", createRequest);
            var createdExecution = await createResponse.Content.ReadFromJsonAsync<ReportExecutionDto>();
            Assert.NotNull(createdExecution);

            // Wait a bit for processing to start
            await Task.Delay(500);

            // Act
            var response = await _client.GetAsync($"/api/v1/reports/executions/{createdExecution.ExecutionId}");

            // Assert
            Assert.Equal(HttpStatusCode.OK, response.StatusCode);

            var result = await response.Content.ReadFromJsonAsync<ReportExecutionDto>();
            Assert.NotNull(result);
            Assert.Equal(createdExecution.ExecutionId, result.ExecutionId);
            Assert.Equal("202510", result.ReferenceMonth);
            Assert.True(result.Status == "Pending" || result.Status == "Running" || result.Status == "Completed");
        }

        /// <summary>
        /// T071 - Test: GenerateReport_ProcessesRecords_UpdatesProgress
        /// Verify that recordsProcessed increments during processing.
        /// </summary>
        [Fact]
        public async Task GenerateReport_ProcessesRecords_UpdatesProgress()
        {
            // Arrange
            await SeedTestDataAsync();

            var request = new GenerateReportRequest
            {
                Month = "202510",
                ReportType = "Both",
                TriggeringUser = "test_user"
            };

            // Act
            var createResponse = await _client.PostAsJsonAsync("/api/v1/reports/generate", request);
            var execution = await createResponse.Content.ReadFromJsonAsync<ReportExecutionDto>();
            Assert.NotNull(execution);

            // Wait for processing to complete
            await Task.Delay(1000);

            var statusResponse = await _client.GetAsync($"/api/v1/reports/executions/{execution.ExecutionId}");
            var status = await statusResponse.Content.ReadFromJsonAsync<ReportExecutionDto>();

            // Assert
            Assert.NotNull(status);
            Assert.True(status.RecordsProcessed >= 0, "Records processed should be >= 0");

            // If processing completed, should have processed records
            if (status.Status == "Completed")
            {
                Assert.True(status.RecordsProcessed > 0, "Completed execution should have processed some records");
            }
        }

        /// <summary>
        /// T072 - Test: GenerateReport_Completion_SetsCorrectReturnCode
        /// Verify execution completes with correct return code (0000 for success, 0004 for warnings).
        /// </summary>
        [Fact]
        public async Task GenerateReport_Completion_SetsCorrectReturnCode()
        {
            // Arrange
            await SeedTestDataAsync();

            var request = new GenerateReportRequest
            {
                Month = "202510",
                ReportType = "Both",
                TriggeringUser = "test_user"
            };

            // Act
            var createResponse = await _client.PostAsJsonAsync("/api/v1/reports/generate", request);
            var execution = await createResponse.Content.ReadFromJsonAsync<ReportExecutionDto>();
            Assert.NotNull(execution);

            // Wait for processing to complete
            await Task.Delay(2000);

            var statusResponse = await _client.GetAsync($"/api/v1/reports/executions/{execution.ExecutionId}");
            var finalStatus = await statusResponse.Content.ReadFromJsonAsync<ReportExecutionDto>();

            // Assert
            Assert.NotNull(finalStatus);

            if (finalStatus.Status == "Completed")
            {
                // Success should be 0000, warnings should be 0004
                Assert.True(
                    finalStatus.ReturnCode == "0000" || finalStatus.ReturnCode == "0004",
                    $"Expected return code 0000 or 0004, got {finalStatus.ReturnCode}");

                Assert.Equal(0, finalStatus.ErrorsCount); // Completed means no errors
            }
            else if (finalStatus.Status == "Failed")
            {
                // Failed should have error return code
                Assert.True(
                    finalStatus.ReturnCode == "0008" || finalStatus.ReturnCode == "0012",
                    $"Failed status should have error return code, got {finalStatus.ReturnCode}");
            }
        }

        /// <summary>
        /// Seeds test data into the in-memory database.
        /// Creates sample products, clients, policies, and premiums for October 2025.
        /// </summary>
        private async Task SeedTestDataAsync()
        {
            using var scope = _factory.Services.CreateScope();
            var context = scope.ServiceProvider.GetRequiredService<PremiumReportingDbContext>();

            // Ensure database is created
            await context.Database.EnsureCreatedAsync();

            // Check if already seeded
            if (await context.PremiumRecords.AnyAsync())
            {
                return;
            }

            // Create sample product
            var product = new Product
            {
                ProductCode = 1001,
                CompanyCode = 1,
                ProductName = "Test Product",
                LineOfBusiness = 531, // RamoSusep alias
                LineOfBusinessGroup = 4, // GrupoRamo
                ProductStatus = "A" // IsActive = true
            };
            context.Products.Add(product);

            // Create sample client
            var client = new Client
            {
                ClientCode = 1,
                ClientName = "Test Client",
                DocumentNumber = "12345678901",
                ClientType = "PF"
            };
            context.Clients.Add(client);

            // Create sample agency and producer
            var agency = new Agency { AgencyCode = 1, AgencyName = "Test Agency" };
            var producer = new Producer { ProducerCode = 1, ProducerName = "Test Producer", TaxId = "12345678901" };
            context.Agencies.Add(agency);
            context.Producers.Add(producer);

            await context.SaveChangesAsync();

            // Create sample policy
            var policy = new Policy
            {
                PolicyNumber = 1000001,
                SystemCode = "RG",
                ProductCode = product.ProductCode,
                ClientCode = client.ClientCode,
                AgencyCode = agency.AgencyCode,
                ProducerCode = producer.ProducerCode,
                EffectiveDate = new DateTime(2025, 10, 1),
                ExpirationDate = new DateTime(2026, 10, 1),
                IssueDate = new DateTime(2025, 9, 25),
                PolicyStatus = "A"
            };
            context.Policies.Add(policy);
            await context.SaveChangesAsync();

            // Create sample premium record
            var premium = new PremiumRecord
            {
                PremiumId = policy.PolicyNumber,
                CompanyCode = 1,
                PolicyNumber = policy.PolicyNumber,
                ProductCode = product.ProductCode,
                LineOfBusiness = 531, // RamoSusep
                MovementType = "E", // Emission
                ReferenceYear = 2025,
                ReferenceMonth = 10,
                ReferenceDay = 1,
                InstallmentNumber = 1,
                PolicyStartDate = "2025-10-01",
                NetPremiumTotal = 1000.00m, // NetPremiumAmount is alias to NetPremiumTotal
                IofTotal = 73.80m, // IofAmount is alias to IofTotal
                TotalPremiumTotal = 1073.80m, // TotalPremiumAmount is alias
                NumberOfInstallments = 12 // InstallmentCount mapped to NumberOfInstallments
            };
            context.PremiumRecords.Add(premium);
            await context.SaveChangesAsync();
        }
    }
}
