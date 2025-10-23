using System;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Net.Http.Json;
using System.Threading.Tasks;
using CaixaSeguradora.Core.DTOs;
using FluentAssertions;
using Microsoft.AspNetCore.Mvc.Testing;
using Xunit;

namespace CaixaSeguradora.IntegrationTests.Workflows;

/// <summary>
/// Integration tests for the complete report generation workflow.
/// Tests User Story 2 - Generate Premium Reports end-to-end.
/// </summary>
public class ReportGenerationWorkflowTests : IClassFixture<WebApplicationFactory<Program>>
{
    private readonly HttpClient _client;
    private readonly WebApplicationFactory<Program> _factory;

    public ReportGenerationWorkflowTests(WebApplicationFactory<Program> factory)
    {
        _factory = factory;
        _client = factory.CreateClient();
    }

    [Fact]
    public async Task CompleteReportGenerationWorkflow_WithValidData_ShouldSucceed()
    {
        // Arrange: Load mock data first
        await LoadMockDataAsync();

        var request = new
        {
            StartDate = "2025-10-01",
            EndDate = "2025-10-31",
            SystemCode = "GL",
            ReportType = "PREMIT",
            IncludeCancelled = false
        };

        // Act 1: Generate report
        HttpResponseMessage generateResponse = await _client.PostAsJsonAsync("/api/v1/reports/generate", request);
        generateResponse.StatusCode.Should().Be(HttpStatusCode.Accepted);

        var generateResult = await generateResponse.Content.ReadFromJsonAsync<ReportGenerationResponse>();
        generateResult.Should().NotBeNull();
        generateResult!.JobId.Should().NotBeEmpty();

        // Act 2: Poll for status
        var jobId = generateResult.JobId;
        ReportStatusResponse? statusResult = null;
        var pollAttempts = 0;
        const int maxPollAttempts = 30;

        while (pollAttempts < maxPollAttempts)
        {
            await Task.Delay(1000); // Wait 1 second between polls

            var statusResponse = await _client.GetAsync($"/api/v1/reports/status/{jobId}");
            statusResponse.StatusCode.Should().Be(HttpStatusCode.OK);

            statusResult = await statusResponse.Content.ReadFromJsonAsync<ReportStatusResponse>();

            if (statusResult?.Status == "Completed" || statusResult?.Status == "Failed")
            {
                break;
            }

            pollAttempts++;
        }

        // Assert: Report should complete successfully
        statusResult.Should().NotBeNull();
        statusResult!.Status.Should().Be("Completed");
        statusResult.OutputFilePath.Should().NotBeNullOrEmpty();

        // Act 3: Download report
        var downloadResponse = await _client.GetAsync($"/api/v1/reports/download/{jobId}");
        downloadResponse.StatusCode.Should().Be(HttpStatusCode.OK);

        var fileBytes = await downloadResponse.Content.ReadAsByteArrayAsync();
        fileBytes.Should().NotBeEmpty();

        // Assert: File should have content
        fileBytes.Length.Should().BeGreaterThan(0);
    }

    [Fact]
    public async Task ReportGeneration_WithInvalidDateRange_ShouldReturnBadRequest()
    {
        // Arrange: End date before start date
        var request = new
        {
            StartDate = "2025-10-31",
            EndDate = "2025-10-01",
            SystemCode = "GL",
            ReportType = "PREMIT"
        };

        // Act
        HttpResponseMessage response = await _client.PostAsJsonAsync("/api/v1/reports/generate", request);

        // Assert
        response.StatusCode.Should().Be(HttpStatusCode.BadRequest);
    }

    [Fact]
    public async Task ReportGeneration_WithEmptyDatabase_ShouldCompleteWithNoRecords()
    {
        // Arrange: Reset database to ensure empty state
        await _client.PostAsync("/api/mock-data/reset", null);

        var request = new
        {
            StartDate = "2025-10-01",
            EndDate = "2025-10-31",
            SystemCode = "GL",
            ReportType = "PREMIT"
        };

        // Act
        HttpResponseMessage generateResponse = await _client.PostAsJsonAsync("/api/v1/reports/generate", request);
        generateResponse.StatusCode.Should().Be(HttpStatusCode.Accepted);

        var result = await generateResponse.Content.ReadFromJsonAsync<ReportGenerationResponse>();
        var jobId = result!.JobId;

        // Wait for completion
        await Task.Delay(3000);

        var statusResponse = await _client.GetAsync($"/api/v1/reports/status/{jobId}");
        var statusResult = await statusResponse.Content.ReadFromJsonAsync<ReportStatusResponse>();

        // Assert: Should complete but with 0 records
        statusResult!.Status.Should().Be("Completed");
        statusResult.RecordsProcessed.Should().Be(0);
    }

    [Fact]
    public async Task ReportHistory_AfterGeneratingMultipleReports_ShouldReturnAllReports()
    {
        // Arrange: Generate 2 reports
        await LoadMockDataAsync();

        var request1 = new { StartDate = "2025-10-01", EndDate = "2025-10-15", SystemCode = "GL", ReportType = "PREMIT" };
        var request2 = new { StartDate = "2025-10-16", EndDate = "2025-10-31", SystemCode = "GL", ReportType = "PREMCED" };

        await _client.PostAsJsonAsync("/api/v1/reports/generate", request1);
        await Task.Delay(1000);
        await _client.PostAsJsonAsync("/api/v1/reports/generate", request2);
        await Task.Delay(3000); // Wait for completion

        // Act
        HttpResponseMessage historyResponse = await _client.GetAsync("/api/v1/reports/history");

        // Assert
        historyResponse.StatusCode.Should().Be(HttpStatusCode.OK);
        ReportGenerationResponse[]? history = await historyResponse.Content.ReadFromJsonAsync<ReportGenerationResponse[]>();
        history.Should().NotBeNull();
        history!.Length.Should().BeGreaterThanOrEqualTo(2);
    }

    private async Task LoadMockDataAsync()
    {
        // Load sample data for testing
        var sampleDataPath = Path.Combine(
            Directory.GetCurrentDirectory(),
            "..", "..", "..", "..",
            "SampleData"
        );

        if (Directory.Exists(sampleDataPath))
        {
            var files = new[] { "products.csv", "clients.csv", "policies.csv", "premiums.csv" };

            foreach (var file in files)
            {
                var filePath = Path.Combine(sampleDataPath, file);
                if (File.Exists(filePath))
                {
                    var content = new MultipartFormDataContent();
                    var fileContent = new ByteArrayContent(await File.ReadAllBytesAsync(filePath));
                    content.Add(fileContent, "file", file);
                    content.Add(new StringContent(Path.GetFileNameWithoutExtension(file)), "entityType");
                    content.Add(new StringContent("csv"), "format");

                    await _client.PostAsync("/api/mock-data/load", content);
                }
            }
        }
    }
}
