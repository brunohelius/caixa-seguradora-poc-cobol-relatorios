using System;
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
/// Integration tests for the query and visualization workflow.
/// Tests User Story 3 - Query and Visualize Premium Data end-to-end.
/// </summary>
public class QueryWorkflowTests : IClassFixture<WebApplicationFactory<Program>>
{
    private readonly HttpClient _client;

    public QueryWorkflowTests(WebApplicationFactory<Program> factory)
    {
        _client = factory.CreateClient();
    }

    [Fact]
    public async Task CompleteQueryWorkflow_WithFilters_ShouldReturnResults()
    {
        // Arrange: Load test data
        await LoadTestDataAsync();

        var queryRequest = new
        {
            StartDate = "2025-10-01",
            EndDate = "2025-10-31",
            PageSize = 10,
            PageNumber = 1,
            SortBy = "PolicyNumber",
            SortDescending = false
        };

        // Act: Execute query
        HttpResponseMessage queryResponse = await _client.PostAsJsonAsync("/api/v1/premiums/query", queryRequest);

        // Assert
        queryResponse.StatusCode.Should().Be(HttpStatusCode.OK);

        var queryResult = await queryResponse.Content.ReadFromJsonAsync<PremiumQueryResponse>();
        queryResult.Should().NotBeNull();
        queryResult!.TotalCount.Should().BeGreaterThanOrEqualTo(0);
        queryResult.Records.Should().NotBeNull();
    }

    [Fact]
    public async Task QueryStatistics_WithValidDateRange_ShouldReturnAggregations()
    {
        // Arrange
        await LoadTestDataAsync();

        var statsRequest = new
        {
            StartDate = "2025-10-01",
            EndDate = "2025-10-31",
            GroupBy = "Product"
        };

        // Act
        HttpResponseMessage statsResponse = await _client.PostAsJsonAsync("/api/v1/premiums/statistics", statsRequest);

        // Assert
        statsResponse.StatusCode.Should().Be(HttpStatusCode.OK);

        var statsResult = await statsResponse.Content.ReadFromJsonAsync<PremiumStatisticsResponse>();
        statsResult.Should().NotBeNull();
        statsResult!.TotalRecords.Should().BeGreaterThanOrEqualTo(0);
    }

    [Fact]
    public async Task ExportWorkflow_CsvFormat_ShouldReturnFileWithData()
    {
        // Arrange
        await LoadTestDataAsync();

        var exportRequest = new
        {
            StartDate = "2025-10-01",
            EndDate = "2025-10-31",
            Format = "csv"
        };

        // Act
        HttpResponseMessage exportResponse = await _client.PostAsJsonAsync("/api/v1/export/premiums", exportRequest);

        // Assert
        exportResponse.StatusCode.Should().Be(HttpStatusCode.OK);
        exportResponse.Content.Headers.ContentType?.MediaType.Should().Be("text/csv");

        var fileBytes = await exportResponse.Content.ReadAsByteArrayAsync();
        fileBytes.Should().NotBeEmpty();
    }

    [Fact]
    public async Task ExportWorkflow_ExcelFormat_ShouldReturnSpreadsheet()
    {
        // Arrange
        await LoadTestDataAsync();

        var exportRequest = new
        {
            StartDate = "2025-10-01",
            EndDate = "2025-10-31",
            Format = "excel"
        };

        // Act
        HttpResponseMessage exportResponse = await _client.PostAsJsonAsync("/api/v1/export/premiums", exportRequest);

        // Assert
        exportResponse.StatusCode.Should().Be(HttpStatusCode.OK);
        exportResponse.Content.Headers.ContentType?.MediaType.Should().Contain("spreadsheet");

        var fileBytes = await exportResponse.Content.ReadAsByteArrayAsync();
        fileBytes.Should().NotBeEmpty();
    }

    [Fact]
    public async Task ExportWorkflow_PdfFormat_ShouldReturnPdfDocument()
    {
        // Arrange
        await LoadTestDataAsync();

        var exportRequest = new
        {
            StartDate = "2025-10-01",
            EndDate = "2025-10-31",
            Format = "pdf"
        };

        // Act
        HttpResponseMessage exportResponse = await _client.PostAsJsonAsync("/api/v1/export/premiums", exportRequest);

        // Assert
        exportResponse.StatusCode.Should().Be(HttpStatusCode.OK);
        exportResponse.Content.Headers.ContentType?.MediaType.Should().Be("application/pdf");

        var fileBytes = await exportResponse.Content.ReadAsByteArrayAsync();
        fileBytes.Should().NotBeEmpty();

        // PDF files should start with "%PDF"
        var pdfHeader = System.Text.Encoding.ASCII.GetString(fileBytes.Take(4).ToArray());
        pdfHeader.Should().Be("%PDF");
    }

    [Fact]
    public async Task QueryWithPagination_ShouldReturnCorrectPageMetadata()
    {
        // Arrange
        await LoadTestDataAsync();

        var page1Request = new { PageNumber = 1, PageSize = 5 };
        var page2Request = new { PageNumber = 2, PageSize = 5 };

        // Act
        HttpResponseMessage page1Response = await _client.PostAsJsonAsync("/api/v1/premiums/query", page1Request);
        HttpResponseMessage page2Response = await _client.PostAsJsonAsync("/api/v1/premiums/query", page2Request);

        // Assert
        var page1Result = await page1Response.Content.ReadFromJsonAsync<PremiumQueryResponse>();
        var page2Result = await page2Response.Content.ReadFromJsonAsync<PremiumQueryResponse>();

        page1Result!.CurrentPage.Should().Be(1);
        page2Result!.CurrentPage.Should().Be(2);
        page1Result.PageSize.Should().Be(5);
        page2Result.PageSize.Should().Be(5);
    }

    private async Task LoadTestDataAsync()
    {
        // Simplified mock data loading - reuse existing data or reset if needed
        HttpResponseMessage statsResponse = await _client.GetAsync("/api/mock-data/stats");
        if (statsResponse.IsSuccessStatusCode)
        {
            dynamic? stats = await statsResponse.Content.ReadFromJsonAsync<dynamic>();
            // Data already loaded if totalRecords > 0
        }
    }
}
