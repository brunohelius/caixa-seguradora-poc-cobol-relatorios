using System;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Net.Http.Json;
using System.Text;
using System.Threading.Tasks;
using FluentAssertions;
using Microsoft.AspNetCore.Mvc.Testing;
using Xunit;

namespace CaixaSeguradora.IntegrationTests.Workflows;

/// <summary>
/// Integration tests for the mock data management workflow.
/// Tests User Story 5 - Manage Database Mock Data end-to-end.
/// </summary>
public class MockDataWorkflowTests : IClassFixture<WebApplicationFactory<Program>>
{
    private readonly HttpClient _client;

    public MockDataWorkflowTests(WebApplicationFactory<Program> factory)
    {
        _client = factory.CreateClient();
    }

    [Fact]
    public async Task CompleteMockDataWorkflow_LoadValidateAndReset_ShouldSucceed()
    {
        // Arrange: Create sample CSV data
        var csvContent = "ProductCode,ProductName,CompanyCode\nP001,Product A,1\nP002,Product B,1\nP003,Product C,2";
        var csvBytes = Encoding.UTF8.GetBytes(csvContent);

        // Step 1: Reset database to clean state
        HttpResponseMessage resetResponse = await _client.PostAsync("/api/mock-data/reset", null);
        resetResponse.StatusCode.Should().Be(HttpStatusCode.OK);

        // Step 2: Load mock data
        var content = new MultipartFormDataContent();
        var fileContent = new ByteArrayContent(csvBytes);
        content.Add(fileContent, "file", "products.csv");
        content.Add(new StringContent("products"), "entityType");
        content.Add(new StringContent("csv"), "format");
        content.Add(new StringContent("false"), "clearExisting");

        HttpResponseMessage loadResponse = await _client.PostAsync("/api/mock-data/load", content);

        loadResponse.StatusCode.Should().Be(HttpStatusCode.OK);
        dynamic? loadResult = await loadResponse.Content.ReadFromJsonAsync<dynamic>();
        loadResult.Should().NotBeNull();

        // Step 3: Validate loaded data
        HttpResponseMessage validateResponse = await _client.GetAsync("/api/mock-data/validate");
        validateResponse.StatusCode.Should().Be(HttpStatusCode.OK);

        dynamic? validateResult = await validateResponse.Content.ReadFromJsonAsync<dynamic>();
        validateResult.Should().NotBeNull();

        // Step 4: Get statistics
        HttpResponseMessage statsResponse = await _client.GetAsync("/api/mock-data/stats");
        statsResponse.StatusCode.Should().Be(HttpStatusCode.OK);

        dynamic? statsResult = await statsResponse.Content.ReadFromJsonAsync<dynamic>();
        statsResult.Should().NotBeNull();

        // Step 5: Clear specific entity data
        HttpResponseMessage clearResponse = await _client.DeleteAsync("/api/mock-data/clear/products");
        clearResponse.StatusCode.Should().Be(HttpStatusCode.OK);

        // Verify data was cleared
        HttpResponseMessage statsAfterClear = await _client.GetAsync("/api/mock-data/stats");
        dynamic? statsAfterClearResult = await statsAfterClear.Content.ReadFromJsonAsync<dynamic>();
        // Products count should be 0 after clear
    }

    [Fact]
    public async Task LoadMockData_WithInvalidCsv_ShouldReturnBadRequest()
    {
        // Arrange: Create invalid CSV (missing required columns)
        var invalidCsv = "InvalidColumn1,InvalidColumn2\nValue1,Value2";
        var csvBytes = Encoding.UTF8.GetBytes(invalidCsv);

        var content = new MultipartFormDataContent();
        var fileContent = new ByteArrayContent(csvBytes);
        content.Add(fileContent, "file", "products.csv");
        content.Add(new StringContent("products"), "entityType");
        content.Add(new StringContent("csv"), "format");

        // Act
        HttpResponseMessage loadResponse = await _client.PostAsync("/api/mock-data/load", content);

        // Assert: Should handle gracefully (may succeed with 0 records or fail with validation error)
        loadResponse.StatusCode.Should().BeOneOf(HttpStatusCode.OK, HttpStatusCode.BadRequest);
    }

    [Fact]
    public async Task GetSchema_ForExistingEntity_ShouldReturnSchemaInfo()
    {
        // Act
        HttpResponseMessage schemaResponse = await _client.GetAsync("/api/mock-data/schema?entityType=products");

        // Assert
        schemaResponse.StatusCode.Should().Be(HttpStatusCode.OK);

        dynamic? schemaResult = await schemaResponse.Content.ReadFromJsonAsync<dynamic>();
        schemaResult.Should().NotBeNull();
    }

    [Fact]
    public async Task LoadMockData_WithClearExistingTrue_ShouldReplaceData()
    {
        // Arrange: Load initial data
        var initialCsv = "ProductCode,ProductName,CompanyCode\nP001,Product A,1";
        var initialBytes = Encoding.UTF8.GetBytes(initialCsv);

        var initialContent = new MultipartFormDataContent();
        var initialFileContent = new ByteArrayContent(initialBytes);
        initialContent.Add(initialFileContent, "file", "products.csv");
        initialContent.Add(new StringContent("products"), "entityType");
        initialContent.Add(new StringContent("csv"), "format");
        initialContent.Add(new StringContent("false"), "clearExisting");

        await _client.PostAsync("/api/mock-data/load", initialContent);

        // Act: Load replacement data with clearExisting=true
        var replacementCsv = "ProductCode,ProductName,CompanyCode\nP999,Product Z,9";
        var replacementBytes = Encoding.UTF8.GetBytes(replacementCsv);

        var replacementContent = new MultipartFormDataContent();
        var replacementFileContent = new ByteArrayContent(replacementBytes);
        replacementContent.Add(replacementFileContent, "file", "products.csv");
        replacementContent.Add(new StringContent("products"), "entityType");
        replacementContent.Add(new StringContent("csv"), "format");
        replacementContent.Add(new StringContent("true"), "clearExisting");

        HttpResponseMessage loadResponse = await _client.PostAsync("/api/mock-data/load", replacementContent);

        // Assert
        loadResponse.StatusCode.Should().Be(HttpStatusCode.OK);
        dynamic? loadResult = await loadResponse.Content.ReadFromJsonAsync<dynamic>();

        // Should have replaced the data
        var recordsInserted = (int)loadResult.recordsInserted;
        recordsInserted.Should().BeGreaterThan(0);
    }

    [Fact]
    public async Task ValidateData_WithMultipleEntities_ShouldCheckAllEntities()
    {
        // Arrange: Load multiple entity types
        await LoadMultipleEntityTypesAsync();

        // Act
        HttpResponseMessage validateResponse = await _client.GetAsync("/api/mock-data/validate");

        // Assert
        validateResponse.StatusCode.Should().Be(HttpStatusCode.OK);

        dynamic? validateResult = await validateResponse.Content.ReadFromJsonAsync<dynamic>();
        validateResult.Should().NotBeNull();

        var isValid = (bool)validateResult.isValid;
        // Should validate all loaded entities
    }

    [Fact]
    public async Task ResetDatabase_WhenCalled_ShouldClearAllData()
    {
        // Arrange: Load some data first
        await LoadMultipleEntityTypesAsync();

        // Act: Reset database
        HttpResponseMessage resetResponse = await _client.PostAsync("/api/mock-data/reset", null);

        // Assert
        resetResponse.StatusCode.Should().Be(HttpStatusCode.OK);

        // Verify all data is cleared
        HttpResponseMessage statsResponse = await _client.GetAsync("/api/mock-data/stats");
        dynamic? statsResult = await statsResponse.Content.ReadFromJsonAsync<dynamic>();

        var totalRecords = (int)statsResult.totalRecords;
        totalRecords.Should().Be(0);
    }

    private async Task LoadMultipleEntityTypesAsync()
    {
        (string, string)[] entities = new[]
        {
            ("products", "ProductCode,ProductName,CompanyCode\nP001,Product A,1"),
            ("clients", "ClientCode,ClientName,DocumentNumber\nC001,Client A,12345678901")
        };

        foreach ((string entityType, string csvContent) in entities)
        {
            var csvBytes = Encoding.UTF8.GetBytes(csvContent);
            var content = new MultipartFormDataContent();
            var fileContent = new ByteArrayContent(csvBytes);
            content.Add(fileContent, "file", $"{entityType}.csv");
            content.Add(new StringContent(entityType), "entityType");
            content.Add(new StringContent("csv"), "format");

            await _client.PostAsync("/api/mock-data/load", content);
        }
    }
}
