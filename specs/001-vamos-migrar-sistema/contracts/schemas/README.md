# API Contract Schemas Documentation

**Feature Branch**: `001-vamos-migrar-sistema`
**Created**: October 22, 2025
**Status**: Phase 1.2 - API Contracts
**OpenAPI Version**: 3.0.3

## Overview

This directory contains the OpenAPI 3.0.3 specification for the SUSEP Circular 360 Premium Reporting API. The API provides modern RESTful access to functionality previously implemented in the legacy COBOL RG1866B batch program.

## Files

- **`openapi.yaml`**: Complete OpenAPI specification with all endpoints, schemas, and examples

## API Structure

### Base URL

```
Development: https://localhost:5001/api/v1
Staging:     https://dev-premiums.caixaseguradora.com.br/api/v1
Production:  https://premiums.caixaseguradora.com.br/api/v1
```

### Endpoint Categories

| Tag | Endpoints | Purpose |
|-----|-----------|---------|
| **Reports** | 5 endpoints | Premium report generation and comparison |
| **Premiums** | 3 endpoints | Premium record querying and statistics |
| **Policies** | 4 endpoints | Policy information and related data |
| **Products** | 2 endpoints | Insurance product catalog |
| **Clients** | 2 endpoints | Client and address information |
| **Batch Jobs** | 4 endpoints | Scheduled report generation |
| **Mock Data** | 3 endpoints | Test data management |
| **Dashboard** | 3 endpoints | Migration metrics and analytics |
| **System** | 2 endpoints | Health and configuration |

**Total**: 28 RESTful endpoints

## Key Design Principles

### 1. Technology-Agnostic Specification

The OpenAPI specification focuses on **what** the API does, not **how** it's implemented:

- No mention of .NET 9, C#, or Entity Framework in endpoint descriptions
- Business-focused terminology (premiums, policies, endorsements)
- Technology details are implementation concerns, not contract concerns

### 2. Regulatory Compliance Focus

- **Decimal Precision**: All financial amounts use `number` type with `format: double` to preserve precision
- **SUSEP Terminology**: Uses Brazilian insurance industry terms (ramo, cosseguro, tomador)
- **Byte-Level Validation**: Includes `/reports/compare` endpoint for COBOL vs .NET output comparison

### 3. RESTful Best Practices

- **Resource-Oriented URLs**: `/policies/{policyNumber}`, `/premiums/{premiumId}`
- **HTTP Verbs**: GET (read), POST (create/execute), DELETE (remove)
- **Stateless Operations**: Each request contains all necessary information
- **Pagination**: Consistent pagination parameters across list endpoints
- **HTTP Status Codes**: Standard codes (200, 201, 400, 404, 500, 503)

### 4. Asynchronous Processing

Long-running operations (report generation) use async pattern:

1. POST `/reports/generate` returns `202 Accepted` with `reportId`
2. GET `/reports/{reportId}` polls for status
3. GET `/reports/{reportId}/download/{fileType}` downloads completed file

### 5. Error Handling

Consistent error response structure:

```json
{
  "error": "Human-readable error message",
  "details": [
    {
      "code": "VALIDATION_ERROR",
      "message": "endDate must be >= startDate",
      "field": "endDate",
      "timestamp": "2025-10-22T14:30:00Z"
    }
  ]
}
```

## Endpoint Details

### Reports Endpoints

#### POST `/reports/generate`

**Purpose**: Generate PREMIT.TXT and/or PREMCED.TXT reports interactively

**COBOL Equivalent**: Batch execution of RG1866B with JCL parameters

**Request**:
```json
{
  "startDate": "2025-10-01",
  "endDate": "2025-10-31",
  "reportTypes": ["PREMIT", "PREMCED"],
  "systemId": "GL",
  "processingMode": "MONTHLY"
}
```

**Response** (202 Accepted):
```json
{
  "reportId": "550e8400-e29b-41d4-a716-446655440000",
  "status": "ACCEPTED",
  "estimatedCompletionTime": "2025-10-22T14:35:00Z",
  "message": "Report generation accepted. Processing 15,432 premium records."
}
```

**Business Logic Mapping**:
- Executes COBOL sections R0500-R5500 logic
- Processes V0PREMIOS cursor (R0500-00-DECLARE-V0PREMIOS)
- Applies business rules from R0700-00-PROCESSA-REGISTRO
- Generates fixed-width output matching COBOL WRITE statements

#### GET `/reports/{reportId}`

**Purpose**: Poll report generation status

**Response** (200 OK):
```json
{
  "reportId": "550e8400-e29b-41d4-a716-446655440000",
  "status": "COMPLETED",
  "progress": {
    "currentRecord": 15432,
    "totalRecords": 15432,
    "percentComplete": 100.0
  },
  "executionTime": "2m 34s",
  "files": [
    {
      "fileType": "PREMIT",
      "fileName": "PREMIT_20251022.TXT",
      "sizeBytes": 11813280,
      "downloadUrl": "/api/v1/reports/550e8400.../download/PREMIT"
    }
  ],
  "summary": {
    "recordsProcessed": 15432,
    "premiumEmissions": 14021,
    "premiumCancellations": 1411,
    "totalPremiumAmount": 156789432.50
  }
}
```

#### POST `/reports/compare`

**Purpose**: Validate .NET output against COBOL output

**Critical for Migration**: Ensures byte-for-byte compatibility for regulatory compliance

**Request**:
```json
{
  "cobolFilePath": "/legacy/reports/PREMIT_20251022.TXT",
  "dotnetReportId": "550e8400-e29b-41d4-a716-446655440000",
  "fileType": "PREMIT"
}
```

**Response** (200 OK):
```json
{
  "comparisonId": "comparison-123",
  "filesMatch": true,
  "differences": [],
  "summary": {
    "totalBytes": 11813280,
    "matchingBytes": 11813280,
    "differenceCount": 0,
    "matchPercentage": 100.0
  }
}
```

### Premiums Endpoints

#### GET `/premiums`

**Purpose**: Query premium records with flexible filtering

**COBOL Equivalent**: V0PREMIOS view access with WHERE clause

**Query Parameters**:
- `policyNumber`: Filter by policy
- `startDate`, `endDate`: Date range filter
- `productCode`, `lineOfBusiness`: Product filters
- `movementType`: E=Emission, C=Cancellation, etc.
- `page`, `pageSize`: Pagination
- `sortBy`, `sortOrder`: Sorting

**Response** (200 OK):
```json
{
  "premiums": [
    {
      "premiumId": 12345,
      "policyNumber": 9876543210123,
      "referenceYear": 2025,
      "referenceMonth": 10,
      "referenceDay": 15,
      "movementType": "E",
      "lineOfBusiness": 118,
      "totalPremiumTotal": 1250.50,
      "netPremiumTotal": 1125.45,
      "commissionTotal": 125.05
    }
  ],
  "pagination": {
    "currentPage": 1,
    "pageSize": 20,
    "totalPages": 772,
    "totalRecords": 15432
  },
  "aggregations": {
    "totalPremiumAmount": 156789432.50,
    "averagePremiumAmount": 10165.23,
    "recordCount": 15432
  }
}
```

#### GET `/premiums/statistics`

**Purpose**: Aggregated analytics for dashboards

**Example Request**:
```
GET /premiums/statistics?startDate=2025-10-01&endDate=2025-10-31&groupBy=lineOfBusiness
```

**Response** (200 OK):
```json
{
  "groupBy": "lineOfBusiness",
  "statistics": [
    {
      "groupKey": "118",
      "groupLabel": "Auto",
      "recordCount": 8521,
      "totalPremium": 89452123.50,
      "averagePremium": 10498.32,
      "minPremium": 125.00,
      "maxPremium": 45000.00
    },
    {
      "groupKey": "969",
      "groupLabel": "Vida Individual",
      "recordCount": 4211,
      "totalPremium": 42337309.00,
      "averagePremium": 10052.89,
      "minPremium": 50.00,
      "maxPremium": 25000.00
    }
  ]
}
```

### Batch Jobs Endpoints

#### POST `/batch-jobs`

**Purpose**: Schedule recurring report generation

**Use Case**: Automate monthly SUSEP report generation

**Request**:
```json
{
  "jobName": "Monthly SUSEP Reports",
  "description": "Automated monthly premium reports for SUSEP compliance",
  "recurrencePattern": "MONTHLY",
  "reportParameters": {
    "startDate": "2025-10-01",
    "endDate": "2025-10-31",
    "reportTypes": ["PREMIT", "PREMCED"],
    "systemId": "GL",
    "processingMode": "MONTHLY"
  },
  "notificationRecipients": [
    "compliance@caixaseguradora.com.br",
    "finance@caixaseguradora.com.br"
  ]
}
```

**Response** (201 Created):
```json
{
  "jobId": "job-550e8400-e29b-41d4-a716-446655440000",
  "jobName": "Monthly SUSEP Reports",
  "status": "SCHEDULED",
  "nextExecutionTime": "2025-11-01T02:00:00Z"
}
```

### Dashboard Endpoints

#### GET `/dashboard/metrics`

**Purpose**: Provide migration analysis metrics for dashboard visualization

**Response** (200 OK):
```json
{
  "programInfo": {
    "programName": "RG1866B",
    "linesOfCode": 5000,
    "creationDate": "2014-05-21",
    "lastModifiedDate": "2022-09-21"
  },
  "dataStructure": {
    "totalDataItems": 687,
    "level01Structures": 7,
    "level77Variables": 390,
    "tablesAccessed": 26
  },
  "processingComplexity": {
    "totalSections": 63,
    "totalParagraphs": 65,
    "cursorCount": 4,
    "externalModuleCalls": 3
  },
  "migrationProgress": {
    "specificationComplete": true,
    "dataModelComplete": true,
    "apiContractsComplete": true,
    "implementationProgress": 15.5
  }
}
```

## Schema Definitions

### Core Financial Data Types

All financial amounts use `number` type with `format: double` to preserve decimal precision:

```yaml
totalPremiumAmount:
  type: number
  format: double
  description: Total premium amount with exact decimal precision
```

**Rationale**: JSON doesn't have native decimal type, but consumers must parse as decimal (not float) to match COBOL arithmetic.

### Date Formats

All dates use ISO 8601 format (`YYYY-MM-DD`):

```yaml
effectiveDate:
  type: string
  format: date
  example: "2025-10-22"
```

**COBOL Mapping**: COBOL stores dates as `PIC X(10)` in format `YYYY-MM-DD`, parsed with `DateTime.ParseExact()` in C#.

### Enumeration Types

Enumerations use COBOL single-character codes:

```yaml
movementType:
  type: string
  enum: [E, C, R, S, A]
  description: E=Emission, C=Cancellation, R=Reversal, S=Supplement, A=Adjustment
```

**Why Single Characters**: Matches COBOL `PIC X(1)` fields, preserves legacy data semantics.

## Pagination Standard

All list endpoints support consistent pagination:

**Query Parameters**:
- `page`: Page number (1-indexed), default 1
- `pageSize`: Items per page (1-100), default 20

**Response Structure**:
```json
{
  "items": [...],
  "pagination": {
    "currentPage": 1,
    "pageSize": 20,
    "totalPages": 50,
    "totalRecords": 987,
    "hasNextPage": true,
    "hasPreviousPage": false
  }
}
```

## Security

### Authentication

All endpoints require JWT bearer token authentication:

```http
Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...
```

**Exception**: `/system/health` endpoint is unauthenticated for monitoring.

### Authorization

Role-based access control (RBAC) enforced on sensitive operations:

- **Read Operations** (`GET`): Requires `premium:read` permission
- **Report Generation** (`POST /reports/generate`): Requires `premium:generate` permission
- **Mock Data Management**: Requires `admin` role
- **Batch Job Management**: Requires `batch:admin` permission

## Error Handling

### Standard Error Response

```json
{
  "error": "Human-readable error message",
  "details": [
    {
      "code": "ERROR_CODE",
      "message": "Detailed error description",
      "field": "fieldName",
      "timestamp": "2025-10-22T14:30:00Z"
    }
  ]
}
```

### HTTP Status Codes

| Code | Meaning | Usage |
|------|---------|-------|
| 200 | OK | Successful GET request |
| 201 | Created | Successful POST creating resource |
| 202 | Accepted | Async operation accepted |
| 204 | No Content | Successful DELETE |
| 400 | Bad Request | Validation error |
| 404 | Not Found | Resource doesn't exist |
| 500 | Internal Server Error | Unexpected server error |
| 503 | Service Unavailable | System unhealthy |

### Common Error Codes

- `VALIDATION_ERROR`: Request validation failed
- `RESOURCE_NOT_FOUND`: Requested resource doesn't exist
- `DATE_RANGE_INVALID`: End date before start date
- `PROCESSING_ERROR`: Report generation failed
- `COMPARISON_MISMATCH`: COBOL vs .NET output differs

## Testing Endpoints

### Mock Data Management

For development and testing:

1. **Load Test Data**: `POST /mock-data/load`
   - Upload CSV/JSON files
   - Validates schema compatibility
   - Returns load summary

2. **Validate Data**: `POST /mock-data/validate`
   - Checks foreign key integrity
   - Validates data types
   - Reports violations

3. **Reset Database**: `POST /mock-data/reset`
   - Clears all test data
   - Resets to clean slate

## Usage Examples

### Generate Monthly Report

```bash
# 1. Request report generation
curl -X POST https://localhost:5001/api/v1/reports/generate \
  -H "Authorization: Bearer $TOKEN" \
  -H "Content-Type: application/json" \
  -d '{
    "startDate": "2025-10-01",
    "endDate": "2025-10-31",
    "reportTypes": ["PREMIT", "PREMCED"],
    "systemId": "GL",
    "processingMode": "MONTHLY"
  }'

# Response: {"reportId": "550e8400-...", "status": "ACCEPTED"}

# 2. Poll for completion
curl https://localhost:5001/api/v1/reports/550e8400-.../

# 3. Download file
curl https://localhost:5001/api/v1/reports/550e8400-.../download/PREMIT \
  -o PREMIT_20251031.TXT
```

### Query Premiums by Policy

```bash
curl "https://localhost:5001/api/v1/premiums?policyNumber=9876543210123&page=1&pageSize=50" \
  -H "Authorization: Bearer $TOKEN"
```

### Get Dashboard Metrics

```bash
curl https://localhost:5001/api/v1/dashboard/metrics \
  -H "Authorization: Bearer $TOKEN"
```

## OpenAPI Code Generation

### Client SDKs

Generate type-safe client SDKs from OpenAPI spec:

```bash
# TypeScript/JavaScript client
openapi-generator-cli generate \
  -i openapi.yaml \
  -g typescript-axios \
  -o ../frontend/src/api-client

# C# client (for testing)
openapi-generator-cli generate \
  -i openapi.yaml \
  -g csharp-netcore \
  -o ../tests/CaixaSeguradora.IntegrationTests/ApiClient
```

### Server Stubs

Generate ASP.NET Core controllers (optional starting point):

```bash
openapi-generator-cli generate \
  -i openapi.yaml \
  -g aspnetcore \
  -o ../backend/src/CaixaSeguradora.Api.Generated
```

**Note**: Generated code is starting point only. Implement actual business logic in separate service layer.

## Versioning

API uses URL versioning: `/api/v1/`

**Breaking Changes** (require new version):
- Removing endpoints
- Removing required fields
- Changing field types
- Changing authentication

**Non-Breaking Changes** (same version):
- Adding new endpoints
- Adding optional fields
- Adding new enum values

## Next Steps

With API contracts complete, proceed to:

1. **Phase 1.3**: Generate `quickstart.md` developer onboarding guide
2. **Phase 1.4**: Update agent context with planning artifacts
3. **Phase 2**: Execute `/speckit.tasks` to generate implementation task breakdown

---

**Document Version**: 1.0
**Status**: ✅ Complete - Ready for Phase 1.3 (Quickstart Guide)
**Created**: October 22, 2025
