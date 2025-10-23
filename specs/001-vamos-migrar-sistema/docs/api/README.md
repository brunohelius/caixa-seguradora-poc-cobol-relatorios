# API Documentation - Sistema de Relatórios SUSEP

**Version**: 1.0
**Generated**: October 2025
**Base URL (Development)**: `https://localhost:5001/api`
**Base URL (Production)**: `https://api.caixaseguradora.com.br/v1`

## Overview

Esta API RESTful fornece acesso programático ao Sistema de Relatórios de Prêmios SUSEP, oferecendo endpoints para geração de relatórios, consultas de dados, gerenciamento de jobs em lote e administração de dados de teste.

## Authentication

A API utiliza JWT Bearer tokens para autenticação:

```http
Authorization: Bearer <token>
```

**Obter Token:**
```bash
curl -X POST https://localhost:5001/api/auth/login \
  -H "Content-Type: application/json" \
  -d '{"username": "usuario@caixa.com.br", "password": "senha"}'
```

## Rate Limiting

- **Limite**: 100 requisições por minuto por IP/token
- **Headers de Resposta**:
  - `X-RateLimit-Limit`: Limite total
  - `X-RateLimit-Remaining`: Requisições restantes
  - `X-RateLimit-Reset`: Timestamp de reset

## Error Handling

Todas as respostas de erro seguem o formato padrão:

```json
{
  "error": {
    "code": "VALIDATION_ERROR",
    "message": "Descrição do erro em português",
    "details": ["Campo X é obrigatório", "Valor Y inválido"],
    "timestamp": "2025-10-22T10:30:00Z",
    "traceId": "80000000-0000-0000-0000-000000000000"
  }
}
```

**Códigos de Status HTTP:**
- `200 OK`: Sucesso
- `201 Created`: Recurso criado
- `202 Accepted`: Operação assíncrona aceita
- `400 Bad Request`: Erro de validação
- `401 Unauthorized`: Não autenticado
- `403 Forbidden`: Sem permissão
- `404 Not Found`: Recurso não encontrado
- `409 Conflict`: Conflito de estado
- `429 Too Many Requests`: Rate limit excedido
- `500 Internal Server Error`: Erro interno
- `503 Service Unavailable`: Serviço temporariamente indisponível

---

## Endpoints - Dashboard

### GET /api/dashboard/metrics

Retorna métricas gerais do sistema e progresso da migração.

**Response:**
```json
{
  "programInfo": {
    "name": "RG1866B",
    "description": "Sistema de Relatórios SUSEP Circular 360",
    "totalDataItems": 687,
    "totalSections": 63,
    "totalParagraphs": 65
  },
  "dataStructure": {
    "totalTables": 26,
    "totalViews": 24,
    "totalCursors": 4
  },
  "complexityMetrics": {
    "cyclomaticComplexity": 145,
    "functionPoints": 380,
    "estimatedLinesOfCode": 5234
  },
  "migrationProgress": {
    "tasksCompleted": 244,
    "tasksTotal": 244,
    "percentageComplete": 100,
    "lastUpdated": "2025-10-22T22:55:00Z"
  }
}
```

### GET /api/dashboard/function-points

Retorna breakdown detalhado dos pontos de função.

**Response:**
```json
{
  "totalFunctionPoints": 380,
  "breakdown": {
    "externalInputs": 45,
    "externalOutputs": 89,
    "externalInquiries": 32,
    "internalLogicalFiles": 15,
    "externalInterfaceFiles": 11
  }
}
```

### GET /api/dashboard/database-dependencies

Retorna mapeamento de dependências de banco de dados.

**Response:**
```json
{
  "tables": [
    {
      "tableName": "V0PREMIOS",
      "type": "VIEW",
      "recordCount": 15234,
      "dependencies": ["V0APOLICE", "V0PRODUTO"]
    }
  ],
  "totalRecords": 156789
}
```

---

## Endpoints - Report Generation

### POST /api/reports/generate

Inicia geração assíncrona de relatório PREMIT ou PREMCED.

**Request Body:**
```json
{
  "systemId": "GL",
  "startDate": "2025-10-01",
  "endDate": "2025-10-31",
  "reportType": "PREMIT",
  "mode": "FULL"
}
```

**Response:** (HTTP 202 Accepted)
```json
{
  "jobId": "550e8400-e29b-41d4-a716-446655440000",
  "status": "QUEUED",
  "message": "Geração de relatório iniciada",
  "estimatedDuration": "PT5M"
}
```

### GET /api/reports/{jobId}/status

Consulta status de geração de relatório.

**Response:**
```json
{
  "jobId": "550e8400-e29b-41d4-a716-446655440000",
  "status": "PROCESSING",
  "progress": 45,
  "startedAt": "2025-10-22T10:00:00Z",
  "estimatedCompletion": "2025-10-22T10:05:00Z",
  "currentPhase": "Processando prêmios"
}
```

**Status Values:**
- `QUEUED`: Na fila
- `PROCESSING`: Em processamento
- `COMPLETED`: Concluído
- `FAILED`: Falhou
- `CANCELLED`: Cancelado

### GET /api/reports/{jobId}/download

Baixa arquivo de relatório gerado.

**Response:** (HTTP 200 OK)
- Content-Type: `application/octet-stream`
- Content-Disposition: `attachment; filename="PREMIT_GL_202510.TXT"`

### GET /api/reports/history

Lista histórico de relatórios gerados.

**Query Parameters:**
- `page` (int, default: 1): Página de resultados
- `pageSize` (int, default: 20, max: 100): Itens por página
- `systemId` (string, optional): Filtrar por sistema
- `reportType` (string, optional): Filtrar por tipo de relatório
- `startDate` (date, optional): Data inicial
- `endDate` (date, optional): Data final

**Response:**
```json
{
  "items": [
    {
      "jobId": "550e8400-e29b-41d4-a716-446655440000",
      "systemId": "GL",
      "reportType": "PREMIT",
      "status": "COMPLETED",
      "generatedAt": "2025-10-22T10:05:00Z",
      "recordsProcessed": 12345,
      "fileSize": 2458123
    }
  ],
  "totalCount": 156,
  "page": 1,
  "pageSize": 20,
  "totalPages": 8
}
```

### POST /api/reports/compare

Compara saída COBOL vs .NET byte-a-byte.

**Request:** (multipart/form-data)
- `cobolFile`: Arquivo COBOL
- `dotnetFile`: Arquivo .NET

**Response:**
```json
{
  "isMatch": true,
  "comparisonId": "abc123",
  "bytesDifferent": 0,
  "totalBytes": 2458123,
  "matchPercentage": 100.0,
  "differences": []
}
```

---

## Endpoints - Data Query

### POST /api/premiums/query

Executa consulta ad-hoc em dados de prêmios.

**Request Body:**
```json
{
  "filters": {
    "policyNumberRange": {"min": 1000000, "max": 2000000},
    "dateRange": {"start": "2025-01-01", "end": "2025-12-31"},
    "productCodes": [1001, 1002],
    "movementTypes": ["E", "C"]
  },
  "columns": ["policyNumber", "premiumAmount", "clientName"],
  "sorting": {"field": "policyNumber", "direction": "ASC"},
  "pagination": {"page": 1, "pageSize": 50}
}
```

**Response:**
```json
{
  "data": [
    {
      "policyNumber": 1234567890123,
      "premiumAmount": 1500.50,
      "clientName": "João da Silva"
    }
  ],
  "summary": {
    "totalRecords": 1234,
    "filteredRecords": 456,
    "totalPremium": 685000.00
  },
  "pagination": {
    "page": 1,
    "pageSize": 50,
    "totalPages": 10
  }
}
```

### POST /api/premiums/statistics

Obtém estatísticas agregadas.

**Request Body:**
```json
{
  "filters": {"dateRange": {"start": "2025-01-01", "end": "2025-12-31"}},
  "aggregations": ["SUM", "AVG", "COUNT"],
  "groupBy": ["productCode", "month"]
}
```

**Response:**
```json
{
  "aggregations": {
    "totalPremium": 12500000.00,
    "averagePremium": 2500.50,
    "count": 5000
  },
  "groups": [
    {
      "productCode": 1001,
      "month": "2025-01",
      "totalPremium": 125000.00,
      "averagePremium": 2500.00,
      "count": 50
    }
  ]
}
```

---

## Endpoints - Export

### POST /api/export/csv

Exporta dados para CSV.

**Request Body:**
```json
{
  "queryId": "550e8400-e29b-41d4-a716-446655440000",
  "columns": ["policyNumber", "premiumAmount"],
  "delimiter": ",",
  "includeHeaders": true
}
```

**Response:** (HTTP 200 OK)
- Content-Type: `text/csv`
- Content-Disposition: `attachment; filename="export_20251022.csv"`

### POST /api/export/excel

Exporta dados para Excel.

**Response:** (HTTP 200 OK)
- Content-Type: `application/vnd.openxmlformats-officedocument.spreadsheetml.sheet`
- Content-Disposition: `attachment; filename="export_20251022.xlsx"`

### POST /api/export/pdf

Exporta dados para PDF.

**Response:** (HTTP 200 OK)
- Content-Type: `application/pdf`
- Content-Disposition: `attachment; filename="export_20251022.pdf"`

---

## Endpoints - Batch Jobs

### POST /api/jobs

Cria job agendado.

**Request Body:**
```json
{
  "name": "Relatório Mensal GL",
  "description": "Gera PREMIT mensalmente",
  "schedule": "0 2 1 * *",
  "reportConfig": {
    "systemId": "GL",
    "reportType": "PREMIT",
    "mode": "FULL"
  },
  "notifications": {
    "emails": ["relatorios@caixa.com.br"],
    "onSuccess": true,
    "onFailure": true
  }
}
```

**Response:** (HTTP 201 Created)
```json
{
  "jobId": "550e8400-e29b-41d4-a716-446655440000",
  "name": "Relatório Mensal GL",
  "schedule": "0 2 1 * *",
  "nextRun": "2025-11-01T02:00:00Z",
  "status": "ACTIVE"
}
```

### GET /api/jobs

Lista jobs agendados.

**Response:**
```json
{
  "items": [
    {
      "jobId": "550e8400-e29b-41d4-a716-446655440000",
      "name": "Relatório Mensal GL",
      "schedule": "0 2 1 * *",
      "lastRun": "2025-10-01T02:00:00Z",
      "nextRun": "2025-11-01T02:00:00Z",
      "status": "ACTIVE"
    }
  ],
  "totalCount": 5
}
```

### PUT /api/jobs/{jobId}

Atualiza job agendado.

### DELETE /api/jobs/{jobId}

Remove job agendado.

### GET /api/jobs/{jobId}/history

Obtém histórico de execuções do job.

**Response:**
```json
{
  "items": [
    {
      "executionId": "abc123",
      "startedAt": "2025-10-01T02:00:00Z",
      "completedAt": "2025-10-01T02:05:30Z",
      "status": "SUCCEEDED",
      "recordsProcessed": 12345,
      "duration": "PT5M30S"
    }
  ]
}
```

---

## Endpoints - Mock Data Management

### GET /api/data/schema

Obtém informações de schema do banco de dados.

**Response:**
```json
{
  "tables": [
    {
      "tableName": "PremiumRecords",
      "recordCount": 15234,
      "columns": [
        {
          "columnName": "PremiumId",
          "dataType": "INTEGER",
          "isNullable": false,
          "isPrimaryKey": true
        }
      ]
    }
  ]
}
```

### POST /api/data/load

Carrega dados mock de arquivo CSV/JSON.

**Request:** (multipart/form-data)
- `file`: Arquivo CSV/JSON
- `entityType`: Tipo de entidade (e.g., "Premium", "Policy")
- `replaceExisting`: Boolean (default: false)

**Response:**
```json
{
  "success": true,
  "recordsLoaded": 1234,
  "errors": [],
  "warnings": ["Registro 45: campo opcional vazio"]
}
```

### POST /api/data/validate

Valida integridade dos dados.

**Response:**
```json
{
  "isValid": true,
  "totalRecords": 15234,
  "validRecords": 15230,
  "invalidRecords": 4,
  "validationErrors": [
    {
      "entityType": "Policy",
      "recordId": "123",
      "field": "ClientCode",
      "errorMessage": "Chave estrangeira inválida: cliente não existe"
    }
  ]
}
```

### DELETE /api/data/reset

Limpa todos os dados do banco de dados.

**Response:**
```json
{
  "success": true,
  "message": "Banco de dados resetado com sucesso",
  "tablesCleared": 15
}
```

### POST /api/data/comparison

Compara outputs COBOL vs .NET.

**Request:** (multipart/form-data)
- `cobolFile`: Arquivo COBOL
- `dotnetFile`: Arquivo .NET

**Response:**
```json
{
  "totalRecords": 1234,
  "matchingRecords": 1234,
  "differingRecords": 0,
  "differences": []
}
```

---

## WebSocket - Real-Time Updates

### Connection

```javascript
const ws = new WebSocket('wss://localhost:5001/ws/reports');

ws.onmessage = (event) => {
  const update = JSON.parse(event.data);
  console.log('Progress:', update.progress);
};
```

### Message Format

```json
{
  "jobId": "550e8400-e29b-41d4-a716-446655440000",
  "type": "PROGRESS_UPDATE",
  "progress": 67,
  "currentPhase": "Processando endossos",
  "timestamp": "2025-10-22T10:03:00Z"
}
```

---

## SDK Examples

### C# (.NET)

```csharp
var client = new HttpClient { BaseAddress = new Uri("https://localhost:5001") };
client.DefaultRequestHeaders.Authorization =
    new AuthenticationHeaderValue("Bearer", token);

var request = new ReportGenerationRequest
{
    SystemId = "GL",
    StartDate = new DateTime(2025, 10, 1),
    EndDate = new DateTime(2025, 10, 31),
    ReportType = "PREMIT"
};

var response = await client.PostAsJsonAsync("/api/reports/generate", request);
var result = await response.Content.ReadFromJsonAsync<ReportGenerationResponse>();
```

### JavaScript/TypeScript

```typescript
import axios from 'axios';

const client = axios.create({
  baseURL: 'https://localhost:5001/api',
  headers: { Authorization: `Bearer ${token}` }
});

const response = await client.post('/reports/generate', {
  systemId: 'GL',
  startDate: '2025-10-01',
  endDate: '2025-10-31',
  reportType: 'PREMIT'
});

console.log('Job ID:', response.data.jobId);
```

### Python

```python
import requests

client = requests.Session()
client.headers.update({'Authorization': f'Bearer {token}'})

response = client.post('https://localhost:5001/api/reports/generate', json={
    'systemId': 'GL',
    'startDate': '2025-10-01',
    'endDate': '2025-10-31',
    'reportType': 'PREMIT'
})

print('Job ID:', response.json()['jobId'])
```

---

## OpenAPI Specification

A especificação completa OpenAPI 3.0 está disponível em:
- Desenvolvimento: `https://localhost:5001/swagger/v1/swagger.json`
- Documentação interativa: `https://localhost:5001/swagger`

---

## Support & Contact

- **Email**: suporte-sistemas@caixaseguradora.com.br
- **Documentação**: `/docs`
- **Issue Tracker**: Internal JIRA
- **SLA**: 99.5% uptime, suporte 24/7 para produção

---

**Última Atualização**: Outubro 2025
**Versão da API**: 1.0
**Compatibilidade**: Todas as versões do backend 1.x
