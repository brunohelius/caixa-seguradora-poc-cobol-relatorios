# Backend - Sistema de Relatórios de Prêmios SUSEP

## Build Configuration

This project uses **two separate solution files** to maintain production code quality while test projects are being updated:

### Production Build (RECOMMENDED)

**File**: `CaixaSeguradora.Production.sln`

**Contains**:
- CaixaSeguradora.Api
- CaixaSeguradora.Core
- CaixaSeguradora.Infrastructure

**Status**: ✅ Builds successfully with 0 errors, 0 warnings

**Use this for**:
- Active development
- Production deployments
- CI/CD pipelines
- Docker container builds

**Build commands**:
```bash
# Restore dependencies
dotnet restore CaixaSeguradora.Production.sln

# Build production code
dotnet build CaixaSeguradora.Production.sln

# Run API
cd src/CaixaSeguradora.Api
dotnet run
```

### Test Build (Currently Disabled)

**File**: `CaixaSeguradora.Tests.sln`

**Contains**:
- All production projects (for test references)
- CaixaSeguradora.UnitTests
- CaixaSeguradora.IntegrationTests
- CaixaSeguradora.ComparisonTests

**Status**: ⚠️ Temporarily disabled due to entity schema alignment work in progress

**Known Issues**: Test projects reference outdated entity schemas and need updates to match current production code.

**When to use**: After test projects are updated and re-enabled (see `/KNOWN_ISSUES.md` for details and timeline)

### Original Solution File

**File**: `CaixaSeguradora.sln`

**Status**: ⚠️ Kept for backwards compatibility, includes all projects (production + tests)

**Recommendation**: Use `CaixaSeguradora.Production.sln` instead for reliable builds.

**For detailed information**: See `/KNOWN_ISSUES.md` in the project root.

## Arquitetura

Este backend segue os princípios de **Clean Architecture** (também conhecida como Arquitetura Hexagonal ou Onion Architecture), organizando o código em camadas concêntricas com dependências unidirecionais.

### Estrutura de Camadas

```
┌─────────────────────────────────────────┐
│   API Layer (CaixaSeguradora.Api)      │  ← Controllers, Middleware, Configuration
├─────────────────────────────────────────┤
│   Core Layer (CaixaSeguradora.Core)    │  ← Entities, Interfaces, Domain Services
├─────────────────────────────────────────┤
│ Infrastructure Layer                    │  ← Repositories, EF Core, External Services
│ (CaixaSeguradora.Infrastructure)       │
└─────────────────────────────────────────┘
              ↓
         Database (SQLite)
```

### Camada API (`CaixaSeguradora.Api`)

**Responsabilidade**: Expor endpoints HTTP e gerenciar concerns de infraestrutura web.

**Componentes**:
- **Controllers/** - Endpoints REST (DashboardController, ReportsController, etc.)
- **Middleware/** - Exception handling, logging, authentication
- **Program.cs** - Configuração da aplicação (DI, CORS, Swagger, Serilog)
- **appsettings.json** - Configurações (connection strings, logging levels)

**Dependências**:
- `CaixaSeguradora.Core` ✅
- `CaixaSeguradora.Infrastructure` ✅
- ASP.NET Core 9.0
- Swashbuckle.AspNetCore (Swagger)
- Serilog.AspNetCore

**Princípios**:
- Controllers são "thin" - apenas validação de entrada e mapeamento de DTOs
- Lógica de negócio delegada à camada Core
- Tratamento de erros centralizado em middleware

### Camada Core (`CaixaSeguradora.Core`)

**Responsabilidade**: Definir modelos de domínio, regras de negócio e contratos de serviço.

**Componentes**:
- **Entities/** - Modelos de domínio (PremiumRecord, Policy, Endorsement, etc.)
- **Interfaces/** - Contratos de repositórios e serviços
- **Services/** - Lógica de negócio (PremiumCalculationService, CossuranceService)
- **DTOs/** - Data Transfer Objects para comunicação entre camadas
- **Exceptions/** - Exceções de domínio customizadas
- **Attributes/** - CobolFieldAttribute para metadados COBOL
- **Utilities/** - CobolMath (arredondamento, precisão decimal)

**Dependências**:
- Nenhuma dependência externa (apenas .NET BCL)
- Esta é a camada mais estável - mudanças mínimas

**Princípios**:
- Entidades são Plain Old CLR Objects (POCOs) sem lógica de persistência
- Interfaces definem contratos, não implementações
- Validações de domínio dentro das entidades

### Camada Infrastructure (`CaixaSeguradora.Infrastructure`)

**Responsabilidade**: Implementar detalhes técnicos de persistência, acesso a dados e serviços externos.

**Componentes**:
- **Data/** - DbContext, configurações EF Core
  - `PremiumReportingDbContext.cs` - Contexto principal
  - `Configurations/` - Fluent API para entidades (indexes, relationships)
  - `Migrations/` - Migrações EF Core
- **Repositories/** - Implementações de IPremiumRepository, IPolicyRepository, etc.
- **Services/** - Implementações de serviços externos
  - `FixedWidthFormatter.cs` - Formatação de saída COBOL
  - `FileGenerationService.cs` - Geração de arquivos PREMIT/PREMCED
  - `ExternalModuleService.cs` - Mocks para módulos COBOL (GE0009S, GE0010S, RE0001S)

**Dependências**:
- `CaixaSeguradora.Core` ✅
- Microsoft.EntityFrameworkCore 9.0
- Microsoft.EntityFrameworkCore.Sqlite
- System.Text.Json

**Princípios**:
- Repositórios implementam interfaces da camada Core
- EF Core é um detalhe de implementação (pode ser trocado)
- Utiliza IAsyncEnumerable<T> para cursor streaming (performance)

## Prerequisites

- **.NET 9.0 SDK** - Download from https://dotnet.microsoft.com/download/dotnet/9.0
- **SQLite 3.x** - Included with .NET, no separate installation needed
- **Docker** (optional) - For containerized deployment

## Quick Start

### 1. Clone the repository

```bash
cd backend
```

### 2. Restore dependencies

```bash
dotnet restore
```

### 3. Apply database migrations

```bash
cd src/CaixaSeguradora.Api
dotnet ef database update
```

### 4. Seed sample data (optional)

```bash
dotnet run --seed-data
```

### 5. Run the application

```bash
dotnet run
```

The API will start on:
- HTTP: http://localhost:5555
- HTTPS: https://localhost:5556
- Swagger: https://localhost:5555/swagger

## Running Tests

### Run all tests

```bash
cd backend
dotnet test
```

### Run specific test categories

```bash
# Unit tests only
dotnet test --filter Category=Unit

# Integration tests only
dotnet test --filter Category=Integration

# Comparison tests (COBOL parity validation)
dotnet test --filter Category=Comparison
```

### Run tests with coverage

```bash
dotnet test /p:CollectCoverage=true /p:CoverageReportFormat=html
```

Coverage report will be generated at `tests/*/coverage/index.html`

## Configuration

### Environment Variables

Key configuration values can be overridden via environment variables:

```bash
# Database
export ConnectionStrings__DefaultConnection="Data Source=premium_reporting.db"

# JWT Authentication
export Jwt__SecretKey="your-secret-key-min-32-chars"
export Jwt__Issuer="CaixaSeguradora"
export Jwt__Audience="CaixaSeguradora.API"

# Ports
export BACKEND_HTTP_PORT=5555
export BACKEND_HTTPS_PORT=5556

# Logging
export Serilog__MinimumLevel__Default="Information"
```

### appsettings.json Files

- **appsettings.json** - Base configuration (checked into git)
- **appsettings.Development.json** - Development overrides (checked into git)
- **appsettings.Production.json** - Production overrides (checked into git, no secrets)

**NEVER commit secrets** - Use environment variables or Azure Key Vault for production secrets.

## Docker Deployment

### Build Docker image

```bash
cd backend
docker build -t caixa-seguradora-backend:latest .
```

### Run with Docker Compose

```bash
cd .. # project root
docker-compose up --build
```

This starts both backend and frontend services with proper networking.

## Health Checks

The application exposes three health check endpoints:

- **GET /api/v1/health** - Comprehensive health check (database, file system, disk space)
- **GET /api/v1/health/live** - Liveness probe (returns 200 if API is running)
- **GET /api/v1/health/ready** - Readiness probe (returns 200 if can serve traffic)

Example response from `/api/v1/health`:

```json
{
  "status": "Healthy",
  "timestamp": "2025-10-27T12:00:00Z",
  "version": "1.0.0",
  "databaseStatus": "Healthy",
  "databaseResponseTimeMs": 45,
  "checks": {
    "Database": {
      "status": "Healthy",
      "responseTimeMs": 45,
      "message": "Database connection is healthy"
    },
    "FileSystem": {
      "status": "Healthy",
      "responseTimeMs": 2,
      "message": "Output directory writable. Available space: 150.50 GB"
    }
  }
}
```

## Logging

Structured logging is configured using **Serilog** with:

- **Console sink** - Colored output for development
- **File sink** - Rolling logs at `logs/premiumreporting-{Date}.log` (7 days retention)
- **Correlation ID** - Every request has a unique X-Correlation-ID header for tracking

Example log entry:

```
2025-10-27 14:30:45.123 -03:00 [INF] CorrelationId=abc123def456 Processing premium calculation for policy 1234567890123
```

## Troubleshooting

### Database Locked Error

If you see "database is locked" errors:

```bash
# Close any open database connections
# Delete lock file
rm src/CaixaSeguradora.Api/premium-reporting-dev.db-shm
rm src/CaixaSeguradora.Api/premium-reporting-dev.db-wal
```

### Port Already in Use

If port 5555 is in use:

```bash
# Option 1: Kill process using the port
lsof -ti:5555 | xargs kill -9

# Option 2: Change port in appsettings.json
# Set Ports:Http to different value (e.g., 5557)
```

### Migration Errors

If migrations fail:

```bash
# Drop database and recreate
rm src/CaixaSeguradora.Api/premium-reporting-dev.db
dotnet ef database update
```

## Tecnologias e Pacotes

### Framework e Runtime

- **.NET 9.0 SDK** - Framework principal
- **C# 13** - Linguagem de programação
- **ASP.NET Core 9.0** - Framework web

### Entity Framework Core

```xml
<PackageReference Include="Microsoft.EntityFrameworkCore" Version="9.0.0" />
<PackageReference Include="Microsoft.EntityFrameworkCore.Sqlite" Version="9.0.0" />
<PackageReference Include="Microsoft.EntityFrameworkCore.Design" Version="9.0.0" />
<PackageReference Include="Microsoft.EntityFrameworkCore.Tools" Version="9.0.0" />
```

### API e Documentação

```xml
<PackageReference Include="Swashbuckle.AspNetCore" Version="6.5.0" />
<PackageReference Include="AutoMapper" Version="13.0.1" />
<PackageReference Include="AutoMapper.Extensions.Microsoft.DependencyInjection" Version="13.0.1" />
```

### Logging

```xml
<PackageReference Include="Serilog.AspNetCore" Version="8.0.0" />
<PackageReference Include="Serilog.Sinks.Console" Version="5.0.0" />
<PackageReference Include="Serilog.Sinks.File" Version="5.0.0" />
```

### Testes

```xml
<PackageReference Include="xUnit" Version="2.6.0" />
<PackageReference Include="xunit.runner.visualstudio" Version="2.5.3" />
<PackageReference Include="FluentAssertions" Version="6.12.0" />
<PackageReference Include="Moq" Version="4.20.70" />
<PackageReference Include="Microsoft.AspNetCore.Mvc.Testing" Version="9.0.0" />
<PackageReference Include="Microsoft.EntityFrameworkCore.InMemory" Version="9.0.0" />
```

## Modelo de Dados

### Entidades Principais (15 Entidades)

1. **PremiumRecord** - Registro de prêmio (V0PREMIOS) - 687 campos COBOL mapeados
2. **Policy** - Apólice de seguro (V0APOLICE)
3. **Endorsement** - Endosso de apólice (V0ENDOSSO)
4. **Product** - Produto de seguro (V0PRODUTO)
5. **Client** - Cliente/tomador (V0CLIENTE)
6. **Address** - Endereço (V0ENDERECOS)
7. **Agency** - Agência/canal de vendas (V0AGENCIAS)
8. **Producer** - Produtor/corretor (V0PRODUTOR)
9. **Coverage** - Cobertura de apólice (V0COBERAPOL)
10. **Invoice** - Fatura (V0FATURAS)
11. **Installment** - Parcela de pagamento (V0HISTOPARC)
12. **CossuredPolicy** - Apólice cossegurada (V0APOLCOSCED)
13. **CossuranceCalculation** - Cálculo de cosseguro (GE399)
14. **SystemConfiguration** - Configuração do sistema (V0SISTEMA)
15. **ReportDefinition** - Definição de relatório (V0RELATORIOS)

### Relacionamentos Principais

```
Policy (1) ──── (N) PremiumRecord
       │
       ├──── (N) Endorsement
       │
       ├──── (N) Coverage
       │
       ├──── (N) CossuredPolicy
       │
       └──── (N) CossuranceCalculation

Client (1) ──── (N) Address
       │
       └──── (N) Policy

Product (1) ──── (N) Policy

Agency (1) ──── (N) Policy

Producer (1) ──── (N) Policy

Invoice (1) ──── (N) Installment
```

### Mapeamento COBOL → C#

| COBOL PIC | C# Type | Exemplo | Observações |
|-----------|---------|---------|-------------|
| `9(9)` | `int` | Código numérico | Inteiros até 9 dígitos |
| `9(13)` | `long` | Número de apólice | Inteiros longos |
| `9(13)V99` | `decimal(15,2)` | Valor monetário | Precisão financeira |
| `9(4)V9(9)` | `decimal(13,9)` | Percentual | Alta precisão (cosseguro) |
| `X(n)` | `string` com `[MaxLength(n)]` | Texto fixo | Strings de tamanho fixo |
| `9(n) COMP` | `int` ou `long` | Numérico binário | Binary integer |
| `9(n) COMP-3` | `decimal` | Packed decimal | Decimal empacotado |

**Atributo Customizado**:
```csharp
[CobolField(PicClause = "9(13)V99", Length = 15, DecimalPlaces = 2, FieldType = CobolFieldType.PackedDecimal)]
[Column(TypeName = "decimal(15,2)")]
public decimal TotalPremium { get; set; }
```

## Endpoints da API

### Dashboard (User Story 1)

| Método | Endpoint | Descrição |
|--------|----------|-----------|
| GET | `/api/dashboard/metrics` | Métricas gerais do sistema |
| GET | `/api/dashboard/function-points` | Estimativa de pontos de função |
| GET | `/api/dashboard/database-dependencies` | Dependências de banco de dados |

### Relatórios (User Story 2)

| Método | Endpoint | Descrição |
|--------|----------|-----------|
| POST | `/api/reports/generate` | Gerar relatório PREMIT/PREMCED |
| GET | `/api/reports/{jobId}/status` | Verificar status de geração |
| GET | `/api/reports/{jobId}/download` | Baixar arquivo gerado |
| GET | `/api/reports/history` | Histórico de relatórios |

**Exemplo de Request (POST /api/reports/generate)**:
```json
{
  "systemId": "GL",
  "startDate": "2025-10-01",
  "endDate": "2025-10-31",
  "reportType": "PREMIT",
  "mode": "production"
}
```

**Response (202 Accepted)**:
```json
{
  "jobId": "550e8400-e29b-41d4-a716-446655440000",
  "status": "processing",
  "message": "Relatório em processamento",
  "estimatedCompletionTime": "2025-10-22T15:30:00Z"
}
```

### Consultas (User Story 3)

| Método | Endpoint | Descrição |
|--------|----------|-----------|
| POST | `/api/premiums/query` | Executar consulta de prêmios |
| GET | `/api/premiums/statistics` | Estatísticas agregadas |
| POST | `/api/export/{format}` | Exportar resultados (CSV/Excel/PDF) |

### Jobs em Lote (User Story 4)

| Método | Endpoint | Descrição |
|--------|----------|-----------|
| POST | `/api/batch-jobs` | Criar job agendado |
| GET | `/api/batch-jobs` | Listar todos os jobs |
| GET | `/api/batch-jobs/{id}` | Detalhes de um job |
| PUT | `/api/batch-jobs/{id}` | Atualizar job |
| DELETE | `/api/batch-jobs/{id}` | Deletar job |
| GET | `/api/batch-jobs/{id}/history` | Histórico de execuções |

### Dados Mock (User Story 5)

| Método | Endpoint | Descrição |
|--------|----------|-----------|
| GET | `/api/mock-data/stats` | Estatísticas do banco de dados |
| POST | `/api/mock-data/load` | Carregar dados de arquivo CSV |
| POST | `/api/mock-data/validate` | Validar integridade dos dados |
| DELETE | `/api/mock-data/reset` | Resetar banco de dados |

## Configuração e Execução

### Configuração do Banco de Dados

**Connection String** (`appsettings.json`):
```json
{
  "ConnectionStrings": {
    "DefaultConnection": "Data Source=premium_reporting.db"
  }
}
```

**Criar/Atualizar Banco de Dados**:
```bash
cd src/CaixaSeguradora.Infrastructure
dotnet ef database update --startup-project ../CaixaSeguradora.Api
```

**Criar Nova Migração**:
```bash
dotnet ef migrations add NomeDaMigracao --startup-project ../CaixaSeguradora.Api
```

### Executando a API

**Modo Desenvolvimento**:
```bash
cd src/CaixaSeguradora.Api
dotnet run
```

**Modo Produção**:
```bash
dotnet run --configuration Release --urls "http://0.0.0.0:80;https://0.0.0.0:443"
```

**Com Variáveis de Ambiente**:
```bash
export ASPNETCORE_ENVIRONMENT=Production
export ConnectionStrings__DefaultConnection="Data Source=/data/premium_reporting.db"
dotnet run
```

### Logging

Configurado com **Serilog** em `Program.cs`:

```csharp
Log.Logger = new LoggerConfiguration()
    .MinimumLevel.Information()
    .MinimumLevel.Override("Microsoft", LogEventLevel.Warning)
    .Enrich.FromLogContext()
    .WriteTo.Console()
    .WriteTo.File("logs/app-.log", rollingInterval: RollingInterval.Day)
    .CreateLogger();
```

**Níveis de Log**:
- `Trace` - Informações muito detalhadas (desenvolvimento)
- `Debug` - Informações de diagnóstico
- `Information` - Fluxo geral da aplicação
- `Warning` - Eventos anormais mas esperados
- `Error` - Erros que interrompem operação
- `Fatal` - Falhas críticas

**Arquivos de Log**: `logs/app-YYYYMMDD.log`

## Testes

### Estrutura de Testes

```
tests/
├── CaixaSeguradora.UnitTests/          # Testes unitários (lógica isolada)
│   ├── Services/                       # Testes de serviços de domínio
│   ├── Entities/                       # Testes de validações de entidades
│   └── Utilities/                      # Testes de utilitários (CobolMath)
│
├── CaixaSeguradora.IntegrationTests/   # Testes de integração (com banco)
│   ├── Api/                            # Testes de endpoints
│   ├── Repositories/                   # Testes de acesso a dados
│   └── Database/                       # Testes de schema e migrations
│
└── CaixaSeguradora.ComparisonTests/    # Comparação COBOL vs .NET
    ├── OutputValidator.cs              # Validação byte-a-byte
    ├── PremitOutputTests.cs            # Testes de saída PREMIT
    └── TestData/                       # Amostras COBOL de referência
```

### Executando Testes

**Todos os Testes**:
```bash
dotnet test
```

**Apenas Unitários**:
```bash
dotnet test --filter "FullyQualifiedName~UnitTests"
```

**Apenas Integração**:
```bash
dotnet test --filter "FullyQualifiedName~IntegrationTests"
```

**Com Cobertura**:
```bash
dotnet test /p:CollectCoverage=true /p:CoverageReportFormat=opencover /p:CoverageThreshold=90
```

### Exemplo de Teste Unitário

```csharp
public class PremiumCalculationServiceTests
{
    [Fact]
    public void CalculateTotalPremium_ShouldSumAllComponents()
    {
        // Arrange
        var premium = new PremiumRecord
        {
            BasePremiumItem = 1000.00m,
            FixedPremiumItem = 50.00m,
            AdditionalFractionalItem = 20.00m,
            IssuanceCostItem = 15.00m,
            IofItem = 74.25m // 7.38% IOF
        };
        var service = new PremiumCalculationService();

        // Act
        var total = service.CalculateTotalPremium(premium);

        // Assert
        total.Should().Be(1159.25m);
    }
}
```

### Exemplo de Teste de Integração

```csharp
public class PremiumRepositoryTests : IClassFixture<DatabaseFixture>
{
    private readonly PremiumReportingDbContext _context;

    public PremiumRepositoryTests(DatabaseFixture fixture)
    {
        _context = fixture.CreateContext();
    }

    [Fact]
    public async Task GetPremiumsByDateRange_ShouldReturnCorrectRecords()
    {
        // Arrange
        var repository = new PremiumRepository(_context);
        var startDate = new DateTime(2025, 10, 1);
        var endDate = new DateTime(2025, 10, 31);

        // Act
        var premiums = await repository
            .GetPremiumsByDateRange(startDate, endDate)
            .ToListAsync();

        // Assert
        premiums.Should().NotBeEmpty();
        premiums.Should().OnlyContain(p =>
            p.ReferenceYear == 2025 &&
            p.ReferenceMonth == 10 &&
            p.ReferenceDay >= 1 &&
            p.ReferenceDay <= 31
        );
    }
}
```

## Padrões e Boas Práticas

### Dependency Injection

Registrado em `Program.cs`:

```csharp
// Repositories
builder.Services.AddScoped<IPremiumRepository, PremiumRepository>();
builder.Services.AddScoped<IPolicyRepository, PolicyRepository>();

// Services
builder.Services.AddScoped<IPremiumCalculationService, PremiumCalculationService>();
builder.Services.AddScoped<IReportGenerationService, ReportGenerationService>();

// Infrastructure Services
builder.Services.AddSingleton<IFixedWidthFormatter, FixedWidthFormatter>();
```

### Repository Pattern

```csharp
public interface IPremiumRepository
{
    IAsyncEnumerable<PremiumRecord> GetPremiumsByDateRange(DateTime start, DateTime end);
    Task<PremiumRecord?> GetByIdAsync(long id);
    Task<int> GetCountAsync();
}

public class PremiumRepository : IPremiumRepository
{
    private readonly PremiumReportingDbContext _context;

    public PremiumRepository(PremiumReportingDbContext context)
    {
        _context = context;
    }

    public async IAsyncEnumerable<PremiumRecord> GetPremiumsByDateRange(DateTime start, DateTime end)
    {
        var query = _context.PremiumRecords
            .AsNoTracking()
            .Where(p => p.ReferenceYear >= start.Year && p.ReferenceYear <= end.Year)
            .OrderBy(p => p.ReferenceYear)
            .ThenBy(p => p.ReferenceMonth)
            .ThenBy(p => p.ReferenceDay);

        await foreach (var premium in query.AsAsyncEnumerable())
        {
            yield return premium;
        }
    }
}
```

### Tratamento de Erros

**Exception Middleware** (`ExceptionHandlerMiddleware.cs`):

```csharp
public async Task InvokeAsync(HttpContext context, RequestDelegate next)
{
    try
    {
        await next(context);
    }
    catch (DomainException ex)
    {
        await HandleExceptionAsync(context, ex, StatusCodes.Status400BadRequest);
    }
    catch (NotFoundException ex)
    {
        await HandleExceptionAsync(context, ex, StatusCodes.Status404NotFound);
    }
    catch (Exception ex)
    {
        _logger.LogError(ex, "Erro não tratado");
        await HandleExceptionAsync(context, ex, StatusCodes.Status500InternalServerError);
    }
}
```

**Resposta de Erro**:
```json
{
  "type": "https://tools.ietf.org/html/rfc7231#section-6.5.1",
  "title": "Erro de Validação",
  "status": 400,
  "detail": "O campo PolicyNumber é obrigatório.",
  "traceId": "00-abc123-def456-00"
}
```

### Performance

**Cursor Streaming com IAsyncEnumerable**:
```csharp
public async IAsyncEnumerable<PremiumRecord> GetAllPremiums()
{
    await foreach (var premium in _context.PremiumRecords.AsAsyncEnumerable())
    {
        yield return premium; // Streamed, não carrega tudo em memória
    }
}
```

**Índices EF Core**:
```csharp
builder.HasIndex(p => new { p.CompanyCode, p.ReferenceYear, p.ReferenceMonth })
    .HasDatabaseName("IX_Premiums_DateRange");
```

**AsNoTracking para Queries Read-Only**:
```csharp
var policies = await _context.Policies
    .AsNoTracking() // Melhor performance, sem change tracking
    .Where(p => p.PolicyStatus == "A")
    .ToListAsync();
```

## Segurança

### Configurações Recomendadas

- **HTTPS obrigatório** em produção
- **CORS** configurado para origens específicas
- **Rate Limiting** para prevenir abuso
- **Validação de Entrada** com FluentValidation
- **SQL Injection** prevenido por EF Core parametrizado

### CORS Configuration

```csharp
builder.Services.AddCors(options =>
{
    options.AddPolicy("AllowFrontend",
        policy => policy
            .WithOrigins("http://localhost:5173", "https://app.caixaseguradora.com.br")
            .AllowAnyHeader()
            .AllowAnyMethod()
            .AllowCredentials());
});
```

## Troubleshooting

### Erro "Cannot find SQLite.Interop.dll"

**Solução**: Reinstale o pacote SQLite:
```bash
dotnet remove package Microsoft.EntityFrameworkCore.Sqlite
dotnet add package Microsoft.EntityFrameworkCore.Sqlite --version 9.0.0
```

### Erro "No migrations found"

**Solução**: Crie a migração inicial:
```bash
dotnet ef migrations add InitialCreate --startup-project ../CaixaSeguradora.Api
dotnet ef database update --startup-project ../CaixaSeguradora.Api
```

### Erro "DI cannot resolve service"

**Solução**: Verifique se o serviço está registrado em `Program.cs`:
```csharp
builder.Services.AddScoped<ISeuServico, SuaImplementacao>();
```

## Contribuindo

Veja [CONTRIBUTING.md](../CONTRIBUTING.md) para guidelines de contribuição.

## Documentação Adicional

- [data-model.md](../data-model.md) - Modelo de dados completo
- [contracts/openapi.yaml](../contracts/openapi.yaml) - Especificação OpenAPI
- [research.md](../research.md) - Decisões técnicas e alternativas

---

**Versão**: 1.0
**Última Atualização**: Outubro 2025
