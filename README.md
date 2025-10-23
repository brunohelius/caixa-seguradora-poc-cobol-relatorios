# Sistema de Relatórios de Prêmios - Caixa Seguradora

## Visão Geral

Aplicação moderna de migração do programa COBOL RG1866B para .NET 9 + React, implementando o Sistema de Relatórios de Prêmios SUSEP Circular 360 para Caixa Seguradora.

**Status**: ✅ MVP Completo - Todas as User Stories Implementadas (Fases 1-7)

### Características Principais

- 🎯 **5 User Stories Completas**: Dashboard, Relatórios, Consultas, Jobs Batch, Gestão de Dados Mock
- 📊 **687 Itens de Dados COBOL** mapeados para C# com precisão decimal
- 🔄 **Compatibilidade Byte-a-Byte** com saída COBOL (requisito regulatório SUSEP)
- 🏗️ **Arquitetura Limpa** com separação de camadas (Api, Core, Infrastructure)
- 🎨 **Interface Moderna** em React com branding Caixa Seguradora
- ✅ **213 de 244 Tarefas Concluídas** (87% completo)

## Tecnologias

### Backend (.NET 9)
- ASP.NET Core Web API 9.0
- Entity Framework Core 9.0 (SQLite para desenvolvimento)
- Serilog (logging estruturado)
- Hangfire (agendamento de jobs)
- Swagger/OpenAPI (documentação da API)
- xUnit + FluentAssertions + Moq (testes)

### Frontend (React 18)
- React 18+ com TypeScript
- Vite (build tool)
- React Router 6+
- Axios (cliente HTTP)
- Recharts (visualização de dados)
- TailwindCSS (estilização com cores Caixa: #0047BB, #FFB81C)
- Vitest + Playwright (testes)

## Início Rápido

### Pré-requisitos

- [.NET 9.0 SDK](https://dotnet.microsoft.com/download/dotnet/9.0)
- [Node.js 18+](https://nodejs.org/) e npm
- [Git](https://git-scm.com/)
- (Opcional) [Docker](https://www.docker.com/) e Docker Compose

### Instalação

```bash
# Clone o repositório
git clone <repository-url>
cd "POC Cobol"

# Backend: Restaurar dependências e construir
cd backend
dotnet restore
dotnet build

# Frontend: Instalar dependências
cd ../frontend
npm install
```

### Executar em Desenvolvimento

**Opção 1: Executar Separadamente**

```bash
# Terminal 1 - Backend (porta 5000)
cd backend/src/CaixaSeguradora.Api
dotnet run --urls "http://localhost:5000"

# Terminal 2 - Frontend (porta 5173)
cd frontend
npm run dev
```

**Opção 2: Docker Compose**

```bash
# Na raiz do projeto
docker-compose up --build
```

### Acessar a Aplicação

- **Frontend**: http://localhost:5173
- **Backend API**: http://localhost:5000
- **Swagger UI**: http://localhost:5000/swagger
- **Hangfire Dashboard**: http://localhost:5000/hangfire

## Estrutura do Projeto

```
POC Cobol/
├── backend/
│   ├── src/
│   │   ├── CaixaSeguradora.Api/          # Controllers, Program.cs
│   │   ├── CaixaSeguradora.Core/         # Entities, Interfaces, Services
│   │   └── CaixaSeguradora.Infrastructure/ # Repositories, EF Core, External Services
│   └── tests/
│       ├── CaixaSeguradora.UnitTests/
│       ├── CaixaSeguradora.IntegrationTests/
│       └── CaixaSeguradora.ComparisonTests/
├── frontend/
│   ├── src/
│   │   ├── components/                   # Componentes React reutilizáveis
│   │   ├── pages/                        # Páginas (Dashboard, Reports, Query, etc.)
│   │   ├── services/                     # Clientes API (Axios)
│   │   └── styles/                       # CSS global e TailwindCSS
│   └── tests/
├── specs/
│   └── 001-vamos-migrar-sistema/        # Documentação SpecKit
│       ├── spec.md                       # Especificação de requisitos
│       ├── plan.md                       # Plano de implementação
│       ├── tasks.md                      # Lista de tarefas (244 tarefas)
│       ├── data-model.md                 # Modelo de dados (15 entidades)
│       ├── research.md                   # Decisões técnicas
│       └── contracts/openapi.yaml        # Contrato da API (28 endpoints)
└── docs/
    └── parser/                           # Análise do programa COBOL original
```

## User Stories Implementadas

### ✅ US1: Dashboard de Métricas de Migração (P1 - MVP)
- **Rota**: `/`
- **Descrição**: Dashboard interativo mostrando análise do programa COBOL RG1866B
- **Funcionalidades**:
  - Métricas do programa (687 itens de dados, 63 seções, 26+ tabelas)
  - Breakdown de Function Points
  - Visualização de dependências de banco de dados
  - Indicadores de progresso da migração

### ✅ US2: Gerar Relatórios de Prêmios (P2)
- **Rota**: `/reports`
- **Descrição**: Geração interativa de relatórios PREMIT/PREMCED via interface web
- **Funcionalidades**:
  - Formulário de parâmetros (intervalo de datas, sistema, tipo de relatório)
  - Processamento assíncrono com polling de status
  - Download de arquivos gerados
  - Histórico de relatórios
  - Comparação byte-a-byte com saída COBOL

### ✅ US3: Consultar e Visualizar Dados de Prêmios (P3)
- **Rota**: `/query`
- **Descrição**: Consulta interativa e visualização de dados de prêmios
- **Funcionalidades**:
  - Filtros dinâmicos (intervalo de datas, número de apólice, produto)
  - Tabela de resultados com paginação e ordenação
  - Estatísticas e agregações
  - Gráficos interativos (Recharts)
  - Exportação em CSV, Excel e PDF

### ✅ US4: Monitorar Jobs de Processamento em Batch (P4)
- **Rota**: `/batch-jobs`
- **Descrição**: Agendamento e monitoramento de jobs automatizados
- **Funcionalidades**:
  - Criar jobs agendados (expressões cron)
  - Listar todos os jobs ativos
  - Executar jobs manualmente
  - Visualizar histórico de execuções
  - Notificações por e-mail

### ✅ US5: Gerenciar Dados Mock do Banco (P4)
- **Rota**: `/data-management`
- **Descrição**: Carregar, validar e gerenciar dados de teste SQLite
- **Funcionalidades**:
  - Upload de arquivos CSV/JSON
  - Validação de dados e integridade referencial
  - Estatísticas de registros por entidade
  - Visualização de esquema
  - Reset de banco de dados
  - Comparação COBOL vs .NET

## API Endpoints

### Dashboard (US1)
- `GET /api/v1/dashboard/metrics` - Métricas gerais
- `GET /api/v1/dashboard/function-points` - Breakdown de FP
- `GET /api/v1/dashboard/database-dependencies` - Dependências de DB

### Relatórios (US2)
- `POST /api/v1/reports/generate` - Gerar relatório (202 Accepted)
- `GET /api/v1/reports/status/{jobId}` - Status de geração
- `GET /api/v1/reports/download/{jobId}` - Download do arquivo
- `GET /api/v1/reports/history` - Histórico de relatórios
- `POST /api/v1/reports/compare` - Comparar com COBOL

### Prêmios/Consultas (US3)
- `POST /api/v1/premiums/query` - Consultar prêmios (com filtros)
- `GET /api/v1/premiums/{id}` - Detalhes de prêmio
- `POST /api/v1/premiums/statistics` - Estatísticas agregadas
- `POST /api/v1/export/premiums` - Exportar (CSV/Excel/PDF)

### Apólices
- `GET /api/v1/policies/{policyNumber}` - Detalhes de apólice
- `GET /api/v1/policies/{policyNumber}/endorsements` - Endossos
- `GET /api/v1/policies/{policyNumber}/coverages` - Coberturas
- `GET /api/v1/policies/{policyNumber}/cossurance` - Cosseguro

### Produtos e Clientes
- `GET /api/v1/products` - Listar produtos
- `GET /api/v1/products/{code}` - Detalhes de produto
- `GET /api/v1/clients/{code}` - Detalhes de cliente
- `GET /api/v1/clients/{code}/addresses` - Endereços

### Batch Jobs (US4)
- `POST /api/v1/batch-jobs` - Criar job
- `GET /api/v1/batch-jobs` - Listar jobs
- `GET /api/v1/batch-jobs/{id}` - Detalhes de job
- `POST /api/v1/batch-jobs/{id}/execute` - Executar manualmente

### Mock Data (US5)
- `POST /api/mock-data/load` - Carregar dados (multipart/form-data)
- `GET /api/mock-data/validate` - Validar dados
- `DELETE /api/mock-data/clear/{entityType}` - Limpar entidade
- `POST /api/mock-data/reset` - Resetar banco
- `GET /api/mock-data/stats` - Estatísticas de registros
- `GET /api/mock-data/schema` - Informações de esquema

### Sistema
- `GET /health` - Health check

**Documentação Completa**: Acesse `/swagger` para especificação OpenAPI interativa.

## Testes

### Backend

```bash
cd backend

# Executar todos os testes
dotnet test

# Testes por categoria
dotnet test --filter Category=Unit
dotnet test --filter Category=Integration
dotnet test --filter Category=Comparison

# Com cobertura de código
dotnet test /p:CollectCoverage=true /p:CoverageReportFormat=html
# Ver relatório em: tests/*/coverage/index.html
```

### Frontend

```bash
cd frontend

# Testes unitários (Vitest)
npm run test
npm run test:watch
npm run test:coverage

# Testes E2E (Playwright)
npm run test:e2e
npm run test:e2e:ui
```

## Dados de Exemplo

Arquivos CSV de exemplo estão em `backend/tests/SampleData/`:
- `products.csv` - 3 produtos
- `clients.csv` - 5 clientes
- `policies.csv` - Apólices de exemplo
- `premiums.csv` - Registros de prêmios

**Carregar via API**:
```bash
curl -X POST "http://localhost:5000/api/mock-data/load" \
  -F "file=@backend/tests/SampleData/products.csv" \
  -F "entityType=products" \
  -F "format=csv"
```

**Carregar via Interface Web**: Navegue até `/data-management` e use o formulário de upload.

## Desenvolvimento

### Convenções de Código

**Backend (C#)**:
- Classes: PascalCase
- Métodos: PascalCase + sufixo `Async` para métodos assíncronos
- Campos privados: `_camelCase`
- Propriedades: PascalCase
- Constantes: UPPER_SNAKE_CASE

**Frontend (TypeScript/React)**:
- Componentes: PascalCase
- Funções: camelCase
- Constantes: UPPER_SNAKE_CASE
- Interfaces: PascalCase (prefixo `I` opcional)

### Regras Críticas

1. **Precisão Financeira**: Sempre use `decimal` (nunca `float` ou `double`) para valores monetários
2. **Metadados COBOL**: Todas as propriedades de entidade devem ter atributo `[CobolField]`
3. **Arquitetura Limpa**: Lógica de negócio APENAS em `Core/Services/`
4. **Português**: Todo conteúdo voltado ao usuário deve estar em português brasileiro
5. **Branding Caixa**: Usar cores oficiais (`#0047BB` azul, `#FFB81C` amarelo)

### Adicionando uma Nova Entidade

1. Criar entidade em `CaixaSeguradora.Core/Entities/`
2. Adicionar `[CobolField]` attributes
3. Criar configuração EF em `CaixaSeguradora.Infrastructure/Data/Configurations/`
4. Aplicar configuração no `DbContext`
5. Criar interface de repositório em `CaixaSeguradora.Core/Interfaces/`
6. Implementar repositório em `CaixaSeguradora.Infrastructure/Repositories/`
7. Gerar migração: `dotnet ef migrations add NomeMigracao`
8. Aplicar migração: `dotnet ef database update`

## Migração COBOL

### Programa Original: RG1866B

- **Linhas de Código**: ~5.000
- **Seções**: 63 (R0500-R5500)
- **Itens de Dados**: 687
- **Tabelas de Banco de Dados**: 26+ (V0PREMIOS, V0APOLICE, GE399, etc.)
- **Módulos Externos**: RE0001S, GE0009S, GE0010S (mockados)

### Mapeamento de Tipos COBOL → C#

| COBOL | C# | Exemplo |
|-------|-------|---------|
| `PIC 9(15)V99` | `decimal` | Valores monetários (17,2) |
| `PIC X(10)` | `string` [MaxLength(10)] | Texto fixo |
| `PIC 9(8)` | `int` | Datas (YYYYMMDD) |
| `COMP-3` | `decimal` | Packed decimal |
| `PIC S9(4)` | `short` | Inteiros com sinal |

### Requisito Regulatório

**SUSEP Circular 360**: Saída dos arquivos PREMIT.TXT e PREMCED.TXT deve ser **byte-a-byte idêntica** ao COBOL.

**Validação**: Testes de comparação em `CaixaSeguradora.ComparisonTests/` verificam correspondência de 100% com 100 amostras COBOL.

## Contribuindo

1. Clone o repositório
2. Crie uma branch para sua feature: `git checkout -b feature/nova-funcionalidade`
3. Faça commit das alterações: `git commit -m "feat: adicionar nova funcionalidade"`
4. Push para o branch: `git push origin feature/nova-funcionalidade`
5. Abra um Pull Request

### Convenções de Commit

Seguir [Conventional Commits](https://www.conventionalcommits.org/):
- `feat:` - Nova funcionalidade
- `fix:` - Correção de bug
- `test:` - Adicionar ou atualizar testes
- `docs:` - Documentação
- `refactor:` - Refatoração de código
- `chore:` - Tarefas de manutenção

## Roadmap

### ✅ Fase 1-7: Implementação Completa (CONCLUÍDO)
- Setup, Foundational, User Stories 1-5

### 🚧 Fase 8: Polish & Cross-Cutting (EM PROGRESSO - 87% completo)
- [x] Testes de integração de workflows (T213)
- [ ] Testes E2E com Playwright (T214)
- [ ] Benchmarks de performance (T215-T218)
- [ ] Documentação completa (T219-T225)
- [ ] Análise de código e linting (T226-T227)
- [ ] Segurança (autenticação JWT, validação, rate limiting) (T228-T233)
- [ ] Validação final (cobertura 90%+, requisitos, critérios de sucesso) (T234-T240)

## Documentação Adicional

- **Especificação Completa**: `specs/001-vamos-migrar-sistema/spec.md`
- **Plano de Implementação**: `specs/001-vamos-migrar-sistema/plan.md`
- **Modelo de Dados**: `specs/001-vamos-migrar-sistema/data-model.md` (15 entidades)
- **Pesquisa Técnica**: `specs/001-vamos-migrar-sistema/research.md` (mapeamento de tipos, formatters)
- **Guia Rápido**: `specs/001-vamos-migrar-sistema/quickstart.md`
- **Tarefas**: `specs/001-vamos-migrar-sistema/tasks.md` (244 tarefas detalhadas)
- **Contrato da API**: `specs/001-vamos-migrar-sistema/contracts/openapi.yaml` (28 endpoints)
- **Análise COBOL**: `docs/parser/FINAL-ANALYSIS-REPORT.md`

## Licença

[Especificar licença]

## Contato

[Informações de contato do time]

---

**Última Atualização**: Outubro 23, 2025
**Status do Projeto**: MVP Completo - 213/244 tarefas (87%)
**Versão**: 1.0.0-rc1
