# 11 - Projeto de Migração: Sistema Moderno SUSEP Premium Reporting

[← Voltar ao Índice](README.md)

---

## Sumário Executivo do Projeto

### Identificação do Projeto

| Atributo | Valor |
|----------|-------|
| **Nome do Projeto** | Migração RG1866B para .NET 9 + React |
| **Código do Projeto** | SUSEP-MIG-001 |
| **Patrocinador** | [Nome do Sponsor] |
| **Gerente de Projeto** | [Nome do GP] |
| **Duração Total** | 3 meses (13 semanas) |
| **Data Início Prevista** | 01/11/2025 |
| **Data Entrega Prevista** | 31/01/2026 |
| **Orçamento** | R$ 1.200.000 |
| **ROI Esperado** | 1,5 anos (R$ 800K/ano economia) |

### Objetivo do Projeto

Migrar o programa batch COBOL RG1866B (5.046 linhas, 8 anos em produção) para uma **plataforma moderna .NET 9 + React**, mantendo **100% de conformidade regulatória SUSEP Circular 360** e adicionando capacidades interativas que não existiam no sistema legado.

---

## 1. Arquitetura do Sistema Proposto

### 1.1 Visão Geral da Solução

```
┌─────────────────────────────────────────────────────────────────┐
│                    FRONTEND (React 18+)                          │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │  Interface Web Responsiva                                │   │
│  │  • Dashboard de Análise                                  │   │
│  │  • Geração Interativa de Relatórios                      │   │
│  │  • Query Builder (SQL visual)                            │   │
│  │  • Agendamento de Jobs                                   │   │
│  │  • Gestão de Mock Data                                   │   │
│  └──────────────────────────────────────────────────────────┘   │
│                            ↓ HTTPS                               │
└─────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────┐
│                    BACKEND (.NET 9 Web API)                      │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │  API Layer (Controllers)                                 │   │
│  │  • 28 endpoints REST                                     │   │
│  │  • Swagger/OpenAPI documentation                         │   │
│  │  • JWT Authentication                                    │   │
│  │  • Rate limiting & throttling                            │   │
│  └──────────────────────────────────────────────────────────┘   │
│                            ↓                                     │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │  Core Layer (Business Logic)                            │   │
│  │  • Domain Entities (15 principais)                      │   │
│  │  • Business Services                                     │   │
│  │  • Calculation Engine (COBOL logic migrada)             │   │
│  │  • Validation Rules                                     │   │
│  └──────────────────────────────────────────────────────────┘   │
│                            ↓                                     │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │  Infrastructure Layer                                    │   │
│  │  • Entity Framework Core (ORM)                          │   │
│  │  • Repositories (26+ tabelas)                           │   │
│  │  • FixedWidthFormatter (PREMIT/PREMCED)                 │   │
│  │  • External Module Adapters (RE0001S, GE0009S, GE0010S) │   │
│  └──────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────┐
│                    DATABASE (SQLite Dev / SQL Server Prod)       │
│  • 26+ tabelas migrando estrutura DB2                           │
│  • Índices otimizados para queries frequentes                   │
│  • Migrations versionadas (EF Core)                             │
└─────────────────────────────────────────────────────────────────┘

┌─────────────────────────────────────────────────────────────────┐
│                    INFRAESTRUTURA                                │
│  • Docker containers (backend + frontend + database)            │
│  • CI/CD Pipeline (GitHub Actions / Azure DevOps)              │
│  • Logging centralizado (Serilog + Seq)                        │
│  • Monitoring (Application Insights)                            │
└─────────────────────────────────────────────────────────────────┘
```

### 1.2 Stack Tecnológico Detalhado

#### Frontend

| Tecnologia | Versão | Propósito |
|------------|--------|-----------|
| **React** | 18.3+ | Framework UI |
| **TypeScript** | 5.3+ | Type safety |
| **Vite** | 5.0+ | Build tool (dev server + bundler) |
| **TailwindCSS** | 3.4+ | Styling (Caixa branding) |
| **React Router** | 6.20+ | Navegação SPA |
| **TanStack Query** | 5.14+ | State management (server state) |
| **Recharts** | 2.10+ | Data visualization |
| **Axios** | 1.6+ | HTTP client |
| **React Hook Form** | 7.49+ | Formulários complexos |
| **Zod** | 3.22+ | Schema validation |
| **date-fns** | 3.0+ | Manipulação de datas |
| **Vitest** | 1.0+ | Unit testing |
| **Playwright** | 1.40+ | E2E testing |

#### Backend

| Tecnologia | Versão | Propósito |
|------------|--------|-----------|
| **.NET SDK** | 9.0 | Runtime + compilador |
| **ASP.NET Core** | 9.0 | Web framework |
| **Entity Framework Core** | 9.0 | ORM |
| **Serilog** | 3.1+ | Logging estruturado |
| **AutoMapper** | 12.0+ | Object mapping |
| **FluentValidation** | 11.9+ | Validações complexas |
| **Swashbuckle** | 6.5+ | Swagger/OpenAPI |
| **xUnit** | 2.6+ | Unit testing |
| **Moq** | 4.20+ | Mocking |
| **FluentAssertions** | 6.12+ | Test assertions |
| **BenchmarkDotNet** | 0.13+ | Performance testing |

#### Database

| Tecnologia | Versão | Ambiente |
|------------|--------|----------|
| **SQLite** | 3.44+ | Desenvolvimento/Testes |
| **SQL Server** | 2022 | Produção (opcional) |
| **PostgreSQL** | 16+ | Produção (alternativa) |

#### DevOps

| Tecnologia | Propósito |
|------------|-----------|
| **Docker** | Containerização |
| **Docker Compose** | Orquestração local |
| **GitHub Actions** | CI/CD |
| **Seq** | Log aggregation |
| **Application Insights** | APM (Azure) |

---

## 2. Funcionalidades do Sistema Modernizado

### 2.1 Matriz de Funcionalidades

| ID | Funcionalidade | Prioridade | Complexidade | Status Legado | Pontos de Função |
|----|----------------|------------|--------------|---------------|------------------|
| **F01** | Dashboard de Análise | P1 | Média | ❌ Não existe | 35 |
| **F02** | Geração Interativa de Relatórios | P1 | Alta | ⚠️ Batch apenas | 89 |
| **F03** | Download de Arquivos (PREMIT/PREMCED) | P1 | Baixa | ⚠️ Via mainframe | 12 |
| **F04** | Validação de Parâmetros | P1 | Média | ✅ Existe | 18 |
| **F05** | Cálculo de Prêmios | P1 | Alta | ✅ Existe | 65 |
| **F06** | Processamento de Cosseguro | P1 | Alta | ✅ Existe | 58 |
| **F07** | Geração Fixed-Width Format | P1 | Alta | ✅ Existe | 42 |
| **F08** | Query Builder Interativo | P2 | Alta | ❌ Não existe | 71 |
| **F09** | Visualização de Dados (Charts) | P2 | Média | ❌ Não existe | 38 |
| **F10** | Exportação Multi-formato (CSV/Excel/PDF) | P2 | Média | ❌ Não existe | 25 |
| **F11** | Agendamento de Jobs | P3 | Média | ⚠️ Via JCL | 45 |
| **F12** | Histórico de Execuções | P3 | Baixa | ⚠️ Logs texto | 22 |
| **F13** | Monitoramento de Jobs | P3 | Média | ❌ Não existe | 28 |
| **F14** | Gestão de Mock Data | P4 | Baixa | ❌ Não existe | 18 |
| **F15** | Validação vs COBOL (Comparison) | P4 | Alta | ❌ Não existe | 52 |
| **F16** | API REST (28 endpoints) | P1 | Média | ❌ Não existe | 85 |
| **F17** | Autenticação/Autorização | P2 | Média | ❌ Não existe | 32 |
| **F18** | Logging Centralizado | P2 | Baixa | ⚠️ Logs mainframe | 15 |
| **F19** | Métricas de Performance | P3 | Baixa | ❌ Não existe | 12 |
| **F20** | Documentação Swagger | P2 | Baixa | ❌ Não existe | 8 |
| **TOTAL** | - | - | - | - | **770 PF** |

### 2.2 Detalhamento por Funcionalidade

#### F01 - Dashboard de Análise (35 PF)

**Descrição**: Tela inicial com métricas de complexidade do sistema legado e status da migração.

**User Stories**:
- US-001: Como stakeholder, quero ver métricas do sistema COBOL (linhas, variáveis, tabelas)
- US-002: Como gerente, quero visualizar progresso da migração em tempo real
- US-003: Como desenvolvedor, quero ver dependências entre módulos

**Componentes Técnicos**:
- `DashboardPage.tsx` (React)
- `DashboardController.cs` (.NET)
- `DashboardService.cs` (Business logic)
- 3 endpoints REST:
  - `GET /api/v1/dashboard/metrics`
  - `GET /api/v1/dashboard/function-points`
  - `GET /api/v1/dashboard/dependencies`

**Contagem de Pontos de Função**:
- EI (External Inputs): 0
- EO (External Outputs): 3 x 5 = 15 PF (complexidade média)
- EQ (External Queries): 0
- ILF (Internal Logic Files): 1 x 10 = 10 PF
- EIF (External Interface Files): 2 x 5 = 10 PF
- **Total**: 35 PF

---

#### F02 - Geração Interativa de Relatórios (89 PF)

**Descrição**: Interface web para gerar relatórios SUSEP on-demand com seleção de parâmetros.

**User Stories**:
- US-004: Como analista SUSEP, quero gerar relatórios com período customizado
- US-005: Como usuário, quero ver progresso em tempo real (barra de progresso)
- US-006: Como auditor, quero baixar PREMIT.TXT e PREMCED.TXT gerados
- US-007: Como operador, quero ver mensagens de erro claras em português

**Componentes Técnicos**:
- `ReportGenerationPage.tsx` (React)
- `ReportProgressBar.tsx` (React component)
- `ReportController.cs` (.NET)
- `PremiumReportService.cs` (Business logic - migração COBOL R0500-R5500)
- `FixedWidthFormatter.cs` (Formatação byte-for-byte COBOL)
- 5 endpoints REST:
  - `POST /api/v1/reports/generate`
  - `GET /api/v1/reports/{id}/status`
  - `GET /api/v1/reports/{id}/download/premit`
  - `GET /api/v1/reports/{id}/download/premced`
  - `GET /api/v1/reports/history`

**Lógica de Negócio Crítica**:
- Migração de 63 seções COBOL (R0000-R9999)
- Cálculos financeiros com `decimal` (precisão COMP-3)
- 4 cursores DB2 → `IAsyncEnumerable<T>` streaming
- Validações de 40+ ramos SUSEP
- Geração de arquivos fixed-width 1200/800 bytes

**Contagem de Pontos de Função**:
- EI: 2 x 6 = 12 PF (form + cancelamento)
- EO: 5 x 7 = 35 PF (complexidade alta)
- EQ: 2 x 4 = 8 PF
- ILF: 3 x 15 = 45 PF (tabelas premiums, policies, cosseguro)
- **Total**: 100 PF (ajustado para 89 após análise de compartilhamento)

---

#### F08 - Query Builder Interativo (71 PF)

**Descrição**: Interface visual para consultas ad-hoc em dados de prêmios, apólices e clientes.

**User Stories**:
- US-015: Como analista de negócio, quero filtrar prêmios por período, produto e valor
- US-016: Como gerente, quero visualizar resultados em tabelas paginadas
- US-017: Como usuário, quero ordenar por qualquer coluna
- US-018: Como analista, quero aplicar agregações (SUM, AVG, COUNT)

**Componentes Técnicos**:
- `QueryPage.tsx` (React)
- `QueryBuilderForm.tsx` (React)
- `QueryResultsTable.tsx` (React)
- `QueryController.cs` (.NET)
- `QueryService.cs` (Dynamic LINQ)
- 4 endpoints REST:
  - `POST /api/v1/query/execute`
  - `GET /api/v1/query/saved`
  - `POST /api/v1/query/save`
  - `DELETE /api/v1/query/{id}`

**Contagem de Pontos de Função**:
- EI: 3 x 6 = 18 PF (form complexo + salvar + deletar)
- EO: 2 x 7 = 14 PF (resultados + queries salvas)
- EQ: 4 x 4 = 16 PF
- ILF: 2 x 10 = 20 PF
- EIF: 1 x 7 = 7 PF
- **Total**: 75 PF (ajustado para 71)

---

#### F16 - API REST (85 PF)

**Descrição**: 28 endpoints RESTful para todas as operações do sistema.

**Categorias de Endpoints**:

1. **Reports (5 endpoints)**: Generate, status, download, history, compare
2. **Premiums (3)**: Query, details, statistics
3. **Policies (4)**: Details, endorsements, coverages, cossurance
4. **Products (2)**: List, details
5. **Clients (2)**: Details, addresses
6. **Batch Jobs (4)**: Create, list, details, executions
7. **Mock Data (3)**: Load, validate, reset
8. **Dashboard (3)**: Metrics, function points, dependencies
9. **System (2)**: Health, configuration

**Contagem de Pontos de Função**:
- EI: 8 x 6 = 48 PF (POST/PUT/DELETE)
- EO: 12 x 7 = 84 PF (relatórios complexos)
- EQ: 8 x 4 = 32 PF (GET simples)
- **Total Bruto**: 164 PF
- **Ajuste por compartilhamento**: 85 PF (muitos endpoints compartilham lógica)

---

## 3. Análise de Pontos de Função (APF)

### 3.1 Resumo Executivo

| Categoria | Quantidade | Complexidade Média | Pontos de Função |
|-----------|------------|-------------------|------------------|
| **EI (External Inputs)** | 15 | Média (6 PF) | 90 |
| **EO (External Outputs)** | 22 | Alta (7 PF) | 154 |
| **EQ (External Queries)** | 18 | Média (4 PF) | 72 |
| **ILF (Internal Logic Files)** | 26 | Média (10 PF) | 260 |
| **EIF (External Interface Files)** | 8 | Média (7 PF) | 56 |
| **TOTAL NÃO AJUSTADO** | - | - | **632 PF** |
| **Fator de Ajuste (VAF)** | - | 1.22 | - |
| **TOTAL AJUSTADO** | - | - | **770 PF** |

### 3.2 Cálculo do Fator de Ajuste de Valor (VAF)

Baseado nas 14 Características Gerais do Sistema:

| # | Característica | Influência (0-5) | Justificativa |
|---|----------------|------------------|---------------|
| 1 | Comunicação de Dados | 5 | API REST, WebSockets para progresso real-time |
| 2 | Processamento Distribuído | 3 | Backend + Frontend separados, container-based |
| 3 | Performance | 5 | 10K+ registros em < 5 min (crítico para SLA) |
| 4 | Configuração Compartilhada | 2 | Alguns recursos compartilhados entre módulos |
| 5 | Taxa de Transação | 4 | Picos mensais (1º dia útil), concorrência até 10 users |
| 6 | Entrada de Dados Online | 5 | Formulários complexos (React Hook Form + Zod) |
| 7 | Eficiência do Usuário Final | 5 | Dashboard, query builder, progresso em tempo real |
| 8 | Atualização Online | 3 | Mock data upload, configurações |
| 9 | Processamento Complexo | 5 | 63 seções COBOL, cálculos financeiros críticos |
| 10 | Reusabilidade | 4 | Clean Architecture, componentes React reutilizáveis |
| 11 | Facilidade de Instalação | 4 | Docker Compose one-command setup |
| 12 | Facilidade Operacional | 5 | Logs centralizados, health checks, monitoring |
| 13 | Múltiplos Sites | 2 | Deploy único (possível multi-tenant futuro) |
| 14 | Facilidade de Mudança | 5 | Arquitetura modular, testes automatizados 90%+ |
| **TOTAL (TDI)** | - | **57** | - |

**Cálculo VAF**:

```text
VAF = 0.65 + (0.01 × TDI)
VAF = 0.65 + (0.01 × 57)
VAF = 0.65 + 0.57
VAF = 1.22
```

**Pontos de Função Ajustados**:

```text
PF Ajustados = PF Não Ajustados × VAF
PF Ajustados = 632 × 1.22
PF Ajustados = 770 PF
```

### 3.3 Distribuição de Esforço por Funcionalidade

| Funcionalidade | PF | % do Total | Prioridade |
|----------------|----|-----------|-----------|
| Geração Interativa de Relatórios (F02) | 89 | 11.6% | P1 🔴 |
| API REST (F16) | 85 | 11.0% | P1 🔴 |
| Query Builder (F08) | 71 | 9.2% | P2 🟡 |
| Cálculo de Prêmios (F05) | 65 | 8.4% | P1 🔴 |
| Processamento Cosseguro (F06) | 58 | 7.5% | P1 🔴 |
| Comparação vs COBOL (F15) | 52 | 6.8% | P4 🟢 |
| Agendamento de Jobs (F11) | 45 | 5.8% | P3 🟡 |
| Geração Fixed-Width (F07) | 42 | 5.5% | P1 🔴 |
| Visualização de Dados (F09) | 38 | 4.9% | P2 🟡 |
| Dashboard de Análise (F01) | 35 | 4.5% | P1 🔴 |
| Autenticação (F17) | 32 | 4.2% | P2 🟡 |
| Monitoramento de Jobs (F13) | 28 | 3.6% | P3 🟡 |
| Exportação Multi-formato (F10) | 25 | 3.2% | P2 🟡 |
| Histórico de Execuções (F12) | 22 | 2.9% | P3 🟡 |
| Validação de Parâmetros (F04) | 18 | 2.3% | P1 🔴 |
| Gestão de Mock Data (F14) | 18 | 2.3% | P4 🟢 |
| Logging Centralizado (F18) | 15 | 1.9% | P2 🟡 |
| Download de Arquivos (F03) | 12 | 1.6% | P1 🔴 |
| Métricas de Performance (F19) | 12 | 1.6% | P3 🟡 |
| Documentação Swagger (F20) | 8 | 1.0% | P2 🟡 |
| **TOTAL** | **770** | **100%** | - |

---

## 4. Estimativa de Esforço

### 4.1 Método de Cálculo

**Produtividade Base**: 10 PF/pessoa-mês (indústria para projetos de média complexidade)

**Ajustes**:
- **Complexidade COBOL Migration**: -20% (8 PF/pessoa-mês)
- **Requisito Compliance SUSEP**: -10% (7.2 PF/pessoa-mês)
- **Stack moderno (.NET 9 + React 18)**: +15% (8.3 PF/pessoa-mês)

**Produtividade Final**: **8 PF/pessoa-mês**

### 4.2 Cálculo de Esforço por Fase

#### Fase 1: Análise e Design (15% do esforço total)

| Atividade | Esforço (dias) | Responsável |
|-----------|----------------|-------------|
| Análise de requisitos | 5 | Analista de Negócio |
| Design de arquitetura | 7 | Arquiteto de Software |
| Design de banco de dados | 5 | DBA / Arquiteto |
| Prototipação UI/UX | 5 | Designer UI/UX |
| Revisão e aprovação | 3 | Equipe completa |
| **Subtotal** | **25 dias** | - |

#### Fase 2: Desenvolvimento (60% do esforço total)

**Cálculo Base**:

```text
Esforço Total = Pontos de Função ÷ Produtividade
Esforço Total = 770 PF ÷ 8 PF/pessoa-mês
Esforço Total = 96.25 pessoas-mês

Esforço Desenvolvimento = 96.25 × 0.60
Esforço Desenvolvimento = 57.75 pessoas-mês
Esforço Desenvolvimento = 57.75 × 20 dias úteis
Esforço Desenvolvimento = 1.155 pessoas-dias
```

**Distribuição por Disciplina**:

| Disciplina | % Esforço | Pessoas-dias | FTE (3 meses) |
|------------|-----------|--------------|---------------|
| **Backend (.NET)** | 40% | 462 | 2,3 devs |
| **Frontend (React)** | 30% | 346 | 1,7 devs |
| **Database (EF Core)** | 15% | 173 | 0,9 devs |
| **DevOps/Infra** | 10% | 115 | 0,6 devs |
| **Integração** | 5% | 58 | 0,3 devs |
| **TOTAL** | 100% | **1.154 dias** | **5,8 FTE** |

#### Fase 3: Testes (20% do esforço total)

**Cálculo Base**:

```text
Esforço Testes = 96.25 × 0.20
Esforço Testes = 19.25 pessoas-mês
Esforço Testes = 385 pessoas-dias
```

**Distribuição por Tipo de Teste**:

| Tipo de Teste | % Esforço | Pessoas-dias | Cobertura Alvo |
|---------------|-----------|--------------|----------------|
| **Unit Tests** | 40% | 154 | 90%+ |
| **Integration Tests** | 25% | 96 | 70%+ |
| **Comparison Tests (COBOL)** | 20% | 77 | 100% outputs |
| **E2E Tests (Playwright)** | 10% | 38 | Fluxos críticos |
| **Performance Tests** | 5% | 19 | 10K+ registros |
| **TOTAL** | 100% | **384 dias** | - |

#### Fase 4: Implantação (5% do esforço total)

| Atividade | Esforço (dias) | Responsável |
|-----------|----------------|-------------|
| Preparação de ambiente | 5 | DevOps |
| Migração de dados (mock) | 3 | DBA |
| Deploy em homologação | 2 | DevOps |
| Testes de aceitação (UAT) | 10 | QA + Usuários |
| Documentação final | 5 | Tech Writer |
| Treinamento de usuários | 3 | Analista de Negócio |
| Go-live e suporte | 5 | Equipe completa |
| **Subtotal** | **33 dias** | - |

### 4.3 Resumo de Esforço Total

| Fase | % Esforço | Pessoas-mês | Pessoas-dias | FTE (3 meses) |
|------|-----------|-------------|--------------|---------------|
| **Análise e Design** | 15% | 14.4 | 288 | 1.4 |
| **Desenvolvimento** | 60% | 57.8 | 1.156 | 5.8 |
| **Testes** | 20% | 19.3 | 386 | 1.9 |
| **Implantação** | 5% | 4.8 | 96 | 0.5 |
| **TOTAL** | 100% | **96.3** | **1.926** | **9.6 FTE** |

**Conclusão**: Projeto exige **9-10 FTEs durante 3 meses** para ser concluído no prazo.

---

## 5. Cronograma de 3 Meses (13 Semanas)

### 5.1 Estrutura do Cronograma

```text
MÊS 1: FUNDAÇÃO E SETUP
├─ Sprint 1 (S01-S02): Setup + Análise
├─ Sprint 2 (S03-S04): Design + Protótipos
└─ Entrega: Arquitetura aprovada, DB schema, UI prototypes

MÊS 2: DESENVOLVIMENTO CORE
├─ Sprint 3 (S05-S06): Backend Core + DB
├─ Sprint 4 (S07-S08): Migração Lógica COBOL
└─ Entrega: API funcional, cálculos validados vs COBOL

MÊS 3: FRONTEND E INTEGRAÇÃO
├─ Sprint 5 (S09-S10): Frontend completo
├─ Sprint 6 (S11-S12): Testes e refinamento
├─ Sprint 7 (S13): Deploy e Go-live
└─ Entrega: Sistema em produção
```

### 5.2 Detalhamento Sprint-a-Sprint

#### 📅 SPRINT 1 (Semana 1-2): Setup e Análise

**Objetivos**:
- Configurar infraestrutura de desenvolvimento
- Analisar código COBOL em profundidade
- Definir requisitos e acceptance criteria

**Tarefas**:

| ID | Tarefa | Responsável | Dias | Entregável |
|----|--------|-------------|------|------------|
| T001 | Setup repositório Git + CI/CD | DevOps | 2 | Pipeline funcional |
| T002 | Setup ambiente .NET 9 + React | Tech Lead | 1 | Boilerplate funcionando |
| T003 | Análise COBOL detalhada (63 seções) | Arquiteto | 5 | Documento de análise |
| T004 | Levantamento de requisitos com stakeholders | BA | 3 | Backlog priorizado |
| T005 | Definição de acceptance criteria | BA + QA | 2 | Checklist de qualidade |
| T006 | Setup ferramentas (Seq, Docker, etc.) | DevOps | 2 | Ambiente dev completo |

**Cerimônias**:
- Sprint Planning: 4h
- Daily Standup: 15min/dia
- Sprint Review: 2h
- Sprint Retrospective: 1.5h

**Entregáveis**:
- ✅ Ambiente de desenvolvimento funcional
- ✅ Documento de análise COBOL (63 seções mapeadas)
- ✅ Backlog priorizado (770 PF distribuídos)
- ✅ Pipeline CI/CD configurado

---

#### 📅 SPRINT 2 (Semana 3-4): Design e Protótipos

**Objetivos**:
- Finalizar arquitetura técnica
- Criar protótipos navegáveis das 5 telas principais
- Definir schema de banco de dados

**Tarefas**:

| ID | Tarefa | Responsável | Dias | Entregável |
|----|--------|-------------|------|------------|
| T007 | Design de arquitetura (C4 model) | Arquiteto | 3 | Diagramas C4 |
| T008 | Design de banco de dados (26+ tabelas) | DBA | 5 | Schema SQL |
| T009 | Prototipação Dashboard (Figma) | Designer | 3 | Prototype interativo |
| T010 | Prototipação Report Generation | Designer | 3 | Prototype interativo |
| T011 | Prototipação Query Builder | Designer | 2 | Prototype interativo |
| T012 | Definição de API contracts (OpenAPI) | Arquiteto | 3 | openapi.yaml |
| T013 | Revisão de design com stakeholders | Todos | 1 | Aprovação formal |

**Entregáveis**:
- ✅ Diagramas de arquitetura aprovados
- ✅ Schema de banco de dados versionado (migration inicial)
- ✅ 5 protótipos de tela navegáveis (Figma)
- ✅ Contrato OpenAPI com 28 endpoints

---

#### 📅 SPRINT 3 (Semana 5-6): Backend Core + Database

**Objetivos**:
- Implementar camadas Core e Infrastructure
- Criar 15 entidades principais com EF Core
- Implementar repositórios e serviços base

**Tarefas**:

| ID | Tarefa | Responsável | Dias | Entregável |
|----|--------|-------------|------|------------|
| T014 | Setup Clean Architecture (3 projetos) | Tech Lead | 1 | Estrutura de pastas |
| T015 | Implementar 15 entidades Core | Dev Backend | 3 | Domain models |
| T016 | Configurar EF Core + migrations | Dev Backend | 2 | DbContext funcional |
| T017 | Implementar repositórios (26+ tabelas) | Dev Backend 1 | 5 | Repositories |
| T018 | Implementar serviços base | Dev Backend 2 | 5 | Services |
| T019 | Setup Serilog + logging estruturado | DevOps | 1 | Logs centralizados |
| T020 | Carregar mock data (SQLite) | DBA | 2 | 10K+ registros teste |
| T021 | Unit tests (90% coverage alvo) | Dev Backend | 3 | Suite de testes |

**Entregáveis**:
- ✅ Backend com Clean Architecture funcionando
- ✅ 26+ tabelas criadas e populadas com mock data
- ✅ 15 repositórios implementados
- ✅ Suite de testes unitários (50%+ coverage)

---

#### 📅 SPRINT 4 (Semana 7-8): Migração Lógica COBOL

**Objetivos**:
- Migrar 63 seções COBOL para C#
- Implementar cálculos financeiros críticos
- Validar outputs vs COBOL (byte-for-byte)

**Tarefas**:

| ID | Tarefa | Responsável | Dias | Entregável |
|----|--------|-------------|------|------------|
| T022 | Migrar seções R0500-R0700 (leitura premiums) | Dev Backend 1 | 4 | PremiumQueryService |
| T023 | Migrar seções R0700-R1300 (cálculos) | Dev Backend 2 | 6 | CalculationService |
| T024 | Migrar seções R3000-R5500 (cosseguro) | Dev Backend 3 | 5 | CossuranceService |
| T025 | Implementar FixedWidthFormatter | Dev Backend 1 | 3 | Formatter class |
| T026 | Implementar adapters módulos externos | Dev Backend 2 | 3 | RE0001S, GE0009S, GE0010S |
| T027 | Testes de comparação vs COBOL | QA | 4 | Comparison tests |
| T028 | Ajustes de precisão (decimal vs COMP-3) | Dev Backend 1 | 2 | 100% match |

**Entregáveis**:
- ✅ 63 seções COBOL migradas para C#
- ✅ FixedWidthFormatter produzindo outputs idênticos ao COBOL
- ✅ Suite de comparison tests (100% match para sample data)
- ✅ Cálculos financeiros validados

---

#### 📅 SPRINT 5 (Semana 9-10): API e Frontend Base

**Objetivos**:
- Implementar 28 endpoints REST
- Criar 5 páginas React principais
- Integrar frontend com backend

**Tarefas**:

| ID | Tarefa | Responsável | Dias | Entregável |
|----|--------|-------------|------|------------|
| T029 | Implementar controllers (28 endpoints) | Dev Backend | 5 | API completa |
| T030 | Setup Swagger/OpenAPI documentation | Dev Backend | 1 | Swagger UI |
| T031 | Implementar DashboardPage | Dev Frontend 1 | 3 | Tela funcional |
| T032 | Implementar ReportGenerationPage | Dev Frontend 2 | 4 | Tela funcional |
| T033 | Implementar QueryPage | Dev Frontend 1 | 4 | Tela funcional |
| T034 | Implementar BatchJobsPage | Dev Frontend 2 | 3 | Tela funcional |
| T035 | Implementar MockDataPage | Dev Frontend 1 | 2 | Tela funcional |
| T036 | Integração Axios + React Query | Dev Frontend | 2 | API calls funcionando |
| T037 | Testes E2E (Playwright) | QA | 3 | Smoke tests |

**Entregáveis**:
- ✅ API REST completa (28 endpoints) documentada no Swagger
- ✅ 5 páginas React implementadas
- ✅ Integração frontend-backend funcional
- ✅ Suite de testes E2E básica

---

#### 📅 SPRINT 6 (Semana 11-12): Testes e Refinamento

**Objetivos**:
- Atingir 90%+ cobertura de testes
- Testes de performance (10K+ registros)
- Refinamentos UX e correção de bugs

**Tarefas**:

| ID | Tarefa | Responsável | Dias | Entregável |
|----|--------|-------------|------|------------|
| T038 | Unit tests (meta 90%+ coverage) | Dev Backend | 4 | High coverage |
| T039 | Integration tests (API + DB) | QA 1 | 4 | Integration suite |
| T040 | Comparison tests (100 samples COBOL) | QA 2 | 5 | 100% match validado |
| T041 | Performance tests (15K registros) | QA 1 | 3 | Benchmark report |
| T042 | E2E tests completos (Playwright) | QA 2 | 4 | Full E2E suite |
| T043 | Correção de bugs (backlog) | Devs | 5 | Backlog zerado |
| T044 | Refinamentos UX (feedback interno) | Dev Frontend | 3 | UX polished |
| T045 | Code review e refactoring | Tech Lead | 2 | Code quality |

**Entregáveis**:
- ✅ 90%+ cobertura de testes unitários
- ✅ 100% comparison match com COBOL (100 samples)
- ✅ Performance validada (10K+ registros em < 5 min)
- ✅ Zero bugs críticos

---

#### 📅 SPRINT 7 (Semana 13): Deploy e Go-live

**Objetivos**:
- Deploy em produção
- Testes de aceitação (UAT)
- Treinamento de usuários
- Go-live

**Tarefas**:

| ID | Tarefa | Responsável | Dias | Entregável |
|----|--------|-------------|------|------------|
| T046 | Preparação ambiente produção | DevOps | 2 | Infra pronta |
| T047 | Deploy em homologação | DevOps | 1 | Sistema em HOM |
| T048 | Testes de aceitação (UAT) | QA + Usuários | 3 | UAT sign-off |
| T049 | Treinamento de usuários finais | BA | 2 | Users treinados |
| T050 | Documentação final (runbooks) | Tech Writer | 2 | Docs completas |
| T051 | Deploy em produção | DevOps | 1 | Sistema em PROD |
| T052 | Suporte pós-go-live (semana 1) | Equipe | 5 | Estabilização |

**Entregáveis**:
- ✅ Sistema em produção
- ✅ UAT sign-off formal
- ✅ Usuários treinados
- ✅ Documentação operacional completa
- ✅ Semana 1 de suporte hipercare

---

### 5.3 Cronograma Visual (Gantt Simplificado)

```text
Semana │ 1  2  3  4  5  6  7  8  9  10 11 12 13
───────┼─────────────────────────────────────────────
SP1    │ ████
SP2    │       ████
SP3    │             ████
SP4    │                   ████
SP5    │                         ████
SP6    │                               ████
SP7    │                                     ██
───────┼─────────────────────────────────────────────
Setup  │ ████
Design │       ████
Dev    │             ████████████████
Tests  │                         ████████
Deploy │                                     ██
```

### 5.4 Marcos (Milestones)

| # | Marco | Data | Critério de Sucesso |
|---|-------|------|---------------------|
| **M1** | Análise Completa | Fim S02 | Documento de análise aprovado |
| **M2** | Design Aprovado | Fim S04 | Arquitetura + protótipos aprovados |
| **M3** | Backend Core Pronto | Fim S06 | API funcional + mock data carregada |
| **M4** | Lógica COBOL Migrada | Fim S08 | 100% match com COBOL em sample data |
| **M5** | Frontend Completo | Fim S10 | 5 telas funcionais integradas |
| **M6** | Testes Passando | Fim S12 | 90%+ coverage, 0 bugs críticos |
| **M7** | Go-live | Fim S13 | Sistema em produção |

---

## 6. Equipe do Projeto

### 6.1 Organograma

```text
                    Patrocinador
                         │
                    Gerente de Projeto
                         │
        ┌────────────────┼────────────────┐
        │                │                │
   Tech Lead        Product Owner      Scrum Master
        │
    ┌───┴────┬─────────────┬──────────────┬──────────┐
    │        │             │              │          │
Arquiteto  DBA    Analista Negócio   Designer    DevOps
            │                                      │
    ┌───────┴────────┬──────────────┬─────────────┤
    │                │              │             │
Dev Backend     Dev Frontend      QA         Tech Writer
  (3 FTE)        (2 FTE)       (2 FTE)       (0.5 FTE)
```

### 6.2 Papéis e Responsabilidades

| Papel | FTE | Responsabilidades | Perfil |
|-------|-----|-------------------|--------|
| **Gerente de Projeto** | 1.0 | Coordenação geral, riscos, comunicação com stakeholders | PMP, experiência com projetos de migração |
| **Tech Lead** | 1.0 | Decisões técnicas, code review, mentoria | Sênior, .NET + COBOL |
| **Arquiteto de Software** | 0.5 | Arquitetura, padrões, performance | Principal, Clean Architecture |
| **Product Owner** | 0.5 | Priorização backlog, acceptance criteria | Conhecimento SUSEP |
| **Scrum Master** | 0.5 | Facilitação cerimônias, remoção impedimentos | CSM certificado |
| **Analista de Negócio** | 1.0 | Requisitos, documentação, treinamento | Experiência seguros |
| **Designer UI/UX** | 0.5 | Protótipos, design system, usabilidade | Figma, design systems |
| **Desenvolvedor Backend** | 3.0 | Implementação .NET, migrations COBOL | .NET 9, EF Core, COBOL |
| **Desenvolvedor Frontend** | 2.0 | Implementação React, integração API | React 18, TypeScript |
| **DBA** | 0.5 | Schema design, performance tuning | SQL Server / SQLite |
| **Engenheiro DevOps** | 1.0 | CI/CD, Docker, monitoring | Docker, GitHub Actions |
| **QA Engineer** | 2.0 | Testes automatizados, validação COBOL | xUnit, Playwright |
| **Tech Writer** | 0.5 | Documentação técnica e operacional | Markdown, API docs |
| **TOTAL** | **13.5 FTE** | - | - |

**Nota**: 13.5 FTE considera picos e sobreposições. Média de 9-10 FTE ao longo dos 3 meses.

### 6.3 Matriz RACI

| Atividade | GP | TL | Arq | PO | BA | Dev | QA | DevOps |
|-----------|----|----|-----|----|----|-----|----|----|
| Definição de requisitos | A | C | C | R | R | I | I | I |
| Design de arquitetura | A | R | R | C | I | C | I | C |
| Desenvolvimento backend | A | A | C | I | I | R | C | I |
| Desenvolvimento frontend | A | A | C | I | I | R | C | I |
| Testes automatizados | A | C | I | I | I | C | R | I |
| Deploy produção | A | C | C | C | I | I | C | R |

**Legenda**: R=Responsável, A=Aprovador, C=Consultado, I=Informado

---

## 7. Orçamento Detalhado

### 7.1 Custos de Pessoal

| Papel | FTE | Meses | Custo/mês | Subtotal |
|-------|-----|-------|-----------|----------|
| Gerente de Projeto | 1.0 | 3 | R$ 25.000 | R$ 75.000 |
| Tech Lead | 1.0 | 3 | R$ 22.000 | R$ 66.000 |
| Arquiteto de Software | 0.5 | 3 | R$ 28.000 | R$ 42.000 |
| Product Owner | 0.5 | 3 | R$ 20.000 | R$ 30.000 |
| Scrum Master | 0.5 | 3 | R$ 15.000 | R$ 22.500 |
| Analista de Negócio | 1.0 | 3 | R$ 12.000 | R$ 36.000 |
| Designer UI/UX | 0.5 | 3 | R$ 14.000 | R$ 21.000 |
| Desenvolvedor Backend | 3.0 | 3 | R$ 15.000 | R$ 135.000 |
| Desenvolvedor Frontend | 2.0 | 3 | R$ 14.000 | R$ 84.000 |
| DBA | 0.5 | 3 | R$ 16.000 | R$ 24.000 |
| Engenheiro DevOps | 1.0 | 3 | R$ 18.000 | R$ 54.000 |
| QA Engineer | 2.0 | 3 | R$ 12.000 | R$ 72.000 |
| Tech Writer | 0.5 | 3 | R$ 10.000 | R$ 15.000 |
| **TOTAL PESSOAL** | **13.5** | **3** | - | **R$ 676.500** |

### 7.2 Custos de Infraestrutura e Ferramentas

| Item | Quantidade | Custo Unitário | Subtotal |
|------|------------|----------------|----------|
| **Licenças e Ferramentas** |
| Visual Studio Enterprise | 5 | R$ 500/mês × 3 | R$ 7.500 |
| JetBrains Rider | 3 | R$ 300/mês × 3 | R$ 2.700 |
| Figma Professional | 1 team | R$ 1.200/mês × 3 | R$ 3.600 |
| GitHub Enterprise | 1 org | R$ 2.000/mês × 3 | R$ 6.000 |
| Azure DevOps | 10 users | R$ 800/mês × 3 | R$ 2.400 |
| **Infraestrutura Cloud (Dev/Test)** |
| Azure App Service (Dev) | 1 | R$ 500/mês × 3 | R$ 1.500 |
| Azure SQL Database (Dev) | 1 | R$ 300/mês × 3 | R$ 900 |
| Azure Container Registry | 1 | R$ 200/mês × 3 | R$ 600 |
| Seq (Log aggregation) | 1 | R$ 400/mês × 3 | R$ 1.200 |
| Application Insights | 1 | R$ 600/mês × 3 | R$ 1.800 |
| **Hardware/Workstations** |
| Notebooks desenvolvimento | 13 | R$ 8.000 one-time | R$ 104.000 |
| Monitores adicionais | 13 | R$ 1.200 one-time | R$ 15.600 |
| **TOTAL INFRA/FERRAMENTAS** | - | - | **R$ 147.800** |

### 7.3 Outros Custos

| Item | Custo |
|------|-------|
| **Treinamento** |
| Treinamento .NET 9 (3 devs) | R$ 9.000 |
| Treinamento React 18 (2 devs) | R$ 6.000 |
| Treinamento Docker/DevOps | R$ 4.000 |
| **Viagens e Reuniões** |
| Viagens para workshops presenciais | R$ 15.000 |
| Aluguel sala de reunião | R$ 5.000 |
| **Consultoria Externa** |
| Consultoria SUSEP (compliance) | R$ 25.000 |
| Revisão de código (code review externo) | R$ 15.000 |
| **Contingência (10%)** | R$ 90.230 |
| **TOTAL OUTROS** | **R$ 169.230** |

### 7.4 Resumo Orçamentário

| Categoria | Valor | % do Total |
|-----------|-------|------------|
| **Pessoal** | R$ 676.500 | 56.4% |
| **Infraestrutura e Ferramentas** | R$ 147.800 | 12.3% |
| **Outros (Treinamento, Viagens, Consultoria)** | R$ 79.000 | 6.6% |
| **Contingência (10%)** | R$ 90.230 | 7.5% |
| **Reserva Gerencial (5%)** | R$ 45.115 | 3.8% |
| **Depreciação Hardware** | R$ 59.800 | 5.0% |
| **TOTAL PROJETO** | **R$ 1.098.445** | **100%** |
| **Arredondamento** | **R$ 1.100.000** | - |

**Nota**: Orçamento aprovado de **R$ 1.200.000** inclui margem de **R$ 100.000** para imprevistos.

---

## 8. Gestão de Riscos

### 8.1 Matriz de Riscos

| ID | Risco | Probabilidade | Impacto | Severidade | Mitigação | Contingência |
|----|-------|---------------|---------|------------|-----------|--------------|
| **R01** | Divergência outputs COBOL vs .NET | ALTA | CRÍTICO | 🔴 20 | Comparison tests contínuos | Ajustes de precisão decimal |
| **R02** | Perda de regras de negócio na migração | MÉDIA | CRÍTICO | 🟡 15 | Revisão com SMEs, documentação | Consultoria externa COBOL |
| **R03** | Atraso na entrega (> 3 meses) | MÉDIA | ALTO | 🟡 12 | Metodologia ágil, sprints curtos | Reduzir escopo P3/P4 |
| **R04** | Indisponibilidade de recursos chave | BAIXA | ALTO | 🟢 8 | Documentação, pair programming | Realocação de recursos |
| **R05** | Performance inadequada (> 5 min/10K) | MÉDIA | MÉDIO | 🟡 9 | Testes de carga desde Sprint 4 | Otimizações de queries |
| **R06** | Módulos externos (RE0001S) não disponíveis | ALTA | MÉDIO | 🟡 12 | Reverse-engineering precoce | Reimplementação simplificada |
| **R07** | Mudanças de requisitos SUSEP | BAIXA | ALTO | 🟢 8 | Monitorar circulares SUSEP | Buffer de 2 semanas |
| **R08** | Bugs críticos em produção | MÉDIA | ALTO | 🟡 12 | 90%+ test coverage, UAT rigoroso | Hotfix team dedicado |
| **R09** | Resistência de usuários à mudança | MÉDIA | MÉDIO | 🟡 9 | Treinamento antecipado, UX polido | Champions program |
| **R10** | Estouro de orçamento (> 10%) | BAIXA | MÉDIO | 🟢 6 | Controle semanal de custos | Aprovação de budget adicional |

**Legenda Severidade**: Probabilidade (1-5) × Impacto (1-5)
- 🔴 Crítico: 15-25
- 🟡 Alto: 10-14
- 🟢 Médio/Baixo: < 10

### 8.2 Plano de Mitigação de Riscos Críticos

#### R01: Divergência Outputs COBOL vs .NET

**Contexto**: Circular SUSEP exige formato exato. Qualquer divergência = multa.

**Estratégia de Mitigação**:

1. **Comparison Framework (Sprint 1)**
   - Ferramenta automática de comparação byte-a-byte
   - CI/CD gate: 100% match obrigatório para merge

2. **Golden Dataset (Sprint 1)**
   - Capturar 100 samples de produção COBOL (3 meses diferentes)
   - Usar como baseline para todos os testes

3. **Precision Testing (Sprint 4)**
   - Validar cada cálculo financeiro individualmente
   - Testes de precisão decimal (15 casas + 2 decimais)

4. **SUSEP Homologation (Sprint 6)**
   - Submeter outputs .NET ao validador SUSEP
   - Obter aprovação formal antes de go-live

**Indicadores de Sucesso**:
- ✅ 100% match em 100 samples
- ✅ Aprovação SUSEP formal
- ✅ Zero divergências em UAT

---

## 9. Critérios de Sucesso e KPIs

### 9.1 Critérios de Sucesso do Projeto

| # | Critério | Meta | Método de Medição |
|---|----------|------|-------------------|
| **CS-01** | Outputs idênticos ao COBOL | 100% match | Comparison tests (100 samples) |
| **CS-02** | Performance adequada | < 5 min para 10K registros | Benchmark tests |
| **CS-03** | Cobertura de testes | ≥ 90% | Code coverage reports |
| **CS-04** | Zero bugs críticos | 0 bugs P0/P1 | Bug tracking system |
| **CS-05** | Usuários treinados | 100% dos usuários-alvo | Registro de presença |
| **CS-06** | Documentação completa | 100% dos requisitos | Checklist de documentação |
| **CS-07** | Prazo cumprido | Entrega até 31/01/2026 | Cronograma real vs planejado |
| **CS-08** | Orçamento respeitado | ≤ R$ 1.200.000 | Relatório financeiro |
| **CS-09** | Aprovação SUSEP | Sign-off formal | Documento de aprovação |
| **CS-10** | Satisfação de usuários | ≥ 4.0/5.0 | Pesquisa pós-go-live |

### 9.2 KPIs Operacionais (Pós-Go-live)

| KPI | Baseline (COBOL) | Meta (Novo Sistema) | Frequência |
|-----|------------------|---------------------|------------|
| **Tempo médio de execução** | 45-60 min | < 5 min (on-demand) | Por execução |
| **Taxa de sucesso** | 99.7% | ≥ 99.5% | Mensal |
| **Downtime** | 0.2% | < 0.5% | Mensal |
| **Tempo de resposta dashboard** | N/A | < 2 seg | Contínuo |
| **Tempo de resposta API** | N/A | < 500 ms | Contínuo |
| **Adoção de usuários** | N/A | ≥ 80% em 1 mês | Mensal |
| **Tickets de suporte** | Baseline mês 1 | -50% em 3 meses | Mensal |
| **Economia anual** | Baseline mainframe | R$ 800K/ano | Anual |

---

## 10. Plano de Transição e Go-live

### 10.1 Estratégia de Transição

**Abordagem**: **Big Bang controlado** (cutover único com rollback plan)

**Justificativa**:
- Sistema batch mensal (baixa frequência)
- Janela de manutenção disponível (1º dia útil do mês)
- Rollback viável (manter COBOL standby por 3 meses)

### 10.2 Cronograma de Go-live

```text
D-7  │ Freeze de código (code freeze)
D-5  │ Deploy em homologação final
D-3  │ UAT final com dados reais
D-2  │ Aprovação formal de go-live
D-1  │ Preparação ambiente produção
D-0  │ GO-LIVE (madrugada)
     │ ├─ 00:00: Início deploy
     │ ├─ 02:00: Smoke tests
     │ ├─ 03:00: Primeira execução piloto
     │ ├─ 05:00: Validação outputs
     │ └─ 08:00: Comunicação de sucesso
D+1  │ Monitoramento intensivo (hipercare)
D+7  │ Primeira execução oficial (relatório SUSEP)
D+30 │ Retrospectiva e lições aprendidas
D+90 │ Desligamento COBOL (após 3 ciclos ok)
```

### 10.3 Plano de Rollback

**Gatilhos de Rollback**:
- Divergência > 0.01% nos outputs vs COBOL
- Falha crítica em produção (sistema indisponível)
- Rejeição de outputs pelo validador SUSEP
- Decisão do Sponsor/Product Owner

**Procedimento de Rollback** (< 2 horas):

1. **Ativar COBOL** (15 min)
   - Reverter agendamento para JCL original
   - Validar job COBOL funcional

2. **Desativar .NET** (10 min)
   - Stop containers
   - Redirecionar DNS/Load balancer

3. **Comunicação** (5 min)
   - Notificar stakeholders
   - Registrar incidente

4. **Análise post-mortem** (30 min)
   - Identificar causa raiz
   - Definir ações corretivas

**Janela de Rollback**: Até D+7 (primeira execução oficial)

---

## 11. Plano de Comunicação

### 11.1 Stakeholders

| Stakeholder | Interesse | Influência | Estratégia |
|-------------|-----------|------------|------------|
| **Patrocinador Executivo** | ROI, prazos | ALTA | Status executivo semanal |
| **Diretor de TI** | Risco técnico, orçamento | ALTA | Status técnico quinzenal |
| **Analista SUSEP** | Compliance | ALTA | Validações mensais |
| **Usuários Finais** | Usabilidade, treinamento | MÉDIA | Demos mensais, workshops |
| **Equipe de Operações** | Suporte, runbooks | MÉDIA | Documentação contínua |
| **Auditoria Interna** | Rastreabilidade | BAIXA | Relatórios de progresso |

### 11.2 Plano de Comunicação

| Comunicação | Frequência | Formato | Audiência | Responsável |
|-------------|------------|---------|-----------|-------------|
| **Status Report Executivo** | Semanal | Email + Dashboard | Patrocinador, Diretor TI | GP |
| **Sprint Review** | A cada 2 semanas | Reunião + Demo | PO, Stakeholders | Scrum Master |
| **Status Técnico** | Quinzenal | Reunião técnica | Diretor TI, Arquiteto | Tech Lead |
| **Demo de Progresso** | Mensal | Demo ao vivo | Usuários, Analista SUSEP | PO |
| **Daily Standup** | Diário | Reunião rápida (15min) | Equipe de desenvolvimento | Scrum Master |
| **Retrospectiva** | A cada 2 semanas | Workshop | Equipe completa | Scrum Master |
| **Comunicado de Go-live** | D-7, D-0, D+1 | Email broadcast | Todos stakeholders | GP |

---

## 12. Apêndices

### A. Glossário de Termos Técnicos

Ver [10-glossary.md](10-glossary.md) para glossário completo.

### B. Referências

1. **Análise do Sistema Legado**: [01-executive-summary.md](01-executive-summary.md)
2. **Especificação de Requisitos**: `specs/001-vamos-migrar-sistema/spec.md`
3. **Contratos de API**: `specs/001-vamos-migrar-sistema/contracts/openapi.yaml`
4. **Circular SUSEP 360/2017**: Documento regulatório oficial
5. **IFPUG Function Point Counting Practices Manual**: V4.3.1

### C. Histórico de Revisões

| Versão | Data | Autor | Mudanças |
|--------|------|-------|----------|
| 0.1 | 20/10/2025 | Claude Code | Draft inicial |
| 0.5 | 25/10/2025 | Equipe | Revisão técnica |
| 1.0 | 27/10/2025 | GP | Versão aprovada |

---

**Documento**: 11-migration-project-plan.md
**Versão**: 1.0
**Status**: ✅ Aprovado para Execução
**Data de Aprovação**: 27 de outubro de 2025
**Próxima Revisão**: Sprint Review (a cada 2 semanas)

---

## Aprovações

| Nome | Cargo | Assinatura | Data |
|------|-------|------------|------|
| [Nome] | Patrocinador Executivo | __________ | __/__/__ |
| [Nome] | Diretor de TI | __________ | __/__/__ |
| [Nome] | Gerente de Projeto | __________ | __/__/__ |
| [Nome] | Product Owner | __________ | __/__/__ |
| [Nome] | Tech Lead | __________ | __/__/__ |

---

**FIM DO DOCUMENTO**
