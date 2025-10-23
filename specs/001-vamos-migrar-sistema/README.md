# Sistema de Relatórios de Prêmios SUSEP - Migração COBOL para .NET 9

**Versão**: 1.0
**Data**: Outubro 2025
**Status**: Implementação Completa

## Visão Geral

Este projeto migra o programa COBOL RG1866B (Sistema de Relatórios de Prêmios conforme Circular SUSEP 360) para uma aplicação web moderna utilizando .NET 9 (backend) e React 18 (frontend). O sistema processa dados de apólices, prêmios, endossos e cosseguros para gerar relatórios regulatórios PREMIT e PREMCED.

### Principais Características

- **Backend**: ASP.NET Core Web API 9.0 com Clean Architecture
- **Frontend**: React 18 + TypeScript + Vite com TailwindCSS
- **Banco de Dados**: SQLite 3 (desenvolvimento) com compatibilidade DB2
- **Testes**: xUnit, FluentAssertions, Vitest, Playwright
- **Infraestrutura**: Docker, Docker Compose

### Funcionalidades Principais

1. **Dashboard Interativo** - Visualização de métricas de migração e complexidade do sistema
2. **Geração de Relatórios** - Criação de relatórios PREMIT/PREMCED conforme regulamentação SUSEP
3. **Consulta de Dados** - Interface para consultas ad-hoc com exportação (CSV, Excel, PDF)
4. **Jobs em Lote** - Agendamento e monitoramento de processamentos batch
5. **Gerenciamento de Dados** - Upload, validação e gerenciamento de dados de teste

## Requisitos

### Backend (.NET)

- .NET 9.0 SDK
- SQLite 3.x
- Visual Studio 2022 ou VS Code com extensão C#

### Frontend (React)

- Node.js 20+ com npm 10+
- Navegador moderno (Chrome 120+, Firefox 120+, Edge 120+)

### Ferramentas de Desenvolvimento

- Git 2.40+
- Docker 24+ e Docker Compose 2.20+ (opcional)
- Postman ou similar para testes de API (opcional)

## Início Rápido

### 1. Clonar o Repositório

```bash
git clone <repository-url>
cd specs/001-vamos-migrar-sistema
```

### 2. Configurar Backend

```bash
cd backend

# Restaurar pacotes NuGet
dotnet restore

# Executar migrações do banco de dados
cd src/CaixaSeguradora.Infrastructure
dotnet ef database update --startup-project ../CaixaSeguradora.Api

# Voltar ao diretório backend
cd ../..

# Executar testes
dotnet test

# Iniciar API
cd src/CaixaSeguradora.Api
dotnet run
```

A API estará disponível em: `https://localhost:5001` (HTTPS) ou `http://localhost:5000` (HTTP)
Swagger UI: `https://localhost:5001/swagger`

### 3. Configurar Frontend

```bash
cd frontend

# Instalar dependências
npm install

# Executar em modo de desenvolvimento
npm run dev
```

O frontend estará disponível em: `http://localhost:5173`

### 4. Usando Docker Compose (Recomendado)

```bash
# Do diretório raiz do projeto
docker-compose up --build
```

Isso iniciará:
- Backend API em `http://localhost:5000`
- Frontend em `http://localhost:3000`
- Volume persistente para banco de dados SQLite

## Estrutura do Projeto

```
001-vamos-migrar-sistema/
├── backend/                      # ASP.NET Core Web API
│   ├── src/
│   │   ├── CaixaSeguradora.Api/           # Camada de API (Controllers, Middleware)
│   │   ├── CaixaSeguradora.Core/          # Camada de Domínio (Entidades, Interfaces)
│   │   └── CaixaSeguradora.Infrastructure/ # Camada de Infraestrutura (Repositórios, Serviços)
│   ├── tests/
│   │   ├── CaixaSeguradora.UnitTests/
│   │   ├── CaixaSeguradora.IntegrationTests/
│   │   └── CaixaSeguradora.ComparisonTests/
│   ├── CaixaSeguradora.sln
│   └── README.md                 # Documentação detalhada do backend
│
├── frontend/                     # React + TypeScript + Vite
│   ├── src/
│   │   ├── components/          # Componentes reutilizáveis
│   │   │   ├── common/         # Componentes genéricos (Button, Card, Spinner)
│   │   │   ├── dashboard/      # Componentes do dashboard
│   │   │   ├── reports/        # Componentes de relatórios
│   │   │   ├── query/          # Componentes de consulta
│   │   │   ├── batch/          # Componentes de jobs em lote
│   │   │   └── data/           # Componentes de gestão de dados
│   │   ├── pages/              # Páginas (routes)
│   │   ├── services/           # Clientes API
│   │   ├── hooks/              # Custom React hooks
│   │   ├── utils/              # Funções utilitárias
│   │   └── styles/             # Estilos globais
│   ├── tests/
│   ├── package.json
│   └── README.md               # Documentação detalhada do frontend
│
├── contracts/                   # Contratos da API
│   ├── openapi.yaml            # Especificação OpenAPI 3.0
│   └── schemas/                # Schemas JSON
│
├── docs/                       # Documentação do projeto
│   ├── parser/                 # Análise do COBOL original
│   ├── migration/              # Documentação da migração
│   └── api/                    # Documentação da API
│
├── spec.md                     # Especificação do projeto
├── plan.md                     # Plano de implementação
├── data-model.md               # Modelo de dados detalhado
├── research.md                 # Decisões técnicas de pesquisa
├── quickstart.md               # Guia de início rápido (detalhado)
├── tasks.md                    # Lista de tarefas (244 tasks)
├── docker-compose.yml
└── README.md                   # Este arquivo
```

## Guias de Uso

### Acessando a Aplicação

1. **Dashboard** (`/`) - Página inicial com métricas do projeto
2. **Geração de Relatórios** (`/reports`) - Interface para gerar relatórios PREMIT/PREMCED
3. **Consulta de Dados** (`/query`) - Consultas ad-hoc com filtros e visualizações
4. **Jobs em Lote** (`/batch-jobs`) - Agendamento e monitoramento de processamentos
5. **Dados Mock** (`/mock-data`) - Upload e validação de dados de teste

### Gerando Relatórios

1. Acesse `/reports`
2. Selecione:
   - **Sistema**: GL (Geral) ou RG (Regional)
   - **Data de Início** e **Data de Fim**: Período de referência
   - **Tipo de Relatório**: PREMIT ou PREMCED
   - **Modo**: Produção ou Teste
3. Clique em "Gerar Relatório"
4. Aguarde o processamento (progresso exibido em tempo real)
5. Baixe o arquivo gerado

### Carregando Dados de Teste

1. Acesse `/mock-data`
2. Selecione o tipo de entidade (ex: Prêmios, Apólices, Clientes)
3. Arraste e solte um arquivo CSV ou clique para selecionar
4. Clique em "Carregar Arquivo"
5. Execute validação para verificar integridade dos dados

**Formato CSV Esperado**:
- Primeira linha: nomes das colunas
- Separador: ponto-e-vírgula (`;`)
- Codificação: UTF-8
- Datas: formato `AAAA-MM-DD`
- Decimais: ponto (`.`) como separador

### Executando Consultas

1. Acesse `/query`
2. Configure filtros:
   - Intervalo de datas
   - Números de apólice
   - Produtos, Clientes, etc.
3. Clique em "Executar Consulta"
4. Visualize resultados em tabela
5. Crie visualizações (gráficos de barras, linhas, pizza)
6. Exporte para CSV, Excel ou PDF

## Arquitetura Técnica

### Backend (Clean Architecture)

```
API Layer (Controllers)
    ↓
Core Layer (Domain Models, Interfaces, Services)
    ↓
Infrastructure Layer (Repositories, EF Core, External Services)
    ↓
Database (SQLite)
```

**Principais Tecnologias**:
- ASP.NET Core Web API 9.0
- Entity Framework Core 9.0
- AutoMapper (mapeamento de DTOs)
- Serilog (logging estruturado)
- Swashbuckle (documentação OpenAPI)
- xUnit, FluentAssertions, Moq (testes)

### Frontend (Component-Based Architecture)

```
App (Router)
  ↓
Pages
  ↓
Components (Reusable)
  ↓
Services (API Clients)
```

**Principais Tecnologias**:
- React 18 com TypeScript
- Vite (build tool)
- React Router 6 (navegação)
- Axios (cliente HTTP)
- Recharts (gráficos)
- TailwindCSS (estilização)
- Vitest, React Testing Library (testes)

## Desenvolvimento

### Executando Testes

**Backend**:
```bash
cd backend

# Todos os testes
dotnet test

# Apenas testes unitários
dotnet test --filter "FullyQualifiedName~UnitTests"

# Apenas testes de integração
dotnet test --filter "FullyQualifiedName~IntegrationTests"

# Com cobertura
dotnet test /p:CollectCoverage=true /p:CoverageThreshold=90
```

**Frontend**:
```bash
cd frontend

# Testes unitários
npm test

# Testes com cobertura
npm run test:coverage

# Testes E2E (Playwright)
npm run test:e2e
```

### Formatação de Código

**Backend**:
```bash
dotnet format
```

**Frontend**:
```bash
npm run lint
npm run format
```

### Build de Produção

**Backend**:
```bash
cd backend/src/CaixaSeguradora.Api
dotnet publish -c Release -o ./publish
```

**Frontend**:
```bash
cd frontend
npm run build
# Artefatos em dist/
```

**Docker**:
```bash
docker-compose -f docker-compose.prod.yml build
docker-compose -f docker-compose.prod.yml up -d
```

## Conformidade Regulatória

Este sistema substitui o programa COBOL RG1866B mantendo:

✅ **Precisão Decimal**: Tipo `decimal` (não `float`/`double`) para cálculos financeiros
✅ **Formato Fixo**: Formatadores customizados replicam layout COBOL (espaços, zeros à esquerda)
✅ **Integridade Transacional**: Semântica COMMIT/ROLLBACK preservada
✅ **Validação de Saída**: Comparação byte-a-byte com saídas COBOL para conformidade
✅ **Rastreabilidade**: Cada seção COBOL mapeada para serviço C# correspondente

**Estruturas de Dados**:
- 687 itens de dados COBOL → Propriedades C#
- 26+ tabelas/views DB2 → Entidades EF Core
- 63 seções COBOL → Métodos de serviço

## Solução de Problemas

### Erro "Cannot connect to database"

**Solução**: Verifique se as migrações foram aplicadas:
```bash
cd backend/src/CaixaSeguradora.Infrastructure
dotnet ef database update --startup-project ../CaixaSeguradora.Api
```

### Erro "Port 5000/5001 already in use"

**Solução**: Altere a porta em `backend/src/CaixaSeguradora.Api/appsettings.json` ou:
```bash
dotnet run --urls "http://localhost:5050;https://localhost:5051"
```

### Frontend não conecta à API

**Solução**: Verifique `frontend/src/services/apiClient.ts` e ajuste a `baseURL`:
```typescript
const apiClient = axios.create({
  baseURL: 'http://localhost:5000/api', // Ajuste conforme necessário
});
```

### Erro "CORS policy" no navegador

**Solução**: Adicione a origem do frontend em `backend/src/CaixaSeguradora.Api/Program.cs`:
```csharp
builder.Services.AddCors(options =>
{
    options.AddPolicy("AllowFrontend",
        policy => policy.WithOrigins("http://localhost:5173")
                       .AllowAnyHeader()
                       .AllowAnyMethod());
});
```

## Documentação Adicional

- **[quickstart.md](./quickstart.md)** - Guia detalhado de configuração e desenvolvimento
- **[backend/README.md](./backend/README.md)** - Documentação da arquitetura backend
- **[frontend/README.md](./frontend/README.md)** - Documentação da arquitetura frontend
- **[data-model.md](./data-model.md)** - Modelo de dados completo (15 entidades)
- **[contracts/openapi.yaml](./contracts/openapi.yaml)** - Especificação OpenAPI da API
- **[spec.md](./spec.md)** - Especificação funcional completa (30 requisitos)
- **[tasks.md](./tasks.md)** - Lista de tarefas de implementação (244 tasks)

## Contribuindo

### Fluxo de Trabalho

1. Criar feature branch: `git checkout -b feature/nome-da-funcionalidade`
2. Implementar com testes
3. Executar formatação e linting
4. Commit com mensagens descritivas
5. Push e criar Pull Request
6. Aguardar revisão de código

### Padrões de Código

- **C#**: Seguir convenções .NET (PascalCase para públicos, camelCase para privados)
- **TypeScript**: Seguir ESLint + Prettier configurados
- **Commits**: Mensagens em português, formato "verbo + descrição" (ex: "Adiciona validação de CPF")
- **Testes**: Mínimo 90% de cobertura para lógica de negócio

## Licença

Propriedade da Caixa Seguradora S.A. - Todos os direitos reservados.

## Suporte

Para questões técnicas ou relato de bugs:
- **Issues**: Use o sistema de issues do repositório
- **Email**: equipe-desenvolvimento@caixaseguradora.com.br
- **Documentação**: Consulte `/docs` para documentação detalhada

---

**Versão**: 1.0
**Última Atualização**: Outubro 2025
**Responsável**: Equipe de Migração COBOL → .NET
