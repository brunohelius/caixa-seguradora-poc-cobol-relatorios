# Frontend - Sistema de Relatórios de Prêmios SUSEP

## Arquitetura

Este frontend é uma **Single Page Application (SPA)** construída com React 18, TypeScript e Vite, seguindo arquitetura baseada em componentes com separação clara de responsabilidades.

### Estrutura de Camadas

```
┌──────────────────────────────────┐
│   Pages (Routes)                 │  ← DashboardPage, ReportGenerationPage, etc.
├──────────────────────────────────┤
│   Components (Reusable UI)       │  ← Cards, Forms, Tables, Charts
├──────────────────────────────────┤
│   Services (API Clients)         │  ← dashboardService, reportService, etc.
├──────────────────────────────────┤
│   Hooks & Utilities              │  ← Custom hooks, helpers, formatters
└──────────────────────────────────┘
              ↓
     Backend API (ASP.NET Core)
```

## Tecnologias e Pacotes

### Core Stack

- **React 18.3.1** - Biblioteca de componentes UI
- **TypeScript 5.3.0** - Superset tipado de JavaScript
- **Vite 5.0.0** - Build tool e dev server ultra-rápido

### Roteamento e Estado

```json
"dependencies": {
  "react-router-dom": "^6.20.0"
}
```

### Cliente HTTP

```json
"dependencies": {
  "axios": "^1.6.2"
}
```

### Visualização de Dados

```json
"dependencies": {
  "recharts": "^2.10.3"
}
```

### Estilização

```json
"dependencies": {
  "tailwindcss": "^3.4.0",
  "@tailwindcss/forms": "^0.5.7"
}
```

### Testes

```json
"devDependencies": {
  "vitest": "^1.0.4",
  "@testing-library/react": "^14.1.2",
  "@testing-library/jest-dom": "^6.1.5",
  "@playwright/test": "^1.40.0"
}
```

## Configuração e Execução

### Instalação de Dependências

```bash
npm install
```

### Desenvolvimento

```bash
npm run dev
```

Abre em: `http://localhost:5173` com Hot Module Replacement (HMR)

### Build de Produção

```bash
npm run build
```

Artefatos gerados em: `dist/`

### Preview do Build

```bash
npm run preview
```

### Linting e Formatação

```bash
# Executar ESLint
npm run lint

# Corrigir problemas automaticamente
npm run lint:fix

# Executar Prettier
npm run format
```

## Rotas da Aplicação

| Rota | Componente | Descrição |
|------|------------|-----------|
| `/` | DashboardPage | Dashboard principal com métricas |
| `/reports` | ReportGenerationPage | Geração de relatórios PREMIT/PREMCED |
| `/query` | QueryPage | Consultas ad-hoc com filtros |
| `/batch-jobs` | BatchJobsPage | Gerenciamento de jobs agendados |
| `/mock-data` | MockDataPage | Upload e validação de dados mock |
| `*` | NotFoundPage | Página 404 |

## Documentação Adicional

- [Guia de Componentes](./docs/COMPONENTS.md) (futuro)
- [Guia de Estilos](./docs/STYLING.md) (futuro)
- [API Integration Guide](./docs/API_INTEGRATION.md) (futuro)

---

**Versão**: 1.0
**Última Atualização**: Outubro 2025
