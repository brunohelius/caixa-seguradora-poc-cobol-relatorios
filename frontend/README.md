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

## Prerequisites

- **Node.js 20+** - Download from https://nodejs.org/
- **npm 10+** - Comes with Node.js
- **Backend API running** - See backend/README.md

## Quick Start

### 1. Install dependencies

```bash
cd frontend
npm install
```

### 2. Configure API endpoint (optional)

The frontend expects the backend API at `http://localhost:5555` by default.

To change this, update `src/services/api.ts`:

```typescript
const API_BASE_URL = process.env.VITE_API_BASE_URL || 'http://localhost:5555';
```

Or set environment variable:

```bash
export VITE_API_BASE_URL=http://localhost:5555
```

### 3. Start development server

```bash
npm run dev
```

Open: `http://localhost:5173` with Hot Module Replacement (HMR)

### 4. Build for production

```bash
npm run build
```

Production build artifacts generated in: `dist/`

### 5. Preview production build

```bash
npm run preview
```

## Running Tests

### Unit Tests (Vitest)

```bash
# Run all unit tests
npm run test

# Run tests in watch mode
npm run test:watch

# Run tests with coverage
npm run test:coverage
```

Coverage report will be at `coverage/index.html`

### End-to-End Tests (Playwright)

```bash
# Install Playwright browsers (first time only)
npx playwright install

# Run E2E tests headless
npm run test:e2e

# Run E2E tests with UI
npm run test:e2e:ui

# Run E2E tests in specific browser
npx playwright test --project=chromium
```

## Code Quality

### Linting

```bash
# Run ESLint
npm run lint

# Auto-fix ESLint issues
npm run lint:fix
```

### Formatting

```bash
# Format code with Prettier
npm run format

# Check formatting without changes
npm run format:check
```

## Docker Deployment

### Build Docker image

```bash
cd frontend
docker build -t caixa-seguradora-frontend:latest .
```

### Run with Docker Compose

```bash
cd .. # project root
docker-compose up --build
```

This starts both backend and frontend services with proper networking.

## Configuration

### Environment Variables

Create a `.env` file in the frontend root (not tracked in git):

```env
# API Base URL
VITE_API_BASE_URL=http://localhost:5555

# App Title
VITE_APP_TITLE="Caixa Seguradora - Relatórios SUSEP"

# Environment
VITE_ENV=development
```

### Build-time Variables

All environment variables prefixed with `VITE_` are embedded at build time and accessible via `import.meta.env`:

```typescript
const apiUrl = import.meta.env.VITE_API_BASE_URL;
const appTitle = import.meta.env.VITE_APP_TITLE;
```

## Troubleshooting

### Port 5173 Already in Use

```bash
# Change port via --port flag
npm run dev -- --port 3000
```

### Backend API Not Reachable

1. Verify backend is running at http://localhost:5555
2. Check browser console for CORS errors
3. Verify backend CORS configuration allows http://localhost:5173

### Build Errors

```bash
# Clear node_modules and reinstall
rm -rf node_modules package-lock.json
npm install

# Clear Vite cache
rm -rf node_modules/.vite
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
