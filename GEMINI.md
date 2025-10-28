# GEMINI.md - Caixa Seguradora Premium Reporting System

## Project Overview

This project is a full-stack web application that modernizes a legacy COBOL batch processing program (RG1866B) for the Caixa Seguradora premium reporting system. The original COBOL program generates reports for the Brazilian insurance regulator (SUSEP) based on the SUSEP Circular 360. The new system is built with a .NET 9 backend and a React frontend, and it is designed to be a complete replacement for the legacy system.

The primary goal of the project is to create a modern, interactive, and maintainable application while ensuring that the generated reports are byte-for-byte identical to the original COBOL output, a critical regulatory requirement.

## Building and Running

### Prerequisites

*   .NET 9.0 SDK
*   Node.js 18+ and npm
*   Docker and Docker Compose (optional)

### Backend

The backend is a .NET 9 Web API project.

**To run the backend:**

```bash
# Navigate to the backend API directory
cd backend/src/CaixaSeguradora.Api

# Run the application
dotnet run
```

The backend will be available at `http://localhost:5555`.

**To run the backend tests:**

```bash
# Navigate to the backend directory
cd backend

# Run all tests
dotnet test
```

### Frontend

The frontend is a React application built with Vite.

**To run the frontend:**

```bash
# Navigate to the frontend directory
cd frontend

# Install dependencies
npm install

# Run the development server
npm run dev
```

The frontend will be available at `http://localhost:5173`.

**To run the frontend tests:**

```bash
# Navigate to the frontend directory
cd frontend

# Run unit tests
npm run test

# Run E2E tests
npm run test:e2e
```

### Docker

The project can also be run using Docker Compose.

**To run the application with Docker:**

```bash
# From the project root directory
docker-compose up --build
```

## Development Conventions

### Backend (C#)

*   **Architecture:** Clean Architecture (Api, Core, Infrastructure)
*   **Styling:** PascalCase for classes and methods, `_camelCase` for private fields.
*   **Async:** `Async` suffix for asynchronous methods.
*   **Validation:** FluentValidation for request validation.
*   **Logging:** Serilog for structured logging.
*   **Background Jobs:** Hangfire for background job processing.

### Frontend (TypeScript/React)

*   **Styling:** PascalCase for components, camelCase for functions.
*   **State Management:** React Hooks.
*   **Styling:** TailwindCSS with Caixa Seguradora brand colors.
*   **API Communication:** Axios for HTTP requests.
*   **Testing:** Vitest for unit tests and Playwright for E2E tests.

### Git

*   **Branching:** `feature/<feature-name>` for new features.
*   **Commits:** Conventional Commits standard (`feat:`, `fix:`, `test:`, etc.).
