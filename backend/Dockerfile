# Backend Dockerfile - Multi-stage build for CaixaSeguradora.Api
# Implements T200 - Phase 10 (Polish)
#
# Build: docker build -t caixa-seguradora-backend:latest .
# Run:   docker run -p 5555:5555 caixa-seguradora-backend:latest

# Stage 1: Build
FROM mcr.microsoft.com/dotnet/sdk:9.0 AS build
WORKDIR /src

# Copy solution and project files (paths relative to repository root)
COPY backend/CaixaSeguradora.sln ./CaixaSeguradora.sln
COPY backend/src/CaixaSeguradora.Api/CaixaSeguradora.Api.csproj ./src/CaixaSeguradora.Api/
COPY backend/src/CaixaSeguradora.Core/CaixaSeguradora.Core.csproj ./src/CaixaSeguradora.Core/
COPY backend/src/CaixaSeguradora.Infrastructure/CaixaSeguradora.Infrastructure.csproj ./src/CaixaSeguradora.Infrastructure/
COPY backend/tests/CaixaSeguradora.UnitTests/CaixaSeguradora.UnitTests.csproj ./tests/CaixaSeguradora.UnitTests/
COPY backend/tests/CaixaSeguradora.IntegrationTests/CaixaSeguradora.IntegrationTests.csproj ./tests/CaixaSeguradora.IntegrationTests/
COPY backend/tests/CaixaSeguradora.ComparisonTests/CaixaSeguradora.ComparisonTests.csproj ./tests/CaixaSeguradora.ComparisonTests/
COPY backend/tests/CaixaSeguradora.PerformanceTests/CaixaSeguradora.PerformanceTests.csproj ./tests/CaixaSeguradora.PerformanceTests/

# Restore dependencies (leverages Docker layer caching)
RUN dotnet restore CaixaSeguradora.sln

# Copy backend source code
COPY backend/. .

# Build and publish the application
WORKDIR /src/src/CaixaSeguradora.Api
RUN dotnet publish -c Release -o /app/publish --no-restore

# Stage 2: Runtime
FROM mcr.microsoft.com/dotnet/aspnet:9.0 AS runtime
WORKDIR /app

# Install curl for health checks
RUN apt-get update && apt-get install -y curl && rm -rf /var/lib/apt/lists/*

# Create non-root user for security
RUN addgroup --system --gid 1001 appuser && \
    adduser --system --uid 1001 --ingroup appuser appuser

# Copy published application from build stage
COPY --from=build /app/publish .

# Create directories for logs and output with proper permissions
RUN mkdir -p /app/logs /app/output && \
    chown -R appuser:appuser /app

# Switch to non-root user
USER appuser

# Expose port
EXPOSE 5555

# Health check (calls the health endpoint)
HEALTHCHECK --interval=30s --timeout=5s --start-period=10s --retries=3 \
    CMD curl --fail http://localhost:5555/api/v1/health/live || exit 1

# Set environment variables
ENV ASPNETCORE_ENVIRONMENT=Production \
    ASPNETCORE_URLS=http://+:5555 \
    DOTNET_RUNNING_IN_CONTAINER=true \
    DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=false

# Entry point
ENTRYPOINT ["dotnet", "CaixaSeguradora.Api.dll"]
