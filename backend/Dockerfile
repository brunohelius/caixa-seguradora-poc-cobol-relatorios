# Build stage
FROM mcr.microsoft.com/dotnet/sdk:9.0 AS build
WORKDIR /src

# Copy csproj files and restore dependencies
COPY ["src/CaixaSeguradora.Api/CaixaSeguradora.Api.csproj", "src/CaixaSeguradora.Api/"]
COPY ["src/CaixaSeguradora.Core/CaixaSeguradora.Core.csproj", "src/CaixaSeguradora.Core/"]
COPY ["src/CaixaSeguradora.Infrastructure/CaixaSeguradora.Infrastructure.csproj", "src/CaixaSeguradora.Infrastructure/"]
RUN dotnet restore "src/CaixaSeguradora.Api/CaixaSeguradora.Api.csproj"

# Copy everything else and build
COPY . .
WORKDIR "/src/src/CaixaSeguradora.Api"
RUN dotnet build "CaixaSeguradora.Api.csproj" -c Release -o /app/build

# Publish stage
FROM build AS publish
RUN dotnet publish "CaixaSeguradora.Api.csproj" -c Release -o /app/publish /p:UseAppHost=false

# Runtime stage
FROM mcr.microsoft.com/dotnet/aspnet:9.0 AS final
WORKDIR /app
EXPOSE 5000
EXPOSE 5001

# Copy published app
COPY --from=publish /app/publish .

# Create data directory for SQLite
RUN mkdir -p /app/data

ENTRYPOINT ["dotnet", "CaixaSeguradora.Api.dll"]
