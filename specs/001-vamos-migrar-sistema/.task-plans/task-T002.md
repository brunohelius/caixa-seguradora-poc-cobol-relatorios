# Consolidated Task Plan: T002 Initialize CaixaSeguradora.Api project

## Task Summary
- **ID**: T002
- **Name**: Initialize CaixaSeguradora.Api project with ASP.NET Core Web API 9.0 in `backend/src/CaixaSeguradora.Api/`
- **Status**: ✅ COMPLETED
- **Priority**: Setup Phase Task
- **Parallelizable**: Yes [P]

## Specification
Initialize CaixaSeguradora.Api project with ASP.NET Core Web API 9.0 in `backend/src/CaixaSeguradora.Api/`

## Implementation Summary
The CaixaSeguradora.Api project has been successfully created and configured with the following characteristics:

1. **Target Framework**: .NET 9.0 (as specified in the .csproj file)
2. **Project Type**: ASP.NET Core Web API
3. **Required Packages**: 
   - Swashbuckle (Swagger/OpenAPI)
   - Serilog (logging)
   - AutoMapper (DTO mapping)
   - Entity Framework Core related packages
4. **Project References**:
   - CaixaSeguradora.Core
   - CaixaSeguradora.Infrastructure
5. **Configuration Files**:
   - Program.cs with comprehensive DI setup
   - appsettings.json with connection strings and CORS
   - Controllers, Middleware, and Properties directories

## Technical Implementation Details

### Project Structure
The project is structured as per the specification:
- Located at `backend/src/CaixaSeguradora.Api/`
- Follows Clean Architecture principles with separation of concerns
- Configured for dependency injection and logging

### Key Configuration
1. **Serilog**: Configured for structured logging with console and file outputs
2. **Swagger/OpenAPI**: Configured for API documentation
3. **Entity Framework**: Configured with SQLite and PremiumReportingDbContext
4. **CORS**: Configured for frontend integration (localhost:5173)
5. **JSON Options**: Configured to handle circular references

### Dependencies Registered
- Repository services (Premium, Policy, Product, etc.)
- Business logic services (Premium Calculation, Cossurance, etc.)
- Report generation services
- Dashboard services
- Query services
- Batch job services
- Mock data services

## Verification Results
- ✅ Project builds successfully with .NET 9.0 target
- ✅ All required NuGet packages referenced
- ✅ Project references to Core and Infrastructure projects established
- ✅ Configuration files present and properly configured
- ✅ Basic ASP.NET Core Web API functionality intact

## Files Created/Modified
- `CaixaSeguradora.Api.csproj`: Project file with .NET 9.0 target and required packages
- `Program.cs`: Main application entry point with DI container configuration
- `appsettings.json`: Configuration file with connection strings and settings
- `appsettings.Development.json`: Development-specific configuration
- `Properties/launchSettings.json`: Development server configuration
- `Controllers/`: Default API controllers directory
- `Middleware/`: Custom middleware components
- `CaixaSeguradora.Api.http`: API testing file
- `logs/`: Logging output directory

## Success Criteria Met
- [x] Project targets .NET 9.0 framework
- [x] Project type is ASP.NET Core Web API
- [x] Location is `backend/src/CaixaSeguradora.Api/`
- [x] Required packages added (Swashbuckle, Serilog, AutoMapper)
- [x] Project builds without errors
- [x] Proper integration with Core and Infrastructure layers
- [x] Configuration files properly set up for development

## Implementation Notes
The project was already created and more advanced than just a basic Web API. It includes comprehensive configuration for all layers of the application, including:
- Serilog structured logging configuration
- Entity Framework with SQLite
- CORS configured for frontend integration
- Comprehensive dependency injection setup for all services
- Error handling middleware
- Swagger/OpenAPI documentation