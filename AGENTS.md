# Repository Guidelines

## Project Structure & Module Organization
The solution entry point lives at `backend/CaixaSeguradora.sln`, with runtime code split across `backend/src/CaixaSeguradora.Api`, `.Core`, and `.Infrastructure`. Domain models and services belong in `.Core`, while integrations and persistence adapters go in `.Infrastructure`. API endpoints, configuration, and hosting code stay inside `.Api`. Automated checks reside under `backend/tests`, separated into `CaixaSeguradora.UnitTests`, `.IntegrationTests`, and `.ComparisonTests`. Reference COBOL assets (`RG1866B.cbl`) and legacy parsing utilities are stored at the repository root and under `docs/parser`, alongside detailed analysis reports. Use `specs/` for business requirements and the `frontend/` directory for future UI work; keep unused placeholders minimal and documented.

## Build, Test, and Development Commands
- `dotnet restore backend/CaixaSeguradora.sln` — pulls NuGet dependencies for all projects.
- `dotnet build backend/CaixaSeguradora.sln -c Release` — compiles the API, core, and infrastructure layers.
- `dotnet run --project backend/src/CaixaSeguradora.Api` — launches the ASP.NET Core API with local settings.
- `dotnet test backend/CaixaSeguradora.sln --collect:"XPlat Code Coverage"` — executes the full xUnit suite and captures coverage.
- `bash docs/parser/run-parser.sh` — re-runs the COBOL parser pipeline when updating legacy analysis artifacts.

## Coding Style & Naming Conventions
Target .NET 9 features with nullable reference types enabled. Use four-space indentation, braces on new lines for multi-line blocks, and expression-bodied members only when they improve readability. Apply `PascalCase` to public types and members, `camelCase` to locals, and suffix interfaces with `I`. Keep API endpoints in minimal APIs consistent with existing route naming and prefer dependency injection over static helpers. Ensure configuration values come from `appsettings.Development.json` or environment variables; do not hardcode secrets.

## Testing Guidelines
Write new unit coverage with xUnit and FluentAssertions under the matching project in `backend/tests`. Name test classes after the type under test (e.g., `WeatherForecastServiceTests`) and methods using the pattern `MethodName_State_ExpectedResult`. For integration scenarios, leverage the existing `.IntegrationTests` harness; keep long-running COBOL comparisons in `.ComparisonTests`. Run `dotnet test` before every push, and add `--filter` expressions for targeted debugging. Aim to keep coverage steady by adding tests whenever new public behavior is introduced.

## Commit & Pull Request Guidelines
Follow the conventional commits style visible in history (`feat:`, `fix:`, `chore:`) while keeping summaries concise and, when helpful, in Portuguese to match existing wording. Each commit should encapsulate a single logical change. Pull requests must describe motivation, list the primary modules touched, mention how to reproduce or verify the change (`dotnet test`, parser runs, etc.), and link to Jira or GitHub issues where available. Attach screenshots or API traces for user-facing updates and call out any configuration steps reviewers must perform.
