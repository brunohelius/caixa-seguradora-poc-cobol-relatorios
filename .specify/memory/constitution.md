# Caixa Seguradora Migration Constitution

## Core Principles

### I. Functional Parity First (NON-NEGOTIABLE)
- Every COBOL section in RG1866B must have a documented C# counterpart with traceability notes.
- All calculations must reproduce legacy COBOL outputs with zero variance; byte-for-byte file parity is mandatory.
- Scope is limited to migration parity—new features are deferred until the legacy replacement is validated.

### II. Clean Architecture Mandatory
- Domain layer remains independent of infrastructure implementations.
- Repositories abstract data access; swapping SQLite (dev) for DB2 (prod) must require no domain changes.
- Services encapsulate business logic; controllers remain thin HTTP adapters.
- Dependency injection is required for all composable components.

### III. Test-Driven Migration (NON-NEGOTIABLE)
- Comparison tests (COBOL vs .NET) guard every migrated business flow.
- Unit tests must cover ≥90 % of calculation services before implementation is considered complete.
- Integration tests validate database access patterns and transaction boundaries.
- End-to-end tests exercise critical report workflows via the public API/UI.

### IV. Data Type Precision
- Financial calculations use `decimal` with explicit precision/scale mappings from COBOL PIC definitions.
- Custom formatters replicate COBOL padding and rounding semantics.
- Data conversion utilities must document rounding modes and overflow handling.

### V. Observability & Traceability
- Structured logging (JSON) is required for all backend services; logs must include correlation IDs and COBOL section references.
- Audit trails capture report requests, parameters, outcomes, and comparison results.
- Performance metrics benchmark .NET processing against COBOL baselines.

## Additional Constraints
- **Technology Stack**: .NET 9 (C#), React 18+, SQLite in development, DB2 compatibility layer for production.
- **Localization**: All user-facing strings, errors, and documentation are presented in Brazilian Portuguese.
- **Compliance**: PREMIT/PREMCED outputs must satisfy SUSEP Circular 360 requirements and pass regulatory audits.
- **Security & Access**: Development databases run in read-only mode for legacy views; secrets are managed via environment configuration and excluded from source control.
- **Deployment**: Containerized workloads (Docker) with parity across development and validation environments.

## Development Workflow & Quality Gates
- Specifications must include both functional and non-functional requirements before implementation begins.
- Plans and task lists reference this constitution and are updated whenever principles change.
- Code reviews verify adherence to parity, testing, and observability requirements before merge.
- No feature is “done” without automated test coverage, comparison validation, and Portuguese localization sign-off.
- Regression gates include performance benchmarks, byte-level comparison suites, and logging/audit verification.

## Governance
- This constitution supersedes conflicting guidance in spec/plan/tasks; deviations require documented amendments.
- Updates to the constitution follow change control: draft → stakeholder review → ratification → version bump.
- Maintain a changelog of amendments and ensure all active workstreams acknowledge the latest version.

**Version**: 1.0.0 | **Ratified**: 2025-02-13 | **Last Amended**: 2025-02-13
