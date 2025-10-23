# Feature Specification: COBOL RG1866B to .NET 9 React Migration

**Feature Branch**: `001-vamos-migrar-sistema`
**Created**: October 22, 2025
**Status**: Draft
**Legacy System**: COBOL RG1866B - SUSEP Circular 360 Premium Reporting System

## Executive Summary

Migrate the legacy COBOL batch processing program RG1866B (SUSEP Circular 360 Premium Reports) to a modern full-stack application using .NET 9 backend with React frontend. The system generates regulated insurance premium reports for Brazilian insurance regulator (SUSEP), processing policy data, premiums, endorsements, and cossurance information from 26+ database tables.

### Current State
- **Legacy System**: COBOL batch program RG1866B (~5,000 lines)
- **Database**: IBM DB2 with 26+ views/tables
- **Processing**: Batch processing with 4 cursors
- **Output**: 2 text files (PREMIT.TXT, PREMCED.TXT)
- **External Dependencies**: 3 COBOL modules (RE0001S, GE0009S, GE0010S)
- **Data Structures**: 687 data items, 63 sections, 65 paragraphs

### Target State
- **Backend**: .NET 9 Web API with C#
- **Frontend**: React application with dashboard
- **Database**: SQLite local database with mocked DB2 data structure
- **Architecture**: Clean Architecture with RESTful APIs
- **UI/UX**: Caixa Seguradora branding and styling
- **Deployment**: Containerized solution

## User Scenarios & Testing

### User Story 1 - View Migration Dashboard (Priority: P1)

Users need to understand the scope and complexity of the migration project, view current system metrics, and track migration progress through an interactive dashboard.

**Why this priority**: Provides immediate value by visualizing the migration scope, establishing baseline metrics, and creating transparency for stakeholders. Can be developed independently without backend logic migration.

**Independent Test**: Can be fully tested by launching the React application, navigating to the dashboard, and verifying all metrics display correctly with mock data. Delivers value by documenting system complexity and providing project visibility.

**Acceptance Scenarios**:

1. **Given** a user accesses the application, **When** they land on the homepage, **Then** they see a comprehensive dashboard showing system overview (program name, lines of code, creation date), data structure metrics (687 data items, table breakdown), processing complexity (63 sections, 65 paragraphs, 4 cursors), database dependencies (26+ tables with access patterns), and external module integrations (3 modules)

2. **Given** the dashboard is displayed, **When** user views the Function Points section, **Then** they see estimated function points for migration, breakdown by component (backend, frontend, database, integration), complexity ratings (High/Medium/Low), and effort estimates

3. **Given** user is on the dashboard, **When** they interact with visualizations, **Then** they can view interactive charts showing data distribution, drill down into specific sections/paragraphs, see database table relationships, and navigate to detailed analysis reports

---

### User Story 2 - Generate Premium Reports (Interactive) (Priority: P2)

Users need to generate SUSEP Circular 360 premium reports on-demand through a web interface, replacing the batch COBOL process with an interactive workflow that allows parameter selection and real-time execution.

**Why this priority**: Core business functionality that delivers immediate operational value by replacing the batch process with interactive capabilities. Represents the primary use case of the legacy system.

**Independent Test**: Can be tested by configuring report parameters (date range, system ID, report type), executing the generation process, and verifying output files match legacy COBOL output format byte-for-byte.

**Acceptance Scenarios**:

1. **Given** user is logged into the system, **When** they navigate to Report Generation, **Then** they see a form with date range selectors (start date, end date), system selection dropdown, report type selection (PREMIT, PREMCED, or Both), and processing mode (weekly cumulative, monthly)

2. **Given** user has filled the report parameters, **When** they click "Generate Report", **Then** system validates all required parameters, displays processing progress indicator, executes premium calculation logic identical to COBOL, accesses required database tables (V0PREMIOS, V0APOLICE, etc.), and generates output files in correct format

3. **Given** report generation completes successfully, **When** user views results, **Then** they can download PREMIT.TXT file, download PREMCED.TXT file, view generation summary (records processed, execution time), and see any warnings or validation messages

4. **Given** report generation encounters errors, **When** processing fails, **Then** system displays clear error message in Portuguese, logs detailed error information for support, maintains data integrity (no partial writes), and allows user to retry with corrected parameters

---

### User Story 3 - Query and Visualize Premium Data (Priority: P3)

Users need to query premium data interactively, view results in tables and charts, and export data for analysis, providing modern data exploration capabilities not available in the legacy batch system.

**Why this priority**: Adds modern capabilities beyond legacy functionality, enabling better business insights. Builds on core report generation but is not critical for initial operational parity.

**Independent Test**: Can be tested by executing various queries against mocked database, verifying results display correctly in tables and charts, and validating export functionality produces accurate files.

**Acceptance Scenarios**:

1. **Given** user is on the Query screen, **When** they build a query, **Then** they can filter by policy number, product, date range, select specific columns to display, sort results by any column, and apply aggregations (sum, average, count)

2. **Given** query results are displayed, **When** user views the data, **Then** they see paginated table with responsive design, summary statistics at the top, visual charts (bar, line, pie) based on data, and export buttons (CSV, Excel, PDF)

3. **Given** user wants to analyze trends, **When** they create visualizations, **Then** system generates charts from query results, allows customization (chart type, colors, labels), updates charts in real-time as filters change, and saves favorite queries for future use

---

### User Story 4 - Monitor Batch Processing Jobs (Priority: P4)

Users need to schedule and monitor batch report generation jobs, view execution history, and receive notifications about job completion or failures, providing operational visibility for automated processing.

**Why this priority**: Addresses operational requirements for scheduled processing but is not critical for initial MVP. Can be added once interactive generation is stable.

**Independent Test**: Can be tested by creating scheduled jobs, monitoring their execution status, and verifying notification delivery without impacting other system functionality.

**Acceptance Scenarios**:

1. **Given** user wants to automate reports, **When** they create a scheduled job, **Then** they can define job name and description, set recurrence pattern (daily, weekly, monthly), specify report parameters, and configure notification recipients

2. **Given** batch jobs are scheduled, **When** user views job monitor, **Then** they see list of all scheduled jobs, current execution status (Running, Completed, Failed), last execution time and duration, and next scheduled execution time

3. **Given** a batch job completes, **When** results are ready, **Then** system sends email notification to configured recipients, stores output files in designated location, updates job history with execution details, and triggers any configured downstream processes

---

### User Story 5 - Manage Database Mock Data (Priority: P4)

Developers and testers need to manage mock SQLite database data that replicates DB2 structure, load test datasets, and validate data integrity for thorough testing of migration logic.

**Why this priority**: Critical for testing but not user-facing functionality. Required for comprehensive validation but can use minimal datasets initially.

**Independent Test**: Can be tested by loading mock data files, verifying schema matches DB2, querying data, and confirming calculations produce expected results.

**Acceptance Scenarios**:

1. **Given** developer needs test data, **When** they access data management interface, **Then** they can view current SQLite schema, load mock data from CSV/JSON files, validate data against DB2 schema rules, and clear and reset test database

2. **Given** tester wants realistic data, **When** they load production-like dataset, **Then** system validates all foreign key relationships, checks data type compatibility, reports any data quality issues, and confirms record counts match source

3. **Given** migration testing is ongoing, **When** comparing outputs, **Then** developers can run side-by-side comparisons (COBOL vs .NET), generate diff reports highlighting discrepancies, export comparison results for analysis, and flag any business logic deviations

---

### Edge Cases

- **What happens when database query returns zero records?** System displays "No data found for selected parameters" message and allows user to adjust filters without error.

- **How does system handle cursor operations with large datasets?** Implements pagination and streaming results for cursors processing millions of records to prevent memory overflow, matching COBOL's cursor-based processing.

- **What if external module calls (GE0009S, GE0010S) are unavailable?** System displays descriptive error, logs the failure for diagnostics, and allows retry. Admin can configure module endpoints and fallback behavior.

- **How are concurrent users handled during report generation?** System queues requests, provides estimated wait time, and processes reports asynchronously to prevent database locking conflicts.

- **What happens with malformed date inputs?** System validates dates before processing, provides clear error messages in Portuguese ("Data inválida"), and highlights the problematic field.

- **How are DB2-specific data types handled in SQLite?** Mapping layer converts DB2 types (DECIMAL with scale, CHAR fixed-length) to SQLite equivalents, preserving precision and padding rules.

- **What if output file generation fails midway?** Transaction-based file writing ensures atomic operations - either complete file is written or nothing, with rollback capability.

- **How are COBOL numeric computations (PIC 9V99) replicated exactly?** Uses decimal types with exact precision matching, includes rounding mode configuration to replicate COBOL arithmetic behavior.

- **What happens when report parameters conflict (end date before start date)?** Validation layer catches logical errors before processing, returns specific error code and user-friendly message.

- **How is legacy fixed-width file format maintained?** Custom formatter applies padding (spaces for strings, zeros for numbers) matching COBOL WRITE statements exactly, validated by byte comparison.

## Requirements

### Functional Requirements

#### Core Report Generation
- **FR-001**: System MUST generate PREMIT.TXT file with identical layout and content to legacy COBOL output, including all columns, fixed-width formatting, and data precision
- **FR-002**: System MUST generate PREMCED.TXT file for cossurance/ceded premium data with exact legacy format compatibility
- **FR-003**: System MUST process premium records using same business logic as COBOL sections R0500-R5500, ensuring calculation parity
- **FR-004**: System MUST support date range parameters for report generation (initial date, final date) matching legacy WHERE clause logic
- **FR-005**: System MUST validate all input parameters before processing and return descriptive errors in Portuguese

#### Database Integration
- **FR-006**: System MUST connect to SQLite database with schema matching 26+ DB2 views/tables used by legacy COBOL
- **FR-007**: System MUST implement cursor-like processing for large datasets (V0PREMIOS, V0ENDERECOS, V0APOLCOSCED, GE399)
- **FR-008**: System MUST maintain transactional integrity for multi-step operations matching COBOL COMMIT/ROLLBACK behavior
- **FR-009**: System MUST handle all SQL error conditions (duplicate keys, not found, etc.) with equivalent error handling to COBOL SQLCODE checks
- **FR-010**: System MUST support read-only access to all database views without modifying existing data

#### Business Logic Migration
- **FR-011**: System MUST replicate all 687 COBOL data items as C# models with equivalent data types and precision
- **FR-012**: System MUST implement all calculation logic from COBOL sections (premium calculations, accumulations, cossurance)
- **FR-013**: System MUST call equivalent functionality for external modules RE0001S, GE0009S, GE0010S through internal services or APIs
- **FR-014**: System MUST apply all business validation rules encoded in COBOL IF statements (date validations, ramo-specific logic, cancellation checks)
- **FR-015**: System MUST handle endorsement processing (V0ENDOSSO) including cancelled endorsements with same logic as COBOL

#### User Interface
- **FR-016**: System MUST provide dashboard as landing page showing system analysis metrics (sections, data items, tables, complexity)
- **FR-017**: System MUST display function points estimation for migration with breakdown by component
- **FR-018**: System MUST provide report generation interface with parameter selection (dates, systems, report types)
- **FR-019**: System MUST show processing progress indicator during long-running operations
- **FR-020**: System MUST display all error messages and labels in Portuguese (Brazilian)
- **FR-021**: System MUST apply Caixa Seguradora branding (colors, logos, typography) following corporate guidelines from website
- **FR-022**: System MUST be responsive and work on desktop, tablet, and mobile viewports

#### Data Management
- **FR-023**: System MUST provide capability to load mock DB2 data into SQLite from CSV or JSON files
- **FR-024**: System MUST validate mock data schema compatibility with DB2 structure
- **FR-025**: System MUST support export of generated reports in multiple formats (TXT original format, CSV, Excel)
- **FR-026**: System MUST maintain data type precision when converting COBOL PIC formats to C# types (e.g., PIC 9(9)V99 to decimal(11,2))

#### Testing & Validation
- **FR-027**: System MUST provide side-by-side comparison capability between COBOL output and .NET output for validation
- **FR-028**: System MUST log all operations with sufficient detail for debugging and audit (similar to COBOL DISPLAY statements)
- **FR-029**: System MUST generate test reports with sample data to validate business logic migration
- **FR-030**: System MUST include automated unit tests for all critical calculation logic

### Non-Functional Requirements

#### Performance & Scalability
- **NFR-001**: System MUST process 10,000+ premium records in under 5 minutes, matching or improving legacy COBOL throughput.
- **NFR-002**: System MUST deliver API responses under 2 seconds for dashboard endpoints and under 500 ms for standard query endpoints.
- **NFR-003**: System MUST sustain at least 10 concurrent report generations with no more than 20% degradation in processing time.

#### Reliability & Data Integrity
- **NFR-004**: System MUST enforce read-only database access for legacy-mirrored views and block unintended write operations.
- **NFR-005**: System MUST guarantee transactional safeguards so partial failures rollback state consistently across services.

#### Observability & Auditability
- **NFR-006**: System MUST emit structured JSON logs enriched with correlation identifiers and COBOL section references.
- **NFR-007**: System MUST persist audit trails for report requests, parameters, comparison outcomes, and user actions subject to regulatory review.

#### Localization & Compliance
- **NFR-008**: System MUST present all UI text, error messages, and generated user-facing documentation in Brazilian Portuguese.
- **NFR-009**: System MUST maintain byte-level PREMIT/PREMCED compatibility that satisfies SUSEP Circular 360 validation.

#### Deployment & Operations
- **NFR-010**: System MUST build and run inside Docker containers, keeping environment parity between development, validation, and production stages.
- **NFR-011**: System MUST externalize secrets and environment configuration, ensuring sensitive values never reside in source control.

### Key Entities

#### Premium Data
- **Premium Record**: Represents a premium emission record from V0PREMIOS view, including policy number, product code, premium amount, emission date, effective date, status, and calculation components
- **Policy**: Insurance policy master data from V0APOLICE, including policy number, client ID, product, start/end dates, status, and agency information
- **Endorsement**: Policy modification from V0ENDOSSO, including endorsement number, type, date, premium impact, and cancellation status
- **Product**: Insurance product definition from V0PRODUTO/V0PRODUTOSVG, including product code, name, line of business (ramo), SUSEP code, and calculation rules

#### Party & Location
- **Client**: Policyholder or insured party from V0CLIENTE/V0TOMADOR, including client ID, name, document number (CPF/CNPJ), type (person/company)
- **Address**: Location data from V0ENDERECOS, including address components, city, state (UF), postal code, and address type
- **Agency**: Sales agency from V0AGENCIAS, including agency code, name, region, and channel information
- **Producer**: Insurance broker/producer from V0PRODUTOR, including producer code, name, commission rates, and affiliation

#### Coverage & Billing
- **Coverage**: Insurance coverage details from V0COBERAPOL, including coverage code, insured amount (IS), premium calculation basis, rates, and limits
- **Invoice**: Billing information from V0FATURAS, including invoice number, installment details, due dates, payment status, and amounts
- **Installment**: Premium installment from V0HISTOPARC, including parcel number, due date, payment date, amount, and status

#### Cossurance & Reinsurance
- **Cossured Policy**: Cossurance arrangement from V0APOLCOSCED, including ceding/acquiring company, percentage share, and premium distribution
- **Cossurance Calculation**: Cossurance computation data from GE399 table, including quota percentages, retained premium, and ceded amounts
- **Reinsurance Data**: Reinsurance calculations processed through external module RE0001S, including treaty information, retention limits, and ceded premium

## Success Criteria

### Measurable Outcomes

#### Functional Accuracy
- **SC-001**: Generated PREMIT.TXT and PREMCED.TXT files match legacy COBOL output byte-for-byte for identical input datasets (100% accuracy target)
- **SC-002**: All business calculations (premium, cossurance, accumulations) produce identical results to COBOL with zero deviation
- **SC-003**: System processes complete test dataset and generates valid output files in under 5 minutes for datasets with 10,000+ premium records

#### User Experience
- **SC-004**: Users can generate reports through web interface in 30 seconds or less from parameter selection to download (excluding processing time)
- **SC-005**: Dashboard loads and displays all migration metrics within 2 seconds on standard internet connection
- **SC-006**: 95% of report generation requests complete successfully on first attempt without user intervention
- **SC-007**: Error messages provide sufficient clarity that users can self-correct 80% of parameter errors without support

#### Technical Quality
- **SC-008**: System maintains 100% database transaction integrity with zero data corruption during concurrent operations
- **SC-009**: All critical business logic has automated unit test coverage achieving 90%+ code coverage
- **SC-010**: System handles 10 concurrent report generation requests without performance degradation exceeding 20%
- **SC-011**: SQLite mock database supports all 26+ tables with referential integrity constraints matching DB2 structure

#### Migration Validation
- **SC-012**: Side-by-side comparison tests with 100 production samples show 100% output equivalence between COBOL and .NET
- **SC-013**: All 63 COBOL sections have documented equivalent functionality in C# codebase with traceability matrix
- **SC-014**: All 687 COBOL data items have mapped C# models with validated type conversions
- **SC-015**: Performance testing shows .NET system processes equivalent workload within 120% of COBOL execution time (allowing 20% overhead for modernization benefits)

#### Operational Readiness
- **SC-016**: System includes comprehensive logging capturing all operations for 30-day troubleshooting and audit period
- **SC-017**: Documentation covers 100% of migrated business rules with examples and test cases
- **SC-018**: Development team successfully trains 5 business users who can independently generate reports and interpret results
- **SC-019**: System deployed successfully in containerized environment with zero-downtime deployment capability

## Out of Scope

The following items are explicitly excluded from this migration:

### Legacy Systems Not Included
- Migration of external COBOL modules RE0001S, GE0009S, GE0010S (will be called through adapters or mocked)
- Migration of other COBOL programs in the REGISTROS GERAIS system beyond RG1866B
- Integration with production DB2 mainframe database (using SQLite mock only)
- Migration of JCL job scheduling infrastructure

### Advanced Features
- Real-time premium calculation engine (beyond report generation)
- Online policy issuance or endorsement processing
- Integration with external SUSEP submission systems
- Multi-language support (Portuguese Brazilian only)
- Mobile native applications (responsive web only)

### Infrastructure
- Production hosting environment setup
- High availability / disaster recovery infrastructure
- Production security hardening and penetration testing
- Production monitoring and alerting setup

### Data Migration
- Historical data migration from DB2 to production database
- Data cleansing or transformation beyond mock data loading
- Archive strategies for old reports

## Assumptions

### Technical Assumptions
1. **Parser Accuracy**: ProLeap COBOL parser output accurately represents program structure, though business logic will be validated against source code
2. **SQLite Capability**: SQLite database can adequately replicate DB2 functionality for development and testing, including complex joins and cursor operations
3. **External Module Behavior**: External COBOL modules (RE0001S, GE0009S, GE0010S) can be adequately mocked or replaced with equivalent C# implementations
4. **Development Environment**: Team has access to .NET 9 SDK, Node.js 20+, and standard development tools
5. **Browser Compatibility**: Modern browsers (Chrome 120+, Firefox 120+, Edge 120+, Safari 17+) will be supported

### Business Assumptions
1. **Report Format Stability**: SUSEP Circular 360 report layouts remain stable during migration period
2. **Business Rules Documentation**: COBOL comments and header documentation accurately describe business intent
3. **Test Data Availability**: Sufficient representative test data can be obtained or generated for validation
4. **User Availability**: Subject matter experts will be available for business logic clarification during migration
5. **Timeline Flexibility**: Migration can proceed iteratively with user story prioritization allowing early feedback

## Dependencies

### External Dependencies
- **COBOL Source Code**: Complete, readable RG1866B.cbl source file (available)
- **DB2 Schema Documentation**: Database schema definitions for all 26+ tables/views
- **External Module Interfaces**: Interface contracts for RE0001S, GE0009S, GE0010S modules
- **Sample Data**: Representative datasets from production for testing and validation
- **Business Logic Documentation**: Any existing documentation of business rules and calculations
- **Caixa Seguradora Branding**: Corporate style guide, logos, color palette from website

### Technical Dependencies
- **.NET 9 SDK**: Backend framework for API development
- **React 18+**: Frontend framework for user interface
- **SQLite**: Local development database
- **Entity Framework Core**: ORM for database access
- **Serilog**: Logging framework
- **xUnit or MSTest**: Unit testing framework

## Constraints

### Technical Constraints
- **Legacy Compatibility**: Output files must maintain byte-level compatibility with legacy format for regulatory compliance
- **Precision Requirements**: Financial calculations must maintain exact decimal precision matching COBOL arithmetic
- **Database Limitations**: SQLite used for development has limitations vs. production DB2 (no stored procedures, limited concurrency)
- **Browser Requirements**: Must work in standard browsers without plugins or special configurations

### Regulatory Constraints
- **SUSEP Compliance**: Report format and content must comply with SUSEP Circular 360 regulations
- **Data Accuracy**: Insurance premium calculations must be auditable and match regulatory requirements
- **Report Retention**: Generated reports must be stored for regulatory retention period (defined by SUSEP)

### Business Constraints
- **Zero Downtime Requirement**: Migration must not disrupt ongoing report generation capabilities
- **Parallel Operation**: New system must run in parallel with legacy for validation period
- **User Acceptance**: Business users must approve migration before legacy decommission

## Migration Strategy

### Phase 1: Foundation (Weeks 1-2)
**Goal**: Establish project structure and baseline understanding

**Activities**:
1. Analyze parser output and COBOL source code comprehensively
2. Create detailed data dictionary mapping all 687 data items to C# types
3. Extract and document all business rules from COBOL IF/PERFORM logic
4. Set up .NET 9 Web API project structure with Clean Architecture
5. Initialize React project with Caixa Seguradora branding
6. Create SQLite database schema matching 26+ DB2 tables
7. Implement dashboard (User Story 1) with migration metrics

**Deliverables**:
- Data dictionary (COBOL to C# type mappings)
- Business rules documentation
- Project scaffolding (backend + frontend)
- Dashboard showing system complexity

### Phase 2: Core Backend (Weeks 3-5)
**Goal**: Implement backend services with business logic

**Activities**:
1. Create C# entity models for all key database tables
2. Implement repository pattern with Entity Framework Core
3. Migrate premium processing logic (sections R0500-R0700)
4. Implement policy and product data access (sections R0720-R0990)
5. Create calculation services for premium accumulation (R1300)
6. Implement cossurance processing logic (sections R3000-R5500)
7. Develop API endpoints for report generation
8. Unit test all business logic with known test cases

**Deliverables**:
- Backend API with core business logic
- Unit tests for calculations
- API documentation (Swagger)

### Phase 3: Report Generation (Weeks 6-7)
**Goal**: Implement end-to-end report generation

**Activities**:
1. Implement file generation services (PREMIT, PREMCED)
2. Create fixed-width file formatters matching COBOL output
3. Implement cursor-like batch processing for large datasets
4. Develop transaction management for data integrity
5. Create React report generation UI (User Story 2)
6. Implement error handling and validation
7. Load representative test data into SQLite
8. Execute side-by-side comparison tests

**Deliverables**:
- Working report generation (interactive)
- Test results comparing COBOL vs .NET output
- Report generation UI

### Phase 4: Validation & Enhancement (Weeks 8-9)
**Goal**: Validate accuracy and add value-added features

**Activities**:
1. Run comprehensive validation with 100+ test scenarios
2. Implement query and visualization features (User Story 3)
3. Add batch job monitoring (User Story 4)
4. Create data management utilities (User Story 5)
5. Conduct user acceptance testing
6. Performance optimization
7. Documentation completion

**Deliverables**:
- Validation report (COBOL parity achieved)
- Enhanced UI features
- Complete user and technical documentation

## References

### Documentation
- **Parser Analysis**: `/docs/parser/FINAL-ANALYSIS-REPORT.md` - Complete COBOL program structure analysis
- **Detailed Structure**: `/docs/parser/detailed-structure.txt` - Section and paragraph breakdown
- **Parser Index**: `/docs/parser/INDEX.md` - Navigation guide to all parser documentation

### Legacy System
- **COBOL Source**: `/RG1866B.cbl` - Original COBOL program source code
- **Program ID**: RG1866B - REGISTROS GERAIS system
- **Function**: SUSEP Circular 360 - Premium emission reports (PREMIT.TXT, PREMCED.TXT)
- **Created**: May 21, 2014 by Wellington F R C Veras

### External Resources
- **SUSEP Circular 360**: Brazilian insurance regulator reporting requirements
- **Caixa Seguradora Website**: https://www.caixaseguradora.com.br/ - Corporate branding reference
- **Dashboard Reference**: https://sicoob-sge3-jv1x.vercel.app/dashboard - UI inspiration

## Notes

### Critical Implementation Considerations

1. **Decimal Precision**: COBOL PIC fields use exact decimal precision. C# decimal type MUST be used (not double/float) for all financial calculations to avoid rounding errors that would cause regulatory non-compliance.

2. **Fixed-Width Formatting**: COBOL writes fixed-width records with space-padding for strings and zero-padding for numbers. Custom formatters required to replicate exact byte layout.

3. **Cursor Processing**: COBOL uses 4 database cursors for sequential processing. .NET implementation should use streaming/pagination to handle large datasets without loading all into memory.

4. **Date Handling**: COBOL date validations are complex (comparing proposal date vs. effective date by ramo). All date logic must be carefully migrated with timezone considerations.

5. **External Module Calls**: COBOL calls RE0001S, GE0009S, GE0010S with specific parameter structures. These must be mocked initially or replaced with equivalent C# services.

6. **Transaction Semantics**: COBOL has explicit COMMIT points. .NET must replicate same transactional boundaries to maintain data integrity.

### Known Limitations

1. **SQLite vs. DB2**: SQLite lacks some DB2 features (no stored procedures, limited concurrency, different date functions). Production deployment would require migration to SQL Server, PostgreSQL, or actual DB2.

2. **Parser Limitations**: While ProLeap parser successfully extracted structure, some complex COBOL idioms may require manual interpretation from source code.

3. **External Module Availability**: If external COBOL modules source code is unavailable, their functionality must be reverse-engineered from call parameters and expected behaviors.
