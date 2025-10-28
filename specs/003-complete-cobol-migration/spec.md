# Feature Specification: Complete COBOL RG1866B Functional Migration

**Feature Branch**: `003-complete-cobol-migration`
**Created**: October 27, 2025
**Status**: Draft
**Input**: User description: "realize toda a migracao do sistema cobol para o sistema .net com react. Remova a dashboard pois nao faz sentido o que foi criado e migre as funcionaldiades de fato do legado para a versao nova, atualizada. Verifique se temos gaps de implementacao ainda e finalize tudo. Temos ja a documentacao gerada, valide e verifique e o sistema legado cobol. Migre o sistema 100% e deixe ele 100% funcional"

## Executive Summary

Complete the functional migration of COBOL RG1866B batch program to .NET 9 + React, removing non-functional dashboard elements and implementing the **actual SUSEP Circular 360 reporting capabilities**. This specification addresses all remaining implementation gaps to achieve a production-ready system that can completely replace the mainframe batch job.

### Key Principles
1. **Function-First**: Implement only what COBOL actually does
2. **Compliance-Critical**: Maintain byte-for-byte SUSEP output compatibility
3. **Production-Ready**: Build system capable of replacing mainframe completely
4. **Gap Closure**: Eliminate all remaining implementation gaps

### Gap Analysis Summary

**✅ Documented (from previous work)**:
- Complete COBOL analysis (5,046 lines)
- Data structure mapping (687 variables)
- Database schema (26+ tables)
- Business rules catalog (35+ change requests)

**❌ Not Yet Implemented**:
- Premium calculation engine (sections R0700-R1300)
- Cossurance processing (sections R3000-R5500)
- Fixed-width file generation (PREMIT/PREMCED)
- External module integration (RE0001S, GE0009S, GE0010S)
- Cursor-based batch processing
- Complete business rule validations

## User Scenarios & Testing

### User Story 1 - Generate Monthly SUSEP Reports (Priority: P1)

Operations team needs to generate regulatory PREMIT and PREMCED reports by providing month/year parameter and receiving output files that match legacy COBOL format exactly for SUSEP submission.

**Why this priority**: Core regulatory function - company cannot meet SUSEP obligations without this. This is the sole purpose of RG1866B.

**Independent Test**: Provide reference month "202510", execute generation, compare output byte-for-byte with COBOL reference, verify SUSEP validation accepts files.

**Acceptance Scenarios**:

1. **Given** October 2025 premium data exists in database, **When** user executes report with parameter "202510", **Then** system processes all premium records for October 2025, applies COBOL business rules exactly, generates PREMIT.TXT with 1200-byte fixed records, generates PREMCED.TXT with 800-byte fixed records, completes in under 5 minutes for 10,000+ records.

2. **Given** report generation completes successfully, **When** user downloads files, **Then** PREMIT.TXT matches COBOL output byte-for-byte, PREMCED.TXT matches cossurance format exactly, files pass SUSEP validation, user sees "Relatórios gerados com sucesso - 10.243 registros" confirmation.

3. **Given** invalid month parameter provided, **When** validation runs, **Then** system rejects with Portuguese error "Período inválido: mês deve estar entre 201401 e [current month]", prevents processing, logs validation failure.

---

### User Story 2 - Execute Premium Calculations (Priority: P1)

System must calculate premium breakdowns, commissions, endorsement impacts, and cossurance allocations with exact COBOL arithmetic parity to ensure regulatory compliance.

**Why this priority**: Calculation accuracy is non-negotiable - any deviation causes SUSEP rejection and potential R$ 1M+ fines.

**Independent Test**: Load golden dataset with known outputs, execute calculations, compare 687 fields, validate zero financial deviation.

**Acceptance Scenarios**:

1. **Given** premium for ramo 0541 (auto), **When** system calculates, **Then** applies commission rates correctly, calculates IOF with COBOL formula, computes net premium with decimal precision, handles installment surcharge, matches COBOL to centavo.

2. **Given** endorsement type 104 (majoração), **When** processing, **Then** retrieves original premium, calculates increment, applies pro-rata for remaining term, updates accumulators, generates correct movement code.

3. **Given** policy with 60% cossurance retention, **When** processing, **Then** calculates retained 40%, computes ceded 60%, allocates commission proportionally, creates PREMCED record, ensures total equals 100%.

---

### User Story 3 - Apply Business Validation Rules (Priority: P1)

System must implement all COBOL business rules including ramo-specific validations, date checks, quantity constraints, and data quality controls from 8 years of maintenance.

**Why this priority**: Rules ensure data quality and compliance - missing any creates regulatory risk.

**Independent Test**: Create test cases for each documented rule, execute validations, verify error handling matches expectations.

**Acceptance Scenarios**:

1. **Given** proposal date exceeds effective date for ramo 0167, **When** validating, **Then** auto-adjusts proposal date to effective date per SUSEP rule, logs adjustment with reason, continues processing, generates valid output.

2. **Given** grupo ramo 09 with zero bilhete number, **When** validating, **Then** rejects record with error "Ramo 09 requer número de bilhete", logs rejection, excludes from output, continues with remaining records.

3. **Given** insured quantity is zero, **When** processing coverage, **Then** auto-sets to 1 (minimum), logs correction with policy number, uses corrected value in calculations.

---

### User Story 4 - Generate Fixed-Width Output Files (Priority: P1)

System must produce PREMIT and PREMCED files with exact fixed-width formatting including zero-padding for numerics, space-padding for strings, proper decimal handling.

**Why this priority**: SUSEP validation is byte-sensitive - single misaligned byte causes complete rejection.

**Independent Test**: Generate from test data, compare each byte with COBOL reference, validate exact 1200/800 byte lengths, verify SUSEP test environment acceptance.

**Acceptance Scenarios**:

1. **Given** premium R$ 1,234.56, **When** writing to file, **Then** formats as "000000000123456" (15 digits, implied 2 decimals), left-pads with zeros, handles negatives with minus sign, maintains exact byte position per SUSEP layout.

2. **Given** policy "ABC123", **When** writing alphanumeric, **Then** right-pads spaces to 20 characters, converts to uppercase if required, maintains character encoding, prevents truncation.

3. **Given** complete premium record, **When** generating line, **Then** produces exactly 1200 bytes (PREMIT) or 800 bytes (PREMCED), validates no overflow/underflow, ensures alignment, writes with single newline.

---

### User Story 5 - Process Large Datasets Efficiently (Priority: P2)

System must handle production volumes (10,000-15,000 records) using cursor-based processing to prevent memory overflow while maintaining throughput.

**Why this priority**: Production scale requires streaming - full table loads cause OutOfMemoryException.

**Independent Test**: Load 15,000 record dataset, execute cursor processing, monitor memory under 2GB, verify completion without timeout, validate output completeness.

**Acceptance Scenarios**:

1. **Given** 12,000 premiums for month, **When** opening main cursor, **Then** fetches in 1000-record batches, processes sequentially, releases memory per batch, maintains cursor position, completes without memory errors.

2. **Given** address lookup needed, **When** opening secondary cursor, **Then** queries for specific client, fetches multiple if present, prioritizes by type, closes after use, doesn't interfere with main cursor.

3. **Given** 15,000 records over 5 minutes, **When** monitoring resources, **Then** memory stable (no leaks), CPU under 80%, connection pool constant, no cursor leaks, all properly disposed.

---

### User Story 6 - Integrate External Services (Priority: P2)

System must provide equivalents for COBOL modules RE0001S (reinsurance), GE0009S (formatting), GE0010S (validation) through internal implementations or service calls.

**Why this priority**: Specialized calculations critical for certain products - missing blocks complete migration.

**Independent Test**: Identify policies using each module, create test cases with known outputs, execute equivalents, compare results, validate edge cases.

**Acceptance Scenarios**:

1. **Given** reinsurance calculation needed, **When** calling service, **Then** passes policy number, date, premium, product code, receives reinsurance amount, percentage, treaty code, return code, handles unavailability gracefully, logs interactions.

2. **Given** CPF/CNPJ formatting needed, **When** processing document, **Then** validates format (11 or 14 digits), applies check digit validation, formats with separators (###.###.###-##), handles errors with fallback, includes formatted value in output.

3. **Given** external service unavailable, **When** attempting call, **Then** retries 3 times with exponential backoff, logs attempts, provides fallback if configured, fails gracefully with clear error, allows manual retry.

---

### User Story 7 - Provide Simple Report Interface (Priority: P3)

Users need minimal web interface to trigger report generation, monitor status, download files - replacing mainframe JCL submission.

**Why this priority**: Basic UI removes mainframe dependency but command-line is acceptable initially.

**Independent Test**: Launch web app, submit request through form, monitor via status page, download files - all without backend changes.

**Acceptance Scenarios**:

1. **Given** user accesses application, **When** navigating to report page, **Then** sees month/year selector, report type selection (PREMIT/PREMCED/Both), execution mode, submit button, Portuguese descriptions.

2. **Given** user submits request, **When** processing, **Then** displays "Processando..." with spinner, shows progress updates every 10 seconds, prevents duplicate submission, maintains responsive UI.

3. **Given** generation completes, **When** viewing results, **Then** shows summary (records processed, execution time, file sizes), download buttons, processing log link, Portuguese success message.

---

### Edge Cases

- **Database connection lost mid-processing?** Detects failure, rolls back uncommitted transactions, closes cursors gracefully, logs interruption point, returns error allowing restart from last commit.

- **Concurrent execution of same month?** Implements file-based locking, rejects concurrent with "Relatório já em processamento" message, provides estimated completion time, releases lock after 60-minute timeout.

- **Division by zero in calculations?** Catches arithmetic exceptions, logs policy and context, applies safe default (skip or zero), continues remaining records, includes warning in summary.

- **SQLite vs DB2 SQL differences?** Uses abstraction layer translating DB2 syntax (DECIMAL, DATE formats, WITH UR), handles function equivalents, validates compatibility during development, documents differences.

- **File system full during write?** Detects write failure immediately, rolls back entire file write (no partial), displays "Espaço em disco insuficiente", cleans temporary files, allows retry after space freed.

- **COMP-3 precision loss?** Maps COMP-3 to C# decimal with exact PIC matching (9(13)V99 → decimal(15,2)), validates no loss in tests, documents rounding differences, uses COBOL-compatible rounding.

- **Zero records for valid month?** Recognizes as valid scenario, generates empty files with headers, logs informational message, returns RC=0004 (warning), allows SUSEP transmission if required.

- **Missing foreign key relationships?** Tolerates missing related records (policy without client), uses NULL or defaults for missing fields, logs data quality warnings, continues without fatal error, generates quality report.

- **Cossurance percentages don't sum to 100%?** Validates total percentage, adjusts largest participation if difference < 1%, rejects if difference > 1%, logs adjustment with policy number, alerts operator.

- **Year-end processing spanning years?** Handles December → January correctly, manages fiscal vs calendar year differences, validates date sequences across boundary, applies correct year to derived dates, prevents duplication.

## Requirements

### Functional Requirements

#### Core Report Generation
- **FR-001**: System MUST generate PREMIT.TXT with exactly 1200-byte fixed-width records matching COBOL layout
- **FR-002**: System MUST generate PREMCED.TXT with exactly 800-byte fixed-width records for cossurance
- **FR-003**: System MUST accept YYYYMM month parameter and process all premiums for that period
- **FR-004**: System MUST validate parameters and reject invalid months with Portuguese errors
- **FR-005**: System MUST process 10,000+ records in under 5 minutes

#### Database Integration
- **FR-006**: System MUST query V0PREMIOS using cursor-based fetching to prevent memory overflow
- **FR-007**: System MUST join 26+ tables/views as COBOL does (V0APOLICE, V0PRODUTO, V0CLIENTE, etc.)
- **FR-008**: System MUST implement nested cursors for V0ENDERECOS, V0APOLCOSCED, GE399
- **FR-009**: System MUST maintain read-only access without writing to legacy tables
- **FR-010**: System MUST handle SQL error codes equivalent to COBOL SQLCODE checks

#### Calculation Engine
- **FR-011**: System MUST replicate COBOL COMP-3 arithmetic using C# decimal with exact precision
- **FR-012**: System MUST implement premium calculations from sections R0700-R1300 identically
- **FR-013**: System MUST compute cossurance allocations from sections R3000-R5500 with zero deviation
- **FR-014**: System MUST calculate commissions (corretagem, agenciamento, administração) per COBOL formulas
- **FR-015**: System MUST handle all endorsement types (101-106) with movement-specific rules

#### Business Rules
- **FR-016**: System MUST validate proposal vs effective dates for ramos 0167, 0860, 0870, 0993, 1061, 1065, 1068
- **FR-017**: System MUST reject grupo ramo 09 without bilhete number as COBOL does
- **FR-018**: System MUST enforce minimum quantity of 1 for insured lives when zero/negative
- **FR-019**: System MUST retrieve SUSEP process numbers for products 1803/1804/1805 with specific ramos
- **FR-020**: System MUST apply all 35+ business rule changes documented in COBOL comments

#### File Generation
- **FR-021**: System MUST pad numeric fields left with zeros to specified width
- **FR-022**: System MUST pad alphanumeric fields right with spaces to fixed width
- **FR-023**: System MUST format dates as YYYYMMDD consistently
- **FR-024**: System MUST represent decimals with implied decimal point (no explicit separator)
- **FR-025**: System MUST generate one output line per record with Unix line endings

#### External Integration
- **FR-026**: System MUST provide reinsurance calculation equivalent to RE0001S
- **FR-027**: System MUST provide formatting functions equivalent to GE0009S (CPF/CNPJ, dates)
- **FR-028**: System MUST provide validation functions equivalent to GE0010S
- **FR-029**: System MUST handle external service failures with retries and fallbacks
- **FR-030**: System MUST log all external service calls with inputs and outputs

#### Transaction Management
- **FR-031**: System MUST implement transaction boundaries matching COBOL COMMIT points
- **FR-032**: System MUST rollback changes if processing fails mid-execution
- **FR-033**: System MUST ensure atomic file writes (complete file or nothing)
- **FR-034**: System MUST maintain database consistency if application crashes
- **FR-035**: System MUST support manual restart after failure without duplication

#### Error Handling
- **FR-036**: System MUST return standard return codes (0000/0004/0008/0012) matching COBOL
- **FR-037**: System MUST log errors with policy number, section name, COBOL paragraph reference
- **FR-038**: System MUST display all user messages in Brazilian Portuguese
- **FR-039**: System MUST maintain audit trail of executions with timestamps and parameters
- **FR-040**: System MUST generate execution summary showing records processed, warnings, errors

#### Web Interface
- **FR-041**: System MUST provide web form for report generation with month selection
- **FR-042**: System MUST display processing status with real-time updates
- **FR-043**: System MUST allow download of generated PREMIT and PREMCED files
- **FR-044**: System MUST show execution history with past report runs
- **FR-045**: System MUST provide access to execution logs through web interface

### Key Entities

#### Premium Processing Domain
- **Premium Record**: Core entity from V0PREMIOS with policy number, endorsement number, product code, premium amounts (bruto, líquido, IOF, adicional), dates (emissão, vigência inicial/final), client ID, operation type, movement code (101-106)
- **Policy**: Master data from V0APOLICE with policy number, client ID, product, effective dates, status, agency, producer, policy-level attributes
- **Endorsement**: Modification from V0ENDOSSO with endorsement number, type (majoração, redução, cancelamento), effective date, premium impact, cancellation indicators
- **Product**: Definition from V0PRODUTO with product code, name, ramo SUSEP, grupo ramo, product-specific calculation rules

#### Party & Geography Domain
- **Client**: Insured party from V0CLIENTE/V0TOMADOR with client ID, name, document (CPF/CNPJ), type (person/company), demographic data
- **Address**: Location from V0ENDERECOS with street, number, complement, neighborhood, city, state (UF), postal code, address type
- **Agency**: Distribution channel from V0AGENCIAS with agency code, name, region, commission structure
- **Producer**: Broker from V0PRODUTOR with producer code, name, commission rates, registration numbers

#### Coverage & Billing Domain
- **Coverage**: Insurance coverage from V0COBERAPOL with coverage code, insured sum (IS), premium components (base, tariff, liquid), rates, limits
- **Invoice**: Billing document from V0FATURAS with invoice number, total amount, issue date, due dates, payment status
- **Installment**: Payment installment from V0HISTOPARC with parcel number, amount, due date, payment date, status

#### Cossurance Domain
- **Cossured Policy**: Arrangement from V0APOLCOSCED linking policy to cossuring companies with participation percentages, cession type (cedido/obtido), amount allocations
- **Cossurance Calculation**: Detailed calculation from GE399 with quota percentages, retained premium, ceded premium, cossurer company codes
- **Reinsurance Data**: Treaty information from RE0001S with retained amounts, ceded amounts, treaty codes, reinsurer identities

#### Operational Domain
- **Report Execution**: Metadata with execution ID, month parameter, start/end timestamps, record counts, status, return code, triggering user
- **Processing Log**: Log entries with timestamp, severity, policy number (if applicable), COBOL section reference, message text
- **File Output**: Generated file metadata with filename, size, checksum, generation timestamp, download count

## Success Criteria

### Measurable Outcomes

#### Functional Accuracy (Non-Negotiable)
- **SC-001**: PREMIT.TXT matches COBOL byte-for-byte for 100 production test cases with zero deviations
- **SC-002**: PREMCED.TXT matches COBOL cossurance byte-for-byte for 100 production test cases
- **SC-003**: Financial calculations within R$ 0.01 of COBOL across 10,000+ test records
- **SC-004**: Premium engine produces identical results for all 40+ ramo SUSEP codes
- **SC-005**: SUSEP test environment accepts files without errors for 20 consecutive submissions

#### Performance (Production Readiness)
- **SC-006**: Processes 10,000 records in under 5 minutes (300 seconds) on standard hardware
- **SC-007**: Processes 15,000 records (peak volume) in under 7 minutes (420 seconds)
- **SC-008**: Memory usage under 2GB throughout processing of largest dataset
- **SC-009**: Database query performance equivalent or better than COBOL DB2 queries
- **SC-010**: Handles 5 concurrent executions without degradation exceeding 20%

#### Data Integrity
- **SC-011**: Zero data corruption across 1,000 test executions including failures
- **SC-012**: Successfully recovers from database connection loss and resumes
- **SC-013**: File writes are atomic - no partial files during failures
- **SC-014**: Transaction rollback works in 100% of simulated failure scenarios
- **SC-015**: No records lost, duplicated, or corrupted during processing

#### Business Rule Coverage
- **SC-016**: All 63 COBOL sections have documented and tested .NET equivalents with traceability
- **SC-017**: All 687 COBOL data items mapped to C# models with type validation tests
- **SC-018**: All 35+ documented COBOL changes have equivalent functionality in .NET
- **SC-019**: All ramo-specific business rules have unit tests demonstrating correctness
- **SC-020**: Date validation logic handles all documented edge cases correctly

#### Error Handling
- **SC-021**: Clear Portuguese error messages for 100% of user-facing errors
- **SC-022**: Error logs allow support to diagnose 95% of issues without developer assistance
- **SC-023**: Returns correct RC codes (0000/0004/0008/0012) for all scenarios
- **SC-024**: No unhandled exceptions during stress testing with 100,000+ test cases
- **SC-025**: Logs all critical operations with timestamps, policy numbers, context for audit

#### Operational Readiness
- **SC-026**: User generates report through web interface in under 2 minutes (excluding processing)
- **SC-027**: Operations monitors execution status real-time through web interface
- **SC-028**: Support accesses execution logs and troubleshoots without developer assistance
- **SC-029**: Runs successfully in Docker container with no configuration issues
- **SC-030**: Complete documentation covers setup, execution, troubleshooting, maintenance

#### Migration Validation
- **SC-031**: Business users validate accuracy for 50 production months and approve migration
- **SC-032**: Parallel run (COBOL + .NET) for 3 months shows identical results
- **SC-033**: SUSEP accepts production reports from .NET for 3 months without rejections
- **SC-034**: Executes production workload with zero critical defects for 90-day observation
- **SC-035**: Legacy COBOL decommissioned with IT governance board approval

## Out of Scope

### Dashboard & Analytics (To Be Removed)
- Migration dashboard showing complexity metrics (already created, will be removed)
- Function point estimation visualizations (not part of COBOL)
- Interactive data exploration and query tools (COBOL is batch-only)
- Real-time monitoring dashboards with charts (not in legacy)
- Any UI beyond simple report generation form

### Enhanced Features Not in COBOL
- Online policy issuance or endorsement entry (COBOL only reports)
- Real-time premium calculator for quotes (not part of RG1866B)
- External SUSEP submission API integration (COBOL generates files only)
- Email notifications when reports complete (COBOL relies on job scheduler)
- Advanced search and filtering of historical data (not in COBOL)

### Infrastructure & Operations
- Production hosting and deployment infrastructure (development scope only)
- High availability and disaster recovery setup (operational concern)
- Production monitoring and alerting configuration (operations responsibility)
- Backup and archive strategies (IT operations)
- Performance tuning beyond meeting 5-minute SLA

### Data Migration & Integration
- Historical data migration from DB2 to new database (data team)
- Production DB2 mainframe integration (using SQLite mock only)
- Other COBOL programs in REGISTROS GERAIS (RG1866B only)
- Replacement of external modules with production services (mocked initially)

### Security & Compliance
- Production security hardening and penetration testing (security team)
- User authentication and authorization (single-user MVP acceptable)
- Encryption of data at rest and in transit (production requirement)
- LGPD compliance audit (legal/compliance team)
- Production audit logging beyond basic execution trail

## Assumptions

### Technical Assumptions
1. COBOL source accurately represents production behavior with critical rules in code/comments
2. Existing documentation (LEGACY_SYSTEM_DOCUMENTATION.md) accurately describes COBOL functionality
3. SQLite adequately replicates DB2 for development including joins, cursors, transactions
4. C# decimal provides sufficient precision to match COBOL COMP-3 packed decimal
5. External modules can be mocked or replaced with equivalent C# implementations

### Business Assumptions
1. SUSEP Circular 360 report format remains stable during migration
2. Representative production data can be obtained or realistically generated
3. No new business rules or calculation changes during migration
4. Business users will validate accuracy and approve cutover
5. Legacy COBOL can run in parallel for 3-month validation period

### Data Assumptions
1. Source database data has sufficient quality for processing (no cleanup required)
2. Monthly premium volume remains within 10,000-15,000 records
3. Reference data (products, ramos, agencies) remains stable
4. Past COBOL outputs available for comparison (at least 12 months)

### Operational Assumptions
1. Team has access to .NET 9 SDK, database tools, standard development environment
2. Docker containerization is acceptable deployment target
3. Development team provides Level 3 support during 90-day stabilization
4. Migration proceeds iteratively with validation gates before cutover

## Dependencies

### External Dependencies
- **COBOL Source Code**: Complete RG1866B.cbl (5,046 lines) - **Available**
- **DB2 Schema Definition**: Complete DDL for 26+ views/tables - **Partially documented**
- **Production Test Data**: Representative datasets for validation - **Needs extraction**
- **COBOL Reference Outputs**: Known-good PREMIT/PREMCED files - **Needs collection**
- **SUSEP Validation Tool**: Access to SUSEP test environment - **Unknown**

### Technical Dependencies
- **.NET 9 SDK**: Backend framework - **Available**
- **Entity Framework Core 9**: Database ORM - **Available**
- **React 18+**: Minimal web interface - **Available**
- **SQLite**: Local development database - **Available**
- **xUnit**: Unit testing framework - **Available**

### Business Dependencies
- **Business SME Availability**: Subject matter experts for clarification - **Required**
- **SUSEP Documentation**: Complete Circular 360 specifications - **Needs research**
- **Operations Team**: Coordination for production deployment - **Required**
- **Compliance Approval**: Sign-off from regulatory team - **Required**

## Constraints

### Regulatory Constraints (Non-Negotiable)
- Output format must match SUSEP Circular 360 byte-exact
- Financial calculations must be auditable and match COBOL to centavo
- Generated reports must be retained for SUSEP-mandated period (typically 5 years)
- All processing must be logged for regulatory audit

### Technical Constraints
- Must maintain byte-level output compatibility with legacy format
- Must use decimal arithmetic (no floating point) for financial calculations
- SQLite limitations (no stored procedures, limited concurrency) acceptable for MVP
- Must work in modern browsers (Chrome, Firefox, Edge) without special plugins

### Business Constraints
- Migration cannot disrupt monthly SUSEP reporting capability
- Must run in parallel with legacy for validation before cutover
- Must match or exceed COBOL performance (5 minutes for 10K records)
- Requires business sign-off before decommissioning COBOL

### Timeline Constraints
- SUSEP reports due by 15th business day of following month (cannot miss)
- Minimum 3-month parallel run required before production cutover
- Development team provides direct support during 90-day stabilization

## References

### Documentation
- **COBOL Source**: `/RG1866B.cbl` (5,046 lines)
- **Legacy Documentation**: `/docs/LEGACY_SYSTEM_DOCUMENTATION.md`
- **Previous Spec**: `/specs/001-vamos-migrar-sistema/spec.md` (to be superseded)

### External Resources
- **SUSEP Circular 360**: Brazilian insurance regulator reporting requirements
- **Caixa Seguradora**: https://www.caixaseguradora.com.br/
