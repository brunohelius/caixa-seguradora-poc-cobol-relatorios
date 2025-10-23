# User Acceptance Testing (UAT) Plan

**Project**: COBOL RG1866B to .NET Migration - SUSEP Circular 360 Premium Reporting
**UAT Phase**: Pre-Production Validation
**Date**: October 23, 2025

---

## Executive Summary

This User Acceptance Testing (UAT) plan defines the approach, scope, participants, test scenarios, and acceptance criteria for validating that the migrated .NET system meets business requirements and is ready for production deployment.

**UAT Objectives**:
1. Validate all 5 user stories function correctly from business user perspective
2. Confirm system meets SUSEP Circular 360 regulatory requirements
3. Verify system performance meets operational needs
4. Ensure data accuracy and integrity
5. Validate Portuguese language and Caixa Seguradora branding

**Timeline**: 2 weeks (10 business days)

---

## UAT Participants

### Business Stakeholders
- **Business Sponsor**: Approves UAT completion and production deployment
- **Subject Matter Experts (SMEs)**: 3-5 users familiar with COBOL system
- **Compliance Officer**: Validates SUSEP regulatory compliance
- **Operations Manager**: Validates operational procedures

### Technical Team
- **QA Lead**: Coordinates UAT execution, tracks defects
- **Technical Lead**: Provides technical support, troubleshooting
- **DevOps Engineer**: Manages UAT environment
- **Business Analyst**: Documents feedback and requirements

---

## UAT Environment

### Environment Specifications

**Backend API**:
- URL: `https://uat.caixaseguradora.com.br/api`
- Version: 1.0.0-uat
- Database: PostgreSQL with anonymized production data snapshot

**Frontend**:
- URL: `https://uat.caixaseguradora.com.br`
- Version: 1.0.0-uat
- Browser support: Chrome 120+, Firefox 115+, Safari 17+, Edge 120+

**Test Data**:
- 50,000 premium records (October 2024 - September 2025)
- 10,000 policy records
- 5,000 client records
- All data anonymized per LGPD requirements

### Access Credentials

Test user accounts (will be provided separately):
- `uat.admin` - Full administrator access
- `uat.operator` - Report generation and query access
- `uat.viewer` - Read-only access

---

## UAT Test Scenarios

### User Story 1: Dashboard and System Overview

#### TS-001: View Dashboard Metrics
**Priority**: High
**User Role**: All users

**Preconditions**:
- User is logged in
- Dashboard page loads

**Test Steps**:
1. Navigate to dashboard (homepage)
2. Verify COBOL program information displayed (RG1866B, SUSEP Circular 360)
3. Verify function points metrics shown (687 data items, 26 tables, 5000+ lines)
4. Verify system dependencies map visible (DB2, PREMIT, PREMCED)
5. Verify dashboard loads in <2 seconds

**Expected Results**:
- All metrics are accurate and clearly displayed
- Dashboard is visually appealing with Caixa Seguradora branding (blue #0047BB, yellow #FFB81C)
- All text is in Portuguese
- Dashboard loads quickly without errors

**Acceptance Criteria**:
- ✅ All information is correct
- ✅ Load time <2 seconds
- ✅ No visual defects

---

### User Story 2: Report Generation Workflow

#### TS-002: Generate Monthly PREMIT Report
**Priority**: Critical (Regulatory Requirement)
**User Role**: Operator, Admin

**Preconditions**:
- User has operator or admin role
- Database contains premium data for October 2025

**Test Steps**:
1. Navigate to "Relatórios" (Reports) page
2. Select report type: "PREMIT"
3. Enter date range: 2025-10-01 to 2025-10-31
4. Click "Gerar Relatório" (Generate Report)
5. Observe progress indicator
6. Wait for completion
7. Download generated PREMIT.TXT file
8. Verify file format matches COBOL output

**Expected Results**:
- Report generation completes in <5 minutes for 10,000 records
- Progress indicator shows real-time status
- Generated file matches COBOL PREMIT.TXT format (byte-for-byte)
- File can be downloaded successfully
- Success message displayed in Portuguese

**Acceptance Criteria**:
- ✅ Report generation successful
- ✅ File format is correct (fixed-width, 200 characters per line)
- ✅ Data accuracy validated against COBOL output
- ✅ Process completes within time limit

#### TS-003: Generate Quarterly PREMCED Report
**Priority**: Critical (Regulatory Requirement)
**User Role**: Operator, Admin

**Test Steps**:
1. Navigate to "Relatórios" page
2. Select report type: "PREMCED"
3. Enter date range: 2025-07-01 to 2025-09-30 (Q3)
4. Click "Gerar Relatório"
5. Monitor progress
6. Download generated PREMCED.TXT file
7. Verify file format and data accuracy

**Expected Results**:
- PREMCED report generated successfully
- File format matches COBOL PREMCED.TXT
- Cossurance calculations are correct
- File can be downloaded

**Acceptance Criteria**:
- ✅ Report generation successful
- ✅ Cossurance calculations match COBOL
- ✅ File format is correct

#### TS-004: Handle Invalid Date Range
**Priority**: Medium
**User Role**: Operator, Admin

**Test Steps**:
1. Navigate to "Relatórios" page
2. Select report type: "PREMIT"
3. Enter invalid date range: End date before start date (e.g., 2025-10-31 to 2025-10-01)
4. Click "Gerar Relatório"

**Expected Results**:
- Error message displayed in Portuguese: "Data inicial não pode ser maior que data final"
- Report generation does not proceed
- Form fields highlighted in red

**Acceptance Criteria**:
- ✅ Clear error message shown
- ✅ No report generated
- ✅ User can correct and retry

---

### User Story 3: Data Query and Visualization

#### TS-005: Query Premiums by Date Range
**Priority**: High
**User Role**: All users

**Test Steps**:
1. Navigate to "Consultas" (Queries) page
2. Select entity type: "Prêmios" (Premiums)
3. Enter date range: 2025-10-01 to 2025-10-31
4. Click "Consultar" (Query)
5. Review query results

**Expected Results**:
- Results display in <3 seconds
- Data is displayed in table format with pagination
- Summary statistics shown (total amount, average, record count)
- Interactive charts displayed (premium trends over time)
- Data is accurate

**Acceptance Criteria**:
- ✅ Query returns results quickly (<3s)
- ✅ Data is accurate
- ✅ Charts are interactive and meaningful

#### TS-006: View Policy Details
**Priority**: Medium
**User Role**: All users

**Test Steps**:
1. Navigate to "Consultas" page
2. Select entity type: "Apólices" (Policies)
3. Enter policy number: POL-TEST-001
4. Click "Consultar"
5. Review policy details (coverages, endorsements, client info)

**Expected Results**:
- Policy details displayed correctly
- All related information shown (coverages, client, premiums)
- Data matches source records

**Acceptance Criteria**:
- ✅ All policy information displayed
- ✅ Data is accurate and complete

#### TS-007: Export Query Results to CSV
**Priority**: Medium
**User Role**: Operator, Admin

**Test Steps**:
1. Execute a premium query (TS-005)
2. Click "Exportar" (Export) button
3. Select format: CSV
4. Download file
5. Open in Excel/Google Sheets

**Expected Results**:
- CSV file downloads successfully
- File contains all queried records
- Column headers are in Portuguese
- Data is properly formatted (decimals use correct separator)

**Acceptance Criteria**:
- ✅ CSV export works
- ✅ Data is complete and accurate
- ✅ File opens correctly in spreadsheet software

---

### User Story 4: Batch Job Scheduling

#### TS-008: Schedule Monthly Report Generation
**Priority**: High
**User Role**: Admin

**Test Steps**:
1. Navigate to "Lotes" (Batch Jobs) page
2. Click "Criar Novo Lote" (Create New Job)
3. Fill form:
   - Job Type: PREMIT Generation
   - Schedule: Tomorrow at 2:00 AM
   - Date Range: Previous month
4. Click "Agendar" (Schedule)
5. Verify job appears in job list with status "Agendado"

**Expected Results**:
- Job is created successfully
- Job appears in job history with correct details
- Job status is "Agendado" (Scheduled)
- Confirmation message displayed

**Acceptance Criteria**:
- ✅ Job scheduling successful
- ✅ Job details are correct
- ✅ Job will execute at scheduled time

#### TS-009: Monitor Running Job
**Priority**: High
**User Role**: Operator, Admin

**Test Steps**:
1. Create an immediate batch job (scheduled for now)
2. Navigate to job details
3. Monitor progress indicator
4. Wait for completion
5. Verify job status changes to "Concluído" (Completed)

**Expected Results**:
- Progress bar shows real-time progress (0-100%)
- Records processed count updates
- Job completes successfully
- Status changes from "Executando" to "Concluído"

**Acceptance Criteria**:
- ✅ Progress tracking works in real-time
- ✅ Job completes successfully
- ✅ Final status is correct

#### TS-010: View Job Execution History
**Priority**: Medium
**User Role**: All users

**Test Steps**:
1. Navigate to "Lotes" page
2. View job history table
3. Filter by date range (last 30 days)
4. Click on a completed job to view details

**Expected Results**:
- All jobs from last 30 days displayed
- Job details show execution time, records processed, output file
- Failed jobs show error messages
- Job history is paginated

**Acceptance Criteria**:
- ✅ Job history is complete and accurate
- ✅ Job details provide useful information
- ✅ Error messages are clear

---

### User Story 5: Mock Data Management

#### TS-011: Upload Mock Premium Data (CSV)
**Priority**: Low (Testing Feature)
**User Role**: Admin

**Test Steps**:
1. Navigate to "Dados Mock" (Mock Data) page
2. Select entity type: "Prêmios"
3. Choose CSV file (provided sample: `premiums_sample.csv`)
4. Click "Carregar" (Upload)
5. Review upload results and validation errors

**Expected Results**:
- File uploads successfully
- Validation runs automatically
- Results show number of records loaded and any validation errors
- Invalid records are reported with clear error messages

**Acceptance Criteria**:
- ✅ CSV upload works
- ✅ Validation catches errors
- ✅ Error messages are helpful

#### TS-012: Reset Database
**Priority**: Low (Testing Feature)
**User Role**: Admin

**Test Steps**:
1. Navigate to "Dados Mock" page
2. Click "Resetar Banco de Dados" (Reset Database)
3. Confirm action in dialog
4. Verify all data is cleared
5. Verify success message

**Expected Results**:
- Confirmation dialog appears
- Database is reset to clean state
- Success message displayed
- Subsequent queries return no results

**Acceptance Criteria**:
- ✅ Reset functionality works
- ✅ Confirmation required before reset
- ✅ Database is truly empty after reset

---

## Cross-Functional Testing Scenarios

### CFT-001: Portuguese Language Validation
**Priority**: High (Regulatory Requirement FR-025)

**Test Steps**:
1. Review all pages in the application
2. Verify all buttons, labels, messages, errors are in Portuguese
3. Check for English text or untranslated keys

**Expected Results**:
- 100% of user-facing text is in Portuguese
- No English text visible
- Grammar and spelling are correct

**Acceptance Criteria**:
- ✅ All text is in Portuguese
- ✅ Professional quality translations

### CFT-002: Caixa Seguradora Branding
**Priority**: Medium (Requirement FR-025)

**Test Steps**:
1. Review all pages for visual consistency
2. Verify primary color is Caixa blue (#0047BB)
3. Verify accent color is Caixa yellow (#FFB81C)
4. Check logo placement and quality

**Expected Results**:
- Consistent Caixa Seguradora branding throughout
- Colors match brand guidelines
- Logo is crisp and professional

**Acceptance Criteria**:
- ✅ Branding is consistent
- ✅ Colors are correct

### CFT-003: Mobile Responsiveness
**Priority**: Medium

**Test Steps**:
1. Access application on mobile devices (iOS and Android)
2. Test all user stories on mobile
3. Verify layout adapts correctly

**Expected Results**:
- Application is usable on mobile devices
- No horizontal scrolling required
- Buttons and links are easily tappable

**Acceptance Criteria**:
- ✅ Mobile experience is acceptable

### CFT-004: Cross-Browser Compatibility
**Priority**: Medium

**Test Steps**:
1. Test application on Chrome, Firefox, Safari, Edge
2. Execute key scenarios on each browser
3. Verify consistent behavior

**Expected Results**:
- Application works on all supported browsers
- No browser-specific bugs

**Acceptance Criteria**:
- ✅ Works on Chrome, Firefox, Safari, Edge

---

## Performance Testing Scenarios

### PT-001: Dashboard Load Performance
**Priority**: High (SC-001)

**Objective**: Verify dashboard loads in <2 seconds

**Test Steps**:
1. Clear browser cache
2. Navigate to dashboard
3. Measure time to interactive
4. Repeat 10 times and average

**Acceptance Criteria**:
- ✅ Average load time <2 seconds
- ✅ P95 load time <3 seconds

### PT-002: Query Performance
**Priority**: High (SC-003)

**Objective**: Verify query results display in <3 seconds

**Test Steps**:
1. Execute premium query for 1 month of data
2. Measure time from click to results displayed
3. Repeat with different date ranges

**Acceptance Criteria**:
- ✅ Average query time <3 seconds
- ✅ Large result sets still perform well

### PT-003: Large Dataset Processing
**Priority**: High (FR-028)

**Objective**: Verify system can process 10,000+ records in <5 minutes

**Test Steps**:
1. Generate PREMIT report for date range with 10,000+ records
2. Measure total processing time
3. Verify output is correct

**Acceptance Criteria**:
- ✅ Processing time <5 minutes
- ✅ Output is accurate
- ✅ No errors or timeouts

### PT-004: Concurrent Users
**Priority**: Medium

**Objective**: Verify system supports 10 concurrent users

**Test Steps**:
1. Have 10 users generate reports simultaneously
2. Monitor system performance
3. Verify all reports complete successfully

**Acceptance Criteria**:
- ✅ All reports complete without errors
- ✅ Performance degradation is acceptable (<20% slowdown)

---

## Regulatory Compliance Testing

### RC-001: SUSEP Circular 360 Compliance
**Priority**: Critical

**Objective**: Verify generated reports meet SUSEP requirements

**Test Steps**:
1. Generate PREMIT report for October 2025
2. Compare output with COBOL-generated PREMIT for same period
3. Verify byte-for-byte match

**Acceptance Criteria**:
- ✅ PREMIT format matches COBOL output exactly
- ✅ All required fields present and correct
- ✅ File structure adheres to SUSEP specifications

### RC-002: Data Accuracy Validation
**Priority**: Critical

**Objective**: Verify financial calculations are correct

**Test Steps**:
1. Select 100 random premium records
2. Manually recalculate premium amounts
3. Compare with system calculations
4. Verify 100% accuracy

**Acceptance Criteria**:
- ✅ 100% calculation accuracy
- ✅ No rounding errors
- ✅ Decimal precision maintained

---

## Defect Management

### Severity Levels

**Critical (P0)**:
- System crash or data corruption
- Regulatory compliance failure
- Security vulnerability
- **Action**: Fix immediately, block UAT until resolved

**High (P1)**:
- Major functionality broken
- No workaround available
- **Action**: Fix within 24 hours

**Medium (P2)**:
- Minor functionality broken
- Workaround available
- **Action**: Fix before production deployment

**Low (P3)**:
- Cosmetic issues
- Minor usability concerns
- **Action**: Fix if time permits, otherwise defer to post-production

### Defect Tracking

All defects will be tracked in:
- **Tool**: GitHub Issues / Jira
- **Labels**: `uat`, `bug`, `severity:critical`, etc.
- **Template**:
  ```
  **Test Scenario**: TS-XXX
  **Severity**: P1
  **Description**: [Brief description]
  **Steps to Reproduce**: [Numbered steps]
  **Expected Result**: [What should happen]
  **Actual Result**: [What actually happened]
  **Screenshots**: [Attach if applicable]
  ```

---

## UAT Schedule

### Week 1: Core Functionality Testing

| Day | Activities | Participants |
|-----|-----------|--------------|
| Day 1 | UAT kickoff meeting, environment setup, credentials distribution | All |
| Day 2-3 | User Stories 1-3 testing (Dashboard, Reports, Queries) | SMEs, QA |
| Day 4-5 | User Stories 4-5 testing (Batch Jobs, Mock Data) | SMEs, QA |

### Week 2: Performance, Compliance, and Sign-Off

| Day | Activities | Participants |
|-----|-----------|--------------|
| Day 6 | Performance testing (PT-001 to PT-004) | QA, DevOps |
| Day 7 | Regulatory compliance testing (RC-001, RC-002) | Compliance Officer, SMEs |
| Day 8 | Defect resolution and retest | Development Team, QA |
| Day 9 | Cross-functional testing (CFT-001 to CFT-004) | All participants |
| Day 10 | Final review, sign-off meeting | Business Sponsor, Stakeholders |

---

## UAT Success Criteria

UAT is considered successful if:

1. ✅ All Critical (P0) and High (P1) defects are resolved
2. ✅ All test scenarios achieve "PASS" status (or acceptable "PASS with notes")
3. ✅ Performance benchmarks are met (SC-001, SC-003, FR-028)
4. ✅ Regulatory compliance validated (RC-001, RC-002)
5. ✅ Business stakeholders provide written sign-off
6. ✅ No showstopper issues remain
7. ✅ Medium (P2) defects have documented workarounds

---

## UAT Deliverables

Upon UAT completion, the following will be delivered:

1. **UAT Execution Report**: Summary of all test scenarios executed, pass/fail status, defects found
2. **Defect Log**: Complete list of all defects, resolutions, and status
3. **Performance Test Results**: Benchmark data for all performance scenarios
4. **Compliance Validation Report**: Evidence of SUSEP Circular 360 compliance
5. **Sign-Off Document**: Formal approval from business stakeholders
6. **Lessons Learned**: Recommendations for production deployment and future enhancements

---

## Risks and Mitigations

| Risk | Impact | Probability | Mitigation |
|------|--------|-------------|------------|
| COBOL output doesn't match | High | Medium | Generate mock COBOL samples for comprehensive testing |
| Performance issues with large datasets | High | Low | Conduct performance testing early, optimize if needed |
| UAT environment instability | Medium | Low | Daily health checks, immediate issue resolution |
| Insufficient test data | Medium | Low | Load production snapshot (anonymized) |
| SME availability limited | Medium | Medium | Schedule UAT sessions in advance, provide flexible hours |

---

## Contingency Plan

If UAT reveals critical issues:

1. **Assess Impact**: Determine if issue blocks production deployment
2. **Emergency Fix**: Development team prioritizes fix
3. **Expedited Retest**: QA validates fix immediately
4. **Decision Point**:
   - If fixed: Continue UAT
   - If not fixable quickly: Extend UAT timeline or defer production launch
5. **Stakeholder Communication**: Keep business informed of status

---

## Post-UAT Activities

After successful UAT:

1. **Production Deployment Preparation**
   - Final code review and security scan
   - Production environment setup
   - Deployment runbook creation

2. **Training**
   - End-user training sessions
   - Administrator training
   - Operations team handoff

3. **Go-Live**
   - Deployment during maintenance window
   - Smoke testing in production
   - Hypercare support (1 week)

4. **Post-Production Review**
   - Lessons learned session
   - Performance monitoring
   - User feedback collection

---

## Contact Information

**UAT Coordinator**: uat.coordinator@caixaseguradora.com.br
**Technical Support**: dev.support@caixaseguradora.com.br
**Helpdesk**: helpdesk@caixaseguradora.com.br
**Escalation**: project.manager@caixaseguradora.com.br

---

## Appendix A: Test Data Samples

Sample test data files will be provided:
- `premiums_sample.csv` - 1,000 premium records
- `policies_sample.csv` - 500 policy records
- `clients_sample.csv` - 300 client records
- `COBOL_PREMIT_Oct2025_sample.TXT` - COBOL output for comparison
- `COBOL_PREMCED_Q3 2025_sample.TXT` - COBOL output for comparison

---

## Appendix B: UAT Checklist

Before starting UAT:
- [ ] UAT environment is stable and accessible
- [ ] Test data is loaded
- [ ] User accounts are created
- [ ] UAT plan reviewed with all participants
- [ ] Defect tracking system is configured
- [ ] Communication channels established

During UAT:
- [ ] Daily standup meetings (15 minutes)
- [ ] Defects logged within 2 hours of discovery
- [ ] Progress tracked against schedule
- [ ] Blockers escalated immediately

After UAT:
- [ ] All test scenarios executed
- [ ] Defect log finalized
- [ ] UAT report written
- [ ] Sign-off obtained
- [ ] Lessons learned documented

---

**Document Version**: 1.0
**Prepared By**: QA Team
**Approved By**: Project Manager
**Date**: October 23, 2025
