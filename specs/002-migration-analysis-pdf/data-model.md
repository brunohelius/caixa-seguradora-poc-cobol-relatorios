# Data Model: PDF Migration Analysis Document

**Date**: October 23, 2025
**Feature**: Comprehensive Migration Analysis PDF Report
**Purpose**: Define data structures used for PDF document generation

## Overview

This document defines the data models used to generate the migration analysis PDF. Data is extracted from existing documentation and transformed into structured models for rendering.

## Core Document Models

### DocumentMetadata
Track PDF version, generation timestamp, and source data provenance.

**Properties**:
- Version (string): Semantic version
- GeneratedAt (DateTime): UTC timestamp
- GeneratedBy (string): User or system
- SourceDataVersions (Dictionary): File paths to git hashes
- ChangeLog (List): Change summary

### CobolMetrics
Quantitative analysis of legacy COBOL RG1866B system.

**Properties**:
- ProgramId: "RG1866B"
- ProgramName: "SUSEP Circular 360 Premium Reporting"
- TotalLines: ~5,000 lines
- DataItems: 687 items
- Sections: 63 sections
- Paragraphs: 65 paragraphs
- Cursors: 4 database cursors
- DatabaseTables: 26+ tables/views
- ExternalModules: RE0001S, GE0009S, GE0010S
- OutputFiles: PREMIT.TXT, PREMCED.TXT

**Source**: `/docs/parser/FINAL-ANALYSIS-REPORT.md`

### MigrationArchitecture
Target .NET 9 and React 18 architecture description.

**Properties**:
- BackendFramework: ".NET 9"
- FrontendFramework: "React 18"
- ArchitecturePattern: "Clean Architecture"
- DatabaseTechnology: "SQLite (dev), SQL Server (prod)"
- Layers: API, Core, Infrastructure
- ApiEndpoints: 28 endpoints
- ApiCategories: Reports, Premiums, Policies, etc.

**Source**: `/specs/001-vamos-migrar-sistema/spec.md`

### FunctionPoint
IFPUG function point calculation data.

**Properties**:
- Type: EI, EO, EQ, ILF, EIF
- Name: Function description
- Complexity: Low, Average, High
- DataElements: Number of DETs
- FileReferences: Number of FTRs
- RecordTypes: Number of RETs
- Points: Calculated function points

**IFPUG Weighting Table**:
- EI: Low=3, Avg=4, High=6
- EO: Low=4, Avg=5, High=7
- EQ: Low=3, Avg=4, High=6
- ILF: Low=7, Avg=10, High=15
- EIF: Low=5, Avg=7, High=10

### ProjectSchedule
12-week timeline (8 dev + 4 homologation).

**Properties**:
- TotalDurationWeeks: 12
- DevelopmentWeeks: 8
- HomologationWeeks: 4
- Phases: Foundation, Core Backend, Report Gen, Validation
- Tasks: Individual work items with dependencies
- Milestones: Key checkpoints
- CriticalPath: Non-delayable tasks

### FinancialAnalysis
Cost calculations and payment schedule.

**Properties**:
- TotalFunctionPoints: Sum of all FPs
- RatePerFunctionPoint: 750 BRL (fixed)
- TotalProjectCost: TotalFP × 750
- PaymentMilestones: 30%, 40%, 20%, 10%
- CostBreakdownByPhase: Phase costs
- ROIAnalysis: Legacy vs modern costs

## Chart Data Models

### GanttChartData
- Tasks: Bars with start/end dates
- Dependencies: Arrows between tasks
- Milestones: Diamond markers
- TodayMarker: Current date line

### PieChartData
- Title: Chart title
- Segments: Label, value, color, percentage

### BarChartData
- Title: Chart title
- Categories: X-axis labels
- Series: Y-axis values

## Data Extraction Strategy

**Source Files**:
1. `docs/parser/FINAL-ANALYSIS-REPORT.md` → CobolMetrics
2. `specs/001-vamos-migrar-sistema/spec.md` → MigrationArchitecture
3. `specs/001-vamos-migrar-sistema/contracts/openapi.yaml` → API counts
4. Manual JSON files → FunctionPoints, ProjectSchedule

**Validation Rules**:
- Financial totals must be consistent
- Schedule must equal 12 weeks
- Function point complexity must match DET/FTR counts
- Payment milestones must sum to 100%

See plan.md for complete implementation details.
