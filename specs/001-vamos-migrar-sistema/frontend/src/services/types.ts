/**
 * TypeScript type definitions matching OpenAPI schemas.
 * These types ensure type safety when communicating with the backend API.
 */

// ==================== Dashboard Types ====================

export interface DashboardMetricsDto {
  programInfo: ProgramInfoDto;
  dataStructure: DataStructureMetricsDto;
  complexity: ComplexityMetricsDto;
  migrationProgress: MigrationProgressDto;
}

export interface ProgramInfoDto {
  programName: string;
  description: string;
  programType: string;
  outputFiles: string[];
  totalLinesOfCode: number;
  lastAnalyzed: string;
}

export interface DataStructureMetricsDto {
  totalDataItems: number;
  workingStorageSections: number;
  fileSections: number;
  linkageSections: number;
  databaseTables: number;
  cursorDeclarations: number;
}

export interface ComplexityMetricsDto {
  totalSections: number;
  totalParagraphs: number;
  decisionPoints: number;
  cyclomaticComplexity: number;
  externalCalls: number;
  sqlStatements: number;
  fileOperations: number;
}

export interface MigrationProgressDto {
  completionPercentage: number;
  tasksCompleted: number;
  totalTasks: number;
  status: string;
  currentPhase: string;
  lastUpdated: string;
  validationMatchPercentage: number;
}

export interface FunctionPointsDto {
  externalInputs: FunctionPointCategoryDto;
  externalOutputs: FunctionPointCategoryDto;
  externalInquiries: FunctionPointCategoryDto;
  internalLogicalFiles: FunctionPointCategoryDto;
  externalInterfaceFiles: FunctionPointCategoryDto;
  totalUnadjustedFunctionPoints: number;
  valueAdjustmentFactor: number;
  totalAdjustedFunctionPoints: number;
  estimatedEffortMonths: number;
  complexityRating: string;
  moduleBreakdown: ModuleFunctionPointsDto[];
}

export interface FunctionPointCategoryDto {
  categoryName: string;
  lowComplexityCount: number;
  averageComplexityCount: number;
  highComplexityCount: number;
  totalCount: number;
  lowComplexityPoints: number;
  averageComplexityPoints: number;
  highComplexityPoints: number;
  totalPoints: number;
}

export interface ModuleFunctionPointsDto {
  moduleName: string;
  functionPoints: number;
  complexity: string;
  estimatedHours: number;
  status: string;
}

export interface DatabaseDependenciesDto {
  tables: DatabaseTableDto[];
  cursors: CursorDeclarationDto[];
  sqlStats: SqlOperationStatsDto;
  relationships: TableRelationshipDto[];
  totalTables: number;
  totalCursors: number;
}

export interface DatabaseTableDto {
  name: string;
  type: string;
  description: string;
  entityName: string;
  columnCount: number;
  accessTypes: string[];
  accessFrequency: string;
  accessedBySections: string[];
  usesCursor: boolean;
  estimatedRowCount: number;
}

export interface CursorDeclarationDto {
  cursorName: string;
  targetTable: string;
  declaredInSection: string;
  whereClause: string;
  orderBy: string;
  cursorType: string;
  dotNetImplementation: string;
  estimatedRecordsProcessed: number;
}

export interface SqlOperationStatsDto {
  selectCount: number;
  insertCount: number;
  updateCount: number;
  deleteCount: number;
  totalOperations: number;
  readOnlyPercentage: number;
}

export interface TableRelationshipDto {
  sourceTable: string;
  targetTable: string;
  relationshipType: string;
  foreignKeyColumns: string[];
  description: string;
}

// ==================== Report Generation Types ====================

export interface ReportGenerationRequest {
  systemId: string;
  startDate: string; // ISO 8601 format (YYYY-MM-DD)
  endDate: string;
  reportType: 'PREMIT' | 'PREMCED' | 'BOTH';
  mode: 'EMISSION' | 'CANCELLATION' | 'ALL';
  companyCode?: number;
}

export interface ReportGenerationResponse {
  jobId: string;
  status: 'QUEUED' | 'PROCESSING' | 'COMPLETED' | 'FAILED';
  message: string;
  estimatedCompletionTime?: string;
}

export interface ReportStatusResponse {
  jobId: string;
  status: 'QUEUED' | 'PROCESSING' | 'COMPLETED' | 'FAILED';
  progress: number; // 0-100
  recordsProcessed: number;
  totalRecords: number;
  startTime: string;
  endTime?: string;
  errorMessage?: string;
  filesGenerated?: string[];
}

export interface ReportHistoryItem {
  jobId: string;
  requestDate: string;
  systemId: string;
  reportType: string;
  status: string;
  recordsProcessed: number;
  filesGenerated: string[];
}

// ==================== Query Types ====================

export interface PremiumQueryRequest {
  filters: {
    policyNumberFrom?: number;
    policyNumberTo?: number;
    startDate?: string;
    endDate?: string;
    companyCode?: number;
    lineOfBusiness?: number;
    movementType?: string;
  };
  columns: string[];
  aggregations?: {
    field: string;
    function: 'SUM' | 'AVG' | 'COUNT' | 'MIN' | 'MAX';
  }[];
  sorting?: {
    field: string;
    direction: 'ASC' | 'DESC';
  }[];
  pagination: {
    page: number;
    pageSize: number;
  };
}

export interface PremiumQueryResponse {
  data: Record<string, any>[];
  summary: {
    totalRecords: number;
    totalPages: number;
    currentPage: number;
  };
  aggregations?: Record<string, number>;
}

export interface PremiumStatisticsRequest {
  startDate: string;
  endDate: string;
  groupBy: 'MONTH' | 'PRODUCT' | 'COMPANY' | 'LINE_OF_BUSINESS';
}

export interface PremiumStatisticsResponse {
  groupLabel: string;
  totalPremium: number;
  policyCount: number;
  averagePremium: number;
}

// ==================== Batch Job Types ====================

export interface BatchJobCreateRequest {
  name: string;
  description: string;
  reportType: 'PREMIT' | 'PREMCED' | 'BOTH';
  systemId: string;
  scheduleExpression: string; // Cron expression
  enabled: boolean;
  parameters: Record<string, any>;
}

export interface BatchJob {
  jobId: string;
  name: string;
  description: string;
  scheduleExpression: string;
  enabled: boolean;
  lastRunTime?: string;
  nextRunTime: string;
  status: 'ACTIVE' | 'PAUSED' | 'COMPLETED' | 'FAILED';
}

export interface JobExecution {
  executionId: string;
  jobId: string;
  startTime: string;
  endTime?: string;
  status: 'RUNNING' | 'COMPLETED' | 'FAILED';
  recordsProcessed: number;
  errorMessage?: string;
}

// ==================== Data Management Types ====================

export interface MockDataLoadRequest {
  entityType: string;
  fileFormat: 'CSV' | 'JSON';
  fileContent: string;
  validateSchema: boolean;
}

export interface MockDataLoadResponse {
  success: boolean;
  recordsLoaded: number;
  recordsRejected: number;
  validationErrors: string[];
}

export interface DataValidationResponse {
  isValid: boolean;
  totalRecords: number;
  validRecords: number;
  invalidRecords: number;
  errors: {
    recordNumber: number;
    field: string;
    message: string;
  }[];
}

export interface SchemaInfo {
  tableName: string;
  columns: {
    name: string;
    type: string;
    nullable: boolean;
    maxLength?: number;
  }[];
  recordCount: number;
}

export interface ComparisonResult {
  testName: string;
  cobolOutput: string;
  dotnetOutput: string;
  matchesExactly: boolean;
  differences: {
    line: number;
    cobolValue: string;
    dotnetValue: string;
  }[];
}

// ==================== Common Types ====================

export interface PaginationParams {
  page: number;
  pageSize: number;
}

export interface SortingParams {
  field: string;
  direction: 'ASC' | 'DESC';
}

export interface FilterParams {
  field: string;
  operator: 'EQUALS' | 'CONTAINS' | 'GREATER_THAN' | 'LESS_THAN' | 'BETWEEN';
  value: any;
}

// ==================== Entity Types (matching backend entities) ====================

export interface PremiumRecord {
  premiumId: number;
  companyCode: number;
  referenceYear: number;
  referenceMonth: number;
  referenceDay: number;
  movementType: string;
  policyNumber: number;
  endorsementNumber: number;
  totalPremiumTotal: number;
  netPremiumTotal: number;
  // ... (add more fields as needed)
}

export interface Policy {
  policyNumber: number;
  companyCode: number;
  productCode: number;
  clientCode: number;
  effectiveDate: string;
  expirationDate: string;
  policyStatus: string;
  insuredAmount: number;
  totalPremium: number;
}

export interface Product {
  productCode: number;
  productName: string;
  lineOfBusiness: number;
  productStatus: string;
}

export interface Client {
  clientCode: number;
  clientName: string;
  clientType: 'F' | 'J'; // Fisica or Juridica
  documentNumber: string;
  email?: string;
  phoneNumber?: string;
}
