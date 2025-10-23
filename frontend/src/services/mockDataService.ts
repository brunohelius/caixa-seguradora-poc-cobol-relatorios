import apiClient from './apiClient';

export interface MockDataUploadRequest {
  file: File;
  entityType: string;
  format?: 'csv' | 'json';
  clearExisting?: boolean;
}

export interface MockDataLoadResponse {
  success: boolean;
  message: string;
  entityType: string;
  recordsInserted: number;
  recordsFailed: number;
  recordsUpdated: number;
  recordsDeleted: number;
  processingTimeMs: number;
  validationErrors: ValidationError[];
  warnings: string[];
  foreignKeyValidation?: any;
}

export interface ValidationError {
  rowNumber: number;
  columnName?: string;
  invalidValue?: string;
  errorMessage: string;
  errorType: string;
}

export interface MockDataStats {
  success: boolean;
  counts: Record<string, number>;
  entityCounts: Array<{ entityType: string; count: number }>;
  totalRecords: number;
}

export interface ValidationIssue {
  severity: 'ERROR' | 'WARNING' | 'INFO';
  category: string;
  message: string;
  entity?: string;
  entityType?: string;
  field?: string;
  fieldName?: string;
  value?: any;
  affectedRecords?: number;
  rowNumber?: number;
  timestamp?: string;
}

export interface ValidationReport {
  isValid: boolean;
  message: string;
  validationTimestamp: string;
  validationTimeMs: number;
  entityResults: any[];
  foreignKeyValidation?: any;
  schemaValidation?: any;
  dataQualityIssues: any[];
  issues: ValidationIssue[];
  errorCount: number;
  warningCount: number;
  infoCount: number;
  statistics: ValidationStatistic[];
}

export interface ValidationStatistic {
  category: string;
  entityType: string;
  count: number;
  percentage: number;
  totalRecords: number;
  validRecords: number;
  invalidRecords: number;
  errorCount: number;
  warningCount: number;
}

export interface ValidationStatistics {
  totalEntities: number;
  totalRecords: number;
  totalChecks: number;
  totalErrors: number;
  totalWarnings: number;
  passRate: number;
}

export interface SchemaInfo {
  EntityTypes: string[];
}

export interface ClearEntityResponse {
  success: boolean;
  message: string;
  deletedCount: number;
  entityType: string;
}

export interface ResetDatabaseResponse {
  success: boolean;
  message: string;
  totalDeleted: number;
}

class MockDataService {
  /**
   * Upload and load CSV/JSON file for specified entity type
   */
  async loadMockData(
    file: File,
    entityType: string,
    format: 'csv' | 'json' = 'csv',
    clearExisting: boolean = false
  ): Promise<MockDataLoadResponse> {
    try {
      const formData = new FormData();
      formData.append('file', file);
      formData.append('entityType', entityType);
      formData.append('format', format);
      formData.append('clearExisting', clearExisting.toString());

      const response = await apiClient.post<MockDataLoadResponse>('/mock-data/load', formData, {
        headers: {
          'Content-Type': 'multipart/form-data',
        },
      });

      return response.data;
    } catch (error) {
      console.error('Error loading mock data:', error);
      throw error;
    }
  }

  /**
   * Validate data integrity (foreign keys, required fields, etc.)
   */
  async validateData(entityType?: string): Promise<ValidationReport> {
    try {
      const params = entityType ? { entityType } : {};
      const response = await apiClient.get<ValidationReport>('/mock-data/validate', { params });
      return response.data;
    } catch (error) {
      console.error('Error validating data:', error);
      throw error;
    }
  }

  /**
   * Reset database - delete all mock data
   */
  async resetDatabase(): Promise<ResetDatabaseResponse> {
    try {
      const response = await apiClient.post<ResetDatabaseResponse>('/mock-data/reset');
      return response.data;
    } catch (error) {
      console.error('Error resetting database:', error);
      throw error;
    }
  }

  /**
   * Clear data from a specific entity table
   */
  async clearEntity(entityType: string): Promise<ClearEntityResponse> {
    try {
      const response = await apiClient.delete<ClearEntityResponse>(`/mock-data/clear/${entityType}`);
      return response.data;
    } catch (error) {
      console.error(`Error clearing entity ${entityType}:`, error);
      throw error;
    }
  }

  /**
   * Get database statistics (record counts per entity)
   */
  async getStats(): Promise<MockDataStats> {
    try {
      const response = await apiClient.get<MockDataStats>('/mock-data/stats');
      return response.data;
    } catch (error) {
      console.error('Error getting stats:', error);
      throw error;
    }
  }

  /**
   * Get database schema information
   */
  async getSchema(entityType?: string): Promise<SchemaInfo> {
    try {
      const params = entityType ? { entityType } : {};
      const response = await apiClient.get<SchemaInfo>('/mock-data/schema', { params });
      return response.data;
    } catch (error) {
      console.error('Error getting schema:', error);
      throw error;
    }
  }
}

export default new MockDataService();
