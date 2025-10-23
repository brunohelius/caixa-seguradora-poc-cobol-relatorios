import apiClient from './apiClient';

export interface MockDataUploadRequest {
  file: File;
  entityType: string;
}

export interface MockDataStats {
  totalRecords: number;
  entityCounts: Record<string, number>;
}

export interface ValidationReport {
  isValid: boolean;
  errorCount: number;
  warningCount: number;
  infoCount: number;
  issues: ValidationIssue[];
  statistics: EntityValidationStats[];
}

export interface ValidationIssue {
  severity: 'ERROR' | 'WARNING' | 'INFO';
  entityType: string;
  entityId?: string;
  rowNumber?: number;
  fieldName?: string;
  message: string;
  timestamp: string;
}

export interface EntityValidationStats {
  entityType: string;
  totalRecords: number;
  validRecords: number;
  invalidRecords: number;
  errorCount: number;
  warningCount: number;
}

class MockDataService {
  /**
   * Upload and load CSV file for specified entity type
   */
  async loadMockData(file: File, entityType: string): Promise<any> {
    try {
      const formData = new FormData();
      formData.append('file', file);
      formData.append('entityType', entityType);

      const response = await apiClient.post<any>('/mock-data/load', formData, {
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
  async validateData(): Promise<ValidationReport> {
    try {
      const response = await apiClient.post<ValidationReport>('/mock-data/validate');
      return response.data;
    } catch (error) {
      console.error('Error validating data:', error);
      throw error;
    }
  }

  /**
   * Reset database - delete all mock data
   */
  async resetDatabase(): Promise<any> {
    try {
      const response = await apiClient.post<any>('/mock-data/reset');
      return response.data;
    } catch (error) {
      console.error('Error resetting database:', error);
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
}

export default new MockDataService();
