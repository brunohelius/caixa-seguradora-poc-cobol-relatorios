import apiClient from './apiClient';
import type {
  DashboardMetricsDto,
  FunctionPointsDto,
  DatabaseDependenciesDto,
} from './types';

/**
 * Dashboard service for fetching COBOL program analysis metrics.
 * Provides data for migration dashboard visualization.
 */
class DashboardService {
  /**
   * Gets comprehensive dashboard metrics including program info, complexity, and migration progress.
   * Data sourced from COBOL analysis (FINAL-ANALYSIS-REPORT.md).
   */
  async getMetrics(): Promise<DashboardMetricsDto> {
    const response = await apiClient.get<DashboardMetricsDto>('/api/Dashboard/metrics');
    return response.data;
  }

  /**
   * Gets function points analysis for project estimation.
   * Based on IFPUG counting practices for complexity assessment.
   */
  async getFunctionPoints(): Promise<FunctionPointsDto> {
    const response = await apiClient.get<FunctionPointsDto>('/api/Dashboard/function-points');
    return response.data;
  }

  /**
   * Gets database dependencies showing tables, cursors, and relationships.
   * Critical for understanding data access patterns and migration complexity.
   */
  async getDatabaseDependencies(): Promise<DatabaseDependenciesDto> {
    const response = await apiClient.get<DatabaseDependenciesDto>('/api/Dashboard/database-dependencies');
    return response.data;
  }

  /**
   * Health check for dashboard service availability.
   */
  async healthCheck(): Promise<{ status: string; service: string; timestamp: string }> {
    const response = await apiClient.get<{ status: string; service: string; timestamp: string }>('/api/Dashboard/health');
    return response.data;
  }
}

export default new DashboardService();
