import apiClient from './apiClient';
import type {
  PremiumQueryResponse,
  PremiumStatisticsResponse,
  Policy,
  Product,
  Client,
  PremiumRecord,
} from './types';

/**
 * Query service for premium, policy, product, and client data retrieval.
 * Provides flexible querying capabilities with filtering, sorting, and pagination.
 */
class QueryService {
  // ==================== Premium Queries ====================

  /**
   * Query premium records with flexible filtering, sorting, and pagination.
   * Supports complex queries for analytics and reporting.
   *
   * @param filters - Query filters (policy number, dates, product, LOB, movement type, amounts)
   * @param pagination - Page number and page size
   * @param sorting - Optional sorting configuration
   * @returns Premium records matching query criteria with pagination info
   */
  async queryPremiums(params: {
    policyNumber?: number;
    startDate?: string;
    endDate?: string;
    productCode?: number;
    lineOfBusiness?: number;
    movementType?: 'E' | 'C' | 'R' | 'S' | 'A';
    minAmount?: number;
    maxAmount?: number;
    sortBy?: 'referenceDate' | 'policyNumber' | 'premiumAmount';
    sortOrder?: 'asc' | 'desc';
    page?: number;
    pageSize?: number;
  }): Promise<PremiumQueryResponse> {
    try {
      const response = await apiClient.get<PremiumQueryResponse>('/api/v1/premiums', {
        params: {
          policyNumber: params.policyNumber,
          startDate: params.startDate,
          endDate: params.endDate,
          productCode: params.productCode,
          lineOfBusiness: params.lineOfBusiness,
          movementType: params.movementType,
          minAmount: params.minAmount,
          maxAmount: params.maxAmount,
          sortBy: params.sortBy || 'referenceDate',
          sortOrder: params.sortOrder || 'desc',
          page: params.page || 1,
          pageSize: params.pageSize || 20,
        },
      });
      return response.data;
    } catch (error) {
      console.error('[Query Service] Error querying premiums:', error);
      throw error;
    }
  }

  /**
   * Get detailed information for a specific premium record by ID.
   *
   * @param premiumId - Premium record identifier
   * @returns Detailed premium record information
   */
  async getPremiumById(premiumId: number): Promise<PremiumRecord> {
    try {
      const response = await apiClient.get<PremiumRecord>(`/api/v1/premiums/${premiumId}`);
      return response.data;
    } catch (error) {
      console.error(`[Query Service] Error getting premium ${premiumId}:`, error);
      throw error;
    }
  }

  /**
   * Get aggregated premium statistics grouped by various dimensions.
   * Useful for dashboard charts and analytics.
   *
   * @param startDate - Statistics period start date (YYYY-MM-DD)
   * @param endDate - Statistics period end date (YYYY-MM-DD)
   * @param groupBy - Grouping dimension (product, lineOfBusiness, movementType, month)
   * @returns Aggregated statistics with totals, averages, and counts
   */
  async getPremiumStatistics(params: {
    startDate: string;
    endDate: string;
    groupBy: 'product' | 'lineOfBusiness' | 'movementType' | 'month';
  }): Promise<PremiumStatisticsResponse[]> {
    try {
      const response = await apiClient.get<{ statistics: PremiumStatisticsResponse[] }>(
        '/api/v1/premiums/statistics',
        {
          params: {
            startDate: params.startDate,
            endDate: params.endDate,
            groupBy: params.groupBy,
          },
        }
      );
      return response.data.statistics;
    } catch (error) {
      console.error('[Query Service] Error getting premium statistics:', error);
      throw error;
    }
  }

  // ==================== Policy Queries ====================

  /**
   * Get comprehensive policy information by policy number.
   * Includes coverages, endorsements, and related client data.
   *
   * @param policyNumber - Policy identification number
   * @returns Complete policy details with relationships
   */
  async getPolicyByNumber(policyNumber: number): Promise<Policy> {
    try {
      const response = await apiClient.get<Policy>(`/api/v1/policies/${policyNumber}`);
      return response.data;
    } catch (error) {
      console.error(`[Query Service] Error getting policy ${policyNumber}:`, error);
      throw error;
    }
  }

  /**
   * Search policies by various criteria.
   *
   * @param params - Search parameters
   * @returns List of policies matching search criteria
   */
  async searchPolicies(params: {
    clientCode?: number;
    productCode?: number;
    startDate?: string;
    endDate?: string;
    status?: string;
    page?: number;
    pageSize?: number;
  }): Promise<{ policies: Policy[]; pagination: { totalRecords: number; currentPage: number; totalPages: number } }> {
    try {
      const response = await apiClient.get<{
        policies: Policy[];
        pagination: { totalRecords: number; currentPage: number; totalPages: number };
      }>('/api/v1/policies', {
        params: {
          clientCode: params.clientCode,
          productCode: params.productCode,
          startDate: params.startDate,
          endDate: params.endDate,
          status: params.status,
          page: params.page || 1,
          pageSize: params.pageSize || 20,
        },
      });
      return response.data;
    } catch (error) {
      console.error('[Query Service] Error searching policies:', error);
      throw error;
    }
  }

  /**
   * Get endorsements for a specific policy.
   *
   * @param policyNumber - Policy identification number
   * @returns List of endorsements for the policy
   */
  async getPolicyEndorsements(policyNumber: number): Promise<any[]> {
    try {
      const response = await apiClient.get<any[]>(`/api/v1/policies/${policyNumber}/endorsements`);
      return response.data;
    } catch (error) {
      console.error(`[Query Service] Error getting endorsements for policy ${policyNumber}:`, error);
      throw error;
    }
  }

  // ==================== Product Queries ====================

  /**
   * Get list of all available products.
   * Supports filtering by line of business.
   *
   * @param lineOfBusiness - Optional filter by SUSEP line of business code
   * @returns List of products
   */
  async getProducts(params?: {
    lineOfBusiness?: number;
    status?: string;
  }): Promise<Product[]> {
    try {
      const response = await apiClient.get<{ products: Product[] }>('/api/v1/products', {
        params: {
          lineOfBusiness: params?.lineOfBusiness,
          status: params?.status,
        },
      });
      return response.data.products;
    } catch (error) {
      console.error('[Query Service] Error getting products:', error);
      throw error;
    }
  }

  /**
   * Get detailed product information by product code.
   *
   * @param productCode - Product identification code
   * @returns Product details
   */
  async getProductByCode(productCode: number): Promise<Product> {
    try {
      const response = await apiClient.get<Product>(`/api/v1/products/${productCode}`);
      return response.data;
    } catch (error) {
      console.error(`[Query Service] Error getting product ${productCode}:`, error);
      throw error;
    }
  }

  /**
   * Get all available lines of business.
   *
   * @returns List of lines of business with codes and descriptions
   */
  async getLinesOfBusiness(): Promise<Array<{ code: number; name: string; group: number }>> {
    try {
      const response = await apiClient.get<{ linesOfBusiness: Array<{ code: number; name: string; group: number }> }>(
        '/api/v1/products/lines-of-business'
      );
      return response.data.linesOfBusiness;
    } catch (error) {
      console.error('[Query Service] Error getting lines of business:', error);
      throw error;
    }
  }

  // ==================== Client Queries ====================

  /**
   * Search clients by name or document number.
   * Supports partial matching for flexible search.
   *
   * @param searchTerm - Client name or document number
   * @returns List of clients matching search term
   */
  async searchClients(params: {
    searchTerm?: string;
    name?: string;
    documentNumber?: string;
    clientType?: 'F' | 'J';
    page?: number;
    pageSize?: number;
  }): Promise<{ clients: Client[]; pagination: { totalRecords: number; currentPage: number; totalPages: number } }> {
    try {
      const response = await apiClient.get<{
        clients: Client[];
        pagination: { totalRecords: number; currentPage: number; totalPages: number };
      }>('/api/v1/clients', {
        params: {
          search: params.searchTerm,
          name: params.name,
          documentNumber: params.documentNumber,
          clientType: params.clientType,
          page: params.page || 1,
          pageSize: params.pageSize || 20,
        },
      });
      return response.data;
    } catch (error) {
      console.error('[Query Service] Error searching clients:', error);
      throw error;
    }
  }

  /**
   * Get detailed client information by client code.
   * Includes addresses and associated policies.
   *
   * @param clientCode - Client identification code
   * @returns Complete client details with relationships
   */
  async getClientByCode(clientCode: number): Promise<Client> {
    try {
      const response = await apiClient.get<Client>(`/api/v1/clients/${clientCode}`);
      return response.data;
    } catch (error) {
      console.error(`[Query Service] Error getting client ${clientCode}:`, error);
      throw error;
    }
  }

  /**
   * Get all policies associated with a client.
   *
   * @param clientCode - Client identification code
   * @returns List of policies for the client
   */
  async getClientPolicies(clientCode: number): Promise<Policy[]> {
    try {
      const response = await apiClient.get<{ policies: Policy[] }>(`/api/v1/clients/${clientCode}/policies`);
      return response.data.policies;
    } catch (error) {
      console.error(`[Query Service] Error getting policies for client ${clientCode}:`, error);
      throw error;
    }
  }

  /**
   * Get addresses for a specific client.
   *
   * @param clientCode - Client identification code
   * @returns List of addresses for the client
   */
  async getClientAddresses(clientCode: number): Promise<any[]> {
    try {
      const response = await apiClient.get<{ addresses: any[] }>(`/api/v1/clients/${clientCode}/addresses`);
      return response.data.addresses;
    } catch (error) {
      console.error(`[Query Service] Error getting addresses for client ${clientCode}:`, error);
      throw error;
    }
  }

  // ==================== Export Functionality ====================

  /**
   * Export query results to CSV format.
   * Downloads file directly to browser.
   *
   * @param queryType - Type of data to export (premiums, policies, products, clients)
   * @param filters - Same filters as used in query methods
   */
  async exportToCSV(params: {
    queryType: 'premiums' | 'policies' | 'products' | 'clients';
    filters: Record<string, any>;
  }): Promise<Blob> {
    try {
      const response = await apiClient.post<Blob>(
        '/api/v1/query/export',
        {
          queryType: params.queryType,
          format: 'CSV',
          filters: params.filters,
        },
        {
          responseType: 'blob',
        }
      );
      return response.data;
    } catch (error) {
      console.error('[Query Service] Error exporting to CSV:', error);
      throw error;
    }
  }

  /**
   * Export query results to Excel format.
   * Downloads file directly to browser.
   *
   * @param queryType - Type of data to export (premiums, policies, products, clients)
   * @param filters - Same filters as used in query methods
   */
  async exportToExcel(params: {
    queryType: 'premiums' | 'policies' | 'products' | 'clients';
    filters: Record<string, any>;
  }): Promise<Blob> {
    try {
      const response = await apiClient.post<Blob>(
        '/api/v1/query/export',
        {
          queryType: params.queryType,
          format: 'EXCEL',
          filters: params.filters,
        },
        {
          responseType: 'blob',
        }
      );
      return response.data;
    } catch (error) {
      console.error('[Query Service] Error exporting to Excel:', error);
      throw error;
    }
  }

  // ==================== Helper Methods ====================

  /**
   * Download blob as file with specified filename.
   * Helper method for export functionality.
   *
   * @param blob - File blob data
   * @param filename - Desired filename
   */
  downloadFile(blob: Blob, filename: string): void {
    const url = window.URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = filename;
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
    window.URL.revokeObjectURL(url);
  }

  /**
   * Format date to ISO 8601 string (YYYY-MM-DD).
   *
   * @param date - Date object or string
   * @returns Formatted date string
   */
  formatDateForAPI(date: Date | string): string {
    if (typeof date === 'string') {
      return date;
    }
    return date.toISOString().split('T')[0];
  }

  /**
   * Validate date range for queries.
   * Ensures start date is before end date and dates are valid.
   *
   * @param startDate - Start date string
   * @param endDate - End date string
   * @returns Validation result with error message if invalid
   */
  validateDateRange(startDate: string, endDate: string): { isValid: boolean; error?: string } {
    const start = new Date(startDate);
    const end = new Date(endDate);

    if (isNaN(start.getTime())) {
      return { isValid: false, error: 'Data inicial inválida' };
    }

    if (isNaN(end.getTime())) {
      return { isValid: false, error: 'Data final inválida' };
    }

    if (start > end) {
      return { isValid: false, error: 'Data inicial deve ser anterior à data final' };
    }

    return { isValid: true };
  }
}

export default new QueryService();
