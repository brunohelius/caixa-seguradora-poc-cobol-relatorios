import apiClient from './apiClient';
import pt from '../i18n/pt-BR.json';

/**
 * Service for SUSEP premium report generation (feature 003).
 * Matches OpenAPI specification from specs/003-complete-cobol-migration/contracts/openapi.yaml
 */

// Types matching OpenAPI spec
export interface GenerateReportRequest {
  month: string; // YYYYMM format
  reportType: 'PREMIT' | 'PREMCED' | 'BOTH';
  executionMode?: 'BATCH' | 'INTERACTIVE';
}

export interface ReportExecution {
  executionId: string; // UUID
  month: string;
  status: 'PENDING' | 'RUNNING' | 'COMPLETED' | 'FAILED';
  startTime: string; // ISO 8601 date-time
  endTime?: string | null;
  recordsProcessed: number;
  warnings: number;
  errors: number;
  returnCode?: '0000' | '0004' | '0008' | '0012' | null;
  message: string;
}

export interface ReportExecutionSummary {
  executionId: string;
  month: string;
  status: 'PENDING' | 'RUNNING' | 'COMPLETED' | 'FAILED';
  startTime: string;
  recordsProcessed: number;
}

export interface ExecutionListResponse {
  items: ReportExecutionSummary[];
  totalCount: number;
  page: number;
  pageSize: number;
  totalPages: number;
}

export interface ApiErrorDetails {
  [key: string]: string | number | boolean | null;
}

export interface ErrorResponse {
  errorCode: string;
  message: string;
  details?: ApiErrorDetails;
}

/**
 * Generate SUSEP premium reports for a specific month.
 * Returns 202 Accepted with execution ID for status polling.
 *
 * @param request - Report generation parameters
 * @returns Promise with execution details
 */
export async function generateReport(
  request: GenerateReportRequest
): Promise<ReportExecution> {
  try {
    const response = await apiClient.post<ReportExecution>(
      '/api/v1/reports/generate',
      request
    );
    return response.data;
  } catch (error) {
    console.error('[reportServiceV2] Error generating report:', error);

    const errorMessage = error instanceof Error ? error.message : String(error);
    const errorStatus = (error as { status?: number }).status;

    // Map backend errors to Portuguese
    if (errorStatus === 400) {
      if (errorMessage.includes('future')) {
        throw new Error(pt.errors.futureMonth);
      }
      if (errorMessage.includes('invalid') || errorMessage.includes('inválido')) {
        throw new Error(pt.errors.invalidMonth);
      }
    }

    if (errorStatus === 409) {
      throw new Error(pt.errors.alreadyProcessing);
    }

    if (errorStatus === 0) {
      throw new Error(pt.errors.networkError);
    }

    throw new Error(errorMessage || pt.errors.internalError);
  }
}

/**
 * Get execution status for polling.
 * Poll this endpoint every 2 seconds while status is RUNNING.
 *
 * @param executionId - UUID from generateReport response
 * @returns Promise with current execution status
 */
export async function getExecutionStatus(
  executionId: string
): Promise<ReportExecution> {
  try {
    const response = await apiClient.get<ReportExecution>(
      `/api/v1/reports/executions/${executionId}`
    );
    return response.data;
  } catch (error) {
    console.error('[reportServiceV2] Error fetching execution status:', error);

    const errorMessage = error instanceof Error ? error.message : String(error);
    const errorStatus = (error as { status?: number }).status;

    if (errorStatus === 404) {
      throw new Error(pt.errors.executionNotFound);
    }

    throw new Error(errorMessage || pt.errors.internalError);
  }
}

/**
 * Download generated file (PREMIT.TXT or PREMCED.TXT).
 * Execution must be in COMPLETED status.
 *
 * @param executionId - UUID of completed execution
 * @param fileType - Which file to download
 * @returns Promise with blob data
 */
export async function downloadFile(
  executionId: string,
  fileType: 'PREMIT' | 'PREMCED'
): Promise<Blob> {
  try {
    const response = await apiClient.get(
      `/api/v1/reports/executions/${executionId}/download/${fileType}`,
      {
        responseType: 'blob',
      }
    );
    return response.data;
  } catch (error) {
    console.error('[reportServiceV2] Error downloading file:', error);

    const errorMessage = error instanceof Error ? error.message : String(error);
    const errorStatus = (error as { status?: number }).status;

    if (errorStatus === 404) {
      throw new Error(pt.errors.fileNotFound);
    }

    if (errorStatus === 400) {
      throw new Error(pt.errors.executionNotCompleted);
    }

    throw new Error(errorMessage || pt.errors.internalError);
  }
}

/**
 * Trigger browser download for a generated file.
 *
 * @param executionId - UUID of completed execution
 * @param fileType - Which file to download
 * @param customFilename - Optional custom filename
 */
export async function triggerDownload(
  executionId: string,
  fileType: 'PREMIT' | 'PREMCED',
  customFilename?: string
): Promise<void> {
  try {
    const blob = await downloadFile(executionId, fileType);
    const url = window.URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;

    // Default filename format: PREMIT_202510.TXT
    const execution = await getExecutionStatus(executionId);
    const filename = customFilename || `${fileType}_${execution.month}.TXT`;
    link.download = filename;

    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
    window.URL.revokeObjectURL(url);
  } catch (error) {
    console.error('[reportServiceV2] Error triggering download:', error);
    throw error;
  }
}

/**
 * Get paginated execution history.
 *
 * @param page - Page number (1-based)
 * @param pageSize - Items per page (max 100)
 * @param filters - Optional filters (month, status)
 * @returns Promise with paginated execution list
 */
export async function getExecutionHistory(
  page: number = 1,
  pageSize: number = 20,
  filters?: {
    month?: string;
    status?: 'PENDING' | 'RUNNING' | 'COMPLETED' | 'FAILED';
  }
): Promise<ExecutionListResponse> {
  try {
    const params: Record<string, string | number> = { page, pageSize };
    if (filters?.month) params.month = filters.month;
    if (filters?.status) params.status = filters.status;

    const response = await apiClient.get<ExecutionListResponse>(
      '/api/v1/reports/executions',
      { params }
    );
    return response.data;
  } catch (error) {
    console.error('[reportServiceV2] Error fetching execution history:', error);
    const errorMessage = error instanceof Error ? error.message : String(error);
    throw new Error(errorMessage || pt.errors.loadHistoryError);
  }
}

/**
 * Poll execution status until completion (COMPLETED or FAILED).
 * Calls onProgress callback on each status update.
 *
 * @param executionId - UUID to poll
 * @param onProgress - Callback invoked on each update
 * @param intervalMs - Polling interval (default: 2000ms as per spec)
 * @returns Promise that resolves with final status
 */
export async function pollExecutionStatus(
  executionId: string,
  onProgress: (status: ReportExecution) => void,
  intervalMs: number = 2000
): Promise<ReportExecution> {
  return new Promise((resolve, reject) => {
    const poll = async () => {
      try {
        const status = await getExecutionStatus(executionId);
        onProgress(status);

        // Terminal states
        if (status.status === 'COMPLETED' || status.status === 'FAILED') {
          resolve(status);
        } else {
          // Continue polling
          setTimeout(poll, intervalMs);
        }
      } catch (error) {
        reject(error);
      }
    };

    // Start polling
    poll();
  });
}

/**
 * Calculate elapsed time in human-readable format.
 *
 * @param startTime - ISO 8601 start timestamp
 * @param endTime - ISO 8601 end timestamp (optional, uses now if not provided)
 * @returns Formatted duration string
 */
export function formatElapsedTime(startTime: string, endTime?: string | null): string {
  const start = new Date(startTime);
  const end = endTime ? new Date(endTime) : new Date();
  const diffMs = end.getTime() - start.getTime();

  const seconds = Math.floor(diffMs / 1000);
  const minutes = Math.floor(seconds / 60);
  const hours = Math.floor(minutes / 60);

  if (hours > 0) {
    return `${hours}h ${minutes % 60}m ${seconds % 60}s`;
  } else if (minutes > 0) {
    return `${minutes}m ${seconds % 60}s`;
  } else {
    return `${seconds}s`;
  }
}

/**
 * Format return code with description in Portuguese.
 *
 * @param returnCode - COBOL-style return code
 * @returns Description in Portuguese
 */
export function formatReturnCode(returnCode: '0000' | '0004' | '0008' | '0012' | null | undefined): string {
  if (!returnCode) return '';
  return pt.returnCodes[returnCode] || returnCode;
}

export default {
  generateReport,
  getExecutionStatus,
  downloadFile,
  triggerDownload,
  getExecutionHistory,
  pollExecutionStatus,
  formatElapsedTime,
  formatReturnCode,
};
