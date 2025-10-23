import apiClient from './apiClient';
import type {
  ReportGenerationRequest,
  ReportGenerationResponse,
  ReportStatusResponse,
  ReportHistoryItem,
  ComparisonResult,
} from './types';

/**
 * Service for premium report generation and management.
 * Handles interaction with /api/v1/reports endpoints.
 */

/**
 * Generate a new premium report (PREMIT.TXT and/or PREMCED.TXT).
 * This endpoint starts an async job and returns a jobId for status tracking.
 *
 * @param request - Report generation parameters
 * @returns Promise with jobId and initial status
 */
export async function generateReport(
  request: ReportGenerationRequest
): Promise<ReportGenerationResponse> {
  try {
    const response = await apiClient.post<ReportGenerationResponse>(
      '/api/Reports/generate',
      request
    );
    return response.data;
  } catch (error) {
    console.error('[reportService] Error generating report:', error);
    throw error;
  }
}

/**
 * Get the status of a report generation job.
 * Use this to poll for progress updates.
 *
 * @param reportId - The jobId returned from generateReport()
 * @returns Promise with current job status and progress
 */
export async function getReportStatus(
  reportId: string
): Promise<ReportStatusResponse> {
  try {
    const response = await apiClient.get<ReportStatusResponse>(
      `/api/Reports/${reportId}`
    );
    return response.data;
  } catch (error) {
    console.error('[reportService] Error fetching report status:', error);
    throw error;
  }
}

/**
 * Download a generated report file.
 * The file will be downloaded as a blob and can be saved to disk.
 *
 * @param reportId - The jobId of the completed report
 * @param fileType - Which file to download ('PREMIT' or 'PREMCED')
 * @returns Promise with blob data
 */
export async function downloadReport(
  reportId: string,
  fileType: 'PREMIT' | 'PREMCED'
): Promise<Blob> {
  try {
    const response = await apiClient.get(
      `/api/Reports/${reportId}/download/${fileType}`,
      {
        responseType: 'blob', // Important: treat response as binary blob
      }
    );
    return response.data;
  } catch (error) {
    console.error('[reportService] Error downloading report:', error);
    throw error;
  }
}

/**
 * Trigger a download of a report file to the user's browser.
 * This is a helper that combines downloadReport() with browser download action.
 *
 * @param reportId - The jobId of the completed report
 * @param fileType - Which file to download ('PREMIT' or 'PREMCED')
 * @param filename - Optional custom filename (defaults to fileType.TXT)
 */
export async function triggerReportDownload(
  reportId: string,
  fileType: 'PREMIT' | 'PREMCED',
  filename?: string
): Promise<void> {
  try {
    const blob = await downloadReport(reportId, fileType);
    const url = window.URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = filename || `${fileType}.TXT`;
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
    window.URL.revokeObjectURL(url);
  } catch (error) {
    console.error('[reportService] Error triggering download:', error);
    throw error;
  }
}

/**
 * Fetch report generation history.
 *
 * @param page - Page number (1-indexed)
 * @param pageSize - Number of records per page
 * @returns Promise with paginated history items
 */
export async function getReportHistory(
  page: number = 1,
  pageSize: number = 10
): Promise<{ items: ReportHistoryItem[]; total: number; page: number; pageSize: number }> {
  try {
    const response = await apiClient.get('/api/Reports/history', {
      params: { page, pageSize },
    });
    return response.data;
  } catch (error) {
    console.error('[reportService] Error fetching report history:', error);
    throw error;
  }
}

/**
 * Compare COBOL-generated report with .NET-generated report.
 * Used for migration validation to ensure byte-for-byte compatibility.
 *
 * @param cobolFilePath - Path to the legacy COBOL report file
 * @param dotnetReportId - The jobId of the .NET report to compare
 * @returns Promise with comparison results
 */
export async function compareReports(
  cobolFilePath: string,
  dotnetReportId: string
): Promise<ComparisonResult> {
  try {
    const response = await apiClient.post<ComparisonResult>(
      '/api/Reports/compare',
      {
        cobolFilePath,
        dotnetReportId,
      }
    );
    return response.data;
  } catch (error) {
    console.error('[reportService] Error comparing reports:', error);
    throw error;
  }
}

/**
 * Cancel a running report generation job.
 *
 * @param reportId - The jobId to cancel
 * @returns Promise that resolves when cancellation is confirmed
 */
export async function cancelReport(reportId: string): Promise<void> {
  try {
    await apiClient.delete(`/api/Reports/${reportId}`);
  } catch (error) {
    console.error('[reportService] Error canceling report:', error);
    throw error;
  }
}

/**
 * Poll for report status until it reaches a terminal state (COMPLETED or FAILED).
 * Useful for showing live progress updates.
 *
 * @param reportId - The jobId to poll
 * @param onProgress - Callback invoked on each status update
 * @param intervalMs - Polling interval in milliseconds (default: 2000)
 * @returns Promise that resolves with final status
 */
export async function pollReportStatus(
  reportId: string,
  onProgress: (status: ReportStatusResponse) => void,
  intervalMs: number = 2000
): Promise<ReportStatusResponse> {
  return new Promise((resolve, reject) => {
    const poll = async () => {
      try {
        const status = await getReportStatus(reportId);
        onProgress(status);

        // Check if job is in terminal state
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
 * Validate report generation request before submission.
 *
 * @param request - The request to validate
 * @returns Validation result with any error messages
 */
export function validateReportRequest(request: ReportGenerationRequest): {
  isValid: boolean;
  errors: string[];
} {
  const errors: string[] = [];

  // Validate systemId
  if (!request.systemId || request.systemId.trim() === '') {
    errors.push('Sistema é obrigatório');
  }

  // Validate date range
  if (!request.startDate) {
    errors.push('Data inicial é obrigatória');
  }
  if (!request.endDate) {
    errors.push('Data final é obrigatória');
  }

  if (request.startDate && request.endDate) {
    const startDate = new Date(request.startDate);
    const endDate = new Date(request.endDate);

    if (startDate > endDate) {
      errors.push('Data inicial não pode ser posterior à data final');
    }

    // Validate date range is not too large (max 1 year)
    const diffTime = Math.abs(endDate.getTime() - startDate.getTime());
    const diffDays = Math.ceil(diffTime / (1000 * 60 * 60 * 24));

    if (diffDays > 365) {
      errors.push('Período máximo permitido é de 1 ano');
    }
  }

  // Validate reportType
  if (!request.reportType || request.reportType === 'BOTH') {
    // BOTH is valid - no error
  } else if (request.reportType !== 'PREMIT' && request.reportType !== 'PREMCED') {
    errors.push('Tipo de relatório inválido');
  }

  // Validate mode
  const validModes = ['EMISSION', 'CANCELLATION', 'ALL'];
  if (!request.mode || !validModes.includes(request.mode)) {
    errors.push('Modo de processamento inválido');
  }

  // Validate companyCode (optional, but if provided must be positive)
  if (request.companyCode !== undefined && request.companyCode <= 0) {
    errors.push('Código da empresa deve ser um número positivo');
  }

  return {
    isValid: errors.length === 0,
    errors,
  };
}

export default {
  generateReport,
  getReportStatus,
  downloadReport,
  triggerReportDownload,
  getReportHistory,
  compareReports,
  cancelReport,
  pollReportStatus,
  validateReportRequest,
};
