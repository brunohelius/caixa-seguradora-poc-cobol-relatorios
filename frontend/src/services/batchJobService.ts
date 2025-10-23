import apiClient from './apiClient';
// Types are defined locally in this file as DTOs

/**
 * Service for batch job management and scheduled report generation.
 * Handles interaction with /api/batch-jobs endpoints.
 */

/**
 * Request DTO for creating a new batch job.
 */
export interface BatchJobRequestDto {
  jobName: string;
  description: string;
  recurrencePattern: 'ONCE' | 'DAILY' | 'WEEKLY' | 'MONTHLY';
  reportParameters: {
    systemId: string;
    reportType: 'PREMIT' | 'PREMCED' | 'BOTH';
    mode: 'EMISSION' | 'CANCELLATION' | 'ALL';
    companyCode?: number;
    startDate?: string;
    endDate?: string;
  };
  executionHour?: number; // 0-23
  executionMinute?: number; // 0-59
  dayOfWeek?: number; // 0-6 (Sunday = 0)
  dayOfMonth?: number; // 1-31
  notificationRecipients: string[]; // Email addresses
  maxRetries: number;
  createdBy: string;
}

/**
 * Response DTO for batch job details.
 */
export interface BatchJobResponseDto {
  jobId: number;
  jobName: string;
  description: string;
  recurrencePattern: string;
  status: 'ACTIVE' | 'PAUSED' | 'COMPLETED' | 'FAILED';
  nextExecutionTime?: string;
  lastExecutionTime?: string;
  createdBy: string;
  createdDate: string;
  isEnabled: boolean;
  retryCount: number;
  maxRetries: number;
  notificationRecipients: string[];
  latestExecution?: BatchJobExecutionSummaryDto;
}

/**
 * Summary DTO for the latest execution.
 */
export interface BatchJobExecutionSummaryDto {
  executionId: number;
  startTime: string;
  endTime?: string;
  status: 'RUNNING' | 'COMPLETED' | 'FAILED';
  recordsProcessed: number;
  errorMessage?: string;
}

/**
 * Full execution details DTO.
 */
export interface BatchJobExecutionDto {
  executionId: number;
  jobId: number;
  startTime: string;
  endTime?: string;
  status: 'RUNNING' | 'COMPLETED' | 'FAILED';
  errorMessage?: string;
  outputFilePath?: string;
  recordsProcessed: number;
  executedBy: string;
  executionLog?: string;
}

/**
 * Request DTO for updating a batch job schedule.
 */
export interface UpdateJobScheduleDto {
  jobName?: string;
  description?: string;
  recurrencePattern?: 'ONCE' | 'DAILY' | 'WEEKLY' | 'MONTHLY';
  reportParameters?: {
    systemId?: string;
    reportType?: 'PREMIT' | 'PREMCED' | 'BOTH';
    mode?: 'EMISSION' | 'CANCELLATION' | 'ALL';
    companyCode?: number;
    startDate?: string;
    endDate?: string;
  };
  executionHour?: number;
  executionMinute?: number;
  dayOfWeek?: number;
  dayOfMonth?: number;
  notificationRecipients?: string[];
  maxRetries?: number;
}

/**
 * Request DTO for executing a batch job.
 */
export interface ExecuteJobRequestDto {
  executedBy: string;
}

/**
 * Create a new batch job with specified schedule and parameters.
 *
 * @param request - Job configuration including recurrence pattern and report parameters
 * @returns Promise with created job details
 */
export async function createBatchJob(
  request: BatchJobRequestDto
): Promise<BatchJobResponseDto> {
  try {
    const response = await apiClient.post<BatchJobResponseDto>(
      '/batch-jobs',
      request
    );
    return response.data;
  } catch (error) {
    console.error('[batchJobService] Error creating batch job:', error);
    throw error;
  }
}

/**
 * Get all batch jobs, optionally filtered by status or creator.
 *
 * @param status - Optional filter by status (ACTIVE, PAUSED, COMPLETED, FAILED)
 * @param createdBy - Optional filter by creator username
 * @returns Promise with list of batch jobs
 */
export async function getBatchJobs(
  status?: string,
  createdBy?: string
): Promise<BatchJobResponseDto[]> {
  try {
    const params: Record<string, string> = {};
    if (status) params.status = status;
    if (createdBy) params.createdBy = createdBy;

    const response = await apiClient.get<BatchJobResponseDto[]>('/batch-jobs', {
      params,
    });
    return response.data;
  } catch (error) {
    console.error('[batchJobService] Error fetching batch jobs:', error);
    throw error;
  }
}

/**
 * Get a specific batch job by ID.
 *
 * @param jobId - Job identifier
 * @returns Promise with batch job details including latest execution
 */
export async function getBatchJob(jobId: number): Promise<BatchJobResponseDto> {
  try {
    const response = await apiClient.get<BatchJobResponseDto>(
      `/batch-jobs/${jobId}`
    );
    return response.data;
  } catch (error) {
    console.error(
      `[batchJobService] Error fetching batch job ${jobId}:`,
      error
    );
    throw error;
  }
}

/**
 * Update a batch job schedule and parameters.
 *
 * @param jobId - Job identifier
 * @param request - Updated job configuration
 * @returns Promise with updated job details
 */
export async function updateBatchJob(
  jobId: number,
  request: UpdateJobScheduleDto
): Promise<BatchJobResponseDto> {
  try {
    const response = await apiClient.put<BatchJobResponseDto>(
      `/batch-jobs/${jobId}`,
      request
    );
    return response.data;
  } catch (error) {
    console.error(`[batchJobService] Error updating batch job ${jobId}:`, error);
    throw error;
  }
}

/**
 * Delete a batch job and all its execution history.
 *
 * @param jobId - Job identifier
 * @returns Promise that resolves when deletion is complete
 */
export async function deleteBatchJob(jobId: number): Promise<void> {
  try {
    await apiClient.delete(`/batch-jobs/${jobId}`);
  } catch (error) {
    console.error(`[batchJobService] Error deleting batch job ${jobId}:`, error);
    throw error;
  }
}

/**
 * Manually trigger immediate execution of a batch job.
 *
 * @param jobId - Job identifier
 * @param executedBy - Username of person triggering the execution
 * @returns Promise with execution record
 */
export async function executeBatchJob(
  jobId: number,
  executedBy: string
): Promise<BatchJobExecutionDto> {
  try {
    const response = await apiClient.post<BatchJobExecutionDto>(
      `/batch-jobs/${jobId}/execute`,
      { executedBy }
    );
    return response.data;
  } catch (error) {
    console.error(`[batchJobService] Error executing batch job ${jobId}:`, error);
    throw error;
  }
}

/**
 * Get execution history for a specific batch job.
 *
 * @param jobId - Job identifier
 * @param limit - Maximum number of executions to return (default 50)
 * @returns Promise with list of executions
 */
export async function getJobExecutions(
  jobId: number,
  limit: number = 50
): Promise<BatchJobExecutionDto[]> {
  try {
    const response = await apiClient.get<BatchJobExecutionDto[]>(
      `/batch-jobs/${jobId}/executions`,
      {
        params: { limit },
      }
    );
    return response.data;
  } catch (error) {
    console.error(
      `[batchJobService] Error fetching executions for job ${jobId}:`,
      error
    );
    throw error;
  }
}

/**
 * Pause a batch job (stops scheduling future executions).
 *
 * @param jobId - Job identifier
 * @returns Promise that resolves when job is paused
 */
export async function pauseBatchJob(jobId: number): Promise<void> {
  try {
    await apiClient.post(`/batch-jobs/${jobId}/pause`);
  } catch (error) {
    console.error(`[batchJobService] Error pausing batch job ${jobId}:`, error);
    throw error;
  }
}

/**
 * Resume a paused batch job.
 *
 * @param jobId - Job identifier
 * @returns Promise that resolves when job is resumed
 */
export async function resumeBatchJob(jobId: number): Promise<void> {
  try {
    await apiClient.post(`/batch-jobs/${jobId}/resume`);
  } catch (error) {
    console.error(`[batchJobService] Error resuming batch job ${jobId}:`, error);
    throw error;
  }
}

/**
 * Toggle batch job enabled state (pause or resume).
 *
 * @param jobId - Job identifier
 * @param enabled - Target state (true = resume, false = pause)
 * @returns Promise that resolves when state is changed
 */
export async function toggleBatchJobStatus(
  jobId: number,
  enabled: boolean
): Promise<void> {
  if (enabled) {
    return resumeBatchJob(jobId);
  } else {
    return pauseBatchJob(jobId);
  }
}

/**
 * Validate batch job request before submission.
 *
 * @param request - The request to validate
 * @returns Validation result with any error messages
 */
export function validateBatchJobRequest(request: BatchJobRequestDto): {
  isValid: boolean;
  errors: string[];
} {
  const errors: string[] = [];

  // Validate job name
  if (!request.jobName || request.jobName.trim() === '') {
    errors.push('Nome do trabalho é obrigatório');
  } else if (request.jobName.length > 100) {
    errors.push('Nome do trabalho deve ter no máximo 100 caracteres');
  }

  // Validate description
  if (!request.description || request.description.trim() === '') {
    errors.push('Descrição é obrigatória');
  }

  // Validate recurrence pattern
  const validPatterns = ['ONCE', 'DAILY', 'WEEKLY', 'MONTHLY'];
  if (!request.recurrencePattern || !validPatterns.includes(request.recurrencePattern)) {
    errors.push('Padrão de recorrência inválido');
  }

  // Validate execution time
  if (request.executionHour !== undefined) {
    if (request.executionHour < 0 || request.executionHour > 23) {
      errors.push('Hora de execução deve estar entre 0 e 23');
    }
  }

  if (request.executionMinute !== undefined) {
    if (request.executionMinute < 0 || request.executionMinute > 59) {
      errors.push('Minuto de execução deve estar entre 0 e 59');
    }
  }

  // Validate day of week (for WEEKLY pattern)
  if (request.recurrencePattern === 'WEEKLY' && request.dayOfWeek !== undefined) {
    if (request.dayOfWeek < 0 || request.dayOfWeek > 6) {
      errors.push('Dia da semana deve estar entre 0 (Domingo) e 6 (Sábado)');
    }
  }

  // Validate day of month (for MONTHLY pattern)
  if (request.recurrencePattern === 'MONTHLY' && request.dayOfMonth !== undefined) {
    if (request.dayOfMonth < 1 || request.dayOfMonth > 31) {
      errors.push('Dia do mês deve estar entre 1 e 31');
    }
  }

  // Validate report parameters
  if (!request.reportParameters) {
    errors.push('Parâmetros do relatório são obrigatórios');
  } else {
    if (!request.reportParameters.systemId || request.reportParameters.systemId.trim() === '') {
      errors.push('ID do sistema é obrigatório');
    }

    const validReportTypes = ['PREMIT', 'PREMCED', 'BOTH'];
    if (!request.reportParameters.reportType || !validReportTypes.includes(request.reportParameters.reportType)) {
      errors.push('Tipo de relatório inválido');
    }

    const validModes = ['EMISSION', 'CANCELLATION', 'ALL'];
    if (!request.reportParameters.mode || !validModes.includes(request.reportParameters.mode)) {
      errors.push('Modo de processamento inválido');
    }
  }

  // Validate notification recipients
  if (!request.notificationRecipients || request.notificationRecipients.length === 0) {
    errors.push('Pelo menos um destinatário de notificação é obrigatório');
  } else {
    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    request.notificationRecipients.forEach((email, index) => {
      if (!emailRegex.test(email)) {
        errors.push(`Email inválido na posição ${index + 1}: ${email}`);
      }
    });
  }

  // Validate max retries
  if (request.maxRetries !== undefined) {
    if (request.maxRetries < 0 || request.maxRetries > 10) {
      errors.push('Número máximo de tentativas deve estar entre 0 e 10');
    }
  }

  // Validate created by
  if (!request.createdBy || request.createdBy.trim() === '') {
    errors.push('Usuário criador é obrigatório');
  }

  return {
    isValid: errors.length === 0,
    errors,
  };
}

/**
 * Poll for job execution status until it reaches a terminal state (COMPLETED or FAILED).
 * Useful for showing live progress updates during manual execution.
 *
 * @param jobId - The job ID to poll
 * @param executionId - The execution ID to poll
 * @param onProgress - Callback invoked on each status update
 * @param intervalMs - Polling interval in milliseconds (default: 3000)
 * @returns Promise that resolves with final execution status
 */
export async function pollExecutionStatus(
  jobId: number,
  executionId: number,
  onProgress: (execution: BatchJobExecutionDto) => void,
  intervalMs: number = 3000
): Promise<BatchJobExecutionDto> {
  return new Promise((resolve, reject) => {
    const poll = async () => {
      try {
        const executions = await getJobExecutions(jobId, 1);
        const currentExecution = executions.find(e => e.executionId === executionId);

        if (!currentExecution) {
          reject(new Error(`Execution ${executionId} not found`));
          return;
        }

        onProgress(currentExecution);

        // Check if execution is in terminal state
        if (currentExecution.status === 'COMPLETED' || currentExecution.status === 'FAILED') {
          resolve(currentExecution);
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

export default {
  createBatchJob,
  getBatchJobs,
  getBatchJob,
  updateBatchJob,
  deleteBatchJob,
  executeBatchJob,
  getJobExecutions,
  pauseBatchJob,
  resumeBatchJob,
  toggleBatchJobStatus,
  validateBatchJobRequest,
  pollExecutionStatus,
};
