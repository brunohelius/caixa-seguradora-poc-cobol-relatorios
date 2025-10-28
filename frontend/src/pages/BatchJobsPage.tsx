import { useState, useEffect } from 'react';
import BatchJobForm from '../components/batch/BatchJobForm';
import BatchJobList from '../components/batch/BatchJobList';
import ExecutionHistoryTable from '../components/batch/ExecutionHistoryTable';
import { Button } from '../components/ui/button';
import { Alert, AlertDescription } from '../components/ui/alert';
import {
  createBatchJob,
  getBatchJobs,
  updateBatchJob,
  deleteBatchJob,
  executeBatchJob,
  getJobExecutions,
  toggleBatchJobStatus,
  pollExecutionStatus,
  type BatchJobRequestDto,
  type BatchJobResponseDto,
  type BatchJobExecutionDto,
} from '../services/batchJobService';

/**
 * Main page for batch job management.
 * Orchestrates job creation, listing, execution, and history display.
 */
export default function BatchJobsPage() {
  // UI state
  const [showForm, setShowForm] = useState(false);
  const [editingJobId, setEditingJobId] = useState<number | null>(null);

  // Jobs state
  const [jobs, setJobs] = useState<BatchJobResponseDto[]>([]);
  const [jobsLoading, setJobsLoading] = useState(false);
  const [isSubmitting, setIsSubmitting] = useState(false);

  // Selected job and its executions
  const [selectedJobId, setSelectedJobId] = useState<number | null>(null);
  const [executions, setExecutions] = useState<BatchJobExecutionDto[]>([]);
  const [executionsLoading, setExecutionsLoading] = useState(false);

  // Execution monitoring
  const [monitoringExecutionId, setMonitoringExecutionId] = useState<number | null>(null);

  /**
   * Load all batch jobs on mount
   */
  useEffect(() => {
    loadJobs();
  }, []);

  /**
   * Load executions when a job is selected
   */
  useEffect(() => {
    if (selectedJobId) {
      loadExecutions(selectedJobId);
    }
  }, [selectedJobId]);

  /**
   * Fetch all batch jobs from API
   */
  const loadJobs = async () => {
    try {
      setJobsLoading(true);
      const fetchedJobs = await getBatchJobs();
      setJobs(fetchedJobs);
    } catch (error) {
      console.error('Error loading batch jobs:', error);
    } finally {
      setJobsLoading(false);
    }
  };

  /**
   * Fetch execution history for a specific job
   */
  const loadExecutions = async (jobId: number) => {
    try {
      setExecutionsLoading(true);
      const fetchedExecutions = await getJobExecutions(jobId, 50);
      setExecutions(fetchedExecutions);
    } catch (error) {
      console.error(`Error loading executions for job ${jobId}:`, error);
    } finally {
      setExecutionsLoading(false);
    }
  };

  /**
   * Handle batch job form submission (create or update)
   */
  const handleSubmitJob = async (request: BatchJobRequestDto) => {
    try {
      setIsSubmitting(true);

      if (editingJobId) {
        // Update existing job
        await updateBatchJob(editingJobId, {
          jobName: request.jobName,
          description: request.description,
          recurrencePattern: request.recurrencePattern,
          reportParameters: request.reportParameters,
          executionHour: request.executionHour,
          executionMinute: request.executionMinute,
          dayOfWeek: request.dayOfWeek,
          dayOfMonth: request.dayOfMonth,
          notificationRecipients: request.notificationRecipients,
          maxRetries: request.maxRetries,
        });
      } else {
        // Create new job
        await createBatchJob(request);
      }

      // Reload jobs and close form
      await loadJobs();
      setShowForm(false);
      setEditingJobId(null);
    } catch (error) {
      console.error('Error submitting batch job:', error);
    } finally {
      setIsSubmitting(false);
    }
  };

  /**
   * Handle toggling job enabled/disabled status
   */
  const handleToggleStatus = async (jobId: number, enabled: boolean) => {
    try {
      await toggleBatchJobStatus(jobId, enabled);
      await loadJobs();
    } catch (error) {
      console.error(`Error toggling status for job ${jobId}:`, error);
    }
  };

  /**
   * Handle editing a batch job
   */
  const handleEditJob = (jobId: number) => {
    setEditingJobId(jobId);
    setShowForm(true);
    // In a real app, you would fetch the job details here
    // For now, the form will handle initial data if needed
  };

  /**
   * Handle deleting a batch job
   */
  const handleDeleteJob = async (jobId: number) => {
    try {
      await deleteBatchJob(jobId);
      await loadJobs();

      // Clear selection if deleted job was selected
      if (selectedJobId === jobId) {
        setSelectedJobId(null);
        setExecutions([]);
      }
    } catch (error) {
      console.error(`Error deleting job ${jobId}:`, error);
    }
  };

  /**
   * Handle executing a batch job immediately
   */
  const handleExecuteNow = async (jobId: number) => {
    try {
      const execution = await executeBatchJob(jobId, 'admin.user');

      // Start monitoring this execution
      setMonitoringExecutionId(execution.executionId);
      setSelectedJobId(jobId);

      // Start polling for execution status
      startExecutionPolling(jobId, execution.executionId);
    } catch (error) {
      console.error(`Error executing job ${jobId}:`, error);
    }
  };

  /**
   * Start polling for execution status
   */
  const startExecutionPolling = async (jobId: number, executionId: number) => {
    try {
      await pollExecutionStatus(
        jobId,
        executionId,
        (execution: BatchJobExecutionDto) => {
          // Update the executions list with the latest status
          setExecutions((prevExecutions) =>
            prevExecutions.map((exec) =>
              exec.executionId === executionId ? execution : exec
            )
          );
        },
        3000 // Poll every 3 seconds
      );

      // Execution complete - reload jobs to update next execution time
      await loadJobs();
      setMonitoringExecutionId(null);
    } catch (error) {
      console.error('Error polling execution status:', error);
      setMonitoringExecutionId(null);
    }
  };

  /**
   * Handle downloading a report file from execution
   */
  const handleDownloadReport = (executionId: number, filePath: string) => {
    // In a real implementation, this would call an API endpoint to download the file
    console.log(`Downloading report from execution ${executionId}: ${filePath}`);
    // For now, just show an alert
    alert(`Download iniciado para: ${filePath}`);
  };

  /**
   * Handle viewing execution logs
   */
  const handleViewLogs = (executionId: number, logs: string) => {
    // In a real implementation, this would open a modal with the logs
    console.log(`Viewing logs for execution ${executionId}:`, logs);
    alert(`Logs da Execução #${executionId}:\n\n${logs}`);
  };

  /**
   * Handle job selection to view its executions
   * COMMENTED OUT - Reserved for future use
   */
  // const handleSelectJob = (_jobId: number) => {
  //   if (selectedJobId === _jobId) {
  //     // Deselect if clicking the same job
  //     setSelectedJobId(null);
  //     setExecutions([]);
  //   } else {
  //     setSelectedJobId(_jobId);
  //   }
  // };

  return (
    <div className="container mx-auto px-4 py-8 max-w-7xl">
      <div className="mb-8">
        <h1 className="text-3xl font-bold text-gray-800 mb-2">
          Trabalhos em Lote
        </h1>
        <p className="text-gray-600">
          Gerencie trabalhos agendados para geração automática de relatórios
        </p>
      </div>

      {/* Create New Job Button */}
      {!showForm && (
        <div className="mb-6">
          <Button
            onClick={() => {
              setShowForm(true);
              setEditingJobId(null);
            }}
            variant="primary"
            size="medium"
          >
            <span className="flex items-center">
              <svg
                className="h-5 w-5 mr-2"
                fill="none"
                viewBox="0 0 24 24"
                stroke="currentColor"
              >
                <path
                  strokeLinecap="round"
                  strokeLinejoin="round"
                  strokeWidth={2}
                  d="M12 4v16m8-8H4"
                />
              </svg>
              Criar Novo Trabalho em Lote
            </span>
          </Button>
        </div>
      )}

      {/* Job Creation/Edit Form */}
      {showForm && (
        <div className="mb-6">
          <BatchJobForm
            onSubmit={handleSubmitJob}
            isSubmitting={isSubmitting}
            mode={editingJobId ? 'edit' : 'create'}
          />
          <div className="mt-4">
            <Button
              onClick={() => {
                setShowForm(false);
                setEditingJobId(null);
              }}
              variant="ghost"
              size="small"
            >
              Cancelar
            </Button>
          </div>
        </div>
      )}

      {/* Jobs List */}
      <div className="mb-6">
        <BatchJobList
          jobs={jobs}
          onToggleStatus={handleToggleStatus}
          onEdit={handleEditJob}
          onDelete={handleDeleteJob}
          onExecuteNow={handleExecuteNow}
          isLoading={jobsLoading}
        />
      </div>

      {/* Execution History Section */}
      {selectedJobId && (
        <div className="mb-6">
          <div className="mb-4">
            <Button
              onClick={() => setSelectedJobId(null)}
              variant="ghost"
              size="small"
            >
              <svg
                className="h-4 w-4 mr-1"
                fill="none"
                viewBox="0 0 24 24"
                stroke="currentColor"
              >
                <path
                  strokeLinecap="round"
                  strokeLinejoin="round"
                  strokeWidth={2}
                  d="M15 19l-7-7 7-7"
                />
              </svg>
              Voltar para lista de trabalhos
            </Button>
          </div>

          <ExecutionHistoryTable
            executions={executions}
            onDownloadReport={handleDownloadReport}
            onViewLogs={handleViewLogs}
            isLoading={executionsLoading}
          />

          {monitoringExecutionId && (
            <Alert variant="info" className="mt-4">
              <div className="flex items-center">
                <svg
                  className="animate-spin h-5 w-5 text-blue-600 mr-3"
                  xmlns="http://www.w3.org/2000/svg"
                  fill="none"
                  viewBox="0 0 24 24"
                >
                  <circle
                    className="opacity-25"
                    cx="12"
                    cy="12"
                    r="10"
                    stroke="currentColor"
                    strokeWidth="4"
                  ></circle>
                  <path
                    className="opacity-75"
                    fill="currentColor"
                    d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"
                  ></path>
                </svg>
                <AlertDescription>
                  Monitorando execução #{monitoringExecutionId}...
                </AlertDescription>
              </div>
            </Alert>
          )}
        </div>
      )}

      {/* Instructions */}
      {!showForm && jobs.length === 0 && !jobsLoading && (
        <Alert variant="info">
          <AlertDescription>
            <h3 className="text-lg font-semibold mb-3">
              Como usar Trabalhos em Lote
            </h3>
            <ol className="list-decimal list-inside space-y-2">
              <li>
                Clique em "Criar Novo Trabalho em Lote" para agendar um novo trabalho
              </li>
              <li>
                Configure o nome, descrição, padrão de recorrência (diário, semanal, mensal)
              </li>
              <li>
                Defina os parâmetros do relatório (sistema, tipo, modo, empresa)
              </li>
              <li>
                Configure horário de execução e destinatários de notificação
              </li>
              <li>
                Use os botões de ação para executar, editar, pausar ou excluir trabalhos
              </li>
              <li>
                Clique em um trabalho para ver seu histórico de execuções
              </li>
            </ol>
          </AlertDescription>
        </Alert>
      )}
    </div>
  );
}
