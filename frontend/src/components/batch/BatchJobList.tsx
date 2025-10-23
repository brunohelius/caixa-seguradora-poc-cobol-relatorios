import { useState } from 'react';
import type { BatchJobResponseDto } from '../../services/batchJobService';

interface BatchJobListProps {
  jobs: BatchJobResponseDto[];
  onToggleStatus: (jobId: number, enabled: boolean) => void;
  onEdit: (jobId: number) => void;
  onDelete: (jobId: number) => void;
  onExecuteNow: (jobId: number) => void;
  isLoading?: boolean;
}

/**
 * Table component for displaying batch jobs with actions.
 * Shows job name, schedule, status, next run, and action buttons.
 */
export default function BatchJobList({
  jobs,
  onToggleStatus,
  onEdit,
  onDelete,
  onExecuteNow,
  isLoading = false,
}: BatchJobListProps) {
  const [confirmDelete, setConfirmDelete] = useState<number | null>(null);

  /**
   * Format date/time for display
   */
  const formatDateTime = (dateString?: string): string => {
    if (!dateString) return '-';

    try {
      const date = new Date(dateString);
      return date.toLocaleString('pt-BR', {
        day: '2-digit',
        month: '2-digit',
        year: 'numeric',
        hour: '2-digit',
        minute: '2-digit',
      });
    } catch {
      return dateString;
    }
  };

  /**
   * Get recurrence pattern label in Portuguese
   */
  const getRecurrenceLabel = (pattern: string): string => {
    const labels: Record<string, string> = {
      ONCE: 'Uma Vez',
      DAILY: 'Diariamente',
      WEEKLY: 'Semanalmente',
      MONTHLY: 'Mensalmente',
    };
    return labels[pattern] || pattern;
  };

  /**
   * Get status badge color
   */
  const getStatusBadgeClass = (status: string): string => {
    const classes: Record<string, string> = {
      ACTIVE: 'bg-green-100 text-green-800',
      PAUSED: 'bg-yellow-100 text-yellow-800',
      COMPLETED: 'bg-blue-100 text-blue-800',
      FAILED: 'bg-red-100 text-red-800',
    };
    return classes[status] || 'bg-gray-100 text-gray-800';
  };

  /**
   * Get status label in Portuguese
   */
  const getStatusLabel = (status: string): string => {
    const labels: Record<string, string> = {
      ACTIVE: 'Ativo',
      PAUSED: 'Pausado',
      COMPLETED: 'Concluído',
      FAILED: 'Falhou',
    };
    return labels[status] || status;
  };

  /**
   * Handle delete confirmation
   */
  const handleDeleteClick = (jobId: number) => {
    setConfirmDelete(jobId);
  };

  /**
   * Confirm deletion
   */
  const handleConfirmDelete = (jobId: number) => {
    onDelete(jobId);
    setConfirmDelete(null);
  };

  /**
   * Cancel deletion
   */
  const handleCancelDelete = () => {
    setConfirmDelete(null);
  };

  if (isLoading) {
    return (
      <div className="bg-white shadow-md rounded-lg p-8 flex items-center justify-center">
        <div className="flex items-center space-x-3">
          <svg
            className="animate-spin h-8 w-8 text-blue-600"
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
          <span className="text-gray-600">Carregando trabalhos...</span>
        </div>
      </div>
    );
  }

  if (jobs.length === 0) {
    return (
      <div className="bg-white shadow-md rounded-lg p-8 text-center">
        <svg
          className="mx-auto h-12 w-12 text-gray-400"
          fill="none"
          viewBox="0 0 24 24"
          stroke="currentColor"
        >
          <path
            strokeLinecap="round"
            strokeLinejoin="round"
            strokeWidth={2}
            d="M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2M9 5a2 2 0 002 2h2a2 2 0 002-2M9 5a2 2 0 012-2h2a2 2 0 012 2"
          />
        </svg>
        <h3 className="mt-2 text-sm font-medium text-gray-900">
          Nenhum trabalho em lote encontrado
        </h3>
        <p className="mt-1 text-sm text-gray-500">
          Comece criando um novo trabalho em lote agendado.
        </p>
      </div>
    );
  }

  return (
    <div className="bg-white shadow-md rounded-lg overflow-hidden">
      <div className="px-6 py-4 border-b border-gray-200">
        <h2 className="text-xl font-bold text-gray-800">
          Trabalhos em Lote ({jobs.length})
        </h2>
      </div>

      <div className="overflow-x-auto">
        <table className="min-w-full divide-y divide-gray-200">
          <thead className="bg-gray-50">
            <tr>
              <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                Nome
              </th>
              <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                Agendamento
              </th>
              <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                Status
              </th>
              <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                Próxima Execução
              </th>
              <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                Última Execução
              </th>
              <th className="px-6 py-3 text-right text-xs font-medium text-gray-500 uppercase tracking-wider">
                Ações
              </th>
            </tr>
          </thead>
          <tbody className="bg-white divide-y divide-gray-200">
            {jobs.map((job) => (
              <tr key={job.jobId} className="hover:bg-gray-50">
                <td className="px-6 py-4 whitespace-nowrap">
                  <div className="flex flex-col">
                    <div className="text-sm font-medium text-gray-900">
                      {job.jobName}
                    </div>
                    <div className="text-xs text-gray-500">
                      {job.description}
                    </div>
                  </div>
                </td>
                <td className="px-6 py-4 whitespace-nowrap">
                  <div className="text-sm text-gray-900">
                    {getRecurrenceLabel(job.recurrencePattern)}
                  </div>
                </td>
                <td className="px-6 py-4 whitespace-nowrap">
                  <span
                    className={`px-2 inline-flex text-xs leading-5 font-semibold rounded-full ${getStatusBadgeClass(
                      job.status
                    )}`}
                  >
                    {getStatusLabel(job.status)}
                  </span>
                  <div className="mt-1">
                    <label className="inline-flex items-center cursor-pointer">
                      <input
                        type="checkbox"
                        checked={job.isEnabled}
                        onChange={(e) => onToggleStatus(job.jobId, e.target.checked)}
                        className="sr-only peer"
                      />
                      <div className="relative w-11 h-6 bg-gray-200 peer-focus:outline-none peer-focus:ring-4 peer-focus:ring-blue-300 rounded-full peer peer-checked:after:translate-x-full rtl:peer-checked:after:-translate-x-full peer-checked:after:border-white after:content-[''] after:absolute after:top-[2px] after:start-[2px] after:bg-white after:border-gray-300 after:border after:rounded-full after:h-5 after:w-5 after:transition-all peer-checked:bg-blue-600"></div>
                      <span className="ms-2 text-xs text-gray-600">
                        {job.isEnabled ? 'Habilitado' : 'Desabilitado'}
                      </span>
                    </label>
                  </div>
                </td>
                <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-500">
                  {formatDateTime(job.nextExecutionTime)}
                </td>
                <td className="px-6 py-4 whitespace-nowrap">
                  <div className="text-sm text-gray-500">
                    {formatDateTime(job.lastExecutionTime)}
                  </div>
                  {job.latestExecution && (
                    <div className="text-xs text-gray-400">
                      {job.latestExecution.recordsProcessed.toLocaleString('pt-BR')} registros
                    </div>
                  )}
                </td>
                <td className="px-6 py-4 whitespace-nowrap text-right text-sm font-medium">
                  {confirmDelete === job.jobId ? (
                    <div className="flex justify-end gap-2">
                      <button
                        onClick={() => handleConfirmDelete(job.jobId)}
                        className="text-red-600 hover:text-red-900"
                      >
                        Confirmar
                      </button>
                      <button
                        onClick={handleCancelDelete}
                        className="text-gray-600 hover:text-gray-900"
                      >
                        Cancelar
                      </button>
                    </div>
                  ) : (
                    <div className="flex justify-end gap-2">
                      <button
                        onClick={() => onExecuteNow(job.jobId)}
                        className="text-green-600 hover:text-green-900"
                        title="Executar Agora"
                      >
                        <svg
                          className="h-5 w-5"
                          fill="none"
                          viewBox="0 0 24 24"
                          stroke="currentColor"
                        >
                          <path
                            strokeLinecap="round"
                            strokeLinejoin="round"
                            strokeWidth={2}
                            d="M14.752 11.168l-3.197-2.132A1 1 0 0010 9.87v4.263a1 1 0 001.555.832l3.197-2.132a1 1 0 000-1.664z"
                          />
                          <path
                            strokeLinecap="round"
                            strokeLinejoin="round"
                            strokeWidth={2}
                            d="M21 12a9 9 0 11-18 0 9 9 0 0118 0z"
                          />
                        </svg>
                      </button>
                      <button
                        onClick={() => onEdit(job.jobId)}
                        className="text-blue-600 hover:text-blue-900"
                        title="Editar"
                      >
                        <svg
                          className="h-5 w-5"
                          fill="none"
                          viewBox="0 0 24 24"
                          stroke="currentColor"
                        >
                          <path
                            strokeLinecap="round"
                            strokeLinejoin="round"
                            strokeWidth={2}
                            d="M11 5H6a2 2 0 00-2 2v11a2 2 0 002 2h11a2 2 0 002-2v-5m-1.414-9.414a2 2 0 112.828 2.828L11.828 15H9v-2.828l8.586-8.586z"
                          />
                        </svg>
                      </button>
                      <button
                        onClick={() => handleDeleteClick(job.jobId)}
                        className="text-red-600 hover:text-red-900"
                        title="Excluir"
                      >
                        <svg
                          className="h-5 w-5"
                          fill="none"
                          viewBox="0 0 24 24"
                          stroke="currentColor"
                        >
                          <path
                            strokeLinecap="round"
                            strokeLinejoin="round"
                            strokeWidth={2}
                            d="M19 7l-.867 12.142A2 2 0 0116.138 21H7.862a2 2 0 01-1.995-1.858L5 7m5 4v6m4-6v6m1-10V4a1 1 0 00-1-1h-4a1 1 0 00-1 1v3M4 7h16"
                          />
                        </svg>
                      </button>
                    </div>
                  )}
                </td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </div>
  );
}
