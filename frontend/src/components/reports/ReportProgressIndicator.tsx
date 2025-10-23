import type { ReportStatusResponse } from '../../services/types';

interface ReportProgressIndicatorProps {
  status: ReportStatusResponse;
  onCancel?: () => void;
}

/**
 * Component that displays report generation progress with visual indicators.
 * Shows progress bar, status text, phase information, and cancel button.
 */
export default function ReportProgressIndicator({
  status,
  onCancel,
}: ReportProgressIndicatorProps) {
  /**
   * Get status color based on current status
   */
  const getStatusColor = (): string => {
    switch (status.status) {
      case 'QUEUED':
        return 'bg-yellow-500';
      case 'PROCESSING':
        return 'bg-blue-500';
      case 'COMPLETED':
        return 'bg-green-500';
      case 'FAILED':
        return 'bg-red-500';
      default:
        return 'bg-gray-500';
    }
  };

  /**
   * Get status text in Portuguese
   */
  const getStatusText = (): string => {
    switch (status.status) {
      case 'QUEUED':
        return 'Na Fila';
      case 'PROCESSING':
        return 'Processando';
      case 'COMPLETED':
        return 'Concluído';
      case 'FAILED':
        return 'Falhou';
      default:
        return 'Desconhecido';
    }
  };

  /**
   * Get status icon
   */
  const getStatusIcon = () => {
    switch (status.status) {
      case 'QUEUED':
        return (
          <svg className="w-6 h-6 text-yellow-500" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M12 8v4l3 3m6-3a9 9 0 11-18 0 9 9 0 0118 0z" />
          </svg>
        );
      case 'PROCESSING':
        return (
          <svg className="w-6 h-6 text-blue-500 animate-spin" fill="none" viewBox="0 0 24 24">
            <circle className="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" strokeWidth="4"></circle>
            <path className="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"></path>
          </svg>
        );
      case 'COMPLETED':
        return (
          <svg className="w-6 h-6 text-green-500" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z" />
          </svg>
        );
      case 'FAILED':
        return (
          <svg className="w-6 h-6 text-red-500" fill="none" viewBox="0 0 24 24" stroke="currentColor">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M10 14l2-2m0 0l2-2m-2 2l-2-2m2 2l2 2m7-2a9 9 0 11-18 0 9 9 0 0118 0z" />
          </svg>
        );
      default:
        return <></>;
    }
  };

  /**
   * Format elapsed time from start to now or end
   */
  const getElapsedTime = (): string => {
    const start = new Date(status.startTime);
    const end = status.endTime ? new Date(status.endTime) : new Date();
    const diffMs = end.getTime() - start.getTime();
    const diffSeconds = Math.floor(diffMs / 1000);
    const diffMinutes = Math.floor(diffSeconds / 60);
    const diffHours = Math.floor(diffMinutes / 60);

    if (diffHours > 0) {
      return `${diffHours}h ${diffMinutes % 60}m`;
    } else if (diffMinutes > 0) {
      return `${diffMinutes}m ${diffSeconds % 60}s`;
    } else {
      return `${diffSeconds}s`;
    }
  };

  /**
   * Calculate estimated time remaining based on progress
   */
  const getEstimatedTimeRemaining = (): string => {
    if (status.status !== 'PROCESSING' || status.progress === 0) {
      return 'Calculando...';
    }

    const start = new Date(status.startTime);
    const now = new Date();
    const elapsedMs = now.getTime() - start.getTime();
    const estimatedTotalMs = (elapsedMs / status.progress) * 100;
    const remainingMs = estimatedTotalMs - elapsedMs;
    const remainingMinutes = Math.ceil(remainingMs / (1000 * 60));

    if (remainingMinutes < 1) {
      return 'Menos de 1 minuto';
    } else if (remainingMinutes === 1) {
      return '1 minuto';
    } else {
      return `${remainingMinutes} minutos`;
    }
  };

  return (
    <div className="bg-white shadow-md rounded-lg p-6 space-y-4">
      {/* Header with status icon and text */}
      <div className="flex items-center justify-between">
        <div className="flex items-center space-x-3">
          {getStatusIcon()}
          <div>
            <h3 className="text-lg font-semibold text-gray-800">
              Status: {getStatusText()}
            </h3>
            <p className="text-sm text-gray-500">
              Job ID: {status.jobId}
            </p>
          </div>
        </div>

        {/* Cancel button - only show if processing or queued */}
        {(status.status === 'PROCESSING' || status.status === 'QUEUED') && onCancel && (
          <button
            onClick={onCancel}
            className="px-4 py-2 bg-red-100 hover:bg-red-200 text-red-700 rounded-md text-sm font-medium transition-colors"
          >
            Cancelar
          </button>
        )}
      </div>

      {/* Progress bar */}
      <div>
        <div className="flex justify-between text-sm text-gray-600 mb-1">
          <span>Progresso</span>
          <span>{status.progress}%</span>
        </div>
        <div className="w-full bg-gray-200 rounded-full h-4 overflow-hidden">
          <div
            className={`h-full ${getStatusColor()} transition-all duration-500 ease-out`}
            style={{ width: `${status.progress}%` }}
          >
            {status.progress > 10 && (
              <span className="flex items-center justify-center h-full text-xs font-semibold text-white">
                {status.progress}%
              </span>
            )}
          </div>
        </div>
      </div>

      {/* Records processed */}
      <div className="grid grid-cols-2 gap-4 text-sm">
        <div className="bg-gray-50 rounded-md p-3">
          <p className="text-gray-500 mb-1">Registros Processados</p>
          <p className="text-2xl font-bold text-gray-800">
            {status.recordsProcessed.toLocaleString('pt-BR')}
          </p>
        </div>
        <div className="bg-gray-50 rounded-md p-3">
          <p className="text-gray-500 mb-1">Total de Registros</p>
          <p className="text-2xl font-bold text-gray-800">
            {status.totalRecords.toLocaleString('pt-BR')}
          </p>
        </div>
      </div>

      {/* Time information */}
      <div className="grid grid-cols-2 gap-4 text-sm">
        <div>
          <p className="text-gray-500">Tempo Decorrido</p>
          <p className="text-lg font-semibold text-gray-800">{getElapsedTime()}</p>
        </div>
        {status.status === 'PROCESSING' && (
          <div>
            <p className="text-gray-500">Tempo Estimado Restante</p>
            <p className="text-lg font-semibold text-gray-800">{getEstimatedTimeRemaining()}</p>
          </div>
        )}
      </div>

      {/* Start and end times */}
      <div className="border-t pt-3 space-y-1 text-xs text-gray-500">
        <p>Início: {new Date(status.startTime).toLocaleString('pt-BR')}</p>
        {status.endTime && (
          <p>Término: {new Date(status.endTime).toLocaleString('pt-BR')}</p>
        )}
      </div>

      {/* Error message if failed */}
      {status.status === 'FAILED' && status.errorMessage && (
        <div className="bg-red-50 border border-red-200 rounded-md p-4">
          <h4 className="text-red-800 font-semibold mb-1">Erro:</h4>
          <p className="text-red-700 text-sm">{status.errorMessage}</p>
        </div>
      )}

      {/* Files generated on completion */}
      {status.status === 'COMPLETED' && status.filesGenerated && status.filesGenerated.length > 0 && (
        <div className="bg-green-50 border border-green-200 rounded-md p-4">
          <h4 className="text-green-800 font-semibold mb-2">Arquivos Gerados:</h4>
          <ul className="list-disc list-inside text-green-700 text-sm space-y-1">
            {status.filesGenerated.map((file, index) => (
              <li key={index}>{file}</li>
            ))}
          </ul>
        </div>
      )}
    </div>
  );
}
