import { useState } from 'react';
import type { ReportStatusResponse } from '../../services/types';

interface ReportResultsCardProps {
  status: ReportStatusResponse;
  onDownload: (fileType: 'PREMIT' | 'PREMCED') => Promise<void>;
}

/**
 * Component that displays completed report results with download buttons.
 * Shows file information and provides download actions.
 */
export default function ReportResultsCard({
  status,
  onDownload,
}: ReportResultsCardProps) {
  const [downloadingFile, setDownloadingFile] = useState<'PREMIT' | 'PREMCED' | null>(null);

  /**
   * Handle download button click
   */
  const handleDownload = async (fileType: 'PREMIT' | 'PREMCED') => {
    try {
      setDownloadingFile(fileType);
      await onDownload(fileType);
    } catch (error) {
      console.error('Error downloading file:', error);
      // Error will be handled by parent component or apiClient interceptor
    } finally {
      setDownloadingFile(null);
    }
  };

  /**
   * Check if a specific file type was generated
   */
  const isFileGenerated = (fileType: 'PREMIT' | 'PREMCED'): boolean => {
    if (!status.filesGenerated) return false;
    return status.filesGenerated.some(file =>
      file.toUpperCase().includes(fileType.toUpperCase())
    );
  };

  // Only show if report is completed
  if (status.status !== 'COMPLETED') {
    return null;
  }

  return (
    <div className="bg-white shadow-md rounded-lg p-6 space-y-4">
      {/* Success header */}
      <div className="flex items-center space-x-3 pb-4 border-b">
        <svg
          className="w-8 h-8 text-green-500"
          fill="none"
          viewBox="0 0 24 24"
          stroke="currentColor"
        >
          <path
            strokeLinecap="round"
            strokeLinejoin="round"
            strokeWidth={2}
            d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"
          />
        </svg>
        <div>
          <h3 className="text-xl font-bold text-gray-800">
            Relatório Concluído com Sucesso!
          </h3>
          <p className="text-sm text-gray-500">
            Os arquivos estão prontos para download
          </p>
        </div>
      </div>

      {/* Summary statistics */}
      <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
        <div className="bg-blue-50 rounded-md p-4">
          <p className="text-sm text-blue-600 font-medium mb-1">Registros Processados</p>
          <p className="text-2xl font-bold text-blue-800">
            {status.recordsProcessed.toLocaleString('pt-BR')}
          </p>
        </div>

        <div className="bg-green-50 rounded-md p-4">
          <p className="text-sm text-green-600 font-medium mb-1">Arquivos Gerados</p>
          <p className="text-2xl font-bold text-green-800">
            {status.filesGenerated?.length || 0}
          </p>
        </div>

        <div className="bg-purple-50 rounded-md p-4">
          <p className="text-sm text-purple-600 font-medium mb-1">Tempo Total</p>
          <p className="text-2xl font-bold text-purple-800">
            {status.startTime && status.endTime ?
              formatDuration(
                new Date(status.endTime).getTime() - new Date(status.startTime).getTime()
              ) :
              'N/A'
            }
          </p>
        </div>
      </div>

      {/* Download section */}
      <div className="space-y-3">
        <h4 className="text-lg font-semibold text-gray-800">Arquivos Disponíveis</h4>

        {/* PREMIT download button */}
        {isFileGenerated('PREMIT') && (
          <div className="flex items-center justify-between bg-gray-50 rounded-md p-4 border border-gray-200">
            <div className="flex items-center space-x-3">
              <svg
                className="w-8 h-8 text-blue-500"
                fill="none"
                viewBox="0 0 24 24"
                stroke="currentColor"
              >
                <path
                  strokeLinecap="round"
                  strokeLinejoin="round"
                  strokeWidth={2}
                  d="M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z"
                />
              </svg>
              <div>
                <p className="font-semibold text-gray-800">PREMIT.TXT</p>
                <p className="text-xs text-gray-500">Relatório de Prêmios Emitidos</p>
              </div>
            </div>

            <button
              onClick={() => handleDownload('PREMIT')}
              disabled={downloadingFile !== null}
              className="flex items-center space-x-2 px-4 py-2 bg-blue-600 hover:bg-blue-700 text-white rounded-md font-medium transition-colors disabled:opacity-50 disabled:cursor-not-allowed"
            >
              {downloadingFile === 'PREMIT' ? (
                <>
                  <svg
                    className="animate-spin h-5 w-5 text-white"
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
                  <span>Baixando...</span>
                </>
              ) : (
                <>
                  <svg
                    className="w-5 h-5"
                    fill="none"
                    viewBox="0 0 24 24"
                    stroke="currentColor"
                  >
                    <path
                      strokeLinecap="round"
                      strokeLinejoin="round"
                      strokeWidth={2}
                      d="M4 16v1a3 3 0 003 3h10a3 3 0 003-3v-1m-4-4l-4 4m0 0l-4-4m4 4V4"
                    />
                  </svg>
                  <span>Download</span>
                </>
              )}
            </button>
          </div>
        )}

        {/* PREMCED download button */}
        {isFileGenerated('PREMCED') && (
          <div className="flex items-center justify-between bg-gray-50 rounded-md p-4 border border-gray-200">
            <div className="flex items-center space-x-3">
              <svg
                className="w-8 h-8 text-purple-500"
                fill="none"
                viewBox="0 0 24 24"
                stroke="currentColor"
              >
                <path
                  strokeLinecap="round"
                  strokeLinejoin="round"
                  strokeWidth={2}
                  d="M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z"
                />
              </svg>
              <div>
                <p className="font-semibold text-gray-800">PREMCED.TXT</p>
                <p className="text-xs text-gray-500">Relatório de Prêmios Cedidos</p>
              </div>
            </div>

            <button
              onClick={() => handleDownload('PREMCED')}
              disabled={downloadingFile !== null}
              className="flex items-center space-x-2 px-4 py-2 bg-purple-600 hover:bg-purple-700 text-white rounded-md font-medium transition-colors disabled:opacity-50 disabled:cursor-not-allowed"
            >
              {downloadingFile === 'PREMCED' ? (
                <>
                  <svg
                    className="animate-spin h-5 w-5 text-white"
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
                  <span>Baixando...</span>
                </>
              ) : (
                <>
                  <svg
                    className="w-5 h-5"
                    fill="none"
                    viewBox="0 0 24 24"
                    stroke="currentColor"
                  >
                    <path
                      strokeLinecap="round"
                      strokeLinejoin="round"
                      strokeWidth={2}
                      d="M4 16v1a3 3 0 003 3h10a3 3 0 003-3v-1m-4-4l-4 4m0 0l-4-4m4 4V4"
                    />
                  </svg>
                  <span>Download</span>
                </>
              )}
            </button>
          </div>
        )}

        {/* No files message */}
        {!isFileGenerated('PREMIT') && !isFileGenerated('PREMCED') && (
          <div className="bg-yellow-50 border border-yellow-200 rounded-md p-4">
            <p className="text-yellow-800 text-sm">
              Nenhum arquivo foi gerado. Isso pode indicar que não havia dados no período especificado.
            </p>
          </div>
        )}
      </div>

      {/* Completion timestamp */}
      <div className="pt-4 border-t text-xs text-gray-500">
        <p>
          Relatório concluído em: {status.endTime ? new Date(status.endTime).toLocaleString('pt-BR') : 'N/A'}
        </p>
      </div>
    </div>
  );
}

/**
 * Format duration in milliseconds to human-readable string
 */
function formatDuration(ms: number): string {
  const seconds = Math.floor(ms / 1000);
  const minutes = Math.floor(seconds / 60);
  const hours = Math.floor(minutes / 60);

  if (hours > 0) {
    return `${hours}h ${minutes % 60}m`;
  } else if (minutes > 0) {
    return `${minutes}m ${seconds % 60}s`;
  } else {
    return `${seconds}s`;
  }
}
