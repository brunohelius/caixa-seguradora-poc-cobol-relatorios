import React, { useState } from 'react';
import queryService from '../../services/queryService';
import type { QueryFilters } from './QueryFilterForm';

interface ExportButtonGroupProps {
  queryType: 'premiums' | 'policies' | 'products' | 'clients';
  filters: QueryFilters;
  disabled?: boolean;
  totalRecords?: number;
}

type ExportFormat = 'CSV' | 'EXCEL';

/**
 * ExportButtonGroup Component
 *
 * Export functionality for query results with:
 * - CSV export button
 * - Excel export button
 * - Loading states during export
 * - Automatic file download
 * - Export confirmation with record count
 * - Error handling with user feedback
 *
 * All text in Portuguese for Caixa Seguradora.
 */
const ExportButtonGroup: React.FC<ExportButtonGroupProps> = ({
  queryType,
  filters,
  disabled = false,
  totalRecords = 0,
}) => {
  const [exportingFormat, setExportingFormat] = useState<ExportFormat | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [success, setSuccess] = useState<string | null>(null);

  const handleExport = async (format: ExportFormat) => {
    setExportingFormat(format);
    setError(null);
    setSuccess(null);

    try {
      let blob: Blob;
      let filename: string;

      if (format === 'CSV') {
        blob = await queryService.exportToCSV({
          queryType,
          filters,
        });
        filename = `${queryType}-export-${new Date().toISOString().split('T')[0]}.csv`;
      } else {
        blob = await queryService.exportToExcel({
          queryType,
          filters,
        });
        filename = `${queryType}-export-${new Date().toISOString().split('T')[0]}.xlsx`;
      }

      // Download file
      queryService.downloadFile(blob, filename);

      setSuccess(`Exportação concluída! Arquivo salvo: ${filename}`);

      // Clear success message after 5 seconds
      setTimeout(() => setSuccess(null), 5000);
    } catch (err) {
      console.error('Erro ao exportar:', err);
      setError(`Erro ao exportar para ${format}. Tente novamente.`);
    } finally {
      setExportingFormat(null);
    }
  };

  // Removed unused function: getExportLabel
  // const getExportLabel = (format: ExportFormat): string => {
  //   if (exportingFormat === format) {
  //     return 'Exportando...';
  //   }
  //   return format === 'CSV' ? 'Exportar CSV' : 'Exportar Excel';
  // };

  const getRecordCountMessage = (): string => {
    if (totalRecords === 0) {
      return 'Nenhum registro para exportar';
    }
    if (totalRecords === 1) {
      return '1 registro será exportado';
    }
    return `${totalRecords.toLocaleString('pt-BR')} registros serão exportados`;
  };

  return (
    <div className="bg-white rounded-lg shadow p-6">
      <h3 className="text-lg font-semibold text-gray-800 mb-4">Exportar Resultados</h3>

      {/* Record Count Info */}
      <div className="mb-4 p-3 bg-gray-50 border border-gray-200 rounded-md">
        <p className="text-sm text-gray-700">
          <span className="font-medium">Total de registros:</span>{' '}
          {totalRecords > 0 ? (
            <span className="text-blue-600 font-semibold">
              {totalRecords.toLocaleString('pt-BR')}
            </span>
          ) : (
            <span className="text-gray-500">0</span>
          )}
        </p>
        <p className="text-xs text-gray-500 mt-1">{getRecordCountMessage()}</p>
      </div>

      {/* Export Buttons */}
      <div className="grid grid-cols-1 sm:grid-cols-2 gap-3">
        {/* CSV Export Button */}
        <button
          onClick={() => handleExport('CSV')}
          disabled={disabled || totalRecords === 0 || exportingFormat !== null}
          className="flex items-center justify-center px-4 py-3 border border-gray-300 rounded-md shadow-sm text-sm font-medium text-gray-700 bg-white hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 disabled:bg-gray-100 disabled:text-gray-400 disabled:cursor-not-allowed transition-colors"
        >
          {exportingFormat === 'CSV' ? (
            <>
              <svg
                className="animate-spin -ml-1 mr-3 h-5 w-5 text-blue-600"
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
                />
                <path
                  className="opacity-75"
                  fill="currentColor"
                  d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"
                />
              </svg>
              <span>Exportando...</span>
            </>
          ) : (
            <>
              <svg
                className="mr-2 h-5 w-5 text-green-600"
                fill="none"
                stroke="currentColor"
                viewBox="0 0 24 24"
              >
                <path
                  strokeLinecap="round"
                  strokeLinejoin="round"
                  strokeWidth={2}
                  d="M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z"
                />
              </svg>
              <span>Exportar CSV</span>
            </>
          )}
        </button>

        {/* Excel Export Button */}
        <button
          onClick={() => handleExport('EXCEL')}
          disabled={disabled || totalRecords === 0 || exportingFormat !== null}
          className="flex items-center justify-center px-4 py-3 border border-gray-300 rounded-md shadow-sm text-sm font-medium text-gray-700 bg-white hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 disabled:bg-gray-100 disabled:text-gray-400 disabled:cursor-not-allowed transition-colors"
        >
          {exportingFormat === 'EXCEL' ? (
            <>
              <svg
                className="animate-spin -ml-1 mr-3 h-5 w-5 text-blue-600"
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
                />
                <path
                  className="opacity-75"
                  fill="currentColor"
                  d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"
                />
              </svg>
              <span>Exportando...</span>
            </>
          ) : (
            <>
              <svg
                className="mr-2 h-5 w-5 text-green-700"
                fill="none"
                stroke="currentColor"
                viewBox="0 0 24 24"
              >
                <path
                  strokeLinecap="round"
                  strokeLinejoin="round"
                  strokeWidth={2}
                  d="M3 10h18M3 14h18m-9-4v8m-7 0h14a2 2 0 002-2V8a2 2 0 00-2-2H5a2 2 0 00-2 2v8a2 2 0 002 2z"
                />
              </svg>
              <span>Exportar Excel</span>
            </>
          )}
        </button>
      </div>

      {/* Success Message */}
      {success && (
        <div className="mt-4 p-3 bg-green-50 border border-green-200 rounded-md">
          <div className="flex items-start">
            <svg
              className="w-5 h-5 text-green-600 mt-0.5 mr-3"
              fill="none"
              stroke="currentColor"
              viewBox="0 0 24 24"
            >
              <path
                strokeLinecap="round"
                strokeLinejoin="round"
                strokeWidth={2}
                d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"
              />
            </svg>
            <p className="text-sm text-green-800">{success}</p>
          </div>
        </div>
      )}

      {/* Error Message */}
      {error && (
        <div className="mt-4 p-3 bg-red-50 border border-red-200 rounded-md">
          <div className="flex items-start">
            <svg
              className="w-5 h-5 text-red-600 mt-0.5 mr-3"
              fill="none"
              stroke="currentColor"
              viewBox="0 0 24 24"
            >
              <path
                strokeLinecap="round"
                strokeLinejoin="round"
                strokeWidth={2}
                d="M12 8v4m0 4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"
              />
            </svg>
            <div>
              <p className="text-sm text-red-800">{error}</p>
              <button
                onClick={() => setError(null)}
                className="mt-1 text-xs font-medium text-red-800 hover:text-red-900 underline"
              >
                Fechar
              </button>
            </div>
          </div>
        </div>
      )}

      {/* Export Info */}
      <div className="mt-4 p-3 bg-blue-50 border border-blue-200 rounded-md">
        <div className="flex items-start">
          <svg
            className="w-5 h-5 text-blue-600 mt-0.5 mr-3 flex-shrink-0"
            fill="none"
            stroke="currentColor"
            viewBox="0 0 24 24"
          >
            <path
              strokeLinecap="round"
              strokeLinejoin="round"
              strokeWidth={2}
              d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"
            />
          </svg>
          <div className="text-xs text-blue-800">
            <p className="font-medium mb-1">Sobre a exportação:</p>
            <ul className="list-disc list-inside space-y-1">
              <li>CSV: Formato compatível com Excel, Google Sheets e outras ferramentas</li>
              <li>Excel: Arquivo .xlsx com formatação e fórmulas</li>
              <li>Os dados exportados respeitam os filtros aplicados na pesquisa</li>
              <li>O download inicia automaticamente após a exportação</li>
            </ul>
          </div>
        </div>
      </div>
    </div>
  );
};

export default ExportButtonGroup;
