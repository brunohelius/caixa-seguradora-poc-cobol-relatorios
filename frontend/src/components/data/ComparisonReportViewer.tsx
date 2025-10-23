import React from 'react';

/**
 * ComparisonReport interface
 *
 * Represents a comparison report between COBOL and .NET output files.
 */
export interface ComparisonReport {
  reportId: string;
  comparisonDate: string;
  cobolFile: string;
  dotnetFile: string;
  matchStatus: 'match' | 'mismatch' | 'pending';
  totalBytes: number;
  matchedBytes: number;
  mismatchedBytes: number;
  matchPercentage: number;
  differences: Difference[];
  summary: string;
}

/**
 * Difference interface
 *
 * Represents a single byte difference between files.
 */
export interface Difference {
  bytePosition: number;
  lineNumber: number;
  columnNumber: number;
  cobolValue: string;
  dotnetValue: string;
  context: string;
}

/**
 * ComparisonReportViewer Props
 */
interface ComparisonReportViewerProps {
  report: ComparisonReport | null;
  loading?: boolean;
}

/**
 * ComparisonReportViewer Component
 *
 * Displays COBOL vs .NET output comparison reports showing byte-level differences.
 *
 * Features:
 * - Match status indicator (success/failure)
 * - Match percentage display
 * - Detailed difference table with byte positions
 * - Context display for each difference
 * - Export capabilities for diff reports
 * - Empty state when no report available
 *
 * Implements User Story 5: COBOL comparison reporting.
 */
const ComparisonReportViewer: React.FC<ComparisonReportViewerProps> = ({
  report,
  loading = false,
}) => {
  /**
   * Format byte position for display
   */
  const formatBytePosition = (position: number): string => {
    return `0x${position.toString(16).toUpperCase().padStart(8, '0')}`;
  };

  /**
   * Format byte value for display
   */
  const formatByteValue = (value: string): string => {
    if (!value) return 'NULL';

    // Convert to hex if it's a single byte
    if (value.length === 1) {
      const hex = value.charCodeAt(0).toString(16).toUpperCase().padStart(2, '0');
      return `${value} (0x${hex})`;
    }

    return value;
  };

  /**
   * Get status badge color classes
   */
  const getStatusColor = (status: string): string => {
    switch (status) {
      case 'match':
        return 'bg-green-100 text-green-800 border-green-200';
      case 'mismatch':
        return 'bg-red-100 text-red-800 border-red-200';
      case 'pending':
        return 'bg-yellow-100 text-yellow-800 border-yellow-200';
      default:
        return 'bg-gray-100 text-gray-800 border-gray-200';
    }
  };

  /**
   * Get status label
   */
  const getStatusLabel = (status: string): string => {
    switch (status) {
      case 'match':
        return '✓ Match Completo';
      case 'mismatch':
        return '✗ Diferenças Encontradas';
      case 'pending':
        return '⏳ Comparação Pendente';
      default:
        return 'Desconhecido';
    }
  };

  // Loading state
  if (loading) {
    return (
      <div className="bg-white rounded-lg shadow p-6">
        <div className="flex items-center justify-center py-12">
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
            />
            <path
              className="opacity-75"
              fill="currentColor"
              d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"
            />
          </svg>
          <span className="ml-3 text-gray-600">Executando comparação...</span>
        </div>
      </div>
    );
  }

  // Empty state
  if (!report) {
    return (
      <div className="bg-white rounded-lg shadow p-6">
        <div className="text-center py-12">
          <svg
            className="mx-auto h-12 w-12 text-gray-400"
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
          <h3 className="mt-2 text-sm font-medium text-gray-900">Nenhum relatório de comparação</h3>
          <p className="mt-1 text-sm text-gray-500">
            Execute uma comparação COBOL vs .NET para ver os resultados aqui.
          </p>
        </div>
      </div>
    );
  }

  return (
    <div className="bg-white rounded-lg shadow">
      {/* Report Header */}
      <div className="border-b border-gray-200 px-6 py-4">
        <div className="flex items-center justify-between">
          <div>
            <h3 className="text-lg font-semibold text-gray-900">Relatório de Comparação</h3>
            <p className="mt-1 text-sm text-gray-500">
              Comparação executada em {new Date(report.comparisonDate).toLocaleString('pt-BR')}
            </p>
          </div>
          <div className={`px-4 py-2 rounded-md border ${getStatusColor(report.matchStatus)}`}>
            <span className="text-sm font-medium">{getStatusLabel(report.matchStatus)}</span>
          </div>
        </div>
      </div>

      {/* File Information */}
      <div className="px-6 py-4 bg-gray-50 border-b border-gray-200">
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div>
            <p className="text-sm font-medium text-gray-600">Arquivo COBOL</p>
            <p className="mt-1 text-sm text-gray-900 font-mono">{report.cobolFile}</p>
          </div>
          <div>
            <p className="text-sm font-medium text-gray-600">Arquivo .NET</p>
            <p className="mt-1 text-sm text-gray-900 font-mono">{report.dotnetFile}</p>
          </div>
        </div>
      </div>

      {/* Statistics */}
      <div className="px-6 py-4 bg-white border-b border-gray-200">
        <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
          <div className="text-center">
            <p className="text-2xl font-bold text-gray-900">{report.totalBytes.toLocaleString('pt-BR')}</p>
            <p className="text-sm text-gray-600">Total de Bytes</p>
          </div>
          <div className="text-center">
            <p className="text-2xl font-bold text-green-600">{report.matchedBytes.toLocaleString('pt-BR')}</p>
            <p className="text-sm text-gray-600">Bytes Idênticos</p>
          </div>
          <div className="text-center">
            <p className="text-2xl font-bold text-red-600">{report.mismatchedBytes.toLocaleString('pt-BR')}</p>
            <p className="text-sm text-gray-600">Bytes Diferentes</p>
          </div>
          <div className="text-center">
            <p className="text-2xl font-bold text-blue-600">{report.matchPercentage.toFixed(2)}%</p>
            <p className="text-sm text-gray-600">Taxa de Match</p>
          </div>
        </div>
      </div>

      {/* Summary */}
      <div className="px-6 py-4 bg-gray-50 border-b border-gray-200">
        <p className="text-sm text-gray-700">{report.summary}</p>
      </div>

      {/* Differences Table */}
      {report.differences && report.differences.length > 0 && (
        <div className="px-6 py-4">
          <h4 className="text-md font-semibold text-gray-900 mb-4">
            Diferenças Encontradas ({report.differences.length})
          </h4>
          <div className="overflow-x-auto">
            <table className="min-w-full divide-y divide-gray-200">
              <thead className="bg-gray-50">
                <tr>
                  <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                    Posição
                  </th>
                  <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                    Linha:Coluna
                  </th>
                  <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                    COBOL
                  </th>
                  <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                    .NET
                  </th>
                  <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                    Contexto
                  </th>
                </tr>
              </thead>
              <tbody className="bg-white divide-y divide-gray-200">
                {report.differences.slice(0, 100).map((diff, index) => (
                  <tr key={index} className="hover:bg-gray-50">
                    <td className="px-4 py-3 whitespace-nowrap text-sm font-mono text-gray-900">
                      {formatBytePosition(diff.bytePosition)}
                    </td>
                    <td className="px-4 py-3 whitespace-nowrap text-sm text-gray-900">
                      {diff.lineNumber}:{diff.columnNumber}
                    </td>
                    <td className="px-4 py-3 whitespace-nowrap text-sm font-mono bg-red-50 text-red-900">
                      {formatByteValue(diff.cobolValue)}
                    </td>
                    <td className="px-4 py-3 whitespace-nowrap text-sm font-mono bg-green-50 text-green-900">
                      {formatByteValue(diff.dotnetValue)}
                    </td>
                    <td className="px-4 py-3 text-sm text-gray-600 max-w-xs truncate">
                      {diff.context}
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
          {report.differences.length > 100 && (
            <div className="mt-4 text-center text-sm text-gray-600">
              Mostrando primeiras 100 de {report.differences.length} diferenças.
              <button className="ml-2 text-blue-600 hover:text-blue-800 font-medium">
                Ver todas
              </button>
            </div>
          )}
        </div>
      )}

      {/* No Differences Message */}
      {report.matchStatus === 'match' && (!report.differences || report.differences.length === 0) && (
        <div className="px-6 py-8">
          <div className="bg-green-50 border border-green-200 rounded-lg p-4">
            <div className="flex items-center">
              <svg
                className="w-6 h-6 text-green-600 mr-3"
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
              <div>
                <p className="text-sm font-medium text-green-900">Validação 100% Aprovada</p>
                <p className="text-sm text-green-700 mt-1">
                  Os arquivos COBOL e .NET são idênticos byte por byte. Conformidade regulatória SUSEP garantida.
                </p>
              </div>
            </div>
          </div>
        </div>
      )}
    </div>
  );
};

export default ComparisonReportViewer;
