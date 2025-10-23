import React, { useState } from 'react';
import type { ValidationReport } from '../../services/mockDataService';

export interface ValidationResultsProps {
  report: ValidationReport | null;
  loading: boolean;
}

/**
 * ValidationResults Component
 *
 * Display validation results with summary cards and detailed issue table.
 *
 * Features:
 * - Summary cards showing error/warning/info counts
 * - Detailed error table with columns: Severity, Entity, Row, Field, Message
 * - Filter by severity (All, Errors, Warnings, Info)
 * - Color-coded severity badges
 * - Export errors to CSV button
 * - Pagination for large error lists
 * - Responsive layout
 *
 * Implements User Story 5: Data validation reporting.
 */
const ValidationResults: React.FC<ValidationResultsProps> = ({ report, loading }) => {
  // Filter state
  const [severityFilter, setSeverityFilter] = useState<'ALL' | 'ERROR' | 'WARNING' | 'INFO'>('ALL');
  const [currentPage, setCurrentPage] = useState(1);
  const pageSize = 20;

  // Filter issues by severity
  const filteredIssues = report?.issues.filter((issue) =>
    severityFilter === 'ALL' ? true : issue.severity === severityFilter
  ) || [];

  // Paginate issues
  const totalPages = Math.ceil(filteredIssues.length / pageSize);
  const paginatedIssues = filteredIssues.slice(
    (currentPage - 1) * pageSize,
    currentPage * pageSize
  );

  // Reset to first page when filter changes
  React.useEffect(() => {
    setCurrentPage(1);
  }, [severityFilter]);

  /**
   * Export issues to CSV
   */
  const exportToCSV = () => {
    if (!report || filteredIssues.length === 0) return;

    const headers = ['Severidade', 'Entidade', 'Linha', 'Campo', 'Mensagem', 'Timestamp'];
    const rows = filteredIssues.map((issue) => [
      issue.severity,
      issue.entityType,
      issue.rowNumber?.toString() || '',
      issue.fieldName || '',
      issue.message,
      issue.timestamp ? new Date(issue.timestamp).toLocaleString('pt-BR') : '',
    ]);

    const csvContent = [
      headers.join(';'),
      ...rows.map((row) => row.map((cell) => `"${cell}"`).join(';')),
    ].join('\n');

    const blob = new Blob([csvContent], { type: 'text/csv;charset=utf-8;' });
    const link = document.createElement('a');
    link.href = URL.createObjectURL(blob);
    link.download = `validation-errors-${new Date().toISOString().split('T')[0]}.csv`;
    link.click();
  };

  /**
   * Get severity badge styling
   */
  const getSeverityBadge = (severity: 'ERROR' | 'WARNING' | 'INFO') => {
    const styles = {
      ERROR: 'bg-red-100 text-red-800 border-red-200',
      WARNING: 'bg-yellow-100 text-yellow-800 border-yellow-200',
      INFO: 'bg-blue-100 text-blue-800 border-blue-200',
    };

    const labels = {
      ERROR: 'Erro',
      WARNING: 'Aviso',
      INFO: 'Info',
    };

    return (
      <span
        className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium border ${styles[severity]}`}
      >
        {labels[severity]}
      </span>
    );
  };

  // Loading state
  if (loading) {
    return (
      <div className="bg-white rounded-lg shadow p-12">
        <div className="text-center">
          <svg
            className="animate-spin h-12 w-12 text-blue-600 mx-auto"
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
          <p className="mt-4 text-gray-600">Validando dados...</p>
        </div>
      </div>
    );
  }

  // No report yet
  if (!report) {
    return (
      <div className="bg-white rounded-lg shadow p-12">
        <div className="text-center text-gray-500">
          <svg
            className="mx-auto h-16 w-16 text-gray-400"
            fill="none"
            stroke="currentColor"
            viewBox="0 0 24 24"
          >
            <path
              strokeLinecap="round"
              strokeLinejoin="round"
              strokeWidth={1.5}
              d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"
            />
          </svg>
          <p className="mt-4 text-lg">Nenhuma validação executada ainda</p>
          <p className="mt-2 text-sm">Execute a validação para ver os resultados</p>
        </div>
      </div>
    );
  }

  return (
    <div className="space-y-6">
      {/* Summary Cards */}
      <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
        {/* Overall Status */}
        <div
          className={`rounded-lg shadow p-6 ${
            report.isValid ? 'bg-green-50 border border-green-200' : 'bg-red-50 border border-red-200'
          }`}
        >
          <div className="flex items-center">
            <div className="flex-shrink-0">
              {report.isValid ? (
                <svg
                  className="h-10 w-10 text-green-600"
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
              ) : (
                <svg
                  className="h-10 w-10 text-red-600"
                  fill="none"
                  stroke="currentColor"
                  viewBox="0 0 24 24"
                >
                  <path
                    strokeLinecap="round"
                    strokeLinejoin="round"
                    strokeWidth={2}
                    d="M10 14l2-2m0 0l2-2m-2 2l-2-2m2 2l2 2m7-2a9 9 0 11-18 0 9 9 0 0118 0z"
                  />
                </svg>
              )}
            </div>
            <div className="ml-4">
              <p className="text-sm font-medium text-gray-600">Status</p>
              <p className={`text-xl font-bold ${report.isValid ? 'text-green-900' : 'text-red-900'}`}>
                {report.isValid ? 'Válido' : 'Inválido'}
              </p>
            </div>
          </div>
        </div>

        {/* Errors */}
        <div className="bg-white rounded-lg shadow p-6 border border-gray-200">
          <div className="flex items-center">
            <div className="flex-shrink-0">
              <svg className="h-10 w-10 text-red-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path
                  strokeLinecap="round"
                  strokeLinejoin="round"
                  strokeWidth={2}
                  d="M12 8v4m0 4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"
                />
              </svg>
            </div>
            <div className="ml-4">
              <p className="text-sm font-medium text-gray-600">Erros</p>
              <p className="text-2xl font-bold text-gray-900">{report.errorCount}</p>
            </div>
          </div>
        </div>

        {/* Warnings */}
        <div className="bg-white rounded-lg shadow p-6 border border-gray-200">
          <div className="flex items-center">
            <div className="flex-shrink-0">
              <svg className="h-10 w-10 text-yellow-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path
                  strokeLinecap="round"
                  strokeLinejoin="round"
                  strokeWidth={2}
                  d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z"
                />
              </svg>
            </div>
            <div className="ml-4">
              <p className="text-sm font-medium text-gray-600">Avisos</p>
              <p className="text-2xl font-bold text-gray-900">{report.warningCount}</p>
            </div>
          </div>
        </div>

        {/* Info */}
        <div className="bg-white rounded-lg shadow p-6 border border-gray-200">
          <div className="flex items-center">
            <div className="flex-shrink-0">
              <svg className="h-10 w-10 text-blue-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path
                  strokeLinecap="round"
                  strokeLinejoin="round"
                  strokeWidth={2}
                  d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"
                />
              </svg>
            </div>
            <div className="ml-4">
              <p className="text-sm font-medium text-gray-600">Informações</p>
              <p className="text-2xl font-bold text-gray-900">{report.infoCount}</p>
            </div>
          </div>
        </div>
      </div>

      {/* Entity Statistics */}
      {report.statistics && report.statistics.length > 0 && (
        <div className="bg-white rounded-lg shadow overflow-hidden">
          <div className="px-6 py-4 border-b border-gray-200">
            <h3 className="text-lg font-semibold text-gray-900">Estatísticas por Entidade</h3>
          </div>
          <div className="overflow-x-auto">
            <table className="min-w-full divide-y divide-gray-200">
              <thead className="bg-gray-50">
                <tr>
                  <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                    Entidade
                  </th>
                  <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                    Total
                  </th>
                  <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                    Válidos
                  </th>
                  <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                    Inválidos
                  </th>
                  <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                    Erros
                  </th>
                  <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                    Avisos
                  </th>
                </tr>
              </thead>
              <tbody className="bg-white divide-y divide-gray-200">
                {report.statistics.map((stat) => (
                  <tr key={stat.entityType} className="hover:bg-gray-50">
                    <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900">
                      {stat.entityType}
                    </td>
                    <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-600">
                      {stat.totalRecords}
                    </td>
                    <td className="px-6 py-4 whitespace-nowrap text-sm text-green-600 font-medium">
                      {stat.validRecords}
                    </td>
                    <td className="px-6 py-4 whitespace-nowrap text-sm text-red-600 font-medium">
                      {stat.invalidRecords}
                    </td>
                    <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-600">
                      {stat.errorCount}
                    </td>
                    <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-600">
                      {stat.warningCount}
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </div>
      )}

      {/* Issues Table */}
      {report.issues.length > 0 && (
        <div className="bg-white rounded-lg shadow overflow-hidden">
          <div className="px-6 py-4 border-b border-gray-200">
            <div className="flex flex-col sm:flex-row sm:items-center sm:justify-between">
              <h3 className="text-lg font-semibold text-gray-900 mb-3 sm:mb-0">
                Problemas Detectados ({filteredIssues.length})
              </h3>
              <div className="flex items-center space-x-3">
                {/* Severity Filter */}
                <select
                  value={severityFilter}
                  onChange={(e) => setSeverityFilter(e.target.value as any)}
                  className="rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 sm:text-sm py-1.5 px-3 border"
                >
                  <option value="ALL">Todos</option>
                  <option value="ERROR">Erros ({report.errorCount})</option>
                  <option value="WARNING">Avisos ({report.warningCount})</option>
                  <option value="INFO">Info ({report.infoCount})</option>
                </select>

                {/* Export Button */}
                <button
                  onClick={exportToCSV}
                  disabled={filteredIssues.length === 0}
                  className="inline-flex items-center px-3 py-1.5 border border-gray-300 shadow-sm text-sm font-medium rounded-md text-gray-700 bg-white hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 disabled:opacity-50 disabled:cursor-not-allowed"
                >
                  <svg className="w-4 h-4 mr-1.5" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path
                      strokeLinecap="round"
                      strokeLinejoin="round"
                      strokeWidth={2}
                      d="M12 10v6m0 0l-3-3m3 3l3-3m2 8H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z"
                    />
                  </svg>
                  Exportar CSV
                </button>
              </div>
            </div>
          </div>

          {paginatedIssues.length > 0 ? (
            <>
              <div className="overflow-x-auto">
                <table className="min-w-full divide-y divide-gray-200">
                  <thead className="bg-gray-50">
                    <tr>
                      <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider w-24">
                        Severidade
                      </th>
                      <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                        Entidade
                      </th>
                      <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider w-20">
                        Linha
                      </th>
                      <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                        Campo
                      </th>
                      <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                        Mensagem
                      </th>
                    </tr>
                  </thead>
                  <tbody className="bg-white divide-y divide-gray-200">
                    {paginatedIssues.map((issue, index) => (
                      <tr key={index} className="hover:bg-gray-50">
                        <td className="px-6 py-4 whitespace-nowrap">
                          {getSeverityBadge(issue.severity)}
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-900">
                          {issue.entityType}
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-600">
                          {issue.rowNumber || '-'}
                        </td>
                        <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-900 font-mono">
                          {issue.fieldName || '-'}
                        </td>
                        <td className="px-6 py-4 text-sm text-gray-600">
                          {issue.message}
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>

              {/* Pagination */}
              {totalPages > 1 && (
                <div className="px-6 py-4 border-t border-gray-200 flex items-center justify-between">
                  <div className="text-sm text-gray-700">
                    Mostrando {(currentPage - 1) * pageSize + 1} a{' '}
                    {Math.min(currentPage * pageSize, filteredIssues.length)} de {filteredIssues.length} problemas
                  </div>
                  <div className="flex space-x-2">
                    <button
                      onClick={() => setCurrentPage((p) => Math.max(1, p - 1))}
                      disabled={currentPage === 1}
                      className="px-3 py-1 border border-gray-300 rounded-md text-sm font-medium text-gray-700 bg-white hover:bg-gray-50 disabled:opacity-50 disabled:cursor-not-allowed"
                    >
                      Anterior
                    </button>
                    <span className="px-3 py-1 text-sm text-gray-700">
                      Página {currentPage} de {totalPages}
                    </span>
                    <button
                      onClick={() => setCurrentPage((p) => Math.min(totalPages, p + 1))}
                      disabled={currentPage === totalPages}
                      className="px-3 py-1 border border-gray-300 rounded-md text-sm font-medium text-gray-700 bg-white hover:bg-gray-50 disabled:opacity-50 disabled:cursor-not-allowed"
                    >
                      Próxima
                    </button>
                  </div>
                </div>
              )}
            </>
          ) : (
            <div className="px-6 py-12 text-center text-gray-500">
              <p>Nenhum problema encontrado nesta categoria</p>
            </div>
          )}
        </div>
      )}

      {/* Empty State - All Valid */}
      {report.issues.length === 0 && (
        <div className="bg-green-50 border border-green-200 rounded-lg p-12">
          <div className="text-center">
            <svg
              className="mx-auto h-16 w-16 text-green-600"
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
            <h3 className="mt-4 text-lg font-medium text-green-900">
              Todos os dados estão válidos!
            </h3>
            <p className="mt-2 text-green-700">
              Nenhum problema foi encontrado durante a validação.
            </p>
          </div>
        </div>
      )}
    </div>
  );
};

export default ValidationResults;
