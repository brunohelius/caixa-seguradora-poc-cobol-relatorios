import React, { useState } from 'react';
import queryService from '../services/queryService';
import QueryFilterForm, { type QueryFilters } from '../components/query/QueryFilterForm';
import QueryResultsTable from '../components/query/QueryResultsTable';
import QueryVisualizationPanel from '../components/query/QueryVisualizationPanel';
import ExportButtonGroup from '../components/query/ExportButtonGroup';
import type { PremiumQueryResponse } from '../services/types';

/**
 * QueryPage Component
 *
 * Main query page composing all query components with tabs for:
 * - Premium queries (default)
 * - Policy lookup (future)
 * - Product browser (future)
 * - Client search (future)
 *
 * Features:
 * - Filter form with comprehensive search options
 * - Results table with sorting and pagination
 * - Statistics visualizations with multiple grouping options
 * - Export functionality (CSV, Excel)
 * - State management for query results
 *
 * Implements User Story 3: Interactive premium data querying and visualization.
 */
const QueryPage: React.FC = () => {
  // Query state
  const [queryResults, setQueryResults] = useState<PremiumQueryResponse | null>(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  // Current filters and pagination
  const [currentFilters, setCurrentFilters] = useState<QueryFilters>({});
  const [currentPage, setCurrentPage] = useState(1);
  const [pageSize] = useState(20);
  const [sortBy, setSortBy] = useState<'referenceDate' | 'policyNumber' | 'premiumAmount'>('referenceDate');
  const [sortOrder, setSortOrder] = useState<'asc' | 'desc'>('desc');

  // Tab state
  const [activeTab, setActiveTab] = useState<'results' | 'visualizations'>('results');

  // Query executed flag (to enable visualizations)
  const [queryExecuted, setQueryExecuted] = useState(false);

  /**
   * Execute premium query with current filters and pagination
   */
  const executeQuery = async (filters: QueryFilters, page: number = 1) => {
    setLoading(true);
    setError(null);

    try {
      const response = await queryService.queryPremiums({
        policyNumber: filters.policyNumber,
        startDate: filters.startDate,
        endDate: filters.endDate,
        productCode: filters.productCode,
        lineOfBusiness: filters.lineOfBusiness,
        movementType: filters.movementType || undefined,
        minAmount: filters.minAmount,
        maxAmount: filters.maxAmount,
        sortBy,
        sortOrder,
        page,
        pageSize,
      });

      setQueryResults(response);
      setCurrentFilters(filters);
      setCurrentPage(page);
      setQueryExecuted(true);
    } catch (err) {
      console.error('Erro ao executar consulta:', err);
      setError('Erro ao executar consulta. Verifique os filtros e tente novamente.');
      setQueryResults(null);
    } finally {
      setLoading(false);
    }
  };

  /**
   * Handle search button click from filter form
   */
  const handleSearch = (filters: QueryFilters) => {
    executeQuery(filters, 1);
  };

  /**
   * Handle clear filters button
   */
  const handleClear = () => {
    setQueryResults(null);
    setCurrentFilters({});
    setCurrentPage(1);
    setError(null);
    setQueryExecuted(false);
  };

  /**
   * Handle page change in results table
   */
  const handlePageChange = (newPage: number) => {
    executeQuery(currentFilters, newPage);
  };

  /**
   * Handle sort change in results table
   */
  const handleSort = (column: string, order: 'asc' | 'desc') => {
    setSortBy(column as any);
    setSortOrder(order);
    // Re-execute query with new sorting
    executeQuery(currentFilters, currentPage);
  };

  return (
    <div className="min-h-screen bg-gray-50 py-8">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        {/* Page Header */}
        <div className="mb-8">
          <h1 className="text-3xl font-bold text-gray-900">Consulta de Dados</h1>
          <p className="mt-2 text-gray-600">
            Pesquise e visualize informações de prêmios, apólices, produtos e clientes
          </p>
        </div>

        {/* Query Type Tabs (future: add Policy, Product, Client tabs) */}
        <div className="bg-white rounded-lg shadow mb-6">
          <div className="border-b border-gray-200">
            <nav className="-mb-px flex space-x-8 px-6" aria-label="Tabs">
              <button
                className="border-blue-500 text-blue-600 whitespace-nowrap py-4 px-1 border-b-2 font-medium text-sm"
              >
                Consulta de Prêmios
              </button>
              <button
                disabled
                className="border-transparent text-gray-400 cursor-not-allowed whitespace-nowrap py-4 px-1 border-b-2 font-medium text-sm"
              >
                Consulta de Apólices (Em breve)
              </button>
              <button
                disabled
                className="border-transparent text-gray-400 cursor-not-allowed whitespace-nowrap py-4 px-1 border-b-2 font-medium text-sm"
              >
                Produtos (Em breve)
              </button>
              <button
                disabled
                className="border-transparent text-gray-400 cursor-not-allowed whitespace-nowrap py-4 px-1 border-b-2 font-medium text-sm"
              >
                Clientes (Em breve)
              </button>
            </nav>
          </div>
        </div>

        {/* Filter Form */}
        <div className="mb-6">
          <QueryFilterForm
            onSearch={handleSearch}
            onClear={handleClear}
            loading={loading}
          />
        </div>

        {/* Error Display */}
        {error && (
          <div className="mb-6 bg-red-50 border border-red-200 rounded-lg p-4">
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
                <h4 className="text-sm font-medium text-red-800">Erro na Consulta</h4>
                <p className="text-sm text-red-700 mt-1">{error}</p>
              </div>
            </div>
          </div>
        )}

        {/* Results/Visualizations Tabs */}
        {queryExecuted && (
          <div className="mb-6">
            <div className="border-b border-gray-200 bg-white rounded-t-lg">
              <nav className="-mb-px flex space-x-8 px-6" aria-label="View Tabs">
                <button
                  onClick={() => setActiveTab('results')}
                  className={`${
                    activeTab === 'results'
                      ? 'border-blue-500 text-blue-600'
                      : 'border-transparent text-gray-500 hover:text-gray-700 hover:border-gray-300'
                  } whitespace-nowrap py-4 px-1 border-b-2 font-medium text-sm transition-colors`}
                >
                  Resultados
                </button>
                <button
                  onClick={() => setActiveTab('visualizations')}
                  className={`${
                    activeTab === 'visualizations'
                      ? 'border-blue-500 text-blue-600'
                      : 'border-transparent text-gray-500 hover:text-gray-700 hover:border-gray-300'
                  } whitespace-nowrap py-4 px-1 border-b-2 font-medium text-sm transition-colors`}
                >
                  Visualizações
                </button>
              </nav>
            </div>
          </div>
        )}

        {/* Tab Content */}
        {activeTab === 'results' && (
          <div className="space-y-6">
            {/* Results Table */}
            <QueryResultsTable
              data={queryResults?.premiums || []}
              totalRecords={queryResults?.pagination.totalRecords || 0}
              currentPage={queryResults?.pagination.currentPage || 1}
              totalPages={queryResults?.pagination.totalPages || 1}
              pageSize={pageSize}
              onPageChange={handlePageChange}
              onSort={handleSort}
              loading={loading}
            />

            {/* Export Button Group */}
            {queryResults && queryResults.premiums.length > 0 && (
              <ExportButtonGroup
                queryType="premiums"
                filters={currentFilters}
                totalRecords={queryResults.pagination.totalRecords}
                disabled={loading}
              />
            )}
          </div>
        )}

        {activeTab === 'visualizations' && (
          <div className="space-y-6">
            <QueryVisualizationPanel
              startDate={currentFilters.startDate || ''}
              endDate={currentFilters.endDate || ''}
              enabled={queryExecuted}
            />
          </div>
        )}

        {/* Empty State (no query executed yet) */}
        {!queryExecuted && !loading && (
          <div className="bg-white rounded-lg shadow p-12">
            <div className="text-center">
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
                  d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z"
                />
              </svg>
              <h3 className="mt-4 text-lg font-medium text-gray-900">
                Nenhuma pesquisa realizada
              </h3>
              <p className="mt-2 text-gray-500 max-w-md mx-auto">
                Use os filtros acima para pesquisar prêmios por apólice, data, produto,
                ramo SUSEP, tipo de movimento ou valor.
              </p>
              <div className="mt-6">
                <div className="inline-flex items-center px-4 py-2 border border-blue-300 rounded-md bg-blue-50">
                  <svg
                    className="w-5 h-5 text-blue-600 mr-2"
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
                  <span className="text-sm text-blue-800">
                    Dica: Combine múltiplos filtros para refinar sua pesquisa
                  </span>
                </div>
              </div>
            </div>
          </div>
        )}
      </div>
    </div>
  );
};

export default QueryPage;
