import React, { useState } from 'react';
import queryService from '../services/queryService';
import QueryFilterForm, { type QueryFilters } from '../components/query/QueryFilterForm';
import QueryResultsTable from '../components/query/QueryResultsTable';
import QueryVisualizationPanel from '../components/query/QueryVisualizationPanel';
import ExportButtonGroup from '../components/query/ExportButtonGroup';
import { Alert, AlertDescription, AlertTitle } from '../components/ui/alert';
import { Card, CardContent } from '../components/ui/card';
import { Tabs, TabsList, TabsTrigger, TabsContent } from '../components/ui/tabs';
import { Button } from '../components/ui/button';
import { Input } from '../components/ui/input';
import { Label } from '../components/ui/label';
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
  const [queryTypeTab, setQueryTypeTab] = useState('premiums');

  // Query executed flag (to enable visualizations)
  const [queryExecuted, setQueryExecuted] = useState(false);

  // States for other query types
  const [policySearch, setPolicySearch] = useState({ type: 'policy', term: '' });
  const [productSearch, setProductSearch] = useState('');
  const [clientSearch, setClientSearch] = useState({ type: 'cpfcnpj', term: '' });

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

        {/* Query Type Tabs */}
        <Card className="mb-6">
          <Tabs value={queryTypeTab} onValueChange={setQueryTypeTab}>
            <TabsList className="w-full justify-start rounded-none border-b bg-white px-6">
              <TabsTrigger value="premiums" className="data-[state=active]:border-b-2 data-[state=active]:border-caixa-blue">
                Consulta de Prêmios
              </TabsTrigger>
              <TabsTrigger value="policies" className="data-[state=active]:border-b-2 data-[state=active]:border-caixa-blue">
                Consulta de Apólices
              </TabsTrigger>
              <TabsTrigger value="products" className="data-[state=active]:border-b-2 data-[state=active]:border-caixa-blue">
                Produtos
              </TabsTrigger>
              <TabsTrigger value="clients" className="data-[state=active]:border-b-2 data-[state=active]:border-caixa-blue">
                Clientes
              </TabsTrigger>
            </TabsList>

            {/* Premiums Tab Content */}
            <TabsContent value="premiums" className="mt-6">
              <QueryFilterForm
                onSearch={handleSearch}
                onClear={handleClear}
                loading={loading}
              />
            </TabsContent>

            {/* Policies Tab Content */}
            <TabsContent value="policies" className="mt-6">
              <div className="card-modern p-6">
                <h3 className="text-lg font-semibold mb-4">Buscar Apólice</h3>
                <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                  <div>
                    <Label>Tipo de Busca</Label>
                    <select
                      value={policySearch.type}
                      onChange={(e) => setPolicySearch({ ...policySearch, type: e.target.value })}
                      className="input-modern w-full mt-1"
                    >
                      <option value="policy">Número da Apólice</option>
                      <option value="client">CPF/CNPJ do Cliente</option>
                      <option value="product">Código do Produto</option>
                    </select>
                  </div>
                  <div className="md:col-span-2">
                    <Label>Termo de Busca</Label>
                    <div className="flex gap-2 mt-1">
                      <Input
                        type="text"
                        placeholder={
                          policySearch.type === 'policy' ? 'Digite o número da apólice' :
                          policySearch.type === 'client' ? 'Digite o CPF/CNPJ' :
                          'Digite o código do produto'
                        }
                        value={policySearch.term}
                        onChange={(e) => setPolicySearch({ ...policySearch, term: e.target.value })}
                        className="input-modern"
                      />
                      <Button className="btn btn-primary">Buscar</Button>
                    </div>
                  </div>
                </div>
                <div className="mt-6 text-center text-gray-500">
                  <p>Digite os critérios acima e clique em Buscar</p>
                </div>
              </div>
            </TabsContent>

            {/* Products Tab Content */}
            <TabsContent value="products" className="mt-6">
              <div className="card-modern p-6">
                <h3 className="text-lg font-semibold mb-4">Buscar Produto</h3>
                <div className="space-y-4">
                  <div>
                    <Label>Código ou Nome do Produto</Label>
                    <div className="flex gap-2 mt-1">
                      <Input
                        type="text"
                        placeholder="Digite o código ou nome do produto"
                        value={productSearch}
                        onChange={(e) => setProductSearch(e.target.value)}
                        className="input-modern flex-1"
                      />
                      <Button className="btn btn-primary">Buscar</Button>
                      <Button className="btn btn-outline">Listar Todos</Button>
                    </div>
                  </div>
                </div>
                <div className="mt-6 text-center text-gray-500">
                  <p>Digite o código ou nome do produto para buscar</p>
                </div>
              </div>
            </TabsContent>

            {/* Clients Tab Content */}
            <TabsContent value="clients" className="mt-6">
              <div className="card-modern p-6">
                <h3 className="text-lg font-semibold mb-4">Buscar Cliente</h3>
                <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                  <div>
                    <Label>Tipo de Busca</Label>
                    <select
                      value={clientSearch.type}
                      onChange={(e) => setClientSearch({ ...clientSearch, type: e.target.value })}
                      className="input-modern w-full mt-1"
                    >
                      <option value="cpfcnpj">CPF/CNPJ</option>
                      <option value="name">Nome</option>
                    </select>
                  </div>
                  <div className="md:col-span-2">
                    <Label>Termo de Busca</Label>
                    <div className="flex gap-2 mt-1">
                      <Input
                        type="text"
                        placeholder={
                          clientSearch.type === 'cpfcnpj'
                            ? 'Digite o CPF ou CNPJ'
                            : 'Digite o nome do cliente'
                        }
                        value={clientSearch.term}
                        onChange={(e) => setClientSearch({ ...clientSearch, term: e.target.value })}
                        className="input-modern"
                      />
                      <Button className="btn btn-primary">Buscar</Button>
                    </div>
                  </div>
                </div>
                <div className="mt-6 text-center text-gray-500">
                  <p>Digite os critérios acima e clique em Buscar</p>
                </div>
              </div>
            </TabsContent>
          </Tabs>
        </Card>

        {/* Error Display */}
        {error && (
          <Alert variant="destructive" className="mb-6">
            <svg
              className="w-5 h-5"
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
            <AlertTitle>Erro na Consulta</AlertTitle>
            <AlertDescription>{error}</AlertDescription>
          </Alert>
        )}

        {/* Results/Visualizations Tabs */}
        {queryExecuted && (
          <div className="mb-6">
            <Tabs value={activeTab} onValueChange={(value) => setActiveTab(value as 'results' | 'visualizations')}>
              <TabsList className="bg-white">
                <TabsTrigger value="results">Resultados</TabsTrigger>
                <TabsTrigger value="visualizations">Visualizações</TabsTrigger>
              </TabsList>

              <TabsContent value="results" className="space-y-6">
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
              </TabsContent>

              <TabsContent value="visualizations" className="space-y-6">
                <QueryVisualizationPanel
                  startDate={currentFilters.startDate || ''}
                  endDate={currentFilters.endDate || ''}
                  enabled={queryExecuted}
                />
              </TabsContent>
            </Tabs>
          </div>
        )}

        {/* Empty State (no query executed yet) */}
        {!queryExecuted && !loading && (
          <Card className="p-12">
            <CardContent className="text-center">
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
                <Alert variant="info" className="inline-flex items-center">
                  <svg
                    className="w-5 h-5 mr-2"
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
                  <AlertDescription>
                    Dica: Combine múltiplos filtros para refinar sua pesquisa
                  </AlertDescription>
                </Alert>
              </div>
            </CardContent>
          </Card>
        )}
      </div>
    </div>
  );
};

export default QueryPage;
