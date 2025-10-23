import React, { useState, useEffect } from 'react';
import FileUploadForm from '../components/data/FileUploadForm';
import ValidationResults from '../components/data/ValidationResults';
import mockDataService, { type MockDataStats, type ValidationReport } from '../services/mockDataService';

/**
 * MockDataPage Component
 *
 * Main page for managing database mock data.
 *
 * Features:
 * - File upload form with entity type selection
 * - Validation results display
 * - Database statistics cards (record counts per entity)
 * - Reset database button with confirmation dialog
 * - Upload workflow state management
 * - Success/error notifications
 * - Auto-refresh stats after operations
 *
 * Implements User Story 5: Mock data management interface.
 */
const MockDataPage: React.FC = () => {
  // State management
  const [stats, setStats] = useState<MockDataStats | null>(null);
  const [validationReport, setValidationReport] = useState<ValidationReport | null>(null);
  const [uploadLoading, setUploadLoading] = useState(false);
  const [validationLoading, setValidationLoading] = useState(false);
  const [statsLoading, setStatsLoading] = useState(false);
  const [showResetDialog, setShowResetDialog] = useState(false);
  const [notification, setNotification] = useState<{
    type: 'success' | 'error' | 'info';
    message: string;
  } | null>(null);

  /**
   * Load statistics on mount
   */
  useEffect(() => {
    loadStats();
  }, []);

  /**
   * Load database statistics
   */
  const loadStats = async () => {
    setStatsLoading(true);
    try {
      const data = await mockDataService.getStats();
      setStats(data);
    } catch (error: any) {
      console.error('Error loading stats:', error);
      showNotification('error', 'Erro ao carregar estatísticas do banco de dados.');
    } finally {
      setStatsLoading(false);
    }
  };

  /**
   * Handle file upload
   */
  const handleUpload = async (file: File, entityType: string) => {
    setUploadLoading(true);
    try {
      await mockDataService.loadMockData(file, entityType);
      showNotification('success', `Arquivo carregado com sucesso! ${entityType} importado.`);

      // Refresh stats and clear validation
      await loadStats();
      setValidationReport(null);
    } catch (error: any) {
      console.error('Error uploading file:', error);
      showNotification('error', error.response?.data?.message || 'Erro ao carregar arquivo. Verifique o formato e tente novamente.');
      throw error; // Re-throw to keep FileUploadForm in error state
    } finally {
      setUploadLoading(false);
    }
  };

  /**
   * Handle validation execution
   */
  const handleValidate = async () => {
    setValidationLoading(true);
    try {
      const report = await mockDataService.validateData();
      setValidationReport(report);

      if (report.isValid) {
        showNotification('success', 'Validação concluída! Todos os dados estão válidos.');
      } else {
        showNotification('info', `Validação concluída com ${report.errorCount} erros e ${report.warningCount} avisos.`);
      }
    } catch (error: any) {
      console.error('Error validating data:', error);
      showNotification('error', 'Erro ao validar dados. Tente novamente.');
    } finally {
      setValidationLoading(false);
    }
  };

  /**
   * Handle database reset
   */
  const handleReset = async () => {
    setShowResetDialog(false);
    setStatsLoading(true);

    try {
      await mockDataService.resetDatabase();
      showNotification('success', 'Banco de dados resetado com sucesso!');

      // Refresh stats and clear validation
      await loadStats();
      setValidationReport(null);
    } catch (error: any) {
      console.error('Error resetting database:', error);
      showNotification('error', 'Erro ao resetar banco de dados.');
    } finally {
      setStatsLoading(false);
    }
  };

  /**
   * Show notification
   */
  const showNotification = (type: 'success' | 'error' | 'info', message: string) => {
    setNotification({ type, message });
    setTimeout(() => setNotification(null), 5000);
  };

  /**
   * Get entity display name
   */
  const getEntityDisplayName = (entityType: string): string => {
    const names: Record<string, string> = {
      Premium: 'Prêmios',
      Policy: 'Apólices',
      Endorsement: 'Endossos',
      Product: 'Produtos',
      Client: 'Clientes',
      Address: 'Endereços',
      Agency: 'Agências',
      Producer: 'Produtores',
      Coverage: 'Coberturas',
      Invoice: 'Faturas',
      Installment: 'Parcelas',
      CossuredPolicy: 'Apólices Cosseguradas',
      CossuranceCalculation: 'Cálculos de Cosseguro',
      SystemConfiguration: 'Configurações',
      ReportDefinition: 'Definições de Relatório',
    };
    return names[entityType] || entityType;
  };

  return (
    <div className="min-h-screen bg-gray-50 py-8">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
        {/* Page Header */}
        <div className="mb-8">
          <h1 className="text-3xl font-bold text-gray-900">Gerenciamento de Dados Mock</h1>
          <p className="mt-2 text-gray-600">
            Carregue, valide e gerencie dados de teste no banco de dados SQLite
          </p>
        </div>

        {/* Notification */}
        {notification && (
          <div
            className={`mb-6 rounded-lg p-4 ${
              notification.type === 'success'
                ? 'bg-green-50 border border-green-200'
                : notification.type === 'error'
                ? 'bg-red-50 border border-red-200'
                : 'bg-blue-50 border border-blue-200'
            }`}
          >
            <div className="flex items-start">
              <svg
                className={`w-5 h-5 mt-0.5 mr-3 ${
                  notification.type === 'success'
                    ? 'text-green-600'
                    : notification.type === 'error'
                    ? 'text-red-600'
                    : 'text-blue-600'
                }`}
                fill="none"
                stroke="currentColor"
                viewBox="0 0 24 24"
              >
                {notification.type === 'success' ? (
                  <path
                    strokeLinecap="round"
                    strokeLinejoin="round"
                    strokeWidth={2}
                    d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"
                  />
                ) : notification.type === 'error' ? (
                  <path
                    strokeLinecap="round"
                    strokeLinejoin="round"
                    strokeWidth={2}
                    d="M10 14l2-2m0 0l2-2m-2 2l-2-2m2 2l2 2m7-2a9 9 0 11-18 0 9 9 0 0118 0z"
                  />
                ) : (
                  <path
                    strokeLinecap="round"
                    strokeLinejoin="round"
                    strokeWidth={2}
                    d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"
                  />
                )}
              </svg>
              <p
                className={`text-sm font-medium ${
                  notification.type === 'success'
                    ? 'text-green-800'
                    : notification.type === 'error'
                    ? 'text-red-800'
                    : 'text-blue-800'
                }`}
              >
                {notification.message}
              </p>
            </div>
          </div>
        )}

        {/* Database Statistics */}
        <div className="mb-8">
          <div className="flex items-center justify-between mb-4">
            <h2 className="text-xl font-semibold text-gray-900">Estatísticas do Banco de Dados</h2>
            <div className="flex space-x-3">
              <button
                onClick={loadStats}
                disabled={statsLoading}
                className="inline-flex items-center px-3 py-2 border border-gray-300 shadow-sm text-sm font-medium rounded-md text-gray-700 bg-white hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 disabled:opacity-50"
              >
                <svg
                  className={`w-4 h-4 mr-2 ${statsLoading ? 'animate-spin' : ''}`}
                  fill="none"
                  stroke="currentColor"
                  viewBox="0 0 24 24"
                >
                  <path
                    strokeLinecap="round"
                    strokeLinejoin="round"
                    strokeWidth={2}
                    d="M4 4v5h.582m15.356 2A8.001 8.001 0 004.582 9m0 0H9m11 11v-5h-.581m0 0a8.003 8.003 0 01-15.357-2m15.357 2H15"
                  />
                </svg>
                Atualizar
              </button>
              <button
                onClick={() => setShowResetDialog(true)}
                disabled={statsLoading || !stats || stats.totalRecords === 0}
                className="inline-flex items-center px-3 py-2 border border-red-300 shadow-sm text-sm font-medium rounded-md text-red-700 bg-white hover:bg-red-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-red-500 disabled:opacity-50 disabled:cursor-not-allowed"
              >
                <svg className="w-4 h-4 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                  <path
                    strokeLinecap="round"
                    strokeLinejoin="round"
                    strokeWidth={2}
                    d="M19 7l-.867 12.142A2 2 0 0116.138 21H7.862a2 2 0 01-1.995-1.858L5 7m5 4v6m4-6v6m1-10V4a1 1 0 00-1-1h-4a1 1 0 00-1 1v3M4 7h16"
                  />
                </svg>
                Resetar Banco
              </button>
            </div>
          </div>

          {/* Stats Grid */}
          <div className="grid grid-cols-1 md:grid-cols-3 lg:grid-cols-5 gap-4">
            {/* Total Records Card */}
            <div className="bg-white rounded-lg shadow p-6 border-l-4 border-blue-500">
              <div className="flex items-center">
                <div className="flex-shrink-0">
                  <svg className="h-8 w-8 text-blue-600" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path
                      strokeLinecap="round"
                      strokeLinejoin="round"
                      strokeWidth={2}
                      d="M4 7v10c0 2.21 3.582 4 8 4s8-1.79 8-4V7M4 7c0 2.21 3.582 4 8 4s8-1.79 8-4M4 7c0-2.21 3.582-4 8-4s8 1.79 8 4"
                    />
                  </svg>
                </div>
                <div className="ml-4">
                  <p className="text-sm font-medium text-gray-600">Total de Registros</p>
                  <p className="text-2xl font-bold text-gray-900">
                    {statsLoading ? '...' : stats?.totalRecords.toLocaleString('pt-BR') || '0'}
                  </p>
                </div>
              </div>
            </div>

            {/* Entity Counts */}
            {stats &&
              stats.entityCounts
                .filter((item) => item.count > 0)
                .sort((a, b) => b.count - a.count)
                .slice(0, 4)
                .map(({ entityType, count }) => (
                  <div key={entityType} className="bg-white rounded-lg shadow p-6 border-l-4 border-gray-300">
                    <div className="flex items-center">
                      <div className="flex-shrink-0">
                        <svg
                          className="h-8 w-8 text-gray-600"
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
                      </div>
                      <div className="ml-4">
                        <p className="text-sm font-medium text-gray-600">{getEntityDisplayName(entityType)}</p>
                        <p className="text-2xl font-bold text-gray-900">{count.toLocaleString('pt-BR')}</p>
                      </div>
                    </div>
                  </div>
                ))}
          </div>

          {/* Empty State */}
          {stats && stats.totalRecords === 0 && (
            <div className="mt-4 bg-yellow-50 border border-yellow-200 rounded-lg p-4">
              <div className="flex items-start">
                <svg
                  className="w-5 h-5 text-yellow-600 mt-0.5 mr-3"
                  fill="none"
                  stroke="currentColor"
                  viewBox="0 0 24 24"
                >
                  <path
                    strokeLinecap="round"
                    strokeLinejoin="round"
                    strokeWidth={2}
                    d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z"
                  />
                </svg>
                <div>
                  <p className="text-sm font-medium text-yellow-800">Banco de dados vazio</p>
                  <p className="text-sm text-yellow-700 mt-1">
                    Carregue arquivos CSV usando o formulário abaixo para popular o banco de dados.
                  </p>
                </div>
              </div>
            </div>
          )}
        </div>

        {/* File Upload Form */}
        <div className="mb-8">
          <FileUploadForm onUpload={handleUpload} loading={uploadLoading} />
        </div>

        {/* Validation Section */}
        <div className="mb-8">
          <div className="flex items-center justify-between mb-4">
            <h2 className="text-xl font-semibold text-gray-900">Validação de Dados</h2>
            <button
              onClick={handleValidate}
              disabled={validationLoading || !stats || stats.totalRecords === 0}
              className="inline-flex items-center px-4 py-2 border border-transparent shadow-sm text-sm font-medium rounded-md text-white bg-blue-600 hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500 disabled:opacity-50 disabled:cursor-not-allowed"
            >
              {validationLoading ? (
                <>
                  <svg
                    className="animate-spin -ml-1 mr-3 h-5 w-5 text-white"
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
                  Validando...
                </>
              ) : (
                <>
                  <svg className="w-5 h-5 mr-2" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path
                      strokeLinecap="round"
                      strokeLinejoin="round"
                      strokeWidth={2}
                      d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"
                    />
                  </svg>
                  Validar Dados
                </>
              )}
            </button>
          </div>

          <ValidationResults report={validationReport} loading={validationLoading} />
        </div>

        {/* Reset Confirmation Dialog */}
        {showResetDialog && (
          <div className="fixed inset-0 bg-gray-500 bg-opacity-75 flex items-center justify-center z-50">
            <div className="bg-white rounded-lg shadow-xl p-6 max-w-md mx-4">
              <div className="flex items-start">
                <div className="flex-shrink-0">
                  <svg
                    className="h-6 w-6 text-red-600"
                    fill="none"
                    stroke="currentColor"
                    viewBox="0 0 24 24"
                  >
                    <path
                      strokeLinecap="round"
                      strokeLinejoin="round"
                      strokeWidth={2}
                      d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z"
                    />
                  </svg>
                </div>
                <div className="ml-3 flex-1">
                  <h3 className="text-lg font-medium text-gray-900">Resetar Banco de Dados</h3>
                  <p className="mt-2 text-sm text-gray-600">
                    Tem certeza que deseja resetar o banco de dados? Esta ação irá deletar todos os registros
                    e não pode ser desfeita.
                  </p>
                  <div className="mt-4 flex space-x-3">
                    <button
                      onClick={handleReset}
                      className="px-4 py-2 bg-red-600 text-white rounded-md hover:bg-red-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-red-500 text-sm font-medium"
                    >
                      Sim, Resetar
                    </button>
                    <button
                      onClick={() => setShowResetDialog(false)}
                      className="px-4 py-2 bg-gray-200 text-gray-800 rounded-md hover:bg-gray-300 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-gray-500 text-sm font-medium"
                    >
                      Cancelar
                    </button>
                  </div>
                </div>
              </div>
            </div>
          </div>
        )}
      </div>
    </div>
  );
};

export default MockDataPage;
