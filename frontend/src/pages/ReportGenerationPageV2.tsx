import { useState, useEffect, useRef } from 'react';
import { Card } from '../components/ui/card';
import { Button } from '../components/ui/button';
import { Alert, AlertDescription } from '../components/ui/alert';
import { Progress } from '../components/ui/progress';
import SimpleReportForm, { type SimpleReportFormData } from '../components/reports/SimpleReportForm';
import {
  generateReport,
  pollExecutionStatus,
  triggerDownload,
  getExecutionHistory,
  formatElapsedTime,
  formatReturnCode,
  type ReportExecution,
  type ReportExecutionSummary,
} from '../services/reportServiceV2';
import pt from '../i18n/pt-BR.json';

/**
 * Report Generation Page for feature 003-complete-cobol-migration.
 * Uses month input (YYYYMM) and simplified form matching OpenAPI spec.
 */
export default function ReportGenerationPageV2() {
  // Current execution state
  const [currentExecution, setCurrentExecution] = useState<ReportExecution | null>(null);
  const [isSubmitting, setIsSubmitting] = useState(false);
  const [pageError, setPageError] = useState<string | null>(null);

  // Execution history
  const [history, setHistory] = useState<ReportExecutionSummary[]>([]);
  const [historyLoading, setHistoryLoading] = useState(false);
  const [currentPage, setCurrentPage] = useState(1);
  const [totalPages, setTotalPages] = useState(1);
  const pageSize = 10;

  // Polling control
  const pollingRef = useRef<boolean>(false);
  const elapsedTimerRef = useRef<number | null>(null);
  const [elapsedTime, setElapsedTime] = useState('');

  /**
   * Load execution history on mount and when page changes
   */
  useEffect(() => {
    loadHistory();
  }, [currentPage]);

  /**
   * Start elapsed time update timer
   */
  useEffect(() => {
    if (currentExecution && (currentExecution.status === 'RUNNING' || currentExecution.status === 'PENDING')) {
      // Update elapsed time every second
      elapsedTimerRef.current = setInterval(() => {
        setElapsedTime(formatElapsedTime(currentExecution.startTime));
      }, 1000);
    } else {
      // Clear timer
      if (elapsedTimerRef.current) {
        clearInterval(elapsedTimerRef.current);
        elapsedTimerRef.current = null;
      }

      // Set final elapsed time
      if (currentExecution && currentExecution.endTime) {
        setElapsedTime(formatElapsedTime(currentExecution.startTime, currentExecution.endTime));
      }
    }

    return () => {
      if (elapsedTimerRef.current) {
        clearInterval(elapsedTimerRef.current);
      }
    };
  }, [currentExecution]);

  /**
   * Fetch execution history
   */
  const loadHistory = async () => {
    try {
      setHistoryLoading(true);
      setPageError(null);
      const response = await getExecutionHistory(currentPage, pageSize);
      setHistory(response.items);
      setTotalPages(response.totalPages);
    } catch (error) {
      console.error('Error loading history:', error);
      const errorMessage = error instanceof Error ? error.message : 'Erro desconhecido';
      setPageError(errorMessage || pt.errors.loadHistoryError);
      setHistory([]);
    } finally {
      setHistoryLoading(false);
    }
  };

  /**
   * Handle form submission to generate report
   */
  const handleGenerateReport = async (formData: SimpleReportFormData) => {
    try {
      setIsSubmitting(true);
      setCurrentExecution(null);
      setPageError(null);

      // Call generate endpoint
      const execution = await generateReport({
        month: formData.month,
        reportType: formData.reportType,
        executionMode: 'INTERACTIVE',
      });

      setCurrentExecution(execution);

      // Start polling for status updates
      startPolling(execution.executionId);
    } catch (error) {
      console.error('Error generating report:', error);
      const errorMessage = error instanceof Error ? error.message : 'Erro desconhecido';
      setPageError(errorMessage || pt.errors.internalError);
      setIsSubmitting(false);
    }
  };

  /**
   * Start polling for execution status
   */
  const startPolling = async (executionId: string) => {
    if (pollingRef.current) {
      console.warn('Polling already in progress');
      return;
    }

    pollingRef.current = true;

    try {
      const finalExecution = await pollExecutionStatus(
        executionId,
        (status: ReportExecution) => {
          setCurrentExecution(status);
        },
        2000 // Poll every 2 seconds as per spec
      );

      // Final status received
      setCurrentExecution(finalExecution);

      // Refresh history
      await loadHistory();
    } catch (error) {
      console.error('Error polling status:', error);
      const errorMessage = error instanceof Error ? error.message : 'Erro desconhecido';
      setPageError(errorMessage || pt.errors.internalError);
    } finally {
      pollingRef.current = false;
      setIsSubmitting(false);
    }
  };

  /**
   * Handle file download
   */
  const handleDownload = async (fileType: 'PREMIT' | 'PREMCED') => {
    if (!currentExecution) {
      console.error('No current execution');
      return;
    }

    try {
      await triggerDownload(currentExecution.executionId, fileType);
    } catch (error) {
      console.error('Error downloading file:', error);
      const errorMessage = error instanceof Error ? error.message : 'Erro desconhecido';
      setPageError(errorMessage || pt.errors.internalError);
    }
  };

  /**
   * Handle download from history
   */
  const handleDownloadFromHistory = async (executionId: string) => {
    try {
      // Default to PREMIT, could ask user which type
      await triggerDownload(executionId, 'PREMIT');
    } catch (error) {
      console.error('Error downloading from history:', error);
      const errorMessage = error instanceof Error ? error.message : 'Erro desconhecido';
      setPageError(errorMessage || pt.errors.internalError);
    }
  };

  /**
   * Reset to generate new report
   */
  const handleNewReport = () => {
    setCurrentExecution(null);
    setPageError(null);
  };

  /**
   * Calculate progress percentage
   */
  const calculateProgress = (): number => {
    if (!currentExecution) return 0;

    if (currentExecution.status === 'COMPLETED') return 100;
    if (currentExecution.status === 'FAILED') return 100;
    if (currentExecution.status === 'PENDING') return 0;

    // Estimate progress based on records processed (assume 10k max)
    const estimatedTotal = 10000;
    const percentage = (currentExecution.recordsProcessed / estimatedTotal) * 100;
    return Math.min(percentage, 95); // Cap at 95% until completion
  };

  /**
   * Render status badge
   */
  const renderStatusBadge = (status: string) => {
    const colors: Record<string, string> = {
      PENDING: 'bg-gray-500',
      RUNNING: 'bg-blue-500',
      COMPLETED: 'bg-green-500',
      FAILED: 'bg-red-500',
    };

    const labels: Record<string, string> = {
      PENDING: pt.executionStatus.statusPENDING,
      RUNNING: pt.executionStatus.statusRUNNING,
      COMPLETED: pt.executionStatus.statusCOMPLETED,
      FAILED: pt.executionStatus.statusFAILED,
    };

    return (
      <span
        className={`inline-flex items-center px-3 py-1 rounded-full text-xs font-medium text-white ${colors[status] || 'bg-gray-500'}`}
        role="status"
        aria-label={`Status do relatório: ${labels[status] || status}`}
      >
        {labels[status] || status}
      </span>
    );
  };

  return (
    <div className="min-h-screen bg-gray-100 py-8 px-4 sm:px-6 lg:px-8">
      <div className="max-w-7xl mx-auto">
        {/* Page Header */}
        <div className="mb-8">
          <h1 className="text-3xl font-bold text-gray-900">
            {pt.reportGeneration.pageTitle}
          </h1>
          <p className="mt-2 text-sm text-gray-600">
            {pt.reportGeneration.pageDescription}
          </p>
        </div>

        {/* Page Error */}
        {pageError && (
          <Alert variant="destructive" className="mb-6">
            <AlertDescription>{pageError}</AlertDescription>
          </Alert>
        )}

        {/* Main Content Grid */}
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-8">
          {/* Left Column: Form and Progress */}
          <div className="space-y-6">
            {/* Form - hide when completed */}
            {!currentExecution || currentExecution.status !== 'COMPLETED' ? (
              <SimpleReportForm
                onSubmit={handleGenerateReport}
                isSubmitting={isSubmitting}
              />
            ) : (
              <Card className="p-6">
                <Button
                  onClick={handleNewReport}
                  className="w-full bg-caixa-blue hover:bg-blue-700 text-white"
                >
                  {pt.reportGeneration.newReportButton}
                </Button>
              </Card>
            )}

            {/* Progress Indicator */}
            {currentExecution && (
              <Card className="p-6">
                <div className="space-y-4">
                  {/* Status Badge */}
                  <div className="flex items-center justify-between">
                    <span className="text-sm font-medium text-gray-700">
                      {pt.executionStatus.statusLabel}
                    </span>
                    {renderStatusBadge(currentExecution.status)}
                  </div>

                  {/* Progress Bar */}
                  {(currentExecution.status === 'RUNNING' || currentExecution.status === 'PENDING') && (
                    <div>
                      <div className="flex items-center justify-between mb-2">
                        <span className="text-sm font-medium text-gray-700">
                          {pt.executionStatus.progressLabel}
                        </span>
                        <span className="text-sm text-gray-600">
                          {calculateProgress().toFixed(0)}%
                        </span>
                      </div>
                      <Progress value={calculateProgress()} className="w-full" />
                    </div>
                  )}

                  {/* Records Processed */}
                  <div className="flex items-center justify-between">
                    <span className="text-sm font-medium text-gray-700">
                      {pt.executionStatus.recordsProcessed}
                    </span>
                    <span className="text-sm text-gray-900 font-medium">
                      {currentExecution.recordsProcessed.toLocaleString('pt-BR')}
                    </span>
                  </div>

                  {/* Elapsed Time */}
                  <div className="flex items-center justify-between">
                    <span className="text-sm font-medium text-gray-700">
                      {pt.executionStatus.elapsedTime}
                    </span>
                    <span className="text-sm text-gray-900 font-medium">
                      {elapsedTime}
                    </span>
                  </div>

                  {/* Message */}
                  <div className="pt-4 border-t">
                    <p className="text-sm text-gray-600">
                      {currentExecution.message}
                    </p>
                  </div>

                  {/* Return Code (if completed) */}
                  {currentExecution.returnCode && (
                    <div className="pt-2">
                      <p className="text-sm text-gray-700">
                        <strong>Código de Retorno:</strong> {currentExecution.returnCode} - {formatReturnCode(currentExecution.returnCode)}
                      </p>
                    </div>
                  )}
                </div>
              </Card>
            )}
          </div>

          {/* Right Column: Results and Download */}
          <div className="space-y-6">
            {/* Download Buttons - show when completed */}
            {currentExecution && currentExecution.status === 'COMPLETED' && (
              <Card className="p-6">
                <h3 className="text-lg font-semibold text-gray-900 mb-4">
                  Download de Arquivos
                </h3>
                <div className="space-y-3">
                  <Button
                    onClick={() => handleDownload('PREMIT')}
                    className="w-full bg-caixa-blue hover:bg-blue-700 text-white"
                  >
                    {pt.downloadButtons.downloadPREMIT}
                  </Button>
                  <Button
                    onClick={() => handleDownload('PREMCED')}
                    className="w-full bg-caixa-blue hover:bg-blue-700 text-white"
                  >
                    {pt.downloadButtons.downloadPREMCED}
                  </Button>
                </div>
              </Card>
            )}

            {/* Instructions */}
            {!currentExecution && (
              <Alert>
                <div className="flex">
                  <div className="flex-shrink-0">
                    <svg className="h-6 w-6 text-blue-500" fill="none" viewBox="0 0 24 24" stroke="currentColor">
                      <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" />
                    </svg>
                  </div>
                  <div className="ml-3">
                    <h3 className="text-sm font-medium text-gray-900 mb-2">
                      {pt.instructions.title}
                    </h3>
                    <AlertDescription>
                      <ol className="list-decimal list-inside space-y-1 text-sm">
                        <li>{pt.instructions.step1}</li>
                        <li>{pt.instructions.step2}</li>
                        <li>{pt.instructions.step3}</li>
                        <li>{pt.instructions.step4}</li>
                        <li>{pt.instructions.step5}</li>
                        <li>{pt.instructions.step6}</li>
                      </ol>
                    </AlertDescription>
                  </div>
                </div>
              </Alert>
            )}
          </div>
        </div>

        {/* Execution History */}
        <div className="mt-8">
          <Card className="p-6">
            <h2 className="text-xl font-semibold text-gray-900 mb-4">
              {pt.executionHistory.title}
            </h2>

            {historyLoading ? (
              <p className="text-center text-gray-600 py-8">
                {pt.executionHistory.loading}
              </p>
            ) : history.length === 0 ? (
              <p className="text-center text-gray-600 py-8">
                {pt.executionHistory.noRecords}
              </p>
            ) : (
              <>
                <div className="overflow-x-auto">
                  <table className="min-w-full divide-y divide-gray-200">
                    <thead className="bg-gray-50">
                      <tr>
                        <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                          {pt.executionHistory.month}
                        </th>
                        <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                          {pt.executionHistory.status}
                        </th>
                        <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                          {pt.executionHistory.startTime}
                        </th>
                        <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                          {pt.executionHistory.recordsProcessed}
                        </th>
                        <th className="px-6 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                          {pt.executionHistory.actions}
                        </th>
                      </tr>
                    </thead>
                    <tbody className="bg-white divide-y divide-gray-200">
                      {history.map((item) => (
                        <tr key={item.executionId} className="hover:bg-gray-50">
                          <td className="px-6 py-4 whitespace-nowrap text-sm font-medium text-gray-900">
                            {item.month}
                          </td>
                          <td className="px-6 py-4 whitespace-nowrap">
                            {renderStatusBadge(item.status)}
                          </td>
                          <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-600">
                            {new Date(item.startTime).toLocaleString('pt-BR')}
                          </td>
                          <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-600">
                            {item.recordsProcessed.toLocaleString('pt-BR')}
                          </td>
                          <td className="px-6 py-4 whitespace-nowrap text-sm text-gray-600">
                            {item.status === 'COMPLETED' && (
                              <Button
                                onClick={() => handleDownloadFromHistory(item.executionId)}
                                variant="link"
                                size="small"
                              >
                                {pt.executionHistory.download}
                              </Button>
                            )}
                          </td>
                        </tr>
                      ))}
                    </tbody>
                  </table>
                </div>

                {/* Pagination */}
                {totalPages > 1 && (
                  <div className="mt-4 flex items-center justify-between">
                    <Button
                      onClick={() => setCurrentPage(p => Math.max(1, p - 1))}
                      disabled={currentPage === 1}
                      variant="outline"
                    >
                      {pt.executionHistory.previousPage}
                    </Button>
                    <span className="text-sm text-gray-600">
                      {pt.executionHistory.page} {currentPage} / {totalPages}
                    </span>
                    <Button
                      onClick={() => setCurrentPage(p => Math.min(totalPages, p + 1))}
                      disabled={currentPage === totalPages}
                      variant="outline"
                    >
                      {pt.executionHistory.nextPage}
                    </Button>
                  </div>
                )}
              </>
            )}
          </Card>
        </div>
      </div>
    </div>
  );
}
