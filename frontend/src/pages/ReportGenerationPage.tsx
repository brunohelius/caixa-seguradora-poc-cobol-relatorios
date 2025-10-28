import { useState, useEffect, useRef } from 'react';
import ReportParametersForm from '../components/reports/ReportParametersForm';
import ReportProgressIndicator from '../components/reports/ReportProgressIndicator';
import ReportResultsCard from '../components/reports/ReportResultsCard';
import ReportHistoryTable from '../components/reports/ReportHistoryTable';
import { Button } from '../components/ui/button';
import { Alert, AlertDescription } from '../components/ui/alert';
import { Card } from '../components/ui/card';
import {
  generateReport,
  pollReportStatus,
  triggerReportDownload,
  getReportHistory,
  cancelReport,
} from '../services/reportService';
import type {
  ReportGenerationRequest,
  ReportStatusResponse,
  ReportHistoryItem,
} from '../services/types';

/**
 * Main page for report generation.
 * Orchestrates form submission, status polling, download, and history display.
 */
export default function ReportGenerationPage() {
  // Current job state
  const [currentJobId, setCurrentJobId] = useState<string | null>(null);
  const [currentStatus, setCurrentStatus] = useState<ReportStatusResponse | null>(null);
  const [isSubmitting, setIsSubmitting] = useState(false);

  // History state
  const [history, setHistory] = useState<ReportHistoryItem[]>([]);
  const [historyLoading, setHistoryLoading] = useState(false);
  const [currentPage, setCurrentPage] = useState(1);
  const [totalPages, setTotalPages] = useState(1);
  const pageSize = 10;

  // Polling control
  const pollingRef = useRef<boolean>(false);

  // Error state
  const [pageError, setPageError] = useState<string | null>(null);

  /**
   * Load report history on mount and when page changes
   */
  useEffect(() => {
    loadHistory();
  }, [currentPage]);

  /**
   * Fetch report history from API
   */
  const loadHistory = async () => {
    try {
      setHistoryLoading(true);
      setPageError(null);
      const response = await getReportHistory(currentPage, pageSize);
      // API returns 'items' and 'total'
      setHistory(response.items || []);
      setTotalPages(Math.ceil((response.total || 0) / pageSize));
    } catch (error) {
      console.error('Error loading report history:', error);
      const errorMessage = error instanceof Error ? error.message : 'Erro desconhecido';
      setPageError(errorMessage || 'Erro ao carregar histórico de relatórios');
      setHistory([]);
    } finally {
      setHistoryLoading(false);
    }
  };

  /**
   * Handle form submission to generate a new report
   */
  const handleGenerateReport = async (request: ReportGenerationRequest) => {
    try {
      setIsSubmitting(true);
      setCurrentStatus(null);

      // Submit report generation request
      const response = await generateReport(request);
      setCurrentJobId(response.jobId);

      // Start polling for status
      startPolling(response.jobId);
    } catch (error) {
      console.error('Error generating report:', error);
      setIsSubmitting(false);
    }
  };

  /**
   * Start polling for report status
   */
  const startPolling = async (jobId: string) => {
    if (pollingRef.current) {
      console.warn('Polling already in progress');
      return;
    }

    pollingRef.current = true;

    try {
      const finalStatus = await pollReportStatus(
        jobId,
        (status: ReportStatusResponse) => {
          setCurrentStatus(status);
        },
        2000 // Poll every 2 seconds
      );

      // Final status received
      setCurrentStatus(finalStatus);

      // Refresh history to include the new report
      await loadHistory();
    } catch (error) {
      console.error('Error polling report status:', error);
    } finally {
      pollingRef.current = false;
      setIsSubmitting(false);
    }
  };

  /**
   * Handle download request from results card
   */
  const handleDownloadFromResults = async (fileType: 'PREMIT' | 'PREMCED') => {
    if (!currentJobId) {
      console.error('No current job ID');
      return;
    }

    try {
      await triggerReportDownload(currentJobId, fileType);
    } catch (error) {
      console.error('Error downloading report:', error);
      throw error;
    }
  };

  /**
   * Handle download request from history table
   */
  const handleDownloadFromHistory = async (jobId: string, fileName: string) => {
    try {
      // Extract file type from filename (PREMIT or PREMCED)
      const fileType = fileName.toUpperCase().includes('PREMIT') ? 'PREMIT' : 'PREMCED';
      await triggerReportDownload(jobId, fileType as 'PREMIT' | 'PREMCED', fileName);
    } catch (error) {
      console.error('Error downloading from history:', error);
      throw error;
    }
  };

  /**
   * Handle cancel request
   */
  const handleCancel = async () => {
    if (!currentJobId) {
      console.error('No current job ID to cancel');
      return;
    }

    try {
      await cancelReport(currentJobId);
      pollingRef.current = false;
      setIsSubmitting(false);
      setCurrentStatus(null);
      setCurrentJobId(null);

      // Refresh history
      await loadHistory();
    } catch (error) {
      console.error('Error canceling report:', error);
    }
  };

  /**
   * Handle page change in history table
   */
  const handlePageChange = (page: number) => {
    setCurrentPage(page);
  };

  /**
   * Reset the current job to start a new one
   */
  const handleNewReport = () => {
    setCurrentJobId(null);
    setCurrentStatus(null);
  };

  return (
    <div className="min-h-screen bg-gray-100 py-8 px-4 sm:px-6 lg:px-8">
      <div className="max-w-7xl mx-auto">
        {/* Page header */}
        <div className="mb-8">
          <h1 className="text-3xl font-bold text-gray-900">
            Geração de Relatórios SUSEP
          </h1>
          <p className="mt-2 text-sm text-gray-600">
            Gere relatórios de prêmios emitidos (PREMIT.TXT) e prêmios cedidos (PREMCED.TXT)
            conforme a Circular SUSEP 360/2008
          </p>
        </div>

        {/* Page error message */}
        {pageError && (
          <Alert variant="destructive" className="mb-6">
            <AlertDescription>
              {pageError}
            </AlertDescription>
          </Alert>
        )}

        {/* Main content grid */}
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-8">
          {/* Left column: Form and Progress */}
          <div className="space-y-6">
            {/* Form - hide when report is completed */}
            {!currentStatus || currentStatus.status !== 'COMPLETED' ? (
              <ReportParametersForm
                onSubmit={handleGenerateReport}
                isSubmitting={isSubmitting}
              />
            ) : (
              /* Show "New Report" button when completed */
              <Card className="p-6">
                <Button
                  onClick={handleNewReport}
                  variant="primary"
                  size="large"
                  className="w-full"
                >
                  Gerar Novo Relatório
                </Button>
              </Card>
            )}

            {/* Progress indicator - show when job is active */}
            {currentStatus && (
              <ReportProgressIndicator
                status={currentStatus}
                onCancel={
                  currentStatus.status === 'PROCESSING' || currentStatus.status === 'QUEUED'
                    ? handleCancel
                    : undefined
                }
              />
            )}
          </div>

          {/* Right column: Results */}
          <div className="space-y-6">
            {/* Results card - show when completed */}
            {currentStatus && currentStatus.status === 'COMPLETED' && (
              <ReportResultsCard
                status={currentStatus}
                onDownload={handleDownloadFromResults}
              />
            )}

            {/* Info card when no job is running */}
            {!currentStatus && (
              <Alert variant="info">
                <div className="flex">
                  <div className="flex-shrink-0">
                    <svg
                      className="h-6 w-6"
                      fill="none"
                      viewBox="0 0 24 24"
                      stroke="currentColor"
                    >
                      <path
                        strokeLinecap="round"
                        strokeLinejoin="round"
                        strokeWidth={2}
                        d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"
                      />
                    </svg>
                  </div>
                  <div className="ml-3">
                    <h3 className="text-sm font-medium mb-2">
                      Como usar este sistema
                    </h3>
                    <AlertDescription>
                      <ol className="list-decimal list-inside space-y-1 text-sm">
                        <li>Preencha os parâmetros do relatório no formulário à esquerda</li>
                        <li>Clique em "Gerar Relatório" para iniciar o processamento</li>
                        <li>Acompanhe o progresso em tempo real</li>
                        <li>Faça download dos arquivos quando concluído</li>
                        <li>Acesse relatórios anteriores no histórico abaixo</li>
                      </ol>
                    </AlertDescription>
                  </div>
                </div>
              </Alert>
            )}
          </div>
        </div>

        {/* History table - full width below */}
        <div className="mt-8">
          <ReportHistoryTable
            history={history}
            isLoading={historyLoading}
            currentPage={currentPage}
            totalPages={totalPages}
            onPageChange={handlePageChange}
            onDownload={handleDownloadFromHistory}
          />
        </div>
      </div>
    </div>
  );
}
