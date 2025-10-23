import { useState } from 'react';
import type { ReportGenerationRequest } from '../../services/types';
import { validateReportRequest } from '../../services/reportService';

interface ReportParametersFormProps {
  onSubmit: (request: ReportGenerationRequest) => void;
  isSubmitting?: boolean;
}

/**
 * Form component for collecting report generation parameters.
 * Validates inputs and submits to parent component.
 */
export default function ReportParametersForm({
  onSubmit,
  isSubmitting = false,
}: ReportParametersFormProps) {
  // Form state
  const [systemId, setSystemId] = useState<string>('GL');
  const [startDate, setStartDate] = useState<string>('');
  const [endDate, setEndDate] = useState<string>('');
  const [reportType, setReportType] = useState<'PREMIT' | 'PREMCED' | 'BOTH'>('BOTH');
  const [mode, setMode] = useState<'EMISSION' | 'CANCELLATION' | 'ALL'>('ALL');
  const [companyCode, setCompanyCode] = useState<string>('');

  // Validation state
  const [errors, setErrors] = useState<string[]>([]);

  /**
   * Handle form submission with validation
   */
  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();

    // Build request object
    const request: ReportGenerationRequest = {
      systemId,
      startDate,
      endDate,
      reportType,
      mode,
      companyCode: companyCode ? parseInt(companyCode, 10) : undefined,
    };

    // Validate request
    const validation = validateReportRequest(request);

    if (!validation.isValid) {
      setErrors(validation.errors);
      return;
    }

    // Clear errors and submit
    setErrors([]);
    onSubmit(request);
  };

  /**
   * Set default date range to current month
   */
  const setCurrentMonth = () => {
    const now = new Date();
    const firstDay = new Date(now.getFullYear(), now.getMonth(), 1);
    const lastDay = new Date(now.getFullYear(), now.getMonth() + 1, 0);

    setStartDate(firstDay.toISOString().split('T')[0]);
    setEndDate(lastDay.toISOString().split('T')[0]);
  };

  /**
   * Set default date range to current week
   */
  const setCurrentWeek = () => {
    const now = new Date();
    const dayOfWeek = now.getDay();
    const firstDay = new Date(now);
    firstDay.setDate(now.getDate() - dayOfWeek);
    const lastDay = new Date(firstDay);
    lastDay.setDate(firstDay.getDate() + 6);

    setStartDate(firstDay.toISOString().split('T')[0]);
    setEndDate(lastDay.toISOString().split('T')[0]);
  };

  return (
    <form onSubmit={handleSubmit} className="bg-white shadow-md rounded-lg p-6 space-y-4">
      <h2 className="text-2xl font-bold text-gray-800 mb-4">
        Parâmetros do Relatório
      </h2>

      {/* Error messages */}
      {errors.length > 0 && (
        <div className="bg-red-50 border border-red-200 rounded-md p-4">
          <h3 className="text-red-800 font-semibold mb-2">
            Erros de validação:
          </h3>
          <ul className="list-disc list-inside text-red-700 text-sm">
            {errors.map((error, index) => (
              <li key={index}>{error}</li>
            ))}
          </ul>
        </div>
      )}

      {/* System Selection */}
      <div>
        <label htmlFor="systemId" className="block text-sm font-medium text-gray-700 mb-1">
          Sistema *
        </label>
        <select
          id="systemId"
          value={systemId}
          onChange={(e) => setSystemId(e.target.value)}
          disabled={isSubmitting}
          className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 disabled:bg-gray-100"
          required
        >
          <option value="GL">GL - General Ledger</option>
          <option value="AP">AP - Accounts Payable</option>
          <option value="AR">AR - Accounts Receivable</option>
          <option value="PR">PR - Payroll</option>
        </select>
      </div>

      {/* Date Range */}
      <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
        <div>
          <label htmlFor="startDate" className="block text-sm font-medium text-gray-700 mb-1">
            Data Inicial *
          </label>
          <input
            type="date"
            id="startDate"
            value={startDate}
            onChange={(e) => setStartDate(e.target.value)}
            disabled={isSubmitting}
            className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 disabled:bg-gray-100"
            required
          />
        </div>

        <div>
          <label htmlFor="endDate" className="block text-sm font-medium text-gray-700 mb-1">
            Data Final *
          </label>
          <input
            type="date"
            id="endDate"
            value={endDate}
            onChange={(e) => setEndDate(e.target.value)}
            disabled={isSubmitting}
            className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 disabled:bg-gray-100"
            required
          />
        </div>
      </div>

      {/* Quick date selection buttons */}
      <div className="flex gap-2">
        <button
          type="button"
          onClick={setCurrentWeek}
          disabled={isSubmitting}
          className="px-3 py-1 text-sm bg-gray-200 hover:bg-gray-300 rounded-md disabled:opacity-50"
        >
          Semana Atual
        </button>
        <button
          type="button"
          onClick={setCurrentMonth}
          disabled={isSubmitting}
          className="px-3 py-1 text-sm bg-gray-200 hover:bg-gray-300 rounded-md disabled:opacity-50"
        >
          Mês Atual
        </button>
      </div>

      {/* Report Type Selection */}
      <div>
        <label className="block text-sm font-medium text-gray-700 mb-2">
          Tipo de Relatório *
        </label>
        <div className="space-y-2">
          <label className="flex items-center">
            <input
              type="radio"
              name="reportType"
              value="BOTH"
              checked={reportType === 'BOTH'}
              onChange={(e) => setReportType(e.target.value as 'BOTH')}
              disabled={isSubmitting}
              className="mr-2"
            />
            <span className="text-sm text-gray-700">Ambos (PREMIT + PREMCED)</span>
          </label>
          <label className="flex items-center">
            <input
              type="radio"
              name="reportType"
              value="PREMIT"
              checked={reportType === 'PREMIT'}
              onChange={(e) => setReportType(e.target.value as 'PREMIT')}
              disabled={isSubmitting}
              className="mr-2"
            />
            <span className="text-sm text-gray-700">PREMIT - Prêmios Emitidos</span>
          </label>
          <label className="flex items-center">
            <input
              type="radio"
              name="reportType"
              value="PREMCED"
              checked={reportType === 'PREMCED'}
              onChange={(e) => setReportType(e.target.value as 'PREMCED')}
              disabled={isSubmitting}
              className="mr-2"
            />
            <span className="text-sm text-gray-700">PREMCED - Prêmios Cedidos</span>
          </label>
        </div>
      </div>

      {/* Processing Mode */}
      <div>
        <label className="block text-sm font-medium text-gray-700 mb-2">
          Modo de Processamento *
        </label>
        <div className="space-y-2">
          <label className="flex items-center">
            <input
              type="radio"
              name="mode"
              value="ALL"
              checked={mode === 'ALL'}
              onChange={(e) => setMode(e.target.value as 'ALL')}
              disabled={isSubmitting}
              className="mr-2"
            />
            <span className="text-sm text-gray-700">Todos (Emissão + Cancelamento)</span>
          </label>
          <label className="flex items-center">
            <input
              type="radio"
              name="mode"
              value="EMISSION"
              checked={mode === 'EMISSION'}
              onChange={(e) => setMode(e.target.value as 'EMISSION')}
              disabled={isSubmitting}
              className="mr-2"
            />
            <span className="text-sm text-gray-700">Apenas Emissão</span>
          </label>
          <label className="flex items-center">
            <input
              type="radio"
              name="mode"
              value="CANCELLATION"
              checked={mode === 'CANCELLATION'}
              onChange={(e) => setMode(e.target.value as 'CANCELLATION')}
              disabled={isSubmitting}
              className="mr-2"
            />
            <span className="text-sm text-gray-700">Apenas Cancelamento</span>
          </label>
        </div>
      </div>

      {/* Company Code (Optional) */}
      <div>
        <label htmlFor="companyCode" className="block text-sm font-medium text-gray-700 mb-1">
          Código da Empresa (Opcional)
        </label>
        <input
          type="number"
          id="companyCode"
          value={companyCode}
          onChange={(e) => setCompanyCode(e.target.value)}
          disabled={isSubmitting}
          placeholder="Deixe em branco para todas as empresas"
          min="1"
          className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 disabled:bg-gray-100"
        />
        <p className="text-xs text-gray-500 mt-1">
          Se especificado, apenas registros dessa empresa serão incluídos
        </p>
      </div>

      {/* Submit Button */}
      <div className="pt-4">
        <button
          type="submit"
          disabled={isSubmitting}
          className="w-full bg-blue-600 hover:bg-blue-700 text-white font-semibold py-3 px-4 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-offset-2 disabled:opacity-50 disabled:cursor-not-allowed transition-colors"
        >
          {isSubmitting ? (
            <span className="flex items-center justify-center">
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
                ></circle>
                <path
                  className="opacity-75"
                  fill="currentColor"
                  d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"
                ></path>
              </svg>
              Gerando Relatório...
            </span>
          ) : (
            'Gerar Relatório'
          )}
        </button>
      </div>

      {/* Required fields note */}
      <p className="text-xs text-gray-500 text-center">
        * Campos obrigatórios
      </p>
    </form>
  );
}
