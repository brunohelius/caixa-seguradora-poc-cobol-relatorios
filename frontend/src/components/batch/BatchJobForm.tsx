import { useState } from 'react';
import type { BatchJobRequestDto } from '../../services/batchJobService';
import { validateBatchJobRequest } from '../../services/batchJobService';

interface BatchJobFormProps {
  onSubmit: (request: BatchJobRequestDto) => void;
  isSubmitting?: boolean;
  initialData?: Partial<BatchJobRequestDto>;
  mode?: 'create' | 'edit';
}

/**
 * Form component for creating or editing batch jobs.
 * Validates inputs and submits to parent component.
 */
export default function BatchJobForm({
  onSubmit,
  isSubmitting = false,
  initialData,
  mode = 'create',
}: BatchJobFormProps) {
  // Form state
  const [jobName, setJobName] = useState<string>(initialData?.jobName || '');
  const [description, setDescription] = useState<string>(initialData?.description || '');
  const [recurrencePattern, setRecurrencePattern] = useState<'ONCE' | 'DAILY' | 'WEEKLY' | 'MONTHLY'>(
    initialData?.recurrencePattern || 'DAILY'
  );

  // Execution time
  const [executionHour, setExecutionHour] = useState<string>(
    initialData?.executionHour !== undefined ? initialData.executionHour.toString() : '2'
  );
  const [executionMinute, setExecutionMinute] = useState<string>(
    initialData?.executionMinute !== undefined ? initialData.executionMinute.toString() : '0'
  );
  const [dayOfWeek, setDayOfWeek] = useState<string>(
    initialData?.dayOfWeek !== undefined ? initialData.dayOfWeek.toString() : '1'
  );
  const [dayOfMonth, setDayOfMonth] = useState<string>(
    initialData?.dayOfMonth !== undefined ? initialData.dayOfMonth.toString() : '1'
  );

  // Report parameters
  const [systemId, setSystemId] = useState<string>(initialData?.reportParameters?.systemId || 'GL');
  const [reportType, setReportType] = useState<'PREMIT' | 'PREMCED' | 'BOTH'>(
    initialData?.reportParameters?.reportType || 'BOTH'
  );
  const [mode_param, setModeParam] = useState<'EMISSION' | 'CANCELLATION' | 'ALL'>(
    initialData?.reportParameters?.mode || 'ALL'
  );
  const [companyCode, setCompanyCode] = useState<string>(
    initialData?.reportParameters?.companyCode?.toString() || ''
  );

  // Notification settings
  const [notificationEmails, setNotificationEmails] = useState<string>(
    initialData?.notificationRecipients?.join(', ') || ''
  );
  const [maxRetries, setMaxRetries] = useState<string>(
    initialData?.maxRetries !== undefined ? initialData.maxRetries.toString() : '3'
  );

  // User information (in production this would come from auth context)
  const [createdBy] = useState<string>('admin.user');

  // Validation state
  const [errors, setErrors] = useState<string[]>([]);

  /**
   * Handle form submission with validation
   */
  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();

    // Parse notification emails
    const emailArray = notificationEmails
      .split(',')
      .map(email => email.trim())
      .filter(email => email.length > 0);

    // Build request object
    const request: BatchJobRequestDto = {
      jobName,
      description,
      recurrencePattern,
      reportParameters: {
        systemId,
        reportType,
        mode: mode_param,
        companyCode: companyCode ? parseInt(companyCode, 10) : undefined,
      },
      executionHour: executionHour ? parseInt(executionHour, 10) : undefined,
      executionMinute: executionMinute ? parseInt(executionMinute, 10) : undefined,
      dayOfWeek: recurrencePattern === 'WEEKLY' && dayOfWeek ? parseInt(dayOfWeek, 10) : undefined,
      dayOfMonth: recurrencePattern === 'MONTHLY' && dayOfMonth ? parseInt(dayOfMonth, 10) : undefined,
      notificationRecipients: emailArray,
      maxRetries: maxRetries ? parseInt(maxRetries, 10) : 3,
      createdBy,
    };

    // Validate request
    const validation = validateBatchJobRequest(request);

    if (!validation.isValid) {
      setErrors(validation.errors);
      return;
    }

    // Clear errors and submit
    setErrors([]);
    onSubmit(request);
  };

  return (
    <form onSubmit={handleSubmit} className="bg-white shadow-md rounded-lg p-6 space-y-6">
      <h2 className="text-2xl font-bold text-gray-800 mb-4">
        {mode === 'create' ? 'Criar Novo Trabalho em Lote' : 'Editar Trabalho em Lote'}
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

      {/* Basic Information */}
      <div className="space-y-4">
        <h3 className="text-lg font-semibold text-gray-700 border-b pb-2">
          Informações Básicas
        </h3>

        <div>
          <label htmlFor="jobName" className="block text-sm font-medium text-gray-700 mb-1">
            Nome do Trabalho *
          </label>
          <input
            type="text"
            id="jobName"
            value={jobName}
            onChange={(e) => setJobName(e.target.value)}
            disabled={isSubmitting}
            placeholder="Ex: Relatório Mensal de Prêmios"
            className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 disabled:bg-gray-100"
            required
            maxLength={100}
          />
        </div>

        <div>
          <label htmlFor="description" className="block text-sm font-medium text-gray-700 mb-1">
            Descrição *
          </label>
          <textarea
            id="description"
            value={description}
            onChange={(e) => setDescription(e.target.value)}
            disabled={isSubmitting}
            placeholder="Descreva o propósito deste trabalho em lote"
            rows={3}
            className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 disabled:bg-gray-100"
            required
          />
        </div>
      </div>

      {/* Schedule Configuration */}
      <div className="space-y-4">
        <h3 className="text-lg font-semibold text-gray-700 border-b pb-2">
          Configuração de Agendamento
        </h3>

        <div>
          <label htmlFor="recurrencePattern" className="block text-sm font-medium text-gray-700 mb-1">
            Padrão de Recorrência *
          </label>
          <select
            id="recurrencePattern"
            value={recurrencePattern}
            onChange={(e) => setRecurrencePattern(e.target.value as any)}
            disabled={isSubmitting}
            className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 disabled:bg-gray-100"
            required
          >
            <option value="ONCE">Uma Vez</option>
            <option value="DAILY">Diariamente</option>
            <option value="WEEKLY">Semanalmente</option>
            <option value="MONTHLY">Mensalmente</option>
          </select>
        </div>

        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div>
            <label htmlFor="executionHour" className="block text-sm font-medium text-gray-700 mb-1">
              Hora de Execução (0-23)
            </label>
            <input
              type="number"
              id="executionHour"
              value={executionHour}
              onChange={(e) => setExecutionHour(e.target.value)}
              disabled={isSubmitting}
              min="0"
              max="23"
              className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 disabled:bg-gray-100"
            />
            <p className="text-xs text-gray-500 mt-1">Default: 2h (2 AM)</p>
          </div>

          <div>
            <label htmlFor="executionMinute" className="block text-sm font-medium text-gray-700 mb-1">
              Minuto de Execução (0-59)
            </label>
            <input
              type="number"
              id="executionMinute"
              value={executionMinute}
              onChange={(e) => setExecutionMinute(e.target.value)}
              disabled={isSubmitting}
              min="0"
              max="59"
              className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 disabled:bg-gray-100"
            />
            <p className="text-xs text-gray-500 mt-1">Default: 0 minutos</p>
          </div>
        </div>

        {/* Weekly schedule */}
        {recurrencePattern === 'WEEKLY' && (
          <div>
            <label htmlFor="dayOfWeek" className="block text-sm font-medium text-gray-700 mb-1">
              Dia da Semana
            </label>
            <select
              id="dayOfWeek"
              value={dayOfWeek}
              onChange={(e) => setDayOfWeek(e.target.value)}
              disabled={isSubmitting}
              className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 disabled:bg-gray-100"
            >
              <option value="0">Domingo</option>
              <option value="1">Segunda-feira</option>
              <option value="2">Terça-feira</option>
              <option value="3">Quarta-feira</option>
              <option value="4">Quinta-feira</option>
              <option value="5">Sexta-feira</option>
              <option value="6">Sábado</option>
            </select>
          </div>
        )}

        {/* Monthly schedule */}
        {recurrencePattern === 'MONTHLY' && (
          <div>
            <label htmlFor="dayOfMonth" className="block text-sm font-medium text-gray-700 mb-1">
              Dia do Mês (1-31)
            </label>
            <input
              type="number"
              id="dayOfMonth"
              value={dayOfMonth}
              onChange={(e) => setDayOfMonth(e.target.value)}
              disabled={isSubmitting}
              min="1"
              max="31"
              className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 disabled:bg-gray-100"
            />
            <p className="text-xs text-gray-500 mt-1">
              Se o dia não existir no mês (ex: 31 em Fevereiro), será executado no último dia do mês
            </p>
          </div>
        )}
      </div>

      {/* Report Parameters */}
      <div className="space-y-4">
        <h3 className="text-lg font-semibold text-gray-700 border-b pb-2">
          Parâmetros do Relatório
        </h3>

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
                checked={mode_param === 'ALL'}
                onChange={(e) => setModeParam(e.target.value as 'ALL')}
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
                checked={mode_param === 'EMISSION'}
                onChange={(e) => setModeParam(e.target.value as 'EMISSION')}
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
                checked={mode_param === 'CANCELLATION'}
                onChange={(e) => setModeParam(e.target.value as 'CANCELLATION')}
                disabled={isSubmitting}
                className="mr-2"
              />
              <span className="text-sm text-gray-700">Apenas Cancelamento</span>
            </label>
          </div>
        </div>

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
        </div>
      </div>

      {/* Notification Settings */}
      <div className="space-y-4">
        <h3 className="text-lg font-semibold text-gray-700 border-b pb-2">
          Configurações de Notificação
        </h3>

        <div>
          <label htmlFor="notificationEmails" className="block text-sm font-medium text-gray-700 mb-1">
            Destinatários de Email *
          </label>
          <input
            type="text"
            id="notificationEmails"
            value={notificationEmails}
            onChange={(e) => setNotificationEmails(e.target.value)}
            disabled={isSubmitting}
            placeholder="usuario1@exemplo.com, usuario2@exemplo.com"
            className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 disabled:bg-gray-100"
            required
          />
          <p className="text-xs text-gray-500 mt-1">
            Separe múltiplos emails com vírgula
          </p>
        </div>

        <div>
          <label htmlFor="maxRetries" className="block text-sm font-medium text-gray-700 mb-1">
            Número Máximo de Tentativas
          </label>
          <input
            type="number"
            id="maxRetries"
            value={maxRetries}
            onChange={(e) => setMaxRetries(e.target.value)}
            disabled={isSubmitting}
            min="0"
            max="10"
            className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 disabled:bg-gray-100"
          />
          <p className="text-xs text-gray-500 mt-1">
            Número de vezes para tentar novamente em caso de falha (0-10)
          </p>
        </div>
      </div>

      {/* Submit Button */}
      <div className="pt-4 flex gap-3">
        <button
          type="submit"
          disabled={isSubmitting}
          className="flex-1 bg-blue-600 hover:bg-blue-700 text-white font-semibold py-3 px-4 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-offset-2 disabled:opacity-50 disabled:cursor-not-allowed transition-colors"
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
              {mode === 'create' ? 'Criando...' : 'Atualizando...'}
            </span>
          ) : (
            mode === 'create' ? 'Criar Trabalho em Lote' : 'Atualizar Trabalho em Lote'
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
