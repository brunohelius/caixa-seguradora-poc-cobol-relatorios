import { useState } from 'react';
import type { BatchJobRequestDto } from '../../services/batchJobService';
import { validateBatchJobRequest } from '../../services/batchJobService';
import { Input } from '../ui/input';
import { Label } from '../ui/label';
import { Button } from '../ui/button';
import { Card, CardContent, CardHeader, CardTitle } from '../ui/card';
import { Alert, AlertDescription, AlertTitle } from '../ui/alert';

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
    <form onSubmit={handleSubmit}>
      <Card>
        <CardHeader>
          <CardTitle>
            {mode === 'create' ? 'Criar Novo Trabalho em Lote' : 'Editar Trabalho em Lote'}
          </CardTitle>
        </CardHeader>
        <CardContent className="space-y-6">
          {/* Error messages */}
          {errors.length > 0 && (
            <Alert variant="destructive">
              <AlertTitle>Erros de validação:</AlertTitle>
              <AlertDescription>
                <ul className="list-disc list-inside">
                  {errors.map((error, index) => (
                    <li key={index}>{error}</li>
                  ))}
                </ul>
              </AlertDescription>
            </Alert>
          )}

          {/* Basic Information */}
          <div className="space-y-4">
            <h3 className="text-lg font-semibold text-gray-700 border-b pb-2">
              Informações Básicas
            </h3>

            <div className="space-y-2">
              <Label htmlFor="jobName">Nome do Trabalho *</Label>
              <Input
                id="jobName"
                type="text"
                value={jobName}
                onChange={(e) => setJobName(e.target.value)}
                disabled={isSubmitting}
                placeholder="Ex: Relatório Mensal de Prêmios"
                required
                maxLength={100}
              />
            </div>

            <div className="space-y-2">
              <Label htmlFor="description">Descrição *</Label>
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

            <div className="space-y-2">
              <Label htmlFor="recurrencePattern">Padrão de Recorrência *</Label>
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
              <div className="space-y-2">
                <Label htmlFor="executionHour">Hora de Execução (0-23)</Label>
                <Input
                  id="executionHour"
                  type="number"
                  value={executionHour}
                  onChange={(e) => setExecutionHour(e.target.value)}
                  disabled={isSubmitting}
                  min="0"
                  max="23"
                />
                <p className="text-xs text-gray-500">Default: 2h (2 AM)</p>
              </div>

              <div className="space-y-2">
                <Label htmlFor="executionMinute">Minuto de Execução (0-59)</Label>
                <Input
                  id="executionMinute"
                  type="number"
                  value={executionMinute}
                  onChange={(e) => setExecutionMinute(e.target.value)}
                  disabled={isSubmitting}
                  min="0"
                  max="59"
                />
                <p className="text-xs text-gray-500">Default: 0 minutos</p>
              </div>
            </div>

            {/* Weekly schedule */}
            {recurrencePattern === 'WEEKLY' && (
              <div className="space-y-2">
                <Label htmlFor="dayOfWeek">Dia da Semana</Label>
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
              <div className="space-y-2">
                <Label htmlFor="dayOfMonth">Dia do Mês (1-31)</Label>
                <Input
                  id="dayOfMonth"
                  type="number"
                  value={dayOfMonth}
                  onChange={(e) => setDayOfMonth(e.target.value)}
                  disabled={isSubmitting}
                  min="1"
                  max="31"
                />
                <p className="text-xs text-gray-500">
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

            <div className="space-y-2">
              <Label htmlFor="systemId">Sistema *</Label>
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
              <Label className="block text-sm font-medium text-gray-700 mb-2">
                Tipo de Relatório *
              </Label>
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
              <Label className="block text-sm font-medium text-gray-700 mb-2">
                Modo de Processamento *
              </Label>
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

            <div className="space-y-2">
              <Label htmlFor="companyCode">Código da Empresa (Opcional)</Label>
              <Input
                id="companyCode"
                type="number"
                value={companyCode}
                onChange={(e) => setCompanyCode(e.target.value)}
                disabled={isSubmitting}
                placeholder="Deixe em branco para todas as empresas"
                min="1"
              />
            </div>
          </div>

          {/* Notification Settings */}
          <div className="space-y-4">
            <h3 className="text-lg font-semibold text-gray-700 border-b pb-2">
              Configurações de Notificação
            </h3>

            <div className="space-y-2">
              <Label htmlFor="notificationEmails">Destinatários de Email *</Label>
              <Input
                id="notificationEmails"
                type="text"
                value={notificationEmails}
                onChange={(e) => setNotificationEmails(e.target.value)}
                disabled={isSubmitting}
                placeholder="usuario1@exemplo.com, usuario2@exemplo.com"
                required
              />
              <p className="text-xs text-gray-500">
                Separe múltiplos emails com vírgula
              </p>
            </div>

            <div className="space-y-2">
              <Label htmlFor="maxRetries">Número Máximo de Tentativas</Label>
              <Input
                id="maxRetries"
                type="number"
                value={maxRetries}
                onChange={(e) => setMaxRetries(e.target.value)}
                disabled={isSubmitting}
                min="0"
                max="10"
              />
              <p className="text-xs text-gray-500">
                Número de vezes para tentar novamente em caso de falha (0-10)
              </p>
            </div>
          </div>

          {/* Submit Button */}
          <div className="pt-4 flex gap-3">
            <Button
              type="submit"
              disabled={isSubmitting}
              className="flex-1"
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
            </Button>
          </div>

          {/* Required fields note */}
          <p className="text-xs text-gray-500 text-center">
            * Campos obrigatórios
          </p>
        </CardContent>
      </Card>
    </form>
  );
}
