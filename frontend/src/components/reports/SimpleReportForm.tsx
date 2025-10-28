import { useState, type FormEvent } from 'react';
import { Card } from '../ui/card';
import { Button } from '../ui/button';
import { Input } from '../ui/input';
import { Label } from '../ui/label';
import { Alert, AlertDescription } from '../ui/alert';
import pt from '../../i18n/pt-BR.json';

export interface SimpleReportFormData {
  month: string; // YYYYMM format
  reportType: 'PREMIT' | 'PREMCED' | 'BOTH';
}

interface SimpleReportFormProps {
  onSubmit: (data: SimpleReportFormData) => void;
  isSubmitting: boolean;
}

/**
 * Simplified report generation form for feature 003-complete-cobol-migration.
 * Uses month input (YYYYMM) instead of date range.
 */
export default function SimpleReportForm({ onSubmit, isSubmitting }: SimpleReportFormProps) {
  const [month, setMonth] = useState('');
  const [reportType, setReportType] = useState<'PREMIT' | 'PREMCED' | 'BOTH'>('BOTH');
  const [errors, setErrors] = useState<string[]>([]);

  const validateMonth = (monthValue: string): string[] => {
    const validationErrors: string[] = [];

    // Check format (YYYYMM)
    if (!/^\d{6}$/.test(monthValue)) {
      validationErrors.push(pt.validation.monthInvalidFormat);
      return validationErrors;
    }

    // Parse month number
    const monthNum = parseInt(monthValue.substring(4, 6));

    // Validate month range
    if (monthNum < 1 || monthNum > 12) {
      validationErrors.push(pt.validation.monthInvalidFormat);
      return validationErrors;
    }

    // Check if future month
    const now = new Date();
    const currentYear = now.getFullYear();
    const currentMonth = now.getMonth() + 1;
    const currentYYYYMM = currentYear * 100 + currentMonth;
    const inputYYYYMM = parseInt(monthValue);

    if (inputYYYYMM > currentYYYYMM) {
      validationErrors.push(pt.validation.monthFuture);
    }

    return validationErrors;
  };

  const handleSubmit = (e: FormEvent) => {
    e.preventDefault();

    // Validate
    const validationErrors: string[] = [];

    if (!month) {
      validationErrors.push(pt.validation.monthRequired);
    } else {
      validationErrors.push(...validateMonth(month));
    }

    if (validationErrors.length > 0) {
      setErrors(validationErrors);
      return;
    }

    // Clear errors and submit
    setErrors([]);
    onSubmit({ month, reportType });
  };

  // Helper to format month as Portuguese text (e.g., "Outubro 2025")
  const formatMonthDisplay = (monthValue: string): string => {
    if (!/^\d{6}$/.test(monthValue)) return '';

    const year = monthValue.substring(0, 4);
    const monthNum = parseInt(monthValue.substring(4, 6));

    const monthNames = [
      'Janeiro', 'Fevereiro', 'Março', 'Abril', 'Maio', 'Junho',
      'Julho', 'Agosto', 'Setembro', 'Outubro', 'Novembro', 'Dezembro'
    ];

    if (monthNum >= 1 && monthNum <= 12) {
      return `${monthNames[monthNum - 1]} ${year}`;
    }

    return '';
  };

  return (
    <Card className="p-6">
      <form onSubmit={handleSubmit} className="space-y-6">
        {/* Month Input */}
        <div>
          <Label htmlFor="month" className="text-sm font-medium text-gray-700">
            {pt.reportGeneration.monthLabel}
          </Label>
          <Input
            id="month"
            type="text"
            value={month}
            onChange={(e) => setMonth(e.target.value)}
            placeholder={pt.reportGeneration.monthPlaceholder}
            maxLength={6}
            className="mt-1"
            disabled={isSubmitting}
            aria-describedby="month-help"
          />
          <p id="month-help" className="mt-1 text-sm text-gray-500">
            {pt.reportGeneration.monthHelp}
          </p>
          {month && month.length === 6 && validateMonth(month).length === 0 && (
            <p className="mt-1 text-sm text-green-600">
              {formatMonthDisplay(month)}
            </p>
          )}
        </div>

        {/* Report Type Selector */}
        <div>
          <Label className="text-sm font-medium text-gray-700 mb-3 block">
            {pt.reportGeneration.reportTypeLabel}
          </Label>
          <div className="space-y-2">
            <label className="flex items-center p-3 border rounded-lg cursor-pointer hover:bg-gray-50 transition-colors">
              <input
                type="radio"
                name="reportType"
                value="BOTH"
                checked={reportType === 'BOTH'}
                onChange={() => setReportType('BOTH')}
                disabled={isSubmitting}
                className="h-4 w-4 text-caixa-blue focus:ring-caixa-blue border-gray-300"
              />
              <span className="ml-3 text-sm">
                {pt.reportGeneration.reportTypeBOTH}
              </span>
            </label>

            <label className="flex items-center p-3 border rounded-lg cursor-pointer hover:bg-gray-50 transition-colors">
              <input
                type="radio"
                name="reportType"
                value="PREMIT"
                checked={reportType === 'PREMIT'}
                onChange={() => setReportType('PREMIT')}
                disabled={isSubmitting}
                className="h-4 w-4 text-caixa-blue focus:ring-caixa-blue border-gray-300"
              />
              <span className="ml-3 text-sm">
                {pt.reportGeneration.reportTypePREMIT}
              </span>
            </label>

            <label className="flex items-center p-3 border rounded-lg cursor-pointer hover:bg-gray-50 transition-colors">
              <input
                type="radio"
                name="reportType"
                value="PREMCED"
                checked={reportType === 'PREMCED'}
                onChange={() => setReportType('PREMCED')}
                disabled={isSubmitting}
                className="h-4 w-4 text-caixa-blue focus:ring-caixa-blue border-gray-300"
              />
              <span className="ml-3 text-sm">
                {pt.reportGeneration.reportTypePREMCED}
              </span>
            </label>
          </div>
        </div>

        {/* Validation Errors */}
        {errors.length > 0 && (
          <Alert variant="destructive">
            <AlertDescription>
              <ul className="list-disc list-inside space-y-1">
                {errors.map((error, index) => (
                  <li key={index}>{error}</li>
                ))}
              </ul>
            </AlertDescription>
          </Alert>
        )}

        {/* Submit Button */}
        <Button
          type="submit"
          disabled={isSubmitting}
          className="w-full bg-caixa-blue hover:bg-blue-700 text-white font-medium py-3 rounded-lg transition-colors"
        >
          {isSubmitting ? pt.reportGeneration.generating : pt.reportGeneration.generateButton}
        </Button>
      </form>
    </Card>
  );
}
