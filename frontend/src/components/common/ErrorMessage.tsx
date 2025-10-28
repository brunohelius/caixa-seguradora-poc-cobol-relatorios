import React from 'react';
import { Alert, AlertDescription, AlertTitle } from '../ui/alert';
import { Button } from '../ui/button';
import { cn } from '@/lib/utils';

export interface ErrorMessageProps {
  message: string;
  title?: string;
  details?: string;
  traceId?: string;
  onRetry?: () => void;
  onDismiss?: () => void;
  className?: string;
}

const ErrorMessage: React.FC<ErrorMessageProps> = ({
  message,
  title = 'Erro',
  details,
  traceId,
  onRetry,
  onDismiss,
  className = '',
}) => {
  return (
    <Alert variant="destructive" className={cn(className)}>
      <AlertTitle className="font-bold">{title}</AlertTitle>
      <AlertDescription className="mt-2">
        <div>{message}</div>

        {details && (
          <details className="mt-3">
            <summary className="cursor-pointer font-semibold text-sm">
              Detalhes técnicos
            </summary>
            <pre className="mt-2 text-xs bg-gray-100 p-3 rounded overflow-auto max-h-48 border border-gray-200">
              {details}
            </pre>
          </details>
        )}

        {traceId && (
          <p className="mt-3 text-xs">
            ID de rastreamento: <code className="bg-gray-100 px-1 py-0.5 rounded">{traceId}</code>
          </p>
        )}

        {(onRetry || onDismiss) && (
          <div className="mt-4 flex gap-2">
            {onRetry && (
              <Button
                onClick={onRetry}
                variant="default"
                size="small"
              >
                Tentar novamente
              </Button>
            )}
            {onDismiss && (
              <Button
                onClick={onDismiss}
                variant="outline"
                size="small"
              >
                Dispensar
              </Button>
            )}
          </div>
        )}
      </AlertDescription>
    </Alert>
  );
};

export default ErrorMessage;
