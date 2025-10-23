import React from 'react';

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
    <div className={`message-error ${className}`} role="alert">
      <strong>{title}:</strong> {message}
      {details && (
        <details style={{ marginTop: '10px' }}>
          <summary style={{ cursor: 'pointer', fontWeight: 'bold' }}>
            Detalhes técnicos
          </summary>
          <pre style={{
            marginTop: '10px',
            fontSize: '0.85em',
            backgroundColor: '#f5f5f5',
            padding: '10px',
            overflow: 'auto',
            maxHeight: '200px',
            border: '1px solid #e2e2e2'
          }}>
            {details}
          </pre>
        </details>
      )}
      {traceId && (
        <p style={{ marginTop: '10px', fontSize: '0.8em' }}>
          ID de rastreamento: <code style={{ backgroundColor: '#f5f5f5', padding: '2px 5px' }}>{traceId}</code>
        </p>
      )}
      {(onRetry || onDismiss) && (
        <div style={{ marginTop: '15px' }}>
          {onRetry && (
            <button onClick={onRetry} style={{ marginRight: '10px' }}>
              Tentar novamente
            </button>
          )}
          {onDismiss && (
            <button onClick={onDismiss}>
              Dispensar
            </button>
          )}
        </div>
      )}
    </div>
  );
};

export default ErrorMessage;
