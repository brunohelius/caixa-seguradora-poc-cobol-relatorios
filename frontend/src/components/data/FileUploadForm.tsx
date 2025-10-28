import React, { useState, useRef } from 'react';

export interface FileUploadFormProps {
  onUpload: (file: File, entityType: string) => Promise<void>;
  loading: boolean;
}

/**
 * FileUploadForm Component
 *
 * File upload form with drag-and-drop support for loading mock data CSV files.
 *
 * Features:
 * - File input with drag-and-drop zone
 * - Entity type dropdown with all 15 entity types
 * - Upload button with progress indicator
 * - File validation (CSV only)
 * - Error display in Portuguese
 * - Visual feedback for drag state
 *
 * Implements User Story 5: Mock data loading functionality.
 */
const FileUploadForm: React.FC<FileUploadFormProps> = ({ onUpload, loading }) => {
  // Form state
  const [selectedFile, setSelectedFile] = useState<File | null>(null);
  const [entityType, setEntityType] = useState<string>('Premium');
  const [dragActive, setDragActive] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [uploadProgress, setUploadProgress] = useState(0);

  // File input ref
  const fileInputRef = useRef<HTMLInputElement>(null);

  // All 15 entity types from data model
  const entityTypes = [
    { value: 'Premium', label: 'Prêmios (PremiumRecord)' },
    { value: 'Policy', label: 'Apólices (Policy)' },
    { value: 'Endorsement', label: 'Endossos (Endorsement)' },
    { value: 'Product', label: 'Produtos (Product)' },
    { value: 'Client', label: 'Clientes (Client)' },
    { value: 'Address', label: 'Endereços (Address)' },
    { value: 'Agency', label: 'Agências (Agency)' },
    { value: 'Producer', label: 'Produtores (Producer)' },
    { value: 'Coverage', label: 'Coberturas (Coverage)' },
    { value: 'Invoice', label: 'Faturas (Invoice)' },
    { value: 'Installment', label: 'Parcelas (Installment)' },
    { value: 'CossuredPolicy', label: 'Apólices Cosseguradas (CossuredPolicy)' },
    { value: 'CossuranceCalculation', label: 'Cálculos de Cosseguro (CossuranceCalculation)' },
    { value: 'SystemConfiguration', label: 'Configurações do Sistema (SystemConfiguration)' },
    { value: 'ReportDefinition', label: 'Definições de Relatório (ReportDefinition)' },
  ];

  /**
   * Validate file type (CSV only)
   */
  const validateFile = (file: File): boolean => {
    if (!file.name.toLowerCase().endsWith('.csv')) {
      setError('Apenas arquivos CSV são permitidos.');
      return false;
    }

    if (file.size === 0) {
      setError('O arquivo está vazio.');
      return false;
    }

    if (file.size > 50 * 1024 * 1024) {
      // 50MB limit
      setError('O arquivo é muito grande. Tamanho máximo: 50MB.');
      return false;
    }

    setError(null);
    return true;
  };

  /**
   * Handle file selection from input
   */
  const handleFileChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const file = e.target.files?.[0];
    if (file && validateFile(file)) {
      setSelectedFile(file);
    }
  };

  /**
   * Handle drag events
   */
  const handleDrag = (e: React.DragEvent) => {
    e.preventDefault();
    e.stopPropagation();

    if (e.type === 'dragenter' || e.type === 'dragover') {
      setDragActive(true);
    } else if (e.type === 'dragleave') {
      setDragActive(false);
    }
  };

  /**
   * Handle file drop
   */
  const handleDrop = (e: React.DragEvent) => {
    e.preventDefault();
    e.stopPropagation();
    setDragActive(false);

    const file = e.dataTransfer.files?.[0];
    if (file && validateFile(file)) {
      setSelectedFile(file);
    }
  };

  /**
   * Handle upload button click
   */
  const handleUpload = async () => {
    if (!selectedFile) {
      setError('Selecione um arquivo primeiro.');
      return;
    }

    if (!entityType) {
      setError('Selecione o tipo de entidade.');
      return;
    }

    try {
      setError(null);
      setUploadProgress(0);

      // Simulate progress (in real implementation, use XMLHttpRequest or axios progress events)
      const progressInterval = setInterval(() => {
        setUploadProgress((prev) => Math.min(prev + 10, 90));
      }, 100);

      await onUpload(selectedFile, entityType);

      clearInterval(progressInterval);
      setUploadProgress(100);

      // Reset form
      setTimeout(() => {
        setSelectedFile(null);
        setUploadProgress(0);
        if (fileInputRef.current) {
          fileInputRef.current.value = '';
        }
      }, 1000);
    } catch (err) {
      const errorMessage = err instanceof Error ? err.message : 'Erro desconhecido';
      setError(errorMessage || 'Erro ao carregar arquivo. Tente novamente.');
      setUploadProgress(0);
    }
  };

  /**
   * Open file picker
   */
  const openFilePicker = () => {
    fileInputRef.current?.click();
  };

  /**
   * Clear selected file
   */
  const clearFile = () => {
    setSelectedFile(null);
    setError(null);
    if (fileInputRef.current) {
      fileInputRef.current.value = '';
    }
  };

  return (
    <div className="bg-white rounded-lg shadow p-6">
      <h2 className="text-xl font-semibold text-gray-900 mb-6">Upload de Dados CSV</h2>

      {/* Entity Type Selector */}
      <div className="mb-6">
        <label htmlFor="entityType" className="block text-sm font-medium text-gray-700 mb-2">
          Tipo de Entidade
        </label>
        <select
          id="entityType"
          value={entityType}
          onChange={(e) => setEntityType(e.target.value)}
          disabled={loading}
          className="block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 sm:text-sm py-2 px-3 border"
        >
          {entityTypes.map((type) => (
            <option key={type.value} value={type.value}>
              {type.label}
            </option>
          ))}
        </select>
        <p className="mt-1 text-xs text-gray-500">
          Selecione o tipo de entidade que corresponde aos dados do arquivo CSV
        </p>
      </div>

      {/* Drag and Drop Zone */}
      <div
        onDragEnter={handleDrag}
        onDragLeave={handleDrag}
        onDragOver={handleDrag}
        onDrop={handleDrop}
        className={`border-2 border-dashed rounded-lg p-8 text-center transition-colors ${
          dragActive
            ? 'border-blue-500 bg-blue-50'
            : 'border-gray-300 bg-gray-50 hover:border-gray-400'
        } ${loading ? 'opacity-50 cursor-not-allowed' : 'cursor-pointer'}`}
        onClick={!loading ? openFilePicker : undefined}
      >
        <input
          ref={fileInputRef}
          type="file"
          accept=".csv"
          onChange={handleFileChange}
          disabled={loading}
          className="hidden"
        />

        {selectedFile ? (
          <div className="flex flex-col items-center">
            <svg
              className="w-12 h-12 text-green-500 mb-3"
              fill="none"
              stroke="currentColor"
              viewBox="0 0 24 24"
            >
              <path
                strokeLinecap="round"
                strokeLinejoin="round"
                strokeWidth={2}
                d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"
              />
            </svg>
            <p className="text-sm font-medium text-gray-900">{selectedFile.name}</p>
            <p className="text-xs text-gray-500 mt-1">
              {(selectedFile.size / 1024).toFixed(2)} KB
            </p>
            {!loading && (
              <button
                onClick={(e) => {
                  e.stopPropagation();
                  clearFile();
                }}
                className="mt-3 text-sm text-red-600 hover:text-red-800"
              >
                Remover arquivo
              </button>
            )}
          </div>
        ) : (
          <div>
            <svg
              className="mx-auto h-12 w-12 text-gray-400"
              stroke="currentColor"
              fill="none"
              viewBox="0 0 48 48"
              aria-hidden="true"
            >
              <path
                d="M28 8H12a4 4 0 00-4 4v20m32-12v8m0 0v8a4 4 0 01-4 4H12a4 4 0 01-4-4v-4m32-4l-3.172-3.172a4 4 0 00-5.656 0L28 28M8 32l9.172-9.172a4 4 0 015.656 0L28 28m0 0l4 4m4-24h8m-4-4v8m-12 4h.02"
                strokeWidth={2}
                strokeLinecap="round"
                strokeLinejoin="round"
              />
            </svg>
            <div className="mt-4">
              <p className="text-sm font-medium text-gray-900">
                Arraste e solte um arquivo CSV aqui
              </p>
              <p className="text-xs text-gray-500 mt-1">ou clique para selecionar</p>
            </div>
            <p className="text-xs text-gray-400 mt-2">Tamanho máximo: 50MB</p>
          </div>
        )}
      </div>

      {/* Error Message */}
      {error && (
        <div className="mt-4 bg-red-50 border border-red-200 rounded-lg p-3">
          <div className="flex items-start">
            <svg
              className="w-5 h-5 text-red-600 mt-0.5 mr-2"
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
            <div className="flex-1">
              <p className="text-sm font-medium text-red-800">Erro</p>
              <p className="text-sm text-red-700 mt-0.5">{error}</p>
            </div>
          </div>
        </div>
      )}

      {/* Upload Progress */}
      {loading && uploadProgress > 0 && (
        <div className="mt-4">
          <div className="flex justify-between text-sm text-gray-600 mb-1">
            <span>Carregando arquivo...</span>
            <span>{uploadProgress}%</span>
          </div>
          <div className="w-full bg-gray-200 rounded-full h-2">
            <div
              className="bg-blue-600 h-2 rounded-full transition-all duration-300"
              style={{ width: `${uploadProgress}%` }}
            />
          </div>
        </div>
      )}

      {/* Upload Button */}
      <div className="mt-6">
        <button
          onClick={handleUpload}
          disabled={!selectedFile || loading}
          className={`w-full flex items-center justify-center px-4 py-3 border border-transparent rounded-md shadow-sm text-sm font-medium text-white transition-colors ${
            !selectedFile || loading
              ? 'bg-gray-300 cursor-not-allowed'
              : 'bg-blue-600 hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500'
          }`}
        >
          {loading ? (
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
              Carregando...
            </>
          ) : (
            <>
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
                  d="M7 16a4 4 0 01-.88-7.903A5 5 0 1115.9 6L16 6a5 5 0 011 9.9M15 13l-3-3m0 0l-3 3m3-3v12"
                />
              </svg>
              Carregar Arquivo
            </>
          )}
        </button>
      </div>

      {/* Instructions */}
      <div className="mt-6 bg-blue-50 border border-blue-200 rounded-lg p-4">
        <div className="flex items-start">
          <svg
            className="w-5 h-5 text-blue-600 mt-0.5 mr-3 flex-shrink-0"
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
          <div className="flex-1">
            <h4 className="text-sm font-medium text-blue-900">Formato do Arquivo CSV</h4>
            <ul className="mt-2 text-sm text-blue-800 space-y-1 list-disc list-inside">
              <li>A primeira linha deve conter os nomes das colunas</li>
              <li>Use ponto-e-vírgula (;) como separador de campos</li>
              <li>Codificação UTF-8 recomendada</li>
              <li>Datas no formato AAAA-MM-DD</li>
              <li>Valores numéricos sem formatação (use ponto para decimais)</li>
            </ul>
          </div>
        </div>
      </div>
    </div>
  );
};

export default FileUploadForm;
