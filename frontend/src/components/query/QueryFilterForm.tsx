import React, { useState, useEffect } from 'react';
import queryService from '../../services/queryService';

export interface QueryFilters {
  policyNumber?: number;
  startDate?: string;
  endDate?: string;
  productCode?: number;
  lineOfBusiness?: number;
  movementType?: 'E' | 'C' | 'R' | 'S' | 'A' | '';
  minAmount?: number;
  maxAmount?: number;
}

interface QueryFilterFormProps {
  onSearch: (filters: QueryFilters) => void;
  onClear: () => void;
  loading?: boolean;
}

/**
 * QueryFilterForm Component
 *
 * Premium query filter form with comprehensive filtering options:
 * - Policy number search
 * - Date range selection (start/end dates)
 * - Product and line of business dropdowns
 * - Movement type selection
 * - Premium amount range (min/max)
 * - Search and clear buttons
 *
 * Includes Portuguese labels and validation messages.
 */
const QueryFilterForm: React.FC<QueryFilterFormProps> = ({
  onSearch,
  onClear,
  loading = false,
}) => {
  // Form state
  const [filters, setFilters] = useState<QueryFilters>({
    policyNumber: undefined,
    startDate: '',
    endDate: '',
    productCode: undefined,
    lineOfBusiness: undefined,
    movementType: '',
    minAmount: undefined,
    maxAmount: undefined,
  });

  // Dropdown options state
  const [products, setProducts] = useState<Array<{ code: number; name: string }>>([]);
  const [linesOfBusiness, setLinesOfBusiness] = useState<Array<{ code: number; name: string }>>([]);

  // Validation state
  const [errors, setErrors] = useState<Record<string, string>>({});

  // Load products and lines of business on mount
  useEffect(() => {
    loadDropdownData();
  }, []);

  const loadDropdownData = async () => {
    try {
      const [productsData, lobData] = await Promise.all([
        queryService.getProducts(),
        queryService.getLinesOfBusiness(),
      ]);

      setProducts(productsData.map(p => ({ code: p.productCode, name: p.productName })));
      setLinesOfBusiness(lobData.map(lob => ({ code: lob.code, name: lob.name })));
    } catch (error) {
      console.error('Erro ao carregar dados dos filtros:', error);
    }
  };

  const handleInputChange = (field: keyof QueryFilters, value: any) => {
    setFilters(prev => ({
      ...prev,
      [field]: value === '' ? undefined : value,
    }));

    // Clear error for this field
    if (errors[field]) {
      setErrors(prev => {
        const newErrors = { ...prev };
        delete newErrors[field];
        return newErrors;
      });
    }
  };

  const validateFilters = (): boolean => {
    const newErrors: Record<string, string> = {};

    // Validate date range
    if (filters.startDate && filters.endDate) {
      const validation = queryService.validateDateRange(filters.startDate, filters.endDate);
      if (!validation.isValid) {
        newErrors.dateRange = validation.error || 'Intervalo de datas inválido';
      }
    }

    // Validate amount range
    if (filters.minAmount !== undefined && filters.maxAmount !== undefined) {
      if (filters.minAmount > filters.maxAmount) {
        newErrors.amountRange = 'Valor mínimo deve ser menor que valor máximo';
      }
    }

    // Validate policy number
    if (filters.policyNumber !== undefined && filters.policyNumber <= 0) {
      newErrors.policyNumber = 'Número da apólice deve ser maior que zero';
    }

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  const handleSearch = (e: React.FormEvent) => {
    e.preventDefault();

    if (validateFilters()) {
      onSearch(filters);
    }
  };

  const handleClear = () => {
    setFilters({
      policyNumber: undefined,
      startDate: '',
      endDate: '',
      productCode: undefined,
      lineOfBusiness: undefined,
      movementType: '',
      minAmount: undefined,
      maxAmount: undefined,
    });
    setErrors({});
    onClear();
  };

  // Movement type options (from COBOL data model)
  const movementTypes = [
    { value: '', label: 'Todos' },
    { value: 'E', label: 'E - Emissão' },
    { value: 'C', label: 'C - Cancelamento' },
    { value: 'R', label: 'R - Renovação' },
    { value: 'S', label: 'S - Substituição' },
    { value: 'A', label: 'A - Alteração' },
  ];

  return (
    <div className="bg-white rounded-lg shadow p-6">
      <h2 className="text-xl font-semibold text-gray-800 mb-4">Filtros de Pesquisa</h2>

      <form onSubmit={handleSearch} className="space-y-4">
        {/* Row 1: Policy Number and Date Range */}
        <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
          <div>
            <label htmlFor="policyNumber" className="block text-sm font-medium text-gray-700 mb-1">
              Número da Apólice
            </label>
            <input
              type="number"
              id="policyNumber"
              value={filters.policyNumber || ''}
              onChange={(e) => handleInputChange('policyNumber', e.target.value ? parseInt(e.target.value) : undefined)}
              className={`w-full px-3 py-2 border rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500 ${
                errors.policyNumber ? 'border-red-500' : 'border-gray-300'
              }`}
              placeholder="Ex: 123456"
              disabled={loading}
            />
            {errors.policyNumber && (
              <p className="mt-1 text-sm text-red-600">{errors.policyNumber}</p>
            )}
          </div>

          <div>
            <label htmlFor="startDate" className="block text-sm font-medium text-gray-700 mb-1">
              Data Inicial
            </label>
            <input
              type="date"
              id="startDate"
              value={filters.startDate || ''}
              onChange={(e) => handleInputChange('startDate', e.target.value)}
              className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
              disabled={loading}
            />
          </div>

          <div>
            <label htmlFor="endDate" className="block text-sm font-medium text-gray-700 mb-1">
              Data Final
            </label>
            <input
              type="date"
              id="endDate"
              value={filters.endDate || ''}
              onChange={(e) => handleInputChange('endDate', e.target.value)}
              className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
              disabled={loading}
            />
          </div>
        </div>

        {errors.dateRange && (
          <p className="text-sm text-red-600">{errors.dateRange}</p>
        )}

        {/* Row 2: Product, Line of Business, Movement Type */}
        <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
          <div>
            <label htmlFor="productCode" className="block text-sm font-medium text-gray-700 mb-1">
              Produto
            </label>
            <select
              id="productCode"
              value={filters.productCode || ''}
              onChange={(e) => handleInputChange('productCode', e.target.value ? parseInt(e.target.value) : undefined)}
              className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
              disabled={loading}
            >
              <option value="">Todos os produtos</option>
              {products.map(product => (
                <option key={product.code} value={product.code}>
                  {product.code} - {product.name}
                </option>
              ))}
            </select>
          </div>

          <div>
            <label htmlFor="lineOfBusiness" className="block text-sm font-medium text-gray-700 mb-1">
              Ramo SUSEP
            </label>
            <select
              id="lineOfBusiness"
              value={filters.lineOfBusiness || ''}
              onChange={(e) => handleInputChange('lineOfBusiness', e.target.value ? parseInt(e.target.value) : undefined)}
              className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
              disabled={loading}
            >
              <option value="">Todos os ramos</option>
              {linesOfBusiness.map(lob => (
                <option key={lob.code} value={lob.code}>
                  {lob.code} - {lob.name}
                </option>
              ))}
            </select>
          </div>

          <div>
            <label htmlFor="movementType" className="block text-sm font-medium text-gray-700 mb-1">
              Tipo de Movimento
            </label>
            <select
              id="movementType"
              value={filters.movementType || ''}
              onChange={(e) => handleInputChange('movementType', e.target.value as any)}
              className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
              disabled={loading}
            >
              {movementTypes.map(type => (
                <option key={type.value} value={type.value}>
                  {type.label}
                </option>
              ))}
            </select>
          </div>
        </div>

        {/* Row 3: Amount Range */}
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div>
            <label htmlFor="minAmount" className="block text-sm font-medium text-gray-700 mb-1">
              Valor Mínimo (R$)
            </label>
            <input
              type="number"
              id="minAmount"
              value={filters.minAmount || ''}
              onChange={(e) => handleInputChange('minAmount', e.target.value ? parseFloat(e.target.value) : undefined)}
              className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
              placeholder="Ex: 1000.00"
              step="0.01"
              disabled={loading}
            />
          </div>

          <div>
            <label htmlFor="maxAmount" className="block text-sm font-medium text-gray-700 mb-1">
              Valor Máximo (R$)
            </label>
            <input
              type="number"
              id="maxAmount"
              value={filters.maxAmount || ''}
              onChange={(e) => handleInputChange('maxAmount', e.target.value ? parseFloat(e.target.value) : undefined)}
              className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
              placeholder="Ex: 10000.00"
              step="0.01"
              disabled={loading}
            />
          </div>
        </div>

        {errors.amountRange && (
          <p className="text-sm text-red-600">{errors.amountRange}</p>
        )}

        {/* Action Buttons */}
        <div className="flex gap-3 pt-4 border-t">
          <button
            type="submit"
            disabled={loading}
            className="flex-1 bg-blue-600 hover:bg-blue-700 disabled:bg-blue-300 text-white font-medium py-2 px-4 rounded-md transition-colors focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-offset-2"
          >
            {loading ? 'Pesquisando...' : 'Pesquisar'}
          </button>

          <button
            type="button"
            onClick={handleClear}
            disabled={loading}
            className="flex-1 bg-gray-200 hover:bg-gray-300 disabled:bg-gray-100 text-gray-700 font-medium py-2 px-4 rounded-md transition-colors focus:outline-none focus:ring-2 focus:ring-gray-500 focus:ring-offset-2"
          >
            Limpar Filtros
          </button>
        </div>
      </form>
    </div>
  );
};

export default QueryFilterForm;
