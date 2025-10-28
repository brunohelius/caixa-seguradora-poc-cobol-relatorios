import React, { useState, useMemo } from 'react';
import {
  Search,
  FileText,
  Calendar,
  DollarSign,
  Shield,
  User,
  AlertCircle,
  CheckCircle,
  XCircle,
  Clock,
  ChevronDown,
  ChevronUp,
  Filter
} from 'lucide-react';
import { Button } from '../components/ui/button';
import { Input } from '../components/ui/input';
import { Label } from '../components/ui/label';

// ============================================================================
// TIPOS E INTERFACES
// ============================================================================

interface Policy {
  id: string;
  policyNumber: string;
  productType: string;
  productName: string;
  clientName: string;
  clientDocument: string;
  startDate: string;
  endDate: string;
  status: 'ativa' | 'cancelada' | 'vencida' | 'suspensa';
  premiumAmount: number;
  coverageAmount: number;
  branch: string;
  susepCode: string;
  endorsements: number;
  lastPaymentDate?: string;
  nextPaymentDate?: string;
}

interface Filters {
  policyNumber: string;
  clientDocument: string;
  startDate: string;
  endDate: string;
  status: string;
  productType: string;
}

// ============================================================================
// MOCK DATA - Dados realistas de apólices
// ============================================================================

const MOCK_POLICIES: Policy[] = [
  {
    id: '1',
    policyNumber: '1234567890001',
    productType: 'auto',
    productName: 'Auto Completo',
    clientName: 'Maria da Silva Santos',
    clientDocument: '123.456.789-00',
    startDate: '2025-01-15',
    endDate: '2026-01-14',
    status: 'ativa',
    premiumAmount: 2850.00,
    coverageAmount: 85000.00,
    branch: '0531',
    susepCode: '15414.900107/2019-71',
    endorsements: 0,
    lastPaymentDate: '2025-01-15',
    nextPaymentDate: '2025-02-15'
  },
  {
    id: '2',
    policyNumber: '1234567890002',
    productType: 'residencial',
    productName: 'Residencial Premium',
    clientName: 'João Carlos Oliveira',
    clientDocument: '987.654.321-00',
    startDate: '2024-06-20',
    endDate: '2025-06-19',
    status: 'ativa',
    premiumAmount: 450.00,
    coverageAmount: 250000.00,
    branch: '0531',
    susepCode: '15414.900108/2019-25',
    endorsements: 1,
    lastPaymentDate: '2025-10-20',
    nextPaymentDate: '2025-11-20'
  },
  {
    id: '3',
    policyNumber: '1234567890003',
    productType: 'vida',
    productName: 'Vida Empresarial',
    clientName: 'Tech Solutions LTDA',
    clientDocument: '12.345.678/0001-90',
    startDate: '2024-03-10',
    endDate: '2025-03-09',
    status: 'cancelada',
    premiumAmount: 1200.00,
    coverageAmount: 500000.00,
    branch: '0531',
    susepCode: '15414.900109/2019-79',
    endorsements: 2,
    lastPaymentDate: '2024-08-10'
  },
  {
    id: '4',
    policyNumber: '1234567890004',
    productType: 'auto',
    productName: 'Auto Essencial',
    clientName: 'Ana Paula Rodrigues',
    clientDocument: '456.789.123-00',
    startDate: '2023-11-01',
    endDate: '2024-10-31',
    status: 'vencida',
    premiumAmount: 1800.00,
    coverageAmount: 55000.00,
    branch: '0531',
    susepCode: '15414.900107/2019-71',
    endorsements: 0,
    lastPaymentDate: '2024-10-01'
  },
  {
    id: '5',
    policyNumber: '1234567890005',
    productType: 'residencial',
    productName: 'Residencial Básico',
    clientName: 'Carlos Eduardo Mendes',
    clientDocument: '321.654.987-00',
    startDate: '2025-02-01',
    endDate: '2026-01-31',
    status: 'suspensa',
    premiumAmount: 280.00,
    coverageAmount: 150000.00,
    branch: '0531',
    susepCode: '15414.900108/2019-25',
    endorsements: 0,
    lastPaymentDate: '2025-02-01'
  },
  {
    id: '6',
    policyNumber: '1234567890006',
    productType: 'vida',
    productName: 'Vida Individual',
    clientName: 'Patricia Lima Costa',
    clientDocument: '789.123.456-00',
    startDate: '2024-09-15',
    endDate: '2025-09-14',
    status: 'ativa',
    premiumAmount: 350.00,
    coverageAmount: 200000.00,
    branch: '0531',
    susepCode: '15414.900109/2019-79',
    endorsements: 0,
    lastPaymentDate: '2025-09-15',
    nextPaymentDate: '2025-10-15'
  },
  {
    id: '7',
    policyNumber: '1234567890007',
    productType: 'auto',
    productName: 'Auto Completo Plus',
    clientName: 'Ricardo Souza Lima',
    clientDocument: '147.258.369-00',
    startDate: '2025-05-10',
    endDate: '2026-05-09',
    status: 'ativa',
    premiumAmount: 3200.00,
    coverageAmount: 120000.00,
    branch: '0531',
    susepCode: '15414.900107/2019-71',
    endorsements: 1,
    lastPaymentDate: '2025-05-10',
    nextPaymentDate: '2025-06-10'
  },
  {
    id: '8',
    policyNumber: '1234567890008',
    productType: 'residencial',
    productName: 'Residencial Condomínio',
    clientName: 'Condomínio Residencial Flores',
    clientDocument: '98.765.432/0001-10',
    startDate: '2024-12-01',
    endDate: '2025-11-30',
    status: 'ativa',
    premiumAmount: 1850.00,
    coverageAmount: 1500000.00,
    branch: '0531',
    susepCode: '15414.900108/2019-25',
    endorsements: 0,
    lastPaymentDate: '2025-10-01',
    nextPaymentDate: '2025-11-01'
  }
];

// ============================================================================
// COMPONENTE PRINCIPAL
// ============================================================================

const PoliciesQueryPage: React.FC = () => {
  // Estados
  const [filters, setFilters] = useState<Filters>({
    policyNumber: '',
    clientDocument: '',
    startDate: '',
    endDate: '',
    status: '',
    productType: ''
  });
  const [selectedPolicy, setSelectedPolicy] = useState<Policy | null>(null);
  const [isLoading, setIsLoading] = useState(false);
  const [showFilters, setShowFilters] = useState(true);

  // Filtragem dos dados
  const filteredPolicies = useMemo(() => {
    return MOCK_POLICIES.filter(policy => {
      if (filters.policyNumber && !policy.policyNumber.includes(filters.policyNumber)) {
        return false;
      }
      if (filters.clientDocument && !policy.clientDocument.replace(/\D/g, '').includes(filters.clientDocument.replace(/\D/g, ''))) {
        return false;
      }
      if (filters.status && policy.status !== filters.status) {
        return false;
      }
      if (filters.productType && policy.productType !== filters.productType) {
        return false;
      }
      if (filters.startDate && policy.startDate < filters.startDate) {
        return false;
      }
      if (filters.endDate && policy.endDate > filters.endDate) {
        return false;
      }
      return true;
    });
  }, [filters]);

  // Handlers
  const handleFilterChange = (field: keyof Filters, value: string) => {
    setFilters(prev => ({ ...prev, [field]: value }));
  };

  const handleClearFilters = () => {
    setFilters({
      policyNumber: '',
      clientDocument: '',
      startDate: '',
      endDate: '',
      status: '',
      productType: ''
    });
    setSelectedPolicy(null);
  };

  const handleSearch = () => {
    setIsLoading(true);
    // Simula busca na API
    setTimeout(() => {
      setIsLoading(false);
    }, 800);
  };

  // Utilitários de formatação
  const formatCurrency = (value: number): string => {
    return new Intl.NumberFormat('pt-BR', {
      style: 'currency',
      currency: 'BRL'
    }).format(value);
  };

  const formatDate = (dateString: string): string => {
    const date = new Date(dateString);
    return date.toLocaleDateString('pt-BR');
  };

  const getStatusConfig = (status: Policy['status']) => {
    const configs = {
      ativa: {
        label: 'Ativa',
        icon: CheckCircle,
        className: 'bg-green-100 text-green-800 border-green-300'
      },
      cancelada: {
        label: 'Cancelada',
        icon: XCircle,
        className: 'bg-red-100 text-red-800 border-red-300'
      },
      vencida: {
        label: 'Vencida',
        icon: AlertCircle,
        className: 'bg-orange-100 text-orange-800 border-orange-300'
      },
      suspensa: {
        label: 'Suspensa',
        icon: Clock,
        className: 'bg-yellow-100 text-yellow-800 border-yellow-300'
      }
    };
    return configs[status];
  };

  const getProductIcon = (productType: string) => {
    const icons = {
      auto: '🚗',
      residencial: '🏠',
      vida: '❤️'
    };
    return icons[productType as keyof typeof icons] || '📄';
  };

  const getProductColor = (productType: string) => {
    const colors = {
      auto: 'bg-blue-50 border-blue-200',
      residencial: 'bg-green-50 border-green-200',
      vida: 'bg-purple-50 border-purple-200'
    };
    return colors[productType as keyof typeof colors] || 'bg-gray-50 border-gray-200';
  };

  // ============================================================================
  // RENDERIZAÇÃO
  // ============================================================================

  return (
    <div className="min-h-screen bg-gray-50 p-4 md:p-6">
      {/* Header */}
      <div className="mb-6">
        <div className="flex items-center gap-3 mb-2">
          <div className="p-2 bg-blue-100 rounded-lg">
            <Shield className="w-6 h-6 text-blue-600" />
          </div>
          <div>
            <h1 className="text-2xl md:text-3xl font-bold text-gray-900">
              Consulta de Apólices
            </h1>
            <p className="text-sm text-gray-600">
              Pesquise e gerencie apólices de seguros
            </p>
          </div>
        </div>
      </div>

      {/* Filtros */}
      <div className="bg-white rounded-lg shadow-sm border border-gray-200 mb-6">
        <button
          onClick={() => setShowFilters(!showFilters)}
          className="w-full px-4 py-3 flex items-center justify-between text-left hover:bg-gray-50 transition-colors"
        >
          <div className="flex items-center gap-2">
            <Filter className="w-5 h-5 text-gray-600" />
            <span className="font-semibold text-gray-900">Filtros de Busca</span>
            {Object.values(filters).some(v => v !== '') && (
              <span className="px-2 py-0.5 bg-blue-100 text-blue-700 text-xs font-medium rounded-full">
                {Object.values(filters).filter(v => v !== '').length} ativos
              </span>
            )}
          </div>
          {showFilters ? (
            <ChevronUp className="w-5 h-5 text-gray-400" />
          ) : (
            <ChevronDown className="w-5 h-5 text-gray-400" />
          )}
        </button>

        {showFilters && (
          <div className="p-4 border-t border-gray-200">
            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
              {/* Número da Apólice */}
              <div>
                <Label htmlFor="policyNumber" className="text-sm font-medium text-gray-700 mb-1">
                  Número da Apólice
                </Label>
                <Input
                  id="policyNumber"
                  type="text"
                  placeholder="Ex: 1234567890001"
                  value={filters.policyNumber}
                  onChange={(e) => handleFilterChange('policyNumber', e.target.value)}
                  className="w-full"
                />
              </div>

              {/* CPF/CNPJ do Cliente */}
              <div>
                <Label htmlFor="clientDocument" className="text-sm font-medium text-gray-700 mb-1">
                  CPF/CNPJ do Cliente
                </Label>
                <Input
                  id="clientDocument"
                  type="text"
                  placeholder="Ex: 123.456.789-00"
                  value={filters.clientDocument}
                  onChange={(e) => handleFilterChange('clientDocument', e.target.value)}
                  className="w-full"
                />
              </div>

              {/* Status */}
              <div>
                <Label htmlFor="status" className="text-sm font-medium text-gray-700 mb-1">
                  Status
                </Label>
                <select
                  id="status"
                  value={filters.status}
                  onChange={(e) => handleFilterChange('status', e.target.value)}
                  className="w-full h-10 px-3 rounded-md border border-gray-300 bg-white text-sm focus:outline-none focus:ring-2 focus:ring-blue-500"
                >
                  <option value="">Todos os status</option>
                  <option value="ativa">Ativa</option>
                  <option value="cancelada">Cancelada</option>
                  <option value="vencida">Vencida</option>
                  <option value="suspensa">Suspensa</option>
                </select>
              </div>

              {/* Tipo de Produto */}
              <div>
                <Label htmlFor="productType" className="text-sm font-medium text-gray-700 mb-1">
                  Tipo de Produto
                </Label>
                <select
                  id="productType"
                  value={filters.productType}
                  onChange={(e) => handleFilterChange('productType', e.target.value)}
                  className="w-full h-10 px-3 rounded-md border border-gray-300 bg-white text-sm focus:outline-none focus:ring-2 focus:ring-blue-500"
                >
                  <option value="">Todos os produtos</option>
                  <option value="auto">Auto</option>
                  <option value="residencial">Residencial</option>
                  <option value="vida">Vida</option>
                </select>
              </div>

              {/* Data Início */}
              <div>
                <Label htmlFor="startDate" className="text-sm font-medium text-gray-700 mb-1">
                  Vigência a partir de
                </Label>
                <Input
                  id="startDate"
                  type="date"
                  value={filters.startDate}
                  onChange={(e) => handleFilterChange('startDate', e.target.value)}
                  className="w-full"
                />
              </div>

              {/* Data Fim */}
              <div>
                <Label htmlFor="endDate" className="text-sm font-medium text-gray-700 mb-1">
                  Vigência até
                </Label>
                <Input
                  id="endDate"
                  type="date"
                  value={filters.endDate}
                  onChange={(e) => handleFilterChange('endDate', e.target.value)}
                  className="w-full"
                />
              </div>
            </div>

            {/* Botões de Ação */}
            <div className="flex flex-col sm:flex-row gap-3 mt-4 pt-4 border-t border-gray-200">
              <Button
                onClick={handleSearch}
                disabled={isLoading}
                className="flex-1 sm:flex-none bg-blue-600 hover:bg-blue-700 text-white"
              >
                <Search className="w-4 h-4 mr-2" />
                {isLoading ? 'Buscando...' : 'Buscar Apólices'}
              </Button>
              <Button
                onClick={handleClearFilters}
                variant="outline"
                className="flex-1 sm:flex-none"
              >
                Limpar Filtros
              </Button>
            </div>
          </div>
        )}
      </div>

      {/* Resultados */}
      <div className="bg-white rounded-lg shadow-sm border border-gray-200">
        {/* Header da Tabela */}
        <div className="px-4 py-3 border-b border-gray-200 bg-gray-50">
          <div className="flex items-center justify-between">
            <div className="flex items-center gap-2">
              <FileText className="w-5 h-5 text-gray-600" />
              <span className="font-semibold text-gray-900">
                Apólices Encontradas
              </span>
              <span className="px-2 py-0.5 bg-blue-100 text-blue-700 text-sm font-medium rounded-full">
                {filteredPolicies.length}
              </span>
            </div>
          </div>
        </div>

        {/* Loading State */}
        {isLoading && (
          <div className="p-12 text-center">
            <div className="inline-block animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600 mb-4"></div>
            <p className="text-gray-600">Carregando apólices...</p>
          </div>
        )}

        {/* Empty State */}
        {!isLoading && filteredPolicies.length === 0 && (
          <div className="p-12 text-center">
            <div className="inline-flex items-center justify-center w-16 h-16 rounded-full bg-gray-100 mb-4">
              <Search className="w-8 h-8 text-gray-400" />
            </div>
            <h3 className="text-lg font-semibold text-gray-900 mb-2">
              Nenhuma apólice encontrada
            </h3>
            <p className="text-gray-600 mb-4">
              Tente ajustar os filtros de busca para encontrar apólices.
            </p>
            <Button
              onClick={handleClearFilters}
              variant="outline"
            >
              Limpar Filtros
            </Button>
          </div>
        )}

        {/* Tabela de Resultados - Desktop */}
        {!isLoading && filteredPolicies.length > 0 && (
          <>
            <div className="hidden lg:block overflow-x-auto">
              <table className="w-full">
                <thead className="bg-gray-50 border-b border-gray-200">
                  <tr>
                    <th className="px-4 py-3 text-left text-xs font-medium text-gray-600 uppercase tracking-wider">
                      Apólice
                    </th>
                    <th className="px-4 py-3 text-left text-xs font-medium text-gray-600 uppercase tracking-wider">
                      Produto
                    </th>
                    <th className="px-4 py-3 text-left text-xs font-medium text-gray-600 uppercase tracking-wider">
                      Cliente
                    </th>
                    <th className="px-4 py-3 text-left text-xs font-medium text-gray-600 uppercase tracking-wider">
                      Vigência
                    </th>
                    <th className="px-4 py-3 text-left text-xs font-medium text-gray-600 uppercase tracking-wider">
                      Status
                    </th>
                    <th className="px-4 py-3 text-right text-xs font-medium text-gray-600 uppercase tracking-wider">
                      Prêmio
                    </th>
                    <th className="px-4 py-3 text-right text-xs font-medium text-gray-600 uppercase tracking-wider">
                      Ações
                    </th>
                  </tr>
                </thead>
                <tbody className="divide-y divide-gray-200">
                  {filteredPolicies.map((policy) => {
                    const statusConfig = getStatusConfig(policy.status);
                    const StatusIcon = statusConfig.icon;

                    return (
                      <tr
                        key={policy.id}
                        className="hover:bg-gray-50 transition-colors cursor-pointer"
                        onClick={() => setSelectedPolicy(policy)}
                      >
                        <td className="px-4 py-4">
                          <div className="flex items-center gap-2">
                            <span className="text-2xl">{getProductIcon(policy.productType)}</span>
                            <div>
                              <div className="font-medium text-gray-900">
                                {policy.policyNumber}
                              </div>
                              <div className="text-xs text-gray-500">
                                SUSEP: {policy.susepCode}
                              </div>
                            </div>
                          </div>
                        </td>
                        <td className="px-4 py-4">
                          <div className="font-medium text-gray-900">{policy.productName}</div>
                          <div className="text-xs text-gray-500">Ramo: {policy.branch}</div>
                        </td>
                        <td className="px-4 py-4">
                          <div className="font-medium text-gray-900">{policy.clientName}</div>
                          <div className="text-xs text-gray-500">{policy.clientDocument}</div>
                        </td>
                        <td className="px-4 py-4">
                          <div className="text-sm text-gray-900">
                            {formatDate(policy.startDate)}
                          </div>
                          <div className="text-xs text-gray-500">
                            até {formatDate(policy.endDate)}
                          </div>
                        </td>
                        <td className="px-4 py-4">
                          <span className={`inline-flex items-center gap-1 px-2.5 py-1 rounded-full text-xs font-medium border ${statusConfig.className}`}>
                            <StatusIcon className="w-3 h-3" />
                            {statusConfig.label}
                          </span>
                        </td>
                        <td className="px-4 py-4 text-right">
                          <div className="font-semibold text-gray-900">
                            {formatCurrency(policy.premiumAmount)}
                          </div>
                          <div className="text-xs text-gray-500">mensal</div>
                        </td>
                        <td className="px-4 py-4 text-right">
                          <Button
                            size="small"
                            variant="outline"
                            onClick={(e) => {
                              e.stopPropagation();
                              setSelectedPolicy(policy);
                            }}
                          >
                            Ver Detalhes
                          </Button>
                        </td>
                      </tr>
                    );
                  })}
                </tbody>
              </table>
            </div>

            {/* Cards Mobile */}
            <div className="lg:hidden divide-y divide-gray-200">
              {filteredPolicies.map((policy) => {
                const statusConfig = getStatusConfig(policy.status);
                const StatusIcon = statusConfig.icon;

                return (
                  <div
                    key={policy.id}
                    onClick={() => setSelectedPolicy(policy)}
                    className={`p-4 hover:bg-gray-50 transition-colors cursor-pointer border-l-4 ${getProductColor(policy.productType)}`}
                  >
                    <div className="flex items-start justify-between mb-3">
                      <div className="flex items-center gap-2">
                        <span className="text-2xl">{getProductIcon(policy.productType)}</span>
                        <div>
                          <div className="font-semibold text-gray-900">
                            {policy.policyNumber}
                          </div>
                          <div className="text-sm text-gray-600">{policy.productName}</div>
                        </div>
                      </div>
                      <span className={`inline-flex items-center gap-1 px-2 py-1 rounded-full text-xs font-medium border ${statusConfig.className}`}>
                        <StatusIcon className="w-3 h-3" />
                        {statusConfig.label}
                      </span>
                    </div>

                    <div className="space-y-2 mb-3">
                      <div className="flex items-center gap-2 text-sm">
                        <User className="w-4 h-4 text-gray-400" />
                        <span className="text-gray-900">{policy.clientName}</span>
                      </div>
                      <div className="flex items-center gap-2 text-sm">
                        <Calendar className="w-4 h-4 text-gray-400" />
                        <span className="text-gray-600">
                          {formatDate(policy.startDate)} até {formatDate(policy.endDate)}
                        </span>
                      </div>
                    </div>

                    <div className="flex items-center justify-between pt-3 border-t border-gray-200">
                      <div className="flex items-center gap-2">
                        <DollarSign className="w-4 h-4 text-gray-400" />
                        <span className="font-semibold text-gray-900">
                          {formatCurrency(policy.premiumAmount)}/mês
                        </span>
                      </div>
                      <Button size="small" variant="outline">
                        Ver Detalhes
                      </Button>
                    </div>
                  </div>
                );
              })}
            </div>
          </>
        )}
      </div>

      {/* Modal de Detalhes */}
      {selectedPolicy && (
        <div
          className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center p-4 z-50"
          onClick={() => setSelectedPolicy(null)}
        >
          <div
            className="bg-white rounded-lg shadow-xl max-w-3xl w-full max-h-[90vh] overflow-y-auto"
            onClick={(e) => e.stopPropagation()}
          >
            {/* Header do Modal */}
            <div className="sticky top-0 bg-white border-b border-gray-200 px-6 py-4 flex items-center justify-between">
              <div className="flex items-center gap-3">
                <span className="text-3xl">{getProductIcon(selectedPolicy.productType)}</span>
                <div>
                  <h2 className="text-xl font-bold text-gray-900">
                    Detalhes da Apólice
                  </h2>
                  <p className="text-sm text-gray-600">
                    {selectedPolicy.policyNumber}
                  </p>
                </div>
              </div>
              <button
                onClick={() => setSelectedPolicy(null)}
                className="p-2 hover:bg-gray-100 rounded-lg transition-colors"
              >
                <XCircle className="w-6 h-6 text-gray-400" />
              </button>
            </div>

            {/* Conteúdo do Modal */}
            <div className="p-6 space-y-6">
              {/* Status Badge */}
              <div className="flex items-center gap-3">
                {(() => {
                  const statusConfig = getStatusConfig(selectedPolicy.status);
                  const StatusIcon = statusConfig.icon;
                  return (
                    <span className={`inline-flex items-center gap-2 px-4 py-2 rounded-lg text-sm font-medium border ${statusConfig.className}`}>
                      <StatusIcon className="w-4 h-4" />
                      {statusConfig.label}
                    </span>
                  );
                })()}
                {selectedPolicy.endorsements > 0 && (
                  <span className="inline-flex items-center gap-1 px-3 py-1.5 rounded-lg text-sm font-medium bg-blue-100 text-blue-800 border border-blue-300">
                    <FileText className="w-4 h-4" />
                    {selectedPolicy.endorsements} Endosso{selectedPolicy.endorsements > 1 ? 's' : ''}
                  </span>
                )}
              </div>

              {/* Grid de Informações */}
              <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                {/* Produto */}
                <div className="space-y-4">
                  <h3 className="font-semibold text-gray-900 flex items-center gap-2">
                    <Shield className="w-5 h-5 text-blue-600" />
                    Informações do Produto
                  </h3>
                  <div className="space-y-3">
                    <div>
                      <span className="text-xs text-gray-500 uppercase tracking-wide">Produto</span>
                      <p className="font-medium text-gray-900">{selectedPolicy.productName}</p>
                    </div>
                    <div>
                      <span className="text-xs text-gray-500 uppercase tracking-wide">Ramo</span>
                      <p className="font-medium text-gray-900">{selectedPolicy.branch}</p>
                    </div>
                    <div>
                      <span className="text-xs text-gray-500 uppercase tracking-wide">Código SUSEP</span>
                      <p className="font-medium text-gray-900">{selectedPolicy.susepCode}</p>
                    </div>
                  </div>
                </div>

                {/* Cliente */}
                <div className="space-y-4">
                  <h3 className="font-semibold text-gray-900 flex items-center gap-2">
                    <User className="w-5 h-5 text-blue-600" />
                    Informações do Cliente
                  </h3>
                  <div className="space-y-3">
                    <div>
                      <span className="text-xs text-gray-500 uppercase tracking-wide">Nome/Razão Social</span>
                      <p className="font-medium text-gray-900">{selectedPolicy.clientName}</p>
                    </div>
                    <div>
                      <span className="text-xs text-gray-500 uppercase tracking-wide">CPF/CNPJ</span>
                      <p className="font-medium text-gray-900">{selectedPolicy.clientDocument}</p>
                    </div>
                  </div>
                </div>

                {/* Vigência */}
                <div className="space-y-4">
                  <h3 className="font-semibold text-gray-900 flex items-center gap-2">
                    <Calendar className="w-5 h-5 text-blue-600" />
                    Período de Vigência
                  </h3>
                  <div className="space-y-3">
                    <div>
                      <span className="text-xs text-gray-500 uppercase tracking-wide">Data de Início</span>
                      <p className="font-medium text-gray-900">{formatDate(selectedPolicy.startDate)}</p>
                    </div>
                    <div>
                      <span className="text-xs text-gray-500 uppercase tracking-wide">Data de Término</span>
                      <p className="font-medium text-gray-900">{formatDate(selectedPolicy.endDate)}</p>
                    </div>
                  </div>
                </div>

                {/* Valores */}
                <div className="space-y-4">
                  <h3 className="font-semibold text-gray-900 flex items-center gap-2">
                    <DollarSign className="w-5 h-5 text-blue-600" />
                    Valores
                  </h3>
                  <div className="space-y-3">
                    <div>
                      <span className="text-xs text-gray-500 uppercase tracking-wide">Prêmio Mensal</span>
                      <p className="text-xl font-bold text-blue-600">
                        {formatCurrency(selectedPolicy.premiumAmount)}
                      </p>
                    </div>
                    <div>
                      <span className="text-xs text-gray-500 uppercase tracking-wide">Importância Segurada</span>
                      <p className="font-medium text-gray-900">
                        {formatCurrency(selectedPolicy.coverageAmount)}
                      </p>
                    </div>
                  </div>
                </div>
              </div>

              {/* Informações de Pagamento */}
              {selectedPolicy.status === 'ativa' && (
                <div className="bg-blue-50 border border-blue-200 rounded-lg p-4">
                  <h3 className="font-semibold text-blue-900 mb-3 flex items-center gap-2">
                    <Calendar className="w-5 h-5" />
                    Informações de Pagamento
                  </h3>
                  <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                    {selectedPolicy.lastPaymentDate && (
                      <div>
                        <span className="text-xs text-blue-700 uppercase tracking-wide">Último Pagamento</span>
                        <p className="font-medium text-blue-900">
                          {formatDate(selectedPolicy.lastPaymentDate)}
                        </p>
                      </div>
                    )}
                    {selectedPolicy.nextPaymentDate && (
                      <div>
                        <span className="text-xs text-blue-700 uppercase tracking-wide">Próximo Vencimento</span>
                        <p className="font-medium text-blue-900">
                          {formatDate(selectedPolicy.nextPaymentDate)}
                        </p>
                      </div>
                    )}
                  </div>
                </div>
              )}
            </div>

            {/* Footer do Modal */}
            <div className="sticky bottom-0 bg-gray-50 border-t border-gray-200 px-6 py-4 flex justify-end gap-3">
              <Button
                variant="outline"
                onClick={() => setSelectedPolicy(null)}
              >
                Fechar
              </Button>
              <Button className="bg-blue-600 hover:bg-blue-700 text-white">
                <FileText className="w-4 h-4 mr-2" />
                Gerar Extrato
              </Button>
            </div>
          </div>
        </div>
      )}
    </div>
  );
};

export default PoliciesQueryPage;
