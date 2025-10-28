import React, { useState, useMemo } from 'react';
import {
  User,
  Users,
  Phone,
  Mail,
  MapPin,
  FileText,
  Search,
  X,
  Building2,
  Calendar,
  DollarSign,
  Shield,
  TrendingUp,
  ChevronRight,
} from 'lucide-react';
import { Input } from '../components/ui/input';
import { Button } from '../components/ui/button';
import { Label } from '../components/ui/label';
import {
  Dialog,
  DialogContent,
  DialogHeader,
  DialogTitle,
} from '../components/ui/dialog';
import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from '../components/ui/table';

// ==================== TYPES ====================

interface ClientPolicy {
  policyNumber: string;
  productName: string;
  coverageAmount: number;
  premium: number;
  startDate: string;
  endDate: string;
  status: 'active' | 'expired' | 'cancelled';
}

interface Client {
  id: string;
  documentNumber: string; // CPF ou CNPJ
  name: string;
  type: 'PF' | 'PJ'; // Pessoa Física ou Jurídica
  email: string;
  phone: string;
  address: {
    street: string;
    number: string;
    complement?: string;
    neighborhood: string;
    city: string;
    state: string;
    zipCode: string;
  };
  activePolicies: number;
  totalPremium: number;
  totalCoverage: number;
  registrationDate: string;
  policies: ClientPolicy[];
}

// ==================== MOCK DATA ====================

const MOCK_CLIENTS: Client[] = [
  {
    id: '1',
    documentNumber: '123.456.789-00',
    name: 'Maria Silva Santos',
    type: 'PF',
    email: 'maria.silva@email.com',
    phone: '(11) 98765-4321',
    address: {
      street: 'Rua das Flores',
      number: '123',
      complement: 'Apto 45',
      neighborhood: 'Jardim Paulista',
      city: 'São Paulo',
      state: 'SP',
      zipCode: '01410-000',
    },
    activePolicies: 2,
    totalPremium: 3250.50,
    totalCoverage: 850000.00,
    registrationDate: '2023-01-15',
    policies: [
      {
        policyNumber: '1001-2023-000123',
        productName: 'Residencial Premium',
        coverageAmount: 500000.00,
        premium: 1850.00,
        startDate: '2023-01-15',
        endDate: '2024-01-15',
        status: 'active',
      },
      {
        policyNumber: '1002-2023-000456',
        productName: 'Auto Completo',
        coverageAmount: 350000.00,
        premium: 1400.50,
        startDate: '2023-03-20',
        endDate: '2024-03-20',
        status: 'active',
      },
      {
        policyNumber: '1003-2022-000789',
        productName: 'Vida Individual',
        coverageAmount: 200000.00,
        premium: 890.00,
        startDate: '2022-06-10',
        endDate: '2023-06-10',
        status: 'expired',
      },
    ],
  },
  {
    id: '2',
    documentNumber: '12.345.678/0001-90',
    name: 'Tech Solutions Ltda',
    type: 'PJ',
    email: 'contato@techsolutions.com.br',
    phone: '(11) 3456-7890',
    address: {
      street: 'Avenida Paulista',
      number: '1578',
      complement: 'Conjunto 801',
      neighborhood: 'Bela Vista',
      city: 'São Paulo',
      state: 'SP',
      zipCode: '01310-200',
    },
    activePolicies: 3,
    totalPremium: 15750.00,
    totalCoverage: 5000000.00,
    registrationDate: '2022-03-10',
    policies: [
      {
        policyNumber: '2001-2022-001234',
        productName: 'Empresarial Multi-Risco',
        coverageAmount: 3000000.00,
        premium: 8500.00,
        startDate: '2022-03-10',
        endDate: '2025-03-10',
        status: 'active',
      },
      {
        policyNumber: '2002-2023-001567',
        productName: 'Responsabilidade Civil',
        coverageAmount: 1500000.00,
        premium: 4200.00,
        startDate: '2023-01-05',
        endDate: '2024-01-05',
        status: 'active',
      },
      {
        policyNumber: '2003-2023-001890',
        productName: 'Frota Empresarial',
        coverageAmount: 500000.00,
        premium: 3050.00,
        startDate: '2023-06-15',
        endDate: '2024-06-15',
        status: 'active',
      },
    ],
  },
  {
    id: '3',
    documentNumber: '987.654.321-00',
    name: 'João Pedro Oliveira',
    type: 'PF',
    email: 'joao.oliveira@email.com',
    phone: '(21) 99876-5432',
    address: {
      street: 'Rua dos Andradas',
      number: '456',
      neighborhood: 'Centro',
      city: 'Rio de Janeiro',
      state: 'RJ',
      zipCode: '20051-000',
    },
    activePolicies: 1,
    totalPremium: 2890.00,
    totalCoverage: 450000.00,
    registrationDate: '2024-05-20',
    policies: [
      {
        policyNumber: '3001-2024-002345',
        productName: 'Auto Premium',
        coverageAmount: 450000.00,
        premium: 2890.00,
        startDate: '2024-05-20',
        endDate: '2025-05-20',
        status: 'active',
      },
    ],
  },
  {
    id: '4',
    documentNumber: '98.765.432/0001-10',
    name: 'Construtora ABC S/A',
    type: 'PJ',
    email: 'financeiro@construtorabc.com.br',
    phone: '(11) 2345-6789',
    address: {
      street: 'Rua da Consolação',
      number: '2000',
      complement: '10º andar',
      neighborhood: 'Consolação',
      city: 'São Paulo',
      state: 'SP',
      zipCode: '01302-000',
    },
    activePolicies: 4,
    totalPremium: 28500.00,
    totalCoverage: 15000000.00,
    registrationDate: '2021-08-12',
    policies: [
      {
        policyNumber: '4001-2021-003456',
        productName: 'Riscos de Engenharia',
        coverageAmount: 8000000.00,
        premium: 12000.00,
        startDate: '2021-08-12',
        endDate: '2026-08-12',
        status: 'active',
      },
      {
        policyNumber: '4002-2022-003789',
        productName: 'Responsabilidade Civil Obras',
        coverageAmount: 5000000.00,
        premium: 9500.00,
        startDate: '2022-02-15',
        endDate: '2025-02-15',
        status: 'active',
      },
      {
        policyNumber: '4003-2023-004012',
        productName: 'Equipamentos',
        coverageAmount: 1500000.00,
        premium: 4500.00,
        startDate: '2023-04-10',
        endDate: '2024-04-10',
        status: 'active',
      },
      {
        policyNumber: '4004-2023-004345',
        productName: 'Frota Pesada',
        coverageAmount: 500000.00,
        premium: 2500.00,
        startDate: '2023-09-01',
        endDate: '2024-09-01',
        status: 'active',
      },
    ],
  },
  {
    id: '5',
    documentNumber: '456.789.123-45',
    name: 'Ana Carolina Ferreira',
    type: 'PF',
    email: 'ana.ferreira@email.com',
    phone: '(31) 98123-4567',
    address: {
      street: 'Avenida Afonso Pena',
      number: '789',
      complement: 'Bloco B, Apto 302',
      neighborhood: 'Centro',
      city: 'Belo Horizonte',
      state: 'MG',
      zipCode: '30130-000',
    },
    activePolicies: 0,
    totalPremium: 0,
    totalCoverage: 0,
    registrationDate: '2023-11-05',
    policies: [
      {
        policyNumber: '5001-2023-005678',
        productName: 'Vida em Grupo',
        coverageAmount: 300000.00,
        premium: 1200.00,
        startDate: '2023-11-05',
        endDate: '2024-11-05',
        status: 'cancelled',
      },
    ],
  },
];

// ==================== UTILITY FUNCTIONS ====================

const formatCurrency = (value: number): string => {
  return new Intl.NumberFormat('pt-BR', {
    style: 'currency',
    currency: 'BRL',
  }).format(value);
};

const formatDate = (dateString: string): string => {
  const date = new Date(dateString);
  return new Intl.DateTimeFormat('pt-BR').format(date);
};

const getStatusColor = (status: string): string => {
  switch (status) {
    case 'active':
      return 'bg-green-100 text-green-800';
    case 'expired':
      return 'bg-gray-100 text-gray-800';
    case 'cancelled':
      return 'bg-red-100 text-red-800';
    default:
      return 'bg-gray-100 text-gray-800';
  }
};

const getStatusLabel = (status: string): string => {
  switch (status) {
    case 'active':
      return 'Ativa';
    case 'expired':
      return 'Expirada';
    case 'cancelled':
      return 'Cancelada';
    default:
      return status;
  }
};

// ==================== MAIN COMPONENT ====================

export const ClientsQueryPage: React.FC = () => {
  // State
  const [searchTerm, setSearchTerm] = useState('');
  const [typeFilter, setTypeFilter] = useState<'all' | 'PF' | 'PJ'>('all');
  const [selectedClient, setSelectedClient] = useState<Client | null>(null);
  const [isLoading] = useState(false);

  // Filtered clients based on search and type filter
  const filteredClients = useMemo(() => {
    return MOCK_CLIENTS.filter((client) => {
      const matchesSearch =
        searchTerm === '' ||
        client.name.toLowerCase().includes(searchTerm.toLowerCase()) ||
        client.documentNumber.includes(searchTerm) ||
        client.email.toLowerCase().includes(searchTerm.toLowerCase());

      const matchesType = typeFilter === 'all' || client.type === typeFilter;

      return matchesSearch && matchesType;
    });
  }, [searchTerm, typeFilter]);

  // Calculate summary statistics
  const statistics = useMemo(() => {
    const totalClients = filteredClients.length;
    const totalActivePolicies = filteredClients.reduce(
      (sum, client) => sum + client.activePolicies,
      0
    );
    const totalPremium = filteredClients.reduce(
      (sum, client) => sum + client.totalPremium,
      0
    );
    const totalCoverage = filteredClients.reduce(
      (sum, client) => sum + client.totalCoverage,
      0
    );

    return {
      totalClients,
      totalActivePolicies,
      totalPremium,
      totalCoverage,
    };
  }, [filteredClients]);

  // Handlers
  const handleClearFilters = () => {
    setSearchTerm('');
    setTypeFilter('all');
  };

  const handleClientClick = (client: Client) => {
    setSelectedClient(client);
  };

  return (
    <div className="min-h-screen bg-gray-50 p-6">
      <div className="mx-auto max-w-7xl">
        {/* Header */}
        <div className="mb-8">
          <h1 className="text-3xl font-bold text-gray-900">
            Consulta de Clientes
          </h1>
          <p className="mt-2 text-gray-600">
            Gerencie e visualize informações de clientes e suas apólices
          </p>
        </div>

        {/* Summary Cards */}
        <div className="mb-6 grid grid-cols-1 gap-4 sm:grid-cols-2 lg:grid-cols-4">
          <div className="rounded-lg border border-gray-200 bg-white p-6 shadow-sm">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm font-medium text-gray-600">
                  Total de Clientes
                </p>
                <p className="mt-2 text-3xl font-bold text-gray-900">
                  {statistics.totalClients}
                </p>
              </div>
              <div className="rounded-full bg-blue-100 p-3">
                <Users className="h-6 w-6 text-caixa-blue" />
              </div>
            </div>
          </div>

          <div className="rounded-lg border border-gray-200 bg-white p-6 shadow-sm">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm font-medium text-gray-600">
                  Apólices Ativas
                </p>
                <p className="mt-2 text-3xl font-bold text-gray-900">
                  {statistics.totalActivePolicies}
                </p>
              </div>
              <div className="rounded-full bg-green-100 p-3">
                <Shield className="h-6 w-6 text-green-600" />
              </div>
            </div>
          </div>

          <div className="rounded-lg border border-gray-200 bg-white p-6 shadow-sm">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm font-medium text-gray-600">
                  Prêmio Total
                </p>
                <p className="mt-2 text-2xl font-bold text-gray-900">
                  {formatCurrency(statistics.totalPremium)}
                </p>
              </div>
              <div className="rounded-full bg-yellow-100 p-3">
                <DollarSign className="h-6 w-6 text-caixa-yellow" />
              </div>
            </div>
          </div>

          <div className="rounded-lg border border-gray-200 bg-white p-6 shadow-sm">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm font-medium text-gray-600">
                  Valor Segurado
                </p>
                <p className="mt-2 text-2xl font-bold text-gray-900">
                  {formatCurrency(statistics.totalCoverage)}
                </p>
              </div>
              <div className="rounded-full bg-purple-100 p-3">
                <TrendingUp className="h-6 w-6 text-purple-600" />
              </div>
            </div>
          </div>
        </div>

        {/* Filters */}
        <div className="mb-6 rounded-lg border border-gray-200 bg-white p-6 shadow-sm">
          <div className="flex flex-col gap-4 lg:flex-row lg:items-end lg:justify-between">
            <div className="flex-1 space-y-4 lg:flex lg:gap-4 lg:space-y-0">
              {/* Search Input */}
              <div className="flex-1">
                <Label htmlFor="search" className="text-sm font-medium">
                  Buscar Cliente
                </Label>
                <div className="relative mt-1">
                  <Search className="absolute left-3 top-1/2 h-4 w-4 -translate-y-1/2 text-gray-400" />
                  <Input
                    id="search"
                    type="text"
                    placeholder="CPF/CNPJ, nome ou e-mail..."
                    value={searchTerm}
                    onChange={(e) => setSearchTerm(e.target.value)}
                    className="pl-10"
                  />
                </div>
              </div>

              {/* Type Filter */}
              <div className="lg:w-48">
                <Label htmlFor="type" className="text-sm font-medium">
                  Tipo de Cliente
                </Label>
                <select
                  id="type"
                  value={typeFilter}
                  onChange={(e) =>
                    setTypeFilter(e.target.value as 'all' | 'PF' | 'PJ')
                  }
                  className="mt-1 block w-full rounded-md border border-gray-300 bg-white px-3 py-2 shadow-sm focus:border-caixa-blue focus:outline-none focus:ring-1 focus:ring-caixa-blue"
                >
                  <option value="all">Todos</option>
                  <option value="PF">Pessoa Física</option>
                  <option value="PJ">Pessoa Jurídica</option>
                </select>
              </div>
            </div>

            {/* Clear Filters Button */}
            {(searchTerm || typeFilter !== 'all') && (
              <Button
                variant="outline"
                onClick={handleClearFilters}
                className="lg:mb-0"
              >
                <X className="mr-2 h-4 w-4" />
                Limpar Filtros
              </Button>
            )}
          </div>
        </div>

        {/* Results Table */}
        <div className="rounded-lg border border-gray-200 bg-white shadow-sm">
          {isLoading ? (
            <div className="flex h-64 items-center justify-center">
              <div className="text-center">
                <div className="mx-auto h-12 w-12 animate-spin rounded-full border-4 border-gray-200 border-t-caixa-blue"></div>
                <p className="mt-4 text-gray-600">Carregando clientes...</p>
              </div>
            </div>
          ) : filteredClients.length === 0 ? (
            <div className="flex h-64 items-center justify-center">
              <div className="text-center">
                <Users className="mx-auto h-12 w-12 text-gray-400" />
                <h3 className="mt-4 text-lg font-medium text-gray-900">
                  Nenhum cliente encontrado
                </h3>
                <p className="mt-2 text-gray-600">
                  Tente ajustar os filtros de busca
                </p>
              </div>
            </div>
          ) : (
            <div className="overflow-x-auto">
              <Table>
                <TableHeader>
                  <TableRow>
                    <TableHead>Cliente</TableHead>
                    <TableHead>Tipo</TableHead>
                    <TableHead>CPF/CNPJ</TableHead>
                    <TableHead>Contato</TableHead>
                    <TableHead className="text-center">
                      Apólices Ativas
                    </TableHead>
                    <TableHead className="text-right">Prêmio Total</TableHead>
                    <TableHead className="text-right">
                      Valor Segurado
                    </TableHead>
                    <TableHead></TableHead>
                  </TableRow>
                </TableHeader>
                <TableBody>
                  {filteredClients.map((client) => (
                    <TableRow
                      key={client.id}
                      className="cursor-pointer hover:bg-gray-50"
                      onClick={() => handleClientClick(client)}
                    >
                      <TableCell>
                        <div className="flex items-center gap-3">
                          <div
                            className={`flex h-10 w-10 items-center justify-center rounded-full ${
                              client.type === 'PF'
                                ? 'bg-blue-100'
                                : 'bg-purple-100'
                            }`}
                          >
                            {client.type === 'PF' ? (
                              <User
                                className={`h-5 w-5 ${
                                  client.type === 'PF'
                                    ? 'text-caixa-blue'
                                    : 'text-purple-600'
                                }`}
                              />
                            ) : (
                              <Building2 className="h-5 w-5 text-purple-600" />
                            )}
                          </div>
                          <div>
                            <p className="font-medium text-gray-900">
                              {client.name}
                            </p>
                            <p className="text-sm text-gray-600">
                              Cliente desde {formatDate(client.registrationDate)}
                            </p>
                          </div>
                        </div>
                      </TableCell>
                      <TableCell>
                        <span
                          className={`inline-flex rounded-full px-2 py-1 text-xs font-semibold ${
                            client.type === 'PF'
                              ? 'bg-blue-100 text-blue-800'
                              : 'bg-purple-100 text-purple-800'
                          }`}
                        >
                          {client.type === 'PF'
                            ? 'Pessoa Física'
                            : 'Pessoa Jurídica'}
                        </span>
                      </TableCell>
                      <TableCell className="font-mono text-sm">
                        {client.documentNumber}
                      </TableCell>
                      <TableCell>
                        <div className="space-y-1">
                          <div className="flex items-center gap-2 text-sm">
                            <Mail className="h-3 w-3 text-gray-400" />
                            <span className="text-gray-600">
                              {client.email}
                            </span>
                          </div>
                          <div className="flex items-center gap-2 text-sm">
                            <Phone className="h-3 w-3 text-gray-400" />
                            <span className="text-gray-600">
                              {client.phone}
                            </span>
                          </div>
                        </div>
                      </TableCell>
                      <TableCell className="text-center">
                        <span
                          className={`inline-flex h-8 w-8 items-center justify-center rounded-full font-semibold ${
                            client.activePolicies > 0
                              ? 'bg-green-100 text-green-800'
                              : 'bg-gray-100 text-gray-600'
                          }`}
                        >
                          {client.activePolicies}
                        </span>
                      </TableCell>
                      <TableCell className="text-right font-medium">
                        {formatCurrency(client.totalPremium)}
                      </TableCell>
                      <TableCell className="text-right font-semibold text-caixa-blue">
                        {formatCurrency(client.totalCoverage)}
                      </TableCell>
                      <TableCell>
                        <ChevronRight className="h-5 w-5 text-gray-400" />
                      </TableCell>
                    </TableRow>
                  ))}
                </TableBody>
              </Table>
            </div>
          )}
        </div>
      </div>

      {/* Client Detail Modal */}
      <Dialog
        open={selectedClient !== null}
        onOpenChange={(open) => !open && setSelectedClient(null)}
      >
        <DialogContent className="max-h-[90vh] max-w-4xl overflow-y-auto">
          {selectedClient && (
            <>
              <DialogHeader>
                <DialogTitle className="flex items-center gap-3 text-2xl">
                  <div
                    className={`flex h-12 w-12 items-center justify-center rounded-full ${
                      selectedClient.type === 'PF'
                        ? 'bg-blue-100'
                        : 'bg-purple-100'
                    }`}
                  >
                    {selectedClient.type === 'PF' ? (
                      <User className="h-6 w-6 text-caixa-blue" />
                    ) : (
                      <Building2 className="h-6 w-6 text-purple-600" />
                    )}
                  </div>
                  {selectedClient.name}
                </DialogTitle>
              </DialogHeader>

              <div className="space-y-6">
                {/* Summary Cards */}
                <div className="grid grid-cols-3 gap-4">
                  <div className="rounded-lg border border-gray-200 bg-gray-50 p-4">
                    <div className="flex items-center gap-2 text-sm text-gray-600">
                      <Shield className="h-4 w-4" />
                      Apólices Ativas
                    </div>
                    <p className="mt-2 text-2xl font-bold text-gray-900">
                      {selectedClient.activePolicies}
                    </p>
                  </div>
                  <div className="rounded-lg border border-gray-200 bg-gray-50 p-4">
                    <div className="flex items-center gap-2 text-sm text-gray-600">
                      <DollarSign className="h-4 w-4" />
                      Prêmio Total
                    </div>
                    <p className="mt-2 text-xl font-bold text-gray-900">
                      {formatCurrency(selectedClient.totalPremium)}
                    </p>
                  </div>
                  <div className="rounded-lg border border-gray-200 bg-gray-50 p-4">
                    <div className="flex items-center gap-2 text-sm text-gray-600">
                      <TrendingUp className="h-4 w-4" />
                      Valor Segurado
                    </div>
                    <p className="mt-2 text-xl font-bold text-caixa-blue">
                      {formatCurrency(selectedClient.totalCoverage)}
                    </p>
                  </div>
                </div>

                {/* Client Information */}
                <div className="rounded-lg border border-gray-200 bg-white p-6">
                  <h3 className="mb-4 flex items-center gap-2 text-lg font-semibold">
                    <FileText className="h-5 w-5 text-caixa-blue" />
                    Informações do Cliente
                  </h3>
                  <div className="grid grid-cols-2 gap-4">
                    <div>
                      <Label className="text-xs text-gray-600">
                        {selectedClient.type === 'PF' ? 'CPF' : 'CNPJ'}
                      </Label>
                      <p className="mt-1 font-mono text-sm font-medium">
                        {selectedClient.documentNumber}
                      </p>
                    </div>
                    <div>
                      <Label className="text-xs text-gray-600">Tipo</Label>
                      <p className="mt-1 text-sm font-medium">
                        {selectedClient.type === 'PF'
                          ? 'Pessoa Física'
                          : 'Pessoa Jurídica'}
                      </p>
                    </div>
                    <div>
                      <Label className="text-xs text-gray-600">E-mail</Label>
                      <p className="mt-1 text-sm font-medium">
                        {selectedClient.email}
                      </p>
                    </div>
                    <div>
                      <Label className="text-xs text-gray-600">Telefone</Label>
                      <p className="mt-1 text-sm font-medium">
                        {selectedClient.phone}
                      </p>
                    </div>
                    <div className="col-span-2">
                      <Label className="text-xs text-gray-600">
                        Data de Cadastro
                      </Label>
                      <p className="mt-1 text-sm font-medium">
                        {formatDate(selectedClient.registrationDate)}
                      </p>
                    </div>
                  </div>
                </div>

                {/* Address */}
                <div className="rounded-lg border border-gray-200 bg-white p-6">
                  <h3 className="mb-4 flex items-center gap-2 text-lg font-semibold">
                    <MapPin className="h-5 w-5 text-caixa-blue" />
                    Endereço
                  </h3>
                  <div className="space-y-2">
                    <p className="text-sm">
                      <span className="font-medium">
                        {selectedClient.address.street},{' '}
                        {selectedClient.address.number}
                      </span>
                      {selectedClient.address.complement && (
                        <span className="text-gray-600">
                          {' '}
                          - {selectedClient.address.complement}
                        </span>
                      )}
                    </p>
                    <p className="text-sm text-gray-600">
                      {selectedClient.address.neighborhood} -{' '}
                      {selectedClient.address.city}/{selectedClient.address.state}
                    </p>
                    <p className="font-mono text-sm text-gray-600">
                      CEP: {selectedClient.address.zipCode}
                    </p>
                  </div>
                </div>

                {/* Policies History */}
                <div className="rounded-lg border border-gray-200 bg-white p-6">
                  <h3 className="mb-4 flex items-center gap-2 text-lg font-semibold">
                    <Calendar className="h-5 w-5 text-caixa-blue" />
                    Histórico de Apólices
                  </h3>
                  <div className="space-y-3">
                    {selectedClient.policies.map((policy) => (
                      <div
                        key={policy.policyNumber}
                        className="rounded-lg border border-gray-200 bg-gray-50 p-4"
                      >
                        <div className="flex items-start justify-between">
                          <div className="flex-1">
                            <div className="flex items-center gap-2">
                              <p className="font-mono text-sm font-medium text-gray-900">
                                {policy.policyNumber}
                              </p>
                              <span
                                className={`rounded-full px-2 py-0.5 text-xs font-semibold ${getStatusColor(
                                  policy.status
                                )}`}
                              >
                                {getStatusLabel(policy.status)}
                              </span>
                            </div>
                            <p className="mt-1 font-medium text-caixa-blue">
                              {policy.productName}
                            </p>
                          </div>
                        </div>
                        <div className="mt-3 grid grid-cols-2 gap-4 border-t border-gray-200 pt-3 lg:grid-cols-4">
                          <div>
                            <Label className="text-xs text-gray-600">
                              Importância Segurada
                            </Label>
                            <p className="mt-1 text-sm font-semibold">
                              {formatCurrency(policy.coverageAmount)}
                            </p>
                          </div>
                          <div>
                            <Label className="text-xs text-gray-600">
                              Prêmio
                            </Label>
                            <p className="mt-1 text-sm font-semibold">
                              {formatCurrency(policy.premium)}
                            </p>
                          </div>
                          <div>
                            <Label className="text-xs text-gray-600">
                              Vigência Início
                            </Label>
                            <p className="mt-1 text-sm">
                              {formatDate(policy.startDate)}
                            </p>
                          </div>
                          <div>
                            <Label className="text-xs text-gray-600">
                              Vigência Fim
                            </Label>
                            <p className="mt-1 text-sm">
                              {formatDate(policy.endDate)}
                            </p>
                          </div>
                        </div>
                      </div>
                    ))}
                  </div>
                </div>
              </div>
            </>
          )}
        </DialogContent>
      </Dialog>
    </div>
  );
};

export default ClientsQueryPage;
