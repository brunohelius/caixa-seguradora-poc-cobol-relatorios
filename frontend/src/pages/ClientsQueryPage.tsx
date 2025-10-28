import React, { useState } from 'react';
import { Card } from '../components/ui/card';
import { Button } from '../components/ui/button';
import { Input } from '../components/ui/input';
import { Label } from '../components/ui/label';
import { Table } from '../components/ui/table';

interface Client {
  cpfCnpj: string;
  name: string;
  clientType: 'PF' | 'PJ';
  email: string;
  phone: string;
  address: {
    street: string;
    number: string;
    city: string;
    state: string;
    zipCode: string;
  };
  activePolicies: number;
  totalPremium: number;
}

const ClientsQueryPage: React.FC = () => {
  const [searchTerm, setSearchTerm] = useState('');
  const [searchType, setSearchType] = useState<'cpfcnpj' | 'name'>('cpfcnpj');
  const [clients, setClients] = useState<Client[]>([]);
  const [loading, setLoading] = useState(false);
  const [selectedClient, setSelectedClient] = useState<Client | null>(null);

  const handleSearch = async () => {
    setLoading(true);
    try {
      // TODO: Implementar chamada à API
      // const response = await fetch(`/api/v1/clients/search?type=${searchType}&term=${searchTerm}`);
      // const data = await response.json();
      // setClients(data);

      // Mock data para demonstração
      setTimeout(() => {
        setClients([
          {
            cpfCnpj: '123.456.789-00',
            name: 'João Silva',
            clientType: 'PF',
            email: 'joao.silva@email.com',
            phone: '(11) 98765-4321',
            address: {
              street: 'Rua das Flores',
              number: '123',
              city: 'São Paulo',
              state: 'SP',
              zipCode: '01234-567'
            },
            activePolicies: 2,
            totalPremium: 2350.00
          },
          {
            cpfCnpj: '12.345.678/0001-90',
            name: 'Empresa XYZ Ltda',
            clientType: 'PJ',
            email: 'contato@empresaxyz.com.br',
            phone: '(11) 3456-7890',
            address: {
              street: 'Av. Paulista',
              number: '1000',
              city: 'São Paulo',
              state: 'SP',
              zipCode: '01310-100'
            },
            activePolicies: 5,
            totalPremium: 15000.00
          }
        ]);
        setLoading(false);
      }, 500);
    } catch (error) {
      console.error('Erro ao buscar clientes:', error);
      setLoading(false);
    }
  };

  const formatCurrency = (value: number) => {
    return new Intl.NumberFormat('pt-BR', {
      style: 'currency',
      currency: 'BRL'
    }).format(value);
  };

  const formatCpfCnpj = (value: string) => {
    return value;
  };

  return (
    <div className="space-y-6">
      <div>
        <h1 className="text-3xl font-bold text-gray-900">Consulta de Clientes</h1>
        <p className="text-gray-600 mt-2">
          Pesquise e visualize informações detalhadas de clientes
        </p>
      </div>

      <Card className="p-6">
        <div className="space-y-4">
          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            <div>
              <Label htmlFor="searchType">Tipo de Busca</Label>
              <select
                id="searchType"
                value={searchType}
                onChange={(e) => setSearchType(e.target.value as any)}
                className="w-full px-3 py-2 border border-gray-300 rounded-md focus:outline-none focus:ring-2 focus:ring-blue-500"
              >
                <option value="cpfcnpj">CPF/CNPJ</option>
                <option value="name">Nome</option>
              </select>
            </div>

            <div className="md:col-span-2">
              <Label htmlFor="searchTerm">Termo de Busca</Label>
              <div className="flex gap-2">
                <Input
                  id="searchTerm"
                  type="text"
                  placeholder={
                    searchType === 'cpfcnpj'
                      ? 'Digite o CPF ou CNPJ'
                      : 'Digite o nome do cliente'
                  }
                  value={searchTerm}
                  onChange={(e) => setSearchTerm(e.target.value)}
                  onKeyPress={(e) => e.key === 'Enter' && handleSearch()}
                />
                <Button onClick={handleSearch} disabled={loading || !searchTerm}>
                  {loading ? 'Buscando...' : 'Buscar'}
                </Button>
              </div>
            </div>
          </div>
        </div>
      </Card>

      {clients.length > 0 && (
        <Card className="p-6">
          <h2 className="text-xl font-semibold mb-4">Resultados da Busca</h2>
          <div className="overflow-x-auto">
            <Table>
              <thead>
                <tr className="bg-gray-50">
                  <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                    CPF/CNPJ
                  </th>
                  <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                    Nome/Razão Social
                  </th>
                  <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                    Tipo
                  </th>
                  <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                    Contato
                  </th>
                  <th className="px-4 py-3 text-center text-xs font-medium text-gray-500 uppercase tracking-wider">
                    Apólices Ativas
                  </th>
                  <th className="px-4 py-3 text-right text-xs font-medium text-gray-500 uppercase tracking-wider">
                    Prêmio Total
                  </th>
                  <th className="px-4 py-3 text-center text-xs font-medium text-gray-500 uppercase tracking-wider">
                    Ações
                  </th>
                </tr>
              </thead>
              <tbody className="bg-white divide-y divide-gray-200">
                {clients.map((client, index) => (
                  <tr key={index} className="hover:bg-gray-50">
                    <td className="px-4 py-4 whitespace-nowrap text-sm font-medium text-gray-900">
                      {formatCpfCnpj(client.cpfCnpj)}
                    </td>
                    <td className="px-4 py-4 whitespace-nowrap text-sm text-gray-900">
                      {client.name}
                    </td>
                    <td className="px-4 py-4 whitespace-nowrap text-sm text-gray-500">
                      <span className={`px-2 py-1 inline-flex text-xs leading-5 font-semibold rounded-full ${
                        client.clientType === 'PF'
                          ? 'bg-blue-100 text-blue-800'
                          : 'bg-purple-100 text-purple-800'
                      }`}>
                        {client.clientType === 'PF' ? 'Pessoa Física' : 'Pessoa Jurídica'}
                      </span>
                    </td>
                    <td className="px-4 py-4 whitespace-nowrap text-sm text-gray-500">
                      <div>
                        <div>{client.email}</div>
                        <div className="text-gray-400">{client.phone}</div>
                      </div>
                    </td>
                    <td className="px-4 py-4 whitespace-nowrap text-sm text-gray-900 text-center font-medium">
                      {client.activePolicies}
                    </td>
                    <td className="px-4 py-4 whitespace-nowrap text-sm text-gray-900 text-right font-medium">
                      {formatCurrency(client.totalPremium)}
                    </td>
                    <td className="px-4 py-4 whitespace-nowrap text-center text-sm font-medium">
                      <Button
                        variant="link"
                        className="text-blue-600 hover:text-blue-900"
                        onClick={() => setSelectedClient(client)}
                      >
                        Detalhes
                      </Button>
                    </td>
                  </tr>
                ))}
              </tbody>
            </Table>
          </div>
        </Card>
      )}

      {clients.length === 0 && searchTerm && !loading && (
        <Card className="p-8 text-center">
          <p className="text-gray-500">Nenhum cliente encontrado para os critérios de busca.</p>
        </Card>
      )}

      {selectedClient && (
        <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center p-4 z-50">
          <Card className="max-w-3xl w-full p-6 max-h-[90vh] overflow-y-auto">
            <div className="space-y-6">
              <div className="flex items-start justify-between">
                <div>
                  <h2 className="text-2xl font-bold text-gray-900">
                    {selectedClient.name}
                  </h2>
                  <p className="text-gray-500">
                    {formatCpfCnpj(selectedClient.cpfCnpj)}
                  </p>
                </div>
                <Button
                  variant="outline"
                  onClick={() => setSelectedClient(null)}
                >
                  Fechar
                </Button>
              </div>

              <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                <Card className="p-4">
                  <h3 className="font-semibold text-gray-900 mb-4">Informações Pessoais</h3>
                  <div className="space-y-3">
                    <div>
                      <Label className="text-gray-500">Tipo</Label>
                      <p className="font-medium">
                        {selectedClient.clientType === 'PF' ? 'Pessoa Física' : 'Pessoa Jurídica'}
                      </p>
                    </div>
                    <div>
                      <Label className="text-gray-500">Email</Label>
                      <p className="font-medium">{selectedClient.email}</p>
                    </div>
                    <div>
                      <Label className="text-gray-500">Telefone</Label>
                      <p className="font-medium">{selectedClient.phone}</p>
                    </div>
                  </div>
                </Card>

                <Card className="p-4">
                  <h3 className="font-semibold text-gray-900 mb-4">Endereço</h3>
                  <div className="space-y-3">
                    <div>
                      <Label className="text-gray-500">Logradouro</Label>
                      <p className="font-medium">
                        {selectedClient.address.street}, {selectedClient.address.number}
                      </p>
                    </div>
                    <div>
                      <Label className="text-gray-500">Cidade/Estado</Label>
                      <p className="font-medium">
                        {selectedClient.address.city} - {selectedClient.address.state}
                      </p>
                    </div>
                    <div>
                      <Label className="text-gray-500">CEP</Label>
                      <p className="font-medium">{selectedClient.address.zipCode}</p>
                    </div>
                  </div>
                </Card>
              </div>

              <Card className="p-4">
                <h3 className="font-semibold text-gray-900 mb-4">Informações de Seguro</h3>
                <div className="grid grid-cols-2 gap-4">
                  <div>
                    <Label className="text-gray-500">Apólices Ativas</Label>
                    <p className="text-2xl font-bold text-blue-600">
                      {selectedClient.activePolicies}
                    </p>
                  </div>
                  <div>
                    <Label className="text-gray-500">Prêmio Total Mensal</Label>
                    <p className="text-2xl font-bold text-green-600">
                      {formatCurrency(selectedClient.totalPremium)}
                    </p>
                  </div>
                </div>
              </Card>

              <div className="flex gap-2">
                <Button className="flex-1">Ver Apólices</Button>
                <Button className="flex-1" variant="outline">Ver Sinistros</Button>
                <Button className="flex-1" variant="outline">Ver Histórico</Button>
              </div>
            </div>
          </Card>
        </div>
      )}
    </div>
  );
};

export default ClientsQueryPage;
