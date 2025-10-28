import React, { useState } from 'react';
import { Card } from '../components/ui/card';
import { Button } from '../components/ui/button';
import { Input } from '../components/ui/input';
import { Label } from '../components/ui/label';
import { Table } from '../components/ui/table';

interface Policy {
  policyNumber: string;
  endorsementNumber: number;
  companyCode: number;
  productCode: string;
  effectiveDate: string;
  expirationDate: string;
  insuredName: string;
  premiumAmount: number;
  status: string;
}

const PoliciesQueryPage: React.FC = () => {
  const [searchTerm, setSearchTerm] = useState('');
  const [searchType, setSearchType] = useState<'policy' | 'client' | 'product'>('policy');
  const [policies, setPolicies] = useState<Policy[]>([]);
  const [loading, setLoading] = useState(false);

  const handleSearch = async () => {
    setLoading(true);
    try {
      // TODO: Implementar chamada à API
      // const response = await fetch(`/api/v1/policies/search?type=${searchType}&term=${searchTerm}`);
      // const data = await response.json();
      // setPolicies(data);

      // Mock data para demonstração
      setTimeout(() => {
        setPolicies([
          {
            policyNumber: '1000001',
            endorsementNumber: 0,
            companyCode: 1,
            productCode: 'AUTO',
            effectiveDate: '2025-01-15',
            expirationDate: '2026-01-15',
            insuredName: 'João Silva',
            premiumAmount: 1500.00,
            status: 'Ativa'
          },
          {
            policyNumber: '1000002',
            endorsementNumber: 1,
            companyCode: 1,
            productCode: 'VIDA',
            effectiveDate: '2025-02-01',
            expirationDate: '2026-02-01',
            insuredName: 'Maria Santos',
            premiumAmount: 850.00,
            status: 'Ativa'
          }
        ]);
        setLoading(false);
      }, 500);
    } catch (error) {
      console.error('Erro ao buscar apólices:', error);
      setLoading(false);
    }
  };

  const formatCurrency = (value: number) => {
    return new Intl.NumberFormat('pt-BR', {
      style: 'currency',
      currency: 'BRL'
    }).format(value);
  };

  const formatDate = (date: string) => {
    return new Date(date).toLocaleDateString('pt-BR');
  };

  return (
    <div className="space-y-6">
      <div>
        <h1 className="text-3xl font-bold text-gray-900">Consulta de Apólices</h1>
        <p className="text-gray-600 mt-2">
          Pesquise e visualize informações detalhadas de apólices de seguro
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
                <option value="policy">Número da Apólice</option>
                <option value="client">CPF/CNPJ do Cliente</option>
                <option value="product">Código do Produto</option>
              </select>
            </div>

            <div className="md:col-span-2">
              <Label htmlFor="searchTerm">Termo de Busca</Label>
              <div className="flex gap-2">
                <Input
                  id="searchTerm"
                  type="text"
                  placeholder={
                    searchType === 'policy' ? 'Digite o número da apólice' :
                    searchType === 'client' ? 'Digite o CPF/CNPJ' :
                    'Digite o código do produto'
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

      {policies.length > 0 && (
        <Card className="p-6">
          <h2 className="text-xl font-semibold mb-4">Resultados da Busca</h2>
          <div className="overflow-x-auto">
            <Table>
              <thead>
                <tr className="bg-gray-50">
                  <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                    Apólice
                  </th>
                  <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                    Endosso
                  </th>
                  <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                    Produto
                  </th>
                  <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                    Segurado
                  </th>
                  <th className="px-4 py-3 text-left text-xs font-medium text-gray-500 uppercase tracking-wider">
                    Vigência
                  </th>
                  <th className="px-4 py-3 text-right text-xs font-medium text-gray-500 uppercase tracking-wider">
                    Prêmio
                  </th>
                  <th className="px-4 py-3 text-center text-xs font-medium text-gray-500 uppercase tracking-wider">
                    Status
                  </th>
                  <th className="px-4 py-3 text-center text-xs font-medium text-gray-500 uppercase tracking-wider">
                    Ações
                  </th>
                </tr>
              </thead>
              <tbody className="bg-white divide-y divide-gray-200">
                {policies.map((policy, index) => (
                  <tr key={index} className="hover:bg-gray-50">
                    <td className="px-4 py-4 whitespace-nowrap text-sm font-medium text-gray-900">
                      {policy.policyNumber}
                    </td>
                    <td className="px-4 py-4 whitespace-nowrap text-sm text-gray-500">
                      {policy.endorsementNumber}
                    </td>
                    <td className="px-4 py-4 whitespace-nowrap text-sm text-gray-500">
                      {policy.productCode}
                    </td>
                    <td className="px-4 py-4 whitespace-nowrap text-sm text-gray-900">
                      {policy.insuredName}
                    </td>
                    <td className="px-4 py-4 whitespace-nowrap text-sm text-gray-500">
                      {formatDate(policy.effectiveDate)} - {formatDate(policy.expirationDate)}
                    </td>
                    <td className="px-4 py-4 whitespace-nowrap text-sm text-gray-900 text-right font-medium">
                      {formatCurrency(policy.premiumAmount)}
                    </td>
                    <td className="px-4 py-4 whitespace-nowrap text-center">
                      <span className={`px-2 py-1 inline-flex text-xs leading-5 font-semibold rounded-full ${
                        policy.status === 'Ativa'
                          ? 'bg-green-100 text-green-800'
                          : 'bg-red-100 text-red-800'
                      }`}>
                        {policy.status}
                      </span>
                    </td>
                    <td className="px-4 py-4 whitespace-nowrap text-center text-sm font-medium">
                      <Button variant="link" className="text-blue-600 hover:text-blue-900">
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

      {policies.length === 0 && searchTerm && !loading && (
        <Card className="p-8 text-center">
          <p className="text-gray-500">Nenhuma apólice encontrada para os critérios de busca.</p>
        </Card>
      )}
    </div>
  );
};

export default PoliciesQueryPage;
