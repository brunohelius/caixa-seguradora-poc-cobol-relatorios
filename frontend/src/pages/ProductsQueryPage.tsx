import React, { useState } from 'react';
import { Card } from '../components/ui/card';
import { Button } from '../components/ui/button';
import { Input } from '../components/ui/input';
import { Label } from '../components/ui/label';

interface Product {
  productCode: string;
  productName: string;
  ramoSusep: number;
  ramoName: string;
  active: boolean;
  description: string;
  minPremium: number;
  maxPremium: number;
}

const ProductsQueryPage: React.FC = () => {
  const [searchTerm, setSearchTerm] = useState('');
  const [products, setProducts] = useState<Product[]>([]);
  const [loading, setLoading] = useState(false);
  const [selectedProduct, setSelectedProduct] = useState<Product | null>(null);

  const handleSearch = async () => {
    setLoading(true);
    try {
      // TODO: Implementar chamada à API
      // const response = await fetch(`/api/v1/products/search?term=${searchTerm}`);
      // const data = await response.json();
      // setProducts(data);

      // Mock data para demonstração
      setTimeout(() => {
        setProducts([
          {
            productCode: 'AUTO',
            productName: 'Seguro Automóvel',
            ramoSusep: 531,
            ramoName: 'Automóvel',
            active: true,
            description: 'Cobertura completa para veículos automotores',
            minPremium: 500.00,
            maxPremium: 5000.00
          },
          {
            productCode: 'VIDA',
            productName: 'Seguro de Vida',
            ramoSusep: 167,
            ramoName: 'Vida Individual',
            active: true,
            description: 'Proteção financeira para você e sua família',
            minPremium: 100.00,
            maxPremium: 10000.00
          },
          {
            productCode: 'RESIDENCIAL',
            productName: 'Seguro Residencial',
            ramoSusep: 193,
            ramoName: 'Residencial',
            active: true,
            description: 'Proteção completa para sua casa',
            minPremium: 200.00,
            maxPremium: 3000.00
          }
        ]);
        setLoading(false);
      }, 500);
    } catch (error) {
      console.error('Erro ao buscar produtos:', error);
      setLoading(false);
    }
  };

  const formatCurrency = (value: number) => {
    return new Intl.NumberFormat('pt-BR', {
      style: 'currency',
      currency: 'BRL'
    }).format(value);
  };

  return (
    <div className="space-y-6">
      <div>
        <h1 className="text-3xl font-bold text-gray-900">Consulta de Produtos</h1>
        <p className="text-gray-600 mt-2">
          Pesquise e visualize informações sobre produtos de seguro disponíveis
        </p>
      </div>

      <Card className="p-6">
        <div className="space-y-4">
          <div>
            <Label htmlFor="searchTerm">Buscar Produto</Label>
            <div className="flex gap-2">
              <Input
                id="searchTerm"
                type="text"
                placeholder="Digite o código ou nome do produto"
                value={searchTerm}
                onChange={(e) => setSearchTerm(e.target.value)}
                onKeyPress={(e) => e.key === 'Enter' && handleSearch()}
              />
              <Button onClick={handleSearch} disabled={loading || !searchTerm}>
                {loading ? 'Buscando...' : 'Buscar'}
              </Button>
              <Button
                variant="outline"
                onClick={() => {
                  setSearchTerm('');
                  handleSearch();
                }}
              >
                Listar Todos
              </Button>
            </div>
          </div>
        </div>
      </Card>

      {products.length > 0 && (
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
          {products.map((product) => (
            <Card
              key={product.productCode}
              className="p-6 hover:shadow-lg transition-shadow cursor-pointer"
              onClick={() => setSelectedProduct(product)}
            >
              <div className="space-y-4">
                <div className="flex items-start justify-between">
                  <div>
                    <h3 className="text-lg font-semibold text-gray-900">
                      {product.productName}
                    </h3>
                    <p className="text-sm text-gray-500">
                      Código: {product.productCode}
                    </p>
                  </div>
                  <span className={`px-2 py-1 text-xs font-semibold rounded-full ${
                    product.active
                      ? 'bg-green-100 text-green-800'
                      : 'bg-red-100 text-red-800'
                  }`}>
                    {product.active ? 'Ativo' : 'Inativo'}
                  </span>
                </div>

                <div className="border-t pt-4">
                  <div className="grid grid-cols-2 gap-2 text-sm">
                    <div>
                      <p className="text-gray-500">Ramo SUSEP</p>
                      <p className="font-medium">{product.ramoSusep}</p>
                    </div>
                    <div>
                      <p className="text-gray-500">Ramo</p>
                      <p className="font-medium">{product.ramoName}</p>
                    </div>
                  </div>
                </div>

                <div className="border-t pt-4">
                  <p className="text-sm text-gray-600 mb-3">
                    {product.description}
                  </p>
                  <div className="grid grid-cols-2 gap-2 text-sm">
                    <div>
                      <p className="text-gray-500">Prêmio Mínimo</p>
                      <p className="font-medium text-green-600">
                        {formatCurrency(product.minPremium)}
                      </p>
                    </div>
                    <div>
                      <p className="text-gray-500">Prêmio Máximo</p>
                      <p className="font-medium text-blue-600">
                        {formatCurrency(product.maxPremium)}
                      </p>
                    </div>
                  </div>
                </div>

                <Button className="w-full" variant="outline">
                  Ver Detalhes
                </Button>
              </div>
            </Card>
          ))}
        </div>
      )}

      {products.length === 0 && searchTerm && !loading && (
        <Card className="p-8 text-center">
          <p className="text-gray-500">Nenhum produto encontrado para os critérios de busca.</p>
        </Card>
      )}

      {selectedProduct && (
        <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center p-4 z-50">
          <Card className="max-w-2xl w-full p-6 max-h-[90vh] overflow-y-auto">
            <div className="space-y-4">
              <div className="flex items-start justify-between">
                <div>
                  <h2 className="text-2xl font-bold text-gray-900">
                    {selectedProduct.productName}
                  </h2>
                  <p className="text-gray-500">Código: {selectedProduct.productCode}</p>
                </div>
                <Button
                  variant="outline"
                  onClick={() => setSelectedProduct(null)}
                >
                  Fechar
                </Button>
              </div>

              <div className="border-t pt-4 space-y-4">
                <div className="grid grid-cols-2 gap-4">
                  <div>
                    <Label>Status</Label>
                    <p className="font-medium">
                      {selectedProduct.active ? 'Ativo' : 'Inativo'}
                    </p>
                  </div>
                  <div>
                    <Label>Ramo SUSEP</Label>
                    <p className="font-medium">{selectedProduct.ramoSusep}</p>
                  </div>
                  <div>
                    <Label>Nome do Ramo</Label>
                    <p className="font-medium">{selectedProduct.ramoName}</p>
                  </div>
                </div>

                <div>
                  <Label>Descrição</Label>
                  <p className="text-gray-600">{selectedProduct.description}</p>
                </div>

                <div className="grid grid-cols-2 gap-4">
                  <div>
                    <Label>Prêmio Mínimo</Label>
                    <p className="text-xl font-bold text-green-600">
                      {formatCurrency(selectedProduct.minPremium)}
                    </p>
                  </div>
                  <div>
                    <Label>Prêmio Máximo</Label>
                    <p className="text-xl font-bold text-blue-600">
                      {formatCurrency(selectedProduct.maxPremium)}
                    </p>
                  </div>
                </div>
              </div>
            </div>
          </Card>
        </div>
      )}
    </div>
  );
};

export default ProductsQueryPage;
