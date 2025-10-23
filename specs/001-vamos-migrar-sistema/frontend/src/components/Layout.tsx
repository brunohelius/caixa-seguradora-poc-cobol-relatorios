import React from 'react';
import { Link, useLocation } from 'react-router-dom';

export interface LayoutProps {
  children: React.ReactNode;
}

const Layout: React.FC<LayoutProps> = ({ children }) => {
  const location = useLocation();

  const navigation = [
    { name: 'Dashboard', href: '/', icon: '📊' },
    { name: 'Gerar Relatórios', href: '/reports', icon: '📄' },
    { name: 'Consultar Dados', href: '/query', icon: '🔍' },
    { name: 'Jobs Agendados', href: '/batch-jobs', icon: '⏰' },
    { name: 'Gerenciar Dados', href: '/data-management', icon: '💾' },
  ];

  const isActive = (path: string) => {
    return location.pathname === path;
  };

  return (
    <div className="min-h-screen bg-gray-50">
      {/* Header */}
      <header className="bg-caixa-blue shadow-md">
        <div className="container-caixa">
          <div className="flex items-center justify-between h-16">
            {/* Logo and Title */}
            <div className="flex items-center">
              <div className="flex-shrink-0">
                <h1 className="text-2xl font-bold text-white flex items-center">
                  <span className="mr-2">🏛️</span>
                  Caixa Seguradora
                </h1>
              </div>
              <div className="ml-6 hidden md:block">
                <p className="text-sm text-gray-200">
                  Sistema de Relatórios PREMIT/PREMCED
                </p>
              </div>
            </div>

            {/* User info */}
            <div className="flex items-center">
              <div className="text-right mr-3 hidden md:block">
                <p className="text-sm font-medium text-white">Usuário Sistema</p>
                <p className="text-xs text-gray-300">Migração COBOL → .NET</p>
              </div>
              <div className="h-10 w-10 rounded-full bg-caixa-orange flex items-center justify-center text-white font-bold">
                US
              </div>
            </div>
          </div>
        </div>
      </header>

      {/* Navigation */}
      <nav className="bg-white shadow-sm border-b border-gray-200">
        <div className="container-caixa">
          <div className="flex space-x-1 overflow-x-auto scrollbar-caixa">
            {navigation.map((item) => (
              <Link
                key={item.name}
                to={item.href}
                className={`flex items-center px-4 py-3 text-sm font-medium border-b-2 transition-colors duration-200 whitespace-nowrap ${
                  isActive(item.href)
                    ? 'border-caixa-blue text-caixa-blue bg-blue-50'
                    : 'border-transparent text-gray-600 hover:text-gray-900 hover:border-gray-300'
                }`}
              >
                <span className="mr-2">{item.icon}</span>
                {item.name}
              </Link>
            ))}
          </div>
        </div>
      </nav>

      {/* Main Content */}
      <main className="container-caixa py-6">
        {children}
      </main>

      {/* Footer */}
      <footer className="bg-white border-t border-gray-200 mt-12">
        <div className="container-caixa py-6">
          <div className="flex flex-col md:flex-row justify-between items-center">
            <div className="text-sm text-gray-600 mb-4 md:mb-0">
              <p>
                © 2025 Caixa Seguradora - Sistema de Migração COBOL para .NET
              </p>
              <p className="mt-1 text-xs text-gray-500">
                Circular SUSEP 360 - Relatórios PREMIT e PREMCED
              </p>
            </div>
            <div className="flex space-x-6 text-sm">
              <a href="#" className="text-gray-600 hover:text-caixa-blue">
                Documentação
              </a>
              <a href="#" className="text-gray-600 hover:text-caixa-blue">
                Suporte
              </a>
              <a href="#" className="text-gray-600 hover:text-caixa-blue">
                Versão: 1.0.0
              </a>
            </div>
          </div>
        </div>
      </footer>
    </div>
  );
};

export default Layout;
