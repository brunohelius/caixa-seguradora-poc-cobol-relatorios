import React from 'react';
import { Link, useLocation } from 'react-router-dom';

export interface LayoutProps {
  children: React.ReactNode;
}

const CAIXA_LOGO = "data:image/png;base64,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";

const Layout: React.FC<LayoutProps> = ({ children }) => {
  const location = useLocation();

  const navigation = [
    { name: 'Dashboard', href: '/' },
    { name: 'Gerar Relatórios', href: '/reports' },
    { name: 'Consultar Dados', href: '/query' },
    { name: 'Jobs Agendados', href: '/batch-jobs' },
    { name: 'Gerenciar Dados', href: '/data-management' },
  ];

  const isActive = (path: string) => {
    return location.pathname === path;
  };

  return (
    <div>
      <header>
        <div className="content-wrapper">
          <div className="float-left">
            <div className="site-title">
              <a href="/">
                <img
                  src={CAIXA_LOGO}
                  alt="Caixa Seguradora"
                  style={{ height: '60px', width: 'auto', display: 'inline-block' }}
                />
              </a>
            </div>
          </div>
          <div className="float-right">
            <section id="login">
              <ul>
                <li className="username">
                  <span>Sistema de Relatórios PREMIT/PREMCED</span>
                </li>
              </ul>
            </section>
            <nav>
              <ul id="menu">
                {navigation.map((item) => (
                  <li key={item.name}>
                    <Link
                      to={item.href}
                      style={{
                        color: isActive(item.href) ? '#000' : '#999',
                        fontWeight: isActive(item.href) ? 'bold' : 'normal'
                      }}
                    >
                      {item.name}
                    </Link>
                  </li>
                ))}
              </ul>
            </nav>
          </div>
        </div>
      </header>

      <main id="body">
        <section className="content-wrapper main-content clear-fix">
          {children}
        </section>
      </main>

      <footer>
        <div className="content-wrapper">
          <div className="float-left">
            <p>&copy; 2025 - Caixa Seguradora - Sistema de Migração COBOL para .NET</p>
            <p>Circular SUSEP 360 - Relatórios PREMIT e PREMCED</p>
          </div>
        </div>
      </footer>
    </div>
  );
};

export default Layout;
