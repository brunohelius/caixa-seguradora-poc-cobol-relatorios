#!/bin/bash

# Script de Validação Rápida do Frontend
# Executa testes Playwright e gera relatório

set -e

echo "=========================================="
echo "🧪 VALIDAÇÃO FRONTEND - PLAYWRIGHT"
echo "=========================================="
echo ""

# Cores para output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "📦 Verificando dependências..."
if ! command -v npx &> /dev/null; then
    echo -e "${RED}❌ Node.js não encontrado. Instale Node.js primeiro.${NC}"
    exit 1
fi
echo -e "${GREEN}✅ Node.js instalado${NC}"

echo ""
echo "🔍 Verificando Playwright..."
if [ ! -d "node_modules/@playwright" ]; then
    echo -e "${YELLOW}⚠️  Playwright não encontrado. Instalando...${NC}"
    npm install @playwright/test
fi
echo -e "${GREEN}✅ Playwright instalado${NC}"

echo ""
echo "🌐 Verificando navegadores..."
if [ ! -d "$HOME/Library/Caches/ms-playwright/chromium_headless_shell-1194" ]; then
    echo -e "${YELLOW}⚠️  Navegadores não encontrados. Instalando...${NC}"
    npx playwright install chromium
fi
echo -e "${GREEN}✅ Navegadores prontos${NC}"

echo ""
echo "=========================================="
echo "🚀 EXECUTANDO TESTES..."
echo "=========================================="
echo ""

# Executar testes
npx playwright test tests/e2e/comprehensive-validation.spec.ts --reporter=list

# Capturar código de saída
TEST_RESULT=$?

echo ""
echo "=========================================="
echo "📊 RESULTADO DOS TESTES"
echo "=========================================="
echo ""

if [ $TEST_RESULT -eq 0 ]; then
    echo -e "${GREEN}✅ TODOS OS TESTES PASSARAM!${NC}"
    echo ""
    echo "📄 Relatórios gerados:"
    echo "  • HTML: playwright-report/index.html"
    echo "  • Resumo: RESUMO_TESTES.md"
    echo "  • Completo: RELATORIO_VALIDACAO_FRONTEND.md"
    echo ""
    echo "Para ver o relatório interativo:"
    echo "  npx playwright show-report"
else
    echo -e "${YELLOW}⚠️  ALGUNS TESTES FALHARAM${NC}"
    echo ""
    echo "Verifique os logs acima para detalhes."
    echo ""
    echo "📄 Relatórios gerados:"
    echo "  • HTML: playwright-report/index.html"
    echo "  • Resumo: RESUMO_TESTES.md"
    echo ""
    echo "Para análise detalhada:"
    echo "  npx playwright show-report"
    echo ""
    echo "Problemas comuns:"
    echo "  1. Backend não está rodando (porta 5000/5001)"
    echo "  2. Dados mock não foram carregados"
    echo "  3. Seletor CSS precisa ajuste"
fi

echo ""
echo "=========================================="
exit $TEST_RESULT
