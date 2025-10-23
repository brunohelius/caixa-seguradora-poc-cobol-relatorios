#!/bin/bash

echo "🎨 Iniciando Frontend - Caixa Seguradora Dashboard..."

# Navega para o diretório frontend
cd "frontend"

# Atualiza o baseURL da API para a porta correta
echo "🔧 Configurando conexão com backend (porta 5555)..."

# Inicia o frontend
echo "🌐 Frontend rodando em: http://localhost:5173"
echo "📊 Dashboard: http://localhost:5173"
echo ""

npm run dev
