#!/bin/bash

echo "🚀 Iniciando Backend - Caixa Seguradora Premium Reporting API..."

# Carrega variáveis de ambiente do .env
if [ -f "backend/.env" ]; then
    echo "📄 Carregando configuração do backend/.env"
    # shellcheck disable=SC1091
    set -a
    source backend/.env
    set +a
fi

# Navega para o diretório da API
cd "backend/src/CaixaSeguradora.Api"

# Inicia o backend
echo "📡 Backend rodando em: http://localhost:${BACKEND_HTTP_PORT:-5555}"
echo "📚 Swagger UI: http://localhost:${BACKEND_HTTP_PORT:-5555}/swagger"
echo "📊 Health Check: http://localhost:${BACKEND_HTTP_PORT:-5555}/api/v1/health"
echo ""

dotnet run
