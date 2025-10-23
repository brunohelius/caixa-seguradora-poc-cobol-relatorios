# Guia de Implantação - Sistema de Relatórios de Prêmios

**Versão**: 1.0.0
**Data**: Outubro 2025
**Projeto**: Migração COBOL RG1866B para .NET 9 + React

---

## Visão Geral

Este guia fornece instruções passo a passo para implantar o Sistema de Relatórios de Prêmios Caixa Seguradora em diferentes ambientes (desenvolvimento, homologação e produção).

## Arquitetura de Implantação

```
┌─────────────────┐
│   Load Balancer │
│   (NGINX/ALB)   │
└────────┬────────┘
         │
    ┌────┴────┐
    │         │
┌───▼──┐  ┌──▼───┐
│ App  │  │ App  │  (Frontend: React SPA)
│ Srv  │  │ Srv  │  Servido via NGINX
└──────┘  └──────┘
         │
┌────────▼─────────┐
│  API Backend     │
│  (.NET 9 API)    │  Portas: 5000 (HTTP), 5001 (HTTPS)
└────────┬─────────┘
         │
┌────────▼─────────┐
│  Banco de Dados  │
│  (SQLite/        │  Desenvolvimento: SQLite
│   PostgreSQL)    │  Produção: PostgreSQL/SQL Server
└──────────────────┘
```

---

## Pré-requisitos

### Ambiente de Desenvolvimento
- .NET 9.0 SDK
- Node.js 18+ e npm
- Git
- Editor de código (VS Code recomendado)

### Ambiente de Produção
- Docker 24+ e Docker Compose
- ou
- Servidor Linux/Windows com .NET 9 Runtime
- NGINX ou IIS (para servir frontend)
- Banco de dados (PostgreSQL ou SQL Server recomendado)

---

## Implantação com Docker Compose (Recomendado)

### 1. Preparação

```bash
# Clone o repositório
git clone <repository-url>
cd "POC Cobol"

# Configurar variáveis de ambiente
cp .env.example .env
# Editar .env com configurações específicas do ambiente
```

### 2. Arquivo docker-compose.yml

O arquivo `docker-compose.yml` na raiz do projeto define 3 serviços:

```yaml
version: '3.8'

services:
  backend:
    build: ./backend
    ports:
      - "5000:5000"
    environment:
      - ASPNETCORE_ENVIRONMENT=Production
      - ASPNETCORE_URLS=http://+:5000
      - ConnectionStrings__DefaultConnection=Data Source=/app/data/premium_reporting.db
    volumes:
      - ./data:/app/data
      - ./logs:/app/logs
    restart: unless-stopped

  frontend:
    build: ./frontend
    ports:
      - "80:80"
    environment:
      - VITE_API_BASE_URL=http://backend:5000
    depends_on:
      - backend
    restart: unless-stopped

  nginx:
    image: nginx:alpine
    ports:
      - "443:443"
    volumes:
      - ./nginx.conf:/etc/nginx/nginx.conf:ro
      - ./ssl:/etc/nginx/ssl:ro
    depends_on:
      - frontend
      - backend
    restart: unless-stopped
```

### 3. Construir e Executar

```bash
# Construir imagens
docker-compose build

# Iniciar todos os serviços
docker-compose up -d

# Verificar logs
docker-compose logs -f

# Verificar status
docker-compose ps
```

### 4. Verificação

```bash
# Health check do backend
curl http://localhost:5000/health

# Acessar frontend
# Abrir navegador: http://localhost (ou https://localhost:443 se NGINX configurado)

# Acessar Swagger
# http://localhost:5000/swagger
```

### 5. Parar Serviços

```bash
# Parar sem remover volumes
docker-compose stop

# Parar e remover containers
docker-compose down

# Parar e remover containers + volumes
docker-compose down -v
```

---

## Implantação Manual (Sem Docker)

### Backend (.NET 9 API)

#### 1. Publicar Aplicação

```bash
cd backend

# Build para produção
dotnet publish src/CaixaSeguradora.Api/CaixaSeguradora.Api.csproj \
  -c Release \
  -o /var/www/caixaseguradora-api \
  --self-contained false

# Ou com runtime incluído
dotnet publish src/CaixaSeguradora.Api/CaixaSeguradora.Api.csproj \
  -c Release \
  -o /var/www/caixaseguradora-api \
  --self-contained true \
  -r linux-x64
```

#### 2. Configurar appsettings.Production.json

```json
{
  "Logging": {
    "LogLevel": {
      "Default": "Warning",
      "Microsoft": "Warning",
      "Microsoft.Hosting.Lifetime": "Information"
    }
  },
  "ConnectionStrings": {
    "DefaultConnection": "Server=db-server;Database=PremiumReporting;User Id=app_user;Password=CHANGE_ME;"
  },
  "AllowedHosts": "*",
  "Hangfire": {
    "ConnectionString": "Server=db-server;Database=Hangfire;User Id=app_user;Password=CHANGE_ME;"
  },
  "Email": {
    "SmtpServer": "smtp.caixaseguradora.com.br",
    "SmtpPort": 587,
    "UseSsl": true,
    "Username": "noreply@caixaseguradora.com.br",
    "Password": "CHANGE_ME"
  }
}
```

#### 3. Criar Serviço Systemd (Linux)

```bash
sudo nano /etc/systemd/system/caixaseguradora-api.service
```

```ini
[Unit]
Description=Caixa Seguradora Premium Reporting API
After=network.target

[Service]
Type=notify
WorkingDirectory=/var/www/caixaseguradora-api
ExecStart=/usr/bin/dotnet /var/www/caixaseguradora-api/CaixaSeguradora.Api.dll
Restart=always
RestartSec=10
KillSignal=SIGINT
SyslogIdentifier=caixaseguradora-api
User=www-data
Environment=ASPNETCORE_ENVIRONMENT=Production
Environment=DOTNET_PRINT_TELEMETRY_MESSAGE=false

[Install]
WantedBy=multi-user.target
```

```bash
# Habilitar e iniciar serviço
sudo systemctl daemon-reload
sudo systemctl enable caixaseguradora-api
sudo systemctl start caixaseguradora-api

# Verificar status
sudo systemctl status caixaseguradora-api

# Ver logs
sudo journalctl -u caixaseguradora-api -f
```

#### 4. Aplicar Migrações de Banco de Dados

```bash
cd /var/www/caixaseguradora-api

# Executar migrações
dotnet ef database update --project CaixaSeguradora.Infrastructure.dll

# Ou usando script SQL
dotnet ef migrations script --output migration.sql --idempotent
# Aplicar migration.sql no banco de dados manualmente
```

---

### Frontend (React SPA)

#### 1. Build para Produção

```bash
cd frontend

# Configurar URL da API
echo "VITE_API_BASE_URL=https://api.caixaseguradora.com.br" > .env.production

# Build
npm run build

# Arquivos de produção estarão em: dist/
```

#### 2. Configurar NGINX

```bash
sudo nano /etc/nginx/sites-available/caixaseguradora
```

```nginx
server {
    listen 80;
    listen [::]:80;
    server_name app.caixaseguradora.com.br;

    # Redirecionar HTTP para HTTPS
    return 301 https://$server_name$request_uri;
}

server {
    listen 443 ssl http2;
    listen [::]:443 ssl http2;
    server_name app.caixaseguradora.com.br;

    # SSL Certificates
    ssl_certificate /etc/nginx/ssl/caixaseguradora.crt;
    ssl_certificate_key /etc/nginx/ssl/caixaseguradora.key;

    # SSL Configuration
    ssl_protocols TLSv1.2 TLSv1.3;
    ssl_ciphers HIGH:!aNULL:!MD5;
    ssl_prefer_server_ciphers on;

    # Root directory
    root /var/www/caixaseguradora-frontend;
    index index.html;

    # Gzip compression
    gzip on;
    gzip_types text/plain text/css application/json application/javascript text/xml application/xml application/xml+rss text/javascript;

    # React Router (SPA)
    location / {
        try_files $uri $uri/ /index.html;
    }

    # Cache static assets
    location ~* \.(js|css|png|jpg|jpeg|gif|ico|svg|woff|woff2|ttf|eot)$ {
        expires 1y;
        add_header Cache-Control "public, immutable";
    }

    # Proxy API requests
    location /api/ {
        proxy_pass http://localhost:5000;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection 'upgrade';
        proxy_set_header Host $host;
        proxy_cache_bypass $http_upgrade;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
}
```

```bash
# Copiar build para NGINX
sudo mkdir -p /var/www/caixaseguradora-frontend
sudo cp -r dist/* /var/www/caixaseguradora-frontend/

# Ativar site
sudo ln -s /etc/nginx/sites-available/caixaseguradora /etc/nginx/sites-enabled/

# Testar configuração
sudo nginx -t

# Recarregar NGINX
sudo systemctl reload nginx
```

---

## Implantação em Kubernetes

### 1. Criar Imagens Docker

```bash
# Backend
docker build -t caixaseguradora/api:1.0.0 ./backend

# Frontend
docker build -t caixaseguradora/frontend:1.0.0 ./frontend

# Push para registry
docker push caixaseguradora/api:1.0.0
docker push caixaseguradora/frontend:1.0.0
```

### 2. Manifests Kubernetes

**backend-deployment.yaml**:
```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: api-backend
spec:
  replicas: 3
  selector:
    matchLabels:
      app: api-backend
  template:
    metadata:
      labels:
        app: api-backend
    spec:
      containers:
      - name: api
        image: caixaseguradora/api:1.0.0
        ports:
        - containerPort: 5000
        env:
        - name: ASPNETCORE_ENVIRONMENT
          value: "Production"
        - name: ConnectionStrings__DefaultConnection
          valueFrom:
            secretKeyRef:
              name: db-secret
              key: connection-string
        resources:
          requests:
            memory: "512Mi"
            cpu: "250m"
          limits:
            memory: "1Gi"
            cpu: "500m"
        livenessProbe:
          httpGet:
            path: /health
            port: 5000
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /health
            port: 5000
          initialDelaySeconds: 5
          periodSeconds: 5
---
apiVersion: v1
kind: Service
metadata:
  name: api-backend-service
spec:
  selector:
    app: api-backend
  ports:
  - protocol: TCP
    port: 5000
    targetPort: 5000
  type: ClusterIP
```

**frontend-deployment.yaml**:
```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: frontend
spec:
  replicas: 2
  selector:
    matchLabels:
      app: frontend
  template:
    metadata:
      labels:
        app: frontend
    spec:
      containers:
      - name: frontend
        image: caixaseguradora/frontend:1.0.0
        ports:
        - containerPort: 80
        resources:
          requests:
            memory: "128Mi"
            cpu: "100m"
          limits:
            memory: "256Mi"
            cpu: "200m"
---
apiVersion: v1
kind: Service
metadata:
  name: frontend-service
spec:
  selector:
    app: frontend
  ports:
  - protocol: TCP
    port: 80
    targetPort: 80
  type: LoadBalancer
```

### 3. Aplicar Manifests

```bash
# Criar secrets
kubectl create secret generic db-secret \
  --from-literal=connection-string='Server=...'

# Aplicar deployments
kubectl apply -f backend-deployment.yaml
kubectl apply -f frontend-deployment.yaml

# Verificar
kubectl get pods
kubectl get services
```

---

## Configurações de Ambiente

### Variáveis de Ambiente - Backend

| Variável | Desenvolvimento | Produção | Descrição |
|----------|-----------------|----------|-----------|
| `ASPNETCORE_ENVIRONMENT` | Development | Production | Ambiente de execução |
| `ASPNETCORE_URLS` | http://localhost:5000 | http://+:5000 | URLs de binding |
| `ConnectionStrings__DefaultConnection` | Data Source=premium_reporting.db | Server=...;Database=... | String de conexão DB |
| `Serilog__MinimumLevel__Default` | Debug | Warning | Nível de log |
| `Hangfire__ConnectionString` | (In-Memory) | Server=...;Database=Hangfire | Hangfire storage |

### Variáveis de Ambiente - Frontend

| Variável | Desenvolvimento | Produção | Descrição |
|----------|-----------------|----------|-----------|
| `VITE_API_BASE_URL` | http://localhost:5000 | https://api.caixaseguradora.com.br | Base URL da API |

---

## Migração de Banco de Dados

### PostgreSQL (Recomendado para Produção)

```bash
# 1. Alterar ConnectionString em appsettings.Production.json
# 2. Instalar provider PostgreSQL
dotnet add package Npgsql.EntityFrameworkCore.PostgreSQL

# 3. Atualizar DbContext para usar PostgreSQL
# Em Program.cs:
# options.UseNpgsql(configuration.GetConnectionString("DefaultConnection"));

# 4. Gerar nova migration
dotnet ef migrations add InitialPostgreSQL

# 5. Aplicar migration
dotnet ef database update
```

### SQL Server

```bash
# 1. Instalar provider SQL Server
dotnet add package Microsoft.EntityFrameworkCore.SqlServer

# 2. Atualizar DbContext
# options.UseSqlServer(configuration.GetConnectionString("DefaultConnection"));

# 3. Aplicar migrations
dotnet ef database update
```

---

## Segurança

### HTTPS/SSL Configuration

#### Visão Geral

O sistema suporta HTTPS para garantir comunicação segura entre clientes e servidores. A configuração de certificados SSL/TLS é obrigatória em ambientes de produção.

#### Tipos de Certificados Suportados

| Formato | Extensão | Descrição | Uso Recomendado |
|---------|----------|-----------|-----------------|
| **PFX/PKCS12** | .pfx, .p12 | Formato binário que contém certificado + chave privada | Produção Windows/Linux, mais simples de configurar |
| **PEM** | .pem, .crt, .key | Formato texto (Base64) com arquivos separados | Produção Linux, compatível com Let's Encrypt |

#### Requisitos de Certificado

**Requisitos de Segurança Obrigatórios (NFR-002)**:
- ✅ TLS 1.2 ou superior (TLS 1.3 preferencial)
- ✅ Cipher suites fortes apenas (sem RC4, MD5, DES)
- ✅ Certificado válido de CA confiável (produção)
- ✅ Certificado deve corresponder ao nome do domínio
- ✅ Certificado não expirado (monitoramento de expiração recomendado)
- ✅ Chave privada seguramente armazenada (permissões 600, não em repositório)

**Tipos de Certificado por Ambiente**:
- **Desenvolvimento**: Certificado auto-assinado (.NET dev-certs)
- **Homologação**: Let's Encrypt ou certificado corporativo
- **Produção**: Certificado corporativo de CA confiável ou Let's Encrypt

**Certificados Wildcard vs Específicos**:
- **Wildcard** (*.caixaseguradora.com.br): Cobre múltiplos subdomínios
- **Específico** (api.caixaseguradora.com.br): Apenas um domínio
- **SAN (Subject Alternative Name)**: Cobre múltiplos domínios específicos

---

### Configuração de HTTPS por Ambiente

#### 1. Desenvolvimento (Certificado Auto-Assinado)

```bash
# Gerar certificado de desenvolvimento .NET
dotnet dev-certs https --clean
dotnet dev-certs https --trust

# Localização do certificado (Windows)
# %APPDATA%\ASP.NET\https\

# Localização do certificado (Linux/macOS)
# ~/.aspnet/https/

# Verificar certificado
dotnet dev-certs https --check
```

**Configuração em appsettings.Development.json**:
```json
{
  "Certificate": {
    "Path": "",
    "Password": "",
    "Type": "Development"
  }
}
```

**Nota**: Em desenvolvimento, deixe `Path` vazio para usar o certificado dev padrão do .NET.

---

#### 2. Homologação/Produção com Let's Encrypt (Linux)

**Passo 1: Instalar Certbot**
```bash
# Ubuntu/Debian
sudo apt-get update
sudo apt-get install certbot

# CentOS/RHEL
sudo yum install certbot
```

**Passo 2: Obter Certificado (NGINX + Let's Encrypt)**
```bash
# Para NGINX (recomendado)
sudo certbot --nginx -d api.caixaseguradora.com.br

# Ou para Apache
sudo certbot --apache -d api.caixaseguradora.com.br

# Ou standalone (sem servidor web instalado)
sudo certbot certonly --standalone -d api.caixaseguradora.com.br
```

**Passo 3: Converter para PFX (para uso com Kestrel)**
```bash
# Certificados Let's Encrypt ficam em /etc/letsencrypt/live/<domain>/
DOMAIN="api.caixaseguradora.com.br"
CERT_DIR="/etc/letsencrypt/live/$DOMAIN"

# Converter PEM para PFX
sudo openssl pkcs12 -export \
  -out /etc/ssl/certs/caixaseguradora.pfx \
  -inkey $CERT_DIR/privkey.pem \
  -in $CERT_DIR/cert.pem \
  -certfile $CERT_DIR/chain.pem \
  -password pass:SUA_SENHA_FORTE_AQUI

# Definir permissões corretas
sudo chmod 600 /etc/ssl/certs/caixaseguradora.pfx
sudo chown www-data:www-data /etc/ssl/certs/caixaseguradora.pfx
```

**Passo 4: Configurar appsettings.Production.json**
```json
{
  "Certificate": {
    "Path": "/etc/ssl/certs/caixaseguradora.pfx",
    "Password": "SUA_SENHA_FORTE_AQUI",
    "Type": "PFX"
  },
  "Kestrel": {
    "Endpoints": {
      "Http": {
        "Url": "http://0.0.0.0:5000"
      },
      "Https": {
        "Url": "https://0.0.0.0:5001"
      }
    }
  },
  "Hsts": {
    "MaxAge": 31536000,
    "IncludeSubDomains": true,
    "Preload": true
  }
}
```

**Passo 5: Configurar Renovação Automática**
```bash
# Testar renovação
sudo certbot renew --dry-run

# Let's Encrypt cria automaticamente um cron job em:
# /etc/cron.d/certbot

# Ou criar script de renovação personalizado
sudo nano /etc/cron.weekly/renew-certs.sh
```

**Script de Renovação (renew-certs.sh)**:
```bash
#!/bin/bash
set -e

DOMAIN="api.caixaseguradora.com.br"
CERT_DIR="/etc/letsencrypt/live/$DOMAIN"
PFX_PATH="/etc/ssl/certs/caixaseguradora.pfx"
PFX_PASSWORD="SUA_SENHA_FORTE_AQUI"

# Renovar certificado
certbot renew --quiet

# Verificar se certificado foi renovado (modificado nas últimas 24h)
if [ $(find "$CERT_DIR/cert.pem" -mtime -1 2>/dev/null | wc -l) -gt 0 ]; then
    echo "Certificado renovado, convertendo para PFX..."

    # Reconverter para PFX
    openssl pkcs12 -export \
        -out "$PFX_PATH" \
        -inkey "$CERT_DIR/privkey.pem" \
        -in "$CERT_DIR/cert.pem" \
        -certfile "$CERT_DIR/chain.pem" \
        -password pass:$PFX_PASSWORD

    chmod 600 "$PFX_PATH"
    chown www-data:www-data "$PFX_PATH"

    # Reiniciar serviço da API
    systemctl restart caixaseguradora-api

    echo "Certificado atualizado e serviço reiniciado."
fi
```

```bash
# Tornar script executável
sudo chmod +x /etc/cron.weekly/renew-certs.sh
```

---

#### 3. Produção com Certificado Corporativo/Comercial

**Passo 1: Obter Certificado da CA Corporativa**

Solicite um certificado da autoridade certificadora (CA) da sua organização ou de uma CA comercial (DigiCert, GlobalSign, etc.).

**Você receberá**:
- Certificado do servidor (.crt ou .cer)
- Chave privada (.key) - **NUNCA compartilhe ou commite no Git**
- Certificado intermediário/cadeia (.crt)

**Passo 2: Converter para PFX**
```bash
# Combinar certificado + chave + cadeia em PFX
openssl pkcs12 -export \
  -out caixaseguradora.pfx \
  -inkey server.key \
  -in server.crt \
  -certfile intermediate.crt \
  -password pass:SUA_SENHA_FORTE_AQUI

# Mover para diretório seguro
sudo mv caixaseguradora.pfx /etc/ssl/private/
sudo chmod 600 /etc/ssl/private/caixaseguradora.pfx
sudo chown www-data:www-data /etc/ssl/private/caixaseguradora.pfx
```

**Passo 3: Usar Secrets Manager (Recomendado para Produção)**

Em vez de armazenar senha no appsettings.json, use gerenciadores de secrets:

**Azure Key Vault**:
```csharp
// Program.cs
var keyVaultName = builder.Configuration["KeyVault:Name"];
if (!string.IsNullOrEmpty(keyVaultName))
{
    var keyVaultUri = new Uri($"https://{keyVaultName}.vault.azure.net/");
    builder.Configuration.AddAzureKeyVault(keyVaultUri, new DefaultAzureCredential());
    Log.Information("Azure Key Vault configurado: {KeyVaultName}", keyVaultName);
}

// Recuperar senha do certificado do Key Vault
var certPassword = builder.Configuration["Certificate-Password"]; // Stored in Key Vault
```

**AWS Secrets Manager**:
```bash
# Armazenar senha no Secrets Manager
aws secretsmanager create-secret \
  --name caixaseguradora/cert-password \
  --secret-string "SUA_SENHA_FORTE_AQUI"

# Recuperar em runtime via SDK
```

**HashiCorp Vault**:
```bash
# Armazenar senha
vault kv put secret/caixaseguradora/cert password="SUA_SENHA_FORTE_AQUI"

# Recuperar via API
```

---

### Docker/Kubernetes HTTPS Configuration

#### Docker Compose com HTTPS

**docker-compose.yml**:
```yaml
version: '3.8'

services:
  backend:
    build: ./backend
    ports:
      - "5000:5000"  # HTTP
      - "5001:5001"  # HTTPS
    environment:
      - ASPNETCORE_ENVIRONMENT=Production
      - ASPNETCORE_URLS=http://+:5000;https://+:5001
      - Certificate__Path=/app/certs/caixaseguradora.pfx
      - Certificate__Password=${CERT_PASSWORD}  # De .env
    volumes:
      - ./certs:/app/certs:ro  # Certificados em volume read-only
      - ./data:/app/data
      - ./logs:/app/logs
    restart: unless-stopped

  nginx:
    image: nginx:alpine
    ports:
      - "443:443"
    volumes:
      - ./nginx/nginx.conf:/etc/nginx/nginx.conf:ro
      - ./certs:/etc/nginx/certs:ro
    depends_on:
      - backend
    restart: unless-stopped
```

**.env** (não commitar no Git):
```bash
CERT_PASSWORD=SUA_SENHA_FORTE_AQUI
```

**Estrutura de diretórios**:
```
project/
├── certs/               # ADICIONAR AO .gitignore
│   ├── caixaseguradora.pfx
│   ├── cert.pem         # Para NGINX
│   └── privkey.pem      # Para NGINX
├── docker-compose.yml
└── .env                 # ADICIONAR AO .gitignore
```

**.gitignore**:
```
certs/
.env
*.pfx
*.key
*.pem
```

---

#### Kubernetes com TLS Secrets

**Passo 1: Criar TLS Secret**
```bash
# A partir de arquivos PEM
kubectl create secret tls caixaseguradora-tls \
  --cert=cert.pem \
  --key=privkey.pem \
  --namespace=caixaseguradora

# Ou a partir de PFX (converter primeiro)
openssl pkcs12 -in caixaseguradora.pfx -out cert.pem -clcerts -nokeys -password pass:SENHA
openssl pkcs12 -in caixaseguradora.pfx -out privkey.pem -nocerts -nodes -password pass:SENHA

kubectl create secret tls caixaseguradora-tls \
  --cert=cert.pem \
  --key=privkey.pem \
  --namespace=caixaseguradora

# Limpar arquivos temporários
rm cert.pem privkey.pem
```

**Passo 2: Deployment com Secret**
```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: api-backend
  namespace: caixaseguradora
spec:
  replicas: 3
  selector:
    matchLabels:
      app: api-backend
  template:
    metadata:
      labels:
        app: api-backend
    spec:
      containers:
      - name: api
        image: caixaseguradora/api:1.0.0
        ports:
        - containerPort: 5000
          name: http
        - containerPort: 5001
          name: https
        env:
        - name: ASPNETCORE_ENVIRONMENT
          value: "Production"
        - name: Certificate__Path
          value: "/app/certs/tls.pfx"
        - name: Certificate__Password
          valueFrom:
            secretKeyRef:
              name: cert-password-secret
              key: password
        volumeMounts:
        - name: tls-certs
          mountPath: /app/certs
          readOnly: true
      volumes:
      - name: tls-certs
        secret:
          secretName: caixaseguradora-tls
```

**Passo 3: Ingress com TLS**
```yaml
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: caixaseguradora-ingress
  namespace: caixaseguradora
  annotations:
    cert-manager.io/cluster-issuer: "letsencrypt-prod"
    nginx.ingress.kubernetes.io/ssl-redirect: "true"
    nginx.ingress.kubernetes.io/force-ssl-redirect: "true"
spec:
  ingressClassName: nginx
  tls:
  - hosts:
    - api.caixaseguradora.com.br
    secretName: caixaseguradora-tls
  rules:
  - host: api.caixaseguradora.com.br
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: api-backend-service
            port:
              number: 5001
```

**Passo 4: Cert-Manager para Let's Encrypt (opcional)**
```bash
# Instalar cert-manager
kubectl apply -f https://github.com/cert-manager/cert-manager/releases/download/v1.13.0/cert-manager.yaml

# Criar ClusterIssuer para Let's Encrypt
kubectl apply -f - <<EOF
apiVersion: cert-manager.io/v1
kind: ClusterIssuer
metadata:
  name: letsencrypt-prod
spec:
  acme:
    server: https://acme-v02.api.letsencrypt.org/directory
    email: admin@caixaseguradora.com.br
    privateKeySecretRef:
      name: letsencrypt-prod
    solvers:
    - http01:
        ingress:
          class: nginx
EOF
```

---

### Monitoramento de Certificados

#### 1. Verificar Validade do Certificado

```bash
# Verificar certificado PFX
openssl pkcs12 -in caixaseguradora.pfx -nokeys -info

# Verificar data de expiração
openssl pkcs12 -in caixaseguradora.pfx -nokeys -passin pass:SENHA | \
  openssl x509 -noout -dates

# Verificar certificado remoto
openssl s_client -connect api.caixaseguradora.com.br:443 -servername api.caixaseguradora.com.br < /dev/null | \
  openssl x509 -noout -dates
```

#### 2. Script de Monitoramento

**check-cert-expiry.sh**:
```bash
#!/bin/bash
DOMAIN="api.caixaseguradora.com.br"
PORT=5001
WARN_DAYS=30

# Obter data de expiração
EXPIRY_DATE=$(echo | openssl s_client -servername $DOMAIN -connect $DOMAIN:$PORT 2>/dev/null | \
  openssl x509 -noout -enddate | cut -d= -f2)

EXPIRY_EPOCH=$(date -d "$EXPIRY_DATE" +%s)
NOW_EPOCH=$(date +%s)
DAYS_LEFT=$(( ($EXPIRY_EPOCH - $NOW_EPOCH) / 86400 ))

echo "Certificado expira em: $EXPIRY_DATE"
echo "Dias restantes: $DAYS_LEFT"

if [ $DAYS_LEFT -lt $WARN_DAYS ]; then
    echo "AVISO: Certificado expira em menos de $WARN_DAYS dias!"
    # Enviar alerta (e-mail, Slack, etc.)
    exit 1
fi
```

#### 3. Nagios/Prometheus Check

**Prometheus com blackbox_exporter**:
```yaml
# prometheus.yml
scrape_configs:
  - job_name: 'ssl_check'
    metrics_path: /probe
    params:
      module: [http_2xx]
    static_configs:
      - targets:
        - https://api.caixaseguradora.com.br
    relabel_configs:
      - source_labels: [__address__]
        target_label: __param_target
      - source_labels: [__param_target]
        target_label: instance
      - target_label: __address__
        replacement: blackbox-exporter:9115
```

---

### Security Checklist

**Checklist de Segurança SSL/TLS (NFR-002)**:

- [ ] **TLS 1.2+ apenas**: TLS 1.0 e 1.1 desabilitados
- [ ] **Cipher suites fortes**: Sem RC4, MD5, DES, 3DES
- [ ] **Certificado válido**: De CA confiável (não auto-assinado em produção)
- [ ] **Correspondência de domínio**: CN ou SAN corresponde ao domínio
- [ ] **Certificado não expirado**: Válido por pelo menos 30 dias
- [ ] **Chave privada segura**: Permissões 600, proprietário correto
- [ ] **Senha protegida**: Não em código, usar secrets manager
- [ ] **HSTS habilitado**: Força HTTPS em produção
- [ ] **HTTPS redirection**: HTTP redireciona para HTTPS
- [ ] **OCSP stapling**: Ativado para melhor performance
- [ ] **Certificate pinning**: Considerar para aplicativos mobile
- [ ] **Renovação automática**: Configurada (Let's Encrypt)
- [ ] **Monitoramento de expiração**: Alertas 30 dias antes
- [ ] **Backup de certificado**: Armazenado seguramente
- [ ] **Documentação**: Processo de renovação documentado

**Testar Configuração SSL**:
```bash
# SSL Labs (recomendado)
# https://www.ssllabs.com/ssltest/analyze.html?d=api.caixaseguradora.com.br

# testssl.sh (ferramenta local)
git clone https://github.com/drwetter/testssl.sh.git
cd testssl.sh
./testssl.sh https://api.caixaseguradora.com.br:5001

# nmap
nmap --script ssl-enum-ciphers -p 5001 api.caixaseguradora.com.br
```

---

### Troubleshooting

#### Erro: "Certificate file not found"
```bash
# Verificar caminho do certificado
ls -la /etc/ssl/certs/caixaseguradora.pfx

# Verificar permissões
sudo chmod 600 /etc/ssl/certs/caixaseguradora.pfx
sudo chown www-data:www-data /etc/ssl/certs/caixaseguradora.pfx
```

#### Erro: "Password incorrect"
```bash
# Testar senha do certificado
openssl pkcs12 -in caixaseguradora.pfx -nokeys -passin pass:SUA_SENHA
```

#### Erro: "Certificate chain incomplete"
```bash
# Verificar cadeia de certificados
openssl pkcs12 -in caixaseguradora.pfx -nokeys -chain -passin pass:SENHA
```

#### Navegador mostra "Not Secure"
- Certificado expirado
- Nome do domínio não corresponde
- Cadeia de certificados incompleta
- Certificado auto-assinado (apenas desenvolvimento)

---

### Secrets Management

**Não commitar secrets no Git!**

Use Azure Key Vault, AWS Secrets Manager, ou HashiCorp Vault:

```csharp
// Program.cs
builder.Configuration.AddAzureKeyVault(
    new Uri($"https://{keyVaultName}.vault.azure.net/"),
    new DefaultAzureCredential());
```

---

## Monitoramento

### Health Checks

Endpoint: `GET /health`

Retorna:
```json
{
  "status": "Healthy",
  "totalDuration": "00:00:00.0123456",
  "entries": {
    "database": { "status": "Healthy" },
    "hangfire": { "status": "Healthy" }
  }
}
```

### Logs

Logs são salvos em `/app/logs/` (Docker) ou `/var/log/caixaseguradora/` (manual).

Formato: JSON estruturado via Serilog.

---

## Backup e Recuperação

### Banco de Dados (SQLite)

```bash
# Backup
cp /app/data/premium_reporting.db /backups/premium_reporting_$(date +%Y%m%d).db

# Restauração
cp /backups/premium_reporting_20251023.db /app/data/premium_reporting.db
```

### Banco de Dados (PostgreSQL)

```bash
# Backup
pg_dump -h localhost -U app_user PremiumReporting > backup.sql

# Restauração
psql -h localhost -U app_user PremiumReporting < backup.sql
```

---

## Troubleshooting

### Backend não inicia

```bash
# Verificar logs
sudo journalctl -u caixaseguradora-api -n 100

# Testar manualmente
cd /var/www/caixaseguradora-api
dotnet CaixaSeguradora.Api.dll
```

### Frontend não carrega

```bash
# Verificar logs do NGINX
sudo tail -f /var/log/nginx/error.log

# Testar configuração
sudo nginx -t
```

### Banco de dados não conecta

```bash
# Verificar connection string
# Verificar firewall
# Testar conectividade
telnet db-server 5432
```

---

## Rollback

### Docker Compose

```bash
# Voltar para versão anterior
docker-compose down
docker-compose pull caixaseguradora/api:1.0.0-previous
docker-compose up -d
```

### Kubernetes

```bash
# Listar revisões
kubectl rollout history deployment/api-backend

# Voltar para revisão anterior
kubectl rollout undo deployment/api-backend
```

---

## Checklist de Implantação

- [ ] Variáveis de ambiente configuradas
- [ ] Secrets configurados (não em código)
- [ ] Banco de dados criado e migrations aplicadas
- [ ] SSL/HTTPS configurado
- [ ] Health checks funcionando
- [ ] Logs sendo coletados
- [ ] Backup configurado
- [ ] Monitoramento ativo
- [ ] Documentação de API acessível
- [ ] Testes de carga realizados
- [ ] Plano de rollback testado

---

**Última Atualização**: Outubro 2025
**Versão do Documento**: 1.0
**Contato**: [Informações de suporte]
