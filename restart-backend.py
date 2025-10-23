#!/usr/bin/env python3
"""
Script para reiniciar o backend automaticamente com as correções aplicadas.
"""

import os
import signal
import subprocess
import time
import sys

def kill_process_on_port(port):
    """Encerra processos rodando na porta especificada."""
    try:
        result = subprocess.run(
            ['lsof', '-ti', f':{port}'],
            capture_output=True,
            text=True
        )
        pids = result.stdout.strip().split('\n')
        for pid in pids:
            if pid:
                print(f"Encerrando processo PID {pid} na porta {port}...")
                os.kill(int(pid), signal.SIGKILL)
                time.sleep(0.5)
        print(f"✅ Porta {port} liberada")
    except Exception as e:
        print(f"⚠️  Erro ao liberar porta {port}: {e}")

def build_backend(project_root):
    """Compila o backend."""
    print("\n📦 Compilando backend...")
    backend_path = os.path.join(project_root, "backend")

    result = subprocess.run(
        ['dotnet', 'build', '--configuration', 'Release'],
        cwd=backend_path,
        capture_output=True,
        text=True
    )

    if result.returncode == 0:
        print("✅ Build bem-sucedido")
        return True
    else:
        print(f"❌ Erro no build:\n{result.stderr}")
        return False

def start_backend(project_root):
    """Inicia o backend."""
    print("\n🚀 Iniciando backend...")
    api_path = os.path.join(project_root, "backend", "src", "CaixaSeguradora.Api")
    log_file = "/tmp/backend-restart.log"

    with open(log_file, 'w') as log:
        process = subprocess.Popen(
            ['dotnet', 'run', '--urls', 'http://localhost:5000'],
            cwd=api_path,
            stdout=log,
            stderr=subprocess.STDOUT
        )

    print(f"Backend iniciado (PID: {process.pid})")
    print(f"Log: tail -f {log_file}")

    # Aguardar inicialização
    print("\n⏳ Aguardando 10 segundos para o backend inicializar...")
    for i in range(10, 0, -1):
        print(f"   {i}...", end='\r')
        time.sleep(1)
    print("   ✅ Pronto!")

    return process.pid

def test_endpoints():
    """Testa os endpoints da dashboard."""
    import json

    print("\n🧪 Testando endpoints...")

    tests = [
        ("Health Check", "http://localhost:5000/api/Dashboard/health"),
        ("Metrics", "http://localhost:5000/api/Dashboard/metrics"),
        ("Function Points", "http://localhost:5000/api/Dashboard/function-points"),
    ]

    for name, url in tests:
        try:
            result = subprocess.run(
                ['curl', '-s', '-m', '5', url],
                capture_output=True,
                text=True,
                timeout=6
            )

            if result.returncode == 0 and result.stdout:
                try:
                    data = json.loads(result.stdout)
                    print(f"✅ {name}: OK")
                except json.JSONDecodeError:
                    print(f"⚠️  {name}: Resposta não é JSON válido")
            else:
                print(f"❌ {name}: Sem resposta")
        except Exception as e:
            print(f"❌ {name}: {e}")

def main():
    project_root = "/Users/brunosouza/Development/Caixa Seguradora/POC Cobol"

    print("=" * 50)
    print("REINÍCIO AUTOMÁTICO DO BACKEND")
    print("=" * 50)

    # Passo 1: Liberar portas
    print("\n1️⃣  Liberando portas 5000 e 5001...")
    kill_process_on_port(5000)
    kill_process_on_port(5001)

    # Passo 2: Compilar
    print("\n2️⃣  Compilando backend...")
    if not build_backend(project_root):
        print("\n❌ Falha na compilação. Verifique os erros acima.")
        sys.exit(1)

    # Passo 3: Iniciar
    print("\n3️⃣  Iniciando backend...")
    backend_pid = start_backend(project_root)

    # Passo 4: Testar
    print("\n4️⃣  Testando endpoints...")
    test_endpoints()

    print("\n" + "=" * 50)
    print("CONCLUSÃO")
    print("=" * 50)
    print(f"✅ Backend rodando (PID: {backend_pid})")
    print(f"📊 Dashboard: http://localhost:5173/reports")
    print(f"📖 Swagger: http://localhost:5000/swagger")
    print(f"📝 Log: tail -f /tmp/backend-restart.log")
    print("\n💡 Recarregue o navegador com Ctrl+Shift+R")

if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        print("\n\n⚠️  Interrompido pelo usuário")
        sys.exit(0)
    except Exception as e:
        print(f"\n\n❌ Erro: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)
