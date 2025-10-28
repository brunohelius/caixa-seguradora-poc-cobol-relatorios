import { Calculator, Scale, CheckCircle, AlertTriangle, TrendingUp, TrendingDown, DollarSign, FileText, GitBranch, Settings, Box, Zap, Database } from 'lucide-react';

export const BusinessLogicTab = () => {
  return (
    <div className="space-y-6">
      {/* Header */}
      <div className="bg-gradient-to-r from-[#0047BB] to-blue-700 text-white rounded-lg shadow-xl p-8">
        <div className="flex items-center gap-4 mb-4">
          <div className="w-16 h-16 bg-[#FFB81C] rounded-lg flex items-center justify-center">
            <Calculator className="w-8 h-8 text-[#0047BB]" />
          </div>
          <div>
            <h2 className="text-3xl font-bold">Lógica de Negócio</h2>
            <p className="text-blue-100 text-lg">147+ Regras de Negócio | 63 Seções COBOL | 38 Fórmulas de Cálculo</p>
          </div>
        </div>
      </div>

      {/* Métricas Principais */}
      <div className="grid grid-cols-2 md:grid-cols-4 gap-6">
        <div className="bg-gradient-to-br from-blue-500 to-blue-600 rounded-lg p-6 text-white shadow-lg transform hover:scale-105 transition-transform">
          <Database className="w-8 h-8 mb-3 opacity-90" />
          <div className="text-4xl font-bold mb-2">63</div>
          <div className="text-sm opacity-90">Seções COBOL</div>
          <div className="text-xs opacity-75 mt-1">R0000-R9999</div>
        </div>

        <div className="bg-gradient-to-br from-purple-500 to-purple-600 rounded-lg p-6 text-white shadow-lg transform hover:scale-105 transition-transform">
          <CheckCircle className="w-8 h-8 mb-3 opacity-90" />
          <div className="text-4xl font-bold mb-2">147+</div>
          <div className="text-sm opacity-90">Regras de Negócio</div>
          <div className="text-xs opacity-75 mt-1">Validações críticas</div>
        </div>

        <div className="bg-gradient-to-br from-green-500 to-green-600 rounded-lg p-6 text-white shadow-lg transform hover:scale-105 transition-transform">
          <GitBranch className="w-8 h-8 mb-3 opacity-90" />
          <div className="text-4xl font-bold mb-2">6</div>
          <div className="text-sm opacity-90">Tipos de Movimento</div>
          <div className="text-xs opacity-75 mt-1">Códigos 101-106</div>
        </div>

        <div className="bg-gradient-to-br from-orange-500 to-orange-600 rounded-lg p-6 text-white shadow-lg transform hover:scale-105 transition-transform">
          <Calculator className="w-8 h-8 mb-3 opacity-90" />
          <div className="text-4xl font-bold mb-2">38</div>
          <div className="text-sm opacity-90">Fórmulas de Cálculo</div>
          <div className="text-xs opacity-75 mt-1">Cálculos financeiros</div>
        </div>
      </div>

      {/* Métricas Adicionais */}
      <div className="grid grid-cols-2 md:grid-cols-3 gap-4">
        <div className="bg-white rounded-lg shadow p-4 border-l-4 border-blue-500">
          <div className="flex items-center gap-3">
            <FileText className="w-6 h-6 text-blue-500" />
            <div>
              <div className="text-2xl font-bold text-gray-900">5.046</div>
              <div className="text-sm text-gray-600">Linhas de Código</div>
            </div>
          </div>
        </div>

        <div className="bg-white rounded-lg shadow p-4 border-l-4 border-green-500">
          <div className="flex items-center gap-3">
            <Scale className="w-6 h-6 text-green-500" />
            <div>
              <div className="text-2xl font-bold text-gray-900">20+</div>
              <div className="text-sm text-gray-600">Ramos SUSEP</div>
            </div>
          </div>
        </div>

        <div className="bg-white rounded-lg shadow p-4 border-l-4 border-purple-500">
          <div className="flex items-center gap-3">
            <AlertTriangle className="w-6 h-6 text-purple-500" />
            <div>
              <div className="text-2xl font-bold text-gray-900">52</div>
              <div className="text-sm text-gray-600">Validações</div>
            </div>
          </div>
        </div>
      </div>

      {/* Fluxo de Processamento Principal */}
      <div className="bg-white rounded-lg shadow-lg p-6">
        <h3 className="text-2xl font-bold text-gray-900 mb-6 flex items-center gap-3">
          <Zap className="w-7 h-7 text-[#FFB81C]" />
          Fluxo de Processamento Principal
        </h3>

        <div className="bg-gradient-to-br from-gray-50 to-blue-50 rounded-lg p-6">
          <div className="space-y-4">
            {[
              { section: 'R0000-INICIO', desc: 'Ponto de entrada do programa', color: 'blue' },
              { section: 'R0100-INICIALIZACAO', desc: 'Inicializar variáveis de trabalho', color: 'blue' },
              { section: 'R0200-ABRIR-ARQUIVOS', desc: 'Abrir PREMIT.TXT e PREMCED.TXT', color: 'blue' },
              { section: 'R0300-LER-PARAMETROS', desc: 'Ler parâmetros de execução', color: 'blue' },
              { section: 'R0400-ABRIR-CURSORES', desc: 'Abrir cursores de banco de dados', color: 'blue' },
              { section: 'R0500-PROCESSAR-LOTE', desc: 'Loop principal de processamento', color: 'green', highlight: true },
              { section: 'R0600-PROCESSAR-PREMIO', desc: 'Processar cada prêmio', color: 'green', indent: 1 },
              { section: 'R0700-BUSCAR-APOLICE', desc: 'Buscar dados da apólice', color: 'purple', indent: 2 },
              { section: 'R0800-BUSCAR-PRODUTO', desc: 'Buscar dados do produto', color: 'purple', indent: 2 },
              { section: 'R0900-BUSCAR-CLIENTES', desc: 'Buscar dados dos clientes', color: 'purple', indent: 2 },
              { section: 'R1000-CALCULAR-PREMIO', desc: 'Calcular prêmio conforme tipo', color: 'orange', indent: 2 },
              { section: 'R3000-PROCESSAR-COSSEGURO', desc: 'Processar cosseguro (se aplicável)', color: 'yellow', indent: 2 },
              { section: 'R4000-FORMATAR-PREMIT', desc: 'Formatar registro PREMIT', color: 'green', indent: 2 },
              { section: 'R5000-ESCREVER-REGISTRO', desc: 'Escrever registro no arquivo', color: 'green', indent: 2 },
              { section: 'R8000-FECHAR-CURSORES', desc: 'Fechar cursores', color: 'blue' },
              { section: 'R8100-FECHAR-ARQUIVOS', desc: 'Fechar arquivos de saída', color: 'blue' },
              { section: 'R8200-GERAR-TOTALIZADORES', desc: 'Gerar totalizadores e estatísticas', color: 'blue' },
              { section: 'R9999-FIM', desc: 'Finalizar programa', color: 'gray' },
            ].map((step, idx) => (
              <div
                key={idx}
                className={`flex items-start gap-4 ${step.indent ? `ml-${step.indent * 8}` : ''} ${step.highlight ? 'bg-yellow-50 border border-yellow-300 rounded-lg p-3' : ''}`}
                style={step.indent ? { marginLeft: `${step.indent * 2}rem` } : {}}
              >
                <div className={`flex-shrink-0 w-8 h-8 rounded-full flex items-center justify-center font-bold text-sm
                  ${step.color === 'blue' ? 'bg-blue-500 text-white' : ''}
                  ${step.color === 'green' ? 'bg-green-500 text-white' : ''}
                  ${step.color === 'purple' ? 'bg-purple-500 text-white' : ''}
                  ${step.color === 'orange' ? 'bg-orange-500 text-white' : ''}
                  ${step.color === 'yellow' ? 'bg-yellow-500 text-white' : ''}
                  ${step.color === 'gray' ? 'bg-gray-500 text-white' : ''}
                `}>
                  {step.section.match(/\d+/)?.[0].substring(0, 2) || '→'}
                </div>
                <div className="flex-1">
                  <div className="font-mono font-bold text-gray-900">{step.section}</div>
                  <div className="text-sm text-gray-600">{step.desc}</div>
                </div>
              </div>
            ))}
          </div>
        </div>
      </div>

      {/* Tipos de Movimento */}
      <div className="bg-white rounded-lg shadow-lg p-6">
        <h3 className="text-2xl font-bold text-gray-900 mb-6 flex items-center gap-3">
          <GitBranch className="w-7 h-7 text-[#FFB81C]" />
          Tipos de Movimento (COD_TIPO_MOVIMENTO)
        </h3>

        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
          <div className="bg-gradient-to-br from-green-50 to-green-100 border-2 border-green-300 rounded-lg p-6 shadow-md hover:shadow-xl transition-shadow">
            <div className="flex items-center justify-between mb-4">
              <div className="text-4xl font-bold text-green-700">101</div>
              <TrendingUp className="w-8 h-8 text-green-600" />
            </div>
            <h4 className="text-xl font-bold text-green-900 mb-2">Emissão</h4>
            <p className="text-green-700 font-semibold mb-3">+Prêmio Integral</p>
            <div className="text-sm text-gray-700 space-y-1">
              <p>• Seção: <span className="font-mono font-bold">R1100</span></p>
              <p>• Prêmio integral da apólice nova</p>
              <p>• Adicional de fracionamento 5.38%</p>
            </div>
          </div>

          <div className="bg-gradient-to-br from-blue-50 to-blue-100 border-2 border-blue-300 rounded-lg p-6 shadow-md hover:shadow-xl transition-shadow">
            <div className="flex items-center justify-between mb-4">
              <div className="text-4xl font-bold text-blue-700">102</div>
              <TrendingUp className="w-8 h-8 text-blue-600" />
            </div>
            <h4 className="text-xl font-bold text-blue-900 mb-2">Endosso Aumento</h4>
            <p className="text-blue-700 font-semibold mb-3">+Prêmio Adicional</p>
            <div className="text-sm text-gray-700 space-y-1">
              <p>• Seção: <span className="font-mono font-bold">R1200</span></p>
              <p>• Prêmio proporcional (pro-rata die)</p>
              <p>• Calculado por dias remanescentes</p>
            </div>
          </div>

          <div className="bg-gradient-to-br from-yellow-50 to-yellow-100 border-2 border-yellow-300 rounded-lg p-6 shadow-md hover:shadow-xl transition-shadow">
            <div className="flex items-center justify-between mb-4">
              <div className="text-4xl font-bold text-yellow-700">103</div>
              <TrendingDown className="w-8 h-8 text-yellow-600" />
            </div>
            <h4 className="text-xl font-bold text-yellow-900 mb-2">Endosso Redução</h4>
            <p className="text-yellow-700 font-semibold mb-3">-Prêmio Devolvido</p>
            <div className="text-sm text-gray-700 space-y-1">
              <p>• Seção: <span className="font-mono font-bold">R1300</span></p>
              <p>• Devolução proporcional</p>
              <p>• Valores negativos</p>
            </div>
          </div>

          <div className="bg-gradient-to-br from-red-50 to-red-100 border-2 border-red-300 rounded-lg p-6 shadow-md hover:shadow-xl transition-shadow">
            <div className="flex items-center justify-between mb-4">
              <div className="text-4xl font-bold text-red-700">104</div>
              <TrendingDown className="w-8 h-8 text-red-600" />
            </div>
            <h4 className="text-xl font-bold text-red-900 mb-2">Cancelamento</h4>
            <p className="text-red-700 font-semibold mb-3">-Prêmio Integral</p>
            <div className="text-sm text-gray-700 space-y-1">
              <p>• Seção: <span className="font-mono font-bold">R1400</span></p>
              <p>• &lt; 7 dias: Devolução integral</p>
              <p>• ≥ 7 dias: Desconto taxa 10%</p>
            </div>
          </div>

          <div className="bg-gradient-to-br from-purple-50 to-purple-100 border-2 border-purple-300 rounded-lg p-6 shadow-md hover:shadow-xl transition-shadow">
            <div className="flex items-center justify-between mb-4">
              <div className="text-4xl font-bold text-purple-700">105</div>
              <TrendingUp className="w-8 h-8 text-purple-600" />
            </div>
            <h4 className="text-xl font-bold text-purple-900 mb-2">Renovação</h4>
            <p className="text-purple-700 font-semibold mb-3">+Prêmio Nova Vigência</p>
            <div className="text-sm text-gray-700 space-y-1">
              <p>• Seção: <span className="font-mono font-bold">R1500</span></p>
              <p>• Nova vigência completa</p>
              <p>• Mesmo produto e cobertura</p>
            </div>
          </div>

          <div className="bg-gradient-to-br from-orange-50 to-orange-100 border-2 border-orange-300 rounded-lg p-6 shadow-md hover:shadow-xl transition-shadow">
            <div className="flex items-center justify-between mb-4">
              <div className="text-4xl font-bold text-orange-700">106</div>
              <Scale className="w-8 h-8 text-orange-600" />
            </div>
            <h4 className="text-xl font-bold text-orange-900 mb-2">Substituição</h4>
            <p className="text-orange-700 font-semibold mb-3">±Diferença Prêmio</p>
            <div className="text-sm text-gray-700 space-y-1">
              <p>• Seção: <span className="font-mono font-bold">R1600</span></p>
              <p>• Diferença entre produtos</p>
              <p>• Pode ser positivo ou negativo</p>
            </div>
          </div>
        </div>
      </div>

      {/* Cálculos de Prêmio - R1000 */}
      <div className="bg-white rounded-lg shadow-lg p-6">
        <h3 className="text-2xl font-bold text-gray-900 mb-6 flex items-center gap-3">
          <Calculator className="w-7 h-7 text-[#FFB81C]" />
          R1000-CALCULAR-PREMIO (Seção Mestre)
        </h3>

        <div className="bg-gray-50 border border-gray-200 rounded-lg p-6 mb-6">
          <h4 className="font-bold text-gray-900 mb-4 text-lg">Lógica de Despacho por Tipo de Movimento</h4>
          <div className="bg-gray-900 text-gray-100 p-5 rounded-lg font-mono text-sm overflow-x-auto">
            <pre>{`R1000-CALCULAR-PREMIO.
    EVALUATE COD-TIPO-MOVIMENTO
        WHEN 101  PERFORM R1100-PROCESSAR-EMISSAO
        WHEN 102  PERFORM R1200-PROCESSAR-ENDOSSO-AUMENTO
        WHEN 103  PERFORM R1300-PROCESSAR-ENDOSSO-REDUCAO
        WHEN 104  PERFORM R1400-PROCESSAR-CANCELAMENTO
        WHEN 105  PERFORM R1500-PROCESSAR-RENOVACAO
        WHEN 106  PERFORM R1600-PROCESSAR-SUBSTITUICAO
        WHEN OTHER
            MOVE 'TIPO DE MOVIMENTO INVALIDO' TO WS-MENSAGEM-ERRO
            PERFORM R9000-TRATAR-ERRO-VALIDACAO
    END-EVALUATE.

    *> Aplicar conversão de moeda se necessário
    IF COD-MOEDA NOT = 'BRL'
        PERFORM R1700-CONVERTER-MOEDA
    END-IF.

    *> Validar limites
    PERFORM R1800-VALIDAR-LIMITES.`}</pre>
          </div>
        </div>

        <div className="bg-gradient-to-r from-blue-50 to-purple-50 border border-blue-200 rounded-lg p-6">
          <h4 className="font-bold text-gray-900 mb-4 text-lg flex items-center gap-2">
            <DollarSign className="w-6 h-6 text-green-600" />
            Migração para .NET
          </h4>
          <div className="bg-white p-5 rounded-lg border border-gray-200 font-mono text-sm overflow-x-auto">
            <pre className="text-gray-800">{`public async Task<PremiumCalculation> CalculatePremiumAsync(
    Premium premium,
    Policy policy,
    Product product)
{
    PremiumCalculation calculation = premium.MovementType switch
    {
        101 => CalculateEmission(premium, policy),
        102 => CalculateEndorsementIncrease(premium, policy, _processingDate),
        103 => CalculateEndorsementDecrease(premium, policy, _processingDate),
        104 => CalculateCancellation(premium, policy, _processingDate),
        105 => CalculateRenewal(premium, policy),
        106 => CalculateReplacement(premium, policy),
        _ => throw new BusinessRuleException(
            $"Tipo de movimento inválido: {premium.MovementType}")
    };

    // Conversão de moeda
    if (premium.CurrencyCode != "BRL")
    {
        calculation = await ConvertCurrencyAsync(calculation, premium.ExchangeRate);
    }

    // Validações de limites
    ValidateLimits(calculation, product);

    return calculation;
}`}</pre>
          </div>
        </div>
      </div>

      {/* Exemplo Detalhado: R1100 Emissão */}
      <div className="bg-white rounded-lg shadow-lg p-6">
        <h3 className="text-2xl font-bold text-gray-900 mb-6 flex items-center gap-3">
          <FileText className="w-7 h-7 text-green-600" />
          Exemplo Detalhado: R1100 - Processamento de Emissão
        </h3>

        <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
          <div>
            <h4 className="font-bold text-gray-900 mb-3 text-lg">COBOL Original</h4>
            <div className="bg-gray-900 text-gray-100 p-5 rounded-lg font-mono text-xs overflow-x-auto">
              <pre>{`R1100-PROCESSAR-EMISSAO.
    MOVE REGISTRO-PREMIO-LIQUIDO TO WS-PREMIO-CALCULADO.
    MOVE REGISTRO-PREMIO-TOTAL TO WS-PREMIO-EMITIDO.
    MOVE REGISTRO-IOF TO WS-IOF-CALCULADO.

    *> Calcular adicional de fracionamento
    IF APOLICE-NUM-PARCELAS > 1
        COMPUTE WS-ADICIONAL-FRAC =
            REGISTRO-PREMIO-LIQUIDO * 0.0538
        ADD WS-ADICIONAL-FRAC TO WS-PREMIO-EMITIDO
    END-IF.

    *> Validar vigência
    IF APOLICE-DATA-VIG-INI > WS-DATA-PROCESSAMENTO
        MOVE 'W' TO WS-STATUS-VALIDACAO
    END-IF.`}</pre>
            </div>
          </div>

          <div>
            <h4 className="font-bold text-gray-900 mb-3 text-lg">Migração .NET</h4>
            <div className="bg-white border border-gray-200 p-5 rounded-lg font-mono text-xs overflow-x-auto">
              <pre className="text-gray-800">{`public PremiumCalculation CalculateEmission(
    Premium premium,
    Policy policy)
{
    const decimal INSTALLMENT_FEE_RATE = 0.0538m;

    var calculation = new PremiumCalculation
    {
        NetPremium = premium.NetPremium,
        GrossPremium = premium.TotalPremium,
        IOF = premium.IOF
    };

    // Adicional de fracionamento (5.38%)
    if (policy.InstallmentCount > 1)
    {
        calculation.InstallmentFee =
            premium.NetPremium * INSTALLMENT_FEE_RATE;
        calculation.GrossPremium +=
            calculation.InstallmentFee;
    }

    // Validar vigência futura
    if (policy.EffectiveStartDate > _processingDate)
    {
        calculation.Warnings.Add("Vigência futura");
    }

    return calculation;
}`}</pre>
            </div>
          </div>
        </div>

        <div className="mt-6 bg-blue-50 border border-blue-200 rounded-lg p-4">
          <h5 className="font-bold text-blue-900 mb-2 flex items-center gap-2">
            <CheckCircle className="w-5 h-5" />
            Regras de Negócio Aplicadas
          </h5>
          <ul className="text-sm text-blue-900 space-y-1">
            <li>• Taxa de fracionamento fixa: <span className="font-bold">5.38%</span></li>
            <li>• Aplicada apenas quando número de parcelas &gt; 1</li>
            <li>• Warning gerado para vigências futuras</li>
            <li>• Preserva tipos decimais para compatibilidade COBOL</li>
          </ul>
        </div>
      </div>

      {/* Exemplo: R1200 Endosso Aumento */}
      <div className="bg-white rounded-lg shadow-lg p-6">
        <h3 className="text-2xl font-bold text-gray-900 mb-6 flex items-center gap-3">
          <TrendingUp className="w-7 h-7 text-blue-600" />
          R1200 - Endosso Aumento (Pro-Rata Die)
        </h3>

        <div className="bg-blue-50 border border-blue-200 rounded-lg p-6 mb-6">
          <h4 className="font-bold text-blue-900 mb-3 text-lg">Regra: Cálculo Proporcional ao Período Remanescente</h4>
          <div className="bg-gray-900 text-gray-100 p-5 rounded-lg font-mono text-sm overflow-x-auto">
            <pre>{`R1200-PROCESSAR-ENDOSSO-AUMENTO.
    *> Calcular dias remanescentes de vigência
    COMPUTE WS-DIAS-VIGENCIA =
        FUNCTION INTEGER-OF-DATE(APOLICE-DATA-VIG-FIM) -
        FUNCTION INTEGER-OF-DATE(WS-DATA-PROCESSAMENTO).

    *> Dias totais da apólice
    COMPUTE WS-DIAS-TOTAIS =
        FUNCTION INTEGER-OF-DATE(APOLICE-DATA-VIG-FIM) -
        FUNCTION INTEGER-OF-DATE(APOLICE-DATA-VIG-INI).

    *> Prêmio proporcional (pro-rata die)
    COMPUTE WS-PREMIO-CALCULADO =
        REGISTRO-PREMIO-LIQUIDO *
        (WS-DIAS-VIGENCIA / WS-DIAS-TOTAIS).

    *> IOF proporcional
    COMPUTE WS-IOF-CALCULADO =
        REGISTRO-IOF *
        (WS-DIAS-VIGENCIA / WS-DIAS-TOTAIS).`}</pre>
          </div>
        </div>

        <div className="bg-white border border-gray-200 rounded-lg p-6">
          <h4 className="font-bold text-gray-900 mb-3 text-lg">Migração .NET</h4>
          <div className="bg-gray-50 p-5 rounded-lg font-mono text-sm overflow-x-auto">
            <pre className="text-gray-800">{`public PremiumCalculation CalculateEndorsementIncrease(
    Premium premium,
    Policy policy,
    DateTime processingDate)
{
    // Calcular dias remanescentes
    var remainingDays = (policy.EffectiveEndDate - processingDate).Days;
    var totalDays = (policy.EffectiveEndDate - policy.EffectiveStartDate).Days;

    // Validar período
    if (remainingDays <= 0)
    {
        throw new BusinessRuleException(
            "Endosso não permitido: vigência já encerrada");
    }

    // Pro-rata die (proporcional aos dias)
    var proportionFactor = (decimal)remainingDays / totalDays;

    return new PremiumCalculation
    {
        NetPremium = premium.NetPremium * proportionFactor,
        IOF = premium.IOF * proportionFactor,
        GrossPremium = (premium.NetPremium + premium.IOF) * proportionFactor,
        ProportionFactor = proportionFactor
    };
}`}</pre>
          </div>
        </div>
      </div>

      {/* Exemplo: R1400 Cancelamento */}
      <div className="bg-white rounded-lg shadow-lg p-6">
        <h3 className="text-2xl font-bold text-gray-900 mb-6 flex items-center gap-3">
          <AlertTriangle className="w-7 h-7 text-red-600" />
          R1400 - Cancelamento (Direito de Arrependimento)
        </h3>

        <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
          <div className="bg-red-50 border-2 border-red-300 rounded-lg p-6">
            <h4 className="font-bold text-red-900 mb-3 flex items-center gap-2">
              <CheckCircle className="w-5 h-5" />
              Menos de 7 Dias
            </h4>
            <p className="text-red-800 mb-3">Devolução Integral (100%)</p>
            <div className="bg-white rounded p-3 text-sm text-gray-700">
              <p className="font-semibold mb-2">Direito de Arrependimento</p>
              <p>Cliente recebe de volta TODO o prêmio pago, incluindo IOF.</p>
            </div>
          </div>

          <div className="bg-yellow-50 border-2 border-yellow-300 rounded-lg p-6">
            <h4 className="font-bold text-yellow-900 mb-3 flex items-center gap-2">
              <AlertTriangle className="w-5 h-5" />
              7 Dias ou Mais
            </h4>
            <p className="text-yellow-800 mb-3">Devolução Parcial (90%)</p>
            <div className="bg-white rounded p-3 text-sm text-gray-700">
              <p className="font-semibold mb-2">Taxa Administrativa</p>
              <p>Desconto de 10% como taxa administrativa. IOF não é devolvido.</p>
            </div>
          </div>
        </div>

        <div className="mt-6 bg-gray-900 text-gray-100 p-5 rounded-lg font-mono text-sm overflow-x-auto">
          <pre>{`R1400-PROCESSAR-CANCELAMENTO.
    *> Verificar carência de 7 dias
    COMPUTE WS-DIAS-DESDE-EMISSAO =
        FUNCTION INTEGER-OF-DATE(WS-DATA-PROCESSAMENTO) -
        FUNCTION INTEGER-OF-DATE(APOLICE-DATA-EMISSAO).

    IF WS-DIAS-DESDE-EMISSAO < 7
        *> Devolução integral (direito de arrependimento)
        COMPUTE WS-PREMIO-CALCULADO =
            REGISTRO-PREMIO-TOTAL * -1
    ELSE
        *> Devolução proporcional (descontar taxa administrativa 10%)
        COMPUTE WS-PREMIO-CALCULADO =
            REGISTRO-PREMIO-TOTAL * -0.90
    END-IF.`}</pre>
        </div>

        <div className="mt-6 bg-white border border-gray-200 p-5 rounded-lg font-mono text-sm overflow-x-auto">
          <pre className="text-gray-800">{`public PremiumCalculation CalculateCancellation(
    Premium premium,
    Policy policy,
    DateTime processingDate)
{
    var daysSinceIssue = (processingDate - policy.IssueDate).Days;
    decimal refundAmount;

    if (daysSinceIssue < 7)
    {
        // Direito de arrependimento: devolução integral
        refundAmount = premium.TotalPremium;
        _logger.LogInformation(
            "Full refund applied (regret period): Policy {PolicyNumber}",
            policy.PolicyNumber);
    }
    else
    {
        // Devolução com desconto de taxa administrativa (10%)
        const decimal ADMIN_FEE_RATE = 0.10m;
        refundAmount = premium.TotalPremium * (1 - ADMIN_FEE_RATE);
        _logger.LogInformation(
            "Partial refund applied (admin fee {FeeRate}%): Policy {PolicyNumber}",
            ADMIN_FEE_RATE * 100, policy.PolicyNumber);
    }

    return new PremiumCalculation
    {
        NetPremium = -refundAmount,
        GrossPremium = -refundAmount,
        IOF = 0m, // IOF não é devolvido
        RefundReason = daysSinceIssue < 7 ? "Regret" : "Cancellation"
    };
}`}</pre>
        </div>
      </div>

      {/* Conversão de Moeda */}
      <div className="bg-white rounded-lg shadow-lg p-6">
        <h3 className="text-2xl font-bold text-gray-900 mb-6 flex items-center gap-3">
          <DollarSign className="w-7 h-7 text-green-600" />
          R1700-CONVERTER-MOEDA
        </h3>

        <div className="bg-gradient-to-r from-green-50 to-blue-50 border border-green-200 rounded-lg p-6 mb-6">
          <h4 className="font-bold text-gray-900 mb-3 text-lg">Regra: Conversão para BRL Usando Taxa de Câmbio</h4>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            <div>
              <h5 className="font-bold text-gray-800 mb-2">COBOL</h5>
              <div className="bg-gray-900 text-gray-100 p-4 rounded-lg font-mono text-xs overflow-x-auto">
                <pre>{`R1700-CONVERTER-MOEDA.
    IF WS-TAXA-CAMBIO = ZEROS
        MOVE 'TAXA DE CAMBIO NAO INFORMADA'
            TO WS-MENSAGEM-ERRO
        PERFORM R9000-TRATAR-ERRO-VALIDACAO
    END-IF.

    COMPUTE WS-PREMIO-CALCULADO =
        WS-PREMIO-CALCULADO * WS-TAXA-CAMBIO.

    COMPUTE WS-PREMIO-EMITIDO =
        WS-PREMIO-EMITIDO * WS-TAXA-CAMBIO.

    COMPUTE WS-IOF-CALCULADO =
        WS-IOF-CALCULADO * WS-TAXA-CAMBIO.`}</pre>
              </div>
            </div>

            <div>
              <h5 className="font-bold text-gray-800 mb-2">.NET</h5>
              <div className="bg-white border border-gray-200 p-4 rounded-lg font-mono text-xs overflow-x-auto">
                <pre className="text-gray-800">{`private async Task<PremiumCalculation> ConvertCurrencyAsync(
    PremiumCalculation calculation,
    decimal exchangeRate)
{
    if (exchangeRate <= 0)
    {
        throw new BusinessRuleException(
            "Taxa de câmbio inválida");
    }

    return new PremiumCalculation
    {
        NetPremium = calculation.NetPremium * exchangeRate,
        GrossPremium = calculation.GrossPremium * exchangeRate,
        IOF = calculation.IOF * exchangeRate,
        ExchangeRate = exchangeRate,
        OriginalCurrency = calculation.OriginalCurrency ?? "USD"
    };
}`}</pre>
              </div>
            </div>
          </div>
        </div>
      </div>

      {/* Validação de Limites */}
      <div className="bg-white rounded-lg shadow-lg p-6">
        <h3 className="text-2xl font-bold text-gray-900 mb-6 flex items-center gap-3">
          <Scale className="w-7 h-7 text-purple-600" />
          R1800-VALIDAR-LIMITES
        </h3>

        <div className="bg-purple-50 border border-purple-200 rounded-lg p-6 mb-6">
          <h4 className="font-bold text-purple-900 mb-3 text-lg">Regra: Validar Valores Contra Limites do Produto</h4>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            <div>
              <h5 className="font-bold text-gray-800 mb-2">COBOL</h5>
              <div className="bg-gray-900 text-gray-100 p-4 rounded-lg font-mono text-xs overflow-x-auto">
                <pre>{`R1800-VALIDAR-LIMITES.
    *> Limite mínimo de prêmio
    IF WS-PREMIO-CALCULADO < PRODUTO-PREMIO-MINIMO
        MOVE 'E' TO WS-STATUS-VALIDACAO
        MOVE 'PREMIO ABAIXO DO MINIMO' TO WS-MENSAGEM-ERRO
        PERFORM R9000-TRATAR-ERRO-VALIDACAO
    END-IF.

    *> Limite máximo de prêmio
    IF WS-PREMIO-CALCULADO > PRODUTO-PREMIO-MAXIMO
        MOVE 'W' TO WS-STATUS-VALIDACAO
        MOVE 'PREMIO ACIMA DO MAXIMO' TO WS-MENSAGEM-ERRO
        PERFORM R9100-TRATAR-WARNING
    END-IF.`}</pre>
              </div>
            </div>

            <div>
              <h5 className="font-bold text-gray-800 mb-2">.NET</h5>
              <div className="bg-white border border-gray-200 p-4 rounded-lg font-mono text-xs overflow-x-auto">
                <pre className="text-gray-800">{`private void ValidateLimits(
    PremiumCalculation calculation,
    Product product)
{
    // Limite mínimo (erro crítico)
    if (calculation.NetPremium < product.MinimumPremium)
    {
        throw new BusinessRuleException(
            $"Prêmio R$ {calculation.NetPremium:N2} " +
            $"abaixo do mínimo R$ {product.MinimumPremium:N2}");
    }

    // Limite máximo (warning, não erro)
    if (calculation.NetPremium > product.MaximumPremium)
    {
        _logger.LogWarning(
            "Prêmio acima do máximo para produto {ProductCode}",
            product.ProductCode);

        calculation.Warnings.Add(
            $"Prêmio acima do máximo: R$ {product.MaximumPremium:N2}");
    }
}`}</pre>
              </div>
            </div>
          </div>
        </div>

        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div className="bg-red-50 border-l-4 border-red-500 p-4 rounded">
            <div className="flex items-center gap-2 mb-2">
              <AlertTriangle className="w-5 h-5 text-red-600" />
              <h5 className="font-bold text-red-900">Limite Mínimo</h5>
            </div>
            <p className="text-sm text-red-800">Gera ERRO e interrompe processamento</p>
          </div>

          <div className="bg-yellow-50 border-l-4 border-yellow-500 p-4 rounded">
            <div className="flex items-center gap-2 mb-2">
              <AlertTriangle className="w-5 h-5 text-yellow-600" />
              <h5 className="font-bold text-yellow-900">Limite Máximo</h5>
            </div>
            <p className="text-sm text-yellow-800">Gera WARNING mas permite continuar</p>
          </div>
        </div>
      </div>

      {/* Processamento de Cosseguro */}
      <div className="bg-white rounded-lg shadow-lg p-6">
        <h3 className="text-2xl font-bold text-gray-900 mb-6 flex items-center gap-3">
          <Box className="w-7 h-7 text-[#FFB81C]" />
          R3000-PROCESSAR-COSSEGURO (Seção Mestre)
        </h3>

        <div className="bg-gradient-to-r from-yellow-50 to-orange-50 border border-yellow-300 rounded-lg p-6 mb-6">
          <p className="text-gray-700 mb-4">
            Cosseguro é quando múltiplas seguradoras compartilham o risco de uma apólice.
            Cada cossegurador tem um percentual de participação, e a soma DEVE ser 100%.
          </p>

          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            <div className="bg-white rounded p-4 border border-yellow-200">
              <div className="text-2xl font-bold text-yellow-700 mb-1">100%</div>
              <div className="text-sm text-gray-600">Soma obrigatória</div>
            </div>
            <div className="bg-white rounded p-4 border border-orange-200">
              <div className="text-2xl font-bold text-orange-700 mb-1">L</div>
              <div className="text-sm text-gray-600">Líder (Aceito)</div>
            </div>
            <div className="bg-white rounded p-4 border border-red-200">
              <div className="text-2xl font-bold text-red-700 mb-1">C</div>
              <div className="text-sm text-gray-600">Cedido</div>
            </div>
          </div>
        </div>

        <div className="bg-gray-900 text-gray-100 p-5 rounded-lg font-mono text-sm overflow-x-auto mb-6">
          <pre>{`R3000-PROCESSAR-COSSEGURO.
    *> Buscar dados de cosseguro/cessão
    PERFORM R3100-ABRIR-CURSOR-COSSEGURO.
    PERFORM R3200-FETCH-COSSEGURO.

    MOVE ZEROS TO WS-PREMIO-LIDER
                   WS-PREMIO-CEDIDO
                   WS-QTD-COSSEGURADORES.

    PERFORM UNTIL WS-FIM-CURSOR-COSSEGURO = 'S'
        ADD 1 TO WS-QTD-COSSEGURADORES

        PERFORM R3500-CALCULAR-PARTICIPACAO
        PERFORM R3600-GERAR-REGISTRO-PREMCED

        PERFORM R3200-FETCH-COSSEGURO
    END-PERFORM.

    PERFORM R3900-FECHAR-CURSOR-COSSEGURO.

    *> Validar soma de participações = 100%
    PERFORM R3800-VALIDAR-PARTICIPACOES.`}</pre>
        </div>

        <div className="bg-white border border-gray-200 p-5 rounded-lg font-mono text-sm overflow-x-auto">
          <pre className="text-gray-800">{`private async Task ProcessCosuranceAsync(
    Premium premium,
    Policy policy,
    PremiumCalculation calculation)
{
    var cosurances = await _cosuranceRepository.GetCosurancesAsync(
        policy.PolicyNumber,
        premium.EndorsementNumber);

    if (!cosurances.Any())
    {
        _logger.LogWarning(
            "Produto marcado com cosseguro mas sem participações: {PolicyNumber}",
            policy.PolicyNumber);
        return;
    }

    decimal totalLeaderPremium = 0m;
    decimal totalCededPremium = 0m;

    foreach (var cosurance in cosurances)
    {
        var participation = CalculateParticipation(calculation, cosurance);

        await GeneratePremcedRecordAsync(premium, policy, cosurance, participation);

        if (cosurance.ParticipationType == "L") // Líder
        {
            totalLeaderPremium += participation.PremiumAmount;
        }
        else // Cedido
        {
            totalCededPremium += participation.PremiumAmount;
        }
    }

    // Validar soma de participações = 100%
    ValidateParticipations(cosurances, calculation.GrossPremium);
}`}</pre>
        </div>
      </div>

      {/* R3500 - Calcular Participação */}
      <div className="bg-white rounded-lg shadow-lg p-6">
        <h3 className="text-2xl font-bold text-gray-900 mb-6 flex items-center gap-3">
          <Calculator className="w-7 h-7 text-orange-600" />
          R3500-CALCULAR-PARTICIPACAO
        </h3>

        <div className="bg-orange-50 border border-orange-200 rounded-lg p-6 mb-6">
          <h4 className="font-bold text-orange-900 mb-3 text-lg">Regra: Calcular Prêmio por Percentual de Participação</h4>

          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            <div>
              <h5 className="font-bold text-gray-800 mb-2">COBOL</h5>
              <div className="bg-gray-900 text-gray-100 p-4 rounded-lg font-mono text-xs overflow-x-auto">
                <pre>{`R3500-CALCULAR-PARTICIPACAO.
    COMPUTE WS-PREMIO-PARTICIPACAO =
        WS-PREMIO-EMITIDO *
        (COSSEGURO-PERCENTUAL / 100).

    IF COSSEGURO-TIPO = 'A'  *> Aceito (líder)
        ADD WS-PREMIO-PARTICIPACAO TO WS-PREMIO-LIDER
    ELSE  *> Cedido
        ADD WS-PREMIO-PARTICIPACAO TO WS-PREMIO-CEDIDO
    END-IF.`}</pre>
              </div>
            </div>

            <div>
              <h5 className="font-bold text-gray-800 mb-2">.NET</h5>
              <div className="bg-white border border-gray-200 p-4 rounded-lg font-mono text-xs overflow-x-auto">
                <pre className="text-gray-800">{`private CosuranceParticipation CalculateParticipation(
    PremiumCalculation calculation,
    Cosurance cosurance)
{
    var premiumAmount = calculation.GrossPremium *
        (cosurance.ParticipationPercentage / 100m);

    return new CosuranceParticipation
    {
        CoinsurerCompanyCode = cosurance.CoinsurerCompanyCode,
        ParticipationPercentage = cosurance.ParticipationPercentage,
        PremiumAmount = premiumAmount,
        ParticipationType = cosurance.ParticipationType
    };
}`}</pre>
              </div>
            </div>
          </div>
        </div>

        <div className="bg-blue-50 border border-blue-200 rounded-lg p-4">
          <h5 className="font-bold text-blue-900 mb-2 flex items-center gap-2">
            <CheckCircle className="w-5 h-5" />
            Exemplo de Cálculo
          </h5>
          <div className="text-sm text-blue-900 space-y-1">
            <p>• Prêmio Total: <span className="font-bold">R$ 10.000,00</span></p>
            <p>• Cossegurador A (Líder): <span className="font-bold">60%</span> = R$ 6.000,00</p>
            <p>• Cossegurador B (Cedido): <span className="font-bold">40%</span> = R$ 4.000,00</p>
            <p className="font-bold text-green-700">• Soma: 100% ✓</p>
          </div>
        </div>
      </div>

      {/* R3800 - Validar Participações */}
      <div className="bg-white rounded-lg shadow-lg p-6">
        <h3 className="text-2xl font-bold text-gray-900 mb-6 flex items-center gap-3">
          <Scale className="w-7 h-7 text-red-600" />
          R3800-VALIDAR-PARTICIPACOES
        </h3>

        <div className="bg-red-50 border border-red-300 rounded-lg p-6 mb-6">
          <h4 className="font-bold text-red-900 mb-3 text-lg flex items-center gap-2">
            <AlertTriangle className="w-5 h-5" />
            Regra Crítica: Soma de Percentuais DEVE Ser 100%
          </h4>

          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            <div>
              <h5 className="font-bold text-gray-800 mb-2">COBOL</h5>
              <div className="bg-gray-900 text-gray-100 p-4 rounded-lg font-mono text-xs overflow-x-auto">
                <pre>{`R3800-VALIDAR-PARTICIPACOES.
    MOVE ZEROS TO WS-SOMA-PARTICIPACOES.

    *> Somar todos os percentuais
    EXEC SQL
        SELECT SUM(PERCENTUAL_PARTICIPACAO)
        INTO :WS-SOMA-PARTICIPACOES
        FROM V0APOLCOSCED
        WHERE NUM_APOLICE = :WS-NUM-APOLICE
          AND NUM_ENDOSSO = :WS-NUM-ENDOSSO
    END-EXEC.

    IF WS-SOMA-PARTICIPACOES NOT = 100
        MOVE 'SOMA DE PARTICIPACOES DIFERENTE DE 100%'
            TO WS-MENSAGEM-ERRO
        PERFORM R9000-TRATAR-ERRO-VALIDACAO
    END-IF.`}</pre>
              </div>
            </div>

            <div>
              <h5 className="font-bold text-gray-800 mb-2">.NET</h5>
              <div className="bg-white border border-gray-200 p-4 rounded-lg font-mono text-xs overflow-x-auto">
                <pre className="text-gray-800">{`private void ValidateParticipations(
    List<Cosurance> cosurances,
    decimal totalPremium)
{
    var totalPercentage = cosurances.Sum(c => c.ParticipationPercentage);

    // Tolerância de 0.01% para arredondamento
    const decimal TOLERANCE = 0.01m;

    if (Math.Abs(totalPercentage - 100m) > TOLERANCE)
    {
        throw new BusinessRuleException(
            $"Soma de participações ({totalPercentage:N2}%) " +
            $"diferente de 100%");
    }

    // Validar soma de prêmios (reconciliação)
    var totalCalculatedPremium = cosurances
        .Sum(c => totalPremium * (c.ParticipationPercentage / 100m));

    if (Math.Abs(totalCalculatedPremium - totalPremium) > 0.01m)
    {
        _logger.LogWarning(
            "Diferença na soma de prêmios de cosseguro");
    }
}`}</pre>
              </div>
            </div>
          </div>
        </div>
      </div>

      {/* Validações por Ramo SUSEP */}
      <div className="bg-white rounded-lg shadow-lg p-6">
        <h3 className="text-2xl font-bold text-gray-900 mb-6 flex items-center gap-3">
          <Settings className="w-7 h-7 text-[#FFB81C]" />
          R2000-VALIDAR-RAMO-SUSEP
        </h3>

        <div className="bg-gradient-to-r from-purple-50 to-blue-50 border border-purple-200 rounded-lg p-6 mb-6">
          <p className="text-gray-700 mb-4">
            Cada ramo SUSEP tem regras específicas de validação que devem ser aplicadas durante o processamento.
          </p>
        </div>

        <div className="grid grid-cols-1 md:grid-cols-2 gap-6 mb-6">
          <div className="bg-blue-50 border-2 border-blue-300 rounded-lg p-6">
            <div className="flex items-center justify-between mb-4">
              <h4 className="text-xl font-bold text-blue-900">0531 - Vida Individual</h4>
              <CheckCircle className="w-6 h-6 text-blue-600" />
            </div>
            <div className="space-y-2 text-sm text-blue-900">
              <p>• Idade: 18-70 anos</p>
              <p>• Vigência máxima: 1 ano (365 dias)</p>
              <p>• Documentação médica: &gt; R$ 100.000</p>
              <p>• Seção: <span className="font-mono font-bold">R2100</span></p>
            </div>
          </div>

          <div className="bg-green-50 border-2 border-green-300 rounded-lg p-6">
            <div className="flex items-center justify-between mb-4">
              <h4 className="text-xl font-bold text-green-900">0532 - Vida em Grupo</h4>
              <CheckCircle className="w-6 h-6 text-green-600" />
            </div>
            <div className="space-y-2 text-sm text-green-900">
              <p>• Mínimo de segurados: 10 pessoas</p>
              <p>• Certificado individual obrigatório</p>
              <p>• Vigência: até 12 meses</p>
              <p>• Seção: <span className="font-mono font-bold">R2200</span></p>
            </div>
          </div>

          <div className="bg-orange-50 border-2 border-orange-300 rounded-lg p-6">
            <div className="flex items-center justify-between mb-4">
              <h4 className="text-xl font-bold text-orange-900">0553 - Acidentes Pessoais</h4>
              <CheckCircle className="w-6 h-6 text-orange-600" />
            </div>
            <div className="space-y-2 text-sm text-orange-900">
              <p>• Idade mínima: 14 anos</p>
              <p>• Coberturas: morte e invalidez</p>
              <p>• Capital segurado máximo validado</p>
              <p>• Seção: <span className="font-mono font-bold">R2300</span></p>
            </div>
          </div>

          <div className="bg-purple-50 border-2 border-purple-300 rounded-lg p-6">
            <div className="flex items-center justify-between mb-4">
              <h4 className="text-xl font-bold text-purple-900">0571 - Previdência Privada</h4>
              <CheckCircle className="w-6 h-6 text-purple-600" />
            </div>
            <div className="space-y-2 text-sm text-purple-900">
              <p>• Regime tributário: progressivo/regressivo</p>
              <p>• Período de diferimento validado</p>
              <p>• Taxa de carregamento máxima</p>
              <p>• Seção: <span className="font-mono font-bold">R2400</span></p>
            </div>
          </div>
        </div>

        <div className="bg-white border border-gray-200 p-5 rounded-lg font-mono text-sm overflow-x-auto">
          <pre className="text-gray-800">{`private void ValidateBySusepBranch(
    Premium premium,
    Policy policy,
    Product product,
    PremiumCalculation calculation)
{
    switch (product.SusepBranch)
    {
        case 531: // Vida Individual
            ValidateLifeInsurance(premium, policy, calculation);
            break;

        case 532: // Vida em Grupo
            ValidateGroupLifeInsurance(premium, policy, calculation);
            break;

        case 553: // Acidentes Pessoais
            ValidatePersonalAccidents(premium, policy, calculation);
            break;

        case 571: // Previdência Privada
            ValidatePensionPlan(premium, policy, calculation);
            break;

        default:
            ValidateGenericBranch(premium, policy, calculation);
            break;
    }
}`}</pre>
        </div>
      </div>

      {/* R2100 - Validar Vida */}
      <div className="bg-white rounded-lg shadow-lg p-6">
        <h3 className="text-2xl font-bold text-gray-900 mb-6 flex items-center gap-3">
          <CheckCircle className="w-7 h-7 text-blue-600" />
          R2100-VALIDAR-VIDA (Ramo 0531)
        </h3>

        <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
          <div>
            <h4 className="font-bold text-gray-900 mb-3 text-lg">COBOL</h4>
            <div className="bg-gray-900 text-gray-100 p-5 rounded-lg font-mono text-xs overflow-x-auto">
              <pre>{`R2100-VALIDAR-VIDA.
    *> Validar idade do segurado
    COMPUTE WS-IDADE-SEGURADO =
        FUNCTION INTEGER-OF-DATE(WS-DATA-PROCESSAMENTO) -
        FUNCTION INTEGER-OF-DATE(CLIENTE-DATA-NASCIMENTO).

    DIVIDE WS-IDADE-SEGURADO BY 365 GIVING WS-IDADE-ANOS.

    IF WS-IDADE-ANOS < 18 OR WS-IDADE-ANOS > 70
        MOVE 'IDADE FORA DO LIMITE PERMITIDO'
            TO WS-MENSAGEM-ERRO
        PERFORM R9000-TRATAR-ERRO-VALIDACAO
    END-IF.

    *> Validar vigência máxima
    COMPUTE WS-DIAS-VIGENCIA =
        FUNCTION INTEGER-OF-DATE(APOLICE-DATA-VIG-FIM) -
        FUNCTION INTEGER-OF-DATE(APOLICE-DATA-VIG-INI).

    IF WS-DIAS-VIGENCIA > 365
        MOVE 'VIGENCIA SUPERIOR A 1 ANO'
            TO WS-MENSAGEM-ERRO
        PERFORM R9000-TRATAR-ERRO-VALIDACAO
    END-IF.`}</pre>
            </div>
          </div>

          <div>
            <h4 className="font-bold text-gray-900 mb-3 text-lg">.NET</h4>
            <div className="bg-white border border-gray-200 p-5 rounded-lg font-mono text-xs overflow-x-auto">
              <pre className="text-gray-800">{`private void ValidateLifeInsurance(
    Premium premium,
    Policy policy,
    PremiumCalculation calculation)
{
    var insured = _clientRepository.GetById(
        policy.InsuredClientCode);

    // Validar idade (18-70 anos)
    var age = CalculateAge(insured.BirthDate, _processingDate);
    if (age < 18 || age > 70)
    {
        throw new BusinessRuleException(
            $"Idade {age} fora do limite permitido (18-70 anos)");
    }

    // Validar vigência máxima (365 dias)
    var policyDuration = (policy.EffectiveEndDate -
        policy.EffectiveStartDate).Days;
    if (policyDuration > 365)
    {
        throw new BusinessRuleException(
            $"Vigência de {policyDuration} dias superior " +
            $"ao máximo de 365 dias");
    }

    // Exigir documentação médica acima de R$ 100.000
    if (calculation.GrossPremium > 100000m)
    {
        calculation.Warnings.Add(
            "Documentação médica obrigatória para " +
            "prêmio acima de R$ 100.000");
    }
}`}</pre>
            </div>
          </div>
        </div>

        <div className="mt-6 bg-blue-50 border border-blue-200 rounded-lg p-4">
          <h5 className="font-bold text-blue-900 mb-2 flex items-center gap-2">
            <AlertTriangle className="w-5 h-5" />
            Regras Específicas do Ramo 0531
          </h5>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-4 text-sm text-blue-900">
            <div>
              <p className="font-bold mb-1">Idade do Segurado</p>
              <p>18-70 anos (validação obrigatória)</p>
            </div>
            <div>
              <p className="font-bold mb-1">Vigência Máxima</p>
              <p>365 dias (1 ano comercial)</p>
            </div>
            <div>
              <p className="font-bold mb-1">Documentação Médica</p>
              <p>Obrigatória acima de R$ 100.000</p>
            </div>
          </div>
        </div>
      </div>

      {/* Footer de Referências */}
      <div className="bg-gradient-to-r from-gray-50 to-gray-100 rounded-lg border border-gray-300 p-6">
        <h3 className="text-xl font-bold text-gray-900 mb-4 flex items-center gap-2">
          <FileText className="w-6 h-6 text-[#0047BB]" />
          Referências
        </h3>
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4 text-sm text-gray-700">
          <div>
            <p className="font-bold text-gray-900 mb-2">Documentação Técnica</p>
            <ul className="space-y-1">
              <li>• Estruturas COBOL: <span className="font-mono text-blue-600">03-data-structures.md</span></li>
              <li>• Modelo de Dados: <span className="font-mono text-blue-600">04-database-model.md</span></li>
              <li>• Código Fonte Original: <span className="font-mono text-blue-600">LEGACY_SYSTEM_DOCUMENTATION.md</span></li>
            </ul>
          </div>
          <div>
            <p className="font-bold text-gray-900 mb-2">Especificação de Migração</p>
            <ul className="space-y-1">
              <li>• Feature Spec: <span className="font-mono text-blue-600">specs/001-vamos-migrar-sistema/spec.md</span></li>
              <li>• Data Model: <span className="font-mono text-blue-600">specs/001-vamos-migrar-sistema/data-model.md</span></li>
              <li>• Research: <span className="font-mono text-blue-600">specs/001-vamos-migrar-sistema/research.md</span></li>
            </ul>
          </div>
        </div>
        <div className="mt-4 pt-4 border-t border-gray-300 text-xs text-gray-600">
          <p><strong>Documento criado em:</strong> 2025-10-27 | <strong>Última atualização:</strong> 2025-10-27 | <strong>Versão:</strong> 1.0</p>
        </div>
      </div>
    </div>
  );
};
