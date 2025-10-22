       IDENTIFICATION               DIVISION.
      *--------------------------------------
      *
       PROGRAM-ID.                  RG1866B.
      *
      *----------------------------------------------------------------*
      *   SISTEMA ................  REGISTROS GERAIS                   *
      *   PROGRAMA ...............  RG1866B                            *
      *----------------------------------------------------------------*
      *   ANALISTA ...............  GILSON                             *
      *   PROGRAMADOR ............  WELLINGTON F R C VERAS.            *
      *   DATA CODIFICACAO .......  21/05/2014                         *
      *----------------------------------------------------------------*
      *   FUNCAO: CIRCULAR SUSEP 360 - PREMIT.TXT E PREMCED.TXT        *
      *           PREMIOS EMITIDOS                                     *
      *                                                                *
      *                                                 CADMUS C97168  *
      * --------------------------------- -----------------    ------- *
      * TABELA                            VIEW                 ACESSO  *
      * --------------------------------- -----------------    ------- *
      * SISTEMAS                          V0SISTEMA            INPUT   *
      * RELATORIOS                        V0RELATORIOS         I-O     *
      * PREMIOS ANALITICO                 V0PREMIOS            INPUT   *
      * PRODUTO                           V0PRODUTO            INPUT   *
      * ENDOSSOS                          V0ENDOSSO            INPUT   *
      * PARCELA HISTORICO                 V0HISTOPARC          INPUT   *
      * FONTES                            V0FONTE              INPUT   *
      * APOLICE AUTO                      V0AUTOAPOL           INPUT   *
      * HISTORICO DE PROPOSTA CONV AUTO   AU055                INPUT   *
      * APOLICES                          V0APOLICE            INPUT   *
      * CLIENTES                          V0CLIENTE            INPUT   *
      * EF APOLICE (EF063)                EF-APOLICE           INPUT   *
      * EF CONTRATO (EF050)               EF-CONTRATO          INPUT   *
      * EF PRODUTO ACESSORIO (EF148)      EF-PROD-ACESSORIO    INPUT   *
      * EF ENDOSSO (EF053)                EF-ENDOSSO           INPUT   *
      * EF FATURAS ENDOSSO (EF054)        EF-FATURAS-ENDOSSO   INPUT   *
      * EF FATURA (EF056)                 EF-FATURA            INPUT   *
      * EF PREMIOS FATURA (EF060)         EF-PREMIOS-FATURA    INPUT   *
      * EF PREMIOS EMITIDOS (EF066)       EF-PREMIOS-EMITIDOS  INPUT   *
      * PRODUTOS VG                       V0PRODUTOSVG         INPUT   *
      * HIST CONT PARCELVA (HTCTPBVA)     HIST-CONT-PARCELVA   INPUT   *
      * HIS COBER PROPOST (COBPRPVA)      HIS-COBER-PROPOST    INPUT   *
      * FATURAS                           V0FATURAS            INPUT   *
      * FATURAS TOTAIS                    V0FATURASTOT         INPUT   *
      * SEGURADOS VGAP                    V0SEGURAVG           INPUT   *
      * AGENTE FINANCEIRO                 V0AGENTE-FINANC      INPUT   *
      * TOMADOR                           V0TOMADOR            INPUT   *
      * ENDERECOS                         V0ENDERECOS          INPUT   *
      * AGENCIAS                          V0AGENCIAS           INPUT   *
      * PRODUTORES                        V0PRODUTOR           INPUT   *
      * APOLICE CORRETOR                  V0APOLCORRET         INPUT   *
      * APOLICE COBERTURAS                V0COBERAPOL          INPUT   *
      * COTACAO DE MOEDAS                 V0COTACAO            INPUT   *
      * APOLICE COSSEGURADORA             V0APOLCOSCED         INPUT   *
      * APOLICE COBRANCA                  APOLCOBR             INPUT   *
      * BILHETE                           BILHETE              INPUT   *
      * FUNCIONARIOS CEF                  FUNCICEF             INPUT   *
      * PROPOSTA_FIDELIZ                  PROPFID              INPUT   *
      *----------------------------------------------------------------*
      * ARQUIVO                                                ACESSO  *
      *--------------------------------------------------------------- *
      * PREMIT                                                 OUTPUT  *
      * PREMCED                                                OUTPUT  *
      *----------------------------------------------------------------*
      *  ALTERACAO - INCLUIR TESTE DE RAMO 31 E 53                     *
      * 25/07/2014 - WELLINGTON VERAS (TE39902)    PROCURAR POR C97168 *
      *----------------------------------------------------------------*
      *  ALTERACAO - CORRIGIR/AJUSTAR O ACESSO/JOIN DAS TAB EF_APOLICE,*
      *              EF_CONTRATO E EF_PROD_ACESSORIO PARA EXCLUIR O    *
      *  GILSON      O RELACIONAMENTO ENTRE O ATRIBUTO NUM_APOLICE     *
      *              CADMUS - 101581                                   *
      * 12/08/2014 - WELLINGTON VERAS (TE39902    )   PROJETO CAD10158 *
      *----------------------------------------------------------------*
      *  ALTERACAO - CORRIGIR/AJUSTAR O ACESSO/JOIN DAS TAB EF_APOLICE,*
      *              EF_CONTRATO E EF_PROD_ACESSORIO PARA EXCLUIR O    *
      *              O RELACIONAMENTO ENTRE O ATRIBUTO NUM_APOLICE     *
      *            - CADMUS - 101581                                   *
      * 26/08/2014 - WELLINGTON VERAS (TE39902)       PROJETO CAD10158 *
      *----------------------------------------------------------------*
      *  ALTERACAO - ACRESCENTAR A COLUNA PRODUTO NO FINAL DO ARQUIVO  *
      *              PREMCED                                           *
      *            - CADMUS - 103462                                   *
      * 10/10/2014 - WELLINGTON VERAS (TE39902)       PROJETO C103462  *
      *----------------------------------------------------------------*
      *  ALTERACAO - RECUPERAR O N£MERO DE PROPOSTA DOS PRODUTOS 1803  *
      *              1805 PELO NUMERO DO T¥TULO GRAVADO NO SIAS.       *
      * 26/02/2015   MOVER ZEROS PARA COD FONTE.                       *
      *     CADMUS - 105223                     NO VERSIONA  C105223   *
      * 15/01/2015 - WELLINGTON VERAS  TE39902   PROCURAR POR 105223   *
      *----------------------------------------------------------------*
      *  ALTERACAO - AJUSTE NOS TIPOS DE MOVIMENTO 104, 105 E 106      *
      * 10/04/2015   SUSEP 360                                         *
      *     CADMUS - 112349                     NO VERSIONA  C112349   *
      * 18/05/2015 - RECUPERAR O NUMERO CORRETO DA PROPOSTA PARA OS    *
      *              RAMOS DO PRODUTO AUTOMOVEL                        *
      * 14/09/2015 - INCLUIR CRITICA DO RAMO-SUSEP PARA DATA-PROPOSTA  *
      *              MAIOR QUE DATA-INICIO-VIG                         *
      * RESPONSAVEL- WELLINGTON VERAS  TE39902   PROCURAR POR 112349   *
      *----------------------------------------------------------------*
      *  ALTERACAO - ADICIONAR NO PREMCED DATA DE EMISSÞO DOS PREMIOS  *
      *              EMITIDOS                                          *
      *     CADMUS - 119167                                            *
      * 16/06/2015 - WELLINGTON VERAS (TE39902)  PROCURAR POR 119167   *
      *----------------------------------------------------------------*
      *  ALTERACAO - IGUALAR O LAYOUT RG1866B-PREMIT COM O LAYOUT DO   *
      *              RG0896B-PREMIT.                                   *
      *     CADMUS - 117159                                            *
      * 20/10/2015 - WELLINGTON VERAS (TE39902)  PROCURAR POR 117159   *
      *----------------------------------------------------------------*
      *  ALTERACAO - RECUPERAR O PRM-TARIFARIO-VAR DA COBERAPOL E      *
      *              ATUALIZAR O CAMPO EMI-TIPO-MOV                    *
      *     CADMUS - 153103                                            *
      * 11/04/2016 - WELLINGTON VERAS (TE39902)  PROCURAR POR 153103   *
      *----------------------------------------------------------------*
      *  ALTERACAO - INCLUIR NO LAYOUT DO ARQUIVO OS CAMPOS CANAL DE   *
      *              VENDAS E O NUMERO DA PARCELA                      *
      *     CADMUS - 136071                                            *
      * 06/05/2016 - GILSON PINTO DA SILVA       PROCURAR POR 136071   *
      *----------------------------------------------------------------*
      *  ALTERACAO - INCLUIR NO LAYOUT DO ARQUIVO OS CAMPOS MATRICULA  *
      *              NOME E CPF DO INDICADOR/AGENCIADOR                *
      *     CADMUS - 136081                                            *
      * 11/05/2016 - GILSON PINTO DA SILVA       PROCURAR POR 136081   *
      *----------------------------------------------------------------*
      *  ALTERACAO - MOVE 01 PARA QTDE DE SEGURADO SE QTDE-SEGURADO    *
      *              FOR MENOR QUE 01                                  *
      *     CADMUS - 123159                                            *
      * 05/07/2016 - WELLINGTON VERAS  TE39902                         *
      *----------------------------------------------------------------*
      * ALTERACAO  - INCLUIR OS CAMPOS PREMIO TARIFARIO, PERCENTUAL E  **
      *              VALOR TARIFA BALCAO NO FINAL DO ARQUIVO.          *
      * 07/07/2016 - JOSE RENATO (TE37067)      - PROCURAR POR 139415. *
      *----------------------------------------------------------------*
      * ALTERACAO  - INCLUIR OS CAMPOS PERCENTUAL E VALOR DA COMISSAO  **
      *              DO INDICADOR NO FINAL DO ARQUIVO.                 *
      * 13/09/2016 - GILSON PINTO DA SILVA      - PROCURAR POR 141119. *
      *----------------------------------------------------------------*
      * ALTERACAO  - INCLUIR O PARAMETRO DIA-INICIAL E DIA-FINAL NA    **
      *              CLAUSULA WHERE DO DECLARE PRINCIPAL. PARA TORNAR  *
      *              O PROCESSAMENTO SEMANAL ACUMULATIVO E MENSAL.     *
      * 27/10/2016 - WELLINGTON F R C VERAS     - PROCURAR POR 142985. *
      *----------------------------------------------------------------*
      *  ALTERACAO - MOVER ZEROS SOMENTE PARA AS DUAS PRIMEIRAS        *
      *              POSICOES DO EMI-COD-FONTE.                        *
      *     CADMUS - 105223                                            *
      * 07/11/2016 - WELLINGTON VERAS  TE39902  - PROCURAR POR 105223  *
      *----------------------------------------------------------------*
      * ALTERACAO  - ALTERA PIC V0RELA-IDE-SISTEMA E V0RELA-COD-RELAT. *
      * 09/01/2017 - JOSE RENATO       TE37067  - PROCURAR POR 142985  *
      *----------------------------------------------------------------*
      * ALTERACAO  - INCLUIR O CAMPO EMI-TIPO-RENOV NO ARQUIVO.        *
      * 11/01/2017 - JOSE RENATO (TE37067)      - PROCURAR POR 146163. *
      *----------------------------------------------------------------*
      * ALTERACAO  - INCLUIR NO LAYOUT DO ARQUIVO OS CAMPOS PARA TRATAR*
      *              E IDENTIFICAR O MOTIVO DO CANCELAMENTO            *
      *     CADMUS - 146365                                            *
      * 18/01/2017 - JOSE RENATO (TE37067)      - PROCURAR POR 146365. *
      *----------------------------------------------------------------*
      * ALTERACAO  - INCLUIR NO LAYOUT DO ARQUIVO OS CAMPOS PARA TRATAR*
      *              E IDENTIFICAR OS PERCENTUAIS DE RESSEGURO DE COTA *
      *     CADMUS - 148834                                            *
      * 10/03/2017 - GILSON PINTO DA SILVA      - PROCURAR POR 148834. *
      *----------------------------------------------------------------*
      * ALTERACAO  - REJEITAR OS REGISTROS DA V0PREMIOS QUANDO O       **
      *              GRUPO-RAMO-SUSEP FOR IGUAL A 09 E NUMBIL FOR IGUAL*
      *              A ZERO                                            *
      * 06/04/2017 - WELLINGTON F R C VERAS     - PROCURAR POR 149755. *
      *----------------------------------------------------------------*
      * ALTERACAO  - ACERTAR O TAMANHO DA PIC NA VARIAVEL              **
      *              V0RELA-COD-USUARIO PARA 08 BYTES                  *
      * 07/08/2017 - WELLINGTON F R C VERAS     - PROCURAR POR 153132  *
      *----------------------------------------------------------------*
      * ALTERACAO  - QUANDO A EMI-DT-PROPT > EMI-DTINIVIG, MOVER  A    **
      *              DATPRO OU A DTINIVIG DO ENDOSSO ZERO, P/ OS RAMOS **
      *              0167, 0860, 0870, 0993, 1061, 1065 E 1068         *
      * 21/08/2017 - JOSE RENATO                - PROCURAR POR 136184. *
      *----------------------------------------------------------------*
      *  ALTERACAO - ALTERACAO DA DATA DO INICIO DE VIGENCIA PARA DATA *
      *              DE EMISSAO NO PARAMETRO DO CALL DA SUBROT RE0001S *
      *              PARA O RAMO GARANTIA (40, 45, 75 E 76) A PARTIR DE*
      *              01/10/2017                                        *
      * 13/09/2017 - WELLINGTON VERAS - TE39902        CADMUS - 154263 *
      *----------------------------------------------------------------*
      *  ALTERACAO - INCLUIR OS PROCESSOS SUSEP NOS ARQUIVOS DE SAIDA  *
      *              PARA OS PRODUTOS 1803, 1804 OU 1805 PARA OS RAMOS *
      *              118, 141 E 351.                                   *
      * 10/04/2018 - WELLINGTON VERAS - TE39902        CADMUS - 136184 *
      *----------------------------------------------------------------*
      *  ALTERACAO - MUDAR DE SMALLINT PARA INTEGER OS CAMPOS          *
      *              EF_SEQ_PREMIO                                     *
      * 24/04/2018 - WELLINGTON VERAS - TE39902        TAREFA - T26640 *
      *----------------------------------------------------------------*
      *  ALTERACAO - EXCLUIR DO CALCULO DA PPNG E DCD OS PRODUTOS 7705,*
      *              7716 E 7725 DO RAMO 77 A PARTIR DO MES 08/2018 EM *
      *              FUNCAO DA MIGRACAO PARA O SISTEMA SMART           *
      * 29/08/2018 - WELLINGTON F R C VERAS   JAZZ - HISTORIA - 169452 *
      *----------------------------------------------------------------*
      * PROJETO JV1.                                                   *
      * 06/12/2018 - INCLUIR A COLUNA EMPRESA NO ARQ DE SAIDA - EXCELL *
      * 13/03/2019 - TESTE DE EMPRESA_SAP 10 (8141) OUTRAS (5631)      *
      *                                                                *
      * RESPONSALVEL - WELLINGTON FRC VERAS.    JAZZ_HISTORIA - 188334 *
      *----------------------------------------------------------------*
      *  ALTERACAO - INCLUIR NO PARAMETRO DE SAIDA DA SUB-ROTN RE0001S *
      *              A DATA DO CUTOFF                                  *
      * 27/03/2019 - WELLINGTON F R C VERAS   JAZZ - HISTORIA - 192299 *
      *----------------------------------------------------------------*
      *  ALTERACAO - INCLUIR CAMPO EMI_PROP_SIVPF E INSERIR TRATATIVAS *
      *              BUSCANDO O N§MERO DA PROPOSTA NO SIVPF            *
      * 17/04/2019 - RAUL BASILI ROTTA        JAZZ - HISTORIA - 198783 *
      *                                PROCURAR POR  TAREFA   - 198785 *
      *----------------------------------------------------------------*
      *  ALTERACAO - INCLUIR CAMPO EMI_PROP_SIVPF E INSERIR TRATATIVAS *
      *              BUSCANDO O N§MERO DA PROPOSTA NO SIVPF            *
      * 29/07/2019 - WELLINGTON VERAS         JAZZ - TAREFA   - 198785 *
      *----------------------------------------------------------------*
      *  ALTERACAO - INCLUIR NO PARAMETRO DE SAIDA DA SUB-ROTN RE0001S *
      *              O CODIGO DO CONTRATO DE RESSEGURO                 *
      * 14/10/2019 - WELLINGTON F R C VERAS   JAZZ - HISTORIA - 212422 *
      *                                              TAREFA   - 221601 *
      *----------------------------------------------------------------*
      *  ALTERACAO - SEPARAR VALOR DA COMISSAO DE ADMINISTRACAO E DA   *
      *              COMISSAO DE AGENCIAMENTO DO VALOR DE CORRETAGEM.  *
      *            - CRIAR CAMPO DE VALOR P/ COMISSAO DE AGENCIAMENTO. *
      *            - MOVER O CAMPO V0PREM-NUMBIL P/ EMI-NUM-CERTIF,    *
      *              CONF. SOLICITACAO SURAD.                          *
      *            - TRATAR APENAS OS RAMO_SUSEP 1381, 1601.
      *            - IGUALA LAYOUT COM O PREMIT GERADO NO RG1896B
      *            - PARA CARGA EM TABELA NA BAIXA PLATAFORMA.
      * 04/03/2020 - JOSE RENATO              JAZZ - TAREFA   - 235637 *
      *----------------------------------------------------------------*
      *  ALTERACAO - INCLUIR RAMOS 0969 E 0982 COM NUMBIL > 0.         *
      * 02/05/2020 - JOSE RENATO              JAZZ - TAREFA   - 243278 *
      *----------------------------------------------------------------*
      *  ALTERACAO - tratar o Cìdigo da Cia. por empresa da HCXS       *
      *              EMPRESA = 00 MOVER 05631 P. CODIGO DA CIA         *
      *              EMPRESA = 10 MOVER 08141 P. CODIGO DA CIA         *
      *              EMPRESA = 11 MOVER 00442 P. CODIGO DA CIA         *
      *                                                                *
      * 20/11/2020 - WELLINGTON FRC VERAS.    JAZZ TAREFA - 266453     *
      *----------------------------------------------------------------*
      *  ALTERACAO - INCLUIR A COLUNA TIPO_OPERACAO NO FINAL DO arquivo*
      *              PREMIT                                            *
      *                                                                *
      * 20/04/2021 - WELLINGTON FRC VERAS.    JAZZ TAREFA - 285991     *
      *----------------------------------------------------------------*
      *  ALTERACAO - PADRONIZAR OS SELECTs NA TABELA RELATORIOS        *
      *            - PADRONIZAR OS DELETEs NA TABELA RELATORIOS        *
      *                                                                *
      * 11/02/2022 - WELLINGTON FRC VERAS.    JAZZ TAREFA - 362429     *
      *----------------------------------------------------------------*
      *  ALTERACAO - O PROGRAMA DEVERAR TRABALHAR COM A DATA DIARIA    *
      *              "GL" DA TABELA SISTEMAS                           *
      *                                                                *
      * 02/03/2022 - WELLINGTON FRC VERAS.    JAZZ TAREFA - 362429     *
      *----------------------------------------------------------------*
      *  ALTERACAO - O PROGRAMA DEVERAR TRABALHAR COM A DATA DIARIA    *
      *              "GL" DA TABELA SISTEMAS E COD_USUARIO = 'RG0840B' *
      *              NA TABELA RELATORIOS.                             *
      * 18/04/2022 - WELLINGTON FRC VERAS.    JAZZ TAREFA - 379341     *
      *----------------------------------------------------------------*
      *  ALTERACAO - Substituir variveis WHOST-DIA-INI, WHOST-DIA-FIM *
      *              pela varivel WHOST-DIA-REFER como sendo SMALLINT *
      *              excluir as variveis WS-ANO-REFER e WS-MES-REFER. *
      *              Alterar no pargrafo R0200 o teste do SQLCODE     *
      *              Alterar no pargrafo R0500                        *
      * 21/09/2022 - WELLINGTON FRC VERAS.    JAZZ TAREFA - 428303     *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       ENVIRONMENT                  DIVISION.
      *--------------------------------------
      *
       CONFIGURATION                SECTION.
      *-------------------------------------
      *
       SPECIAL-NAMES.
      *
      *--* INIBIDO PARA GERAR PONTO NA DECIMAL
      *
    *****  DECIMAL-POINT      IS    COMMA.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       INPUT-OUTPUT                 SECTION.
      *--------------------------------------
      *
       FILE-CONTROL.
      *
           SELECT     PREMIT
                      ASSIGN        TO        PREMIT
                      FILE  STATUS  IS        EMI-STATUS.
      *
           SELECT     PREMCED
                      ASSIGN        TO        PREMCED
                      FILE  STATUS  IS        CED-STATUS.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       DATA                         DIVISION.
      *--------------------------------------
      *
       FILE                         SECTION.
      *-------------------------------------
      *
       FD              PREMIT
136071*****            RECORD     480
136071*****            RECORD     545
139415*****            RECORD     584
141119*****            RECORD     607
146163*****            RECORD     609
146365*****            RECORD     611
146365*****            RECORD     676
148834*****            RECORD     704
188334***              RECORD     708
198785***              RECORD     723
235637***              RECORD     761
285991                 RECORD     765
                       RECORDING  MODE       F
                       BLOCK      CONTAINS   0  RECORDS
                       LABEL      RECORD        OMITTED.
      *
136071*****            REG-PREMIT            PIC  X(480).
136071*****            REG-PREMIT            PIC  X(545).
139415*****            REG-PREMIT            PIC  X(584).
141119*****            REG-PREMIT            PIC  X(607).
146163*****            REG-PREMIT            PIC  X(609).
146365*****            REG-PREMIT            PIC  X(611).
146365*****            REG-PREMIT            PIC  X(676).
148834*****            REG-PREMIT            PIC  X(704).
188334***              REG-PREMIT            PIC  X(708).
198785***              REG-PREMIT            PIC  X(761).
285991 01              REG-PREMIT            PIC  X(765).
      *
       FD              PREMCED
119167***              RECORD     160
188334***              RECORD     164
285991                 RECORD     168
                       RECORDING  MODE       F
                       BLOCK      CONTAINS   0  RECORDS
                       LABEL      RECORD        OMITTED.
      *
119167***              REG-PREMCED           PIC  X(160).
188334***              REG-PREMCED           PIC  X(164).
285991 01              REG-PREMCED           PIC  X(168).
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       WORKING-STORAGE              SECTION.
      *-------------------------------------
      *
           EXEC  SQL   BEGIN  DECLARE  SECTION   END-EXEC.
      *
      *----------------------------------------------------------------*
      *              DEFINICAO DAS VARIAVEIS INDICADORAS               *
      *----------------------------------------------------------------*
      *
       77          VIND-DTH-EFETV      PIC S9(004)      VALUE +0 COMP.
      *
      *----------------------------------------------------------------*
      *            DEFINICAO DAS VARIAVEIS HOST AUXILIARES             *
      *----------------------------------------------------------------*
      *
       77          WHOST-INIVIG-AP     PIC  X(010)      VALUE SPACES.
136184 77          WHOST-DATPRO-AP     PIC  X(010)      VALUE SPACES.
154263 77          WHOST-DTEMIS-AP     PIC  X(010)      VALUE SPACES.
      *
       77          WHOST-QTD-DOCT      PIC S9(009)      VALUE +0 COMP.
       77          WHOST-QTD-ITEM      PIC S9(009)      VALUE +0 COMP.
      *
       77          WHOST-NUM-APOL      PIC S9(013)      VALUE +0 COMP-3.
       77          WHOST-NUM-ENDS      PIC S9(009)      VALUE +0 COMP.
       77          WHOST-RMO-COBT      PIC S9(004)      VALUE +0 COMP.
       77          WHOST-DTINIVIG      PIC  X(010)      VALUE SPACES.
       77          WHOST-DTEMIS        PIC  X(010)      VALUE SPACES.
       77          WHOST-DTPROP        PIC  X(010)      VALUE SPACES.
142985 77          WHOST-DIA-REFER     PIC S9(004)      VALUE +0 COMP.
      *
       77          WHOST-COD-COSG      PIC S9(004)      VALUE +0 COMP.
       77          WHOST-QTDE-REG      PIC S9(009)      VALUE +0 COMP.
       77          WHOST-QTD-COSG      PIC S9(009)      VALUE +0 COMP.
       77          WHOST-PERC-CED      PIC S9(004)V9(9) VALUE +0 COMP-3.
      *
C10158 77          WHOST-TIP-PRM-I     PIC  X(001)      VALUE SPACES.
C10158 77          WHOST-TIP-PRM-F     PIC  X(001)      VALUE SPACES.
      *
       77          WHOST-PCT-COSG      PIC S9(004)V9(9) VALUE +0 COMP-3.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *            DEFINICAO DAS VARIAVEIS HOST AUXILIARES             *
      *----------------------------------------------------------------*
      *
      *--* TABELA DE SISTEMAS (V0SISTEMA)
      *----------------------------------
      *
       77          V0SIST-DTMOVABE     PIC  X(010)      VALUE SPACES.

      *--* TABELA DE RELATORIOS (V0RELATORIOS)
      *---------------------------------------
      *
153132 77          V0RELA-COD-USUARIO  PIC  X(008)      VALUE SPACES.
       77          V0RELA-DTA-SOLICTA  PIC  X(010)      VALUE SPACES.
142985 77          V0RELA-IDE-SISTEMA  PIC  X(002)      VALUE SPACES.
142985 77          V0RELA-COD-RELAT    PIC  X(008)      VALUE SPACES.
       77          V0RELA-PERI-INICIAL PIC  X(010)      VALUE SPACES.
       77          V0RELA-PERI-FINAL   PIC  X(010)      VALUE SPACES.
       77          V0RELA-ANO-REFER    PIC S9(004)      VALUE +0 COMP.
       77          V0RELA-MES-REFER    PIC S9(004)      VALUE +0 COMP.
       77          V0RELA-DATA-REFR    PIC  X(010)      VALUE SPACES.

      *--* TABELA PREMIOS ANALITICOS (V0PREMIOS)
      *-----------------------------------------
      *
       77          V0PREM-COD-EMP      PIC S9(009)      VALUE +0 COMP.
       77          V0PREM-ANO-REFER    PIC S9(004)      VALUE +0 COMP.
       77          V0PREM-MES-REFER    PIC S9(004)      VALUE +0 COMP.
       77          V0PREM-DIA-REFER    PIC S9(004)      VALUE +0 COMP.
       77          V0PREM-TIPO-MOVT    PIC  X(001)      VALUE SPACES.
       77          V0PREM-NUM-APOL     PIC S9(013)      VALUE +0 COMP-3.
       77          V0PREM-NRENDOS      PIC S9(009)      VALUE +0 COMP.
       77          V0PREM-NRPARCEL     PIC S9(004)      VALUE +0 COMP.
       77          V0PREM-NUM-OCORR    PIC S9(004)      VALUE +0 COMP.
       77          V0PREM-OCORHIST     PIC S9(004)      VALUE +0 COMP.
       77          V0PREM-RAMOFR       PIC S9(004)      VALUE +0 COMP.
       77          V0PREM-MODALIFR     PIC S9(004)      VALUE +0 COMP.
       77          V0PREM-OPERACAO     PIC S9(004)      VALUE +0 COMP.
       77          V0PREM-TIPO-OPER    PIC S9(004)      VALUE +0 COMP.
       77          V0PREM-CODCLIEN     PIC S9(009)      VALUE +0 COMP.
       77          V0PREM-VALOR-COT    PIC S9(006)V9(9) VALUE +0 COMP-3.
       77          V0PREM-IMP-SEG-IT   PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLPRMBAS-IT  PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLPREFIX-IT  PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLPRMTAR-IT  PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLDESCON-IT  PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLPRMLIQ-IT  PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLADIFRA-IT  PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLCUSEMI-IT  PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLIOCC-IT    PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLPRMTOT-IT  PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLCOMIS-IT   PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLADMN-IT    PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLAGENC-IT   PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLPREFCM-IT  PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-IMP-SEG-IL   PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLPRMBAS-IL  PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLPREFIX-IL  PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLPRMTAR-IL  PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLDESCON-IL  PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLPRMLIQ-IL  PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLADIFRA-IL  PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLCUSEMI-IL  PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLIOCC-IL    PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLPRMTOT-IL  PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLCOMIS-IL   PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLADMN-IL    PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLAGENC-IL   PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLPREFCM-IL  PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-IMP-SEG-IC   PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLPRMBAS-IC  PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLPREFIX-IC  PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLPRMTAR-IC  PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLDESCON-IC  PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLPRMLIQ-IC  PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLADIFRA-IC  PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLCOMIS-IC   PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLADMN-IC    PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLAGENC-IC   PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLPREFCM-IC  PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-IMP-SEG-IR   PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLPRMTAR-IR  PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLDESCON-IR  PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLPRMLIQ-IR  PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLADIFRA-IR  PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-VLCOMIS-IR   PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0PREM-IMP-SEG-T    PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLPRMBAS-T   PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLPREFIX-T   PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLPRMTAR-T   PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLDESCON-T   PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLPRMLIQ-T   PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLADIFRA-T   PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLCUSEMI-T   PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLIOCC-T     PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLPRMTOT-T   PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLCOMIS-T    PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLADMN-T     PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLAGENC-T    PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLPREFCM-T   PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-IMP-SEG-L    PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLPRMBAS-L   PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLPREFIX-L   PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLPRMTAR-L   PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLDESCON-L   PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLPRMLIQ-L   PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLADIFRA-L   PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLCUSEMI-L   PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLIOCC-L     PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLPRMTOT-L   PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLCOMIS-L    PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLADMN-L     PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLAGENC-L    PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLPREFCM-L   PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-IMP-SEG-C    PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLPRMBAS-C   PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLPREFIX-C   PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLPRMTAR-C   PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLDESCON-C   PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLPRMLIQ-C   PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLADIFRA-C   PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLCOMIS-C    PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLADMN-C     PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLAGENC-C    PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLPREFCM-C   PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-IMP-SEG-R    PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLPRMTAR-R   PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLDESCON-R   PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLPRMLIQ-R   PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLADIFRA-R   PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLCOMIS-R    PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-VLPREMIO     PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0PREM-ORGAO        PIC S9(004)      VALUE +0 COMP.
       77          V0PREM-RAMO         PIC S9(004)      VALUE +0 COMP.
       77          V0PREM-CODSUBES     PIC S9(004)      VALUE +0 COMP.
       77          V0PREM-FONTE        PIC S9(004)      VALUE +0 COMP.
       77          V0PREM-NRENDOCA     PIC S9(009)      VALUE +0 COMP.
       77          V0PREM-QTPARCEL     PIC S9(004)      VALUE +0 COMP.
       77          V0PREM-CORRECAO     PIC  X(001)      VALUE SPACES.
       77          V0PREM-RNUDOC       PIC S9(009)      VALUE +0 COMP.
       77          V0PREM-APOLIDER     PIC  X(015)      VALUE SPACES.
       77          V0PREM-ENDOSLID     PIC  X(015)      VALUE SPACES.
       77          V0PREM-ORDLIDER     PIC S9(015)      VALUE +0 COMP-3.
       77          V0PREM-CODLIDER     PIC S9(004)      VALUE +0 COMP.
       77          V0PREM-DTEMILID     PIC  X(010)      VALUE SPACES.
       77          V0PREM-MOEDA-PRM    PIC S9(004)      VALUE +0 COMP.
       77          V0PREM-MOEDA-IMP    PIC S9(004)      VALUE +0 COMP.
       77          V0PREM-NUMBIL       PIC S9(015)      VALUE +0 COMP-3.
       77          V0PREM-TIPSGU       PIC  X(001)      VALUE SPACES.
       77          V0PREM-TIPO-ENDS    PIC  X(001)      VALUE SPACES.
       77          V0PREM-IDRISCO      PIC  X(001)      VALUE SPACES.
       77          V0PREM-TIPAPO       PIC  X(001)      VALUE SPACES.
       77          V0PREM-TIPCALC      PIC  X(001)      VALUE SPACES.
       77          V0PREM-PODPUBL      PIC  X(001)      VALUE SPACES.
       77          V0PREM-TPCOSCED     PIC  X(001)      VALUE SPACES.
       77          V0PREM-NUM-ATA      PIC S9(009)      VALUE +0 COMP.
       77          V0PREM-ANO-ATA      PIC S9(004)      VALUE +0 COMP.
       77          V0PREM-PCADMCOS     PIC S9(003)V99   VALUE +0 COMP-3.
       77          V0PREM-DTINIVIG     PIC  X(010)      VALUE SPACES.
       77          V0PREM-DTTERVIG     PIC  X(010)      VALUE SPACES.
       77          V0PREM-DTEMIS       PIC  X(010)      VALUE SPACES.
       77          V0PREM-DTMOVTO      PIC  X(010)      VALUE SPACES.
       77          V0PREM-DTCOTACAO    PIC  X(010)      VALUE SPACES.
       77          V0PREM-DTVENCTO     PIC  X(010)      VALUE SPACES.
       77          V0PREM-DTQITBCO     PIC  X(010)      VALUE SPACES.
       77          V0PREM-NOME-RAZAO   PIC  X(040)      VALUE SPACES.
       77          V0PREM-CODPRODU     PIC S9(004)      VALUE +0 COMP.
       77          V0PREM-RAMO-SUSEP   PIC S9(004)      VALUE +0 COMP.
       77          V0PREM-SEGM-RAMO    PIC  X(001)      VALUE SPACES.
       77          V0PREM-NUM-PROPT    PIC S9(009)      VALUE +0 COMP.
       77          V0PREM-NUM-TITULO   PIC S9(013)      VALUE +0 COMP-3.
       77          V0PREM-CANAL-VENDA  PIC S9(004)      VALUE +0 COMP.
       77          V0PREM-NUM-DOC-COB  PIC S9(017)      VALUE +0 COMP-3.
       77          V0PREM-BCO-AVISO    PIC S9(004)      VALUE +0 COMP.
       77          V0PREM-AGE-AVISO    PIC S9(004)      VALUE +0 COMP.
       77          V0PREM-NUM-AVISO    PIC S9(009)      VALUE +0 COMP.
       77          V0PREM-DATA-AVISO   PIC  X(010)      VALUE SPACES.
       77          V0PREM-TIPO-PENDC   PIC  X(001)      VALUE SPACES.
       77          V0PREM-CODUSU       PIC  X(008)      VALUE SPACES.
       77          V0PREM-TIMESTAMP    PIC  X(026)      VALUE SPACES.

      *--* TABELA DE PRODUTO  (V0PRODUTO)
      *----------------------------------
      *
       77          V0PROD-CODPRODU     PIC S9(004)      VALUE +0 COMP.
       77          V0PROD-NUM-PROCS    PIC  X(025)      VALUE SPACES.

      *--* TABELA DE ENDOSSOS (V0ENDOSSO)
      *----------------------------------
      *
       77          V0ENDO-NUM-APOL     PIC S9(013)      VALUE +0 COMP-3.
       77          V0ENDO-NRENDOS      PIC S9(009)      VALUE +0 COMP.
       77          V0ENDO-FONTE        PIC S9(004)      VALUE +0 COMP.
       77          V0ENDO-NRPROPOS     PIC S9(009)      VALUE +0 COMP.
       77          V0ENDO-DATPRO       PIC  X(010)      VALUE SPACES.
       77          V0ENDO-BCORCAP      PIC S9(004)      VALUE +0 COMP.
       77          V0ENDO-AGERCAP      PIC S9(004)      VALUE +0 COMP.
       77          V0ENDO-TIPO-ENDS    PIC  X(001)      VALUE SPACES.
       77          V0ENDO-OCOR-ENDR    PIC S9(004)      VALUE +0 COMP.
      *
       77          V0ENDS-NUM-APOL     PIC S9(013)      VALUE +0 COMP-3.
       77          V0ENDS-NRENDOS      PIC S9(009)      VALUE +0 COMP.
       77          V0ENDS-DTEMIS       PIC  X(010)      VALUE SPACES.
       77          V0ENDS-TIPO-ENDO    PIC  X(001)      VALUE SPACES.

      *--* TABELA HISTORICO DE PARCELAS (V0HISTOPARC)
      *----------------------------------------------
      *
       77          V0HISP-NUM-APOL     PIC S9(013)      VALUE +0 COMP-3.
       77          V0HISP-NRENDOS      PIC S9(009)      VALUE +0 COMP.
       77          V0HISP-NRPARCEL     PIC S9(004)      VALUE +0 COMP.
       77          V0HISP-OCORHIST     PIC S9(004)      VALUE +0 COMP.
       77          V0HISP-OPERACAO     PIC S9(004)      VALUE +0 COMP.
       77          V0HISP-DTMOVTO      PIC  X(010)      VALUE SPACES.
       77          V0HISP-NRENDOCA     PIC S9(009)      VALUE +0 COMP.
       77          V0HISP-VLPRMTOT     PIC S9(013)V99   VALUE +0 COMP-3.

      *--* TABELA DE FONTES (V0FONTES)
      *-------------------------------
      *
       77          V0FONT-COD-FONTE    PIC S9(004)      VALUE +0 COMP.
       77          V0FONT-ESTADO       PIC  X(002)      VALUE SPACES.

      *--* TABELA APOLICE-AUTO (V0AUTOAPOL)
      *------------------------------------
      *
       77          V0AUTA-NUM-APOL     PIC S9(013)      VALUE +0 COMP-3.
       77          V0AUTA-NRENDOS      PIC S9(009)      VALUE +0 COMP.
       77          V0AUTA-NRITEM       PIC S9(009)      VALUE +0 COMP.
       77          V0AUTA-NRPROP-C     PIC S9(015)      VALUE +0 COMP-3.
       77          V0AUTA-DTINIVIG     PIC  X(010)      VALUE SPACES.
       77          V0AUTA-DTTERVIG     PIC  X(010)      VALUE SPACES.
       77          V0AUTA-SITUACAO     PIC  X(001)      VALUE SPACES.

      *--* TABELA HIST DE PROPOSTA CONV AUTO (AU055)
      *---------------------------------------------
      *
       77          AU055-NUMPROP-VC    PIC S9(015)      VALUE +0 COMP-3.
       77          AU055-OCORR-HIST    PIC S9(004)      VALUE +0 COMP.
       77          AU055-DTH-OPERAC    PIC  X(010)      VALUE SPACES.
       77          AU055-IND-OPERAC    PIC  X(003)      VALUE SPACES.

      *--* TABELA PROPOSTA_AUTO (V0AUTOPROP)
      *-------------------------------------
      *
       77          V0AUPR-FONTE        PIC S9(004)      VALUE +0 COMP.
       77          V0AUPR-NRPROPT      PIC S9(009)      VALUE +0 COMP.
       77          V0AUPR-NRITEM       PIC S9(009)      VALUE +0 COMP.
       77          V0AUPR-DTH-EFETV    PIC  X(026)      VALUE SPACES.

      *--* TABELA DE APOLICES (V0APOLICE)
      *----------------------------------
      *
       77          V0APOL-NUM-APOL     PIC S9(013)      VALUE +0 COMP-3.
       77          V0APOL-CODCLIEN     PIC S9(009)      VALUE +0 COMP.
       77          V0APOL-NUM-BILH     PIC S9(015)      VALUE +0 COMP-3.
       77          V0APOL-TIPSGU       PIC  X(001)      VALUE SPACES.
       77          V0APOL-TPCOSCED     PIC  X(001)      VALUE SPACES.
       77          V0APOL-QTCOSCED     PIC S9(004)      VALUE +0 COMP.
       77          V0APOL-PCTCED       PIC S9(004)V9(5) VALUE +0 COMP-3.

      *--* TABELA DE CLIENTES (V0CLIENTE)
      *----------------------------------
      *
       77          V0CLIE-CODCLIEN     PIC S9(009)      VALUE +0 COMP.
       77          V0CLIE-CGC-CPF      PIC S9(015)      VALUE +0 COMP-3.
       77          V0CLIE-TIP-PESS     PIC  X(001)      VALUE SPACES.
       77          V0CLIE-NOME-RAZAO   PIC  X(040)      VALUE SPACES.

      *--* TABELA EF-APOLICE (EF063)
      *-----------------------------
      *
       77          EF063-NUM-APOL      PIC S9(013)      VALUE +0 COMP-3.
       77          EF063-NUM-CONTR     PIC S9(015)      VALUE +0 COMP-3.

      *--* TABELA EF-CONTRATO (EF050)
      *------------------------------
      *
       77          EF050-NUM-CONTR     PIC S9(015)      VALUE +0 COMP-3.
       77          EF050-COD-PRODU     PIC S9(004)      VALUE +0 COMP.

      *--* TABELA EF-PROD-ACESSORIO (EF148)
      *------------------------------------
      *
       77          EF148-NR-CONT-APO   PIC S9(015)      VALUE +0 COMP-3.
       77          EF148-COD-PRODU     PIC S9(004)      VALUE +0 COMP.
       77          EF148-COD-COBERT    PIC S9(004)      VALUE +0 COMP.
       77          EF148-RAMO-CONTB    PIC S9(004)      VALUE +0 COMP.
       77          EF148-PRODU-ACS     PIC S9(004)      VALUE +0 COMP.
       77          EF148-COBER-ACS     PIC S9(004)      VALUE +0 COMP.
       77          EF148-NUM-APOL      PIC S9(013)      VALUE +0 COMP-3.
C10158 77          EF148-DTH-INIVIG    PIC  X(010)      VALUE SPACES.

      *--* TABELA EF-ENDOSSO (EF053)
      *-----------------------------
      *
       77          EF053-NUM-ENDOS     PIC S9(009)      VALUE +0 COMP.
       77          EF053-NUM-CONTR     PIC S9(015)      VALUE +0 COMP-3.
       77          EF053-TIPO-ENDS     PIC  X(001)      VALUE SPACES.
       77          EF053-SEQC-OPER     PIC S9(004)      VALUE +0 COMP.

      *--* TABELA EF-FATURAS-ENDOSSO (EF054)
      *-------------------------------------
      *
       77          EF054-NR-CONT-FAT   PIC S9(015)      VALUE +0 COMP-3.
       77          EF054-NUM-ENDOS     PIC S9(009)      VALUE +0 COMP.
       77          EF054-SEQC-OP-FAT   PIC S9(004)      VALUE +0 COMP.

      *--* TABELA EF-FATURA (EF056)
      *----------------------------
      *
       77          EF056-NUM-CONTR     PIC S9(015)      VALUE +0 COMP-3.
       77          EF056-SEQC-OPER     PIC S9(004)      VALUE +0 COMP.
       77          EF056-COD-PRODU     PIC S9(004)      VALUE +0 COMP.
       77          EF056-DTH-REFER     PIC  X(010)      VALUE SPACES.
       77          EF056-DTH-VENCT     PIC  X(010)      VALUE SPACES.
       77          EF056-STA-FATUR     PIC  X(001)      VALUE SPACES.

      *--* TABELA EF-PREMIOS-FATURA (EF060)
      *------------------------------------
      *
       77          EF060-NR-CONT-SEG   PIC S9(015)      VALUE +0 COMP-3.
       77          EF060-NR-CONT-APO   PIC S9(015)      VALUE +0 COMP-3.
       77          EF060-SEQC-OP-FAT   PIC S9(004)      VALUE +0 COMP.
T26640 77          EF060-SEQC-PREMIO   PIC S9(009)      VALUE +0 COMP.
       77          EF060-NUM-ENDOS     PIC S9(009)      VALUE +0 COMP.

      *--* TABELA EF-PREMIO-EMITIDO (EF066)
      *------------------------------------
      *
       77          EF066-NR-CONT-SEG   PIC S9(015)      VALUE +0 COMP-3.
T26640 77          EF066-SEQC-PREMIO   PIC S9(009)      VALUE +0 COMP.

      *--* TABELA PRODUTOS-VG (V0PRODUTOSVG)
      *-------------------------------------
      *
       77          V0PDVG-NUM-APOL     PIC S9(013)      VALUE +0 COMP-3.
       77          V0PDVG-CODSUBES     PIC S9(004)      VALUE +0 COMP.
       77          V0PDVG-ORIG-PRODU   PIC  X(010)      VALUE SPACES.

      *--* TABELA HIST-CONT-PARCELVA (HTCTPBVA)
      *----------------------------------------
      *
       77          HTCPVA-NUM-CERT     PIC S9(015)      VALUE +0 COMP-3.
       77          HTCPVA-OCR-HIST     PIC S9(004)      VALUE +0 COMP.
       77          HTCPVA-NUM-APOL     PIC S9(013)      VALUE +0 COMP-3.
       77          HTCPVA-COD-SUBG     PIC S9(004)      VALUE +0 COMP.
       77          HTCPVA-COD-FONT     PIC S9(004)      VALUE +0 COMP.
       77          HTCPVA-NUM-ENDS     PIC S9(009)      VALUE +0 COMP.
       77          HTCPVA-COD-OPER     PIC S9(004)      VALUE +0 COMP.
       77          HTCPVA-SITUACAO     PIC  X(001)      VALUE SPACES.

      *--* TABELA HIS-COBER-PROPOST (COBPRPVA)
      *---------------------------------------
      *
       77          CPRPVA-NUM-CERT     PIC S9(015)      VALUE +0 COMP-3.
       77          CPRPVA-OCR-HIST     PIC S9(004)      VALUE +0 COMP.
       77          CPRPVA-DTINIVIG     PIC  X(010)      VALUE SPACES.
       77          CPRPVA-DTTERVIG     PIC  X(010)      VALUE SPACES.
       77          CPRPVA-QT-VIDAS     PIC S9(009)      VALUE +0 COMP.
       77          CPRPVA-IMPSEGUR     PIC S9(013)V9(2) VALUE +0 COMP-3.

      *--* TABELA DE FATURAS (V0FATURAS)
      *---------------------------------
      *
       77          V0FATR-NUM-APOL     PIC S9(013)      VALUE +0 COMP-3.
       77          V0FATR-COD-SUBG     PIC S9(004)      VALUE +0 COMP.
       77          V0FATR-NR-FATUR     PIC S9(009)      VALUE +0 COMP.
       77          V0FATR-COD-OPER     PIC S9(004)      VALUE +0 COMP.
       77          V0FATR-TIP-ENDS     PIC  X(001)      VALUE SPACES.
       77          V0FATR-NUM-ENDS     PIC S9(009)      VALUE +0 COMP.
       77          V0FATR-DTINIVIG     PIC  X(010)      VALUE SPACES.
       77          V0FATR-DTTERVIG     PIC  X(010)      VALUE SPACES.
       77          V0FATR-SIT-REGT     PIC  X(001)      VALUE SPACES.

      *--* TABELA DE FATURAS TOTAIS (V0FATURASTOT)
      *-------------------------------------------
      *
       77          V0FTOT-NUM-APOL     PIC S9(013)      VALUE +0 COMP-3.
       77          V0FTOT-COD-SUBG     PIC S9(004)      VALUE +0 COMP.
       77          V0FTOT-NR-FATUR     PIC S9(009)      VALUE +0 COMP.
       77          V0FTOT-COD-OPER     PIC S9(004)      VALUE +0 COMP.
       77          V0FTOT-QTVDA-VG     PIC S9(009)      VALUE +0 COMP.
       77          V0FTOT-QTVDA-AP     PIC S9(009)      VALUE +0 COMP.
       77          V0FTOT-SIT-REGT     PIC  X(001)      VALUE SPACES.

      *--* TABELA SEGURADOS-VGAP (V0SEGURAVG)
      *--------------------------------------
      *
       77          V0SGVG-NUM-APOL     PIC S9(013)      VALUE +0 COMP-3.
       77          V0SGVG-COD-SUBG     PIC S9(004)      VALUE +0 COMP.
       77          V0SGVG-NR-CERTF     PIC S9(015)      VALUE +0 COMP-3.
       77          V0SGVG-TIPO-SEG     PIC  X(001)      VALUE SPACES.
       77          V0SGVG-NUM-ITEM     PIC S9(009)      VALUE +0 COMP.
       77          V0SGVG-COD-CLIE     PIC S9(009)      VALUE +0 COMP.
       77          V0SGVG-COD-FONT     PIC S9(004)      VALUE +0 COMP.
       77          V0SGVG-OCR-ENDR     PIC S9(004)      VALUE +0 COMP.
       77          V0SGVG-SITUACAO     PIC  X(001)      VALUE SPACES.

      *--* TABELA DE AGENTE FINANCEIRO (V0AGENTE-FINANC)
      *-------------------------------------------------
      *
       77          V0AGFI-NUM-APOL     PIC S9(013)      VALUE +0 COMP-3.
       77          V0AGFI-COD-AGEN     PIC S9(009)      VALUE +0 COMP.
       77          V0AGFI-COD-SUBG     PIC S9(004)      VALUE +0 COMP.
       77          V0AGFI-CODCLIEN     PIC S9(009)      VALUE +0 COMP.
       77          V0AGFI-OCOR-END     PIC S9(004)      VALUE +0 COMP.

      *--* TABELA DE TOMADOR (V0TOMADOR)
      *---------------------------------
      *
       77          V0TOMD-FONTE        PIC S9(004)      VALUE +0 COMP.
       77          V0TOMD-NRPROPOS     PIC S9(009)      VALUE +0 COMP.
       77          V0TOMD-CODCLIEN     PIC S9(009)      VALUE +0 COMP.
       77          V0TOMD-OCOR-END     PIC S9(004)      VALUE +0 COMP.

      *--* TABELA DE ENDERECOS (V0ENDERECOS)
      *-------------------------------------
      *
       77          V0ENDR-CODCLIEN     PIC S9(009)      VALUE +0 COMP.
       77          V0ENDR-COD-ENDR     PIC S9(004)      VALUE +0 COMP.
       77          V0ENDR-OCOR-ENDR    PIC S9(004)      VALUE +0 COMP.
       77          V0ENDR-SIGLA-UF     PIC  X(002)      VALUE SPACES.

      *--* TABELA DE AGENCIAS (V0AGENCIAS)
      *-----------------------------------
      *
       77          V0AGEN-COD-BANCO    PIC S9(004)      VALUE +0 COMP.
       77          V0AGEN-COD-AGENC    PIC S9(004)      VALUE +0 COMP.
       77          V0AGEN-ESTADO       PIC  X(002)      VALUE SPACES.

      *--* TABELA DE PRODUTORES (V0PRODUTOR)
      *-------------------------------------
      *
       77          V0PRDT-COD-PRDT     PIC S9(009)      VALUE +0 COMP.
       77          V0PRDT-TIP-PRDT     PIC  X(001)      VALUE SPACES.
       77          V0PRDT-CGC-CPF      PIC S9(015)      VALUE +0 COMP-3.

      *--* TABELA APOLICE-CORRETOR (V0APOLCORRET)
      *------------------------------------------
      *
       77          V0ACOR-NUM-APOL     PIC S9(013)      VALUE +0 COMP-3.
       77          V0ACOR-RAMOFR       PIC S9(004)      VALUE +0 COMP.
       77          V0ACOR-MODALIFR     PIC S9(004)      VALUE +0 COMP.
       77          V0ACOR-CODCORR      PIC S9(009)      VALUE +0 COMP.
       77          V0ACOR-CODSUBES     PIC S9(004)      VALUE +0 COMP.
       77          V0ACOR-DTINIVIG     PIC  X(010)      VALUE SPACES.
       77          V0ACOR-DTTERVIG     PIC  X(010)      VALUE SPACES.
       77          V0ACOR-PCPARCOR     PIC S9(003)V99   VALUE +0 COMP-3.
       77          V0ACOR-PCCOMCOR     PIC S9(003)V99   VALUE +0 COMP-3.
       77          V0ACOR-TIPCOM       PIC  X(001)      VALUE SPACES.
       77          V0ACOR-INDCRT       PIC  X(001)      VALUE SPACES.

      *--* TABELA APOLICE COBERTURAS (V0COBERAPOL)
      *-------------------------------------------
      *
       77          V0COBA-NUM-APOL     PIC S9(013)      VALUE +0 COMP-3.
       77          V0COBA-NRENDOS      PIC S9(009)      VALUE +0 COMP.
       77          V0COBA-NUM-ITEM     PIC S9(009)      VALUE +0 COMP.
       77          V0COBA-OCORHIST     PIC S9(004)      VALUE +0 COMP.
       77          V0COBA-RAMOFR       PIC S9(004)      VALUE +0 COMP.
       77          V0COBA-MODALIFR     PIC S9(004)      VALUE +0 COMP.
       77          V0COBA-COD-COBER    PIC S9(004)      VALUE +0 COMP.
       77          V0COBA-IMP-SEG-IX   PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0COBA-PRM-TAR-IX   PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          V0COBA-IMP-SEG-VR   PIC S9(013)V99   VALUE +0 COMP-3.
       77          V0COBA-PRM-TAR-VR   PIC S9(010)V9(5) VALUE +0 COMP-3.

      *--* TABELA COTACAO DE MOEDAS (V0COTACAO)
      *----------------------------------------
      *
       77          V0COTA-CODUNIMO     PIC S9(004)      VALUE +0 COMP.
       77          V0COTA-DTINIVIG     PIC  X(010)      VALUE SPACES.
       77          V0COTA-DTTERVIG     PIC  X(010)      VALUE SPACES.
       77          V0COTA-VALCPR       PIC S9(006)V9(9) VALUE +0 COMP-3.
       77          V0COTA-VALVEND      PIC S9(006)V9(9) VALUE +0 COMP-3.

      *--* TABELA GE-ENDOS-COSSEG-COBER (GE397)
      *----------------------------------------
      *
       77          GE397-NUM-APOL      PIC S9(013)      VALUE +0 COMP-3.
       77          GE397-NUM-ENDS      PIC S9(009)      VALUE +0 COMP.
       77          GE397-RAMO-CBT      PIC S9(004)      VALUE +0 COMP.
       77          GE397-COD-COBT      PIC S9(004)      VALUE +0 COMP.
       77          GE397-IMP-SEG-VR    PIC S9(013)V99   VALUE +0 COMP-3.
       77          GE397-PRM-TAR-VR    PIC S9(010)V9(5) VALUE +0 COMP-3.

      *--* TABELA DE COSSEGURO (V0APOLCOSCED)
      *---------------------------------------
      *
       77          V0APCD-NUM-APOL     PIC S9(013)      VALUE +0 COMP-3.
       77          V0APCD-COD-COSS     PIC S9(004)      VALUE +0 COMP.
       77          V0APCD-PCPARTIC     PIC S9(004)V9(5) VALUE +0 COMP-3.
       77          V0APCD-PCCOMCOS     PIC S9(003)V99   VALUE +0 COMP-3.
       77          V0APCD-DTINIVIG     PIC  X(010)      VALUE SPACES.
       77          V0APCD-DTTERVIG     PIC  X(010)      VALUE SPACES.

      *--* TABELA GE-ENDOS-RAMO-VLR-COSSEG (GE399)
      *-------------------------------------------
      *
       77          GE399-NUM-APOL      PIC S9(013)      VALUE +0 COMP-3.
       77          GE399-NUM-ENDS      PIC S9(009)      VALUE +0 COMP.
       77          GE399-RAMO-CBT      PIC S9(004)      VALUE +0 COMP.
       77          GE399-COD-COSG      PIC S9(004)      VALUE +0 COMP.
       77          GE399-IMP-SEG-CD    PIC S9(013)V99   VALUE +0 COMP-3.
       77          GE399-PCT-RMO-IS    PIC S9(004)V9(9) VALUE +0 COMP-3.
       77          GE399-PRM-TAR-CD    PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          GE399-PCT-RMO-PR    PIC S9(004)V9(9) VALUE +0 COMP-3.
       77          GE399-COM-CSG-CD    PIC S9(010)V9(5) VALUE +0 COMP-3.
       77          GE399-PCT-COM-RM    PIC S9(004)V9(9) VALUE +0 COMP-3.

105223*--* TABELA LT_MOV_PROPOSTA (LTMVPROP)
=     *-------------------------------------
=     *
=      77          LTMVPROP-NUM-TITULO    PIC S9(013)V  VALUE +0 COMP-3.
105223
112349*--* TABELA AU_PROP_CONV_VC (AU057)
=     *----------------------------------
=     *
=      77          AU057-NUM-PROPOSTA-VC  PIC S9(015)V  VALUE +0 COMP-3.
112349
136081*--* TABELA DE APOLICE COBRANCA (APOLCOBR)
=     *-----------------------------------------
=     *
=      77          APOLCOBR-NUM-APOL   PIC S9(013)      VALUE +0 COMP-3.
=      77          APOLCOBR-NUM-ENDS   PIC S9(009)      VALUE +0 COMP.
=      77          APOLCOBR-COD-PROD   PIC S9(004)      VALUE +0 COMP.
=      77          APOLCOBR-NUM-MATR   PIC S9(015)      VALUE +0 COMP-3.
=
=     *--* TABELA DE BILHETES (BILHETE)
=     *--------------------------------
=     *
=      77          BILHETE-NUM-BILH    PIC S9(015)      VALUE +0 COMP-3.
=      77          BILHETE-NUM-APOL    PIC S9(013)      VALUE +0 COMP-3.
=      77          BILHETE-NUM-MATR    PIC S9(015)      VALUE +0 COMP-3.
=
=     *--* TABELA DE FUNCIONARIOS_CEF (FUNCICEF)
=     *-----------------------------------------
=     *
=      77          FUNCICEF-NUM-MATR   PIC S9(015)      VALUE +0 COMP-3.
=      77          FUNCICEF-NOM-FUNC   PIC  X(040)      VALUE SPACES.
=      77          FUNCICEF-NUM-CPF    PIC S9(011)      VALUE +0 COMP-3.
136081*
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
           EXEC  SQL   END  DECLARE  SECTION   END-EXEC.
      *
362429     EXEC  SQL   INCLUDE       SISTEMAS  END-EXEC.
      *
           EXEC  SQL   INCLUDE       BILHETE   END-EXEC.
      *
           EXEC  SQL   INCLUDE       PROPFID   END-EXEC.
      *
           EXEC  SQL   INCLUDE       SQLCA     END-EXEC.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       01          WS-ARQUIVOS.
      *
285991*--*  ARQUIVO PREMIT  -  TAMANHO - 765
      *
         05        REGT-PREMIT.
      *
           10      EMI-SEQ             PIC  9(010).
           10      EMI-COD-CIA         PIC  9(005).
           10      EMI-NUM-PROC        PIC  X(025).
428303     10      EMI-DT-BASE         PIC  9(006).
=          10      EMI-DT-BASE-R       REDEFINES        EMI-DT-BASE.
=            15    EMI-DT-BASE-AA      PIC  9(004).
428303       15    EMI-DT-BASE-MM      PIC  9(002).
           10      EMI-TIPO-MOV        PIC  9(003).
           10      EMI-UF-DEP          PIC  X(002).
           10      EMI-UF-RISCO        PIC  X(054).
           10      EMI-COD-RAMO        PIC  X(004).
           10      EMI-APOL-FIL        PIC  9(007).
           10      EMI-NUM-APOL        PIC  9(013).
           10      EMI-ENDS-FIL        PIC  9(011).
           10      EMI-NUM-END         PIC  9(009).
           10      EMI-NR-PROPT        PIC  X(020).
           10      EMI-NR-PROPT-R1     REDEFINES        EMI-NR-PROPT.
105223       15    EMI-PROP-FIL1       PIC  9(007).
=            15    EMI-COD-FONTE       PIC  9(004).
=            15    EMI-NUM-PROP        PIC  9(009).
=          10      EMI-NR-PROPT-R2     REDEFINES        EMI-NR-PROPT.
=            15    EMI-PROP-FIL2       PIC  9(009).
105223       15    EMI-NUM-PROP1       PIC  9(011).
           10      EMI-DT-PROPT        PIC  9(008).
           10      EMI-CPF-SEG         PIC  9(014).
           10      EMI-QTD-SEG         PIC  9(004).
           10      EMI-CPF-TOM         PIC  9(014).
           10      EMI-QTD-TOM         PIC  9(004).
           10      EMI-DT-EMIS         PIC  9(008).
           10      EMI-DTINIVIG        PIC  9(008).
           10      EMI-DTFIMVIG        PIC  9(008).
           10      EMI-PR-EMIT         PIC  +9(012).99.
           10      EMI-PR-COS-CED      PIC  +9(012).99.
           10      EMI-AD-FRAC         PIC  +9(012).99.
           10      EMI-CUST-APOL       PIC  +9(012).99.
           10      EMI-VLR-IOF         PIC  +9(012).99.
           10      EMI-VLR-COMIS       PIC  +9(012).99.
           10      EMI-COMIS-COSS      PIC  +9(012).99.
           10      EMI-PRO-LAB         PIC  +9(012).99.
           10      EMI-CPF-ESTIP       PIC  9(014).
           10      EMI-IMP-SEG         PIC  +9(012).99.
117159     10      EMI-CERTIFIC        PIC  X(020).
=          10      EMI-CERTIFIC-R      REDEFINES        EMI-CERTIFIC.
=            15    EMI-CERT-FIL        PIC  9(005).
=            15    EMI-NUM-CERTIF      PIC  9(015).
=          10      EMI-CPF-SUBEST      PIC  9(014).
=          10      EMI-CPF-PARCEIRO    PIC  9(014).
=          10      EMI-COD-PRODU       PIC  9(004).
=          10      EMI-DTINIVIG-CRTF   PIC  9(008).
=          10      EMI-DTTERVIG-CRTF   PIC  9(008).
=          10      EMI-ORIGEM-REG      PIC  X(008).
136071     10      EMI-NUM-PARCEL      PIC  9(004).
=          10      EMI-FIL-INDICA      PIC  X(070).
=          10      EMI-FIL-INDC-R      REDEFINES        EMI-FIL-INDICA.
136071       15    EMI-CANAL-VNDA      PIC  9(004).
136081       15    EMI-NUM-MATRIC      PIC  9(015).
=            15    EMI-NOME-AGENC      PIC  X(040).
136081       15    EMI-NUM-CPF         PIC  9(011).
139415     10      EMI-VLPRM-TARIFA    PIC +9(012).99.
=          10      EMI-TIPO-REDE       PIC  X(002).
146163     10      EMI-TIPO-RENOV      PIC  X(002).
139415     10      EMI-PCTAR-BALCAO    PIC  9999.99.
=          10      EMI-VLTAR-BALCAO    PIC +9(012).99.
141119     10      EMI-PCCOM-INDICD    PIC  9999.99.
141119     10      EMI-VLCOM-INDICD    PIC +9(012).99.
235637     10      EMI-DATA-AVERB      PIC  9(008).
=          10      EMI-DATA-INCLU      PIC  9(008).
=          10      EMI-DTFAT-PC01      PIC  9(008).
=          10      EMI-DATA-MOVTO      PIC  9(008).
=          10      EMI-SIT-CERTIF      PIC  X(001).
=          10      EMI-DES-SITUAC      PIC  X(045).
=          10      EMI-DT-EMIS-END     PIC  9(008).
=          10      EMI-INI-VIG-END     PIC  9(008).
=          10      EMI-FIM-VIG-END     PIC  9(008).
=          10      EMI-RAMO-COBERT     PIC  9(004).
=          10      EMI-MODL-COBERT     PIC  9(004).
=          10      EMI-PCT-QUOTA-R     PIC  9999.99999.
=          10      EMI-COM-QUOTA-R     PIC  9999.99999.
=          10      EMI-COD-EMPR        PIC  9999.
235637     10      EMI-VLR-AGENC       PIC +9(012).99.

235637*    10      EMI-DATA-EMISS      PIC  9(008).
=     *    10      EMI-DATA-CANCL      PIC  9(008).
=     *    10      EMI-COD-OPERAC      PIC  9(004).
=     *    10      EMI-DESC-OPERC      PIC  X(045).
235637*    10      EMI-PROP-SIVPF      PIC  9(015).
285991     10      EMI-TIPO-OPER       PIC  9(004).
      *
285991*--*  ARQUIVO PREMCED  -  TAMANHO - 168
      *
         05        REGT-PREMCED.
      *
           10      CED-SEQ             PIC  9(010).
           10      CED-COD-CIA         PIC  9(005).
           10      CED-COD-COSS        PIC  9(005).
           10      CED-NUM-PROC        PIC  X(025).
           10      CED-DT-BASE         PIC  9(006).
428303     10      CED-DT-BASE         PIC  9(006).
=          10      CED-DT-BASE-R       REDEFINES     CED-DT-BASE.
=            15    CED-DT-BASE-AA      PIC  9(004).
428303       15    CED-DT-BASE-MM      PIC  9(002).
           10      CED-TIPO-MOV        PIC  9(003).
           10      CED-APOL-FIL        PIC  9(007).
           10      CED-NUM-APOL        PIC  9(013).
103462     10      CED-NUM-ENDOSSO     PIC  X(020).
=          10      CED-NUM-ENDOSSO-R   REDEFINES      CED-NUM-ENDOSSO.
=            15    CED-ENDS-FIL        PIC  9(011).
103462       15    CED-NUM-END         PIC  9(009).
           10      CED-NR-PROPT        PIC  X(020).
           10      CED-NR-PROPT-R      REDEFINES      CED-NR-PROPT.
             15    CED-PROP-FIL        PIC  9(007).
             15    CED-COD-FONTE       PIC  9(004).
             15    CED-NUM-PROP        PIC  9(009).
105223     10      CED-NR-PROPT-R2     REDEFINES      CED-NR-PROPT.
=            15    CED-PROP-FIL2       PIC  9(009).
=            15    CED-NUM-PROP1       PIC  9(011).
           10      CED-PR-COS-CED      PIC  -(012)9.99.
           10      CED-COMIS-COSS      PIC  -(012)9.99.
103462     10      CED-COD-PRODU       PIC  9(004).
119167     10      CED-DT-EMIS         PIC  9(008).
188334     10      CED-COD-EMPR        PIC  9999.
285991     10      CED-TIPO-OPER       PIC  9999.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *                 DEFINICAO DAS TABELAS INTERNAS                 *
      *----------------------------------------------------------------*
      *
       01          WS-TABELAS.
      *
         05        WTAB-SIGLA-UF       PIC  X(054)      VALUE SPACES.
         05        WTAB-SIGLA-UF-R     REDEFINES        WTAB-SIGLA-UF
                                       OCCURS 27        TIMES.
           10      WTAB-UF             PIC  X(002).
      *
         05        WTABL-ESTADOS       PIC  X(054)      VALUE
              'ACALAMAPBACEDFESGOMAMGMSMTPAPBPEPIPRRJRNRORRRSSCSESPTO'.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       01          AREA-DE-WORK.
      *
         05        EMI-STATUS          PIC  9(002)      VALUE ZEROS.
         05        CED-STATUS          PIC  9(002)      VALUE ZEROS.
      *
      *--* CHAVES PARA CONTROLE DE LEITURA
      *
         05        WFIM-V0RELATORIO    PIC  X(001)      VALUE SPACES.
         05        WFIM-V0PREMIOS      PIC  X(001)      VALUE SPACES.
         05        WFIM-V0ENDERECOS    PIC  X(001)      VALUE SPACES.
         05        WFIM-COSSEG-CED     PIC  X(001)      VALUE SPACES.
         05        WFIM-V0HTCPVA       PIC  X(001)      VALUE SPACES.
      *
         05        WTEM-APOL-EF        PIC  X(001)      VALUE SPACES.
      *
         05        WIND                PIC  9(003)      VALUE ZEROS.
         05        AC-COUNT            PIC  9(009)      VALUE ZEROS.
         05        AC-L-V0PREMIOS      PIC  9(009)      VALUE ZEROS.
         05        AC-G-PREMIT         PIC  9(009)      VALUE ZEROS.
         05        AC-G-PREMCED        PIC  9(009)      VALUE ZEROS.
         05        WS-SEQ-PREMIT       PIC  9(009)      VALUE ZEROS.
         05        WS-SEQ-PREMCED      PIC  9(009)      VALUE ZEROS.
      *
         05        WRAMO-SUSEP         PIC  9(004)       VALUE ZEROS.
         05        WRMO-SUSEP-R        REDEFINES         WRAMO-SUSEP.
           10      WCOD-GRUPO          PIC  9(002).
           10      WCOD-RAMO           PIC  9(002).
      *
      *--*  AREA DE CHAVE DE QUEBRA
      *
         05        WCOD-CLIEN-SEG      PIC S9(009)      VALUE +0 COMP.
         05        WCOD-CLIEN-EST      PIC S9(009)      VALUE +0 COMP.
         05        WCOD-CLIEN-TOM      PIC S9(009)      VALUE +0 COMP.
      *
         05        WCOD-ORGAO-ANT      PIC S9(004)      VALUE +0 COMP.
         05        WTIP-SEGUR-ANT      PIC  X(001)      VALUE SPACES.
         05        WTIP-COSCED-ANT     PIC  X(001)      VALUE SPACES.
      *
         05        WS-CHAVE-ANT.
           10      CHVANT-RAMO-SUSEP   PIC S9(004)      VALUE +0 COMP.
           10      CHVANT-TIPO-ENDS    PIC  X(001)      VALUE SPACES.
           10      CHVANT-TIPO-MOVT    PIC  X(001)      VALUE SPACES.
           10      CHVANT-DATA-MOVT    PIC  X(010)      VALUE SPACES.
           10      CHVANT-NUM-APOL     PIC S9(013)      VALUE +0 COMP-3.
           10      CHVANT-NRENDOCA     PIC S9(009)      VALUE +0 COMP.
           10      CHVANT-NRENDOS      PIC S9(009)      VALUE +0 COMP.
           10      CHVANT-TIPO-OPER    PIC S9(004)      VALUE +0 COMP.
      *
136071     10      CHVANT-CODPRODU     PIC S9(004)      VALUE +0 COMP.
=          10      CHVANT-NRPARCEL     PIC S9(004)      VALUE +0 COMP.
136071     10      CHVANT-CANAL-VD     PIC S9(004)      VALUE +0 COMP.
136081     10      CHVANT-NUM-BILH     PIC S9(015)      VALUE +0 COMP-3.
146365     10      CHVANT-DAT-EMIS     PIC  X(010)      VALUE SPACES.
146365     10      CHVANT-OPERACAO     PIC S9(004)      VALUE +0 COMP.
      *
148834     10      CHVANT-RAMO-CBT     PIC S9(004)      VALUE +0 COMP.
=          10      CHVANT-MODL-CBT     PIC S9(004)      VALUE +0 COMP.
=          10      CHVANT-DTINIVIG     PIC  X(010)      VALUE SPACES.
148834     10      CHVANT-DTTERVIG     PIC  X(010)      VALUE SPACES.
      *
      *--*  AREA DE ACUMULADORES
      *
         05        WS-ACUMULADOR.
           10      ACC-IMP-SEGR-T      PIC S9(013)V99   VALUE +0 COMP-3.
           10      ACC-VLPRMTAR-T      PIC S9(013)V99   VALUE +0 COMP-3.
           10      ACC-VLDESCON-T      PIC S9(013)V99   VALUE +0 COMP-3.
           10      ACC-VLPRMLIQ-T      PIC S9(013)V99   VALUE +0 COMP-3.
           10      ACC-VLADIFRA-T      PIC S9(013)V99   VALUE +0 COMP-3.
           10      ACC-VLCUSEMI-T      PIC S9(013)V99   VALUE +0 COMP-3.
           10      ACC-VLIOCC-T        PIC S9(013)V99   VALUE +0 COMP-3.
           10      ACC-VLPRMTOT-T      PIC S9(013)V99   VALUE +0 COMP-3.
           10      ACC-VLCOMIS-T       PIC S9(013)V99   VALUE +0 COMP-3.
           10      ACC-VLADMN-T        PIC S9(013)V99   VALUE +0 COMP-3.
           10      ACC-VLAGENC-T       PIC S9(013)V99   VALUE +0 COMP-3.
           10      ACC-IMP-SEGR-C      PIC S9(013)V99   VALUE +0 COMP-3.
           10      ACC-VLPRMTAR-C      PIC S9(013)V99   VALUE +0 COMP-3.
           10      ACC-VLDESCON-C      PIC S9(013)V99   VALUE +0 COMP-3.
           10      ACC-VLPRMLIQ-C      PIC S9(013)V99   VALUE +0 COMP-3.
           10      ACC-VLCOMIS-C       PIC S9(013)V99   VALUE +0 COMP-3.
148834     10      ACC-VLPRMTAR-L      PIC S9(013)V99   VALUE +0 COMP-3.
148834     10      ACC-VLPRMTAR-R      PIC S9(013)V99   VALUE +0 COMP-3.
      *
141119*--* AREA AUXILIAR DE CALCULO E MOVIMENTACAO
=     *
=        05        WTIP-REDE-AUX       PIC  X(002)      VALUE SPACES.
141119   05        WTIP-REDE-BALC      PIC  X(002)      VALUE SPACES.
146163   05        WTIP-RENOV-AUX      PIC  X(002)      VALUE SPACES.
146163   05        WTIP-RENOV-BALC     PIC  X(002)      VALUE SPACES.
141119   05        WPCT-TARF-BALC      PIC S9(003)V99   VALUE +0 COMP-3.
=        05        WVLR-TARF-BALC      PIC S9(013)V99   VALUE +0 COMP-3.
=        05        WPCT-COMS-INDC      PIC S9(003)V99   VALUE +0 COMP-3.
=        05        WVLR-COMS-INDC      PIC S9(013)V99   VALUE +0 COMP-3.
141119*
         05        WS-VLPRMTAR-C       PIC S9(013)V99   VALUE +0 COMP-3.
         05        WS-VLPRMLIQ-C       PIC S9(013)V99   VALUE +0 COMP-3.
         05        WS-VLCOMISS-C       PIC S9(013)V99   VALUE +0 COMP-3.
      *
148834   05        WPCT-QUOTA          PIC S9(004)V9(5) VALUE +0 COMP-3.
148834   05        WPCT-COM-QUOTA      PIC S9(004)V9(5) VALUE +0 COMP-3.
      *
      *--* AREA DE DATAS E HORA AUXILIARES
      *
         05        WDATA-AUX           PIC  X(010)      VALUE SPACES.
         05        WDATA-AUX-R         REDEFINES        WDATA-AUX.
           10      WDAT-AUX-ANO        PIC  9(004).
           10      FILLER              PIC  X(001).
           10      WDAT-AUX-MES        PIC  9(002).
           10      FILLER              PIC  X(001).
           10      WDAT-AUX-DIA        PIC  9(002).
      *
         05        WDATA-DBF           PIC  9(008)      VALUE ZEROS.
         05        WDATA-DBF-R         REDEFINES        WDATA-DBF.
           10      WDAT-DBF-ANO        PIC  9(004).
           10      WDAT-DBF-MES        PIC  9(002).
           10      WDAT-DBF-DIA        PIC  9(002).

136184*
=        05        WDATA-PROP          PIC  9(008)      VALUE ZEROS.
=        05        WDATA-PROP-R        REDEFINES        WDATA-PROP.
=          10      WDAT-PROP-ANO       PIC  9(004).
=          10      WDAT-PROP-MES       PIC  9(002).
136184     10      WDAT-PROP-DIA       PIC  9(002).

         05        WS-DATA-ACCEPT.
           10      WS-ANO-ACCEPT       PIC  9(002)      VALUE ZEROS.
           10      WS-MES-ACCEPT       PIC  9(002)      VALUE ZEROS.
           10      WS-DIA-ACCEPT       PIC  9(002)      VALUE ZEROS.

         05        WS-HORA-ACCEPT.
           10      WS-HOR-ACCEPT       PIC  9(002)      VALUE ZEROS.
           10      WS-MIN-ACCEPT       PIC  9(002)      VALUE ZEROS.
           10      WS-SEG-ACCEPT       PIC  9(002)      VALUE ZEROS.

         05        WS-DATA-CURR.
           10      WS-DIA-CURR         PIC  9(002)      VALUE ZEROS.
           10      FILLER              PIC  X(001)      VALUE SPACES.
           10      WS-MES-CURR         PIC  9(002)      VALUE ZEROS.
           10      FILLER              PIC  X(001)      VALUE SPACES.
           10      WS-ANO-CURR         PIC  9(004)      VALUE ZEROS.

         05        WS-HORA-CURR.
           10      WS-HOR-CURR         PIC  9(002)      VALUE ZEROS.
           10      FILLER              PIC  X(001)      VALUE SPACES.
           10      WS-MIN-CURR         PIC  9(002)      VALUE ZEROS.
           10      FILLER              PIC  X(001)      VALUE SPACES.
           10      WS-SEG-CURR         PIC  9(002)      VALUE ZEROS.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *          DEFINICAO DAS AREAS DE PARAMETROS DE LINKAGE          *
      *----------------------------------------------------------------*
148834*
=     *--* LINKAGE SUB-ROTINA RE0001S - CALCULA PERCENT. DO RESSEGURO
=     *
=      01       LKRE-PARM-RE0001S.
=     *
=        05     LKRE01-TIP-CALC        PIC  9(001).
=        05     LKRE01-NUM-APOL        PIC  9(013).
=        05     LKRE01-NRENDOS         PIC  9(009).
=        05     LKRE01-DTINIVIG        PIC  X(010).
=        05     LKRE01-PCTCED          PIC  9(004)V9(9).
=        05     LKRE01-RAMOFR          PIC  9(004).
=        05     LKRE01-MODALIFR        PIC  9(004).
=        05     LKRE01-PCTRSP          PIC  9(004)V9(9).
=        05     LKRE01-PCTRSP-IS       PIC  9(004)V9(9).
=        05     LKRE01-PCTCOT          PIC  9(004)V9(9).
=        05     LKRE01-PCTCTF          PIC  9(004)V9(9).
=        05     LKRE01-PCTDNO          PIC  9(004)V9(9).
=        05     LKRE01-PCTCOMCO        PIC  9(004)V9(9).
148834   05     LKRE01-PCTCOMRS        PIC  9(004)V9(9).
192299   05     LKRE01-DTCUTOFF        PIC  X(010)      VALUE SPACES.
192299   05     LKRE01-RECP-PSL        PIC  X(001)      VALUE SPACES.
192299   05     LKRE01-RECP-PSL        PIC  X(001)      VALUE SPACES.
212422   05     LKRE01-CONTR-RE        PIC  X(025)      VALUE SPACES.
148834   05     LKRE01-SQL-CODE        PIC  9(009).
=        05     LKRE01-RTN-CODE        PIC  9(002).
=        05     LKRE01-MENSAGEM        PIC  X(040).
148834*
      *--* AREA DE PARAMETRO DE ENTRADA E SAIDA DA SUB-ROTINA GE0009S
      *
       01       LKGE-PARM-GE0009S.
      *
         05     LKGE09-PARM-INPUT.
           10   LKGE09-NUM-APOLICE     PIC  9(13).
           10   LKGE09-NUM-ENDOSSO     PIC  9(09).
           10   LKGE09-CANAL-VENDA     PIC  9(04).
           10   LKGE09-COD-PRODUTO     PIC  9(05).
      *
         05     LKGE09-PARM-OUTPUT.
           10   LKGE09-TIP-REDE        PIC  X(02).
146163     10   LKGE09-TIP-RENOV       PIC  X(02).
           10   LKGE09-PCT-TARF        PIC  9(03)V99.
           10   LKGE09-SQL-CODE        PIC S9(09).
           10   LKGE09-RTN-CODE        PIC  9(02).
           10   LKGE09-MSG-ERRO        PIC  X(50).
      *
      *--* AREA DE PARAMETRO DE ENTRADA E SAIDA DA SUB-ROTINA GE0010S
      *
       01       LKGE-PARM-GE0010S.
      *
         05     LKGE10-PARM-INPUT.
           10   LKGE10-NUM-APOLICE     PIC  9(13).
           10   LKGE10-NUM-ENDOSSO     PIC  9(09).
           10   LKGE10-COD-RAMO-EM     PIC  9(04).
           10   LKGE10-COD-PRODUTO     PIC  9(05).
           10   LKGE10-CANAL-VENDA     PIC  9(04).
           10   LKGE10-DAT-INIVIGC     PIC  X(10).
           10   LKGE10-TIPO-FUNCIO     PIC  X(01).
      *
         05     LKGE10-PARM-OUTPUT.
           10   LKGE10-TIP-REDE        PIC  X(02).
146163     10   LKGE10-TIP-RENOV       PIC  X(02).
           10   LKGE10-PCT-COMS        PIC  9(03)V99.
           10   LKGE10-VLR-COMS        PIC  9(13)V99.
           10   LKGE10-SQL-CODE        PIC S9(09).
           10   LKGE10-RTN-CODE        PIC  9(02).
           10   LKGE10-MSG-ERRO        PIC  X(50).
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
      *--* AREAS DE DISPLAY DE ERRO SQL
      *
       01       WABEND.
      *
         05     FILLER                 PIC  X(010)      VALUE
               ' RG1866B  '.
         05     FILLER                 PIC  X(026)      VALUE
               ' *** ERRO EXEC SQL NUMERO '.
         05     WNR-EXEC-SQL           PIC  X(003)      VALUE '000'.
         05     FILLER                 PIC  X(013)      VALUE
               ' *** SQLCODE '.
         05     WSQLCODE               PIC  ZZZZZ999-   VALUE ZEROS.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       PROCEDURE                   DIVISION.
      *-------------------------------------
      *
       R0000-00-PRINCIPAL          SECTION.
      *------------------------------------
      *
           MOVE         '000'            TO               WNR-EXEC-SQL.
      *
           EXEC   SQL    WHENEVER        SQLWARNING
                         CONTINUE        END-EXEC.
      *
           EXEC   SQL    WHENEVER        SQLERROR
                         CONTINUE        END-EXEC.
      *
           EXEC   SQL    WHENEVER        NOT FOUND
                         CONTINUE        END-EXEC.
      *
           MOVE         '00/00/0000'        TO         WS-DATA-CURR.
      *
           ACCEPT        WS-DATA-ACCEPT     FROM       DATE.
           MOVE          WS-DIA-ACCEPT      TO         WS-DIA-CURR.
           MOVE          WS-MES-ACCEPT      TO         WS-MES-CURR.
           MOVE          WS-ANO-ACCEPT      TO         WS-ANO-CURR.
           ADD           2000               TO         WS-ANO-CURR.
      *
           MOVE         '00:00:00'          TO         WS-HORA-CURR.
      *
           ACCEPT        WS-HORA-ACCEPT     FROM       TIME.
           MOVE          WS-HOR-ACCEPT      TO         WS-HOR-CURR.
           MOVE          WS-MIN-ACCEPT      TO         WS-MIN-CURR.
           MOVE          WS-SEG-ACCEPT      TO         WS-SEG-CURR.
      *
           DISPLAY      'RG1866B - INICIO DE EXECUCAO (' WS-DATA-CURR
                                           ' - '       WS-HORA-CURR ')'.
      *
      *--*
      *
           OPEN          OUTPUT   PREMIT.
      *
           IF  EMI-STATUS  NOT   EQUAL   ZEROS
               DISPLAY  'R0000 - ERRO NO OPEN DO ARQ PREMIT'
               DISPLAY  'STATUS  - '  EMI-STATUS
               GO   TO   R9999-00-ROT-ERRO.
      *
           OPEN          OUTPUT   PREMCED.
      *
           IF  CED-STATUS  NOT   EQUAL   ZEROS
               DISPLAY  'R0000 - ERRO NO OPEN DO ARQ PREMCED'
               DISPLAY  'STATUS  - '  CED-STATUS
               GO   TO   R9999-00-ROT-ERRO.
      *
362429     PERFORM       R0100-00-SELECT-SISTEMAS.
      *
           PERFORM       R0200-00-SELECT-V0RELATORIO.
      *
           IF  WFIM-V0RELATORIO  NOT  EQUAL  SPACES
               DISPLAY  '*----------------------------------------*'
               DISPLAY  'RG1866B - NAO HA SOLICITACAO PARA EXECUCAO'
               DISPLAY  '*----------------------------------------*'
               GO   TO   R0000-90-FINALIZA.
      *
           PERFORM       R0500-00-DECLARE-V0PREMIOS.
      *
           PERFORM       R0600-00-FETCH-V0PREMIOS.
      *
           IF            WFIM-V0PREMIOS  NOT  EQUAL  SPACES
               PERFORM   R9900-00-ENCERRA-SEM-MOVTO
           ELSE
               MOVE      ZEROS    TO    V0PROD-CODPRODU
               MOVE      ZEROS    TO    V0APOL-NUM-APOL
               MOVE      ZEROS    TO    V0ENDO-NUM-APOL
               MOVE      ZEROS    TO    V0ENDO-NRENDOS
               MOVE      ZEROS    TO    V0HISP-NRENDOCA
               MOVE      ZEROS    TO    V0FONT-COD-FONTE.
      *
           PERFORM       R0700-00-PROCESSA-REGISTRO  UNTIL
                         WFIM-V0PREMIOS  NOT  EQUAL  SPACES.
      *
           PERFORM       R0300-00-DELETE-V0RELATORIO.
      *
       R0000-90-FINALIZA.
      *
           CLOSE      PREMIT.
      *
           CLOSE      PREMCED.
      *
           DISPLAY   'REG. LIDOS NA PREMIOS - '  AC-L-V0PREMIOS.
           DISPLAY   'REG. GRAVD NO PREMIT  - '  AC-G-PREMIT.
           DISPLAY   'REG. GRAVD NO PREMCED - '  AC-G-PREMCED.
      *
           MOVE      '00:00:00'          TO         WS-HORA-CURR.
      *
           ACCEPT     WS-HORA-ACCEPT     FROM       TIME.
           MOVE       WS-HOR-ACCEPT      TO         WS-HOR-CURR.
           MOVE       WS-MIN-ACCEPT      TO         WS-MIN-CURR.
           MOVE       WS-SEG-ACCEPT      TO         WS-SEG-CURR.

           DISPLAY   '                              '
           DISPLAY   'RG1866B - FINAL DE EXECUCAO  (' WS-DATA-CURR
                                          ' - '       WS-HORA-CURR ')'.
      *
           DISPLAY   '*---   RG1866B  -  FIM  NORMAL   ---*'.
      *
           MOVE       ZEROS       TO     RETURN-CODE.
      *
           STOP       RUN.
      *
       R0000-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
362429*
=      R0100-00-SELECT-SISTEMAS    SECTION.
=     *------------------------------------
=     *
=          MOVE           '010'          TO               WNR-EXEC-SQL.
=     *
=          EXEC  SQL
=             SELECT  DATA_MOV_ABERTO
=               INTO :SISTEMAS-DATA-MOV-ABERTO
=               FROM  SEGUROS.SISTEMAS
=              WHERE  IDE_SISTEMA    =    'GL'
=               WITH  UR
=          END-EXEC.
=     *
=          IF  SQLCODE  NOT   EQUAL  ZEROS
=              DISPLAY 'R0100 - ERRO NO SELECT DA SISTEMAS'
=              GO   TO  R9999-00-ROT-ERRO
=          ELSE
=           DISPLAY 'DATA DO SISTEMA RG/GL: ' SISTEMAS-DATA-MOV-ABERTO.
=     *
=      R0100-99-SAIDA.
362429     EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R0200-00-SELECT-V0RELATORIO   SECTION.
      *--------------------------------------
      *
           MOVE         '020'             TO         WNR-EXEC-SQL.
      *
           EXEC  SQL
              SELECT  CODUSU           ,
                      DATA_SOLICITACAO ,
                      IDSISTEM         ,
                      CODRELAT         ,
                      PERI_INICIAL     ,
                      PERI_FINAL       ,
                      DATA_REFERENCIA  ,
                      ANO_REFERENCIA,
                      MES_REFERENCIA
                INTO :V0RELA-COD-USUARIO ,
                     :V0RELA-DTA-SOLICTA ,
                     :V0RELA-IDE-SISTEMA ,
                     :V0RELA-COD-RELAT   ,
                     :V0RELA-PERI-INICIAL,
                     :V0RELA-PERI-FINAL  ,
                     :V0RELA-DATA-REFR   ,
                     :V0RELA-ANO-REFER   ,
                     :V0RELA-MES-REFER
                FROM  SEGUROS.V0RELATORIOS
379341         WHERE  CODUSU             =  'RG0840B'
362429           AND  DATA_SOLICITACAO   =  :SISTEMAS-DATA-MOV-ABERTO
=                AND  IDSISTEM           =  'RG'
=                AND  CODRELAT           =  'RG1866B'
=                AND  SITUACAO           =  '0'
362429          WITH  UR
           END-EXEC.
      *
           IF    SQLCODE  NOT  EQUAL  ZEROS
             IF  SQLCODE       EQUAL  100
                 MOVE     'S'  TO     WFIM-V0RELATORIO
                 GO            TO     R0200-99-SAIDA
             ELSE
                 DISPLAY  'R0200 - ERRO NO SELECT DA V0RELATORIOS'
                 GO   TO   R9999-00-ROT-ERRO
428303       END-IF
           ELSE
             DISPLAY   ' '
             DISPLAY   'RG1866B - LIDO NA RELATORIOS RG1866B  ( '
                        V0RELA-MES-REFER '/' V0RELA-ANO-REFER ')'
428303     END-IF.
      *
142985     DISPLAY   'DATA INICIAL PRCESMTO - ' V0RELA-PERI-INICIAL.
142985     DISPLAY   'DATA TERMINO PRCESMTO - ' V0RELA-PERI-FINAL.
      *
428303     MOVE       V0RELA-DATA-REFR      TO  WDATA-AUX.
428303     MOVE       WDAT-AUX-DIA          TO  WHOST-DIA-REFER.
      *
       R0200-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R0300-00-DELETE-V0RELATORIO   SECTION.
      *--------------------------------------
      *
           MOVE         '030'              TO             WNR-EXEC-SQL.
      *
           EXEC SQL  DELETE
               FROM  SEGUROS.V0RELATORIOS
362429        WHERE  CODUSU             =  :V0RELA-COD-USUARIO
=               AND  DATA_SOLICITACAO   =  :V0RELA-DTA-SOLICTA
=               AND  IDSISTEM           =  :V0RELA-IDE-SISTEMA
=               AND  CODRELAT           =  :V0RELA-COD-RELAT
=               AND  PERI_INICIAL       =  :V0RELA-PERI-INICIAL
=               AND  PERI_FINAL         =  :V0RELA-PERI-FINAL
=               AND  MES_REFERENCIA     =  :V0RELA-MES-REFER
=               AND  ANO_REFERENCIA     =  :V0RELA-ANO-REFER
362429          AND  SITUACAO           =  '0'
           END-EXEC.
      *
           IF    SQLCODE  NOT  EQUAL   ZEROS
             IF  SQLCODE       EQUAL   100
                 NEXT     SENTENCE
             ELSE
                 DISPLAY 'R0300 - ERRO NO DELETE DA V0RELATORIOS'
                 GO   TO  R9999-00-ROT-ERRO.
      *
       R0300-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R0500-00-DECLARE-V0PREMIOS  SECTION.
      *------------------------------------
      *
           MOVE           '050'             TO         WNR-EXEC-SQL.
      *
           EXEC  SQL  DECLARE    V0PREMIOS   CURSOR   FOR
188334        SELECT  COD_EMPRESA,
                      ANO_REFERENCIA,
                      MES_REFERENCIA,
142985                DIA_REFERENCIA,
                      RAMO_SUSEP,
                      TIPO_MOVTO,
                      TIPO_ENDOSSO,
                      DTMOVTO,
                      NUM_APOLICE,
                      NRENDOCA,
                      NRENDOS,
                      TIPO_OPERACAO,
                      NRPARCEL,
                      OCORHIST,
                      RAMOFR,
                      MODALIFR,
                      OPERACAO,
                      CODCLIEN,
                      DTVENCTO,
                      IMP_SEG,
                      VLPRMTAR,
                      VLDESCON,
                      VLPRMLIQ,
                      VLADIFRA,
                      VLCUSEMI,
                      VLIOCC,
                      VLPRMTOT,
                      VLCOMIS,
                      VLADMN,
                      VLAGENC,
                      IMP_SEG_LID,
                      VLPRMTAR_LID,
                      VLDESCON_LID,
                      VLPRMLIQ_LID,
                      VLADIFRA_LID,
                      VLCUSEMI_LID,
                      VLIOCC_LID,
                      VLPRMTOT_LID,
                      VLCOMIS_LID,
                      VLADMN_LID,
                      VLAGENC_LID,
                      IMP_SEG_CED,
                      VLPRMTAR_CED,
                      VLDESCON_CED,
                      VLPRMLIQ_CED,
                      VLADIFRA_CED,
                      VLCOMIS_CED,
                      VLADMN_CED,
                      VLAGENC_CED,
                      VLIMP_SEG_RES,
                      VLPRMTAR_RES,
                      VLDESCON_RES,
                      VLPRMLIQ_RES,
                      VLADIFRA_RES,
                      VLCOMIS_RES,
                      VLPREMIO,
                      ORGAO,
                      RAMO,
                      CODSUBES,
                      FONTE,
                      NUM_PROPT,
                      QTPARCEL,
                      CORRECAO,
                      COD_MOEDA_PRM,
                      COD_MOEDA_IMP,
                      NUMBIL,
                      TIPSGU,
                      DTEMIS,
                      TPCOSCED,
                      DTINIVIG,
                      DTTERVIG,
                      CODPRODU,
136071                CANAL_VENDA
                FROM  SEGUROS.V0PREMIOS
               WHERE  ANO_REFERENCIA        =  :V0RELA-ANO-REFER
                 AND  MES_REFERENCIA        =  :V0RELA-MES-REFER
428303           AND  DIA_REFERENCIA        =  :WHOST-DIA-REFER
                 AND  TIPO_MOVTO            =  '0'
                 AND  TIPSGU                =  '1'
                 AND  TIPO_OPERACAO        IN  (1101,1104,1105,1109,
                                                1111,1114,1115)
243278           AND (RAMO_SUSEP           IN  (1381,1601)
243278            OR (RAMO_SUSEP           IN  (0969,0982)
243278           AND  NUMBIL                >   0))
               ORDER  BY
                      RAMO_SUSEP,
                      TIPO_MOVTO,
                      TIPO_ENDOSSO,
                      DTMOVTO,
                      NUM_APOLICE,
                      NRENDOCA,
                      NRENDOS,
                      TIPO_OPERACAO,
                      NRPARCEL,
                      OCORHIST
               WITH   UR
           END-EXEC.
      *
           EXEC  SQL   OPEN   V0PREMIOS      END-EXEC.
      *
           IF  SQLCODE  NOT   EQUAL  ZEROS
               DISPLAY 'R0500 - ERRO NO DECLARE DA V0PREMIOS'
               GO   TO  R9999-00-ROT-ERRO
           ELSE
               MOVE     SPACES   TO  WFIM-V0PREMIOS.
      *
       R0500-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R0600-00-FETCH-V0PREMIOS    SECTION.
      *------------------------------------
      *
           MOVE           '060'          TO               WNR-EXEC-SQL.
      *
       R0600-10-LER-V0PREMIOS.
      *
           EXEC  SQL     FETCH           V0PREMIOS
188334           INTO   :V0PREM-COD-EMP,
                        :V0PREM-ANO-REFER,
                        :V0PREM-MES-REFER,
142985                  :V0PREM-DIA-REFER,
                        :V0PREM-RAMO-SUSEP,
                        :V0PREM-TIPO-MOVT,
                        :V0PREM-TIPO-ENDS,
                        :V0PREM-DTMOVTO,
                        :V0PREM-NUM-APOL,
                        :V0PREM-NRENDOCA,
                        :V0PREM-NRENDOS,
                        :V0PREM-TIPO-OPER,
                        :V0PREM-NRPARCEL,
                        :V0PREM-OCORHIST,
                        :V0PREM-RAMOFR,
                        :V0PREM-MODALIFR,
                        :V0PREM-OPERACAO,
                        :V0PREM-CODCLIEN,
                        :V0PREM-DTVENCTO,
                        :V0PREM-IMP-SEG-T,
                        :V0PREM-VLPRMTAR-T,
                        :V0PREM-VLDESCON-T,
                        :V0PREM-VLPRMLIQ-T,
                        :V0PREM-VLADIFRA-T,
                        :V0PREM-VLCUSEMI-T,
                        :V0PREM-VLIOCC-T,
                        :V0PREM-VLPRMTOT-T,
                        :V0PREM-VLCOMIS-T,
                        :V0PREM-VLADMN-T,
                        :V0PREM-VLAGENC-T,
                        :V0PREM-IMP-SEG-L,
                        :V0PREM-VLPRMTAR-L,
                        :V0PREM-VLDESCON-L,
                        :V0PREM-VLPRMLIQ-L,
                        :V0PREM-VLADIFRA-L,
                        :V0PREM-VLCUSEMI-L,
                        :V0PREM-VLIOCC-L,
                        :V0PREM-VLPRMTOT-L,
                        :V0PREM-VLCOMIS-L,
                        :V0PREM-VLADMN-L,
                        :V0PREM-VLAGENC-L,
                        :V0PREM-IMP-SEG-C,
                        :V0PREM-VLPRMTAR-C,
                        :V0PREM-VLDESCON-C,
                        :V0PREM-VLPRMLIQ-C,
                        :V0PREM-VLADIFRA-C,
                        :V0PREM-VLCOMIS-C,
                        :V0PREM-VLADMN-C,
                        :V0PREM-VLAGENC-C,
                        :V0PREM-IMP-SEG-R,
                        :V0PREM-VLPRMTAR-R,
                        :V0PREM-VLDESCON-R,
                        :V0PREM-VLPRMLIQ-R,
                        :V0PREM-VLADIFRA-R,
                        :V0PREM-VLCOMIS-R,
                        :V0PREM-VLPREMIO,
                        :V0PREM-ORGAO,
                        :V0PREM-RAMO,
                        :V0PREM-CODSUBES,
                        :V0PREM-FONTE,
                        :V0PREM-NUM-PROPT,
                        :V0PREM-QTPARCEL,
                        :V0PREM-CORRECAO,
                        :V0PREM-MOEDA-PRM,
                        :V0PREM-MOEDA-IMP,
                        :V0PREM-NUMBIL,
                        :V0PREM-TIPSGU,
                        :V0PREM-DTEMIS,
                        :V0PREM-TPCOSCED,
                        :V0PREM-DTINIVIG,
                        :V0PREM-DTTERVIG,
                        :V0PREM-CODPRODU,
136071                  :V0PREM-CANAL-VENDA
           END-EXEC.
      *
           IF     SQLCODE  NOT  EQUAL    ZEROS
              IF  SQLCODE       EQUAL    100
                  MOVE     'S'     TO    WFIM-V0PREMIOS
                  EXEC     SQL  CLOSE         V0PREMIOS   END-EXEC
                  GO       TO   R0600-99-SAIDA
              ELSE
                  DISPLAY 'R0600 - ERRO DE FETCH NA V0PREMIOS'
                  GO   TO  R9999-00-ROT-ERRO
169452        END-IF
           ELSE
149755       MOVE    V0PREM-RAMO-SUSEP  TO       WRAMO-SUSEP
=            IF  WCOD-GRUPO  =  09
=                IF  V0PREM-NUMBIL  =  00
149755               GO  TO  R0600-10-LER-V0PREMIOS
169452           ELSE
=                  IF  V0PREM-RAMO      =  77  AND
=                     (V0PREM-CODPRODU  =  7705 OR 7716 OR 7725)
=                      GO   TO   R0600-10-LER-V0PREMIOS
=                  END-IF
=                END-IF
169452       END-IF
149755     END-IF.
      *
           ADD    1    TO      AC-COUNT.
           ADD    1    TO      AC-L-V0PREMIOS.
      *
           IF          AC-COUNT           >        99999
             MOVE      ZEROS              TO       AC-COUNT
             ACCEPT    WS-HORA-ACCEPT     FROM     TIME
             MOVE      WS-HOR-ACCEPT      TO       WS-HOR-CURR
             MOVE      WS-MIN-ACCEPT      TO       WS-MIN-CURR
             MOVE      WS-SEG-ACCEPT      TO       WS-SEG-CURR
             DISPLAY  'RG1866B - LIDOS NA PREMIOS   ('
                       AC-L-V0PREMIOS   ' - '  WS-HORA-CURR ')'.
      *
       R0600-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R0700-00-PROCESSA-REGISTRO  SECTION.
      *------------------------------------
      *
           MOVE          '070'           TO         WNR-EXEC-SQL.
      *
      *--* FORMATA REGISTROS
      *
           MOVE          ZEROS           TO         REGT-PREMIT
                                                    REGT-PREMCED.
      *
           ADD      1                    TO         WS-SEQ-PREMIT.
           MOVE     WS-SEQ-PREMIT        TO         EMI-SEQ.
      *
188334     MOVE     V0PREM-COD-EMP       TO         EMI-COD-EMPR
=                                                   CED-COD-EMPR.
      *
188334     IF  V0PREM-COD-EMP    =  000
=              MOVE    '05631'           TO         EMI-COD-CIA
=                                                   CED-COD-CIA
=          ELSE
=            IF  V0PREM-COD-EMP  =  010
=                MOVE  '08141'           TO         EMI-COD-CIA
=                                                   CED-COD-CIA
266453       ELSE
=              MOVE    '00442'           TO         EMI-COD-CIA
=                                                   CED-COD-CIA
266453       END-IF
188334     END-IF.
      *
C97168*--* MONTA O NUMERO DO PROCESSO NA SUSEP
=     *
=          IF V0PREM-RAMO  =  31  OR  53
=     *
=             IF V0PREM-ORGAO  =  100
=                MOVE  '15414.001504/2004-94'  TO  V0PROD-NUM-PROCS
=             ELSE
=               IF V0PREM-ORGAO  =  110
=     *
=                  MOVE   SPACES       TO      WHOST-INIVIG-AP
=     *
=                  IF V0PREM-NRENDOS  =  ZEROS
=                     MOVE   V0PREM-DTINIVIG  TO  WHOST-INIVIG-AP
=                  ELSE
=                     PERFORM  R0720-00-SELECT-DTINIVIG-AP
=                  END-IF
=     *
=                  IF WHOST-INIVIG-AP  >  '2014-01-30'
=                     MOVE     V0PREM-CODPRODU  TO  V0PROD-CODPRODU
=                     PERFORM  R0740-00-SELECT-V0PRODUTO
=                  ELSE
=                     MOVE  '15414.001779/2011-57'  TO  V0PROD-NUM-PROCS
=                  END-IF
=     *
=               ELSE
=     *
=                  MOVE     V0PREM-CODPRODU  TO  V0PROD-CODPRODU
=                  PERFORM  R0740-00-SELECT-V0PRODUTO
=     *
=               END-IF
=          ELSE
=     *
=             IF V0PREM-CODPRODU  =  V0PROD-CODPRODU
=                NEXT   SENTENCE
=             ELSE
=                MOVE     V0PREM-CODPRODU  TO  V0PROD-CODPRODU
=                PERFORM  R0740-00-SELECT-V0PRODUTO
=             END-IF
=     *
=          END-IF.
C97168*
136184     IF  V0PREM-CODPRODU   =  1803 OR 1804 OR 1805
=              IF  V0PREM-RAMO-SUSEP   =    0118
=                  MOVE  '15414.000427/2007-06'       TO EMI-NUM-PROC
=                                                        CED-NUM-PROC
=              ELSE
=                IF  V0PREM-RAMO-SUSEP   =   0141
=                    MOVE  '15414.001968/2010-49'     TO EMI-NUM-PROC
=                                                        CED-NUM-PROC
=                ELSE
=                  IF  V0PREM-RAMO-SUSEP   =   0351
=                      MOVE  '15414.901948/2014-01'   TO EMI-NUM-PROC
=                                                        CED-NUM-PROC
=                  END-IF
=                END-IF
=              END-IF
=          ELSE
=              MOVE    V0PROD-NUM-PROCS  TO         EMI-NUM-PROC
=                                                   CED-NUM-PROC
136184     END-IF.
      *
      *--* MONTA O ANO E MES DE REFERENCIA DO MOVIMENTO
      *
428303     MOVE     V0RELA-ANO-REFER         TO     EMI-DT-BASE-AA
=                                                   CED-DT-BASE-AA.
=     *
=          MOVE     V0RELA-MES-REFER         TO     EMI-DT-BASE-MM
428303                                              CED-DT-BASE-MM.
      *
      *--* ACESSA A TABELA ENDOSSOS
      *
           IF  V0PREM-NUM-APOL = V0ENDO-NUM-APOL AND
               V0PREM-NRENDOS  = V0ENDO-NRENDOS
               NEXT   SENTENCE
           ELSE
               MOVE     V0PREM-NUM-APOL      TO     V0ENDO-NUM-APOL
               MOVE     V0PREM-NRENDOS       TO     V0ENDO-NRENDOS
               PERFORM  R0760-00-SELECT-V0ENDOSSO
           END-IF.
      *
112349     MOVE      V0ENDO-DATPRO           TO     WDATA-AUX.
=          MOVE      WDAT-AUX-ANO            TO     WDAT-DBF-ANO.
=          MOVE      WDAT-AUX-MES            TO     WDAT-DBF-MES.
112349     MOVE      WDAT-AUX-DIA            TO     WDAT-DBF-DIA.
136071**** MOVE      WDATA-DBF               TO     EMI-DTPROP-END.
      *
      *--* IDENTIFICA O ENDOSSO DE CANCELAMENTO
      *
112349     MOVE    ZEROS                     TO     V0HISP-VLPRMTOT.
      *
           IF  V0PREM-TIPO-OPER  = 1104 OR 1114 OR 1164 OR 1174
               IF  V0PREM-NRENDOCA = ZEROS
                   MOVE     ZEROS           TO WHOST-QTD-DOCT
                   MOVE     V0PREM-NUM-APOL TO V0ENDS-NUM-APOL
                   MOVE     V0PREM-NUM-APOL TO V0HISP-NUM-APOL
                   MOVE     ZEROS           TO V0ENDS-NRENDOS
                   MOVE     V0PREM-NRENDOS  TO V0HISP-NRENDOS
                   MOVE     V0PREM-OPERACAO TO V0HISP-OPERACAO
                   MOVE     V0PREM-DTMOVTO  TO V0ENDS-DTEMIS
                   MOVE     V0PREM-DTMOVTO  TO V0HISP-DTMOVTO
                   MOVE     V0PREM-NRENDOCA TO V0HISP-NRENDOCA
                   MOVE     ZEROS           TO V0HISP-VLPRMTOT
                   MOVE     SPACES          TO V0ENDS-TIPO-ENDO
                   PERFORM  R0780-00-SELECT-ENDOS-CANCLM
                   IF  V0ENDS-TIPO-ENDO  =  '5'
                       PERFORM  R0800-00-SELECT-V0HISTOPARC
                   ELSE
                       PERFORM  R0820-00-SELECT-QTD-DOCT-CANC
               ELSE
113598**       IF  V0PREM-NRENDOCA = V0HISP-NRENDOCA
=    ***           NEXT   SENTENCE
113598**       ELSE
                   MOVE     ZEROS           TO WHOST-QTD-DOCT
                   MOVE     V0PREM-NUM-APOL TO V0ENDS-NUM-APOL
                   MOVE     V0PREM-NUM-APOL TO V0HISP-NUM-APOL
                   MOVE     V0PREM-NRENDOCA TO V0ENDS-NRENDOS
                   MOVE     V0PREM-NRENDOS  TO V0HISP-NRENDOS
                   MOVE     V0PREM-OPERACAO TO V0HISP-OPERACAO
                   MOVE     SPACES          TO V0ENDS-DTEMIS
                   MOVE     V0PREM-DTMOVTO  TO V0HISP-DTMOVTO
                   MOVE     V0PREM-NRENDOCA TO V0HISP-NRENDOCA
                   MOVE     ZEROS           TO V0HISP-VLPRMTOT
                   MOVE     SPACES          TO V0ENDS-TIPO-ENDO
                   PERFORM  R0840-00-SELECT-ENDS-CANCELM
                   IF  V0ENDS-TIPO-ENDO  =  '5'
                       PERFORM  R0800-00-SELECT-V0HISTOPARC
                   ELSE
                       PERFORM  R0820-00-SELECT-QTD-DOCT-CANC.
      *
      *--* MONTA TIPO DE MOVIMENTO
      *
           MOVE     ZEROS                TO         EMI-TIPO-MOV
                                                    CED-TIPO-MOV.
      *
           IF  V0PREM-TIPO-OPER  =  1101 OR 1105 OR 1161 OR 1165
               IF  V0PREM-NRENDOS  =  ZEROS  OR
                   V0ENDO-TIPO-ENDS  =  '0'
                   MOVE    101   TO  EMI-TIPO-MOV
                                     CED-TIPO-MOV
               ELSE
                   MOVE    102   TO  EMI-TIPO-MOV
                                     CED-TIPO-MOV
               END-IF
           ELSE
             IF  V0PREM-TIPO-OPER  =  1104 OR 1164
                 IF  V0PREM-NRENDOS  =  ZEROS  OR
                     V0ENDO-TIPO-ENDS  =  '0'
                     IF  V0HISP-VLPRMTOT  =  ZEROS
                         MOVE   106   TO  EMI-TIPO-MOV
                                          CED-TIPO-MOV
                     ELSE
153103                   MOVE     ZEROS           TO V0COBA-PRM-TAR-IX
=                                                    V0COBA-PRM-TAR-VR
=                        MOVE     V0PREM-NUM-APOL TO V0COBA-NUM-APOL
=                        MOVE     V0ENDS-NRENDOS  TO V0COBA-NRENDOS
=                        MOVE     V0PREM-RAMOFR   TO V0COBA-RAMOFR
=                        PERFORM  R0850-00-SELECT-V0COBERAPOL
=                        IF  V0COBA-PRM-TAR-VR  =  ZEROS
=                            MOVE   106   TO  EMI-TIPO-MOV
=                                             CED-TIPO-MOV
=                        ELSE
=                            MOVE   104   TO  EMI-TIPO-MOV
=                                             CED-TIPO-MOV
153103                   END-IF
                     END-IF
                 ELSE
                   IF  V0HISP-VLPRMTOT  =  ZEROS
                       MOVE   107   TO  EMI-TIPO-MOV
                                        CED-TIPO-MOV
                   ELSE
153103                 MOVE     ZEROS           TO V0COBA-PRM-TAR-IX
=                                                  V0COBA-PRM-TAR-VR
=                      MOVE     V0PREM-NUM-APOL TO V0COBA-NUM-APOL
=                      MOVE     V0ENDS-NRENDOS  TO V0COBA-NRENDOS
=                      MOVE     V0PREM-RAMOFR   TO V0COBA-RAMOFR
=                      PERFORM  R0850-00-SELECT-V0COBERAPOL
=                      IF  V0COBA-PRM-TAR-VR  =  ZEROS
=                          MOVE   107   TO  EMI-TIPO-MOV
=                                           CED-TIPO-MOV
=                      ELSE
=                          MOVE   105   TO  EMI-TIPO-MOV
=                                           CED-TIPO-MOV
153103                 END-IF
                   END-IF
                 END-IF
             ELSE
               IF  V0PREM-TIPO-OPER  =  1111 OR 1114 OR 1115 OR
                   V0PREM-TIPO-OPER  =  1171 OR 1174 OR 1175
                   MOVE   103    TO     EMI-TIPO-MOV
                                        CED-TIPO-MOV
               ELSE
                 IF  V0PREM-TIPO-ENDS  =  '2'
                     IF  V0HISP-VLPRMTOT  =  ZEROS
                         MOVE   107   TO  EMI-TIPO-MOV
                                          CED-TIPO-MOV
                     ELSE
153103                   MOVE     ZEROS           TO V0COBA-PRM-TAR-IX
=                                                    V0COBA-PRM-TAR-VR
=                        MOVE     V0PREM-NUM-APOL TO V0COBA-NUM-APOL
=                        MOVE     V0ENDS-NRENDOS  TO V0COBA-NRENDOS
=                        MOVE     V0PREM-RAMOFR   TO V0COBA-RAMOFR
=                        PERFORM  R0850-00-SELECT-V0COBERAPOL
=                        IF  V0COBA-PRM-TAR-VR  =  ZEROS
=                            MOVE   107   TO  EMI-TIPO-MOV
=                                             CED-TIPO-MOV
=                        ELSE
=                            MOVE   105   TO  EMI-TIPO-MOV
=                                             CED-TIPO-MOV
153103                   END-IF
                     END-IF
                 ELSE
                   MOVE     108     TO  EMI-TIPO-MOV
                                        CED-TIPO-MOV
                 END-IF
               END-IF
             END-IF
           END-IF.
      *
           IF  EMI-TIPO-MOV  =  000
               DISPLAY 'R0700 - ERRO NA IDENTIFICACAO DO TIPO-MOV'
               GO   TO  R9999-00-ROT-ERRO.
      *
      *--* MONTA SIGLA DO ESTADO (UF) DA FONTE PRODUTORA (FILIAL)
      *
           IF  V0PREM-FONTE = V0FONT-COD-FONTE
               NEXT  SENTENCE
           ELSE
               MOVE     V0PREM-FONTE TO V0FONT-COD-FONTE
               PERFORM  R0860-00-SELECT-V0FONTE.
      *
           MOVE     V0FONT-ESTADO        TO         EMI-UF-DEP.
      *
      *--*
      *
149755     MOVE     V0PREM-RAMO-SUSEP    TO         EMI-COD-RAMO.
      *
           MOVE     ZEROS                TO         EMI-APOL-FIL
                                                    CED-APOL-FIL.
      *
           MOVE     V0PREM-NUM-APOL      TO         EMI-NUM-APOL
                                                    CED-NUM-APOL.
      *
           MOVE     ZEROS                TO         EMI-ENDS-FIL
                                                    CED-ENDS-FIL.
      *
           MOVE     V0PREM-NRENDOS       TO         EMI-NUM-END
                                                    CED-NUM-END.
      *
           MOVE     ALL  ZEROS           TO         EMI-NR-PROPT
                                                    CED-NR-PROPT.
      *
           MOVE     SPACES               TO         WDATA-AUX.
      *
           IF  V0PREM-TIPSGU  =  '1'
               IF  V0PREM-ORGAO   =  100  OR  110
                   MOVE     V0PREM-NUM-APOL TO V0AUTA-NUM-APOL
                   MOVE     V0PREM-NRENDOS  TO V0AUTA-NRENDOS
                   PERFORM  R0880-00-SELECT-V0AUTOAPOL
                   IF  V0PREM-ORGAO  =  110
                       PERFORM  R0900-00-SELECT-V0AUTOPROP
                       IF  V0AUPR-DTH-EFETV  =  SPACES
                           MOVE   AU055-DTH-OPERAC  TO  WDATA-AUX
                       ELSE
                           MOVE   V0AUPR-DTH-EFETV  TO  WDATA-AUX
                   ELSE
                       MOVE   AU055-DTH-OPERAC  TO  WDATA-AUX
               ELSE
105223***          MOVE   V0PREM-FONTE      TO      EMI-COD-FONTE
105223***                                           CED-COD-FONTE
                   MOVE   V0ENDO-NRPROPOS   TO      EMI-NUM-PROP
                   MOVE   V0ENDO-NRPROPOS   TO      CED-NUM-PROP
                   MOVE   V0ENDO-DATPRO     TO      WDATA-AUX
           ELSE
105223***      MOVE   V0PREM-FONTE          TO      EMI-COD-FONTE
105223***                                           CED-COD-FONTE
               MOVE   V0ENDO-NRPROPOS       TO      EMI-NUM-PROP
               MOVE   V0ENDO-NRPROPOS       TO      CED-NUM-PROP
               MOVE   V0ENDO-DATPRO         TO      WDATA-AUX.
      *
105223     MOVE       ZEROS                 TO      EMI-COD-FONTE
=                                                   CED-COD-FONTE.
      *
105223     IF  V0PREM-CODPRODU  EQUAL  1803  OR  1805
=              PERFORM  R0920-00-LT-MOV-PROPOSTA
=              MOVE     LTMVPROP-NUM-TITULO   TO     EMI-NUM-PROP1
105223                                               CED-NUM-PROP1
112349     ELSE
=              IF  V0PREM-CODPRODU  EQUAL  3172  OR  3173 OR 3174
=                                      OR  3175  OR  3176 OR 3177
=                                      OR  3178  OR  3179 OR 3180
=                                      OR  3181  OR  3182 OR 5302
=                                      OR  5303  OR  5304
=                  PERFORM  R0940-00-AU-PROP-CONV-VC
=                  MOVE     AU057-NUM-PROPOSTA-VC    TO  EMI-NR-PROPT
112349                                                   CED-NR-PROPT
105223         ELSE
=                  MOVE     V0ENDO-NRPROPOS          TO  EMI-NUM-PROP
=                                                        CED-NUM-PROP
=              END-IF
105223     END-IF.
      *
           MOVE     WDAT-AUX-ANO         TO         WDAT-DBF-ANO.
           MOVE     WDAT-AUX-MES         TO         WDAT-DBF-MES.
           MOVE     WDAT-AUX-DIA         TO         WDAT-DBF-DIA.
           MOVE     WDATA-DBF            TO         EMI-DT-PROPT.
      *
      *--* MONTA CGC/CPF DO SEGURADO E/OU SUBESTIPULANTE
      *
           MOVE     ZEROS                TO         WCOD-CLIEN-SEG
                                                    WCOD-CLIEN-EST
                                                    WCOD-CLIEN-TOM.
      *
           MOVE     V0PREM-CODCLIEN      TO         V0CLIE-CODCLIEN.
      *
           PERFORM  R0960-00-SELECT-V0CLIENTE.
      *
           MOVE     V0CLIE-CGC-CPF       TO    EMI-CPF-SUBEST.
      *
           IF  V0PREM-CODSUBES  =  ZEROS
               MOVE   V0CLIE-CGC-CPF     TO    EMI-CPF-SEG
               MOVE   ZEROS              TO    EMI-CPF-ESTIP
               MOVE   V0CLIE-CODCLIEN    TO    WCOD-CLIEN-SEG
           ELSE
               MOVE     V0CLIE-CGC-CPF   TO    EMI-CPF-ESTIP
               MOVE     V0CLIE-CODCLIEN  TO    WCOD-CLIEN-EST
               MOVE     V0PREM-NUM-APOL  TO    V0APOL-NUM-APOL
               PERFORM  R0980-00-SELECT-V0APOLICE
               PERFORM  R0960-00-SELECT-V0CLIENTE
               MOVE     V0CLIE-CGC-CPF   TO    EMI-CPF-SEG
               MOVE     V0CLIE-CODCLIEN  TO    WCOD-CLIEN-SEG
           END-IF.
      *
      *--* OBTER A QUANTIDADE DE SEGURADOS POR APOLICE E ENDOSSO
      *
           MOVE     ZEROS                TO         EMI-QTD-SEG
                                                    WHOST-QTD-ITEM
                                                    EF063-NUM-APOL
                                                    EF063-NUM-CONTR
                                                    EF050-COD-PRODU
                                                    EF148-PRODU-ACS
                                                    V0PDVG-NUM-APOL
                                                    V0PDVG-CODSUBES.
      *
           MOVE     SPACES               TO         WTEM-APOL-EF
C10158                                              WHOST-TIP-PRM-I
C10158                                              WHOST-TIP-PRM-F
                                                    V0PDVG-ORIG-PRODU.
      *
           IF  V0PREM-NUMBIL  >  ZEROS
               MOVE     01               TO         EMI-QTD-SEG
           ELSE
           IF  WCOD-GRUPO  =  09 OR 13
               IF  V0PREM-RAMO  =  77
C10158             MOVE     V0PREM-NUM-APOL TO   EF148-NUM-APOL
=                  MOVE     V0PREM-RAMOFR   TO   EF148-RAMO-CONTB
=                  MOVE     V0PREM-CODPRODU TO   EF148-PRODU-ACS
C10158             MOVE     V0PREM-DTINIVIG TO   EF148-DTH-INIVIG
                   PERFORM  R0990-00-SELECT-EF-APOLICE
                   IF  WTEM-APOL-EF  =  'S'
C10158                 IF  EF053-TIPO-ENDS  =  '0' OR '1' OR '4' OR '6'
=                          MOVE    '1'     TO      WHOST-TIP-PRM-I
=                          MOVE    '3'     TO      WHOST-TIP-PRM-F
=                      ELSE
=                          MOVE    '2'     TO      WHOST-TIP-PRM-I
=                          MOVE    '4'     TO      WHOST-TIP-PRM-F
C10158                 END-IF
                       PERFORM  R1000-00-SELECT-EF-PRM-EMIT
                       MOVE     WHOST-QTD-ITEM  TO  EMI-QTD-SEG
                   ELSE
                       MOVE     V0PREM-NUM-APOL TO V0PDVG-NUM-APOL
                       MOVE     V0PREM-CODSUBES TO V0PDVG-CODSUBES
                       MOVE     SPACES          TO V0PDVG-ORIG-PRODU
                       PERFORM  R1020-00-SELECT-V0PRODUTOSVG
                       IF  V0PDVG-ORIG-PRODU  =  'ESPEC' OR 'EMPRE'
                           MOVE     V0PREM-NUM-APOL TO HTCPVA-NUM-APOL
                           MOVE     V0PREM-NUM-APOL TO V0FATR-NUM-APOL
                           MOVE     V0PREM-NRENDOS  TO HTCPVA-NUM-ENDS
                           MOVE     V0PREM-NRENDOS  TO V0FATR-NUM-ENDS
                           MOVE     V0PREM-CODSUBES TO HTCPVA-COD-SUBG
                           MOVE     V0PREM-FONTE    TO HTCPVA-COD-FONT
                           MOVE     ZEROS           TO HTCPVA-NUM-CERT
                           PERFORM  R1040-00-SELECT-HTCTPBVA
                           IF  HTCPVA-NUM-CERT  =  ZEROS
                               MOVE     ZEROS      TO   V0FTOT-QTVDA-VG
                               MOVE     ZEROS      TO   V0FTOT-QTVDA-AP
                               PERFORM  R1060-00-SELECT-V0FATURAS
                               MOVE     WHOST-QTD-ITEM  TO EMI-QTD-SEG
                           ELSE
                               MOVE     ZEROS      TO   CPRPVA-QT-VIDAS
                               PERFORM  R1080-00-SELECT-COBPRPVA
                               MOVE     CPRPVA-QT-VIDAS TO EMI-QTD-SEG
                       ELSE
                           MOVE     V0PREM-NUM-APOL TO HTCPVA-NUM-APOL
                           MOVE     V0PREM-NRENDOS  TO HTCPVA-NUM-ENDS
                           MOVE     V0PREM-CODSUBES TO HTCPVA-COD-SUBG
                           MOVE     V0PREM-FONTE    TO HTCPVA-COD-FONT
                           PERFORM  R1100-00-SELECT-QTDE-VIDAS
                           MOVE     WHOST-QTD-ITEM  TO  EMI-QTD-SEG
               ELSE
                   MOVE     V0PREM-NUM-APOL TO V0PDVG-NUM-APOL
                   MOVE     V0PREM-CODSUBES TO V0PDVG-CODSUBES
                   MOVE     SPACES          TO V0PDVG-ORIG-PRODU
                   PERFORM  R1020-00-SELECT-V0PRODUTOSVG
                   IF  V0PDVG-ORIG-PRODU  =  'ESPEC' OR 'EMPRE'
                       MOVE     V0PREM-NUM-APOL TO HTCPVA-NUM-APOL
                       MOVE     V0PREM-NUM-APOL TO V0FATR-NUM-APOL
                       MOVE     V0PREM-NRENDOS  TO HTCPVA-NUM-ENDS
                       MOVE     V0PREM-NRENDOS  TO V0FATR-NUM-ENDS
                       MOVE     V0PREM-CODSUBES TO HTCPVA-COD-SUBG
                       MOVE     V0PREM-FONTE    TO HTCPVA-COD-FONT
                       MOVE     ZEROS           TO HTCPVA-NUM-CERT
                       PERFORM  R1040-00-SELECT-HTCTPBVA
                       IF  HTCPVA-NUM-CERT  =  ZEROS
                           MOVE     ZEROS      TO   V0FTOT-QTVDA-VG
                           MOVE     ZEROS      TO   V0FTOT-QTVDA-AP
                           PERFORM  R1060-00-SELECT-V0FATURAS
                           MOVE     WHOST-QTD-ITEM  TO EMI-QTD-SEG
                       ELSE
                           MOVE     ZEROS      TO   CPRPVA-QT-VIDAS
                           PERFORM  R1080-00-SELECT-COBPRPVA
                           MOVE     CPRPVA-QT-VIDAS TO EMI-QTD-SEG
                   ELSE
                       MOVE     V0PREM-NUM-APOL TO HTCPVA-NUM-APOL
                       MOVE     V0PREM-NRENDOS  TO HTCPVA-NUM-ENDS
                       MOVE     V0PREM-CODSUBES TO HTCPVA-COD-SUBG
                       MOVE     V0PREM-FONTE    TO HTCPVA-COD-FONT
                       PERFORM  R1100-00-SELECT-QTDE-VIDAS
                       MOVE     WHOST-QTD-ITEM  TO  EMI-QTD-SEG
           ELSE
           IF  V0PREM-RAMO = 14 OR 48 OR 60 OR 61 OR
               V0PREM-RAMO = 65 OR 68 OR 70 OR 77
C10158         MOVE     V0PREM-NUM-APOL TO   EF148-NUM-APOL
=              MOVE     V0PREM-RAMOFR   TO   EF148-RAMO-CONTB
=              MOVE     V0PREM-CODPRODU TO   EF148-PRODU-ACS
C10158         MOVE     V0PREM-DTINIVIG TO   EF148-DTH-INIVIG
               PERFORM  R0990-00-SELECT-EF-APOLICE
               IF  WTEM-APOL-EF  =  'S'
C10158             IF  EF053-TIPO-ENDS  =  '0' OR '1' OR '4' OR '6'
=                      MOVE    '1'     TO      WHOST-TIP-PRM-I
=                      MOVE    '3'     TO      WHOST-TIP-PRM-F
=                  ELSE
=                      MOVE    '2'     TO      WHOST-TIP-PRM-I
=                      MOVE    '4'     TO      WHOST-TIP-PRM-F
C10158             END-IF
                   PERFORM  R1000-00-SELECT-EF-PRM-EMIT
                   MOVE     WHOST-QTD-ITEM  TO  EMI-QTD-SEG
               ELSE
                   MOVE     01     TO       EMI-QTD-SEG
           ELSE
           IF  V0PREM-RAMO = 31 OR 53
               MOVE     V0PREM-NUM-APOL TO V0AUTA-NUM-APOL
               MOVE     V0PREM-NRENDOS  TO V0AUTA-NRENDOS
               PERFORM  R1120-00-SELECT-V0AUTOAPOL
               MOVE     WHOST-QTD-ITEM  TO  EMI-QTD-SEG
           ELSE
               MOVE     01     TO       EMI-QTD-SEG.
123159*
=          IF  EMI-QTD-SEG  <  1
=              MOVE     01         TO       EMI-QTD-SEG
=          END-IF.
123159*
      *--* MONTA CGC/CPF DO TOMADOR
      *
           IF  V0PREM-RAMO = 45 OR 75
               MOVE     V0PREM-FONTE     TO  V0TOMD-FONTE
               MOVE     V0ENDO-NRPROPOS  TO  V0TOMD-NRPROPOS
               PERFORM  R1140-00-SELECT-V0TOMADOR
               IF  V0TOMD-CODCLIEN  =  ZEROS
                   MOVE   ZEROS    TO  EMI-CPF-TOM
                   MOVE   ZEROS    TO  EMI-QTD-TOM
               ELSE
                   PERFORM  R0960-00-SELECT-V0CLIENTE
                   MOVE     V0CLIE-CGC-CPF  TO   EMI-CPF-TOM
                   MOVE     01              TO   EMI-QTD-TOM
           ELSE
               MOVE    ZEROS    TO    EMI-CPF-TOM
               MOVE    ZEROS    TO    EMI-QTD-TOM.
      *
      *--* MONTA SIGLA DO ESTADO (UF) DO RISCO - BEM SEGURADO
      *
           MOVE     SPACES               TO         EMI-UF-RISCO
                                                    V0AGEN-ESTADO
                                                    V0ENDR-SIGLA-UF.
      *
           MOVE     V0PREM-NUM-APOL      TO         V0ACOR-NUM-APOL.
           MOVE     V0PREM-CODSUBES      TO         V0ACOR-CODSUBES.
           MOVE     V0PREM-RAMOFR        TO         V0ACOR-RAMOFR.
           MOVE     V0PREM-MODALIFR      TO         V0ACOR-MODALIFR.
           MOVE     V0PREM-DTINIVIG      TO         V0ACOR-DTINIVIG.
      *
           MOVE     ZEROS                TO         V0PRDT-COD-PRDT.
      *
           MOVE     WCOD-CLIEN-SEG       TO         V0ENDR-CODCLIEN.
           MOVE     V0ENDO-OCOR-ENDR     TO         V0ENDR-OCOR-ENDR.
      *
           IF  V0PREM-NUMBIL  >  ZEROS
               PERFORM  R1160-00-SELECT-V0ENDERECOS
               IF  V0ENDR-SIGLA-UF  =  SPACES
                   PERFORM  R1180-00-SELECT-V0AGENCIAS
                   IF  V0AGEN-ESTADO  =  SPACES
                       MOVE   V0FONT-ESTADO          TO  EMI-UF-RISCO
                   ELSE
                       MOVE   V0AGEN-ESTADO          TO  EMI-UF-RISCO
               ELSE
                   MOVE   V0ENDR-SIGLA-UF            TO  EMI-UF-RISCO
           ELSE
             IF  WCOD-GRUPO  =  09 OR 13
                 IF  V0PREM-RAMO = 77
                     IF  WTEM-APOL-EF  =  'S'
                         MOVE  WTABL-ESTADOS         TO  EMI-UF-RISCO
                     ELSE
                       IF  V0PDVG-ORIG-PRODU  =  'ESPEC' OR 'EMPRE'
                           PERFORM  R1200-00-SELECT-V0PRODUTOR
                           IF  V0PRDT-COD-PRDT  =  ZEROS
                               MOVE   V0FONT-ESTADO   TO   EMI-UF-RISCO
                           ELSE
                               MOVE  'MG          '   TO   EMI-UF-RISCO
                       ELSE
                           PERFORM  R1220-00-PROCESSA-UF-VIDA
                 ELSE
                   IF  V0PDVG-ORIG-PRODU  =   SPACES OR 'ESPEC'  OR
                                             'EMPRE' OR 'GLOBAL'
                       PERFORM  R1200-00-SELECT-V0PRODUTOR
                       IF  V0PRDT-COD-PRDT  =  ZEROS
                           MOVE   V0FONT-ESTADO      TO   EMI-UF-RISCO
                       ELSE
                           MOVE  'MG          '      TO   EMI-UF-RISCO
                   ELSE
                       PERFORM  R1220-00-PROCESSA-UF-VIDA
           ELSE
             IF  V0PREM-RAMO = 14 OR 48 OR 60 OR 61 OR
                               65 OR 68 OR 70 OR 77
                 IF  WTEM-APOL-EF  =  'S'
                     MOVE  WTABL-ESTADOS             TO  EMI-UF-RISCO
                 ELSE
                   PERFORM  R1200-00-SELECT-V0PRODUTOR
                   IF  V0PRDT-COD-PRDT  =  ZEROS
                       PERFORM  R1160-00-SELECT-V0ENDERECOS
                       IF  V0ENDR-SIGLA-UF  =  SPACES
                           PERFORM  R1180-00-SELECT-V0AGENCIAS
                           IF  V0AGEN-ESTADO  =  SPACES
                               MOVE   V0FONT-ESTADO  TO   EMI-UF-RISCO
                           ELSE
                               MOVE   V0AGEN-ESTADO  TO   EMI-UF-RISCO
                       ELSE
                           MOVE   V0ENDR-SIGLA-UF    TO   EMI-UF-RISCO
                   ELSE
                       MOVE     'MG          '       TO   EMI-UF-RISCO
           ELSE
             PERFORM  R1200-00-SELECT-V0PRODUTOR
             IF  V0PRDT-COD-PRDT  =  ZEROS
                 PERFORM  R1160-00-SELECT-V0ENDERECOS
                 IF  V0ENDR-SIGLA-UF  =  SPACES
                     PERFORM  R1180-00-SELECT-V0AGENCIAS
                     IF  V0AGEN-ESTADO  =  SPACES
                         MOVE   V0FONT-ESTADO        TO   EMI-UF-RISCO
                     ELSE
                       MOVE   V0AGEN-ESTADO          TO   EMI-UF-RISCO
                 ELSE
                     MOVE   V0ENDR-SIGLA-UF          TO   EMI-UF-RISCO
             ELSE
                 MOVE     'MG          '             TO   EMI-UF-RISCO.
      *
      *--*
      *
           IF  V0PREM-TIPO-OPER = 1104 OR 1105 OR 1114 OR 1115 OR
               V0PREM-TIPO-OPER = 1164 OR 1165 OR 1174 OR 1175
               MOVE    V0PREM-DTMOVTO     TO        WDATA-AUX
           ELSE
               MOVE    V0PREM-DTEMIS      TO        WDATA-AUX.
      *
           MOVE     WDAT-AUX-ANO         TO         WDAT-DBF-ANO.
           MOVE     WDAT-AUX-MES         TO         WDAT-DBF-MES.
           MOVE     WDAT-AUX-DIA         TO         WDAT-DBF-DIA.
      *
           MOVE     WDATA-DBF            TO         EMI-DT-EMIS
119167                                              CED-DT-EMIS.
      *
           MOVE     V0PREM-DTINIVIG      TO         WDATA-AUX.
           MOVE     WDAT-AUX-ANO         TO         WDAT-DBF-ANO.
           MOVE     WDAT-AUX-MES         TO         WDAT-DBF-MES.
           MOVE     WDAT-AUX-DIA         TO         WDAT-DBF-DIA.
           MOVE     WDATA-DBF            TO         EMI-DTINIVIG.
      *
           MOVE     V0PREM-DTTERVIG      TO         WDATA-AUX.
           MOVE     WDAT-AUX-ANO         TO         WDAT-DBF-ANO.
           MOVE     WDAT-AUX-MES         TO         WDAT-DBF-MES.
           MOVE     WDAT-AUX-DIA         TO         WDAT-DBF-DIA.
           MOVE     WDATA-DBF            TO         EMI-DTFIMVIG.
136184*
=          IF EMI-DT-PROPT  >  EMI-DTINIVIG
=             PERFORM  R0720-00-SELECT-DTINIVIG-AP
=             IF (V0PREM-RAMO-SUSEP EQUAL 0167 OR 0860 OR 0870
=                                      OR 1061 OR 1065 OR 1068)     OR
=                (V0PREM-RAMO-SUSEP EQUAL 0993                     AND
=                (V0PREM-NUM-APOL   EQUAL 109300000004 OR 109300000006
=                                      OR 109300000085 OR 109300000194
=                                      OR 109300000257 OR 109300000271
=                                      OR 109300000319 OR 109300000461
=                                      OR 109300000540 OR 109300000570
=                                      OR 109300000576 OR 109300000635))
=                 MOVE WHOST-DATPRO-AP         TO      WDATA-AUX
=                 MOVE WDAT-AUX-ANO            TO      WDAT-PROP-ANO
=                 MOVE WDAT-AUX-MES            TO      WDAT-PROP-MES
=                 MOVE WDAT-AUX-DIA            TO      WDAT-PROP-DIA
=                 IF   WDATA-PROP               <      EMI-DT-PROPT
=                      MOVE WHOST-DATPRO-AP    TO      WDATA-AUX
=                 ELSE
=                      MOVE WHOST-INIVIG-AP    TO      WDATA-AUX
=                 END-IF
=                 MOVE WDAT-AUX-ANO            TO      WDAT-DBF-ANO
=                 MOVE WDAT-AUX-MES            TO      WDAT-DBF-MES
=                 MOVE WDAT-AUX-DIA            TO      WDAT-DBF-DIA
=                 MOVE WDATA-DBF               TO      EMI-DT-PROPT
=             END-IF
136184     END-IF.
      *
      *--* OBTEM O VALOR DA IMPORTANCIA SEGURADA
      *
           IF  V0PREM-IMP-SEG-T = ZEROS
               MOVE     V0PREM-NUM-APOL TO V0COBA-NUM-APOL
               MOVE     V0PREM-NRENDOS  TO V0COBA-NRENDOS
               MOVE     V0PREM-RAMOFR   TO V0COBA-RAMOFR
               PERFORM  R1250-00-SELECT-V0COBERAPOL
               IF  V0PREM-MOEDA-IMP = 01
                   NEXT   SENTENCE
               ELSE
                   MOVE     V0PREM-MOEDA-IMP TO V0COTA-CODUNIMO
                   MOVE     V0PREM-DTINIVIG  TO V0COTA-DTINIVIG
                   PERFORM  R1260-00-SELECT-V0COTACAO
                   COMPUTE  V0PREM-IMP-SEG-T ROUNDED = V0PREM-IMP-SEG-T
                                                     * V0COTA-VALVEND
               END-IF
           END-IF.
      *
      *--* OBTEM OS DADOS PARA O CALCULO DA TARIFA BALCAO E DA
      *--* COMISSAO DO INDICADOR
141119*
=          MOVE       SPACES             TO        WTIP-REDE-AUX
141119                                             WTIP-REDE-BALC
146163                                             WTIP-RENOV-AUX
=                                                  WTIP-RENOV-BALC.
141119*
=          MOVE       ZEROS              TO        WPCT-TARF-BALC
=                                                  WVLR-TARF-BALC
=                                                  WPCT-COMS-INDC
=                                                  WVLR-COMS-INDC.
=     *
=          IF (V0PREM-NUMBIL =  ZEROS  AND
=             (WCOD-GRUPO    =  09  OR
=              V0PREM-RAMO   =  77  OR
=              WTEM-APOL-EF  = 'S'))
=              NEXT   SENTENCE
=          ELSE
=              IF  V0PREM-DTINIVIG  >  '2013-12-31'
=                  PERFORM  R1270-00-CALL-GE0009S
=              ELSE
=                  MOVE     '4'      TO   LKGE10-TIPO-FUNCIO
=                  PERFORM  R1280-00-CALL-GE0010S
=              END-IF
141119         MOVE     WTIP-REDE-AUX  TO  WTIP-REDE-BALC
146163         MOVE     WTIP-RENOV-AUX TO  WTIP-RENOV-BALC
141119         MOVE    '1'             TO  LKGE10-TIPO-FUNCIO
=              PERFORM  R1280-00-CALL-GE0010S
=          END-IF.
141119*
      *--* SALVA INFORMACOES PARA COSSEGURO
      *
           MOVE     V0PREM-ORGAO         TO         WCOD-ORGAO-ANT.
           MOVE     V0PREM-TIPSGU        TO         WTIP-SEGUR-ANT.
           MOVE     V0PREM-TPCOSCED      TO         WTIP-COSCED-ANT.
      *
           MOVE     V0PREM-NUM-APOL      TO         WHOST-NUM-APOL.
           MOVE     V0PREM-NRENDOS       TO         WHOST-NUM-ENDS.
           MOVE     V0PREM-RAMOFR        TO         WHOST-RMO-COBT.
           MOVE     V0PREM-DTINIVIG      TO         WHOST-DTINIVIG.
      *
      *--* ACUMULA OS VALORES DE PREMIO
      *
           INITIALIZE   WS-CHAVE-ANT
                        WS-ACUMULADOR.
      *
           MOVE     V0PREM-IMP-SEG-T     TO         ACC-IMP-SEGR-T.
           MOVE     V0PREM-IMP-SEG-C     TO         ACC-IMP-SEGR-C.
      *
           MOVE     V0PREM-RAMO-SUSEP    TO         CHVANT-RAMO-SUSEP.
           MOVE     V0PREM-TIPO-MOVT     TO         CHVANT-TIPO-MOVT.
           MOVE     V0PREM-TIPO-ENDS     TO         CHVANT-TIPO-ENDS.
           MOVE     V0PREM-DTMOVTO       TO         CHVANT-DATA-MOVT.
           MOVE     V0PREM-NUM-APOL      TO         CHVANT-NUM-APOL.
           MOVE     V0PREM-NRENDOCA      TO         CHVANT-NRENDOCA.
           MOVE     V0PREM-NRENDOS       TO         CHVANT-NRENDOS.
           MOVE     V0PREM-TIPO-OPER     TO         CHVANT-TIPO-OPER.
      *
136071     MOVE     V0PREM-CODPRODU      TO         CHVANT-CODPRODU.
=          MOVE     V0PREM-NRPARCEL      TO         CHVANT-NRPARCEL.
136071     MOVE     V0PREM-CANAL-VENDA   TO         CHVANT-CANAL-VD.
136081     MOVE     V0PREM-NUMBIL        TO         CHVANT-NUM-BILH.
      *
146365     MOVE     V0PREM-DTEMIS        TO         CHVANT-DAT-EMIS.
146365     MOVE     V0PREM-OPERACAO      TO         CHVANT-OPERACAO.
      *
148834     MOVE     V0PREM-RAMOFR        TO         CHVANT-RAMO-CBT.
=          MOVE     V0PREM-MODALIFR      TO         CHVANT-MODL-CBT.
=          MOVE     V0PREM-DTINIVIG      TO         CHVANT-DTINIVIG.
148834     MOVE     V0PREM-DTTERVIG      TO         CHVANT-DTTERVIG.
      *
           PERFORM  R1300-00-ACUMULA-VALORES UNTIL
                    WFIM-V0PREMIOS       NOT EQUAL  SPACES            OR
                    V0PREM-RAMO-SUSEP    NOT EQUAL  CHVANT-RAMO-SUSEP OR
                    V0PREM-TIPO-MOVT     NOT EQUAL  CHVANT-TIPO-MOVT  OR
                    V0PREM-TIPO-ENDS     NOT EQUAL  CHVANT-TIPO-ENDS  OR
                    V0PREM-DTMOVTO       NOT EQUAL  CHVANT-DATA-MOVT  OR
                    V0PREM-NUM-APOL      NOT EQUAL  CHVANT-NUM-APOL   OR
                    V0PREM-NRENDOCA      NOT EQUAL  CHVANT-NRENDOCA   OR
                    V0PREM-NRENDOS       NOT EQUAL  CHVANT-NRENDOS    OR
                    V0PREM-TIPO-OPER     NOT EQUAL  CHVANT-TIPO-OPER.
      *
           IF  CHVANT-TIPO-OPER  =  1114 OR 1174
               COMPUTE   ACC-IMP-SEGR-T   =   ACC-IMP-SEGR-T  *  -1
               COMPUTE   ACC-VLPRMTAR-T   =   ACC-VLPRMTAR-T  *  -1
               COMPUTE   ACC-VLDESCON-T   =   ACC-VLDESCON-T  *  -1
               COMPUTE   ACC-VLPRMLIQ-T   =   ACC-VLPRMLIQ-T  *  -1
               COMPUTE   ACC-VLADIFRA-T   =   ACC-VLADIFRA-T  *  -1
               COMPUTE   ACC-VLCUSEMI-T   =   ACC-VLCUSEMI-T  *  -1
               COMPUTE   ACC-VLIOCC-T     =   ACC-VLIOCC-T    *  -1
               COMPUTE   ACC-VLPRMTOT-T   =   ACC-VLPRMTOT-T  *  -1
               COMPUTE   ACC-VLADMN-T     =   ACC-VLADMN-T    *  -1
               COMPUTE   ACC-VLAGENC-T    =   ACC-VLAGENC-T   *  -1
               COMPUTE   ACC-VLCOMIS-T    =   ACC-VLCOMIS-T   *  -1
               COMPUTE   ACC-IMP-SEGR-C   =   ACC-IMP-SEGR-C  *  -1
               COMPUTE   ACC-VLPRMTAR-C   =   ACC-VLPRMTAR-C  *  -1
               COMPUTE   ACC-VLDESCON-C   =   ACC-VLDESCON-C  *  -1
               COMPUTE   ACC-VLPRMLIQ-C   =   ACC-VLPRMLIQ-C  *  -1
               COMPUTE   ACC-VLCOMIS-C    =   ACC-VLCOMIS-C   *  -1
148834         COMPUTE   ACC-VLPRMTAR-L   =   ACC-VLPRMTAR-L  *  -1
148834         COMPUTE   ACC-VLPRMTAR-R   =   ACC-VLPRMTAR-R  *  -1
           END-IF.
      *
           MOVE     ACC-VLPRMLIQ-T       TO         EMI-PR-EMIT.
           MOVE     ACC-VLPRMLIQ-C       TO         EMI-PR-COS-CED.
           MOVE     ACC-VLADIFRA-T       TO         EMI-AD-FRAC.
           MOVE     ACC-VLCUSEMI-T       TO         EMI-CUST-APOL.
           MOVE     ACC-VLIOCC-T         TO         EMI-VLR-IOF.
           MOVE     ACC-VLCOMIS-T        TO         EMI-VLR-COMIS.
           MOVE     ACC-VLCOMIS-C        TO         EMI-COMIS-COSS.
235637     MOVE     ACC-VLADMN-T         TO         EMI-PRO-LAB.
235637     MOVE     ACC-VLAGENC-T        TO         EMI-VLR-AGENC.
      *
           MOVE     ACC-IMP-SEGR-T       TO         EMI-IMP-SEG.
      *
           MOVE     CHVANT-CODPRODU      TO         EMI-COD-PRODU.
      *
117159*--* CAMPOS DESPREZADOS NO LAYOUT DO ARQUIVO PREMIT
=     *
=          MOVE     ZEROS                 TO        EMI-CERT-FIL
=                                                   EMI-NUM-CERTIF
=                                                   EMI-CPF-PARCEIRO
=                                                   EMI-DTINIVIG-CRTF
=                                                   EMI-DTTERVIG-CRTF.
=     *
=          MOVE      SPACES               TO        EMI-ORIGEM-REG.
117159*
      *--* MOVIMENTA O NUMERO DA PARCELA E CANAL DE VENDA
136071*
=          IF  CHVANT-NRPARCEL  =  ZEROS
=              MOVE  01                     TO      EMI-NUM-PARCEL
=          ELSE
=              MOVE  CHVANT-NRPARCEL        TO      EMI-NUM-PARCEL
=          END-IF.
=     *
=          MOVE      CHVANT-CANAL-VD        TO      EMI-CANAL-VNDA.
136071*
      *--* OBTEM E MOVIMENTA A MATRICULA, NOME E CPF DO INDICADOR
136081*
=          IF  CHVANT-NUM-BILH  =  ZEROS
=              MOVE     CHVANT-NUM-APOL      TO     APOLCOBR-NUM-APOL
=              MOVE     CHVANT-NRENDOS       TO     APOLCOBR-NUM-ENDS
=              PERFORM  R1400-00-SELECT-APOL-COBR
=          ELSE
=              MOVE     CHVANT-NUM-BILH      TO     BILHETE-NUM-BILH
=              PERFORM  R1500-00-SELECT-BILHETE
=          END-IF.
=     *
=          MOVE      FUNCICEF-NUM-MATR       TO     EMI-NUM-MATRIC.
=     *
=          IF  FUNCICEF-NUM-MATR  =  ZEROS
=              MOVE     SPACES               TO     FUNCICEF-NOM-FUNC
=              MOVE     ZEROS                TO     FUNCICEF-NUM-CPF
=          ELSE
=              PERFORM  R1600-00-SELECT-FUNCIO-CEF
=          END-IF.
=     *
=          MOVE      FUNCICEF-NOM-FUNC       TO     EMI-NOME-AGENC.
=          MOVE      FUNCICEF-NUM-CPF        TO     EMI-NUM-CPF.
136081*
      *--* MOVIMENTACAO DO PREMIO TARIFARIO
139415*
=          MOVE      ACC-VLPRMTAR-T          TO     EMI-VLPRM-TARIFA.
=     *
=     *--* CALCULO E MOVIMENTACAO DA TARIFA BALCAO
=     *
=          IF  WPCT-TARF-BALC  NOT EQUAL  ZEROS
=              COMPUTE  WVLR-TARF-BALC ROUNDED = (ACC-VLPRMTAR-T
=                                              *  WPCT-TARF-BALC) / 100
=          END-IF.
=     *
139415     MOVE      WTIP-REDE-BALC          TO     EMI-TIPO-REDE.
146163     MOVE      WTIP-RENOV-BALC         TO     EMI-TIPO-RENOV.
139415     MOVE      WPCT-TARF-BALC          TO     EMI-PCTAR-BALCAO.
=          MOVE      WVLR-TARF-BALC          TO     EMI-VLTAR-BALCAO.
139415*
      *--* CALCULO E MOVIMENTACAO DA COMISSAO DO INDICADOR
141119*
=          IF  WPCT-COMS-INDC  NOT EQUAL  ZEROS
=              COMPUTE  WVLR-COMS-INDC ROUNDED = (ACC-VLPRMTAR-T
=                                              *  WPCT-COMS-INDC) / 100
=          END-IF.
=     *
=          MOVE      WPCT-COMS-INDC          TO     EMI-PCCOM-INDICD.
=          MOVE      WVLR-COMS-INDC          TO     EMI-VLCOM-INDICD.
141119*
      *--* TRATA MOTIVO DO CANCELAMENTO
146365*
=          IF  WPCT-TARF-BALC  NOT EQUAL  ZEROS  AND
=             (CHVANT-OPERACAO  =  401 OR 402 OR 403 OR 404)
=              MOVE      CHVANT-DAT-EMIS  TO     WDATA-AUX
=              MOVE      WDAT-AUX-ANO     TO     WDAT-DBF-ANO
=              MOVE      WDAT-AUX-MES     TO     WDAT-DBF-MES
=              MOVE      WDAT-AUX-DIA     TO     WDAT-DBF-DIA
235637*        MOVE      WDATA-DBF        TO     EMI-DATA-EMISS
=     *
=              MOVE      CHVANT-DATA-MOVT TO     WDATA-AUX
=              MOVE      WDAT-AUX-ANO     TO     WDAT-DBF-ANO
=              MOVE      WDAT-AUX-MES     TO     WDAT-DBF-MES
=              MOVE      WDAT-AUX-DIA     TO     WDAT-DBF-DIA
235637*        MOVE      WDATA-DBF        TO     EMI-DATA-CANCL
=     *
235637*        MOVE      CHVANT-OPERACAO  TO     EMI-COD-OPERAC
235637*        MOVE      SPACES           TO     EMI-DESC-OPERC
=     *
235637*        IF  CHVANT-OPERACAO  =  401
=     *            MOVE 'CANCELAMENTO AUTOMATICO POR FALTA DE PAGTO' TO
=     *            EMI-DESC-OPERC
=     *        END-IF
=     *        IF  CHVANT-OPERACAO  =  402
=     *            MOVE 'CANCELAMENTO MANUAL POR ERRO DE EMISSAO   ' TO
=     *            EMI-DESC-OPERC
=     *        END-IF
=     *        IF  CHVANT-OPERACAO  =  403
=     *            MOVE 'CANCELAMENTO MANUAL POR FALTA DE PAGTO    ' TO
=     *            EMI-DESC-OPERC
=     *        END-IF
=     *        IF  CHVANT-OPERACAO  =  404
=     *            MOVE 'CANCELAMENTO POR ENDOSSO COM RESTITUICAO  ' TO
=     *            EMI-DESC-OPERC
=     *        END-IF
=     *    ELSE
235637*        MOVE ZEROS                 TO        EMI-DATA-EMISS
235637*                                             EMI-DATA-CANCL
235637*                                             EMI-COD-OPERAC
235637*        MOVE SPACES                TO        EMI-DESC-OPERC
235637*    END-IF.
146365*
148834*--* OBTEM OS PERCENTUAIS DE RESSEGURO
=     *
=          MOVE      CHVANT-RAMO-CBT           TO    EMI-RAMO-COBERT.
=          MOVE      CHVANT-MODL-CBT           TO    EMI-MODL-COBERT.
=     *
=          IF (ACC-VLPRMTAR-R  =  ZEROS   AND
=              ACC-VLPRMTAR-L  NOT EQUAL  ZEROS)
=              MOVE   ZEROS                    TO    WPCT-QUOTA
=              MOVE   ZEROS                    TO    WPCT-COM-QUOTA
=          ELSE
=              PERFORM  R1700-00-PROCESSA-RESSEGURO
=          END-IF.
=     *
=          MOVE      WPCT-QUOTA                TO    EMI-PCT-QUOTA-R.
=          MOVE      WPCT-COM-QUOTA            TO    EMI-COM-QUOTA-R.
148834*
198785*
=          INITIALIZE  DCLBILHETE
=                      DCLPROPOSTA-FIDELIZ.
=     *
=          IF CHVANT-NUM-BILH NOT EQUAL ZEROS
=             EXEC SQL
=               SELECT  NUM_PROPOSTA_SIVPF
=                 INTO :NUM-PROPOSTA-SIVPF
=                 FROM  SEGUROS.PROPOSTA_FIDELIZ
=                WHERE  NUM_SICOB       =       :CHVANT-NUM-BILH
=             END-EXEC
235637*       IF SQLCODE EQUAL ZEROS
=     *          MOVE  NUM-PROPOSTA-SIVPF  TO  EMI-PROP-SIVPF
=     *       ELSE
=     *          MOVE  ZEROS               TO  EMI-PROP-SIVPF
=     *       END-IF
235637*
=          ELSE
=     *
=             EXEC SQL
=               SELECT  NUM_BILHETE
=                 INTO :BILHETE-NUM-BILHETE
=                 FROM  SEGUROS.BILHETE
=                WHERE  NUM_APOLICE   =  :V0PREM-NUM-APOL
=             END-EXEC
=             IF SQLCODE EQUAL ZEROS
=                EXEC SQL
=                  SELECT  NUM_PROPOSTA_SIVPF
=                    INTO :NUM-PROPOSTA-SIVPF
=                    FROM  SEGUROS.PROPOSTA_FIDELIZ
=                   WHERE  NUM_SICOB       =       :BILHETE-NUM-BILHETE
=                END-EXEC
235637*          IF SQLCODE EQUAL ZEROS
=     *             MOVE  NUM-PROPOSTA-SIVPF  TO  EMI-PROP-SIVPF
=     *          ELSE
=     *             MOVE  ZEROS               TO  EMI-PROP-SIVPF
=     *          END-IF
=             END-IF
=     *
=          END-IF.
198785*
235637*--* MOVE NUMBIL PARA O NUM-CERTIF (AO INVES DE MOVER ZEROS) E
=     *--* NOVOS CAMPOS NO LAYOUT DO ARQUIVO PREMIT.
=     *
=          MOVE     CHVANT-NUM-BILH        TO     EMI-NUM-CERTIF.
=          MOVE     ZEROS                  TO     EMI-DATA-AVERB
=                                                 EMI-DATA-INCLU
=                                                 EMI-DTFAT-PC01.
=     *
=          MOVE     CHVANT-DATA-MOVT       TO     WDATA-AUX
=          MOVE     WDAT-AUX-ANO           TO     WDAT-DBF-ANO
=          MOVE     WDAT-AUX-MES           TO     WDAT-DBF-MES
=          MOVE     WDAT-AUX-DIA           TO     WDAT-DBF-DIA
=
=          MOVE     WDATA-DBF              TO     EMI-DATA-MOVTO
=          MOVE     SPACES                 TO     EMI-SIT-CERTIF
=                                                 EMI-DES-SITUAC.
=          MOVE     ZEROS                  TO     EMI-DT-EMIS-END
=                                                 EMI-INI-VIG-END
235637                                            EMI-FIM-VIG-END.
      *
285991     MOVE      CHVANT-TIPO-OPER      TO     EMI-TIPO-OPER
=                                                 CED-TIPO-OPER.
      *
      *--* GRAVA O ARQUIVO PREMIT POR APOLICE E/OU ENDOSSO
      *
           WRITE     REG-PREMIT            FROM   REGT-PREMIT.
      *
           IF  EMI-STATUS  =  ZEROS
               ADD      1     TO      AC-G-PREMIT
           ELSE
               DISPLAY 'R0700 - ERRO NO WRITE DO ARQ PREMIT'
               DISPLAY 'STATUS      - '  EMI-STATUS
               DISPLAY 'ANO REFER   - '  V0RELA-ANO-REFER
               DISPLAY 'MES REFER   - '  V0RELA-MES-REFER
               DISPLAY 'RAMO SUSEP  - '  CHVANT-RAMO-SUSEP
               DISPLAY 'NR APOLICE  - '  CHVANT-NUM-APOL
               DISPLAY 'ENDS MOVTO  - '  CHVANT-NRENDOCA
               DISPLAY 'NR ENDOSSO  - '  CHVANT-NRENDOS
               DISPLAY 'TIPO ENDO   - '  CHVANT-TIPO-ENDS
               DISPLAY 'DATA MOVTO  - '  CHVANT-DATA-MOVT
               DISPLAY 'TIPO OPER   - '  CHVANT-TIPO-OPER
               GO   TO  R9999-00-ROT-ERRO.
      *
      *--* GRAVA REGISTRO DE COSSEGURO CEDIDO
      *
           IF  WTIP-SEGUR-ANT  =  '1'
               IF  WCOD-ORGAO-ANT  =  100  OR  110
                   PERFORM  R3000-00-GRAVA-COSSEG-CED
               ELSE
                   IF  ACC-VLPRMTAR-C  =  ZEROS   AND
                       ACC-VLCOMIS-C   =  ZEROS
                       NEXT   SENTENCE
                   ELSE
                       PERFORM  R3100-00-PROCESSA-COSG-CED
                   END-IF
               END-IF
           END-IF.
      *
       R0700-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R0720-00-SELECT-DTINIVIG-AP   SECTION.
      *--------------------------------------
      *
           MOVE           '072'            TO             WNR-EXEC-SQL.
      *
           EXEC  SQL
                 SELECT  DTINIVIG,
136184                   DATPRO
                   INTO :WHOST-INIVIG-AP,
136184                  :WHOST-DATPRO-AP
                   FROM  SEGUROS.V0ENDOSSO
C97168            WHERE  NUM_APOLICE     =   :V0PREM-NUM-APOL
                    AND  NRENDOS         =    0
                   WITH  UR
           END-EXEC.
      *
           IF  SQLCODE  NOT   EQUAL  ZEROS
               DISPLAY 'R0720 - ERRO NO SELECT DA V0ENDOSSO'
               DISPLAY 'APOLICE  - ' V0PREM-NUM-APOL
               DISPLAY 'ENDOSSO  - ' V0PREM-NRENDOS
               DISPLAY 'RAMO COB - ' V0PREM-RAMOFR
               GO   TO  R9999-00-ROT-ERRO.
      *
       R0720-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R0740-00-SELECT-V0PRODUTO     SECTION.
      *--------------------------------------
      *
           MOVE           '074'            TO             WNR-EXEC-SQL.
      *
           EXEC SQL     SELECT
                  VALUE(NUM_PROCESSO_SUSEP,' ')
                  INTO :V0PROD-NUM-PROCS
                  FROM  SEGUROS.V0PRODUTO
                 WHERE  CODPRODU        =   :V0PROD-CODPRODU
                  WITH  UR
           END-EXEC.
      *
           IF    SQLCODE  NOT  EQUAL  ZEROS
             IF  SQLCODE       EQUAL  100
                 MOVE     SPACES  TO  V0PROD-NUM-PROCS
             ELSE
                 DISPLAY 'R0740 - ERRO NO SELECT DA V0PRODUTO'
                 DISPLAY 'APOLICE  - ' V0PREM-NUM-APOL
                 DISPLAY 'ENDOSSO  - ' V0PREM-NRENDOS
                 DISPLAY 'RAMO COB - ' V0PREM-RAMOFR
                 DISPLAY 'PRODUTO  - ' V0PROD-CODPRODU
                 GO   TO  R9999-00-ROT-ERRO.
      *
       R0740-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R0760-00-SELECT-V0ENDOSSO     SECTION.
      *--------------------------------------
      *
           MOVE          '076'             TO             WNR-EXEC-SQL.
      *
           EXEC  SQL
                 SELECT  NUM_APOLICE,
                         NRENDOS,
                         NRPROPOS,
                         DATPRO,
                         BCORCAP,
                         AGERCAP,
                         TIPO_ENDOSSO,
                         OCORR_ENDERECO
                   INTO :V0ENDO-NUM-APOL,
                        :V0ENDO-NRENDOS,
                        :V0ENDO-NRPROPOS,
                        :V0ENDO-DATPRO,
                        :V0ENDO-BCORCAP,
                        :V0ENDO-AGERCAP,
                        :V0ENDO-TIPO-ENDS,
                        :V0ENDO-OCOR-ENDR
                   FROM  SEGUROS.V0ENDOSSO
                  WHERE  NUM_APOLICE     =   :V0ENDO-NUM-APOL
                    AND  NRENDOS         =   :V0ENDO-NRENDOS
                   WITH  UR
           END-EXEC.
      *
           IF  SQLCODE  NOT   EQUAL  ZEROS
               DISPLAY 'R0760 - ERRO NO SELECT DA V0ENDOSSO'
               DISPLAY 'APOLICE  - ' V0ENDO-NUM-APOL
               DISPLAY 'ENDOSSO  - ' V0ENDO-NRENDOS
               GO   TO  R9999-00-ROT-ERRO.
      *
       R0760-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R0780-00-SELECT-ENDOS-CANCLM  SECTION.
      *--------------------------------------
      *
           MOVE           '078'            TO             WNR-EXEC-SQL.
      *
           EXEC  SQL
                 SELECT  NUM_APOLICE,
                         NRENDOS,
                         DTEMIS,
                         TIPO_ENDOSSO
                   INTO :V0ENDS-NUM-APOL,
                        :V0ENDS-NRENDOS,
                        :V0ENDS-DTEMIS,
                        :V0ENDS-TIPO-ENDO
                   FROM  SEGUROS.V0ENDOSSO
                  WHERE  NUM_APOLICE     =    :V0ENDS-NUM-APOL
                    AND  NRENDOS         >     00
                    AND  DTEMIS          =    :V0ENDS-DTEMIS
                    AND  TIPO_ENDOSSO    =    '5'
                    AND  SITUACAO       IN   ('0','1','2')
                   WITH  UR
           END-EXEC.
      *
           IF    SQLCODE  NOT  EQUAL  ZEROS
             IF  SQLCODE       EQUAL  100
                 MOVE     ZEROS   TO  V0ENDS-NRENDOS
                 MOVE     SPACES  TO  V0ENDS-DTEMIS
                 MOVE     SPACES  TO  V0ENDS-TIPO-ENDO
             ELSE
                 DISPLAY 'R0780 - ERRO NO SELECT DA V0ENDOSSO'
                 DISPLAY 'APOLICE  - ' V0ENDO-NUM-APOL
                 DISPLAY 'ENDOSSO  - ' V0ENDO-NRENDOS
                 DISPLAY 'DT EMIS  - ' V0ENDS-DTEMIS
                 GO   TO  R9999-00-ROT-ERRO
           ELSE
             IF  V0ENDS-TIPO-ENDO  NOT  EQUAL  '5'
                 DISPLAY 'R0780 - TP END INVALIDO PARA ENDS DE CANCELM'
                 DISPLAY 'APOLICE  - ' V0ENDO-NUM-APOL
                 DISPLAY 'ENDOSSO  - ' V0ENDO-NRENDOS
                 DISPLAY 'DT EMIS  - ' V0ENDS-DTEMIS
                 GO   TO  R9999-00-ROT-ERRO.
      *
       R0780-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R0800-00-SELECT-V0HISTOPARC    SECTION.
      *---------------------------------------
      *
           MOVE            '080'            TO            WNR-EXEC-SQL.
      *
           EXEC  SQL     SELECT
                   VALUE(SUM(VLPRMTOT),+0)
                   INTO :V0HISP-VLPRMTOT
                   FROM  SEGUROS.V0HISTOPARC
                  WHERE  NUM_APOLICE       =   :V0ENDS-NUM-APOL
                    AND  NRENDOS           =   :V0ENDS-NRENDOS
                    AND  OPERACAO          <    0200
                    AND  OCORHIST          =    01
                   WITH  UR
           END-EXEC.
      *
           IF    SQLCODE  NOT  EQUAL  ZEROS
             IF  SQLCODE       EQUAL  100
                 MOVE     ZEROS   TO  V0HISP-VLPRMTOT
             ELSE
                 DISPLAY 'R0800 - ERRO NO SELECT DA V0HISTOPARC'
                 DISPLAY 'APOLICE  - ' V0ENDS-NUM-APOL
                 DISPLAY 'ENDOSSO  - ' V0ENDS-NRENDOS
                 GO   TO  R9999-00-ROT-ERRO.
      *
       R0800-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R0820-00-SELECT-QTD-DOCT-CANC  SECTION.
      *---------------------------------------
      *
           MOVE            '082'            TO            WNR-EXEC-SQL.
      *
           EXEC  SQL     SELECT
                   VALUE(COUNT(*),+0)
                   INTO :WHOST-QTD-DOCT
                   FROM  SEGUROS.V0HISTOPARC
                  WHERE  NUM_APOLICE       =   :V0HISP-NUM-APOL
                    AND  NRENDOS          <>   :V0HISP-NRENDOS
                    AND  OPERACAO          =   :V0HISP-OPERACAO
                    AND  DTMOVTO           =   :V0HISP-DTMOVTO
                    AND  NRENDOCA          =   :V0HISP-NRENDOCA
                   WITH  UR
           END-EXEC.
      *
           IF    SQLCODE  NOT  EQUAL  ZEROS
             IF  SQLCODE       EQUAL  100
                 MOVE     ZEROS   TO  WHOST-QTD-DOCT
             ELSE
                 DISPLAY 'R0820 - ERRO NO SELECT DA V0HISTOPARC'
                 DISPLAY 'APOLICE  - ' V0HISP-NUM-APOL
                 DISPLAY 'ENDOSSO  - ' V0HISP-NRENDOS
                 DISPLAY 'OPERACAO - ' V0HISP-OPERACAO
                 DISPLAY 'DT MOVTO - ' V0HISP-DTMOVTO
                 DISPLAY 'END CANC - ' V0HISP-NRENDOCA
                 GO   TO  R9999-00-ROT-ERRO.
      *
       R0820-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R0840-00-SELECT-ENDS-CANCELM  SECTION.
      *--------------------------------------
      *
           MOVE           '084'            TO             WNR-EXEC-SQL.
      *
           EXEC  SQL
                 SELECT  NUM_APOLICE,
                         NRENDOS,
                         DTEMIS,
                         TIPO_ENDOSSO
                   INTO :V0ENDS-NUM-APOL,
                        :V0ENDS-NRENDOS,
                        :V0ENDS-DTEMIS,
                        :V0ENDS-TIPO-ENDO
                   FROM  SEGUROS.V0ENDOSSO
                  WHERE  NUM_APOLICE     =    :V0ENDS-NUM-APOL
                    AND  NRENDOS         =    :V0ENDS-NRENDOS
                    AND  TIPO_ENDOSSO    =    '5'
                    AND  SITUACAO       IN   ('0','1','2')
                   WITH  UR
           END-EXEC.
      *
           IF    SQLCODE  NOT  EQUAL  ZEROS
             IF  SQLCODE       EQUAL  100
                 MOVE     SPACES  TO  V0ENDS-DTEMIS
                 MOVE     SPACES  TO  V0ENDS-TIPO-ENDO
             ELSE
                 DISPLAY 'R0840 - ERRO NO SELECT DA V0ENDOSSO'
                 DISPLAY 'APOLICE  - ' V0ENDS-NUM-APOL
                 DISPLAY 'ENDOSSO  - ' V0ENDS-NRENDOS
                 GO   TO  R9999-00-ROT-ERRO
           ELSE
             IF  V0ENDS-TIPO-ENDO  NOT  EQUAL  '5'
                 DISPLAY 'R0840 - TP END INVALIDO PARA ENDS DE CANCELM'
                 DISPLAY 'APOLICE  - ' V0ENDS-NUM-APOL
                 DISPLAY 'ENDOSSO  - ' V0ENDS-NRENDOS
                 GO   TO  R9999-00-ROT-ERRO.
      *
       R0840-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R0850-00-SELECT-V0COBERAPOL   SECTION.
      *--------------------------------------
      *
           MOVE          '850'             TO             WNR-EXEC-SQL.
      *
           EXEC  SQL   SELECT
                 VALUE(SUM(PRM_TARIFARIO_IX),+0),
                 VALUE(SUM(PRM_TARIFARIO_VAR),+0)
                 INTO :V0COBA-PRM-TAR-IX,
                      :V0COBA-PRM-TAR-VR
                 FROM  SEGUROS.V0COBERAPOL
                WHERE  NUM_APOLICE       =    :V0COBA-NUM-APOL
                  AND  NRENDOS           =    :V0COBA-NRENDOS
                  AND  RAMOFR            =    :V0COBA-RAMOFR
                  AND  NUM_ITEM          =     0
                  AND  COD_COBERTURA     =     0
                 WITH  UR
           END-EXEC.
      *
           IF    SQLCODE  NOT  EQUAL  ZEROS
             IF  SQLCODE       EQUAL  100
                 MOVE   ZEROS         TO     V0COBA-PRM-TAR-IX
                                             V0COBA-PRM-TAR-VR
             ELSE
                 DISPLAY 'R0850 - ERRO NO SELECT DA V0COBERAPOL'
                 DISPLAY 'APOLICE  - ' V0COBA-NUM-APOL
                 DISPLAY 'ENDOSSO  - ' V0COBA-NRENDOS
                 DISPLAY 'RAMO COB - ' V0COBA-RAMOFR
                 GO   TO  R9999-00-ROT-ERRO.
      *
       R0850-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R0860-00-SELECT-V0FONTE        SECTION.
      *---------------------------------------
      *
           MOVE            '086'            TO            WNR-EXEC-SQL.
      *
           EXEC  SQL
                 SELECT  FONTE ,
                         ESTADO
                   INTO :V0FONT-COD-FONTE ,
                        :V0FONT-ESTADO
                   FROM  SEGUROS.V0FONTE
                  WHERE  FONTE         =    :V0FONT-COD-FONTE
                   WITH  UR
           END-EXEC.
      *
           IF    SQLCODE  NOT  EQUAL  ZEROS
             IF  SQLCODE       EQUAL  100
                 MOVE     SPACES  TO  V0FONT-ESTADO
             ELSE
                 DISPLAY 'R0860 - ERRO NO SELECT DA V0FONTE'
                 DISPLAY 'APOLICE   - ' V0PREM-NUM-APOL
                 DISPLAY 'ENDOSSO   - ' V0PREM-NRENDOS
                 DISPLAY 'RAMO COB  - ' V0PREM-RAMOFR
                 DISPLAY 'COD FONTE - ' V0FONT-COD-FONTE
                 GO   TO  R9999-00-ROT-ERRO.
      *
       R0860-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R0880-00-SELECT-V0AUTOAPOL    SECTION.
      *--------------------------------------
      *
           MOVE          '088'             TO             WNR-EXEC-SQL.
      *
           EXEC  SQL
                 SELECT  DISTINCT
                         A.NUM_PROPOSTA_CONV,
                         B.DTH_OPERACAO
                   INTO :V0AUTA-NRPROP-C,
                        :AU055-DTH-OPERAC
                   FROM  SEGUROS.V0AUTOAPOL       A,
                         SEGUROS.AU_HIS_PROP_CONV B
                  WHERE  A.NUM_APOLICE      =     :V0AUTA-NUM-APOL
                    AND  A.NRENDOS          =     :V0AUTA-NRENDOS
                    AND  A.SITUACAO        <>     '2'
                    AND  B.NUM_PROPOSTA_VC  =      A.NUM_PROPOSTA_CONV
                    AND  B.DTH_OPERACAO     =
                (SELECT  MIN(C.DTH_OPERACAO)
                   FROM  SEGUROS.AU_HIS_PROP_CONV C
                  WHERE  C.NUM_PROPOSTA_VC  =     B.NUM_PROPOSTA_VC)
               GROUP BY  A.NUM_PROPOSTA_CONV,
                         B.DTH_OPERACAO
               ORDER BY  A.NUM_PROPOSTA_CONV,
                         B.DTH_OPERACAO
                   WITH  UR
           END-EXEC.
      *
           IF  SQLCODE  NOT   EQUAL  ZEROS
               DISPLAY 'R0880 - ERRO NO SELECT DA V0AUTOAPOL'
               DISPLAY 'APOLICE  - ' V0AUTA-NUM-APOL
               DISPLAY 'ENDOSSO  - ' V0AUTA-NRENDOS
               GO   TO  R9999-00-ROT-ERRO
           ELSE
               MOVE     V0AUTA-NRPROP-C   TO   EMI-NR-PROPT
               MOVE     V0AUTA-NRPROP-C   TO   CED-NR-PROPT
           END-IF.
      *
       R0880-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R0900-00-SELECT-V0AUTOPROP   SECTION.
      *-------------------------------------
      *
           MOVE           '090'           TO               WNR-EXEC-SQL.
      *
           EXEC  SQL
              SELECT  B.DTH_PROP_EFETIVACAO
                INTO :V0AUPR-DTH-EFETV:VIND-DTH-EFETV
                FROM  SEGUROS.V0AUTOAPOL A,
                      SEGUROS.V0AUTOPROP B
               WHERE  A.NUM_APOLICE      =     :V0AUTA-NUM-APOL
                 AND  A.NRENDOS          =     :V0AUTA-NRENDOS
                 AND  A.SITUACAO        <>     '2'
                 AND  B.FONTE            =     A.FONTE
                 AND  B.NRPROPOS         =     A.NRPROPOS
                 AND  B.NRITEM           =     A.NRITEM
                WITH  UR
           END-EXEC.
      *
           IF    SQLCODE  NOT  EQUAL  ZEROS
             IF  SQLCODE       EQUAL  100
                 MOVE     SPACES  TO  V0AUPR-DTH-EFETV
             ELSE
                 DISPLAY 'R0900 - ERRO NO SELECT DA V0AUTOPROP'
                 DISPLAY 'APOLICE  - ' V0AUTA-NUM-APOL
                 DISPLAY 'ENDOSSO  - ' V0AUTA-NRENDOS
                 GO   TO  R9999-00-ROT-ERRO
             END-IF
           ELSE
             IF  VIND-DTH-EFETV  <  ZEROS
                 MOVE   SPACES  TO  V0AUPR-DTH-EFETV
             END-IF
           END-IF.
      *
       R0900-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
105223*
=      R0920-00-LT-MOV-PROPOSTA       SECTION.
=     *---------------------------------------
=     *
=          MOVE            '092'            TO            WNR-EXEC-SQL.
=     *
=          EXEC  SQL
=             SELECT  A.NUM_TITULO
=               INTO :LTMVPROP-NUM-TITULO
=               FROM  SEGUROS.LT_MOV_PROPOSTA A,
=                     SEGUROS.LOTERICO01      B
=              WHERE  A.NUM_APOLICE      =    B.NUM_APOLICE
=                AND  A.COD_MOVIMENTO    =   '9'
=                AND  A.SIT_MOVIMENTO    =   '1'
=                AND  A.NUM_APOLICE      =   :V0PREM-NUM-APOL
=                AND  A.NUM_ENDOSSO      =    0
=                AND  B.DTTERVIG         =
=            (SELECT  MAX(C.DTTERVIG)
=               FROM  SEGUROS.LOTERICO01 C
=              WHERE  A.NUM_APOLICE      =    C.NUM_APOLICE
=                AND  C.NUM_APOLICE      =   :V0PREM-NUM-APOL)
=               WITH  UR
=          END-EXEC.
=     *
=          IF    SQLCODE  NOT  EQUAL  ZEROS
=            IF  SQLCODE       EQUAL  100
=                MOVE     ZEROS   TO  LTMVPROP-NUM-TITULO
=            ELSE
=                DISPLAY 'R0920 - ERRO NO SELECT DA LT_MOV_PROPOSTA'
=                DISPLAY 'APOLICE  - ' V0PREM-NUM-APOL
=                DISPLAY 'ENDOSSO  - ' V0PREM-NRENDOS
=                DISPLAY 'RAMO COB - ' V0PREM-RAMOFR
=                GO   TO  R9999-00-ROT-ERRO
=            END-IF
=          END-IF.
=     *
=      R0920-99-SAIDA.
=          EXIT.
105223*
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
112349*
=      R0940-00-AU-PROP-CONV-VC       SECTION.
=     *---------------------------------------
=     *
=          MOVE            '094'            TO            WNR-EXEC-SQL.
=     *
=          EXEC  SQL
=                SELECT  NUM_PROPOSTA_VC
=                  INTO :AU057-NUM-PROPOSTA-VC
=                  FROM  SEGUROS.AU_PROP_CONV_VC
=                 WHERE  NUM_APOLICE    =    :V0PREM-NUM-APOL
=                   AND  NUM_ENDOSSO    =    :V0PREM-NRENDOS
=                WITH UR
=          END-EXEC.
=     *
=          IF    SQLCODE  NOT  EQUAL  ZEROS
=            IF  SQLCODE       EQUAL  100
=                MOVE     ZEROS   TO  AU057-NUM-PROPOSTA-VC
=            ELSE
=                DISPLAY 'R0940 - ERRO NO SELECT DA AU_PROP_CONV_VC'
=                DISPLAY 'APOLICE  - ' V0PREM-NUM-APOL
=                DISPLAY 'ENDOSSO  - ' V0PREM-NRENDOS
=                DISPLAY 'RAMO COB - ' V0PREM-RAMOFR
=                GO   TO  R9999-00-ROT-ERRO
=            END-IF
=          END-IF.
=     *
=      R0940-99-SAIDA.
=          EXIT.
112349*
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R0960-00-SELECT-V0CLIENTE     SECTION.
      *--------------------------------------
      *
           MOVE          '096'             TO             WNR-EXEC-SQL.
      *
           EXEC  SQL
                 SELECT  COD_CLIENTE,
                         TIPO_PESSOA,
                         CGCCPF
                   INTO :V0CLIE-CODCLIEN,
                        :V0CLIE-TIP-PESS,
                        :V0CLIE-CGC-CPF
                   FROM  SEGUROS.V0CLIENTE
                  WHERE  COD_CLIENTE     =   :V0CLIE-CODCLIEN
                   WITH  UR
           END-EXEC.
      *
           IF  SQLCODE  NOT   EQUAL  ZEROS
               DISPLAY 'R0960 - ERRO NO SELECT DA V0CLIENTE'
               DISPLAY 'CLIENTE  - ' V0CLIE-CODCLIEN
               GO   TO  R9999-00-ROT-ERRO.
      *
       R0960-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R0980-00-SELECT-V0APOLICE     SECTION.
      *--------------------------------------
      *
           MOVE          '098'             TO             WNR-EXEC-SQL.
      *
           EXEC  SQL
                 SELECT  NUM_APOLICE,
                         CODCLIEN,
                         NUMBIL,
                         TIPSGU
                   INTO :V0APOL-NUM-APOL,
                        :V0APOL-CODCLIEN,
                        :V0APOL-NUM-BILH,
                        :V0APOL-TIPSGU
                   FROM  SEGUROS.V0APOLICE
                  WHERE  NUM_APOLICE     =   :V0APOL-NUM-APOL
                   WITH  UR
           END-EXEC.
      *
           IF  SQLCODE  NOT   EQUAL  ZEROS
               DISPLAY 'R0980 - ERRO NO SELECT DA V0APOLICE'
               DISPLAY 'APOLICE  - ' V0APOL-NUM-APOL
               GO   TO  R9999-00-ROT-ERRO
           ELSE
               MOVE     V0APOL-CODCLIEN  TO  V0CLIE-CODCLIEN.
      *
       R0980-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R0990-00-SELECT-EF-APOLICE  SECTION.
      *------------------------------------
      *
           MOVE          '099'           TO               WNR-EXEC-SQL.
      *
           EXEC  SQL  SELECT
            DISTINCT  A.NUM_APOLICE,
                      A.NUM_CONTRATO,
                      B.COD_PRODUTO
                INTO :EF063-NUM-APOL,
                     :EF063-NUM-CONTR,
                     :EF050-COD-PRODU
                FROM  SEGUROS.EF_APOLICE        A,
                      SEGUROS.EF_CONTRATO       B,
                      SEGUROS.EF_PROD_ACESSORIO C
C10158         WHERE  A.NUM_CONTRATO            =   B.NUM_CONTRATO
=                AND  B.NUM_CONTRATO            =   C.NUM_CONTRATO_APOL
=                AND  B.COD_PRODUTO             =   C.COD_PRODUTO
=                AND  B.COD_TIPO_CONTRATO       =   02
=                AND  C.NUM_APOLICE             =   :EF148-NUM-APOL
=                AND  C.NUM_RAMO_CONTABIL       =   :EF148-RAMO-CONTB
=                AND  C.COD_PRODUTO_ACESS       =   :EF148-PRODU-ACS
=                AND (:EF148-DTH-INIVIG   BETWEEN   C.DTH_INI_VIGENCIA
C10158           AND  VALUE(C.DTH_FIM_VIGENCIA,DATE('9999-12-31')))
            GROUP BY  A.NUM_APOLICE,
                      A.NUM_CONTRATO,
                      B.COD_PRODUTO
            ORDER BY  A.NUM_APOLICE,
                      A.NUM_CONTRATO,
                      B.COD_PRODUTO
             WITH UR
           END-EXEC.
      *
           IF    SQLCODE  NOT  EQUAL  ZEROS
             IF  SQLCODE       EQUAL  100
                 MOVE     'N'     TO  WTEM-APOL-EF
                 MOVE     ZEROS   TO  EF063-NUM-CONTR
             ELSE
                 DISPLAY 'R0990 - ERRO NO SELECT DA EF-APOLICE'
                 DISPLAY 'NUM APOLICE - ' EF148-NUM-APOL
                 DISPLAY 'RAMO CONTAB - ' EF148-RAMO-CONTB
                 DISPLAY 'PRODUTO ACS - ' EF148-PRODU-ACS
                 GO   TO  R9999-00-ROT-ERRO
           ELSE
               MOVE   'S'              TO  WTEM-APOL-EF
               MOVE   EF063-NUM-CONTR  TO  EF053-NUM-CONTR
               MOVE   V0PREM-NRENDOS   TO  EF053-NUM-ENDOS
               MOVE   V0ENDO-TIPO-ENDS TO  EF053-TIPO-ENDS
               MOVE   EF050-COD-PRODU  TO  EF056-COD-PRODU.
      *
       R0990-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R1000-00-SELECT-EF-PRM-EMIT SECTION.
      *------------------------------------
      *
           MOVE          '100'           TO               WNR-EXEC-SQL.
      *
           EXEC  SQL   SELECT
                 VALUE(COUNT(DISTINCT E.NUM_CONTRATO_SEGUR),+0)
                 INTO :WHOST-QTD-ITEM
                 FROM  SEGUROS.EF_APOLICE         A
                 JOIN  SEGUROS.EF_ENDOSSO         B
                   ON  B.NUM_CONTRATO          =  A.NUM_CONTRATO
                 JOIN  SEGUROS.EF_FATURAS_ENDOSSO C
                   ON  C.NUM_CONTRATO_FATUR    =  B.NUM_CONTRATO
                  AND  C.NUM_ENDOSSO           =  B.NUM_ENDOSSO
                 JOIN  SEGUROS.EF_FATURA          D
                   ON  D.NUM_CONTRATO          =  C.NUM_CONTRATO_FATUR
                  AND  D.SEQ_OPERACAO          =  C.SEQ_OPERACAO_FATUR
                 JOIN  SEGUROS.EF_PREMIOS_FATURA  E
                   ON  E.NUM_CONTRATO_APOL     =  D.NUM_CONTRATO
                  AND  E.SEQ_OPERACAO_FATUR    =  D.SEQ_OPERACAO
                 JOIN  SEGUROS.EF_PREMIO_EMITIDO  F
                   ON  F.NUM_CONTRATO_SEGUR    =  E.NUM_CONTRATO_SEGUR
                  AND  F.SEQ_PREMIO            =  E.SEQ_PREMIO
                WHERE  A.NUM_APOLICE           =  :EF063-NUM-APOL
                  AND  B.NUM_ENDOSSO           =  :EF053-NUM-ENDOS
                  AND  B.NUM_CONTRATO          =  :EF053-NUM-CONTR
                  AND  B.COD_TIPO_ENDOSSO      =  :EF053-TIPO-ENDS
                  AND  D.COD_PRODUTO           =  :EF056-COD-PRODU
                  AND  D.STA_FATURA           IN  ('2','3')
                  AND  F.STA_PREMIO           IN  ('A','I')
C10158            AND  F.IND_TIPO_PREMIO      IN  (:WHOST-TIP-PRM-I,
C10158                                             :WHOST-TIP-PRM-F)
                 WITH  UR
           END-EXEC.
      *
           IF    SQLCODE  NOT  EQUAL  ZEROS
             IF  SQLCODE       EQUAL  100
                 MOVE     ZEROS   TO  WHOST-QTD-ITEM
             ELSE
                 DISPLAY 'R1000 - ERRO NO SELECT DA EF-PREMIO-EMITIDO'
                 DISPLAY 'APOLICE  - ' EF063-NUM-APOL
                 DISPLAY 'ENDOSSO  - ' EF053-NUM-ENDOS
                 DISPLAY 'CONTRATO - ' EF053-NUM-CONTR
                 DISPLAY 'TP ENDOS - ' EF053-TIPO-ENDS
                 DISPLAY 'PRODUTO  - ' EF056-COD-PRODU
                 GO   TO  R9999-00-ROT-ERRO.
      *
       R1000-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R1020-00-SELECT-V0PRODUTOSVG  SECTION.
      *--------------------------------------
      *
           MOVE           '102'            TO             WNR-EXEC-SQL.
      *
           EXEC  SQL
                 SELECT  NUM_APOLICE,
                         CODSUBES,
                   VALUE(ORIG_PRODU,' ')
                   INTO :V0PDVG-NUM-APOL,
                        :V0PDVG-CODSUBES,
                        :V0PDVG-ORIG-PRODU
                   FROM  SEGUROS.V0PRODUTOSVG
                  WHERE  NUM_APOLICE        =  :V0PDVG-NUM-APOL
                    AND  CODSUBES           =  :V0PDVG-CODSUBES
                    AND  ORIG_PRODU   IS  NOT  NULL
                   WITH  UR
           END-EXEC.
      *
           IF    SQLCODE  NOT  EQUAL  ZEROS
             IF  SQLCODE       EQUAL  100
                 MOVE     SPACES  TO  V0PDVG-ORIG-PRODU
             ELSE
                 DISPLAY 'R1020 - ERRO NO SELECT DA V0PRODUTOSVG'
                 DISPLAY 'APOLICE  - ' V0PDVG-NUM-APOL
                 DISPLAY 'COD SUBG - ' V0PDVG-CODSUBES
                 GO   TO  R9999-00-ROT-ERRO.
      *
       R1020-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R1040-00-SELECT-HTCTPBVA    SECTION.
      *------------------------------------
      *
           MOVE          '104'           TO               WNR-EXEC-SQL.
      *
           EXEC  SQL
                 SELECT  DISTINCT
                         NUM_CERTIFICADO
                   INTO :HTCPVA-NUM-CERT
                   FROM  SEGUROS.HIST_CONT_PARCELVA
                  WHERE  NUM_APOLICE              =  :HTCPVA-NUM-APOL
                    AND  NUM_ENDOSSO              =  :HTCPVA-NUM-ENDS
                    AND  COD_SUBGRUPO             =  :HTCPVA-COD-SUBG
                    AND  COD_FONTE                =  :HTCPVA-COD-FONT
                    AND  COD_OPERACAO       BETWEEN   0200 AND 0299
               GROUP BY  NUM_CERTIFICADO
               ORDER BY  NUM_CERTIFICADO
                WITH UR
           END-EXEC.
      *
           IF    SQLCODE  NOT  EQUAL  ZEROS
             IF  SQLCODE       EQUAL  100
                 MOVE     ZEROS   TO  HTCPVA-NUM-CERT
                 MOVE     ZEROS   TO  CPRPVA-NUM-CERT
                 MOVE     SPACES  TO  CPRPVA-DTINIVIG
             ELSE
                 DISPLAY 'R1040 - ERRO NO SELECT DA HIST-CONT-PARCELVA'
                 DISPLAY 'APOLICE  - ' HTCPVA-NUM-APOL
                 DISPLAY 'ENDOSSO  - ' HTCPVA-NUM-ENDS
                 DISPLAY 'COD SUBG - ' HTCPVA-COD-SUBG
                 DISPLAY 'COD FONT - ' HTCPVA-COD-FONT
                 GO   TO  R9999-00-ROT-ERRO
           ELSE
               MOVE   HTCPVA-NUM-CERT  TO  CPRPVA-NUM-CERT
               MOVE   V0PREM-DTINIVIG  TO  CPRPVA-DTINIVIG.
      *
       R1040-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R1060-00-SELECT-V0FATURAS   SECTION.
      *------------------------------------
      *
           MOVE          '106'           TO               WNR-EXEC-SQL.
      *
           EXEC  SQL   SELECT
                       A.NUM_APOLICE,
                       A.NUM_ENDOSSO,
                       A.COD_SUBGRUPO,
                       A.NUM_FATURA,
                       A.COD_OPERACAO,
                       B.QTD_VIDAS_VG,
                       B.QTD_VIDAS_AP
                 INTO :V0FATR-NUM-APOL,
                      :V0FATR-NUM-ENDS,
                      :V0FATR-COD-SUBG,
                      :V0FATR-NR-FATUR,
                      :V0FATR-COD-OPER,
                      :V0FTOT-QTVDA-VG,
                      :V0FTOT-QTVDA-AP
                 FROM  SEGUROS.V0FATURAS    A,
                       SEGUROS.V0FATURASTOT B
                WHERE  A.NUM_APOLICE        =  :V0FATR-NUM-APOL
                  AND  A.NUM_ENDOSSO        =  :V0FATR-NUM-ENDS
                  AND  A.COD_OPERACAO       <   0200
                  AND  B.NUM_APOLICE        =  A.NUM_APOLICE
                  AND  B.COD_SUBGRUPO       =  A.COD_SUBGRUPO
                  AND  B.NUM_FATURA         =  A.NUM_FATURA
                  AND  B.COD_OPERACAO       =  A.COD_OPERACAO
                 WITH  UR
           END-EXEC.
      *
           IF    SQLCODE  NOT  EQUAL  ZEROS
             IF  SQLCODE       EQUAL  100
                 MOVE     ZEROS   TO  WHOST-QTD-ITEM
             ELSE
                 DISPLAY 'R1060 - ERRO NO SELECT DA V0FATURAS'
                 DISPLAY 'APOLICE  - ' V0FATR-NUM-APOL
                 DISPLAY 'ENDOSSO  - ' V0FATR-NUM-ENDS
                 GO   TO  R9999-00-ROT-ERRO
           ELSE
             IF  V0FTOT-QTVDA-VG  =  ZEROS  AND
                 V0FTOT-QTVDA-AP  =  ZEROS
                 MOVE   ZEROS    TO  WHOST-QTD-ITEM
             ELSE
               IF  V0FTOT-QTVDA-VG  =  ZEROS
                   MOVE  V0FTOT-QTVDA-AP  TO   WHOST-QTD-ITEM
               ELSE
                   MOVE  V0FTOT-QTVDA-VG  TO   WHOST-QTD-ITEM.
      *
       R1060-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R1080-00-SELECT-COBPRPVA    SECTION.
      *------------------------------------
      *
           MOVE          '108'           TO               WNR-EXEC-SQL.
      *
           EXEC  SQL
                 SELECT  A.NUM_CERTIFICADO,
                         A.QUANT_VIDAS
                   INTO :CPRPVA-NUM-CERT,
                        :CPRPVA-QT-VIDAS
                   FROM  SEGUROS.HIS_COBER_PROPOST A
                  WHERE  A.NUM_CERTIFICADO       =  :CPRPVA-NUM-CERT
                    AND  A.DATA_INIVIGENCIA     <=  :CPRPVA-DTINIVIG
                    AND  A.DATA_TERVIGENCIA     >=  :CPRPVA-DTINIVIG
                    AND  A.OCORR_HISTORICO       =
                (SELECT  MAX(B.OCORR_HISTORICO)
                   FROM  SEGUROS.HIS_COBER_PROPOST B
                  WHERE  B.NUM_CERTIFICADO       =  A.NUM_CERTIFICADO
                    AND  B.DATA_INIVIGENCIA      =  A.DATA_INIVIGENCIA
                    AND  B.DATA_TERVIGENCIA      =  A.DATA_TERVIGENCIA)
                   WITH  UR
           END-EXEC.
      *
           IF    SQLCODE  NOT  EQUAL  ZEROS
             IF  SQLCODE       EQUAL  100  OR  -811
                 PERFORM  R1090-00-SELECT-HSTCOBPROP
             ELSE
                 DISPLAY 'R1080 - ERRO NO SELECT DA HIS-COBER-PROPOST'
                 DISPLAY 'APOLICE  - ' HTCPVA-NUM-APOL
                 DISPLAY 'ENDOSSO  - ' HTCPVA-NUM-ENDS
                 DISPLAY 'COD SUBG - ' HTCPVA-COD-SUBG
                 DISPLAY 'COD FONT - ' HTCPVA-COD-FONT
                 DISPLAY 'CERTIFIC - ' CPRPVA-NUM-CERT
                 DISPLAY 'INI VIGC - ' CPRPVA-DTINIVIG
                 GO   TO  R9999-00-ROT-ERRO.
      *
       R1080-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R1090-00-SELECT-HSTCOBPROP  SECTION.
      *------------------------------------
      *
           MOVE          '109'           TO               WNR-EXEC-SQL.
      *
           EXEC  SQL
                 SELECT  A.NUM_CERTIFICADO,
                         A.QUANT_VIDAS
                   INTO :CPRPVA-NUM-CERT,
                        :CPRPVA-QT-VIDAS
                   FROM  SEGUROS.HIS_COBER_PROPOST A
                  WHERE  A.NUM_CERTIFICADO       =  :CPRPVA-NUM-CERT
                    AND  A.DATA_INIVIGENCIA      =  :CPRPVA-DTINIVIG
                    AND  A.OCORR_HISTORICO       =
                (SELECT  MAX(B.OCORR_HISTORICO)
                   FROM  SEGUROS.HIS_COBER_PROPOST B
                  WHERE  B.NUM_CERTIFICADO       =  A.NUM_CERTIFICADO
                    AND  B.DATA_INIVIGENCIA      =  A.DATA_INIVIGENCIA
                    AND  B.DATA_TERVIGENCIA      =  A.DATA_TERVIGENCIA)
                   WITH  UR
           END-EXEC.
      *
           IF    SQLCODE  NOT  EQUAL  ZEROS
             IF  SQLCODE       EQUAL  100
                 MOVE     ZEROS   TO  CPRPVA-QT-VIDAS
             ELSE
                 DISPLAY 'R1090 - ERRO NO SELECT DA HIS-COBER-PROPOST'
                 DISPLAY 'APOLICE  - ' HTCPVA-NUM-APOL
                 DISPLAY 'ENDOSSO  - ' HTCPVA-NUM-ENDS
                 DISPLAY 'COD SUBG - ' HTCPVA-COD-SUBG
                 DISPLAY 'COD FONT - ' HTCPVA-COD-FONT
                 DISPLAY 'CERTIFIC - ' CPRPVA-NUM-CERT
                 DISPLAY 'INI VIGC - ' CPRPVA-DTINIVIG
                 GO   TO  R9999-00-ROT-ERRO.
      *
       R1090-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R1100-00-SELECT-QTDE-VIDAS  SECTION.
      *------------------------------------
      *
           MOVE          '110'           TO               WNR-EXEC-SQL.
      *
           EXEC  SQL   SELECT
                 VALUE(COUNT(DISTINCT NUM_CERTIFICADO),+0)
                 INTO :WHOST-QTD-ITEM
                 FROM  SEGUROS.HIST_CONT_PARCELVA
                WHERE  NUM_APOLICE              =  :HTCPVA-NUM-APOL
                  AND  NUM_ENDOSSO              =  :HTCPVA-NUM-ENDS
                  AND  COD_SUBGRUPO             =  :HTCPVA-COD-SUBG
                  AND  COD_FONTE                =  :HTCPVA-COD-FONT
                  AND  COD_OPERACAO       BETWEEN   0200 AND 0299
                 WITH  UR
           END-EXEC.
      *
           IF    SQLCODE  NOT  EQUAL  ZEROS
             IF  SQLCODE       EQUAL  100
                 MOVE     ZEROS   TO  WHOST-QTD-ITEM
             ELSE
                 DISPLAY 'R1100 - ERRO NO SELECT DA HIST-CONT-PARCELVA'
                 DISPLAY 'APOLICE  - ' HTCPVA-NUM-APOL
                 DISPLAY 'ENDOSSO  - ' HTCPVA-NUM-ENDS
                 DISPLAY 'COD SUBG - ' HTCPVA-COD-SUBG
                 DISPLAY 'COD FONT - ' HTCPVA-COD-FONT
                 GO   TO  R9999-00-ROT-ERRO.
      *
       R1100-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R1120-00-SELECT-V0AUTOAPOL  SECTION.
      *------------------------------------
      *
           MOVE          '112'           TO               WNR-EXEC-SQL.
      *
           EXEC  SQL   SELECT
                 VALUE(COUNT(DISTINCT NRITEM),+0)
                 INTO :WHOST-QTD-ITEM
                 FROM  SEGUROS.V0AUTOAPOL
                WHERE  NUM_APOLICE      =   :V0AUTA-NUM-APOL
                  AND  NRENDOS          =   :V0AUTA-NRENDOS
                  AND  SITUACAO        <>   '2'
                 WITH  UR
           END-EXEC.
      *
           IF    SQLCODE  NOT  EQUAL  ZEROS
             IF  SQLCODE       EQUAL  100
                 MOVE     ZEROS   TO  WHOST-QTD-ITEM
             ELSE
                 DISPLAY 'R1120 - ERRO NO SELECT DA V0AUTOAPOL'
                 DISPLAY 'APOLICE  - ' V0AUTA-NUM-APOL
                 DISPLAY 'ENDOSSO  - ' V0AUTA-NRENDOS
                 GO   TO  R9999-00-ROT-ERRO.
      *
       R1120-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R1140-00-SELECT-V0TOMADOR  SECTION.
      *-----------------------------------
      *
           MOVE          '114'          TO                WNR-EXEC-SQL.
      *
           EXEC  SQL
                 SELECT  COD_CLIENTE
                   INTO :V0TOMD-CODCLIEN
                   FROM  SEGUROS.V0TOMADOR
                  WHERE  FONTE           =   :V0TOMD-FONTE
                    AND  NRPROPOS        =   :V0TOMD-NRPROPOS
                   WITH  UR
           END-EXEC.
      *
           IF    SQLCODE  NOT  EQUAL  ZEROS
             IF  SQLCODE       EQUAL  100
                 MOVE     ZEROS   TO  V0TOMD-CODCLIEN
             ELSE
                 DISPLAY 'R1140 - ERRO NO SELECT DA V0TOMADOR'
                 DISPLAY 'APOLICE  - ' V0PREM-NUM-APOL
                 DISPLAY 'ENDOSSO  - ' V0PREM-NRENDOS
                 DISPLAY 'RAMO COB - ' V0PREM-RAMOFR
                 DISPLAY 'COD FONT - ' V0TOMD-FONTE
                 DISPLAY 'NR PROPT - ' V0TOMD-NRPROPOS
                 GO   TO  R9999-00-ROT-ERRO
           ELSE
               MOVE   V0TOMD-CODCLIEN  TO  WCOD-CLIEN-TOM
               MOVE   V0TOMD-CODCLIEN  TO  V0CLIE-CODCLIEN.
      *
       R1140-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R1160-00-SELECT-V0ENDERECOS SECTION.
      *------------------------------------
      *
           MOVE          '116'           TO               WNR-EXEC-SQL.
      *
           EXEC  SQL
                 SELECT  SIGLA_UF
                   INTO :V0ENDR-SIGLA-UF
                   FROM  SEGUROS.V0ENDERECOS
                  WHERE  COD_CLIENTE       =   :V0ENDR-CODCLIEN
                    AND  OCORR_ENDERECO    =   :V0ENDR-OCOR-ENDR
                   WITH  UR
           END-EXEC.
      *
           IF    SQLCODE  NOT  EQUAL  ZEROS
             IF  SQLCODE       EQUAL  100
                 PERFORM  R1170-00-SELECT-MAX-ENDERECO
             ELSE
                 DISPLAY 'R1160 - ERRO NO SELECT DA V0ENDERECOS'
                 DISPLAY 'APOLICE  - ' V0PREM-NUM-APOL
                 DISPLAY 'ENDOSSO  - ' V0PREM-NRENDOS
                 DISPLAY 'RAMO COB - ' V0PREM-RAMOFR
                 DISPLAY 'COD CLIE - ' V0ENDR-CODCLIEN
                 DISPLAY 'OCR ENDR - ' V0ENDR-OCOR-ENDR
                 GO   TO  R9999-00-ROT-ERRO
           ELSE
             IF  V0ENDR-SIGLA-UF  =  'AC' OR 'AL' OR 'AM' OR 'AP' OR
                 V0ENDR-SIGLA-UF  =  'BA' OR 'CE' OR 'DF' OR 'ES' OR
                 V0ENDR-SIGLA-UF  =  'GO' OR 'MA' OR 'MG' OR 'MS' OR
                 V0ENDR-SIGLA-UF  =  'MT' OR 'PA' OR 'PB' OR 'PE' OR
                 V0ENDR-SIGLA-UF  =  'PI' OR 'PR' OR 'RJ' OR 'RN' OR
                 V0ENDR-SIGLA-UF  =  'RO' OR 'RR' OR 'RS' OR 'SC' OR
                 V0ENDR-SIGLA-UF  =  'SE' OR 'SP' OR 'TO'
                 NEXT   SENTENCE
             ELSE
                 MOVE     SPACES   TO  V0ENDR-SIGLA-UF
                 PERFORM  R1170-00-SELECT-MAX-ENDERECO.
      *
       R1160-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R1170-00-SELECT-MAX-ENDERECO  SECTION.
      *--------------------------------------
      *
           MOVE           '117'            TO             WNR-EXEC-SQL.
      *
           EXEC  SQL
                 SELECT  SIGLA_UF
                   INTO :V0ENDR-SIGLA-UF
                   FROM  SEGUROS.V0ENDERECOS
                  WHERE  COD_CLIENTE       =   :V0ENDR-CODCLIEN
                    AND  OCORR_ENDERECO    =
                (SELECT  MAX(OCORR_ENDERECO)
                   FROM  SEGUROS.V0ENDERECOS
                  WHERE  COD_CLIENTE       =   :V0ENDR-CODCLIEN
                    AND  OCORR_ENDERECO   <>   :V0ENDR-OCOR-ENDR
                    AND  SIGLA_UF         <>   '  ')
                   WITH  UR
           END-EXEC.
      *
           IF    SQLCODE  NOT  EQUAL  ZEROS
             IF  SQLCODE       EQUAL  100
                 MOVE     SPACES  TO  V0ENDR-SIGLA-UF
             ELSE
                 DISPLAY 'R1170 - ERRO NO SELECT DA V0ENDERECOS'
                 DISPLAY 'APOLICE  - ' V0PREM-NUM-APOL
                 DISPLAY 'ENDOSSO  - ' V0PREM-NRENDOS
                 DISPLAY 'RAMO COB - ' V0PREM-RAMOFR
                 DISPLAY 'COD CLIE - ' V0ENDR-CODCLIEN
                 DISPLAY 'OCR ENDR - ' V0ENDR-OCOR-ENDR
                 GO   TO  R9999-00-ROT-ERRO
           ELSE
             IF  V0ENDR-SIGLA-UF  =  'AC' OR 'AL' OR 'AM' OR 'AP' OR
                 V0ENDR-SIGLA-UF  =  'BA' OR 'CE' OR 'DF' OR 'ES' OR
                 V0ENDR-SIGLA-UF  =  'GO' OR 'MA' OR 'MG' OR 'MS' OR
                 V0ENDR-SIGLA-UF  =  'MT' OR 'PA' OR 'PB' OR 'PE' OR
                 V0ENDR-SIGLA-UF  =  'PI' OR 'PR' OR 'RJ' OR 'RN' OR
                 V0ENDR-SIGLA-UF  =  'RO' OR 'RR' OR 'RS' OR 'SC' OR
                 V0ENDR-SIGLA-UF  =  'SE' OR 'SP' OR 'TO'
                 NEXT   SENTENCE
             ELSE
                 MOVE   SPACES   TO   V0ENDR-SIGLA-UF.
      *
       R1170-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R1180-00-SELECT-V0AGENCIAS     SECTION.
      *---------------------------------------
      *
           MOVE           '118'             TO            WNR-EXEC-SQL.
      *
           EXEC  SQL
                 SELECT  BANCO ,
                         AGENCIA ,
                         ESTADO
                   INTO :V0AGEN-COD-BANCO ,
                        :V0AGEN-COD-AGENC ,
                        :V0AGEN-ESTADO
                   FROM  SEGUROS.V0AGENCIAS
                  WHERE  BANCO            =    :V0ENDO-BCORCAP
                    AND  AGENCIA          =    :V0ENDO-AGERCAP
                   WITH  UR
           END-EXEC.
      *
           IF    SQLCODE  NOT  EQUAL  ZEROS
             IF  SQLCODE       EQUAL  100
                 MOVE     SPACES  TO  V0AGEN-ESTADO
             ELSE
                 DISPLAY 'R1180 - ERRO NO SELECT DA V0AGENCIAS'
                 DISPLAY 'APOLICE  - ' V0PREM-NUM-APOL
                 DISPLAY 'ENDOSSO  - ' V0PREM-NRENDOS
                 DISPLAY 'RAMO COB - ' V0PREM-RAMOFR
                 DISPLAY 'BCO RCAP - ' V0ENDO-BCORCAP
                 DISPLAY 'AGE RCAP - ' V0ENDO-AGERCAP
                 GO   TO  R9999-00-ROT-ERRO
           ELSE
             IF  V0AGEN-ESTADO  =  'AC' OR 'AL' OR 'AM' OR 'AP' OR
                 V0AGEN-ESTADO  =  'BA' OR 'CE' OR 'DF' OR 'ES' OR
                 V0AGEN-ESTADO  =  'GO' OR 'MA' OR 'MG' OR 'MS' OR
                 V0AGEN-ESTADO  =  'MT' OR 'PA' OR 'PB' OR 'PE' OR
                 V0AGEN-ESTADO  =  'PI' OR 'PR' OR 'RJ' OR 'RN' OR
                 V0AGEN-ESTADO  =  'RO' OR 'RR' OR 'RS' OR 'SC' OR
                 V0AGEN-ESTADO  =  'SE' OR 'SP' OR 'TO'
                 NEXT   SENTENCE
             ELSE
                 MOVE   SPACES   TO   V0AGEN-ESTADO.
      *
       R1180-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R1200-00-SELECT-V0PRODUTOR     SECTION.
      *---------------------------------------
      *
           MOVE           '120'             TO           WNR-EXEC-SQL.
      *
           EXEC  SQL
                 SELECT  A.CODPDT
                   INTO :V0PRDT-COD-PRDT
                   FROM  SEGUROS.V0PRODUTOR   A,
                         SEGUROS.V0APOLCORRET B
                  WHERE  A.CGCCPF             =  65147241000103
                    AND  B.CODCORR            =  A.CODPDT
                    AND  B.NUM_APOLICE        =  :V0ACOR-NUM-APOL
                    AND  B.CODSUBES           =  :V0ACOR-CODSUBES
                    AND  B.RAMOFR             =  :V0ACOR-RAMOFR
                    AND  B.MODALIFR           =  :V0ACOR-MODALIFR
                    AND  B.DTINIVIG          <=  :V0ACOR-DTINIVIG
                    AND  B.DTTERVIG          >=  :V0ACOR-DTINIVIG
                    AND  B.TIPCOM             =  '1'
                    AND  B.INDCRT             =  '1'
                   WITH  UR
           END-EXEC.
      *
           IF    SQLCODE  NOT  EQUAL  ZEROS
             IF  SQLCODE       EQUAL  100
                 MOVE     ZEROS   TO  V0PRDT-COD-PRDT
             ELSE
                 DISPLAY 'R1200 - ERRO NO SELECT DA V0PRODUTOR'
                 DISPLAY 'APOLICE  - ' V0ACOR-NUM-APOL
                 DISPLAY 'COD SUBG - ' V0ACOR-CODSUBES
                 DISPLAY 'RAMO CBT - ' V0ACOR-RAMOFR
                 DISPLAY 'MODL CBT - ' V0ACOR-MODALIFR
                 DISPLAY 'INIC VIG - ' V0ACOR-DTINIVIG
                 GO   TO  R9999-00-ROT-ERRO.
      *
       R1200-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R1220-00-PROCESSA-UF-VIDA      SECTION.
      *---------------------------------------
      *
           MOVE           '122'             TO         WNR-EXEC-SQL.
      *
           MOVE       V0PREM-NUM-APOL       TO         HTCPVA-NUM-APOL.
           MOVE       V0PREM-NRENDOS        TO         HTCPVA-NUM-ENDS.
           MOVE       V0PREM-CODSUBES       TO         HTCPVA-COD-SUBG.
           MOVE       V0PREM-FONTE          TO         HTCPVA-COD-FONT.
      *
           PERFORM    R1230-00-DECLARE-V0ENDERECOS.
      *
           PERFORM    R1240-00-FETCH-V0ENDERECOS  UNTIL
                      WFIM-V0ENDERECOS NOT EQUAL  SPACES.
      *
           IF  WTAB-SIGLA-UF  =  SPACES
               PERFORM  R1200-00-SELECT-V0PRODUTOR
               IF  V0PRDT-COD-PRDT  =  ZEROS
                   MOVE   V0FONT-ESTADO   TO   EMI-UF-RISCO
               ELSE
                   MOVE  'MG          '   TO   EMI-UF-RISCO
           ELSE
               MOVE   WTAB-SIGLA-UF   TO   EMI-UF-RISCO.
      *
       R1220-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R1230-00-DECLARE-V0ENDERECOS  SECTION.
      *--------------------------------------
      *
           MOVE          '123'             TO             WNR-EXEC-SQL.
      *
           EXEC  SQL     DECLARE       V0ENDERECOS        CURSOR   FOR
                 SELECT  DISTINCT
                         C.SIGLA_UF
                   FROM  SEGUROS.HIST_CONT_PARCELVA A,
                         SEGUROS.V0SEGURAVG  B,
                         SEGUROS.V0ENDERECOS C
                  WHERE  A.NUM_APOLICE         =  :HTCPVA-NUM-APOL
                    AND  A.NUM_ENDOSSO         =  :HTCPVA-NUM-ENDS
                    AND  A.COD_SUBGRUPO        =  :HTCPVA-COD-SUBG
                    AND  A.COD_FONTE           =  :HTCPVA-COD-FONT
                    AND  A.COD_OPERACAO  BETWEEN   0200 AND 0299
                    AND  B.NUM_APOLICE         =  A.NUM_APOLICE
                    AND  B.COD_SUBGRUPO        =  A.COD_SUBGRUPO
                    AND  B.COD_FONTE           =  A.COD_FONTE
                    AND  B.NUM_CERTIFICADO     =  A.NUM_CERTIFICADO
                    AND  B.TIPO_SEGURADO       =  '1'
                    AND  C.COD_CLIENTE         =  B.COD_CLIENTE
                    AND  C.OCORR_ENDERECO      =  B.OCORR_ENDERECO
               GROUP BY  C.SIGLA_UF
               ORDER BY  C.SIGLA_UF
                WITH UR
           END-EXEC.
      *
           EXEC  SQL   OPEN   V0ENDERECOS    END-EXEC.
      *
           IF  SQLCODE  NOT   EQUAL  ZEROS
               DISPLAY 'R1230 - ERRO NO DECLARE DA V0ENDERECOS'
               DISPLAY 'APOLICE  - ' HTCPVA-NUM-APOL
               DISPLAY 'ENDOSSO  - ' HTCPVA-NUM-ENDS
               DISPLAY 'COD SUBG - ' HTCPVA-COD-SUBG
               DISPLAY 'COD FONT - ' HTCPVA-COD-FONT
               GO   TO  R9999-00-ROT-ERRO
           ELSE
               MOVE     ZEROS    TO  WIND
               MOVE     SPACES   TO  WTAB-SIGLA-UF
               MOVE     SPACES   TO  V0ENDR-SIGLA-UF
               MOVE     SPACES   TO  WFIM-V0ENDERECOS.
      *
       R1230-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R1240-00-FETCH-V0ENDERECOS  SECTION.
      *------------------------------------
      *
           MOVE          '124'           TO               WNR-EXEC-SQL.
      *
       R1240-10-LER-ENDERECO.
      *
           EXEC  SQL     FETCH           V0ENDERECOS
                 INTO   :V0ENDR-SIGLA-UF
           END-EXEC.
      *
           IF     SQLCODE  NOT  EQUAL    ZEROS
              IF  SQLCODE       EQUAL    100
                  MOVE     'S'     TO    WFIM-V0ENDERECOS
                  EXEC     SQL  CLOSE         V0ENDERECOS  END-EXEC
              ELSE
                  DISPLAY 'R1240 - ERRO DE FETCH DA V0ENDERECOS'
                  DISPLAY 'APOLICE  - ' HTCPVA-NUM-APOL
                  DISPLAY 'ENDOSSO  - ' HTCPVA-NUM-ENDS
                  DISPLAY 'COD SUBG - ' HTCPVA-COD-SUBG
                  DISPLAY 'COD FONT - ' HTCPVA-COD-FONT
                  GO   TO  R9999-00-ROT-ERRO
           ELSE
               IF  V0ENDR-SIGLA-UF  =  'AC' OR 'AL' OR 'AM' OR 'AP' OR
                   V0ENDR-SIGLA-UF  =  'BA' OR 'CE' OR 'DF' OR 'ES' OR
                   V0ENDR-SIGLA-UF  =  'GO' OR 'MA' OR 'MG' OR 'MS' OR
                   V0ENDR-SIGLA-UF  =  'MT' OR 'PA' OR 'PB' OR 'PE' OR
                   V0ENDR-SIGLA-UF  =  'PI' OR 'PR' OR 'RJ' OR 'RN' OR
                   V0ENDR-SIGLA-UF  =  'RO' OR 'RR' OR 'RS' OR 'SC' OR
                   V0ENDR-SIGLA-UF  =  'SE' OR 'SP' OR 'TO'
                   ADD    1     TO      WIND
                   IF  WIND  >  27
                       DISPLAY 'R1240 - ERRO NA QTDE DE UF CADASTRADA'
                       DISPLAY 'APOLICE  - ' HTCPVA-NUM-APOL
                       DISPLAY 'ENDOSSO  - ' HTCPVA-NUM-ENDS
                       DISPLAY 'COD SUBG - ' HTCPVA-COD-SUBG
                       DISPLAY 'COD FONT - ' HTCPVA-COD-FONT
                       GO   TO  R9999-00-ROT-ERRO
                   ELSE
                       MOVE  V0ENDR-SIGLA-UF  TO  WTAB-UF(WIND)
               ELSE
                   GO   TO   R1240-10-LER-ENDERECO.
      *
       R1240-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R1250-00-SELECT-V0COBERAPOL   SECTION.
      *--------------------------------------
      *
           MOVE          '125'             TO             WNR-EXEC-SQL.
      *
           EXEC  SQL   SELECT
                 VALUE(SUM(IMP_SEGURADA_IX),+0),
                 VALUE(SUM(PRM_TARIFARIO_IX),+0),
                 VALUE(SUM(IMP_SEGURADA_VAR),+0),
                 VALUE(SUM(PRM_TARIFARIO_VAR),+0)
                 INTO :V0COBA-IMP-SEG-IX,
                      :V0COBA-PRM-TAR-IX,
                      :V0COBA-IMP-SEG-VR,
                      :V0COBA-PRM-TAR-VR
                 FROM  SEGUROS.V0COBERAPOL
                WHERE  NUM_APOLICE       =    :V0COBA-NUM-APOL
                  AND  NRENDOS           =    :V0COBA-NRENDOS
                  AND  RAMOFR            =    :V0COBA-RAMOFR
                  AND  NUM_ITEM          =     0
                  AND  COD_COBERTURA     =     0
                 WITH  UR
           END-EXEC.
      *
           IF    SQLCODE  NOT  EQUAL  ZEROS
             IF  SQLCODE       EQUAL  100
                 MOVE   ZEROS  TO     V0COBA-IMP-SEG-IX
                 MOVE   ZEROS  TO     V0COBA-PRM-TAR-IX
                 MOVE   ZEROS  TO     V0COBA-IMP-SEG-VR
                 MOVE   ZEROS  TO     V0COBA-PRM-TAR-VR
             ELSE
                 DISPLAY 'R1250 - ERRO NO SELECT DA V0COBERAPOL'
                 DISPLAY 'APOLICE  - ' V0COBA-NUM-APOL
                 DISPLAY 'ENDOSSO  - ' V0COBA-NRENDOS
                 DISPLAY 'RAMO CBT - ' V0COBA-RAMOFR
                 GO   TO  R9999-00-ROT-ERRO
           ELSE
             IF  V0COBA-IMP-SEG-IX  =  ZEROS
                 MOVE   V0COBA-IMP-SEG-VR  TO  V0PREM-IMP-SEG-T
             ELSE
                 MOVE   V0COBA-IMP-SEG-IX  TO  V0PREM-IMP-SEG-T.
      *
       R1250-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R1260-00-SELECT-V0COTACAO     SECTION.
      *--------------------------------------
      *
           MOVE           '126'            TO             WNR-EXEC-SQL.
      *
           EXEC  SQL    SELECT
                        VAL_VENDA
                 INTO  :V0COTA-VALVEND
                 FROM   SEGUROS.V0COTACAO
                WHERE   CODUNIMO        =     :V0COTA-CODUNIMO
                  AND   DTINIVIG       <=     :V0COTA-DTINIVIG
                  AND   DTTERVIG       >=     :V0COTA-DTINIVIG
                 WITH   UR
           END-EXEC.
      *
           IF  SQLCODE  NOT   EQUAL  ZEROS
               DISPLAY 'R1260 - ERRO NO SELECT DA V0COTACAO'
               DISPLAY 'APOLICE  - ' V0PREM-NUM-APOL
               DISPLAY 'ENDOSSO  - ' V0PREM-NRENDOS
               DISPLAY 'RAMO COB - ' V0PREM-RAMOFR
               DISPLAY 'MOED IMP - ' V0PREM-MOEDA-IMP
               DISPLAY 'INIC VIG - ' V0PREM-DTINIVIG
               GO   TO  R9999-00-ROT-ERRO.
      *
       R1260-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R1270-00-CALL-GE0009S        SECTION.
      *-------------------------------------
      *
           MOVE           '127'           TO       WNR-EXEC-SQL.
      *
           MOVE    V0PREM-NUM-APOL        TO       LKGE09-NUM-APOLICE.
           MOVE    V0PREM-NRENDOS         TO       LKGE09-NUM-ENDOSSO.
           MOVE    V0PREM-CANAL-VENDA     TO       LKGE09-CANAL-VENDA.
           MOVE    V0PREM-CODPRODU        TO       LKGE09-COD-PRODUTO.
      *
           INITIALIZE     LKGE09-PARM-OUTPUT.
      *
           CALL    'GE0009S'           USING       LKGE-PARM-GE0009S.
      *
           IF  LKGE09-SQL-CODE  =  ZEROS  AND
               LKGE09-RTN-CODE  =  ZEROS  AND
               LKGE09-MSG-ERRO  =  SPACES
               MOVE   LKGE09-TIP-REDE     TO       WTIP-REDE-AUX
146163         MOVE   LKGE09-TIP-RENOV    TO       WTIP-RENOV-AUX
               MOVE   LKGE09-PCT-TARF     TO       WPCT-TARF-BALC
               MOVE   ZEROS               TO       WVLR-TARF-BALC
           ELSE
               DISPLAY 'R1270 - ERRO NO CALL DA SUB-ROT GE0009S'
               MOVE     LKGE09-SQL-CODE   TO    SQLCODE
               DISPLAY 'MENSAGEM - ' LKGE09-MSG-ERRO
               DISPLAY 'APOLICE  - ' LKGE09-NUM-APOLICE
               DISPLAY 'ENDOSSO  - ' LKGE09-NUM-ENDOSSO
               DISPLAY 'CANAL VD - ' LKGE09-CANAL-VENDA
               DISPLAY 'PRODUTO  - ' LKGE09-COD-PRODUTO
               GO   TO  R9999-00-ROT-ERRO
           END-IF.
      *
       R1270-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R1280-00-CALL-GE0010S        SECTION.
      *-------------------------------------
      *
           MOVE           '128'           TO       WNR-EXEC-SQL.
      *
           MOVE    V0PREM-NUM-APOL        TO       LKGE10-NUM-APOLICE.
           MOVE    V0PREM-NRENDOS         TO       LKGE10-NUM-ENDOSSO.
           MOVE    V0PREM-RAMO            TO       LKGE10-COD-RAMO-EM.
           MOVE    V0PREM-CODPRODU        TO       LKGE10-COD-PRODUTO.
           MOVE    V0PREM-CANAL-VENDA     TO       LKGE10-CANAL-VENDA.
           MOVE    V0PREM-DTINIVIG        TO       LKGE10-DAT-INIVIGC.
      *
           INITIALIZE     LKGE10-PARM-OUTPUT.
      *
           CALL    'GE0010S'           USING       LKGE-PARM-GE0010S.
      *
           IF  LKGE10-SQL-CODE  =  ZEROS  AND
               LKGE10-RTN-CODE  =  ZEROS  AND
               LKGE10-MSG-ERRO  =  SPACES
               MOVE   LKGE10-TIP-REDE     TO       WTIP-REDE-AUX
146163         MOVE   LKGE10-TIP-RENOV    TO       WTIP-RENOV-AUX
               IF  LKGE10-TIPO-FUNCIO  =  '1'
                   MOVE   LKGE10-PCT-COMS   TO     WPCT-COMS-INDC
                   MOVE   LKGE10-VLR-COMS   TO     WVLR-COMS-INDC
               ELSE
                   MOVE   LKGE10-PCT-COMS   TO     WPCT-TARF-BALC
                   MOVE   LKGE10-VLR-COMS   TO     WVLR-TARF-BALC
               END-IF
           ELSE
               DISPLAY 'R1280 - ERRO NO CALL DA SUB-ROT GE0010S'
               MOVE     LKGE10-SQL-CODE   TO    SQLCODE
               DISPLAY 'MENSAGEM - ' LKGE10-MSG-ERRO
               DISPLAY 'APOLICE  - ' LKGE10-NUM-APOLICE
               DISPLAY 'ENDOSSO  - ' LKGE10-NUM-ENDOSSO
               DISPLAY 'RAMO EMS - ' LKGE10-COD-RAMO-EM
               DISPLAY 'PRODUTO  - ' LKGE10-COD-PRODUTO
               DISPLAY 'CANAL VD - ' LKGE10-CANAL-VENDA
               DISPLAY 'INIC VIG - ' LKGE10-DAT-INIVIGC
               GO   TO  R9999-00-ROT-ERRO
           END-IF.
      *
       R1280-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R1300-00-ACUMULA-VALORES    SECTION.
      *------------------------------------
      *
           MOVE         '130'            TO         WNR-EXEC-SQL.
      *
           ADD      V0PREM-VLPRMTAR-T    TO         ACC-VLPRMTAR-T.
           ADD      V0PREM-VLDESCON-T    TO         ACC-VLDESCON-T.
           ADD      V0PREM-VLPRMLIQ-T    TO         ACC-VLPRMLIQ-T.
           ADD      V0PREM-VLADIFRA-T    TO         ACC-VLADIFRA-T.
           ADD      V0PREM-VLCUSEMI-T    TO         ACC-VLCUSEMI-T.
           ADD      V0PREM-VLIOCC-T      TO         ACC-VLIOCC-T.
           ADD      V0PREM-VLPRMTOT-T    TO         ACC-VLPRMTOT-T.
      *
           ADD      V0PREM-VLCOMIS-T     TO         ACC-VLCOMIS-T.
      *
           ADD      V0PREM-VLADMN-T      TO         ACC-VLADMN-T.
235637*    ADD      V0PREM-VLADMN-T      TO         ACC-VLCOMIS-T.
      *
           ADD      V0PREM-VLAGENC-T     TO         ACC-VLAGENC-T.
235637*    ADD      V0PREM-VLAGENC-T     TO         ACC-VLCOMIS-T.
      *
           ADD      V0PREM-VLPRMTAR-C    TO         ACC-VLPRMTAR-C.
           ADD      V0PREM-VLDESCON-C    TO         ACC-VLDESCON-C.
           ADD      V0PREM-VLPRMLIQ-C    TO         ACC-VLPRMLIQ-C.
           ADD      V0PREM-VLCOMIS-C     TO         ACC-VLCOMIS-C.
148834*
=          ADD      V0PREM-VLPRMTAR-L    TO         ACC-VLPRMTAR-L.
=          ADD      V0PREM-VLPRMTAR-R    TO         ACC-VLPRMTAR-R.
148834*
           PERFORM  R0600-00-FETCH-V0PREMIOS.
      *
       R1300-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
136081*
=      R1400-00-SELECT-APOL-COBR    SECTION.
=     *-------------------------------------
=     *
=          MOVE           '140'           TO              WNR-EXEC-SQL.
=     *
=          EXEC  SQL
=             SELECT  NUM_MATRICULA
=               INTO :APOLCOBR-NUM-MATR
=               FROM  SEGUROS.APOLICE_COBRANCA
=              WHERE  NUM_APOLICE    =    :APOLCOBR-NUM-APOL
=                AND  NUM_ENDOSSO    =    :APOLCOBR-NUM-ENDS
=               WITH  UR
=          END-EXEC.
=     *
=          IF    SQLCODE  NOT  EQUAL  ZEROS
=            IF  SQLCODE       EQUAL  100
=                MOVE     ZEROS   TO  FUNCICEF-NUM-MATR
=            ELSE
=                DISPLAY 'R1400 - ERRO NO SELECT DA APOLICE-COBRANCA'
=                DISPLAY 'APOLICE  - ' CHVANT-NUM-APOL
=                DISPLAY 'ENDOSSO  - ' CHVANT-NRENDOS
=                DISPLAY 'BILHETE  - ' CHVANT-NUM-BILH
=                DISPLAY 'TIP ENDS - ' CHVANT-TIPO-ENDS
=                GO   TO  R9999-00-ROT-ERRO
=          ELSE
=              MOVE   APOLCOBR-NUM-MATR  TO   FUNCICEF-NUM-MATR.
=     *
=      R1400-99-SAIDA.
=          EXIT.
=     *
=     *----------------------------------------------------------------*
=          EJECT
=     *----------------------------------------------------------------*
=     *
=      R1500-00-SELECT-BILHETE      SECTION.
=     *-------------------------------------
=     *
=          MOVE           '150'           TO              WNR-EXEC-SQL.
=     *
=          EXEC  SQL
=             SELECT  NUM_MATRICULA
=               INTO :BILHETE-NUM-MATR
=               FROM  SEGUROS.BILHETE
=              WHERE  NUM_BILHETE   =   :BILHETE-NUM-BILH
=               WITH  UR
=          END-EXEC.
=     *
=          IF    SQLCODE  NOT  EQUAL  ZEROS
=            IF  SQLCODE       EQUAL  100
=                MOVE     ZEROS   TO  FUNCICEF-NUM-MATR
=            ELSE
=                DISPLAY 'R1500 - ERRO NO SELECT DA BILHETE'
=                DISPLAY 'APOLICE  - ' CHVANT-NUM-APOL
=                DISPLAY 'ENDOSSO  - ' CHVANT-NRENDOS
=                DISPLAY 'BILHETE  - ' CHVANT-NUM-BILH
=                DISPLAY 'TIP ENDS - ' CHVANT-TIPO-ENDS
=                GO   TO  R9999-00-ROT-ERRO
=          ELSE
=              MOVE   BILHETE-NUM-MATR   TO   FUNCICEF-NUM-MATR.
=     *
=      R1500-99-SAIDA.
=          EXIT.
=     *
=     *----------------------------------------------------------------*
=          EJECT
=     *----------------------------------------------------------------*
=     *
=      R1600-00-SELECT-FUNCIO-CEF   SECTION.
=     *-------------------------------------
=     *
=          MOVE           '160'           TO              WNR-EXEC-SQL.
=     *
=          EXEC  SQL
=             SELECT  NUM_MATRICULA,
=                     NOME_FUNCIONARIO,
=                     NUM_CPF
=               INTO :FUNCICEF-NUM-MATR,
=                    :FUNCICEF-NOM-FUNC,
=                    :FUNCICEF-NUM-CPF
=               FROM  SEGUROS.FUNCIONARIOS_CEF
=              WHERE  NUM_MATRICULA       =    :FUNCICEF-NUM-MATR
=               WITH  UR
=          END-EXEC.
=     *
=          IF    SQLCODE  NOT  EQUAL  ZEROS
=            IF  SQLCODE       EQUAL  100
=                MOVE     SPACES  TO  FUNCICEF-NOM-FUNC
=                MOVE     ZEROS   TO  FUNCICEF-NUM-CPF
=            ELSE
=                DISPLAY 'R1600 - ERRO NO SELECT DA FUNCIONARIOS-CEF'
=                DISPLAY 'APOLICE  - ' CHVANT-NUM-APOL
=                DISPLAY 'ENDOSSO  - ' CHVANT-NRENDOS
=                DISPLAY 'BILHETE  - ' CHVANT-NUM-BILH
=                DISPLAY 'TIP ENDS - ' CHVANT-TIPO-ENDS
=                DISPLAY 'MATR FUNC- ' FUNCICEF-NUM-MATR
=                GO   TO  R9999-00-ROT-ERRO.
=     *
=      R1600-99-SAIDA.
=          EXIT.
136081*
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
148834*
=      R1700-00-PROCESSA-RESSEGURO   SECTION.
=     *--------------------------------------
=     *
=          MOVE           '170'            TO         WNR-EXEC-SQL.
=     *
=          MOVE           ZEROS            TO         WPCT-QUOTA
=                                                     WPCT-COM-QUOTA.
=     *
=          INITIALIZE   LKRE-PARM-RE0001S.
=     *
=          MOVE      2                     TO         LKRE01-TIP-CALC.
=          MOVE      CHVANT-NUM-APOL       TO         LKRE01-NUM-APOL.
=          MOVE      CHVANT-NRENDOS        TO         LKRE01-NRENDOS.
=          MOVE      CHVANT-RAMO-CBT       TO         LKRE01-RAMOFR.
=          MOVE      CHVANT-MODL-CBT       TO         LKRE01-MODALIFR.
=     *
=          IF  ACC-VLPRMTAR-C  =  ZEROS
=              MOVE     ZEROS           TO      LKRE01-PCTCED
=          ELSE
=              COMPUTE  LKRE01-PCTCED  ROUNDED  =  (ACC-VLPRMTAR-C
=                                               *   100)
=                                               /   ACC-VLPRMTAR-T
=                   ON  SIZE  ERROR
=                             MOVE   ZEROS    TO    LKRE01-PCTCED
148834     END-IF.
154263*
=          IF  CHVANT-RAMO-CBT = 39 OR 47 OR 50 OR 51 OR 61 OR 65 OR
=              CHVANT-RAMO-CBT = 66 OR 67 OR 68 OR 78
=              MOVE   CHVANT-DTINIVIG           TO    LKRE01-DTINIVIG
=          ELSE
=            IF  CHVANT-RAMO-CBT = 40 OR 45 OR 75 OR 76
=                IF  CHVANT-DAT-EMIS  >  '2017-09-30'
=                    IF  CHVANT-NRENDOS  =  ZEROS
=                        MOVE   CHVANT-DAT-EMIS TO    LKRE01-DTINIVIG
=                    ELSE
=                        MOVE     SPACES       TO     WHOST-DTEMIS-AP
=                        PERFORM  R1800-00-SELECT-DTEMIS-APOL
=                    END-IF
=                ELSE
=                    MOVE  CHVANT-DTINIVIG     TO     LKRE01-DTINIVIG
=                END-IF
=            ELSE
=              IF  CHVANT-DAT-EMIS  >  '2014-08-31'
=                  MOVE  CHVANT-DAT-EMIS        TO  LKRE01-DTINIVIG
=              ELSE
=                  MOVE  V0RELA-PERI-FINAL      TO  LKRE01-DTINIVIG
=              END-IF
=            END-IF
=          END-IF.
154263*
148834     CALL     'RE0001S'      USING     LKRE-PARM-RE0001S.
=     *
=          IF  LKRE01-RTN-CODE  =  ZEROS  AND
=              LKRE01-SQL-CODE  =  ZEROS
=              COMPUTE  WPCT-QUOTA      ROUNDED  =  LKRE01-PCTCOT   * 1
=              COMPUTE  WPCT-COM-QUOTA  ROUNDED  =  LKRE01-PCTCOMCO * 1
=          ELSE
=              DISPLAY 'R1700 - ERRO NO CALL DA SUB-ROT RE0001S'
=              MOVE     LKRE01-SQL-CODE   TO    SQLCODE
=              DISPLAY 'MENSAGEM - ' LKRE01-MENSAGEM
=              DISPLAY 'APOLICE  - ' LKRE01-NUM-APOL
=              DISPLAY 'ENDOSSO  - ' LKRE01-NRENDOS
=              DISPLAY 'RAMO COB - ' LKRE01-RAMOFR
=              DISPLAY 'MODL COB - ' LKRE01-MODALIFR
=              DISPLAY 'INIC VIG - ' LKRE01-DTINIVIG
=              GO   TO  R9999-00-ROT-ERRO
=          END-IF.
=     *
=      R1700-99-SAIDA.
148834     EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
154263 R1800-00-SELECT-DTEMIS-APOL  SECTION.
=     *-------------------------------------
=     *
=          MOVE          '1800'           TO              WNR-EXEC-SQL.
=     *
=          EXEC  SQL
=                SELECT  DATA_EMISSAO
=                  INTO :WHOST-DTEMIS-AP
=                  FROM  SEGUROS.ENDOSSOS
=                 WHERE  NUM_APOLICE    =    :V0PREM-NUM-APOL
=                   AND  NUM_ENDOSSO    =     00
=                  WITH  UR
=          END-EXEC.
=     *
=          IF  SQLCODE  NOT   EQUAL  ZEROS
=              DISPLAY 'R1800 - ERRO NO SELECT DA ENDOSSOS'
=              DISPLAY 'APOLICE - '  V0PREM-NUM-APOL
=              GO   TO  R9999-00-ROT-ERRO
=          ELSE
=              MOVE     WHOST-DTEMIS-AP     TO     LKRE01-DTINIVIG
=          END-IF.
=     *
=      R1800-99-SAIDA.
=          EXIT.
154263*
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R3000-00-GRAVA-COSSEG-CED   SECTION.
      *------------------------------------
      *
           MOVE         '300'             TO         WNR-EXEC-SQL.
      *
           ADD       1                    TO         WS-SEQ-PREMCED.
           MOVE      WS-SEQ-PREMCED       TO         CED-SEQ.
      *
           IF  WCOD-ORGAO-ANT  =  100
               MOVE     06238             TO         CED-COD-COSS
           ELSE
               MOVE     05118             TO         CED-COD-COSS.
      *
           MOVE      ACC-VLPRMLIQ-C       TO         CED-PR-COS-CED.
           MOVE      ACC-VLCOMIS-C        TO         CED-COMIS-COSS.
      *
           MOVE      CHVANT-CODPRODU      TO         CED-COD-PRODU.
      *
103462     IF  CED-NUM-END  EQUAL  ZEROS
=              MOVE '00000000000000000000'    TO     CED-NUM-ENDOSSO
103462     END-IF.
      *
           WRITE     REG-PREMCED        FROM         REGT-PREMCED.
      *
           IF  CED-STATUS  =  ZEROS
               ADD      1     TO      AC-G-PREMCED
           ELSE
               DISPLAY 'R3000 - ERRO NO WRITE DO ARQ PREMCED'
               DISPLAY 'STATUS      - '  CED-STATUS
               DISPLAY 'ANO REFER   - '  V0RELA-ANO-REFER
               DISPLAY 'MES REFER   - '  V0RELA-MES-REFER
               DISPLAY 'RAMO SUSEP  - '  CHVANT-RAMO-SUSEP
               DISPLAY 'NR APOLICE  - '  CHVANT-NUM-APOL
               DISPLAY 'ENDS MOVTO  - '  CHVANT-NRENDOCA
               DISPLAY 'NR ENDOSSO  - '  CHVANT-NRENDOS
               DISPLAY 'TIPO ENDO   - '  CHVANT-TIPO-ENDS
               DISPLAY 'DATA MOVTO  - '  CHVANT-DATA-MOVT
               DISPLAY 'TIPO OPER   - '  CHVANT-TIPO-OPER
               GO   TO  R9999-00-ROT-ERRO.
      *
       R3000-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R3100-00-PROCESSA-COSG-CED  SECTION.
      *------------------------------------
      *
           MOVE           '310'          TO             WNR-EXEC-SQL.
      *
           MOVE           ZEROS          TO             WHOST-QTDE-REG
                                                        WHOST-QTD-COSG
                                                        WHOST-PERC-CED
                                                        WHOST-COD-COSG
                                                        WHOST-PCT-COSG.
      *
      *--* CONFIRMA SE A DISTRIBUICAO DE COSSEG CEDIDO EH PROPORCIONAL
      *--* AO RAMO DE COBERTURA E OBTEM OS PERCENTUAIS PROPORCIONAIS POR
      *--* COBERTURA, SENAO, TRATA COSSEGURO CEDIDO POR APOLICE
      *
           PERFORM      R4600-00-SELECT-GE397.
      *
           IF  WHOST-QTDE-REG  =  ZEROS
               PERFORM  R4700-00-PROCESSA-APOL-COSG
           ELSE
               PERFORM  R5100-00-PROCESSA-COSG-COBT
           END-IF.
      *
           PERFORM      R5500-00-CALCULA-COSG-CED  UNTIL
                        WFIM-COSSEG-CED   =   'S'.
      *
       R3100-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R4600-00-SELECT-GE397      SECTION.
      *-----------------------------------
      *
           MOVE          '460'          TO                WNR-EXEC-SQL.
      *
           EXEC  SQL   SELECT
                 VALUE(COUNT(*),+0)
                 INTO :WHOST-QTDE-REG
                 FROM  SEGUROS.GE_ENDOS_COSSEG_COBER
                WHERE  NUM_APOLICE     =      :WHOST-NUM-APOL
                  AND  NUM_ENDOSSO     =      :WHOST-NUM-ENDS
           END-EXEC.
      *
           IF    SQLCODE  NOT  EQUAL  ZEROS
             IF  SQLCODE       EQUAL  100
                 MOVE   ZEROS     TO  WHOST-QTDE-REG
             ELSE
                 DISPLAY 'R4600 - ERRO NO SELECT DA GE-ENDOS-COSSEG-COB'
                 DISPLAY 'APOLICE  - ' WHOST-NUM-APOL
                 DISPLAY 'ENDOSSO  - ' WHOST-NUM-ENDS
                 DISPLAY 'RAMO CBT - ' WHOST-RMO-COBT
                 DISPLAY 'INI VIGC - ' WHOST-DTINIVIG
                 GO   TO  R9999-00-ROT-ERRO
             END-IF
           END-IF.
      *
       R4600-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R4700-00-PROCESSA-APOL-COSG  SECTION.
      *-------------------------------------
      *
           MOVE          '470'            TO            WNR-EXEC-SQL.
      *
           PERFORM   R4800-00-SELECT-V0APOLCOSCED.
      *
           IF  WHOST-QTD-COSG  =  ZEROS
               DISPLAY 'R4700 - QTDE DE CONG. DIVERGE DO VALOR CEDIDO'
               DISPLAY 'APOLICE  - ' WHOST-NUM-APOL
               DISPLAY 'ENDOSSO  - ' WHOST-NUM-ENDS
               DISPLAY 'RAMO CBT - ' WHOST-RMO-COBT
               DISPLAY 'INI VIGC - ' WHOST-DTINIVIG
               GO   TO  R9999-00-ROT-ERRO.
      *
           PERFORM   R4900-00-DECLARE-V0APOLCOSCED.
      *
           PERFORM   R5000-00-FETCH-V0APOLCOSCED.
      *
       R4700-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R4800-00-SELECT-V0APOLCOSCED  SECTION.
      *--------------------------------------
      *
           MOVE           '480'            TO             WNR-EXEC-SQL.
      *
           EXEC  SQL   SELECT
                 VALUE(COUNT(*),+0),
                 VALUE(SUM(PCPARTIC),+0)
                 INTO :WHOST-QTD-COSG,
                      :WHOST-PERC-CED
                 FROM  SEGUROS.V0APOLCOSCED
                WHERE  NUM_APOLICE        =    :WHOST-NUM-APOL
                  AND  DTINIVIG          <=    :WHOST-DTINIVIG
                  AND  DTTERVIG          >=    :WHOST-DTINIVIG
                  AND  PCPARTIC          >      0
                 WITH  UR
           END-EXEC.
      *
           IF    SQLCODE  NOT  EQUAL  ZEROS
             IF  SQLCODE       EQUAL  100
                 MOVE     ZEROS   TO  WHOST-QTD-COSG
                 MOVE     ZEROS   TO  WHOST-PERC-CED
             ELSE
                 DISPLAY 'R4800 - ERRO NO SELECT DA V0APOLCOSCED'
                 DISPLAY 'APOLICE  - ' WHOST-NUM-APOL
                 DISPLAY 'ENDOSSO  - ' WHOST-NUM-ENDS
                 DISPLAY 'RAMO CBT - ' WHOST-RMO-COBT
                 DISPLAY 'INI VIGC - ' WHOST-DTINIVIG
                 GO   TO  R9999-00-ROT-ERRO.
      *
       R4800-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R4900-00-DECLARE-V0APOLCOSCED  SECTION.
      *---------------------------------------
      *
           MOVE           '490'             TO            WNR-EXEC-SQL.
      *
           EXEC  SQL     DECLARE        V0APOLCOSCED      CURSOR   FOR
                 SELECT  CODCOSS ,
                         PCPARTIC,
                         PCCOMCOS
                   FROM  SEGUROS.V0APOLCOSCED
                  WHERE  NUM_APOLICE        =    :WHOST-NUM-APOL
                    AND  DTINIVIG          <=    :WHOST-DTINIVIG
                    AND  DTTERVIG          >=    :WHOST-DTINIVIG
                    AND  PCPARTIC          >      0
                   WITH  UR
           END-EXEC.
      *
           EXEC  SQL    OPEN      V0APOLCOSCED     END-EXEC.
      *
           IF  SQLCODE  NOT   EQUAL  ZEROS
               DISPLAY 'R4900 - ERRO NO DECLARE DA V0APOLCOSCED'
               GO   TO  R9999-00-ROT-ERRO
           ELSE
               MOVE     SPACES    TO    WFIM-COSSEG-CED.
      *
       R4900-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R5000-00-FETCH-V0APOLCOSCED  SECTION.
      *-------------------------------------
      *
           MOVE          '500'            TO              WNR-EXEC-SQL.
      *
           EXEC  SQL     FETCH            V0APOLCOSCED
                 INTO   :V0APCD-COD-COSS,
                        :V0APCD-PCPARTIC,
                        :V0APCD-PCCOMCOS
           END-EXEC.
      *
           IF    SQLCODE  NOT  EQUAL   ZEROS
             IF  SQLCODE       EQUAL   100
                 MOVE     'S'     TO   WFIM-COSSEG-CED
                 EXEC     SQL  CLOSE        V0APOLCOSCED   END-EXEC
             ELSE
                 DISPLAY 'R5000 - ERRO NO FETCH DA V0APOLCOSCED'
                 GO   TO  R9999-00-ROT-ERRO
             END-IF
           ELSE
               MOVE   V0APCD-COD-COSS  TO  WHOST-COD-COSG
               MOVE   V0APCD-PCPARTIC  TO  WHOST-PCT-COSG
           END-IF.
      *
       R5000-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R5100-00-PROCESSA-COSG-COBT  SECTION.
      *-------------------------------------
      *
           MOVE          '510'            TO              WNR-EXEC-SQL.
      *
           PERFORM   R5200-00-SELECT-GE399.
      *
           IF  WHOST-QTD-COSG  =  ZEROS
               DISPLAY 'R5100 - QTDE DE CONG. DIVERGE DO VALOR CEDIDO'
               DISPLAY 'APOLICE  - ' WHOST-NUM-APOL
               DISPLAY 'ENDOSSO  - ' WHOST-NUM-ENDS
               DISPLAY 'RAMO CBT - ' WHOST-RMO-COBT
               DISPLAY 'INI VIGC - ' WHOST-DTINIVIG
               GO   TO  R9999-00-ROT-ERRO
           END-IF.
      *
           PERFORM   R5300-00-DECLARE-GE399.
      *
           PERFORM   R5400-00-FETCH-GE399.
      *
       R5100-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R5200-00-SELECT-GE399      SECTION.
      *-----------------------------------
      *
           MOVE          '520'          TO                WNR-EXEC-SQL.
      *
           EXEC  SQL   SELECT
                 VALUE(COUNT(*),+0),
                 VALUE(SUM(PCT_PROP_RAMO_PR),+0)
                 INTO :WHOST-QTD-COSG,
                      :WHOST-PERC-CED
                 FROM  SEGUROS.GE_ENDOS_RAMO_VLR_COSSEG
                WHERE  NUM_APOLICE          =   :WHOST-NUM-APOL
                  AND  NUM_ENDOSSO          =   :WHOST-NUM-ENDS
                  AND  COD_RAMO_COBER       =   :WHOST-RMO-COBT
                 WITH  UR
           END-EXEC.
      *
           IF    SQLCODE  NOT  EQUAL  ZEROS
             IF  SQLCODE       EQUAL  100
                 MOVE     ZEROS   TO  WHOST-QTD-COSG
                 MOVE     ZEROS   TO  WHOST-PERC-CED
             ELSE
                 DISPLAY 'R5200 - ERRO NO SELECT DA GE-END-RAMO-VLR-CSG'
                 DISPLAY 'APOLICE  - ' WHOST-NUM-APOL
                 DISPLAY 'ENDOSSO  - ' WHOST-NUM-ENDS
                 DISPLAY 'RAMO CBT - ' WHOST-RMO-COBT
                 DISPLAY 'INI VIGC - ' WHOST-DTINIVIG
                 GO   TO  R9999-00-ROT-ERRO
             END-IF
           END-IF.
      *
       R5200-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R5300-00-DECLARE-GE399      SECTION.
      *------------------------------------
      *
           MOVE          '530'           TO              WNR-EXEC-SQL.
      *
           EXEC  SQL     DECLARE      GE399              CURSOR   FOR
                 SELECT  COD_COSSEGURADORA ,
                         PCT_PROP_RAMO_PR ,
                         PCT_PROP_COM_RAMO
                   FROM  SEGUROS.GE_ENDOS_RAMO_VLR_COSSEG
                  WHERE  NUM_APOLICE          =   :WHOST-NUM-APOL
                    AND  NUM_ENDOSSO          =   :WHOST-NUM-ENDS
                    AND  COD_RAMO_COBER       =   :WHOST-RMO-COBT
                  ORDER  BY
                         COD_COSSEGURADORA
                   WITH  UR
           END-EXEC.
      *
           EXEC  SQL    OPEN         GE399         END-EXEC.
      *
           IF  SQLCODE  NOT   EQUAL  ZEROS
               DISPLAY 'R5300 - ERRO NO DECLARE DA GE-END-RAMO-VLR-COSG'
               DISPLAY 'APOLICE  - ' WHOST-NUM-APOL
               DISPLAY 'ENDOSSO  - ' WHOST-NUM-ENDS
               DISPLAY 'RAMO CBT - ' WHOST-RMO-COBT
               DISPLAY 'INI VIGC - ' WHOST-DTINIVIG
               GO   TO  R9999-00-ROT-ERRO
           ELSE
               MOVE     SPACES    TO    WFIM-COSSEG-CED
           END-IF.
      *
       R5300-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R5400-00-FETCH-GE399       SECTION.
      *-----------------------------------
      *
           MOVE          '540'          TO                WNR-EXEC-SQL.
      *
           EXEC  SQL    FETCH           GE399
                 INTO  :GE399-COD-COSG ,
                       :GE399-PCT-RMO-PR ,
                       :GE399-PCT-COM-RM
           END-EXEC.
      *
           IF    SQLCODE  NOT  EQUAL   ZEROS
             IF  SQLCODE       EQUAL   100
                 MOVE     'S'     TO   WFIM-COSSEG-CED
                 EXEC     SQL  CLOSE        GE399          END-EXEC
             ELSE
                 DISPLAY 'R5400 - ERRO NO FETCH DA GE-END-RAMO-VLR-COSG'
                 DISPLAY 'APOLICE  - ' WHOST-NUM-APOL
                 DISPLAY 'ENDOSSO  - ' WHOST-NUM-ENDS
                 DISPLAY 'RAMO CBT - ' WHOST-RMO-COBT
                 DISPLAY 'INI VIGC - ' WHOST-DTINIVIG
                 GO   TO  R9999-00-ROT-ERRO
             END-IF
           ELSE
               MOVE   GE399-COD-COSG     TO   WHOST-COD-COSG
               MOVE   GE399-PCT-RMO-PR   TO   WHOST-PCT-COSG
           END-IF.
      *
       R5400-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R5500-00-CALCULA-COSG-CED   SECTION.
      *------------------------------------
      *
           MOVE          '550'           TO              WNR-EXEC-SQL.
      *
           MOVE          ZEROS           TO              WS-VLPRMTAR-C
                                                         WS-VLPRMLIQ-C
                                                         WS-VLCOMISS-C.
      *
      *--* VALORES DE PREMIOS E COMISSOES POR CONGENERE
      *
           COMPUTE   WS-VLPRMTAR-C   ROUNDED   =     ACC-VLPRMTAR-C
                                               *    (WHOST-PCT-COSG
                                               /     WHOST-PERC-CED)
                ON   SIZE  ERROR
                           MOVE   ZEROS   TO   WS-VLPRMTAR-C.
      *
           COMPUTE   WS-VLPRMLIQ-C   ROUNDED   =     ACC-VLPRMLIQ-C
                                               *    (WHOST-PCT-COSG
                                               /     WHOST-PERC-CED)
                ON   SIZE  ERROR
                           MOVE   ZEROS   TO   WS-VLPRMLIQ-C.
      *
           COMPUTE   WS-VLCOMISS-C   ROUNDED   =     ACC-VLCOMIS-C
                                               *    (WHOST-PCT-COSG
                                               /     WHOST-PERC-CED)
                ON   SIZE  ERROR
                           MOVE   ZEROS   TO   WS-VLCOMISS-C.
      *
      *--* GRAVA REGISTRO DE COSSEGURO CEDIDO
      *
           ADD       1                    TO         WS-SEQ-PREMCED.
           MOVE      WS-SEQ-PREMCED       TO         CED-SEQ.
      *
           MOVE      WHOST-COD-COSG       TO         CED-COD-COSS.
      *
           MOVE      WS-VLPRMLIQ-C        TO         CED-PR-COS-CED.
           MOVE      WS-VLCOMISS-C        TO         CED-COMIS-COSS.
      *
           MOVE      CHVANT-CODPRODU      TO         CED-COD-PRODU.
      *
           WRITE     REG-PREMCED        FROM         REGT-PREMCED.
      *
           IF  CED-STATUS  =  ZEROS
               ADD      1     TO      AC-G-PREMCED
           ELSE
               DISPLAY 'R5500 - ERRO NO WRITE DO ARQ PREMCED'
               DISPLAY 'STATUS      - '  CED-STATUS
               DISPLAY 'ANO REFER   - '  V0RELA-ANO-REFER
               DISPLAY 'MES REFER   - '  V0RELA-MES-REFER
               DISPLAY 'RAMO SUSEP  - '  CHVANT-RAMO-SUSEP
               DISPLAY 'NR APOLICE  - '  CHVANT-NUM-APOL
               DISPLAY 'ENDS MOVTO  - '  CHVANT-NRENDOCA
               DISPLAY 'NR ENDOSSO  - '  CHVANT-NRENDOS
               DISPLAY 'TIPO ENDO   - '  CHVANT-TIPO-ENDS
               DISPLAY 'DATA MOVTO  - '  CHVANT-DATA-MOVT
               DISPLAY 'TIPO OPER   - '  CHVANT-TIPO-OPER
               GO   TO  R9999-00-ROT-ERRO.
      *
      *--* LER PROXIMO REGISTRO
      *
           IF  WHOST-QTDE-REG  =  ZEROS
               PERFORM   R5000-00-FETCH-V0APOLCOSCED
           ELSE
               PERFORM   R5400-00-FETCH-GE399
           END-IF.
      *
       R5500-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *                ROTINA DE ENCERRAMENTO SEM MOVIMENTO            *
      *----------------------------------------------------------------*
      *
       R9900-00-ENCERRA-SEM-MOVTO   SECTION.
      *-------------------------------------
      *
           DISPLAY     '*------------------------------------------*'.
           DISPLAY     '*                                          *'.
           DISPLAY     '* RG1866B - CIRC. SUSEP 360 - PREMIT       *'.
           DISPLAY     '* -------                     PREMCED      *'.
           DISPLAY     '*                                          *'.
           DISPLAY     '*    NAO HOUVE MOVIMENTACAO NO PERIODO     *'.
           DISPLAY     '*                                          *'.
           DISPLAY     '*------------------------------------------*'.
      *
       R9900-99-SAIDA.
           EXIT.
      *
      *----------------------------------------------------------------*
           EJECT
      *----------------------------------------------------------------*
      *
       R9999-00-ROT-ERRO         SECTION.
      *----------------------------------
      *
           CLOSE    PREMIT.
      *
           CLOSE    PREMCED.
      *
           MOVE     SQLCODE    TO      WSQLCODE.
      *
           DISPLAY  WABEND.
      *
           EXEC  SQL   ROLLBACK   WORK    END-EXEC.
      *
           MOVE   99   TO         RETURN-CODE.
      *
           STOP   RUN.
      *
      *----RG1866B-----------------------------------------------------*
      *
