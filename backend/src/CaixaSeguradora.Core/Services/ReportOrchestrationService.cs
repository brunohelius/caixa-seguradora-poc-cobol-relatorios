using CaixaSeguradora.Core.DTOs;
using CaixaSeguradora.Core.Interfaces;
using Microsoft.Extensions.Logging;

namespace CaixaSeguradora.Core.Services
{
    /// <summary>
    /// Core orchestration service for report generation workflow.
    /// Coordinates data fetching, aggregation, validation, and file generation.
    /// Implements the main processing loop equivalent to COBOL sections R0500-R0600.
    /// </summary>
    public class ReportOrchestrationService
    {
        private readonly IPremiumRepository _premiumRepository;
        private readonly IPolicyRepository _policyRepository;
        private readonly IProductRepository _productRepository;
        private readonly IClientRepository _clientRepository;
        private readonly IAddressRepository _addressRepository;
        private readonly ICossuranceRepository _cossuranceRepository;
        private readonly IExecutionTrackingService _executionTrackingService;
        private readonly IFileWriterService _fileWriterService;
        private readonly IBusinessRuleValidationService _validationService;
        private readonly ILogger<ReportOrchestrationService> _logger;

        public ReportOrchestrationService(
            IPremiumRepository premiumRepository,
            IPolicyRepository policyRepository,
            IProductRepository productRepository,
            IClientRepository clientRepository,
            IAddressRepository addressRepository,
            ICossuranceRepository cossuranceRepository,
            IExecutionTrackingService executionTrackingService,
            IFileWriterService fileWriterService,
            IBusinessRuleValidationService validationService,
            ILogger<ReportOrchestrationService> logger)
        {
            _premiumRepository = premiumRepository;
            _policyRepository = policyRepository;
            _productRepository = productRepository;
            _clientRepository = clientRepository;
            _addressRepository = addressRepository;
            _cossuranceRepository = cossuranceRepository;
            _executionTrackingService = executionTrackingService;
            _fileWriterService = fileWriterService;
            _validationService = validationService;
            _logger = logger;
        }

        /// <summary>
        /// Generates monthly premium report for specified month.
        /// Main orchestration method that coordinates all processing phases.
        /// </summary>
        /// <param name="request">Report generation request with month and parameters</param>
        /// <param name="cancellationToken">Cancellation token</param>
        /// <returns>Execution ID for tracking progress</returns>
        public async Task<Guid> GenerateReportAsync(
            GenerateReportRequest request,
            CancellationToken cancellationToken = default)
        {
            // Phase 1: Create execution tracking record
            var executionId = await _executionTrackingService.CreateExecutionAsync(
                request.Month,
                request.ReportType,
                request.ExecutionMode,
                request.TriggeringUser ?? "System",
                cancellationToken);

            try
            {
                // Phase 2: Update status to Running
                await _executionTrackingService.UpdateStatusAsync(executionId, "Running", cancellationToken);
                await _executionTrackingService.LogEventAsync(
                    executionId,
                    "INFO",
                    "R0500-00",
                    $"Iniciando geração de relatório para mês {request.Month}",
                    cancellationToken: cancellationToken);

                // Phase 3: Validate month parameter
                if (!ValidateMonth(request.Month, out var validationError))
                {
                    await _executionTrackingService.LogEventAsync(
                        executionId,
                        "ERROR",
                        "R0500-10",
                        $"Validação do mês falhou: {validationError}",
                        cancellationToken: cancellationToken);

                    await _executionTrackingService.CompleteExecutionAsync(
                        executionId,
                        "0008", // Error return code
                        0, 0, 0, 1,
                        cancellationToken);

                    throw new ArgumentException(validationError);
                }

                // Phase 4: Calculate date range from month
                var (startDate, endDate) = ParseMonthToDateRange(request.Month);

                await _executionTrackingService.LogEventAsync(
                    executionId,
                    "INFO",
                    "R0500-20",
                    $"Período de processamento: {startDate:yyyy-MM-dd} a {endDate:yyyy-MM-dd}",
                    cancellationToken: cancellationToken);

                // Phase 5: Fetch premium records (data retrieval phase)
                await _executionTrackingService.LogEventAsync(
                    executionId,
                    "INFO",
                    "R0600-00",
                    "Iniciando busca e processamento de registros de prêmios (cursor-based streaming)",
                    cancellationToken: cancellationToken);

                // Phase 7: Process premium records using cursor-based streaming (business logic phase)
                // Memory-efficient: processes records one at a time without loading all into memory
                int processedCount = 0;
                int premitRecords = 0;
                int premcedRecords = 0;
                int warningsCount = 0;
                int errorsCount = 0;
                int autoCorrectedCount = 0;

                await _executionTrackingService.LogEventAsync(
                    executionId,
                    "INFO",
                    "R0700-00",
                    "Iniciando processamento de registros de prêmios em modo streaming",
                    cancellationToken: cancellationToken);

                // Open cursor and stream records (COBOL: OPEN CUR-V0PREMIOS, FETCH loop)
                var premiumsEnumerable = _premiumRepository.GetPremiumsForReportAsync(
                    startDate,
                    endDate,
                    cancellationToken);

                await foreach (var premium in premiumsEnumerable)
                {
                    try
                    {
                        // Phase 5 (US3): Validate business rules before processing
                        var validationResult = await ValidatePremiumRecordAsync(
                            premium,
                            executionId,
                            cancellationToken);

                        // Track auto-corrections
                        if (validationResult.AutoCorrected.Any())
                        {
                            autoCorrectedCount += validationResult.AutoCorrected.Count;
                        }

                        // Track warnings
                        if (validationResult.Warnings.Any())
                        {
                            warningsCount += validationResult.Warnings.Count;
                        }

                        // Skip invalid records
                        if (!validationResult.IsValid)
                        {
                            errorsCount += validationResult.Errors.Count;
                            await _executionTrackingService.LogEventAsync(
                                executionId,
                                "ERROR",
                                "R0800-00",
                                $"Registro rejeitado devido a erros de validação: {string.Join(", ", validationResult.Errors.Select(e => e.Message))}",
                                premium.PolicyNumber,
                                cancellationToken: cancellationToken);
                            continue; // Skip to next record
                        }

                        // Process individual premium record (simplified for Phase 3)
                        // Full calculation logic will be in Phase 4 (US2)
                        await ProcessPremiumRecordAsync(premium, executionId, cancellationToken);

                        processedCount++;
                        premitRecords++; // For now, count all as PREMIT records

                        // Update progress every 1000 records for large dataset efficiency (US5 requirement)
                        if (processedCount % 1000 == 0)
                        {
                            await _executionTrackingService.UpdateProgressAsync(
                                executionId,
                                processedCount,
                                cancellationToken);

                            await _executionTrackingService.LogEventAsync(
                                executionId,
                                "INFO",
                                "R0700-50",
                                $"Progresso: {processedCount} registros processados (Erros: {errorsCount}, Avisos: {warningsCount})",
                                cancellationToken: cancellationToken);
                        }
                        // Log progress every 100 records for near-real-time monitoring
                        else if (processedCount % 100 == 0)
                        {
                            _logger.LogInformation(
                                "Processed {Count} premium records (Errors: {Errors}, Warnings: {Warnings})",
                                processedCount, errorsCount, warningsCount);
                        }
                    }
                    catch (Exception ex)
                    {
                        errorsCount++;
                        await _executionTrackingService.LogEventAsync(
                            executionId,
                            "ERROR",
                            "R0700-10",
                            $"Erro ao processar prêmio: {ex.Message}",
                            premium.PolicyNumber,
                            ex.StackTrace,
                            cancellationToken);
                    }
                }

                // Final progress update
                await _executionTrackingService.UpdateProgressAsync(
                    executionId,
                    processedCount,
                    cancellationToken);

                // Phase 8: Data aggregation and validation (summary phase)
                await _executionTrackingService.LogEventAsync(
                    executionId,
                    "INFO",
                    "R1300-00",
                    $"Processamento concluído: {processedCount} registros processados",
                    cancellationToken: cancellationToken);

                // Phase 9: File generation (stub for now - Phase 6 will implement)
                await _executionTrackingService.LogEventAsync(
                    executionId,
                    "INFO",
                    "R5600-00",
                    "Geração de arquivos será implementada na Fase 6 (US4)",
                    cancellationToken: cancellationToken);

                // Phase 10: Complete execution with final statistics
                var returnCode = errorsCount > 0 ? "0008" : (warningsCount > 0 ? "0004" : "0000");

                await _executionTrackingService.CompleteExecutionAsync(
                    executionId,
                    returnCode,
                    premitRecords,
                    premcedRecords,
                    warningsCount,
                    errorsCount,
                    cancellationToken);

                await _executionTrackingService.LogEventAsync(
                    executionId,
                    "INFO",
                    "R9999-00",
                    $"Execução finalizada. RC={returnCode}, Processados={processedCount}, Erros={errorsCount}, Avisos={warningsCount}, Correções={autoCorrectedCount}",
                    cancellationToken: cancellationToken);

                return executionId;
            }
            catch (Exception ex)
            {
                _logger.LogError(ex, "Error generating report for execution {ExecutionId}", executionId);

                await _executionTrackingService.LogEventAsync(
                    executionId,
                    "ERROR",
                    "R9999-99",
                    $"Erro crítico durante geração do relatório: {ex.Message}",
                    stackTrace: ex.StackTrace,
                    cancellationToken: cancellationToken);

                await _executionTrackingService.CompleteExecutionAsync(
                    executionId,
                    "0012", // Critical error return code
                    0, 0, 0, 1,
                    cancellationToken);

                throw;
            }
        }

        /// <summary>
        /// Validates a premium record against business rules.
        /// COBOL equivalent: Section R0800-00-VALIDA-REGISTRO
        /// Phase 5 (US3): Business rule validation
        /// </summary>
        private async Task<Models.ValidationResult> ValidatePremiumRecordAsync(
            Core.Entities.PremiumRecord premium,
            Guid executionId,
            CancellationToken cancellationToken)
        {
            // Fetch related data for validation
            var policy = await _policyRepository.GetByPolicyNumberAsync(
                premium.PolicyNumber,
                cancellationToken);

            var product = policy != null
                ? await _productRepository.GetByProductCodeAsync(policy.ProductCode, cancellationToken)
                : null;

            // Run validation service
            var validationResult = await _validationService.ValidatePremiumAsync(
                premium,
                policy,
                product,
                cancellationToken);

            // Log validation results
            foreach (var error in validationResult.Errors)
            {
                await _executionTrackingService.LogEventAsync(
                    executionId,
                    "ERROR",
                    "R0800-10",
                    $"Validação falhou - {error.ErrorCode}: {error.Message} (Campo: {error.FieldName})",
                    premium.PolicyNumber,
                    cancellationToken: cancellationToken);
            }

            foreach (var warning in validationResult.Warnings)
            {
                await _executionTrackingService.LogEventAsync(
                    executionId,
                    "WARNING",
                    "R0800-20",
                    $"Aviso - {warning.WarningCode}: {warning.Message} (Campo: {warning.FieldName})",
                    premium.PolicyNumber,
                    cancellationToken: cancellationToken);
            }

            foreach (var correction in validationResult.AutoCorrected)
            {
                await _executionTrackingService.LogEventAsync(
                    executionId,
                    "INFO",
                    "R0800-30",
                    $"Auto-correção - {correction.FieldName}: {correction.OriginalValue} → {correction.CorrectedValue}. Motivo: {correction.Reason}",
                    premium.PolicyNumber,
                    cancellationToken: cancellationToken);
            }

            return validationResult;
        }

        /// <summary>
        /// Processes a single premium record with nested cursor patterns (US5 implementation).
        /// Demonstrates memory-efficient streaming for related data (addresses, cossurance).
        /// Full calculation logic will be implemented in Phase 4 (US2).
        /// </summary>
        private async Task ProcessPremiumRecordAsync(
            Core.Entities.PremiumRecord premium,
            Guid executionId,
            CancellationToken cancellationToken)
        {
            // Phase 3: Simplified processing - just validate data exists
            // Phase 4 will add premium calculations (COBOL sections R0700-R1300)
            // Phase 5 validation already applied before calling this method

            // Fetch related data for validation
            var policy = await _policyRepository.GetByPolicyNumberAsync(
                premium.PolicyNumber,
                cancellationToken);

            if (policy == null)
            {
                throw new InvalidOperationException(
                    $"Apólice não encontrada: {premium.PolicyNumber}");
            }

            // Phase 7 (US5): Nested cursor pattern for addresses (COBOL R1220-R1240)
            // Stream addresses without loading all into memory
            var addressCount = 0;
            await foreach (var address in _addressRepository.GetAddressesByClientAsync(
                policy.ProposerClientCode,
                cancellationToken))
            {
                addressCount++;
                // Process address (e.g., find residential address for PREMIT output)
                if (address.AddressType == "R")
                {
                    // Use residential address for reporting
                    _logger.LogTrace(
                        "Found residential address for client {ClientCode}: {State}",
                        policy.ProposerClientCode,
                        address.State);
                    break; // Stop after finding first residential address
                }
            }

            // Phase 7 (US5): Nested cursor pattern for cossurance (COBOL R5000-R5500)
            // Check if policy has cossurance arrangements for PREMCED generation
            var hasCossurance = await _cossuranceRepository.HasCossuranceAsync(
                premium.PolicyNumber,
                cancellationToken);

            if (hasCossurance)
            {
                var cossuranceCount = 0;
                await foreach (var cossurance in _cossuranceRepository.GetCossuranceByPolicyAsync(
                    premium.PolicyNumber,
                    cancellationToken))
                {
                    cossuranceCount++;
                    // Process cossurance record for PREMCED output
                    _logger.LogTrace(
                        "Processing cossurance for policy {PolicyNumber}: Cossurer {CossurerCode}, Share {Percentage}%",
                        premium.PolicyNumber,
                        cossurance.CossurerCode,
                        cossurance.ParticipationPercentage * 100);
                }

                _logger.LogDebug(
                    "Processed {Count} cossurance records for policy {PolicyNumber}",
                    cossuranceCount,
                    premium.PolicyNumber);
            }

            // Log processing (debug level - not stored in DB for performance)
            _logger.LogDebug(
                "Processed premium for policy {PolicyNumber}, premium {PremiumAmount}, addresses {AddressCount}, hasCossurance {HasCossurance}",
                premium.PolicyNumber,
                premium.NetPremiumAmount,
                addressCount,
                hasCossurance);

            // Additional processing will be added in Phase 4
        }

        /// <summary>
        /// Validates month parameter format and business rules.
        /// </summary>
        private bool ValidateMonth(string month, out string? errorMessage)
        {
            errorMessage = null;

            // Check format: YYYYMM
            if (string.IsNullOrEmpty(month) || month.Length != 6)
            {
                errorMessage = "Mês deve estar no formato YYYYMM";
                return false;
            }

            if (!int.TryParse(month, out _))
            {
                errorMessage = "Mês deve conter apenas dígitos";
                return false;
            }

            // Parse year and month
            var year = int.Parse(month[..4]);
            var monthNum = int.Parse(month[4..]);

            if (monthNum < 1 || monthNum > 12)
            {
                errorMessage = "Mês inválido (deve ser 01-12)";
                return false;
            }

            // Check not future month
            var referenceDate = new DateTime(year, monthNum, 1);
            var currentMonth = new DateTime(DateTime.UtcNow.Year, DateTime.UtcNow.Month, 1);

            if (referenceDate > currentMonth)
            {
                errorMessage = "Não é permitido gerar relatório para mês futuro";
                return false;
            }

            return true;
        }

        /// <summary>
        /// Converts YYYYMM format to date range (first day to last day of month).
        /// </summary>
        private (DateTime StartDate, DateTime EndDate) ParseMonthToDateRange(string month)
        {
            var year = int.Parse(month[..4]);
            var monthNum = int.Parse(month[4..]);

            var startDate = new DateTime(year, monthNum, 1);
            var endDate = startDate.AddMonths(1).AddDays(-1);

            return (startDate, endDate);
        }
    }
}
