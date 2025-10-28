using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using PdfGenerator.Models;

namespace PdfGenerator.Services
{
    /// <summary>
    /// Implementation of function point calculation using IFPUG 4.3.1 methodology.
    /// </summary>
    public class FunctionPointCalculator : IFunctionPointCalculator
    {
        private const decimal RATE_PER_FUNCTION_POINT = 750m; // R$ 750 per FP

        // IFPUG Complexity Weight Table
        private readonly Dictionary<(FunctionType, ComplexityLevel), int> _ifpugWeights = new()
        {
            // Internal Logical Files (ILF)
            { (FunctionType.ILF, ComplexityLevel.Low), 7 },
            { (FunctionType.ILF, ComplexityLevel.Average), 10 },
            { (FunctionType.ILF, ComplexityLevel.High), 15 },

            // External Interface Files (EIF)
            { (FunctionType.EIF, ComplexityLevel.Low), 5 },
            { (FunctionType.EIF, ComplexityLevel.Average), 7 },
            { (FunctionType.EIF, ComplexityLevel.High), 10 },

            // External Inputs (EI)
            { (FunctionType.EI, ComplexityLevel.Low), 3 },
            { (FunctionType.EI, ComplexityLevel.Average), 4 },
            { (FunctionType.EI, ComplexityLevel.High), 6 },

            // External Outputs (EO)
            { (FunctionType.EO, ComplexityLevel.Low), 4 },
            { (FunctionType.EO, ComplexityLevel.Average), 5 },
            { (FunctionType.EO, ComplexityLevel.High), 7 },

            // External Inquiries (EQ)
            { (FunctionType.EQ, ComplexityLevel.Low), 3 },
            { (FunctionType.EQ, ComplexityLevel.Average), 4 },
            { (FunctionType.EQ, ComplexityLevel.High), 6 }
        };

        /// <summary>
        /// Calculates the total function points for a system based on IFPUG methodology.
        /// </summary>
        public async Task<FunctionPointResult> CalculateFunctionPointsAsync(IEnumerable<FunctionPoint> functionPoints)
        {
            return await Task.Run(() =>
            {
                var result = new FunctionPointResult
                {
                    Breakdown = new Dictionary<FunctionType, FunctionTypeBreakdown>()
                };

                // Initialize breakdown for each function type
                foreach (FunctionType type in Enum.GetValues(typeof(FunctionType)))
                {
                    result.Breakdown[type] = new FunctionTypeBreakdown
                    {
                        Type = type,
                        Count = 0,
                        TotalPoints = 0,
                        ComplexityDistribution = new Dictionary<ComplexityLevel, int>
                        {
                            { ComplexityLevel.Low, 0 },
                            { ComplexityLevel.Average, 0 },
                            { ComplexityLevel.High, 0 }
                        }
                    };
                }

                // Calculate unadjusted function points
                decimal unadjustedPoints = 0;

                foreach (var fp in functionPoints)
                {
                    var validation = ValidateFunctionPoint(fp);
                    if (!validation.IsValid)
                    {
                        result.Warnings.AddRange(validation.Errors);
                        continue;
                    }

                    if (validation.Warnings.Any())
                    {
                        result.Warnings.AddRange(validation.Warnings);
                    }

                    var points = fp.CalculatePoints();
                    unadjustedPoints += points;

                    // Update breakdown
                    if (Enum.TryParse<FunctionType>(fp.Type, out var functionType))
                    {
                        var breakdown = result.Breakdown[functionType];
                        breakdown.Count++;
                        breakdown.TotalPoints += points;

                        if (Enum.TryParse<ComplexityLevel>(fp.Complexity, out var complexity))
                        {
                            breakdown.ComplexityDistribution[complexity]++;
                        }
                    }
                }

                result.UnadjustedFunctionPoints = unadjustedPoints;

                // Calculate Value Adjustment Factor (VAF)
                // Using default GSC values for standard system
                var defaultGSC = GetDefaultGeneralSystemCharacteristics();
                result.ValueAdjustmentFactor = CalculateValueAdjustmentFactor(defaultGSC);

                // Calculate adjusted function points
                result.AdjustedFunctionPoints = result.UnadjustedFunctionPoints * result.ValueAdjustmentFactor;

                return result;
            });
        }

        /// <summary>
        /// Calculates the financial analysis based on function points.
        /// </summary>
        public async Task<FinancialAnalysis> CalculateFinancialAnalysisAsync(decimal totalFunctionPoints, ProjectSchedule schedule)
        {
            return await Task.Run(() =>
            {
                var analysis = new FinancialAnalysis
                {
                    TotalFunctionPoints = totalFunctionPoints,
                    RatePerFunctionPoint = RATE_PER_FUNCTION_POINT,
                    AnalysisDate = DateTime.Today
                };

                // Calculate total cost
                analysis.CalculateTotalCost();

                // Create payment milestones based on schedule
                analysis.PaymentMilestones = CreatePaymentMilestones(analysis.TotalProjectCost, schedule);

                // Create cost breakdown by category
                analysis.CostBreakdowns = CreateCostBreakdown(totalFunctionPoints, analysis.TotalProjectCost);

                // Calculate financial metrics
                analysis.Metrics = CalculateFinancialMetrics(analysis);

                return analysis;
            });
        }

        /// <summary>
        /// Validates function points against IFPUG standards.
        /// </summary>
        public ValidationResult ValidateFunctionPoint(FunctionPoint functionPoint)
        {
            var result = new ValidationResult { IsValid = true };

            // Validate required fields
            if (string.IsNullOrEmpty(functionPoint.Name))
            {
                result.Errors.Add("Function point name is required");
                result.IsValid = false;
            }

            if (string.IsNullOrEmpty(functionPoint.Type))
            {
                result.Errors.Add("Function point type is required");
                result.IsValid = false;
            }

            if (string.IsNullOrEmpty(functionPoint.Complexity))
            {
                result.Errors.Add("Function point complexity is required");
                result.IsValid = false;
            }

            // Validate type is valid IFPUG type
            if (!Enum.TryParse<FunctionType>(functionPoint.Type, out _))
            {
                result.Errors.Add($"Invalid function type: {functionPoint.Type}. Must be one of: ILF, EIF, EI, EO, EQ");
                result.IsValid = false;
            }

            // Validate complexity level
            if (!Enum.TryParse<ComplexityLevel>(functionPoint.Complexity, out _))
            {
                result.Errors.Add($"Invalid complexity: {functionPoint.Complexity}. Must be one of: Low, Average, High");
                result.IsValid = false;
            }

            // Add warnings for unusual combinations
            if (functionPoint.Type == "ILF" && functionPoint.Complexity == "Low")
            {
                result.Warnings.Add("ILF with Low complexity is unusual. Verify the DET and RET counts.");
            }

            if (functionPoint.Type == "EIF" && functionPoint.Complexity == "High")
            {
                result.Warnings.Add("EIF with High complexity is unusual. Consider if this should be an ILF.");
            }

            return result;
        }

        /// <summary>
        /// Gets the IFPUG complexity weight for a given function type and complexity.
        /// </summary>
        public int GetIFPUGWeight(FunctionType functionType, ComplexityLevel complexity)
        {
            var key = (functionType, complexity);
            return _ifpugWeights.ContainsKey(key) ? _ifpugWeights[key] : 0;
        }

        /// <summary>
        /// Calculates the Value Adjustment Factor (VAF) based on 14 General System Characteristics.
        /// </summary>
        public decimal CalculateValueAdjustmentFactor(Dictionary<string, int> characteristics)
        {
            // VAF = 0.65 + (0.01 * TDI)
            // TDI (Total Degree of Influence) = Sum of all 14 GSC ratings (0-5 each)

            var totalDegreeOfInfluence = 0;

            foreach (var characteristic in characteristics)
            {
                // Ensure rating is between 0 and 5
                var rating = Math.Min(5, Math.Max(0, characteristic.Value));
                totalDegreeOfInfluence += rating;
            }

            // Calculate VAF
            decimal vaf = 0.65m + (0.01m * totalDegreeOfInfluence);

            // Ensure VAF is within valid range (0.65 to 1.35)
            return Math.Min(1.35m, Math.Max(0.65m, vaf));
        }

        /// <summary>
        /// Get default General System Characteristics for a standard migration project
        /// </summary>
        private Dictionary<string, int> GetDefaultGeneralSystemCharacteristics()
        {
            return new Dictionary<string, int>
            {
                { "Data Communications", 3 },          // Moderate - Web-based system
                { "Distributed Data Processing", 2 },  // Some distributed processing
                { "Performance", 4 },                  // High performance requirements
                { "Heavily Used Configuration", 3 },   // Moderate usage
                { "Transaction Rate", 3 },             // Moderate transaction volume
                { "Online Data Entry", 4 },            // Most data entered online
                { "End-User Efficiency", 4 },          // High emphasis on usability
                { "Online Update", 3 },                // Moderate online updates
                { "Complex Processing", 4 },           // Complex business rules (COBOL migration)
                { "Reusability", 3 },                  // Moderate reusability
                { "Installation Ease", 3 },            // Standard installation
                { "Operational Ease", 3 },             // Standard operations
                { "Multiple Sites", 2 },               // Limited sites
                { "Facilitate Change", 4 }             // High flexibility requirements
            };
        }

        /// <summary>
        /// Create payment milestones based on project schedule
        /// </summary>
        private List<PaymentMilestone> CreatePaymentMilestones(decimal totalCost, ProjectSchedule schedule)
        {
            var milestones = new List<PaymentMilestone>();

            // Standard payment schedule for software projects
            var paymentSchedule = new[]
            {
                new { Name = "Início do Projeto", Percentage = 15m, Week = 0 },
                new { Name = "Análise Concluída", Percentage = 20m, Week = 2 },
                new { Name = "Design Aprovado", Percentage = 15m, Week = 4 },
                new { Name = "Desenvolvimento 50%", Percentage = 20m, Week = 6 },
                new { Name = "Testes Integrados", Percentage = 15m, Week = 10 },
                new { Name = "Entrega Final", Percentage = 15m, Week = 12 }
            };

            foreach (var payment in paymentSchedule)
            {
                var milestone = new PaymentMilestone
                {
                    Id = $"PM-{milestones.Count + 1:D3}",
                    Name = payment.Name,
                    Description = $"Pagamento de {payment.Percentage}% após {payment.Name}",
                    DueDate = schedule.StartDate.AddDays(payment.Week * 7),
                    Percentage = payment.Percentage,
                    Amount = totalCost * (payment.Percentage / 100m),
                    Status = PaymentStatus.Pending
                };

                // Add deliverables based on milestone
                milestone.Deliverables = GetMilestoneDeliverables(payment.Name);

                milestones.Add(milestone);
            }

            return milestones;
        }

        /// <summary>
        /// Get deliverables for a specific milestone
        /// </summary>
        private List<string> GetMilestoneDeliverables(string milestoneName)
        {
            var deliverables = new Dictionary<string, List<string>>
            {
                ["Início do Projeto"] = new List<string>
                {
                    "Plano de Projeto",
                    "Cronograma Detalhado",
                    "Análise de Riscos"
                },
                ["Análise Concluída"] = new List<string>
                {
                    "Documento de Análise COBOL",
                    "Mapeamento de Dados",
                    "Especificação de Requisitos"
                },
                ["Design Aprovado"] = new List<string>
                {
                    "Arquitetura do Sistema",
                    "Modelo de Dados",
                    "Protótipos de Interface"
                },
                ["Desenvolvimento 50%"] = new List<string>
                {
                    "Código Backend Implementado",
                    "APIs Documentadas",
                    "Testes Unitários"
                },
                ["Testes Integrados"] = new List<string>
                {
                    "Relatório de Testes",
                    "Validação COBOL vs .NET",
                    "Manual de Usuário"
                },
                ["Entrega Final"] = new List<string>
                {
                    "Sistema Completo",
                    "Documentação Técnica",
                    "Treinamento Realizado"
                }
            };

            return deliverables.ContainsKey(milestoneName)
                ? deliverables[milestoneName]
                : new List<string>();
        }

        /// <summary>
        /// Create cost breakdown by category
        /// </summary>
        private List<CostBreakdown> CreateCostBreakdown(decimal totalFunctionPoints, decimal totalCost)
        {
            var breakdowns = new List<CostBreakdown>
            {
                new CostBreakdown
                {
                    Category = "Análise e Design",
                    Description = "Análise COBOL e design da nova arquitetura",
                    Percentage = 25m,
                    FunctionPoints = totalFunctionPoints * 0.25m,
                    Cost = totalCost * 0.25m,
                    Items = new List<CostItem>
                    {
                        new CostItem { Name = "Análise COBOL", Quantity = totalFunctionPoints * 0.15m, UnitCost = RATE_PER_FUNCTION_POINT },
                        new CostItem { Name = "Design Arquitetura", Quantity = totalFunctionPoints * 0.10m, UnitCost = RATE_PER_FUNCTION_POINT }
                    }
                },
                new CostBreakdown
                {
                    Category = "Desenvolvimento Backend",
                    Description = "Implementação .NET Core e APIs",
                    Percentage = 35m,
                    FunctionPoints = totalFunctionPoints * 0.35m,
                    Cost = totalCost * 0.35m,
                    Items = new List<CostItem>
                    {
                        new CostItem { Name = "APIs REST", Quantity = totalFunctionPoints * 0.20m, UnitCost = RATE_PER_FUNCTION_POINT },
                        new CostItem { Name = "Lógica de Negócio", Quantity = totalFunctionPoints * 0.15m, UnitCost = RATE_PER_FUNCTION_POINT }
                    }
                },
                new CostBreakdown
                {
                    Category = "Desenvolvimento Frontend",
                    Description = "Interface React e componentes",
                    Percentage = 20m,
                    FunctionPoints = totalFunctionPoints * 0.20m,
                    Cost = totalCost * 0.20m,
                    Items = new List<CostItem>
                    {
                        new CostItem { Name = "Componentes React", Quantity = totalFunctionPoints * 0.12m, UnitCost = RATE_PER_FUNCTION_POINT },
                        new CostItem { Name = "Integração API", Quantity = totalFunctionPoints * 0.08m, UnitCost = RATE_PER_FUNCTION_POINT }
                    }
                },
                new CostBreakdown
                {
                    Category = "Testes e Validação",
                    Description = "Testes e validação COBOL vs .NET",
                    Percentage = 15m,
                    FunctionPoints = totalFunctionPoints * 0.15m,
                    Cost = totalCost * 0.15m,
                    Items = new List<CostItem>
                    {
                        new CostItem { Name = "Testes Automatizados", Quantity = totalFunctionPoints * 0.08m, UnitCost = RATE_PER_FUNCTION_POINT },
                        new CostItem { Name = "Validação Comparativa", Quantity = totalFunctionPoints * 0.07m, UnitCost = RATE_PER_FUNCTION_POINT }
                    }
                },
                new CostBreakdown
                {
                    Category = "Implantação e Treinamento",
                    Description = "Deploy e capacitação da equipe",
                    Percentage = 5m,
                    FunctionPoints = totalFunctionPoints * 0.05m,
                    Cost = totalCost * 0.05m,
                    Items = new List<CostItem>
                    {
                        new CostItem { Name = "Configuração Ambiente", Quantity = totalFunctionPoints * 0.03m, UnitCost = RATE_PER_FUNCTION_POINT },
                        new CostItem { Name = "Treinamento", Quantity = totalFunctionPoints * 0.02m, UnitCost = RATE_PER_FUNCTION_POINT }
                    }
                }
            };

            // Calculate item costs
            foreach (var breakdown in breakdowns)
            {
                foreach (var item in breakdown.Items)
                {
                    item.CalculateCost();
                }
            }

            return breakdowns;
        }

        /// <summary>
        /// Calculate financial metrics for the project
        /// </summary>
        private FinancialMetrics CalculateFinancialMetrics(FinancialAnalysis analysis)
        {
            var metrics = new FinancialMetrics
            {
                TotalBudget = analysis.TotalProjectCost,
                ActualSpent = 0, // Will be updated during project
                CommittedCost = analysis.TotalProjectCost,
                ForecastAtCompletion = analysis.TotalProjectCost
            };

            // Estimate savings from COBOL migration (typical values)
            // Assuming 30% reduction in operational costs
            var currentCOBOLCostPerYear = analysis.TotalProjectCost * 0.8m; // Rough estimate
            metrics.EstimatedAnnualSavings = currentCOBOLCostPerYear * 0.3m;
            metrics.EstimatedMonthlySavings = metrics.EstimatedAnnualSavings / 12;

            // Calculate payback period
            metrics.PaybackPeriodMonths = analysis.CalculatePaybackPeriodMonths(metrics.EstimatedMonthlySavings);

            // Calculate ROI
            metrics.ReturnOnInvestment = analysis.CalculateROI(metrics.EstimatedAnnualSavings);

            // Calculate other metrics
            metrics.CalculateCostVariance();

            return metrics;
        }
    }
}