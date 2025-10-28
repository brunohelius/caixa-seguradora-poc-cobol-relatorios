using System;
using System.Collections.Generic;
using System.Linq;

namespace PdfGenerator.Models
{
    /// <summary>
    /// Represents the complete financial analysis for the migration project.
    /// Based on function points and project schedule.
    /// </summary>
    public class FinancialAnalysis
    {
        public decimal TotalFunctionPoints { get; set; }
        public decimal RatePerFunctionPoint { get; set; } = 750m; // R$ 750 per FP as per standard
        public decimal TotalProjectCost { get; set; }
        public string Currency { get; set; } = "BRL";
        public List<PaymentMilestone> PaymentMilestones { get; set; } = new();
        public List<CostBreakdown> CostBreakdowns { get; set; } = new();
        public FinancialMetrics Metrics { get; set; } = new();
        public DateTime AnalysisDate { get; set; } = DateTime.Today;
        public string AnalysisVersion { get; set; } = "1.0.0";

        /// <summary>
        /// Calculate total project cost based on function points
        /// </summary>
        public void CalculateTotalCost()
        {
            TotalProjectCost = TotalFunctionPoints * RatePerFunctionPoint;
        }

        /// <summary>
        /// Get total paid amount to date
        /// </summary>
        public decimal GetPaidAmount()
        {
            return PaymentMilestones
                .Where(pm => pm.IsPaid)
                .Sum(pm => pm.Amount);
        }

        /// <summary>
        /// Get remaining amount to be paid
        /// </summary>
        public decimal GetRemainingAmount()
        {
            return TotalProjectCost - GetPaidAmount();
        }

        /// <summary>
        /// Get next payment milestone
        /// </summary>
        public PaymentMilestone GetNextPaymentMilestone()
        {
            return PaymentMilestones
                .Where(pm => !pm.IsPaid)
                .OrderBy(pm => pm.DueDate)
                .FirstOrDefault();
        }

        /// <summary>
        /// Calculate ROI (Return on Investment)
        /// </summary>
        public decimal CalculateROI(decimal estimatedAnnualSavings)
        {
            if (TotalProjectCost == 0) return 0;
            return ((estimatedAnnualSavings - TotalProjectCost) / TotalProjectCost) * 100;
        }

        /// <summary>
        /// Calculate payback period in months
        /// </summary>
        public int CalculatePaybackPeriodMonths(decimal estimatedMonthlySavings)
        {
            if (estimatedMonthlySavings <= 0) return 0;
            return (int)Math.Ceiling(TotalProjectCost / estimatedMonthlySavings);
        }
    }

    /// <summary>
    /// Represents a payment milestone in the project
    /// </summary>
    public class PaymentMilestone
    {
        public string Id { get; set; } = string.Empty;
        public string Name { get; set; } = string.Empty;
        public string Description { get; set; } = string.Empty;
        public DateTime DueDate { get; set; }
        public decimal Percentage { get; set; }
        public decimal Amount { get; set; }
        public bool IsPaid { get; set; }
        public DateTime? PaymentDate { get; set; }
        public string InvoiceNumber { get; set; } = string.Empty;
        public List<string> Deliverables { get; set; } = new();
        public PaymentStatus Status { get; set; } = PaymentStatus.Pending;

        /// <summary>
        /// Check if payment is overdue
        /// </summary>
        public bool IsOverdue()
        {
            return !IsPaid && DateTime.Today > DueDate;
        }

        /// <summary>
        /// Get days until payment (negative if overdue)
        /// </summary>
        public int GetDaysUntilPayment()
        {
            return (int)(DueDate - DateTime.Today).TotalDays;
        }
    }

    /// <summary>
    /// Cost breakdown by category
    /// </summary>
    public class CostBreakdown
    {
        public string Category { get; set; } = string.Empty;
        public string Description { get; set; } = string.Empty;
        public decimal FunctionPoints { get; set; }
        public decimal Cost { get; set; }
        public decimal Percentage { get; set; }
        public List<CostItem> Items { get; set; } = new();

        /// <summary>
        /// Calculate total cost for this category
        /// </summary>
        public decimal GetTotalCost()
        {
            return Items?.Sum(i => i.Cost) ?? Cost;
        }
    }

    /// <summary>
    /// Individual cost item
    /// </summary>
    public class CostItem
    {
        public string Name { get; set; } = string.Empty;
        public string Description { get; set; } = string.Empty;
        public decimal Quantity { get; set; }
        public decimal UnitCost { get; set; }
        public decimal Cost { get; set; }
        public string Unit { get; set; } = "FP"; // Function Points by default

        /// <summary>
        /// Calculate cost based on quantity and unit cost
        /// </summary>
        public void CalculateCost()
        {
            Cost = Quantity * UnitCost;
        }
    }

    /// <summary>
    /// Financial metrics for the project
    /// </summary>
    public class FinancialMetrics
    {
        public decimal TotalBudget { get; set; }
        public decimal ActualSpent { get; set; }
        public decimal CommittedCost { get; set; }
        public decimal ForecastAtCompletion { get; set; }
        public decimal CostVariance { get; set; }
        public decimal CostPerformanceIndex { get; set; }
        public decimal EstimatedAnnualSavings { get; set; }
        public decimal EstimatedMonthlySavings { get; set; }
        public int PaybackPeriodMonths { get; set; }
        public decimal ReturnOnInvestment { get; set; }
        public decimal NetPresentValue { get; set; }
        public decimal InternalRateOfReturn { get; set; }

        /// <summary>
        /// Calculate cost variance (Budget - Actual)
        /// </summary>
        public void CalculateCostVariance()
        {
            CostVariance = TotalBudget - ActualSpent;
        }

        /// <summary>
        /// Calculate Cost Performance Index (CPI)
        /// </summary>
        public void CalculateCPI(decimal earnedValue)
        {
            if (ActualSpent > 0)
            {
                CostPerformanceIndex = earnedValue / ActualSpent;
            }
        }

        /// <summary>
        /// Check if project is over budget
        /// </summary>
        public bool IsOverBudget()
        {
            return ActualSpent > TotalBudget;
        }

        /// <summary>
        /// Calculate budget utilization percentage
        /// </summary>
        public decimal GetBudgetUtilizationPercentage()
        {
            if (TotalBudget == 0) return 0;
            return (ActualSpent / TotalBudget) * 100;
        }
    }

    /// <summary>
    /// Payment status
    /// </summary>
    public enum PaymentStatus
    {
        Pending,
        InvoiceSent,
        Processing,
        Paid,
        Overdue,
        Disputed,
        Cancelled
    }
}