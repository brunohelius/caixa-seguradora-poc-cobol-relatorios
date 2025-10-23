namespace CaixaSeguradora.Core.Interfaces;

/// <summary>
/// Service interface for external COBOL module integrations.
/// Provides mock implementations of RE0001S, GE0009S, and GE0010S modules.
/// </summary>
/// <remarks>
/// External Modules (from COBOL RG1866B):
/// - RE0001S: Reinsurance calculation module
/// - GE0009S: General calculation/validation module
/// - GE0010S: Additional business logic module
///
/// Migration Strategy:
/// - Phase 1: Mock implementations return dummy data for testing
/// - Phase 2: Real implementations via P/Invoke, REST API, or native C# rewrite
/// - All modules called via COBOL CALL statements with LINKAGE SECTION parameters
/// </remarks>
public interface IExternalModuleService
{
    /// <summary>
    /// Calls RE0001S reinsurance calculation module.
    /// Maps to COBOL: CALL 'RE0001S' USING LKRE-PARM-RE0001S
    /// </summary>
    /// <param name="request">Reinsurance calculation request (maps to LKRE-INPUT-AREA)</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Reinsurance calculation result (maps to LKRE-OUTPUT-AREA)</returns>
    /// <remarks>
    /// RE0001S Module Purpose: Calculate reinsurance premium distribution
    /// Input: Policy number, effective date, premium amount
    /// Output: Retained premium, ceded premium, return code
    /// </remarks>
    Task<ReinsuranceResult> CallRE0001SAsync(
        ReinsuranceRequest request,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Calls GE0009S general calculation module.
    /// Maps to COBOL: CALL 'GE0009S' USING LKGE-PARM-GE0009S
    /// Referenced in COBOL section R1270-00-CALL-GE0009S.
    /// </summary>
    /// <param name="request">General calculation request (maps to LKGE-INPUT-AREA)</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Calculation result (maps to LKGE-OUTPUT-AREA)</returns>
    /// <remarks>
    /// GE0009S Module Purpose: General purpose business calculations
    /// Exact function TBD based on LINKAGE SECTION analysis
    /// Mock implementation returns success with default values
    /// </remarks>
    Task<GeneralCalculationResult> CallGE0009SAsync(
        GeneralCalculationRequest request,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Calls GE0010S additional business logic module.
    /// Maps to COBOL: CALL 'GE0010S' USING LKGE-PARM-GE0010S
    /// Referenced in COBOL section R1280-00-CALL-GE0010S.
    /// </summary>
    /// <param name="request">Business logic request (maps to LKGE-INPUT-AREA)</param>
    /// <param name="cancellationToken">Cancellation token</param>
    /// <returns>Business logic result (maps to LKGE-OUTPUT-AREA)</returns>
    /// <remarks>
    /// GE0010S Module Purpose: Additional business validations/calculations
    /// Exact function TBD based on LINKAGE SECTION analysis
    /// Mock implementation returns success with default values
    /// </remarks>
    Task<BusinessLogicResult> CallGE0010SAsync(
        BusinessLogicRequest request,
        CancellationToken cancellationToken = default);

    /// <summary>
    /// Checks availability of external modules.
    /// Useful for diagnostics and fallback logic.
    /// </summary>
    /// <param name="moduleName">Module name (RE0001S, GE0009S, or GE0010S)</param>
    /// <returns>True if module is available and responding</returns>
    Task<bool> IsModuleAvailableAsync(string moduleName);

    /// <summary>
    /// Gets module version information.
    /// For mock implementations, returns "MOCK-1.0.0".
    /// </summary>
    /// <param name="moduleName">Module name</param>
    /// <returns>Version string</returns>
    string GetModuleVersion(string moduleName);
}

/// <summary>
/// Request for RE0001S reinsurance calculation module.
/// Maps to COBOL LINKAGE SECTION: LKRE-INPUT-AREA.
/// </summary>
public class ReinsuranceRequest
{
    /// <summary>
    /// Policy number (COBOL: LKRE-POLICY-NUMBER PIC X(10)).
    /// </summary>
    public string PolicyNumber { get; set; } = string.Empty;

    /// <summary>
    /// Policy effective date in YYYYMMDD format (COBOL: LKRE-EFFECTIVE-DATE PIC 9(8)).
    /// </summary>
    public string EffectiveDate { get; set; } = string.Empty;

    /// <summary>
    /// Premium amount for reinsurance calculation (COBOL: LKRE-PREMIUM-AMOUNT PIC 9(13)V99).
    /// </summary>
    public decimal PremiumAmount { get; set; }

    /// <summary>
    /// Company code (additional context).
    /// </summary>
    public int CompanyCode { get; set; }

    /// <summary>
    /// Line of business code (affects reinsurance treaty).
    /// </summary>
    public int LineOfBusiness { get; set; }
}

/// <summary>
/// Result from RE0001S reinsurance calculation module.
/// Maps to COBOL LINKAGE SECTION: LKRE-OUTPUT-AREA.
/// </summary>
public class ReinsuranceResult
{
    /// <summary>
    /// Premium retained by ceding company (COBOL: LKRE-RETAINED-PREMIUM PIC 9(13)V99).
    /// </summary>
    public decimal RetainedPremium { get; set; }

    /// <summary>
    /// Premium ceded to reinsurers (COBOL: LKRE-CEDED-PREMIUM PIC 9(13)V99).
    /// </summary>
    public decimal CededPremium { get; set; }

    /// <summary>
    /// Return code (COBOL: LKRE-RETURN-CODE PIC 9(2)).
    /// 00 = Success, 01+ = Error codes.
    /// </summary>
    public int ReturnCode { get; set; }

    /// <summary>
    /// Return message (descriptive error or success message).
    /// </summary>
    public string ReturnMessage { get; set; } = string.Empty;

    /// <summary>
    /// Indicates if call was successful (return code = 00).
    /// </summary>
    public bool IsSuccess => ReturnCode == 0;
}

/// <summary>
/// Request for GE0009S general calculation module.
/// Maps to COBOL LINKAGE SECTION: LKGE-PARM-GE0009S input area.
/// </summary>
/// <remarks>
/// Exact structure TBD - placeholder for known COBOL parameters.
/// </remarks>
public class GeneralCalculationRequest
{
    /// <summary>
    /// Input parameter 1 (generic placeholder).
    /// </summary>
    public string InputParameter1 { get; set; } = string.Empty;

    /// <summary>
    /// Input parameter 2 (generic placeholder).
    /// </summary>
    public decimal InputParameter2 { get; set; }

    /// <summary>
    /// Input parameter 3 (generic placeholder).
    /// </summary>
    public int InputParameter3 { get; set; }

    /// <summary>
    /// Company code context.
    /// </summary>
    public int CompanyCode { get; set; }

    /// <summary>
    /// Policy number context.
    /// </summary>
    public long PolicyNumber { get; set; }
}

/// <summary>
/// Result from GE0009S general calculation module.
/// Maps to COBOL LINKAGE SECTION: LKGE-PARM-GE0009S output area.
/// </summary>
public class GeneralCalculationResult
{
    /// <summary>
    /// Output value 1 (generic placeholder).
    /// </summary>
    public decimal OutputValue1 { get; set; }

    /// <summary>
    /// Output value 2 (generic placeholder).
    /// </summary>
    public string OutputValue2 { get; set; } = string.Empty;

    /// <summary>
    /// Return code (00 = Success).
    /// </summary>
    public int ReturnCode { get; set; }

    /// <summary>
    /// Return message.
    /// </summary>
    public string ReturnMessage { get; set; } = string.Empty;

    /// <summary>
    /// Indicates if call was successful.
    /// </summary>
    public bool IsSuccess => ReturnCode == 0;
}

/// <summary>
/// Request for GE0010S business logic module.
/// Maps to COBOL LINKAGE SECTION: LKGE-PARM-GE0010S input area.
/// </summary>
/// <remarks>
/// Exact structure TBD - placeholder for known COBOL parameters.
/// </remarks>
public class BusinessLogicRequest
{
    /// <summary>
    /// Input parameter 1 (generic placeholder).
    /// </summary>
    public string InputParameter1 { get; set; } = string.Empty;

    /// <summary>
    /// Input parameter 2 (generic placeholder).
    /// </summary>
    public decimal InputParameter2 { get; set; }

    /// <summary>
    /// Input parameter 3 (generic placeholder).
    /// </summary>
    public int InputParameter3 { get; set; }

    /// <summary>
    /// Company code context.
    /// </summary>
    public int CompanyCode { get; set; }

    /// <summary>
    /// Premium reference.
    /// </summary>
    public decimal PremiumAmount { get; set; }
}

/// <summary>
/// Result from GE0010S business logic module.
/// Maps to COBOL LINKAGE SECTION: LKGE-PARM-GE0010S output area.
/// </summary>
public class BusinessLogicResult
{
    /// <summary>
    /// Output value 1 (generic placeholder).
    /// </summary>
    public decimal OutputValue1 { get; set; }

    /// <summary>
    /// Output value 2 (generic placeholder).
    /// </summary>
    public string OutputValue2 { get; set; } = string.Empty;

    /// <summary>
    /// Validation flag (generic placeholder).
    /// </summary>
    public bool ValidationPassed { get; set; }

    /// <summary>
    /// Return code (00 = Success).
    /// </summary>
    public int ReturnCode { get; set; }

    /// <summary>
    /// Return message.
    /// </summary>
    public string ReturnMessage { get; set; } = string.Empty;

    /// <summary>
    /// Indicates if call was successful.
    /// </summary>
    public bool IsSuccess => ReturnCode == 0;
}
