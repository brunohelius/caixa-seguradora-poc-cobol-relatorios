using System.Globalization;

namespace CaixaSeguradora.Core.Utilities;

/// <summary>
/// Provides COBOL-compatible mathematical operations for financial calculations.
/// Ensures arithmetic precision and rounding behavior matches COBOL ROUNDED clause semantics.
/// </summary>
/// <remarks>
/// COBOL uses different rounding modes than standard .NET:
/// - ROUNDED: Equivalent to MidpointRounding.AwayFromZero (0.5 rounds away from zero)
/// - ROUNDED MODE IS NEAREST-EVEN: Equivalent to MidpointRounding.ToEven (banker's rounding)
/// - TRUNCATE: Simple truncation without rounding
///
/// All financial calculations must use decimal type (NOT float/double) to maintain precision.
/// </remarks>
public static class CobolMath
{
    /// <summary>
    /// Rounds a decimal value away from zero when exactly halfway between two values.
    /// Matches COBOL ROUNDED clause behavior (default COBOL rounding).
    /// </summary>
    /// <param name="value">The value to round</param>
    /// <param name="decimalPlaces">Number of decimal places to round to</param>
    /// <returns>Rounded decimal value</returns>
    /// <example>
    /// RoundHalfUp(2.5m, 0) returns 3
    /// RoundHalfUp(2.25m, 1) returns 2.3
    /// RoundHalfUp(-2.5m, 0) returns -3
    /// </example>
    public static decimal RoundHalfUp(decimal value, int decimalPlaces)
    {
        if (decimalPlaces < 0)
            throw new ArgumentException("Decimal places cannot be negative", nameof(decimalPlaces));

        return Math.Round(value, decimalPlaces, MidpointRounding.AwayFromZero);
    }

    /// <summary>
    /// Rounds a decimal value to the nearest even number when exactly halfway between two values.
    /// Matches COBOL ROUNDED MODE IS NEAREST-EVEN behavior (banker's rounding).
    /// </summary>
    /// <param name="value">The value to round</param>
    /// <param name="decimalPlaces">Number of decimal places to round to</param>
    /// <returns>Rounded decimal value</returns>
    /// <example>
    /// RoundHalfEven(2.5m, 0) returns 2 (rounds to even)
    /// RoundHalfEven(3.5m, 0) returns 4 (rounds to even)
    /// RoundHalfEven(2.25m, 1) returns 2.2
    /// </example>
    public static decimal RoundHalfEven(decimal value, int decimalPlaces)
    {
        if (decimalPlaces < 0)
            throw new ArgumentException("Decimal places cannot be negative", nameof(decimalPlaces));

        return Math.Round(value, decimalPlaces, MidpointRounding.ToEven);
    }

    /// <summary>
    /// Truncates a decimal value to specified decimal places without rounding.
    /// Matches COBOL behavior when ROUNDED clause is not specified.
    /// </summary>
    /// <param name="value">The value to truncate</param>
    /// <param name="decimalPlaces">Number of decimal places to keep</param>
    /// <returns>Truncated decimal value</returns>
    /// <example>
    /// TruncateDecimal(2.999m, 2) returns 2.99
    /// TruncateDecimal(2.111m, 0) returns 2
    /// TruncateDecimal(-2.999m, 2) returns -2.99
    /// </example>
    public static decimal TruncateDecimal(decimal value, int decimalPlaces)
    {
        if (decimalPlaces < 0)
            throw new ArgumentException("Decimal places cannot be negative", nameof(decimalPlaces));

        var multiplier = (decimal)Math.Pow(10, decimalPlaces);
        return Math.Truncate(value * multiplier) / multiplier;
    }

    /// <summary>
    /// Adds two decimal values with optional rounding to specified precision.
    /// Use for COBOL ADD statements with ROUNDED clause.
    /// </summary>
    /// <param name="a">First operand</param>
    /// <param name="b">Second operand</param>
    /// <param name="decimalPlaces">Number of decimal places for result (optional)</param>
    /// <param name="roundMode">Rounding mode to apply</param>
    /// <returns>Sum with specified precision</returns>
    public static decimal Add(decimal a, decimal b, int? decimalPlaces = null, CobolRoundMode roundMode = CobolRoundMode.HalfUp)
    {
        var result = a + b;

        if (decimalPlaces.HasValue)
        {
            return ApplyRounding(result, decimalPlaces.Value, roundMode);
        }

        return result;
    }

    /// <summary>
    /// Subtracts two decimal values with optional rounding to specified precision.
    /// Use for COBOL SUBTRACT statements with ROUNDED clause.
    /// </summary>
    /// <param name="a">Minuend</param>
    /// <param name="b">Subtrahend</param>
    /// <param name="decimalPlaces">Number of decimal places for result (optional)</param>
    /// <param name="roundMode">Rounding mode to apply</param>
    /// <returns>Difference with specified precision</returns>
    public static decimal Subtract(decimal a, decimal b, int? decimalPlaces = null, CobolRoundMode roundMode = CobolRoundMode.HalfUp)
    {
        var result = a - b;

        if (decimalPlaces.HasValue)
        {
            return ApplyRounding(result, decimalPlaces.Value, roundMode);
        }

        return result;
    }

    /// <summary>
    /// Multiplies two decimal values with optional rounding to specified precision.
    /// Use for COBOL MULTIPLY statements with ROUNDED clause.
    /// </summary>
    /// <param name="a">Multiplicand</param>
    /// <param name="b">Multiplier</param>
    /// <param name="decimalPlaces">Number of decimal places for result (optional)</param>
    /// <param name="roundMode">Rounding mode to apply</param>
    /// <returns>Product with specified precision</returns>
    public static decimal Multiply(decimal a, decimal b, int? decimalPlaces = null, CobolRoundMode roundMode = CobolRoundMode.HalfUp)
    {
        var result = a * b;

        if (decimalPlaces.HasValue)
        {
            return ApplyRounding(result, decimalPlaces.Value, roundMode);
        }

        return result;
    }

    /// <summary>
    /// Divides two decimal values with optional rounding to specified precision.
    /// Use for COBOL DIVIDE statements with ROUNDED clause.
    /// </summary>
    /// <param name="dividend">Dividend</param>
    /// <param name="divisor">Divisor</param>
    /// <param name="decimalPlaces">Number of decimal places for result (optional)</param>
    /// <param name="roundMode">Rounding mode to apply</param>
    /// <returns>Quotient with specified precision</returns>
    /// <exception cref="DivideByZeroException">Thrown when divisor is zero</exception>
    public static decimal Divide(decimal dividend, decimal divisor, int? decimalPlaces = null, CobolRoundMode roundMode = CobolRoundMode.HalfUp)
    {
        if (divisor == 0)
            throw new DivideByZeroException("Division by zero is not allowed");

        var result = dividend / divisor;

        if (decimalPlaces.HasValue)
        {
            return ApplyRounding(result, decimalPlaces.Value, roundMode);
        }

        return result;
    }

    /// <summary>
    /// Computes percentage of a value with COBOL-compatible rounding.
    /// Common operation in premium calculations.
    /// </summary>
    /// <param name="value">Base value</param>
    /// <param name="percentage">Percentage (0-100 or 0-1 depending on isDecimal)</param>
    /// <param name="isDecimal">True if percentage is 0-1 (e.g., 0.15 for 15%), false if 0-100</param>
    /// <param name="decimalPlaces">Decimal places for result</param>
    /// <param name="roundMode">Rounding mode</param>
    /// <returns>Calculated percentage value</returns>
    /// <example>
    /// ComputePercentage(1000m, 15m, false, 2) returns 150.00 (15% of 1000)
    /// ComputePercentage(1000m, 0.15m, true, 2) returns 150.00
    /// </example>
    public static decimal ComputePercentage(decimal value, decimal percentage, bool isDecimal = false, int decimalPlaces = 2, CobolRoundMode roundMode = CobolRoundMode.HalfUp)
    {
        var multiplier = isDecimal ? percentage : percentage / 100m;
        var result = value * multiplier;

        return ApplyRounding(result, decimalPlaces, roundMode);
    }

    /// <summary>
    /// Validates that a decimal value matches expected COBOL precision (total digits and decimal places).
    /// Throws exception if validation fails.
    /// </summary>
    /// <param name="value">Value to validate</param>
    /// <param name="totalDigits">Total number of digits (PIC 9(n)V9(m) where n+m = totalDigits)</param>
    /// <param name="decimalPlaces">Number of decimal places (m in above)</param>
    /// <param name="fieldName">Field name for error message</param>
    /// <exception cref="ArgumentException">Thrown when value exceeds precision limits</exception>
    public static void ValidatePrecision(decimal value, int totalDigits, int decimalPlaces, string fieldName = "value")
    {
        var absoluteValue = Math.Abs(value);
        var maxValue = (decimal)Math.Pow(10, totalDigits - decimalPlaces) - (decimal)Math.Pow(10, -decimalPlaces);

        if (absoluteValue > maxValue)
        {
            throw new ArgumentException(
                $"{fieldName} exceeds COBOL precision PIC 9({totalDigits - decimalPlaces})V9({decimalPlaces}). " +
                $"Maximum allowed: {maxValue}, got: {absoluteValue}",
                nameof(value));
        }
    }

    /// <summary>
    /// Compares two decimal values for equality within a specified tolerance.
    /// Useful for comparing calculated vs. expected results in tests.
    /// </summary>
    /// <param name="a">First value</param>
    /// <param name="b">Second value</param>
    /// <param name="tolerance">Maximum allowed difference (default: 0.01 for financial calculations)</param>
    /// <returns>True if values are equal within tolerance</returns>
    public static bool AreEqual(decimal a, decimal b, decimal tolerance = 0.01m)
    {
        return Math.Abs(a - b) <= tolerance;
    }

    /// <summary>
    /// Applies the specified rounding mode to a decimal value.
    /// </summary>
    /// <param name="value">Value to round</param>
    /// <param name="decimalPlaces">Number of decimal places</param>
    /// <param name="roundMode">Rounding mode</param>
    /// <returns>Rounded value</returns>
    private static decimal ApplyRounding(decimal value, int decimalPlaces, CobolRoundMode roundMode)
    {
        return roundMode switch
        {
            CobolRoundMode.HalfUp => RoundHalfUp(value, decimalPlaces),
            CobolRoundMode.HalfEven => RoundHalfEven(value, decimalPlaces),
            CobolRoundMode.Truncate => TruncateDecimal(value, decimalPlaces),
            _ => throw new ArgumentException($"Unknown rounding mode: {roundMode}", nameof(roundMode))
        };
    }

    /// <summary>
    /// Converts a decimal value to integer representation matching COBOL COMP-3 (packed decimal).
    /// Used for binary comparison and storage.
    /// </summary>
    /// <param name="value">Decimal value</param>
    /// <param name="decimalPlaces">Number of implied decimal places</param>
    /// <returns>Integer representation (value * 10^decimalPlaces)</returns>
    /// <example>
    /// ToPackedDecimalInt(123.45m, 2) returns 12345
    /// </example>
    public static long ToPackedDecimalInt(decimal value, int decimalPlaces)
    {
        var multiplier = (decimal)Math.Pow(10, decimalPlaces);
        return (long)Math.Round(value * multiplier, 0, MidpointRounding.AwayFromZero);
    }

    /// <summary>
    /// Converts an integer representation back to decimal value from COBOL COMP-3 format.
    /// </summary>
    /// <param name="packedValue">Integer representation</param>
    /// <param name="decimalPlaces">Number of implied decimal places</param>
    /// <returns>Decimal value</returns>
    /// <example>
    /// FromPackedDecimalInt(12345, 2) returns 123.45m
    /// </example>
    public static decimal FromPackedDecimalInt(long packedValue, int decimalPlaces)
    {
        var divisor = (decimal)Math.Pow(10, decimalPlaces);
        return packedValue / divisor;
    }
}

/// <summary>
/// COBOL-compatible rounding modes.
/// </summary>
public enum CobolRoundMode
{
    /// <summary>
    /// Round half away from zero (COBOL ROUNDED default).
    /// 2.5 -> 3, -2.5 -> -3
    /// </summary>
    HalfUp,

    /// <summary>
    /// Round half to nearest even number (COBOL ROUNDED MODE IS NEAREST-EVEN).
    /// 2.5 -> 2, 3.5 -> 4
    /// </summary>
    HalfEven,

    /// <summary>
    /// Truncate without rounding (no ROUNDED clause in COBOL).
    /// 2.9 -> 2, -2.9 -> -2
    /// </summary>
    Truncate
}
