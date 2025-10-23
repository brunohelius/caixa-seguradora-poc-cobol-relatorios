using System.Globalization;

namespace CaixaSeguradora.Infrastructure.Formatters;

/// <summary>
/// Provides COBOL-compatible fixed-width field formatting for report generation.
/// Ensures byte-for-byte compatibility with legacy COBOL output files.
/// </summary>
public static class FixedWidthFormatter
{
    /// <summary>
    /// Formats a numeric value with left-zero-padding to match COBOL PIC 9(n) fields.
    /// </summary>
    /// <param name="value">The numeric value to format</param>
    /// <param name="totalWidth">Total width including decimal places and separators</param>
    /// <param name="decimalPlaces">Number of decimal places (0 for integers)</param>
    /// <param name="includeDecimalPoint">Whether to include decimal point character (default: false for COBOL compatibility)</param>
    /// <returns>Fixed-width numeric string with left-zero-padding</returns>
    /// <example>
    /// FormatNumeric(123, 7, 2, false) returns "0012300" (COBOL PIC 9(5)V99 format)
    /// FormatNumeric(123.45m, 10, 2, true) returns "0000123.45"
    /// </example>
    public static string FormatNumeric(decimal value, int totalWidth, int decimalPlaces = 0, bool includeDecimalPoint = false)
    {
        if (totalWidth <= 0)
        {
            throw new ArgumentException("Total width must be greater than zero", nameof(totalWidth));
        }

        if (decimalPlaces < 0)
        {
            throw new ArgumentException("Decimal places cannot be negative", nameof(decimalPlaces));
        }

        // Handle negative values by converting to absolute value
        // COBOL uses separate sign indicators, not minus signs in numeric fields
        var absoluteValue = Math.Abs(value);

        if (includeDecimalPoint)
        {
            // Format with decimal point (e.g., "0000123.45")
            var formatted = absoluteValue.ToString($"F{decimalPlaces}", CultureInfo.InvariantCulture);

            // Pad with zeros on the left
            return formatted.PadLeft(totalWidth, '0');
        }
        else
        {
            // COBOL style: implied decimal point (e.g., "0012345" for 123.45 with V99)
            // Multiply by 10^decimalPlaces to shift decimal
            var multiplier = (decimal)Math.Pow(10, decimalPlaces);
            var shiftedValue = Math.Round(absoluteValue * multiplier, 0, MidpointRounding.AwayFromZero);

            // Convert to integer string and pad
            var intValue = (long)shiftedValue;
            return intValue.ToString(CultureInfo.InvariantCulture).PadLeft(totalWidth, '0');
        }
    }

    /// <summary>
    /// Formats an integer value with left-zero-padding to match COBOL PIC 9(n) fields.
    /// </summary>
    /// <param name="value">The integer value to format</param>
    /// <param name="totalWidth">Total width of the field</param>
    /// <returns>Fixed-width integer string with left-zero-padding</returns>
    /// <example>
    /// FormatNumeric(42, 5) returns "00042"
    /// </example>
    public static string FormatNumeric(int value, int totalWidth)
    {
        if (totalWidth <= 0)
        {
            throw new ArgumentException("Total width must be greater than zero", nameof(totalWidth));
        }

        var absoluteValue = Math.Abs(value);
        return absoluteValue.ToString(CultureInfo.InvariantCulture).PadLeft(totalWidth, '0');
    }

    /// <summary>
    /// Formats an alphanumeric string with right-space-padding to match COBOL PIC X(n) fields.
    /// </summary>
    /// <param name="value">The string value to format</param>
    /// <param name="totalWidth">Total width of the field</param>
    /// <param name="truncate">Whether to truncate if value exceeds width (default: true)</param>
    /// <returns>Fixed-width alphanumeric string with right-space-padding</returns>
    /// <example>
    /// FormatAlphanumeric("ABC", 10) returns "ABC       "
    /// FormatAlphanumeric("ABCDEFGHIJK", 5, true) returns "ABCDE"
    /// </example>
    public static string FormatAlphanumeric(string? value, int totalWidth, bool truncate = true)
    {
        if (totalWidth <= 0)
        {
            throw new ArgumentException("Total width must be greater than zero", nameof(totalWidth));
        }

        // Handle null or empty strings
        if (string.IsNullOrEmpty(value))
        {
            return new string(' ', totalWidth);
        }

        // Truncate if needed and allowed
        if (value.Length > totalWidth)
        {
            if (truncate)
            {
                return value.Substring(0, totalWidth);
            }
            else
            {
                throw new ArgumentException($"Value length ({value.Length}) exceeds total width ({totalWidth})", nameof(value));
            }
        }

        // Pad with spaces on the right
        return value.PadRight(totalWidth, ' ');
    }

    /// <summary>
    /// Formats a DateTime value to COBOL date format strings.
    /// Supports YYYYMMDD, DDMMYYYY, and YYYY-MM-DD formats.
    /// </summary>
    /// <param name="date">The DateTime value to format</param>
    /// <param name="format">Date format type (default: YYYYMMDD)</param>
    /// <returns>Fixed-width date string</returns>
    /// <example>
    /// FormatDate(new DateTime(2025, 10, 22), DateFormat.YYYYMMDD) returns "20251022"
    /// FormatDate(new DateTime(2025, 10, 22), DateFormat.DDMMYYYY) returns "22102025"
    /// FormatDate(new DateTime(2025, 10, 22), DateFormat.ISO8601) returns "2025-10-22"
    /// </example>
    public static string FormatDate(DateTime date, DateFormat format = DateFormat.YYYYMMDD)
    {
        return format switch
        {
            DateFormat.YYYYMMDD => date.ToString("yyyyMMdd", CultureInfo.InvariantCulture),
            DateFormat.DDMMYYYY => date.ToString("ddMMyyyy", CultureInfo.InvariantCulture),
            DateFormat.ISO8601 => date.ToString("yyyy-MM-dd", CultureInfo.InvariantCulture),
            DateFormat.MMDDYYYY => date.ToString("MMddyyyy", CultureInfo.InvariantCulture),
            _ => throw new ArgumentException($"Unsupported date format: {format}", nameof(format))
        };
    }

    /// <summary>
    /// Formats a DateTime value to COBOL date format string using custom pattern.
    /// </summary>
    /// <param name="date">The DateTime value to format</param>
    /// <param name="pattern">.NET date format pattern (e.g., "yyyyMMdd", "dd/MM/yyyy")</param>
    /// <returns>Formatted date string</returns>
    public static string FormatDate(DateTime date, string pattern)
    {
        if (string.IsNullOrWhiteSpace(pattern))
        {
            throw new ArgumentException("Pattern cannot be null or empty", nameof(pattern));
        }

        return date.ToString(pattern, CultureInfo.InvariantCulture);
    }

    /// <summary>
    /// Formats a nullable DateTime value, returning spaces if null.
    /// </summary>
    /// <param name="date">The nullable DateTime value</param>
    /// <param name="format">Date format type</param>
    /// <param name="totalWidth">Width to use for null values (default: 10 for YYYY-MM-DD)</param>
    /// <returns>Formatted date string or spaces if null</returns>
    public static string FormatDate(DateTime? date, DateFormat format = DateFormat.YYYYMMDD, int totalWidth = 10)
    {
        if (!date.HasValue)
        {
            return new string(' ', totalWidth);
        }

        return FormatDate(date.Value, format);
    }

    /// <summary>
    /// Formats a boolean value to COBOL-style single character flag.
    /// </summary>
    /// <param name="value">The boolean value</param>
    /// <param name="trueChar">Character to use for true (default: 'S' for Sim/Yes)</param>
    /// <param name="falseChar">Character to use for false (default: 'N' for Não/No)</param>
    /// <returns>Single character representing the boolean value</returns>
    public static string FormatBoolean(bool value, char trueChar = 'S', char falseChar = 'N')
    {
        return value ? trueChar.ToString() : falseChar.ToString();
    }

    /// <summary>
    /// Formats a nullable boolean value to COBOL-style single character flag.
    /// </summary>
    /// <param name="value">The nullable boolean value</param>
    /// <param name="trueChar">Character to use for true (default: 'S')</param>
    /// <param name="falseChar">Character to use for false (default: 'N')</param>
    /// <param name="nullChar">Character to use for null (default: space)</param>
    /// <returns>Single character representing the boolean value</returns>
    public static string FormatBoolean(bool? value, char trueChar = 'S', char falseChar = 'N', char nullChar = ' ')
    {
        if (!value.HasValue)
        {
            return nullChar.ToString();
        }

        return FormatBoolean(value.Value, trueChar, falseChar);
    }

    /// <summary>
    /// Creates a fixed-width record by concatenating multiple formatted fields.
    /// Useful for building complete COBOL-style fixed-width file records.
    /// </summary>
    /// <param name="fields">Array of formatted field strings</param>
    /// <returns>Concatenated fixed-width record</returns>
    public static string BuildRecord(params string[] fields)
    {
        if (fields == null || fields.Length == 0)
        {
            return string.Empty;
        }

        return string.Concat(fields);
    }

    /// <summary>
    /// Validates that a formatted string matches expected width.
    /// Throws exception if width mismatch occurs.
    /// </summary>
    /// <param name="formattedValue">The formatted string to validate</param>
    /// <param name="expectedWidth">Expected width</param>
    /// <param name="fieldName">Field name for error message</param>
    public static void ValidateWidth(string formattedValue, int expectedWidth, string fieldName = "field")
    {
        if (formattedValue == null)
        {
            throw new ArgumentNullException(nameof(formattedValue));
        }

        if (formattedValue.Length != expectedWidth)
        {
            throw new InvalidOperationException(
                $"{fieldName} width mismatch: expected {expectedWidth}, got {formattedValue.Length}. Value: '{formattedValue}'");
        }
    }
}

/// <summary>
/// Supported date format types for COBOL compatibility.
/// </summary>
public enum DateFormat
{
    /// <summary>YYYYMMDD format (8 digits, no separators)</summary>
    YYYYMMDD,

    /// <summary>DDMMYYYY format (8 digits, no separators)</summary>
    DDMMYYYY,

    /// <summary>MMDDYYYY format (8 digits, no separators)</summary>
    MMDDYYYY,

    /// <summary>YYYY-MM-DD format (ISO 8601 with hyphens, 10 characters)</summary>
    ISO8601
}
