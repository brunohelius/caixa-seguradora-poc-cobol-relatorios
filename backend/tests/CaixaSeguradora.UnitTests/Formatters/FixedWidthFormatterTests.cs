using Xunit;
using FluentAssertions;
using CaixaSeguradora.Infrastructure.Formatters;

namespace CaixaSeguradora.UnitTests.Formatters;

/// <summary>
/// Comprehensive unit tests for FixedWidthFormatter to ensure 100% coverage
/// and COBOL byte-for-byte compatibility.
/// </summary>
public class FixedWidthFormatterTests
{
    #region FormatNumeric(decimal) Tests

    [Theory]
    [InlineData(12345.67, 15, 2, false, "000000001234567")] // Basic: left-pad zeros, implied decimal
    [InlineData(12345.67, 15, 2, true, "000000012345.67")] // With decimal point
    [InlineData(123.45, 10, 2, false, "0000012345")] // Shorter value
    [InlineData(0, 10, 2, false, "0000000000")] // Zero value
    [InlineData(0.01, 5, 2, false, "00001")] // Small value
    [InlineData(999.99, 7, 2, false, "0099999")] // Max value for width
    [InlineData(123, 7, 0, false, "0000123")] // Integer (no decimals)
    [InlineData(123.456, 8, 2, false, "00012346")] // Rounding: 123.456 → 123.46
    [InlineData(123.454, 8, 2, false, "00012345")] // Rounding: 123.454 → 123.45
    [InlineData(123.455, 8, 2, false, "00012346")] // Rounding: 123.455 → 123.46 (AwayFromZero)
    [InlineData(0.125, 10, 2, false, "0000000013")] // Banker's rounding: 0.125 → 0.13
    [InlineData(0.135, 10, 2, false, "0000000014")] // Banker's rounding: 0.135 → 0.14
    [InlineData(99999.99, 9, 2, false, "009999999")] // Large value
    [InlineData(1.1, 5, 2, false, "00110")] // Simple decimal
    [InlineData(0.99, 5, 2, false, "00099")] // Less than 1
    public void FormatNumeric_Decimal_VariousInputs_ReturnsCorrectFormat(
        decimal value, int totalWidth, int decimalPlaces, bool includeDecimalPoint, string expected)
    {
        // Act
        var result = FixedWidthFormatter.FormatNumeric(value, totalWidth, decimalPlaces, includeDecimalPoint);

        // Assert
        result.Should().Be(expected);
        result.Length.Should().Be(totalWidth);
    }

    [Theory]
    [InlineData(-12345.67, 15, 2, false, "000000001234567")] // Negative converted to absolute
    [InlineData(-123.45, 10, 2, false, "0000012345")] // Negative smaller value
    [InlineData(-0.01, 5, 2, false, "00001")] // Negative small value
    [InlineData(-999.99, 7, 2, false, "0099999")] // Negative max value
    public void FormatNumeric_Decimal_NegativeValues_ReturnsAbsoluteValue(
        decimal value, int totalWidth, int decimalPlaces, bool includeDecimalPoint, string expected)
    {
        // Act
        var result = FixedWidthFormatter.FormatNumeric(value, totalWidth, decimalPlaces, includeDecimalPoint);

        // Assert
        result.Should().Be(expected);
        result.Length.Should().Be(totalWidth);
    }

    [Theory]
    [InlineData(0, 2)]
    [InlineData(-1, 2)]
    [InlineData(-10, 2)]
    public void FormatNumeric_Decimal_InvalidTotalWidth_ThrowsArgumentException(int totalWidth, int decimalPlaces)
    {
        // Act
        Action act = () => FixedWidthFormatter.FormatNumeric(123.45m, totalWidth, decimalPlaces);

        // Assert
        act.Should().Throw<ArgumentException>()
            .WithMessage("*Total width must be greater than zero*")
            .And.ParamName.Should().Be("totalWidth");
    }

    [Theory]
    [InlineData(10, -1)]
    [InlineData(10, -5)]
    public void FormatNumeric_Decimal_NegativeDecimalPlaces_ThrowsArgumentException(int totalWidth, int decimalPlaces)
    {
        // Act
        Action act = () => FixedWidthFormatter.FormatNumeric(123.45m, totalWidth, decimalPlaces);

        // Assert
        act.Should().Throw<ArgumentException>()
            .WithMessage("*Decimal places cannot be negative*")
            .And.ParamName.Should().Be("decimalPlaces");
    }

    [Theory]
    [InlineData(123.456789d, 15, 5, false, "000000012345679")] // 5 decimal places
    [InlineData(1.23456789d, 12, 6, false, "000001234568")] // 6 decimal places
    [MemberData(nameof(GetVerySmallDecimalTestData))] // Very small value with high precision
    public void FormatNumeric_Decimal_VariousDecimalPlaces_ReturnsCorrectFormat(
        decimal value, int totalWidth, int decimalPlaces, bool includeDecimalPoint, string expected)
    {
        // Act
        var result = FixedWidthFormatter.FormatNumeric(value, totalWidth, decimalPlaces, includeDecimalPoint);

        // Assert
        result.Should().Be(expected);
        result.Length.Should().Be(totalWidth);
    }

    /// <summary>
    /// Provides test data for very small decimal values with high precision.
    /// Used because decimal literals (0.000001m) cannot be used in InlineData attributes.
    /// </summary>
    public static IEnumerable<object[]> GetVerySmallDecimalTestData()
    {
        yield return new object[] { 0.000001m, 10, 6, false, "0000000001" };
    }

    [Fact]
    public void FormatNumeric_Decimal_MaxDecimalValue_HandlesCorrectly()
    {
        // Arrange
        var maxDecimal = 999999999999.99m;

        // Act
        var result = FixedWidthFormatter.FormatNumeric(maxDecimal, 17, 2);

        // Assert
        result.Should().Be("00099999999999999");
        result.Length.Should().Be(17);
    }

    [Fact]
    public void FormatNumeric_Decimal_ZeroWithDecimals_ReturnsAllZeros()
    {
        // Act
        var result = FixedWidthFormatter.FormatNumeric(0m, 10, 2);

        // Assert
        result.Should().Be("0000000000");
        result.Length.Should().Be(10);
    }

    #endregion

    #region FormatNumeric(int) Tests

    [Theory]
    [InlineData(42, 5, "00042")]
    [InlineData(0, 5, "00000")]
    [InlineData(123, 10, "0000000123")]
    [InlineData(1, 3, "001")]
    [InlineData(9999, 8, "00009999")]
    [InlineData(12345, 5, "12345")] // Exact width
    public void FormatNumeric_Integer_VariousInputs_ReturnsCorrectFormat(int value, int totalWidth, string expected)
    {
        // Act
        var result = FixedWidthFormatter.FormatNumeric(value, totalWidth);

        // Assert
        result.Should().Be(expected);
        result.Length.Should().Be(totalWidth);
    }

    [Theory]
    [InlineData(-42, 5, "00042")]
    [InlineData(-123, 10, "0000000123")]
    [InlineData(-9999, 8, "00009999")]
    public void FormatNumeric_Integer_NegativeValues_ReturnsAbsoluteValue(int value, int totalWidth, string expected)
    {
        // Act
        var result = FixedWidthFormatter.FormatNumeric(value, totalWidth);

        // Assert
        result.Should().Be(expected);
        result.Length.Should().Be(totalWidth);
    }

    [Theory]
    [InlineData(0)]
    [InlineData(-1)]
    [InlineData(-10)]
    public void FormatNumeric_Integer_InvalidTotalWidth_ThrowsArgumentException(int totalWidth)
    {
        // Act
        Action act = () => FixedWidthFormatter.FormatNumeric(123, totalWidth);

        // Assert
        act.Should().Throw<ArgumentException>()
            .WithMessage("*Total width must be greater than zero*")
            .And.ParamName.Should().Be("totalWidth");
    }

    #endregion

    #region FormatAlphanumeric Tests

    [Theory]
    [InlineData("ABC", 10, true, "ABC       ")] // Right-pad with spaces
    [InlineData("TEST", 8, true, "TEST    ")] // 4 spaces padding
    [InlineData("HELLO", 5, true, "HELLO")] // Exact width
    [InlineData("X", 1, true, "X")] // Single character
    [InlineData("A", 20, true, "A                   ")] // Large padding
    public void FormatAlphanumeric_ValidInputs_ReturnsCorrectFormat(
        string value, int totalWidth, bool truncate, string expected)
    {
        // Act
        var result = FixedWidthFormatter.FormatAlphanumeric(value, totalWidth, truncate);

        // Assert
        result.Should().Be(expected);
        result.Length.Should().Be(totalWidth);
    }

    [Theory]
    [InlineData("ABCDEFGHIJK", 5, true, "ABCDE")] // Truncate long string
    [InlineData("TOOLONGVALUE", 8, true, "TOOLONGV")] // Truncate
    [InlineData("TRUNCATEME", 5, true, "TRUNC")] // Truncate to 5
    public void FormatAlphanumeric_LongString_WithTruncate_TruncatesCorrectly(
        string value, int totalWidth, bool truncate, string expected)
    {
        // Act
        var result = FixedWidthFormatter.FormatAlphanumeric(value, totalWidth, truncate);

        // Assert
        result.Should().Be(expected);
        result.Length.Should().Be(totalWidth);
    }

    [Theory]
    [InlineData("TOOLONG", 5)]
    [InlineData("ABCDEFGHIJK", 8)]
    public void FormatAlphanumeric_LongString_WithoutTruncate_ThrowsArgumentException(string value, int totalWidth)
    {
        // Act
        Action act = () => FixedWidthFormatter.FormatAlphanumeric(value, totalWidth, truncate: false);

        // Assert
        act.Should().Throw<ArgumentException>()
            .WithMessage($"*Value length ({value.Length}) exceeds total width ({totalWidth})*")
            .And.ParamName.Should().Be("value");
    }

    [Theory]
    [InlineData(null, 10, "          ")] // Null returns spaces
    [InlineData("", 10, "          ")] // Empty string returns spaces
    public void FormatAlphanumeric_NullOrEmpty_ReturnsSpaces(string? value, int totalWidth, string expected)
    {
        // Act
        var result = FixedWidthFormatter.FormatAlphanumeric(value, totalWidth);

        // Assert
        result.Should().Be(expected);
        result.Length.Should().Be(totalWidth);
    }

    [Theory]
    [InlineData("@#$%", 10, "@#$%      ")] // Special characters
    [InlineData("TEST-123", 12, "TEST-123    ")] // Alphanumeric with hyphen
    [InlineData("A/B", 8, "A/B     ")] // Forward slash
    public void FormatAlphanumeric_SpecialCharacters_HandlesCorrectly(
        string value, int totalWidth, string expected)
    {
        // Act
        var result = FixedWidthFormatter.FormatAlphanumeric(value, totalWidth);

        // Assert
        result.Should().Be(expected);
        result.Length.Should().Be(totalWidth);
    }

    [Theory]
    [InlineData("São Paulo", 15, "São Paulo      ")] // Portuguese characters with tilde
    [InlineData("José", 10, "José      ")] // Accented character
    [InlineData("Conceição", 12, "Conceição   ")] // Cedilla
    [InlineData("André", 10, "André     ")] // Acute accent
    public void FormatAlphanumeric_PortugueseCharacters_HandlesCorrectly(
        string value, int totalWidth, string expected)
    {
        // Act
        var result = FixedWidthFormatter.FormatAlphanumeric(value, totalWidth);

        // Assert
        result.Should().Be(expected);
        result.Length.Should().Be(totalWidth);
    }

    [Theory]
    [InlineData(0)]
    [InlineData(-1)]
    [InlineData(-10)]
    public void FormatAlphanumeric_InvalidTotalWidth_ThrowsArgumentException(int totalWidth)
    {
        // Act
        Action act = () => FixedWidthFormatter.FormatAlphanumeric("TEST", totalWidth);

        // Assert
        act.Should().Throw<ArgumentException>()
            .WithMessage("*Total width must be greater than zero*")
            .And.ParamName.Should().Be("totalWidth");
    }

    #endregion

    #region FormatDate Tests

    [Theory]
    [InlineData(2025, 10, 22, DateFormat.YYYYMMDD, "20251022")]
    [InlineData(2025, 1, 1, DateFormat.YYYYMMDD, "20250101")]
    [InlineData(2025, 12, 31, DateFormat.YYYYMMDD, "20251231")]
    [InlineData(2000, 2, 29, DateFormat.YYYYMMDD, "20000229")] // Leap year
    public void FormatDate_YYYYMMDD_VariousDates_ReturnsCorrectFormat(
        int year, int month, int day, DateFormat format, string expected)
    {
        // Arrange
        var date = new DateTime(year, month, day);

        // Act
        var result = FixedWidthFormatter.FormatDate(date, format);

        // Assert
        result.Should().Be(expected);
        result.Length.Should().Be(8);
    }

    [Theory]
    [InlineData(2025, 10, 22, DateFormat.DDMMYYYY, "22102025")]
    [InlineData(2025, 1, 1, DateFormat.DDMMYYYY, "01012025")]
    [InlineData(2025, 12, 31, DateFormat.DDMMYYYY, "31122025")]
    public void FormatDate_DDMMYYYY_VariousDates_ReturnsCorrectFormat(
        int year, int month, int day, DateFormat format, string expected)
    {
        // Arrange
        var date = new DateTime(year, month, day);

        // Act
        var result = FixedWidthFormatter.FormatDate(date, format);

        // Assert
        result.Should().Be(expected);
        result.Length.Should().Be(8);
    }

    [Theory]
    [InlineData(2025, 10, 22, DateFormat.ISO8601, "2025-10-22")]
    [InlineData(2025, 1, 1, DateFormat.ISO8601, "2025-01-01")]
    [InlineData(2025, 12, 31, DateFormat.ISO8601, "2025-12-31")]
    public void FormatDate_ISO8601_VariousDates_ReturnsCorrectFormat(
        int year, int month, int day, DateFormat format, string expected)
    {
        // Arrange
        var date = new DateTime(year, month, day);

        // Act
        var result = FixedWidthFormatter.FormatDate(date, format);

        // Assert
        result.Should().Be(expected);
        result.Length.Should().Be(10);
    }

    [Theory]
    [InlineData(2025, 10, 22, DateFormat.MMDDYYYY, "10222025")]
    [InlineData(2025, 1, 15, DateFormat.MMDDYYYY, "01152025")]
    public void FormatDate_MMDDYYYY_VariousDates_ReturnsCorrectFormat(
        int year, int month, int day, DateFormat format, string expected)
    {
        // Arrange
        var date = new DateTime(year, month, day);

        // Act
        var result = FixedWidthFormatter.FormatDate(date, format);

        // Assert
        result.Should().Be(expected);
        result.Length.Should().Be(8);
    }

    [Fact]
    public void FormatDate_MinValue_HandlesCorrectly()
    {
        // Arrange
        var date = DateTime.MinValue; // 0001-01-01

        // Act
        var result = FixedWidthFormatter.FormatDate(date, DateFormat.YYYYMMDD);

        // Assert
        result.Should().Be("00010101");
    }

    [Fact]
    public void FormatDate_MaxValue_HandlesCorrectly()
    {
        // Arrange
        var date = DateTime.MaxValue; // 9999-12-31

        // Act
        var result = FixedWidthFormatter.FormatDate(date, DateFormat.YYYYMMDD);

        // Assert
        result.Should().Be("99991231");
    }

    [Theory]
    [InlineData("yyyyMMdd", "20251022")]
    [InlineData("dd/MM/yyyy", "22/10/2025")]
    [InlineData("yyyy-MM-dd HH:mm:ss", "2025-10-22 00:00:00")]
    [InlineData("MMM dd, yyyy", "Oct 22, 2025")]
    public void FormatDate_CustomPattern_ReturnsCorrectFormat(string pattern, string expected)
    {
        // Arrange
        var date = new DateTime(2025, 10, 22);

        // Act
        var result = FixedWidthFormatter.FormatDate(date, pattern);

        // Assert
        result.Should().Be(expected);
    }

    [Theory]
    [InlineData(null)]
    [InlineData("")]
    [InlineData("   ")]
    public void FormatDate_CustomPattern_EmptyPattern_ThrowsArgumentException(string? pattern)
    {
        // Arrange
        var date = new DateTime(2025, 10, 22);

        // Act
        Action act = () => FixedWidthFormatter.FormatDate(date, pattern!);

        // Assert
        act.Should().Throw<ArgumentException>()
            .WithMessage("*Pattern cannot be null or empty*")
            .And.ParamName.Should().Be("pattern");
    }

    [Theory]
    [InlineData(DateFormat.YYYYMMDD, 8, "        ")] // 8 spaces for YYYYMMDD
    [InlineData(DateFormat.ISO8601, 10, "          ")] // 10 spaces for ISO8601
    [InlineData(DateFormat.DDMMYYYY, 8, "        ")] // 8 spaces for DDMMYYYY
    public void FormatDate_NullableDate_NullValue_ReturnsSpaces(
        DateFormat format, int totalWidth, string expected)
    {
        // Arrange
        DateTime? date = null;

        // Act
        var result = FixedWidthFormatter.FormatDate(date, format, totalWidth);

        // Assert
        result.Should().Be(expected);
        result.Length.Should().Be(totalWidth);
    }

    [Fact]
    public void FormatDate_NullableDate_HasValue_ReturnsFormattedDate()
    {
        // Arrange
        DateTime? date = new DateTime(2025, 10, 22);

        // Act
        var result = FixedWidthFormatter.FormatDate(date, DateFormat.YYYYMMDD);

        // Assert
        result.Should().Be("20251022");
    }

    #endregion

    #region FormatBoolean Tests

    [Theory]
    [InlineData(true, 'S', 'N', "S")] // Default: true → "S"
    [InlineData(false, 'S', 'N', "N")] // Default: false → "N"
    [InlineData(true, '1', '0', "1")] // Binary: true → "1"
    [InlineData(false, '1', '0', "0")] // Binary: false → "0"
    [InlineData(true, 'Y', 'N', "Y")] // English: true → "Y"
    [InlineData(false, 'Y', 'N', "N")] // English: false → "N"
    [InlineData(true, 'T', 'F', "T")] // T/F format
    [InlineData(false, 'T', 'F', "F")] // T/F format
    public void FormatBoolean_VariousCharacters_ReturnsCorrectFormat(
        bool value, char trueChar, char falseChar, string expected)
    {
        // Act
        var result = FixedWidthFormatter.FormatBoolean(value, trueChar, falseChar);

        // Assert
        result.Should().Be(expected);
        result.Length.Should().Be(1);
    }

    [Fact]
    public void FormatBoolean_DefaultCharacters_True_ReturnsS()
    {
        // Act
        var result = FixedWidthFormatter.FormatBoolean(true);

        // Assert
        result.Should().Be("S");
    }

    [Fact]
    public void FormatBoolean_DefaultCharacters_False_ReturnsN()
    {
        // Act
        var result = FixedWidthFormatter.FormatBoolean(false);

        // Assert
        result.Should().Be("N");
    }

    [Theory]
    [InlineData(true, 'S', 'N', ' ', "S")]
    [InlineData(false, 'S', 'N', ' ', "N")]
    public void FormatBoolean_Nullable_HasValue_ReturnsFormattedValue(
        bool value, char trueChar, char falseChar, char nullChar, string expected)
    {
        // Arrange
        bool? nullableValue = value;

        // Act
        var result = FixedWidthFormatter.FormatBoolean(nullableValue, trueChar, falseChar, nullChar);

        // Assert
        result.Should().Be(expected);
        result.Length.Should().Be(1);
    }

    [Theory]
    [InlineData('S', 'N', ' ', " ")] // Default null character
    [InlineData('1', '0', '?', "?")] // Custom null character
    [InlineData('Y', 'N', '-', "-")] // Hyphen for null
    public void FormatBoolean_Nullable_NullValue_ReturnsNullChar(
        char trueChar, char falseChar, char nullChar, string expected)
    {
        // Arrange
        bool? nullableValue = null;

        // Act
        var result = FixedWidthFormatter.FormatBoolean(nullableValue, trueChar, falseChar, nullChar);

        // Assert
        result.Should().Be(expected);
        result.Length.Should().Be(1);
    }

    [Fact]
    public void FormatBoolean_Nullable_DefaultParams_NullValue_ReturnsSpace()
    {
        // Arrange
        bool? nullableValue = null;

        // Act
        var result = FixedWidthFormatter.FormatBoolean(nullableValue);

        // Assert
        result.Should().Be(" ");
    }

    #endregion

    #region BuildRecord Tests

    [Fact]
    public void BuildRecord_MultipleFields_ConcatenatesCorrectly()
    {
        // Arrange
        var field1 = FixedWidthFormatter.FormatNumeric(123, 5);
        var field2 = FixedWidthFormatter.FormatAlphanumeric("ABC", 10);
        var field3 = FixedWidthFormatter.FormatDate(new DateTime(2025, 10, 22), DateFormat.YYYYMMDD);

        // Act
        var result = FixedWidthFormatter.BuildRecord(field1, field2, field3);

        // Assert
        result.Should().Be("00123ABC       20251022");
        result.Length.Should().Be(5 + 10 + 8);
    }

    [Fact]
    public void BuildRecord_SingleField_ReturnsField()
    {
        // Arrange
        var field = FixedWidthFormatter.FormatNumeric(123, 5);

        // Act
        var result = FixedWidthFormatter.BuildRecord(field);

        // Assert
        result.Should().Be("00123");
    }

    [Fact]
    public void BuildRecord_EmptyArray_ReturnsEmptyString()
    {
        // Act
        var result = FixedWidthFormatter.BuildRecord();

        // Assert
        result.Should().BeEmpty();
    }

    [Fact]
    public void BuildRecord_NullArray_ReturnsEmptyString()
    {
        // Act
        var result = FixedWidthFormatter.BuildRecord(null!);

        // Assert
        result.Should().BeEmpty();
    }

    [Fact]
    public void BuildRecord_ManyFields_MaintainsOrder()
    {
        // Arrange
        var fields = new[]
        {
            "A",
            "B",
            "C",
            "D",
            "E"
        };

        // Act
        var result = FixedWidthFormatter.BuildRecord(fields);

        // Assert
        result.Should().Be("ABCDE");
    }

    [Fact]
    public void BuildRecord_ComplexRecord_AllFieldTypes_BuildsCorrectly()
    {
        // Arrange - Simulate COBOL fixed-width record with all field types
        var policyNumber = FixedWidthFormatter.FormatNumeric(12345, 10);
        var clientName = FixedWidthFormatter.FormatAlphanumeric("JOÃO SILVA", 30);
        var premiumAmount = FixedWidthFormatter.FormatNumeric(1234.56m, 15, 2);
        var effectiveDate = FixedWidthFormatter.FormatDate(new DateTime(2025, 10, 22), DateFormat.YYYYMMDD);
        var activeFlag = FixedWidthFormatter.FormatBoolean(true);

        // Act
        var result = FixedWidthFormatter.BuildRecord(
            policyNumber,
            clientName,
            premiumAmount,
            effectiveDate,
            activeFlag
        );

        // Assert
        result.Should().Be("0000012345JOÃO SILVA                    00000000012345620251022S");
        result.Length.Should().Be(10 + 30 + 15 + 8 + 1); // 64 total
    }

    #endregion

    #region ValidateWidth Tests

    [Theory]
    [InlineData("12345", 5, "test_field")]
    [InlineData("ABC", 3, "name")]
    [InlineData("          ", 10, "padding")]
    public void ValidateWidth_MatchingWidth_DoesNotThrow(string formattedValue, int expectedWidth, string fieldName)
    {
        // Act
        Action act = () => FixedWidthFormatter.ValidateWidth(formattedValue, expectedWidth, fieldName);

        // Assert
        act.Should().NotThrow();
    }

    [Theory]
    [InlineData("12345", 10, "test_field", 5, 10)] // Too short
    [InlineData("ABCDEFGHIJ", 5, "name", 10, 5)] // Too long
    [InlineData("TEST", 8, "code", 4, 8)] // Too short
    public void ValidateWidth_MismatchedWidth_ThrowsInvalidOperationException(
        string formattedValue, int expectedWidth, string fieldName, int actualLength, int expected)
    {
        // Act
        Action act = () => FixedWidthFormatter.ValidateWidth(formattedValue, expectedWidth, fieldName);

        // Assert
        act.Should().Throw<InvalidOperationException>()
            .WithMessage($"*{fieldName} width mismatch: expected {expected}, got {actualLength}*");
    }

    [Fact]
    public void ValidateWidth_NullValue_ThrowsArgumentNullException()
    {
        // Act
        Action act = () => FixedWidthFormatter.ValidateWidth(null!, 10);

        // Assert
        act.Should().Throw<ArgumentNullException>()
            .And.ParamName.Should().Be("formattedValue");
    }

    [Fact]
    public void ValidateWidth_DefaultFieldName_UsesGenericName()
    {
        // Act
        Action act = () => FixedWidthFormatter.ValidateWidth("ABC", 10);

        // Assert
        act.Should().Throw<InvalidOperationException>()
            .WithMessage("*field width mismatch*");
    }

    [Fact]
    public void ValidateWidth_EmptyString_ZeroExpectedWidth_DoesNotThrow()
    {
        // Act
        Action act = () => FixedWidthFormatter.ValidateWidth("", 0, "empty_field");

        // Assert
        act.Should().NotThrow();
    }

    #endregion

    #region Integration Tests (Full Record Scenarios)

    [Fact]
    public void FullScenario_PremiumRecord_PREMIT_Format_BuildsCorrectly()
    {
        // Arrange - Simulate building a PREMIT.TXT record
        var companyCode = FixedWidthFormatter.FormatNumeric(123, 3);
        var suspCode = FixedWidthFormatter.FormatNumeric(45, 2);
        var policyNumber = FixedWidthFormatter.FormatNumeric(9876543210, 10);
        var endorsementNumber = FixedWidthFormatter.FormatNumeric(1, 4);
        var premiumAmount = FixedWidthFormatter.FormatNumeric(12345.67m, 15, 2);
        var effectiveDate = FixedWidthFormatter.FormatDate(new DateTime(2025, 10, 1), DateFormat.YYYYMMDD);
        var expirationDate = FixedWidthFormatter.FormatDate(new DateTime(2026, 10, 1), DateFormat.YYYYMMDD);

        // Act
        var record = FixedWidthFormatter.BuildRecord(
            companyCode,
            suspCode,
            policyNumber,
            endorsementNumber,
            premiumAmount,
            effectiveDate,
            expirationDate
        );

        // Assert
        record.Should().Be("12345987654321000010000000012345672025100120261001");
        record.Length.Should().Be(3 + 2 + 10 + 4 + 15 + 8 + 8); // 50 total

        // Validate each field width
        FixedWidthFormatter.ValidateWidth(companyCode, 3, "companyCode");
        FixedWidthFormatter.ValidateWidth(suspCode, 2, "suspCode");
        FixedWidthFormatter.ValidateWidth(policyNumber, 10, "policyNumber");
        FixedWidthFormatter.ValidateWidth(endorsementNumber, 4, "endorsementNumber");
        FixedWidthFormatter.ValidateWidth(premiumAmount, 15, "premiumAmount");
        FixedWidthFormatter.ValidateWidth(effectiveDate, 8, "effectiveDate");
        FixedWidthFormatter.ValidateWidth(expirationDate, 8, "expirationDate");
    }

    [Fact]
    public void FullScenario_ClientRecord_AllFieldTypes_BuildsCorrectly()
    {
        // Arrange - Simulate building a client master record
        var clientId = FixedWidthFormatter.FormatNumeric(12345678, 8);
        var clientName = FixedWidthFormatter.FormatAlphanumeric("MARIA CONCEIÇÃO SANTOS", 40);
        var cpf = FixedWidthFormatter.FormatAlphanumeric("12345678900", 11);
        var birthDate = FixedWidthFormatter.FormatDate(new DateTime(1980, 5, 15), DateFormat.DDMMYYYY);
        var activeFlag = FixedWidthFormatter.FormatBoolean(true, 'S', 'N');
        var registrationDate = FixedWidthFormatter.FormatDate(new DateTime(2020, 1, 10), DateFormat.YYYYMMDD);
        var balance = FixedWidthFormatter.FormatNumeric(9876.54m, 12, 2);

        // Act
        var record = FixedWidthFormatter.BuildRecord(
            clientId,
            clientName,
            cpf,
            birthDate,
            activeFlag,
            registrationDate,
            balance
        );

        // Assert
        record.Should().NotBeNullOrEmpty();
        record.Length.Should().Be(8 + 40 + 11 + 8 + 1 + 8 + 12); // 88 total
        record.Should().StartWith("12345678");
        record.Should().Contain("MARIA CONCEIÇÃO SANTOS");
        record.Should().Contain("S"); // Active flag
    }

    #endregion
}
