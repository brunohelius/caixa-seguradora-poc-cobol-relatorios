using System;

namespace CaixaSeguradora.Core.Attributes
{
    /// <summary>
    /// Attribute to define fixed-width file field layout for output records.
    /// Used to generate PREMIT.TXT and PREMCED.TXT files with exact byte positioning.
    /// Critical for SUSEP file format compliance.
    /// </summary>
    /// <example>
    /// [FixedWidthField(Position = 1, Length = 5, Type = FieldType.Numeric, DecimalPlaces = 0)]
    /// public int CompanyCode { get; set; }
    ///
    /// [FixedWidthField(Position = 6, Length = 15, Type = FieldType.Numeric, DecimalPlaces = 2)]
    /// public decimal TotalPremium { get; set; }
    /// </example>
    [AttributeUsage(AttributeTargets.Property, AllowMultiple = false)]
    public class FixedWidthFieldAttribute : Attribute
    {
        /// <summary>
        /// Starting position of the field in the output record (1-based).
        /// Position 1 is the first byte of the record.
        /// </summary>
        public int Position { get; set; }

        /// <summary>
        /// Total length of the field in bytes.
        /// For numeric fields: includes decimal places (implied decimal point).
        /// For alphanumeric fields: total character count.
        /// </summary>
        public int Length { get; set; }

        /// <summary>
        /// Field data type for formatting purposes.
        /// Determines padding rules (left-pad zeros for numeric, right-pad spaces for alphanumeric).
        /// </summary>
        public FieldType Type { get; set; } = FieldType.Alphanumeric;

        /// <summary>
        /// Number of decimal places for numeric fields (implied decimal point).
        /// Example: 1234567 with DecimalPlaces=2 → "0000001234567" represents 12345.67
        /// </summary>
        public int DecimalPlaces { get; set; } = 0;

        /// <summary>
        /// Format string for custom formatting (optional).
        /// Overrides default formatting rules if provided.
        /// </summary>
        public string Format { get; set; }

        /// <summary>
        /// Padding character for alignment (default: '0' for numeric, ' ' for alphanumeric).
        /// </summary>
        public char PaddingChar { get; set; }

        /// <summary>
        /// Description of the field for documentation purposes.
        /// </summary>
        public string Description { get; set; }

        /// <summary>
        /// Default constructor.
        /// </summary>
        public FixedWidthFieldAttribute()
        {
        }

        /// <summary>
        /// Constructor with position and length.
        /// </summary>
        public FixedWidthFieldAttribute(int position, int length)
        {
            Position = position;
            Length = length;
        }

        /// <summary>
        /// Constructor with position, length, and type.
        /// </summary>
        public FixedWidthFieldAttribute(int position, int length, FieldType type)
        {
            Position = position;
            Length = length;
            Type = type;

            // Set default padding character based on type
            PaddingChar = type == FieldType.Numeric ? '0' : ' ';
        }

        /// <summary>
        /// Constructor with all primary parameters.
        /// </summary>
        public FixedWidthFieldAttribute(int position, int length, FieldType type, int decimalPlaces)
        {
            Position = position;
            Length = length;
            Type = type;
            DecimalPlaces = decimalPlaces;

            // Set default padding character based on type
            PaddingChar = type == FieldType.Numeric ? '0' : ' ';
        }

        /// <summary>
        /// Returns the ending position of the field (inclusive).
        /// </summary>
        public int EndPosition => Position + Length - 1;

        /// <summary>
        /// Validates that field does not exceed record boundaries.
        /// </summary>
        /// <param name="recordLength">Total record length in bytes</param>
        /// <returns>True if field fits within record</returns>
        public bool IsValidForRecordLength(int recordLength)
        {
            return Position >= 1 && EndPosition <= recordLength;
        }

        /// <summary>
        /// Returns a human-readable description of the field layout.
        /// </summary>
        public override string ToString()
        {
            return $"Position {Position}-{EndPosition} (Length={Length}, Type={Type}, Decimals={DecimalPlaces})";
        }
    }

    /// <summary>
    /// Field type enumeration for fixed-width formatting.
    /// </summary>
    public enum FieldType
    {
        /// <summary>
        /// Alphanumeric field (right-padded with spaces).
        /// Example: "ABC" with length 10 → "ABC       "
        /// </summary>
        Alphanumeric,

        /// <summary>
        /// Numeric field (left-padded with zeros, implied decimal point).
        /// Example: 123.45 with length 10, decimals 2 → "0000012345"
        /// </summary>
        Numeric,

        /// <summary>
        /// Date field (formatted as YYYYMMDD).
        /// Example: 2025-10-27 → "20251027"
        /// </summary>
        Date,

        /// <summary>
        /// Signed numeric field (may include overpunch or sign character).
        /// </summary>
        SignedNumeric
    }
}
