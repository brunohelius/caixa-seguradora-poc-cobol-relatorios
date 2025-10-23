using System;

namespace CaixaSeguradora.Core.Attributes
{
    [AttributeUsage(AttributeTargets.Property, AllowMultiple = false)]
    public class CobolFieldAttribute : Attribute
    {
        public string OriginalName { get; set; }
        public CobolFieldType FieldType { get; set; }
        public int Position { get; set; }
        public int Length { get; set; }
        public int DecimalPlaces { get; set; }
        public string PicClause { get; set; }

        // Parameterless constructor for property-based initialization
        public CobolFieldAttribute()
        {
        }

        // Full constructor for backward compatibility
        public CobolFieldAttribute(
            string originalName,
            CobolFieldType fieldType = CobolFieldType.Alphanumeric,
            int position = 0,
            int length = 0,
            int decimalPlaces = 0,
            string picClause = "")
        {
            OriginalName = originalName;
            FieldType = fieldType;
            Position = position;
            Length = length;
            DecimalPlaces = decimalPlaces;
            PicClause = picClause;
        }
    }
}
