using System;

namespace CaixaSeguradora.Core.Attributes
{
    [AttributeUsage(AttributeTargets.Property)]
    public class CobolFieldAttribute : Attribute
    {
        public string PicClause { get; set; }
        public int Length { get; set; }
        public int DecimalPlaces { get; set; }
        public CobolFieldType FieldType { get; set; } = CobolFieldType.Display;
        public int StartPosition { get; set; }
        public string Description { get; set; }

        // Parameterless constructor
        public CobolFieldAttribute()
        {
        }

        // Constructor with PicClause
        public CobolFieldAttribute(string picClause)
        {
            PicClause = picClause;
        }

        // Constructor with PicClause and FieldType
        public CobolFieldAttribute(string picClause, CobolFieldType fieldType)
        {
            PicClause = picClause;
            FieldType = fieldType;
        }

        // Constructor with PicClause, FieldType, and Length (matching entity usage)
        public CobolFieldAttribute(string picClause, CobolFieldType fieldType, int startPosition, int length)
        {
            PicClause = picClause;
            FieldType = fieldType;
            StartPosition = startPosition;
            Length = length;
        }

        // Constructor with PicClause, FieldType, Length, and DecimalPlaces (for decimal fields)
        public CobolFieldAttribute(string picClause, CobolFieldType fieldType, int startPosition, int length, int decimalPlaces)
        {
            PicClause = picClause;
            FieldType = fieldType;
            StartPosition = startPosition;
            Length = length;
            DecimalPlaces = decimalPlaces;
        }

        // Constructor with PicClause, FieldType, Length, DecimalPlaces, and Description (for decimal fields with description)
        public CobolFieldAttribute(string picClause, CobolFieldType fieldType, int startPosition, int length, int decimalPlaces, string description)
        {
            PicClause = picClause;
            FieldType = fieldType;
            StartPosition = startPosition;
            Length = length;
            DecimalPlaces = decimalPlaces;
            Description = description;
        }

        // Constructor with PicClause, Length, and DecimalPlaces
        public CobolFieldAttribute(string picClause, int length, int decimalPlaces)
        {
            PicClause = picClause;
            Length = length;
            DecimalPlaces = decimalPlaces;
        }

        // Constructor with PicClause and Length
        public CobolFieldAttribute(string picClause, int length)
        {
            PicClause = picClause;
            Length = length;
        }
    }
}
