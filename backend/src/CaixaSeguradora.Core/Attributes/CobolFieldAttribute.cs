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
    }
}
