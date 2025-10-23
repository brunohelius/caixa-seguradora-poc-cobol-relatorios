namespace CaixaSeguradora.Core.Attributes
{
    public enum CobolFieldType
    {
        Display,          // PIC X(n) or PIC 9(n) - standard display
        Numeric,          // PIC 9(n) COMP - binary integer
        PackedDecimal,    // PIC 9(n) COMP-3 - packed decimal
        SignedNumeric     // PIC S9(n) COMP - signed binary
    }
}
