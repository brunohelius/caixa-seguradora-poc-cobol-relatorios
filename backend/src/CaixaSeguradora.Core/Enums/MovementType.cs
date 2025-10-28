using System.ComponentModel;

namespace CaixaSeguradora.Core.Enums
{
    /// <summary>
    /// Premium movement types from COBOL program EMI-TIPO-MOV.
    /// Defines all possible premium transaction types for SUSEP reporting.
    /// Values 101-106 match COBOL numeric codes.
    /// </summary>
    public enum MovementType
    {
        /// <summary>
        /// Emissão Normal - New policy issuance.
        /// Initial premium for a newly issued policy.
        /// COBOL Code: 101
        /// </summary>
        [Description("Emissão Normal")]
        Emission = 101,

        /// <summary>
        /// Renovação - Policy renewal.
        /// Premium for renewing an existing policy for another term.
        /// COBOL Code: 102
        /// </summary>
        [Description("Renovação")]
        Renewal = 102,

        /// <summary>
        /// Endosso de Majoração - Endorsement with premium increase.
        /// Policy modification that increases coverage and premium.
        /// Examples: Adding coverage, increasing insured sum, extending term.
        /// COBOL Code: 103
        /// </summary>
        [Description("Endosso de Majoração")]
        EndorsementIncrease = 103,

        /// <summary>
        /// Endosso de Redução - Endorsement with premium decrease.
        /// Policy modification that reduces coverage and premium.
        /// Examples: Removing coverage, decreasing insured sum, shortening term.
        /// COBOL Code: 104
        /// </summary>
        [Description("Endosso de Redução")]
        EndorsementDecrease = 104,

        /// <summary>
        /// Cancelamento - Policy cancellation.
        /// Generates negative premium for refund calculation.
        /// Requires pro-rata calculation for unused term.
        /// COBOL Code: 105
        /// </summary>
        [Description("Cancelamento")]
        Cancellation = 105,

        /// <summary>
        /// Restituição - Premium restitution/refund.
        /// Return of premium to insured (e.g., after cancellation, overpayment).
        /// COBOL Code: 106
        /// </summary>
        [Description("Restituição")]
        Restitution = 106
    }

    /// <summary>
    /// Extension methods for MovementType enum.
    /// </summary>
    public static class MovementTypeExtensions
    {
        /// <summary>
        /// Gets the COBOL numeric code for the movement type.
        /// </summary>
        public static int ToCobolCode(this MovementType movementType)
        {
            return (int)movementType;
        }

        /// <summary>
        /// Gets the Portuguese description for the movement type.
        /// </summary>
        public static string GetDescription(this MovementType movementType)
        {
            var fieldInfo = movementType.GetType().GetField(movementType.ToString());
            var attributes = (DescriptionAttribute[])fieldInfo.GetCustomAttributes(typeof(DescriptionAttribute), false);
            return attributes.Length > 0 ? attributes[0].Description : movementType.ToString();
        }

        /// <summary>
        /// Determines if the movement type generates a negative premium.
        /// </summary>
        public static bool IsNegativePremium(this MovementType movementType)
        {
            return movementType == MovementType.Cancellation || movementType == MovementType.Restitution;
        }

        /// <summary>
        /// Determines if the movement type is an endorsement.
        /// </summary>
        public static bool IsEndorsement(this MovementType movementType)
        {
            return movementType == MovementType.EndorsementIncrease || movementType == MovementType.EndorsementDecrease;
        }

        /// <summary>
        /// Parses a COBOL code to MovementType enum.
        /// </summary>
        public static MovementType FromCobolCode(int code)
        {
            return code switch
            {
                101 => MovementType.Emission,
                102 => MovementType.Renewal,
                103 => MovementType.EndorsementIncrease,
                104 => MovementType.EndorsementDecrease,
                105 => MovementType.Cancellation,
                106 => MovementType.Restitution,
                _ => throw new ArgumentException($"Invalid movement type code: {code}", nameof(code))
            };
        }
    }
}
