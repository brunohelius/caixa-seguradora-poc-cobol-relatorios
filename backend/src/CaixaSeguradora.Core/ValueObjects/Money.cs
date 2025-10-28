using System;

namespace CaixaSeguradora.Core.ValueObjects
{
    /// <summary>
    /// Money value object with currency support.
    /// Ensures financial amounts always carry currency context.
    /// Provides type-safe arithmetic operations preventing currency mismatches.
    /// Critical for maintaining decimal precision in financial calculations.
    /// </summary>
    public class Money : IEquatable<Money>, IComparable<Money>
    {
        /// <summary>
        /// The monetary amount with decimal precision.
        /// Uses C# decimal type to match COBOL COMP-3 precision.
        /// </summary>
        public decimal Amount { get; }

        /// <summary>
        /// ISO 4217 currency code (e.g., "BRL", "USD", "EUR").
        /// Defaults to "BRL" for Brazilian Real.
        /// </summary>
        public string Currency { get; }

        /// <summary>
        /// Creates a new Money instance.
        /// </summary>
        /// <param name="amount">Monetary amount</param>
        /// <param name="currency">ISO currency code (default: BRL)</param>
        public Money(decimal amount, string currency = "BRL")
        {
            Amount = amount;
            Currency = currency?.ToUpperInvariant() ?? "BRL";
        }

        /// <summary>
        /// Creates a zero money instance for the specified currency.
        /// </summary>
        public static Money Zero(string currency = "BRL") => new Money(0, currency);

        /// <summary>
        /// Adds two Money instances (same currency required).
        /// </summary>
        public Money Add(Money other)
        {
            ValidateSameCurrency(other);
            return new Money(Amount + other.Amount, Currency);
        }

        /// <summary>
        /// Subtracts another Money instance (same currency required).
        /// </summary>
        public Money Subtract(Money other)
        {
            ValidateSameCurrency(other);
            return new Money(Amount - other.Amount, Currency);
        }

        /// <summary>
        /// Multiplies the amount by a scalar value.
        /// </summary>
        /// <param name="multiplier">Scalar multiplier (e.g., rate, quantity)</param>
        /// <returns>New Money instance with multiplied amount</returns>
        public Money Multiply(decimal multiplier)
        {
            return new Money(Amount * multiplier, Currency);
        }

        /// <summary>
        /// Divides the amount by a scalar value.
        /// </summary>
        /// <param name="divisor">Scalar divisor</param>
        /// <returns>New Money instance with divided amount</returns>
        /// <exception cref="DivideByZeroException">When divisor is zero</exception>
        public Money Divide(decimal divisor)
        {
            if (divisor == 0)
                throw new DivideByZeroException("Cannot divide money by zero");

            return new Money(Amount / divisor, Currency);
        }

        /// <summary>
        /// Returns the absolute value of the amount.
        /// </summary>
        public Money Abs()
        {
            return new Money(Math.Abs(Amount), Currency);
        }

        /// <summary>
        /// Returns the negated amount.
        /// Useful for refunds, cancellations, reversals.
        /// </summary>
        public Money Negate()
        {
            return new Money(-Amount, Currency);
        }

        /// <summary>
        /// Rounds the amount to the specified number of decimal places.
        /// Uses banker's rounding (MidpointRounding.ToEven) to match COBOL behavior.
        /// </summary>
        /// <param name="decimalPlaces">Number of decimal places (default: 2 for BRL)</param>
        public Money Round(int decimalPlaces = 2)
        {
            return new Money(Math.Round(Amount, decimalPlaces, MidpointRounding.ToEven), Currency);
        }

        /// <summary>
        /// Checks if the amount is zero.
        /// </summary>
        public bool IsZero => Amount == 0;

        /// <summary>
        /// Checks if the amount is positive (greater than zero).
        /// </summary>
        public bool IsPositive => Amount > 0;

        /// <summary>
        /// Checks if the amount is negative (less than zero).
        /// </summary>
        public bool IsNegative => Amount < 0;

        /// <summary>
        /// Validates that two Money instances have the same currency.
        /// </summary>
        private void ValidateSameCurrency(Money other)
        {
            if (Currency != other.Currency)
                throw new InvalidOperationException(
                    $"Cannot perform operation on different currencies: {Currency} and {other.Currency}");
        }

        #region Equality and Comparison

        public bool Equals(Money? other)
        {
            if (other is null) return false;
            return Amount == other.Amount && Currency == other.Currency;
        }

        public override bool Equals(object? obj)
        {
            return Equals(obj as Money);
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(Amount, Currency);
        }

        public int CompareTo(Money? other)
        {
            if (other is null) return 1;
            ValidateSameCurrency(other);
            return Amount.CompareTo(other.Amount);
        }

        #endregion

        #region Operators

        public static Money operator +(Money left, Money right) => left.Add(right);
        public static Money operator -(Money left, Money right) => left.Subtract(right);
        public static Money operator *(Money money, decimal multiplier) => money.Multiply(multiplier);
        public static Money operator *(decimal multiplier, Money money) => money.Multiply(multiplier);
        public static Money operator /(Money money, decimal divisor) => money.Divide(divisor);
        public static Money operator -(Money money) => money.Negate();

        public static bool operator ==(Money left, Money right)
        {
            if (left is null) return right is null;
            return left.Equals(right);
        }

        public static bool operator !=(Money left, Money right) => !(left == right);
        public static bool operator <(Money left, Money right) => left.CompareTo(right) < 0;
        public static bool operator <=(Money left, Money right) => left.CompareTo(right) <= 0;
        public static bool operator >(Money left, Money right) => left.CompareTo(right) > 0;
        public static bool operator >=(Money left, Money right) => left.CompareTo(right) >= 0;

        #endregion

        #region String Representation

        /// <summary>
        /// Returns formatted string representation (e.g., "BRL 1,234.56").
        /// </summary>
        public override string ToString()
        {
            return $"{Currency} {Amount:N2}";
        }

        /// <summary>
        /// Returns formatted string with custom format.
        /// </summary>
        /// <param name="format">Decimal format string</param>
        /// <returns>Formatted money string</returns>
        public string ToString(string format)
        {
            return $"{Currency} {Amount.ToString(format)}";
        }

        /// <summary>
        /// Returns formatted string suitable for Portuguese locale (R$ for BRL).
        /// </summary>
        public string ToPortugueseString()
        {
            string symbol = Currency == "BRL" ? "R$" : Currency;
            return $"{symbol} {Amount:N2}";
        }

        #endregion
    }
}
