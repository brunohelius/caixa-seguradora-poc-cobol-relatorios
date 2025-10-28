using System;
using FluentAssertions;
using PdfGenerator.Formatting;
using Xunit;

namespace PdfGenerator.Tests.Formatting
{
    public class BrazilianFormatterTests
    {
        [Theory]
        [InlineData(1234.56, "R$ 1.234,56")]
        [InlineData(0, "R$ 0,00")]
        [InlineData(-500.50, "-R$ 500,50")]
        [InlineData(1000000.00, "R$ 1.000.000,00")]
        [InlineData(99.99, "R$ 99,99")]
        public void FormatCurrency_ShouldFormatCorrectly(decimal value, string expected)
        {
            // Act
            var result = BrazilianFormatter.FormatCurrency(value);

            // Assert
            result.Should().Be(expected);
        }

        [Theory]
        [InlineData("2025-10-23", "23/10/2025")]
        [InlineData("2025-01-01", "01/01/2025")]
        [InlineData("2025-12-31", "31/12/2025")]
        [InlineData("2024-02-29", "29/02/2024")] // Leap year
        public void FormatDate_ShouldFormatCorrectly(string dateStr, string expected)
        {
            // Arrange
            var date = DateTime.Parse(dateStr);

            // Act
            var result = BrazilianFormatter.FormatDate(date);

            // Assert
            result.Should().Be(expected);
        }

        [Theory]
        [InlineData("2025-10-23 14:30:45", "23/10/2025 14:30:45")]
        [InlineData("2025-01-01 00:00:00", "01/01/2025 00:00:00")]
        [InlineData("2025-12-31 23:59:59", "31/12/2025 23:59:59")]
        public void FormatDateTime_ShouldFormatCorrectly(string dateStr, string expected)
        {
            // Arrange
            var date = DateTime.Parse(dateStr);

            // Act
            var result = BrazilianFormatter.FormatDateTime(date);

            // Assert
            result.Should().Be(expected);
        }

        [Theory]
        [InlineData("2025-10-01", "outubro/2025")]
        [InlineData("2025-01-01", "janeiro/2025")]
        [InlineData("2025-12-01", "dezembro/2025")]
        public void FormatMonthYear_ShouldFormatCorrectly(string dateStr, string expected)
        {
            // Arrange
            var date = DateTime.Parse(dateStr);

            // Act
            var result = BrazilianFormatter.FormatMonthYear(date);

            // Assert
            result.Should().Be(expected);
        }

        [Theory]
        [InlineData(1234, "1.234")]
        [InlineData(0, "0")]
        [InlineData(-500, "-500")]
        [InlineData(1000000, "1.000.000")]
        [InlineData(99, "99")]
        public void FormatNumber_ShouldFormatCorrectly(int value, string expected)
        {
            // Act
            var result = BrazilianFormatter.FormatNumber(value);

            // Assert
            result.Should().Be(expected);
        }

        [Theory]
        [InlineData(1234.56, "1.234,56")]
        [InlineData(0.0, "0,00")]
        [InlineData(-500.50, "-500,50")]
        [InlineData(1000000.123, "1.000.000,12")]
        [InlineData(99.999, "100,00")]
        public void FormatDecimal_ShouldFormatCorrectly(decimal value, string expected)
        {
            // Act
            var result = BrazilianFormatter.FormatDecimal(value);

            // Assert
            result.Should().Be(expected);
        }

        [Theory]
        [InlineData(0.1234, "12,34%")]
        [InlineData(1.0, "100,00%")]
        [InlineData(0.5, "50,00%")]
        [InlineData(0.005, "0,50%")]
        [InlineData(2.5, "250,00%")]
        public void FormatPercentage_ShouldFormatCorrectly(decimal value, string expected)
        {
            // Act
            var result = BrazilianFormatter.FormatPercentage(value);

            // Assert
            result.Should().Be(expected);
        }

        [Fact]
        public void TranslateMonth_ShouldReturnPortugueseMonth()
        {
            // Arrange & Act & Assert
            BrazilianFormatter.TranslateMonth("January").Should().Be("Janeiro");
            BrazilianFormatter.TranslateMonth("February").Should().Be("Fevereiro");
            BrazilianFormatter.TranslateMonth("March").Should().Be("Março");
            BrazilianFormatter.TranslateMonth("April").Should().Be("Abril");
            BrazilianFormatter.TranslateMonth("May").Should().Be("Maio");
            BrazilianFormatter.TranslateMonth("June").Should().Be("Junho");
            BrazilianFormatter.TranslateMonth("July").Should().Be("Julho");
            BrazilianFormatter.TranslateMonth("August").Should().Be("Agosto");
            BrazilianFormatter.TranslateMonth("September").Should().Be("Setembro");
            BrazilianFormatter.TranslateMonth("October").Should().Be("Outubro");
            BrazilianFormatter.TranslateMonth("November").Should().Be("Novembro");
            BrazilianFormatter.TranslateMonth("December").Should().Be("Dezembro");
        }

        [Fact]
        public void TranslateDayOfWeek_ShouldReturnPortugueseDayOfWeek()
        {
            // Arrange & Act & Assert
            BrazilianFormatter.TranslateDayOfWeek(DayOfWeek.Sunday).Should().Be("Domingo");
            BrazilianFormatter.TranslateDayOfWeek(DayOfWeek.Monday).Should().Be("Segunda-feira");
            BrazilianFormatter.TranslateDayOfWeek(DayOfWeek.Tuesday).Should().Be("Terça-feira");
            BrazilianFormatter.TranslateDayOfWeek(DayOfWeek.Wednesday).Should().Be("Quarta-feira");
            BrazilianFormatter.TranslateDayOfWeek(DayOfWeek.Thursday).Should().Be("Quinta-feira");
            BrazilianFormatter.TranslateDayOfWeek(DayOfWeek.Friday).Should().Be("Sexta-feira");
            BrazilianFormatter.TranslateDayOfWeek(DayOfWeek.Saturday).Should().Be("Sábado");
        }
    }
}