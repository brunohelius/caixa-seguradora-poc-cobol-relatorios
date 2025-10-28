using System;
using System.Globalization;
using System.Text;

namespace PdfGenerator.Formatting
{
    /// <summary>
    /// Utility class for Brazilian Portuguese formatting
    /// </summary>
    public static class BrazilianFormatter
    {
        private static readonly CultureInfo BrazilianCulture = new CultureInfo("pt-BR");

        /// <summary>
        /// Format currency in Brazilian Real (R$)
        /// </summary>
        public static string FormatCurrency(decimal value)
        {
            return value.ToString("C", BrazilianCulture);
        }

        /// <summary>
        /// Format date in Brazilian format (DD/MM/YYYY)
        /// </summary>
        public static string FormatDate(DateTime date)
        {
            return date.ToString("dd/MM/yyyy", BrazilianCulture);
        }

        /// <summary>
        /// Format date with time in Brazilian format
        /// </summary>
        public static string FormatDateTime(DateTime date)
        {
            return date.ToString("dd/MM/yyyy HH:mm:ss", BrazilianCulture);
        }

        /// <summary>
        /// Format month and year in Brazilian format
        /// </summary>
        public static string FormatMonthYear(DateTime date)
        {
            return date.ToString("MMMM/yyyy", BrazilianCulture);
        }

        /// <summary>
        /// Format number with thousands separator
        /// </summary>
        public static string FormatNumber(int value)
        {
            return value.ToString("N0", BrazilianCulture);
        }

        /// <summary>
        /// Format decimal number with specific decimal places
        /// </summary>
        public static string FormatDecimal(decimal value, int decimalPlaces = 2)
        {
            return value.ToString($"N{decimalPlaces}", BrazilianCulture);
        }

        /// <summary>
        /// Format percentage
        /// </summary>
        public static string FormatPercentage(double value, int decimalPlaces = 1)
        {
            return value.ToString($"P{decimalPlaces}", BrazilianCulture);
        }

        /// <summary>
        /// Format file size in human-readable format
        /// </summary>
        public static string FormatFileSize(long bytes)
        {
            string[] sizes = { "B", "KB", "MB", "GB", "TB" };
            double len = bytes;
            int order = 0;

            while (len >= 1024 && order < sizes.Length - 1)
            {
                order++;
                len = len / 1024;
            }

            return $"{len:0.##} {sizes[order]}";
        }

        /// <summary>
        /// Format duration in Portuguese
        /// </summary>
        public static string FormatDuration(TimeSpan duration)
        {
            var parts = new List<string>();

            if (duration.Days > 0)
            {
                parts.Add($"{duration.Days} {(duration.Days == 1 ? "dia" : "dias")}");
            }

            if (duration.Hours > 0)
            {
                parts.Add($"{duration.Hours} {(duration.Hours == 1 ? "hora" : "horas")}");
            }

            if (duration.Minutes > 0)
            {
                parts.Add($"{duration.Minutes} {(duration.Minutes == 1 ? "minuto" : "minutos")}");
            }

            if (duration.Seconds > 0 || parts.Count == 0)
            {
                parts.Add($"{duration.Seconds} {(duration.Seconds == 1 ? "segundo" : "segundos")}");
            }

            return string.Join(", ", parts);
        }

        /// <summary>
        /// Convert number to words in Portuguese
        /// </summary>
        public static string NumberToWords(int number)
        {
            if (number == 0)
                return "zero";

            if (number < 0)
                return "menos " + NumberToWords(Math.Abs(number));

            string words = "";

            if ((number / 1000000) > 0)
            {
                words += NumberToWords(number / 1000000) + " milhão ";
                number %= 1000000;
            }

            if ((number / 1000) > 0)
            {
                words += NumberToWords(number / 1000) + " mil ";
                number %= 1000;
            }

            if ((number / 100) > 0)
            {
                words += GetHundreds(number / 100) + " ";
                number %= 100;
            }

            if (number > 0)
            {
                if (number < 20)
                    words += GetUnits(number);
                else
                {
                    words += GetTens(number / 10);
                    if ((number % 10) > 0)
                        words += " e " + GetUnits(number % 10);
                }
            }

            return words.Trim();
        }

        private static string GetUnits(int number)
        {
            return number switch
            {
                1 => "um",
                2 => "dois",
                3 => "três",
                4 => "quatro",
                5 => "cinco",
                6 => "seis",
                7 => "sete",
                8 => "oito",
                9 => "nove",
                10 => "dez",
                11 => "onze",
                12 => "doze",
                13 => "treze",
                14 => "quatorze",
                15 => "quinze",
                16 => "dezesseis",
                17 => "dezessete",
                18 => "dezoito",
                19 => "dezenove",
                _ => ""
            };
        }

        private static string GetTens(int number)
        {
            return number switch
            {
                2 => "vinte",
                3 => "trinta",
                4 => "quarenta",
                5 => "cinquenta",
                6 => "sessenta",
                7 => "setenta",
                8 => "oitenta",
                9 => "noventa",
                _ => ""
            };
        }

        private static string GetHundreds(int number)
        {
            return number switch
            {
                1 => "cem",
                2 => "duzentos",
                3 => "trezentos",
                4 => "quatrocentos",
                5 => "quinhentos",
                6 => "seiscentos",
                7 => "setecentos",
                8 => "oitocentos",
                9 => "novecentos",
                _ => ""
            };
        }

        /// <summary>
        /// Capitalize first letter of each word (Title Case in Portuguese)
        /// </summary>
        public static string ToTitleCase(string text)
        {
            if (string.IsNullOrEmpty(text))
                return text;

            var textInfo = BrazilianCulture.TextInfo;
            return textInfo.ToTitleCase(text.ToLower());
        }

        /// <summary>
        /// Format CPF (Brazilian tax ID)
        /// </summary>
        public static string FormatCPF(string cpf)
        {
            if (string.IsNullOrEmpty(cpf))
                return cpf;

            cpf = cpf.Replace(".", "").Replace("-", "");

            if (cpf.Length != 11)
                return cpf;

            return $"{cpf.Substring(0, 3)}.{cpf.Substring(3, 3)}.{cpf.Substring(6, 3)}-{cpf.Substring(9, 2)}";
        }

        /// <summary>
        /// Format CNPJ (Brazilian company tax ID)
        /// </summary>
        public static string FormatCNPJ(string cnpj)
        {
            if (string.IsNullOrEmpty(cnpj))
                return cnpj;

            cnpj = cnpj.Replace(".", "").Replace("-", "").Replace("/", "");

            if (cnpj.Length != 14)
                return cnpj;

            return $"{cnpj.Substring(0, 2)}.{cnpj.Substring(2, 3)}.{cnpj.Substring(5, 3)}/{cnpj.Substring(8, 4)}-{cnpj.Substring(12, 2)}";
        }
    }
}