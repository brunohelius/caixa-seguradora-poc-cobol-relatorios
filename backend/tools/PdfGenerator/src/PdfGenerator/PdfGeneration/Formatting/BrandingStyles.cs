using QuestPDF.Helpers;

namespace PdfGenerator.PdfGeneration.Formatting
{
    /// <summary>
    /// Defines Caixa Seguradora branding styles for PDF generation.
    /// </summary>
    public class BrandingStyles
    {
        // Caixa Seguradora brand colors
        public string PrimaryColor { get; } = "#0047BB";      // Caixa Blue
        public string SecondaryColor { get; } = "#FFB81C";    // Caixa Yellow
        public string DarkBlue { get; } = "#003D7A";
        public string DarkYellow { get; } = "#FF9500";
        public string LightBlue { get; } = "#4B7BCD";
        public string LightYellow { get; } = "#FFC94D";

        // Text colors
        public string TextColor { get; } = "#333333";
        public string TextDark { get; } = "#222222";
        public string TextMedium { get; } = "#555555";
        public string SecondaryTextColor { get; } = "#666666";
        public string LightTextColor { get; } = "#999999";

        // Background colors
        public string HeaderBackground { get; } = "#F5F5F5";
        public string AlternateRowBackground { get; } = "#F9F9F9";
        public string HighlightBackground { get; } = "#FFF9E6";

        // Status colors
        public string SuccessColor { get; } = "#28A745";
        public string WarningColor { get; } = "#FFC107";
        public string ErrorColor { get; } = "#DC3545";
        public string InfoColor { get; } = "#17A2B8";

        // Font settings
        public string PrimaryFont { get; } = "Arial";
        public string SecondaryFont { get; } = "Helvetica";
        public string MonospaceFont { get; } = "Courier New";

        // Font sizes (in points)
        public float TitleFontSize { get; } = 24f;
        public float HeadingFontSize { get; } = 18f;
        public float SubheadingFontSize { get; } = 14f;
        public float BodyFontSize { get; } = 11f;
        public float SmallFontSize { get; } = 9f;
        public float FootnoteFontSize { get; } = 8f;

        // Spacing and margins (in points)
        public float DefaultMargin { get; } = 20f;
        public float SectionMargin { get; } = 30f;
        public float ParagraphSpacing { get; } = 10f;
        public float LineSpacing { get; } = 1.5f;

        // Table styles
        public float TableHeaderHeight { get; } = 30f;
        public float TableRowHeight { get; } = 25f;
        public float TableBorderWidth { get; } = 0.5f;
        public string TableBorderColor { get; } = "#DDDDDD";
        public string TableHeaderBackground { get; } = "#0047BB";
        public string TableHeaderTextColor { get; } = "#FFFFFF";

        // Chart styles
        public float ChartTitleFontSize { get; } = 14f;
        public float ChartLabelFontSize { get; } = 10f;
        public float ChartLegendFontSize { get; } = 9f;

        /// <summary>
        /// Get color palette for charts (cycling through brand colors).
        /// </summary>
        public string[] GetChartColorPalette()
        {
            return new[]
            {
                PrimaryColor,
                SecondaryColor,
                DarkBlue,
                DarkYellow,
                LightBlue,
                LightYellow,
                "#002654", // Darker blue
                "#CC7700"  // Darker yellow
            };
        }

        /// <summary>
        /// Get status color based on percentage completion.
        /// </summary>
        public string GetStatusColor(decimal percentage)
        {
            return percentage switch
            {
                >= 100 => SuccessColor,
                >= 75 => PrimaryColor,
                >= 50 => WarningColor,
                >= 25 => DarkYellow,
                _ => ErrorColor
            };
        }

        /// <summary>
        /// Get table row background color (alternating).
        /// </summary>
        public string GetRowBackground(int rowIndex)
        {
            return rowIndex % 2 == 0 ? Colors.White : AlternateRowBackground;
        }

        /// <summary>
        /// Apply heading style configuration.
        /// </summary>
        public void ApplyHeadingStyle(dynamic text, int level = 1)
        {
            var fontSize = level switch
            {
                1 => TitleFontSize,
                2 => HeadingFontSize,
                3 => SubheadingFontSize,
                _ => BodyFontSize
            };

            text.FontSize(fontSize)
                .Bold()
                .FontColor(PrimaryColor);
        }

        /// <summary>
        /// Apply body text style configuration.
        /// </summary>
        public void ApplyBodyStyle(dynamic text)
        {
            text.FontSize(BodyFontSize)
                .FontColor(TextColor)
                .LineHeight(LineSpacing);
        }

        /// <summary>
        /// Apply code/monospace style configuration.
        /// </summary>
        public void ApplyCodeStyle(dynamic text)
        {
            text.FontFamily(MonospaceFont)
                .FontSize(SmallFontSize)
                .FontColor(DarkBlue)
                .BackgroundColor("#F5F5F5");
        }

        /// <summary>
        /// Apply footer style configuration.
        /// </summary>
        public void ApplyFooterStyle(dynamic text)
        {
            text.FontSize(FootnoteFontSize)
                .FontColor(SecondaryTextColor);
        }
    }
}