using QuestPDF.Fluent;
using QuestPDF.Infrastructure;
using QuestPDF.Helpers;
using PdfGenerator.PdfGeneration.Formatting;

namespace PdfGenerator.PdfGeneration.Sections;

/// <summary>
/// Component Specifications section listing React components
/// </summary>
public class ComponentSpecsSection : IPdfSection
{
    public string SectionId => "component-specs";
    public string Title => "Especificações de Componentes";
    public int Order => 5;
    public bool IncludeInToc => true;

    public void Render(IContainer container, SectionContext context)
    {
        container.Column(column =>
        {
            column.Item().PageBreak();

            // Section title
            column.Item()
                .Text("4. ESPECIFICAÇÕES DE COMPONENTES")
                .FontColor(BrandingStyles.PrimaryBlue)
                .Bold()
                .FontSize(18);

            column.Item().Height(10, Unit.Millimetre);

            // Component overview
            column.Item()
                .Text("4.1 Componentes React")
                .FontColor(BrandingStyles.TextDark)
                .Bold()
                .FontSize(14);

            column.Item().Height(5, Unit.Millimetre);

            column.Item()
                .Text("Arquitetura baseada em componentes funcionais com TypeScript, " +
                      "seguindo padrões de composição e reutilização.")
                .FontColor(BrandingStyles.TextMedium)
                .FontSize(11);

            column.Item().Height(8, Unit.Millimetre);

            // Pages components
            column.Item()
                .Text("4.2 Componentes de Página")
                .FontColor(BrandingStyles.TextDark)
                .Bold()
                .FontSize(12);

            column.Item().Height(5, Unit.Millimetre);

            var pages = context.Architecture.ReactComponents
                .Where(c => c.Category == "Pages")
                .Take(10);

            foreach (var component in pages)
            {
                RenderComponent(column, component);
            }

            column.Item().Height(10, Unit.Millimetre);

            // Dashboard components
            column.Item()
                .Text("4.3 Componentes de Dashboard")
                .FontColor(BrandingStyles.TextDark)
                .Bold()
                .FontSize(12);

            column.Item().Height(5, Unit.Millimetre);

            var dashboardComponents = context.Architecture.ReactComponents
                .Where(c => c.Category == "Dashboard")
                .Take(10);

            foreach (var component in dashboardComponents)
            {
                RenderComponent(column, component);
            }

            column.Item().Height(10, Unit.Millimetre);

            // Common components
            column.Item()
                .Text("4.4 Componentes Comuns")
                .FontColor(BrandingStyles.TextDark)
                .Bold()
                .FontSize(12);

            column.Item().Height(5, Unit.Millimetre);

            column.Item().Table(table =>
            {
                table.ColumnsDefinition(columns =>
                {
                    columns.RelativeColumn(2);
                    columns.RelativeColumn(3);
                    columns.RelativeColumn(2);
                });

                table.Header(header =>
                {
                    header.Cell().Background(BrandingStyles.AccentYellow).Padding(5)
                        .Text("Componente").FontColor(BrandingStyles.TextDark).Bold().FontSize(10);
                    header.Cell().Background(BrandingStyles.AccentYellow).Padding(5)
                        .Text("Props").FontColor(BrandingStyles.TextDark).Bold().FontSize(10);
                    header.Cell().Background(BrandingStyles.AccentYellow).Padding(5)
                        .Text("Uso").FontColor(BrandingStyles.TextDark).Bold().FontSize(10);
                });

                var commonComponents = new[]
                {
                    ("Button", "onClick, variant, size, disabled", "Ações primárias"),
                    ("Card", "title, children, footer", "Containers de conteúdo"),
                    ("Table", "data, columns, onSort, pagination", "Listagem de dados"),
                    ("Input", "value, onChange, type, validation", "Entrada de dados"),
                    ("Select", "options, value, onChange, multi", "Seleção de opções"),
                    ("Modal", "isOpen, onClose, title, children", "Diálogos modais"),
                    ("Alert", "type, message, dismissible", "Mensagens de feedback"),
                    ("Spinner", "size, color, overlay", "Indicador de loading"),
                    ("Badge", "variant, children", "Status e contadores"),
                    ("Breadcrumb", "items, separator", "Navegação hierárquica")
                };

                foreach (var (name, props, usage) in commonComponents)
                {
                    table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(5)
                        .Text(name).FontColor(BrandingStyles.TextDark).Bold().FontSize(9);
                    table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(5)
                        .Text(props).FontColor(BrandingStyles.TextMedium).FontSize(9);
                    table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(5)
                        .Text(usage).FontColor(BrandingStyles.TextLight).FontSize(9);
                }
            });

            column.Item().Height(10, Unit.Millimetre);

            // State management
            column.Item()
                .Text("4.5 Gerenciamento de Estado")
                .FontColor(BrandingStyles.TextDark)
                .Bold()
                .FontSize(12);

            column.Item().Height(5, Unit.Millimetre);

            column.Item()
                .Background(Colors.Blue.Lighten5)
                .Padding(10)
                .Column(stateCol =>
                {
                    stateCol.Item().Text("React Context API + Custom Hooks")
                        .FontColor(BrandingStyles.PrimaryBlue).Bold().FontSize(11);

                    stateCol.Item().Height(3, Unit.Millimetre);

                    var contexts = new[]
                    {
                        "AuthContext - Autenticação e autorização",
                        "PremiumContext - Dados de prêmios e cálculos",
                        "ReportContext - Geração e histórico de relatórios",
                        "ThemeContext - Preferências visuais do usuário"
                    };

                    foreach (var ctx in contexts)
                    {
                        stateCol.Item().Text($"• {ctx}")
                            .FontColor(BrandingStyles.TextMedium).FontSize(10);
                        stateCol.Item().Height(2, Unit.Millimetre);
                    }
                });
        });
    }

    private void RenderComponent(ColumnDescriptor column, Models.ReactComponent component)
    {
        column.Item()
            .Border(1)
            .BorderColor(BrandingStyles.BorderGray)
            .Padding(8)
            .Row(row =>
            {
                row.ConstantItem(150, Unit.Point).Column(nameCol =>
                {
                    nameCol.Item()
                        .Text(component.Name)
                        .FontColor(BrandingStyles.PrimaryBlue)
                        .Bold()
                        .FontSize(10);

                    nameCol.Item()
                        .Text($"Path: {component.FilePath}")
                        .FontColor(BrandingStyles.TextLight)
                        .FontSize(8);
                });

                row.RelativeItem().Column(propsCol =>
                {
                    propsCol.Item()
                        .Text("Props:")
                        .FontColor(BrandingStyles.TextMedium)
                        .FontSize(9);

                    propsCol.Item()
                        .Text(string.Join(", ", component.Props))
                        .FontColor(BrandingStyles.TextDark)
                        .FontSize(9);
                });

                row.ConstantItem(100, Unit.Point)
                    .AlignRight()
                    .Text($"Lines: {component.LinesOfCode}")
                    .FontColor(BrandingStyles.TextLight)
                    .FontSize(9);
            });

        column.Item().Height(5, Unit.Millimetre);
    }
}