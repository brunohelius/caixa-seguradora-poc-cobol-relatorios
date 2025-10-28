using QuestPDF.Fluent;
using QuestPDF.Infrastructure;
using QuestPDF.Helpers;
using PdfGenerator.PdfGeneration.Formatting;

namespace PdfGenerator.PdfGeneration.Sections;

/// <summary>
/// Migration Architecture section showing .NET 9 + React architecture
/// </summary>
public class MigrationArchitectureSection : IPdfSection
{
    public string SectionId => "migration-architecture";
    public string Title => "Arquitetura de Migração";
    public int Order => 4;
    public bool IncludeInToc => true;

    public void Render(IContainer container, SectionContext context)
    {
        container.Column(column =>
        {
            column.Item().PageBreak();

            // Section title
            column.Item()
                .Text("3. ARQUITETURA DE MIGRAÇÃO")
                .FontColor(BrandingStyles.PrimaryBlue)
                .Bold()
                .FontSize(18);

            column.Item().Height(10, Unit.Millimetre);

            // Architecture overview
            column.Item()
                .Text("3.1 Clean Architecture")
                .FontColor(BrandingStyles.TextDark)
                .Bold()
                .FontSize(14);

            column.Item().Height(5, Unit.Millimetre);

            // Layer diagram
            column.Item()
                .Border(1)
                .BorderColor(BrandingStyles.BorderGray)
                .Padding(15)
                .Column(layerColumn =>
                {
                    // API Layer
                    RenderLayer(layerColumn, "API Layer", BrandingStyles.PrimaryBlue,
                        "CaixaSeguradora.Api",
                        new[] { "Controllers", "Middleware", "Filters", "Program.cs" },
                        "ASP.NET Core Web API, Swagger, Authentication");

                    layerColumn.Item().Height(10, Unit.Millimetre);

                    // Core Layer
                    RenderLayer(layerColumn, "Core Layer", BrandingStyles.AccentYellow,
                        "CaixaSeguradora.Core",
                        new[] { "Entities", "Interfaces", "Services", "DTOs" },
                        "Business Logic, Domain Models, Zero Dependencies");

                    layerColumn.Item().Height(10, Unit.Millimetre);

                    // Infrastructure Layer
                    RenderLayer(layerColumn, "Infrastructure Layer", Colors.Green.Medium,
                        "CaixaSeguradora.Infrastructure",
                        new[] { "Data/DbContext", "Repositories", "External Services", "Formatters" },
                        "EF Core, File I/O, Azure Services, COBOL Formatting");
                });

            column.Item().Height(10, Unit.Millimetre);

            // Technology stack
            column.Item()
                .Text("3.2 Stack Tecnológica")
                .FontColor(BrandingStyles.TextDark)
                .Bold()
                .FontSize(14);

            column.Item().Height(5, Unit.Millimetre);

            column.Item().Row(row =>
            {
                // Backend stack
                row.RelativeItem().Column(backendCol =>
                {
                    backendCol.Item()
                        .Background(Colors.Blue.Lighten5)
                        .Padding(10)
                        .Text("Backend (.NET 9)")
                        .FontColor(BrandingStyles.PrimaryBlue)
                        .Bold()
                        .FontSize(12);

                    backendCol.Item().Height(5, Unit.Millimetre);

                    var backendTech = new[]
                    {
                        ("Framework", ".NET 9.0 LTS"),
                        ("Language", "C# 13"),
                        ("API", "ASP.NET Core"),
                        ("ORM", "Entity Framework Core 9"),
                        ("Database", "Azure SQL / SQLite"),
                        ("Logging", "Serilog"),
                        ("Testing", "xUnit + Moq"),
                        ("Documentation", "Swagger/OpenAPI")
                    };

                    foreach (var (tech, value) in backendTech)
                    {
                        backendCol.Item().Row(techRow =>
                        {
                            techRow.ConstantItem(100, Unit.Point)
                                .Text(tech)
                                .FontColor(BrandingStyles.TextMedium)
                                .FontSize(9);
                            techRow.RelativeItem()
                                .Text(value)
                                .FontColor(BrandingStyles.TextDark)
                                .Bold()
                                .FontSize(9);
                        });
                        backendCol.Item().Height(2, Unit.Millimetre);
                    }
                });

                row.ConstantItem(10, Unit.Point);

                // Frontend stack
                row.RelativeItem().Column(frontendCol =>
                {
                    frontendCol.Item()
                        .Background(Colors.Green.Lighten5)
                        .Padding(10)
                        .Text("Frontend (React)")
                        .FontColor(Colors.Green.Darken2)
                        .Bold()
                        .FontSize(12);

                    frontendCol.Item().Height(5, Unit.Millimetre);

                    var frontendTech = new[]
                    {
                        ("Framework", "React 18.2"),
                        ("Language", "TypeScript 5.3"),
                        ("Styling", "TailwindCSS 3.4"),
                        ("State", "React Context"),
                        ("Router", "React Router 6"),
                        ("HTTP", "Axios"),
                        ("Charts", "Recharts"),
                        ("Build", "Vite 5.0")
                    };

                    foreach (var (tech, value) in frontendTech)
                    {
                        frontendCol.Item().Row(techRow =>
                        {
                            techRow.ConstantItem(100, Unit.Point)
                                .Text(tech)
                                .FontColor(BrandingStyles.TextMedium)
                                .FontSize(9);
                            techRow.RelativeItem()
                                .Text(value)
                                .FontColor(BrandingStyles.TextDark)
                                .Bold()
                                .FontSize(9);
                        });
                        frontendCol.Item().Height(2, Unit.Millimetre);
                    }
                });
            });

            column.Item().Height(10, Unit.Millimetre);

            // API Endpoints
            column.Item()
                .Text("3.3 API Endpoints")
                .FontColor(BrandingStyles.TextDark)
                .Bold()
                .FontSize(14);

            column.Item().Height(5, Unit.Millimetre);

            // Endpoints table
            column.Item().Table(table =>
            {
                table.ColumnsDefinition(columns =>
                {
                    columns.ConstantColumn(80, Unit.Point);
                    columns.RelativeColumn(3);
                    columns.RelativeColumn(2);
                });

                table.Header(header =>
                {
                    header.Cell().Background(BrandingStyles.PrimaryBlue).Padding(5)
                        .Text("Categoria").FontColor(Colors.White).Bold().FontSize(10);
                    header.Cell().Background(BrandingStyles.PrimaryBlue).Padding(5)
                        .Text("Endpoint").FontColor(Colors.White).Bold().FontSize(10);
                    header.Cell().Background(BrandingStyles.PrimaryBlue).Padding(5)
                        .Text("Método").FontColor(Colors.White).Bold().FontSize(10);
                });

                var endpoints = context.Architecture.ApiEndpoints.Take(15);
                foreach (var endpoint in endpoints)
                {
                    table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(5)
                        .Text(endpoint.Category).FontColor(BrandingStyles.TextDark).FontSize(9);
                    table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(5)
                        .Text(endpoint.Path).FontColor(BrandingStyles.TextMedium).FontSize(9);
                    table.Cell().BorderBottom(1).BorderColor(Colors.Grey.Lighten3).Padding(5)
                        .Text(endpoint.Method).FontColor(GetMethodColor(endpoint.Method)).Bold().FontSize(9);
                }
            });

            column.Item().Height(10, Unit.Millimetre);

            // Data flow
            column.Item()
                .Text("3.4 Fluxo de Dados")
                .FontColor(BrandingStyles.TextDark)
                .Bold()
                .FontSize(14);

            column.Item().Height(5, Unit.Millimetre);

            column.Item()
                .Background(Colors.Grey.Lighten5)
                .Padding(10)
                .Text("DB2 (Mainframe) → ETL Process → Azure SQL → .NET API → React UI → User\n\n" +
                      "• Extração: Stored Procedures DB2 exportam dados para staging\n" +
                      "• Transformação: .NET Services aplicam regras de negócio\n" +
                      "• Carga: Entity Framework persiste em Azure SQL\n" +
                      "• API: Controllers expõem dados via REST/GraphQL\n" +
                      "• Frontend: React consome APIs e renderiza dashboards")
                .FontColor(BrandingStyles.TextMedium)
                .FontSize(10)
                .LineHeight(1.5f);
        });
    }

    private void RenderLayer(ColumnDescriptor column, string title, string color,
        string projectName, string[] components, string description)
    {
        column.Item()
            .Border(2)
            .BorderColor(color)
            .Column(layerCol =>
            {
                layerCol.Item()
                    .Background(color)
                    .Padding(8)
                    .Text(title)
                    .FontColor(Colors.White)
                    .Bold()
                    .FontSize(11);

                layerCol.Item()
                    .Padding(10)
                    .Column(contentCol =>
                    {
                        contentCol.Item()
                            .Text(projectName)
                            .FontColor(BrandingStyles.TextDark)
                            .Bold()
                            .FontSize(10);

                        contentCol.Item().Height(3, Unit.Millimetre);

                        contentCol.Item().Row(row =>
                        {
                            foreach (var component in components)
                            {
                                row.RelativeItem()
                                    .PaddingRight(5)
                                    .Text($"• {component}")
                                    .FontColor(BrandingStyles.TextMedium)
                                    .FontSize(9);
                            }
                        });

                        contentCol.Item().Height(3, Unit.Millimetre);

                        contentCol.Item()
                            .Text(description)
                            .FontColor(BrandingStyles.TextLight)
                            .Italic()
                            .FontSize(9);
                    });
            });
    }

    private string GetMethodColor(string method) => method switch
    {
        "GET" => Colors.Green.Medium,
        "POST" => Colors.Blue.Medium,
        "PUT" => Colors.Orange.Medium,
        "DELETE" => Colors.Red.Medium,
        _ => BrandingStyles.TextMedium
    };
}