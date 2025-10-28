using System;
using System.Collections.Generic;

namespace PdfGenerator.Models
{
    /// <summary>
    /// Architecture specification for the migration project
    /// </summary>
    public class MigrationArchitecture
    {
        public string ArchitecturePattern { get; set; } = "Clean Architecture";
        public string DesignPrinciple { get; set; } = "Domain-Driven Design (DDD)";

        // Architecture layers
        public List<ArchitectureLayer> Layers { get; set; } = new List<ArchitectureLayer>
        {
            new ArchitectureLayer
            {
                Name = "CaixaSeguradora.Api",
                Type = "Presentation Layer",
                Technologies = new List<string> { "ASP.NET Core 9", "Swagger", "CORS" },
                Responsibilities = new List<string>
                {
                    "HTTP endpoints",
                    "Request validation",
                    "Response formatting",
                    "Authentication/Authorization"
                },
                Dependencies = new List<string> { "CaixaSeguradora.Core" }
            },
            new ArchitectureLayer
            {
                Name = "CaixaSeguradora.Core",
                Type = "Domain Layer",
                Technologies = new List<string> { "C# 13", ".NET 9" },
                Responsibilities = new List<string>
                {
                    "Business entities",
                    "Business rules",
                    "Domain services",
                    "Interfaces"
                },
                Dependencies = new List<string>() // No external dependencies
            },
            new ArchitectureLayer
            {
                Name = "CaixaSeguradora.Infrastructure",
                Type = "Infrastructure Layer",
                Technologies = new List<string> { "Entity Framework Core 9", "SQLite", "Serilog" },
                Responsibilities = new List<string>
                {
                    "Data access",
                    "External services",
                    "File I/O",
                    "Logging"
                },
                Dependencies = new List<string> { "CaixaSeguradora.Core" }
            }
        };

        // Frontend architecture
        public FrontendArchitecture Frontend { get; set; } = new FrontendArchitecture();

        // Infrastructure components
        public List<InfrastructureComponent> Infrastructure { get; set; } = new List<InfrastructureComponent>();

        // Technology stack
        public TechnologyStack TechStack { get; set; } = new TechnologyStack();

        // Non-functional requirements
        public NonFunctionalRequirements NFRs { get; set; } = new NonFunctionalRequirements();
    }

    public class ArchitectureLayer
    {
        public string Name { get; set; }
        public string Type { get; set; }
        public List<string> Technologies { get; set; } = new List<string>();
        public List<string> Responsibilities { get; set; } = new List<string>();
        public List<string> Dependencies { get; set; } = new List<string>();
        public int EstimatedClasses { get; set; }
        public int EstimatedMethods { get; set; }
    }

    public class FrontendArchitecture
    {
        public string Framework { get; set; } = "React 18";
        public string BuildTool { get; set; } = "Vite";
        public string StateManagement { get; set; } = "React Hooks + Context API";
        public string UILibrary { get; set; } = "TailwindCSS";
        public string ChartLibrary { get; set; } = "Recharts";
        public List<string> Components { get; set; } = new List<string>
        {
            "DashboardPage",
            "ReportGenerationPage",
            "QueryBuilderPage",
            "BatchJobsPage",
            "SettingsPage"
        };
        public int TotalComponents { get; set; } = 45;
        public int TotalPages { get; set; } = 8;
        public List<ReactComponent> ReactComponents { get; set; } = new List<ReactComponent>();
    }

    public class ReactComponent
    {
        public string Name { get; set; } = string.Empty;
        public string Type { get; set; } = string.Empty; // Page, Component, Hook
        public string Path { get; set; } = string.Empty;
        public string Description { get; set; } = string.Empty;
        public List<string> Props { get; set; } = new List<string>();
        public List<string> Dependencies { get; set; } = new List<string>();
        public int EstimatedLines { get; set; }
    }

    public class InfrastructureComponent
    {
        public string Name { get; set; }
        public string Type { get; set; }
        public string Purpose { get; set; }
        public string Technology { get; set; }
        public Dictionary<string, string> Configuration { get; set; } = new Dictionary<string, string>();
    }

    public class TechnologyStack
    {
        public Dictionary<string, string> Backend { get; set; } = new Dictionary<string, string>
        {
            { ".NET", "9.0" },
            { "C#", "13.0" },
            { "Entity Framework Core", "9.0" },
            { "ASP.NET Core", "9.0" },
            { "SQLite", "3.46" }
        };

        public Dictionary<string, string> Frontend { get; set; } = new Dictionary<string, string>
        {
            { "React", "18.3" },
            { "TypeScript", "5.5" },
            { "Vite", "5.3" },
            { "TailwindCSS", "3.4" },
            { "Axios", "1.7" }
        };

        public Dictionary<string, string> DevOps { get; set; } = new Dictionary<string, string>
        {
            { "Docker", "27.0" },
            { "Docker Compose", "2.29" },
            { "GitHub Actions", "Latest" },
            { "Vercel", "Latest" }
        };

        public Dictionary<string, string> Testing { get; set; } = new Dictionary<string, string>
        {
            { "xUnit", "2.9" },
            { "FluentAssertions", "6.12" },
            { "Moq", "4.20" },
            { "Vitest", "2.0" },
            { "Playwright", "1.45" }
        };
    }

    public class NonFunctionalRequirements
    {
        public string Performance { get; set; } = "< 2 segundos de resposta para consultas";
        public string Scalability { get; set; } = "Suportar 100 usuários simultâneos";
        public string Availability { get; set; } = "99.9% uptime";
        public string Security { get; set; } = "Autenticação JWT, HTTPS, CORS configurado";
        public string Compatibility { get; set; } = "Byte-for-byte com saída COBOL";
        public string Maintainability { get; set; } = "90% cobertura de testes";
    }
}