using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Design;
using Microsoft.Extensions.Configuration;
using CaixaSeguradora.Infrastructure.Data;
using CaixaSeguradora.Infrastructure.Services;
using CaixaSeguradora.Infrastructure.Repositories;
using CaixaSeguradora.Core.Interfaces;
using CaixaSeguradora.Core.Services;
using CaixaSeguradora.Api.Middleware;
using Serilog;
using Serilog.Events;

// Configure Serilog for structured logging
Log.Logger = new LoggerConfiguration()
    .MinimumLevel.Information()
    .MinimumLevel.Override("Microsoft", LogEventLevel.Warning)
    .MinimumLevel.Override("Microsoft.EntityFrameworkCore", LogEventLevel.Warning)
    .Enrich.FromLogContext()
    .WriteTo.Console(
        outputTemplate: "[{Timestamp:HH:mm:ss} {Level:u3}] {Message:lj}{NewLine}{Exception}")
    .WriteTo.File(
        path: "logs/caixa-seguradora-.log",
        rollingInterval: RollingInterval.Day,
        outputTemplate: "{Timestamp:yyyy-MM-dd HH:mm:ss.fff zzz} [{Level:u3}] {Message:lj}{NewLine}{Exception}",
        retainedFileCountLimit: 30)
    .CreateLogger();

try
{
    Log.Information("Starting Caixa Seguradora Premium Reporting API");

    var builder = WebApplication.CreateBuilder(args);

    // Use Serilog for logging
    builder.Host.UseSerilog();

    // Add services to the container
    builder.Services.AddControllers()
        .AddJsonOptions(options =>
        {
            // Prevent circular reference errors in JSON serialization
            options.JsonSerializerOptions.ReferenceHandler = System.Text.Json.Serialization.ReferenceHandler.IgnoreCycles;
            // Make JSON more readable in development
            options.JsonSerializerOptions.WriteIndented = builder.Environment.IsDevelopment();
        });
    builder.Services.AddEndpointsApiExplorer();
    builder.Services.AddSwaggerGen(options =>
    {
        options.SwaggerDoc("v1", new Microsoft.OpenApi.Models.OpenApiInfo
        {
            Title = "Caixa Seguradora - Premium Reporting API",
            Version = "v1",
            Description = "API para geração de relatórios PREMIT/PREMCED - Migração COBOL para .NET",
            Contact = new Microsoft.OpenApi.Models.OpenApiContact
            {
                Name = "Caixa Seguradora",
                Email = "suporte@caixaseguradora.com.br"
            }
        });
    });

    // Configure DbContext with SQLite
    builder.Services.AddDbContext<PremiumReportingDbContext>(options =>
    {
        options.UseSqlite(builder.Configuration.GetConnectionString("DefaultConnection"));
        // Enable sensitive data logging only in development
        if (builder.Environment.IsDevelopment())
        {
            options.EnableSensitiveDataLogging();
            options.EnableDetailedErrors();
        }
    });

    // Add EF Core design-time services for migrations
    builder.Services.AddDbContextFactory<PremiumReportingDbContext>(options =>
    {
        options.UseSqlite(builder.Configuration.GetConnectionString("DefaultConnection"));
    });

    // Configure CORS
    var allowedOrigins = builder.Configuration.GetSection("Cors:AllowedOrigins").Get<string[]>() ?? new[] { "http://localhost:5173" };
    builder.Services.AddCors(options =>
    {
        options.AddDefaultPolicy(policy =>
        {
            policy.WithOrigins(allowedOrigins)
                  .AllowAnyHeader()
                  .AllowAnyMethod()
                  .AllowCredentials();
        });
    });

    // Register repository services (will be used by specific repositories later)
    // builder.Services.AddScoped(typeof(IRepository<>), typeof(Repository<>));

    // Register repositories (User Story 2)
    builder.Services.AddScoped<IPremiumRepository, PremiumRepository>();
    builder.Services.AddScoped<IPolicyRepository, PolicyRepository>();
    builder.Services.AddScoped<IProductRepository, ProductRepository>();
    builder.Services.AddScoped<IEndorsementRepository, EndorsementRepository>();
    builder.Services.AddScoped<ICoverageRepository, CoverageRepository>();
    builder.Services.AddScoped<IClientRepository, ClientRepository>();
    builder.Services.AddScoped<IAddressRepository, AddressRepository>();
    builder.Services.AddScoped<ICossuredPolicyRepository, CossuredPolicyRepository>();
    builder.Services.AddScoped<ICossuranceCalculationRepository, CossuranceCalculationRepository>();

    // Register business logic services (User Story 2)
    builder.Services.AddScoped<IPremiumCalculationService, PremiumCalculationService>();
    builder.Services.AddScoped<ICossuranceService, CossuranceService>();
    builder.Services.AddScoped<IExternalModuleService, ExternalModuleService>();

    // Register report generation service (User Story 2)
    builder.Services.AddScoped<IReportGenerationService, ReportGenerationService>();

    // Register dashboard service (User Story 1)
    builder.Services.AddScoped<IDashboardService, DashboardService>();

    // Register query services (User Story 3)
    builder.Services.AddScoped<IPremiumQueryService, PremiumQueryService>();
    builder.Services.AddScoped<IPolicyQueryService, PolicyQueryService>();

    // Register batch job services (Phase 6)
    builder.Services.AddScoped<IBatchJobRepository, BatchJobRepository>();
    builder.Services.AddScoped<IBackgroundJobService, BackgroundJobService>();

    // Register mock data services (User Story 5)
    builder.Services.AddScoped<ICsvParserService, CsvParserService>();
    builder.Services.AddScoped<IDataValidationService, DataValidationService>();

    var app = builder.Build();

    // Configure the HTTP request pipeline

    // Global exception handler MUST be first in the pipeline
    app.UseGlobalExceptionHandler();

    // Serilog request logging
    app.UseSerilogRequestLogging(options =>
    {
        options.MessageTemplate = "HTTP {RequestMethod} {RequestPath} responded {StatusCode} in {Elapsed:0.0000} ms";
        options.EnrichDiagnosticContext = (diagnosticContext, httpContext) =>
        {
            diagnosticContext.Set("RequestHost", httpContext.Request.Host.Value);
            diagnosticContext.Set("RequestScheme", httpContext.Request.Scheme);
            diagnosticContext.Set("UserAgent", httpContext.Request.Headers.UserAgent.ToString());
            diagnosticContext.Set("ClientIP", httpContext.Connection.RemoteIpAddress?.ToString());
        };
    });

    if (app.Environment.IsDevelopment())
    {
        app.UseSwagger();
        app.UseSwaggerUI(options =>
        {
            options.SwaggerEndpoint("/swagger/v1/swagger.json", "Premium Reporting API v1");
            options.RoutePrefix = "swagger";
            options.DocumentTitle = "Caixa Seguradora - API Documentation";
        });
    }

    app.UseHttpsRedirection();
    app.UseCors();
    app.UseAuthorization();
    app.MapControllers();

    Log.Information("Application configured successfully, starting web server");
    app.Run();
}
catch (Exception ex)
{
    Log.Fatal(ex, "Application terminated unexpectedly");
}
finally
{
    Log.CloseAndFlush();
}
