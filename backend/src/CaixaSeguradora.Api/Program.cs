using System.Text;
using Microsoft.AspNetCore.Authentication.JwtBearer;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Design;
using Microsoft.Extensions.Configuration;
using Microsoft.IdentityModel.Tokens;
using CaixaSeguradora.Infrastructure.Data;
using CaixaSeguradora.Infrastructure.Services;
using CaixaSeguradora.Infrastructure.Repositories;
using CaixaSeguradora.Core.Interfaces;
using CaixaSeguradora.Core.Services;
using CaixaSeguradora.Core.DTOs;
using CaixaSeguradora.Api.Middleware;
using CaixaSeguradora.Api.Validators;
using Serilog;
using Serilog.Events;
using Hangfire;
using Hangfire.Dashboard;
using FluentValidation;
using FluentValidation.AspNetCore;
using AspNetCoreRateLimit;

// Configure Serilog for structured logging (T194 - Phase 10)
Log.Logger = new LoggerConfiguration()
    .MinimumLevel.Information()
    .MinimumLevel.Override("Microsoft", LogEventLevel.Warning)
    .MinimumLevel.Override("Microsoft.EntityFrameworkCore", LogEventLevel.Warning)
    .Enrich.FromLogContext()
    .WriteTo.Console(
        outputTemplate: "[{Timestamp:HH:mm:ss} {Level:u3}] {CorrelationId} {Message:lj}{NewLine}{Exception}")
    .WriteTo.File(
        path: "logs/premiumreporting-.log",
        rollingInterval: RollingInterval.Day,
        outputTemplate: "{Timestamp:yyyy-MM-dd HH:mm:ss.fff zzz} [{Level:u3}] CorrelationId={CorrelationId} {Message:lj}{NewLine}{Exception}",
        retainedFileCountLimit: 7) // 7 days retention as per T194
    .CreateLogger();

try
{
    Log.Information("Starting Caixa Seguradora Premium Reporting API");

    WebApplicationBuilder builder = WebApplication.CreateBuilder(args);

    // Configure Kestrel for HTTP/HTTPS (User Story 6 - T231)
    builder.WebHost.ConfigureKestrel((context, serverOptions) =>
    {
        var config = context.Configuration;

        // Get ports from environment variables or configuration
        var httpPort = int.TryParse(config["BACKEND_HTTP_PORT"] ?? config["Ports:Http"], out var port)
            ? port
            : 5555; // Default to 5555 (avoiding macOS Control Center on 5000)

        var httpsPort = int.TryParse(config["BACKEND_HTTPS_PORT"] ?? config["Ports:Https"], out var sslPort)
            ? sslPort
            : 5556;

        var certPath = config["Certificate:Path"];
        var certPassword = config["Certificate:Password"];

        // Configure HTTP endpoint
        serverOptions.ListenAnyIP(httpPort);
        Log.Information("HTTP endpoint configured on port {HttpPort}", httpPort);

        // Configure HTTPS endpoint if certificate is provided
        if (!string.IsNullOrEmpty(certPath) && File.Exists(certPath))
        {
            serverOptions.ListenAnyIP(httpsPort, listenOptions =>
            {
                listenOptions.UseHttps(certPath, certPassword);
            });
            Log.Information("HTTPS endpoint configured on port {HttpsPort} using certificate: {CertPath}", httpsPort, certPath);
        }
        else
        {
            Log.Warning("HTTPS certificate not configured. Running HTTP only on port {HttpPort}.", httpPort);
        }
    });

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

    // Configure FluentValidation (User Story 6 - T229)
    builder.Services.AddFluentValidationAutoValidation(config =>
    {
        // Disable DataAnnotations validation (use FluentValidation only)
        config.DisableDataAnnotationsValidation = true;
    });

    // Register all validators from the Validators namespace
    builder.Services.AddValidatorsFromAssemblyContaining<LoginRequestValidator>();

    // Configure custom validation error response behavior
    builder.Services.Configure<Microsoft.AspNetCore.Mvc.ApiBehaviorOptions>(options =>
    {
        options.InvalidModelStateResponseFactory = context =>
        {
            var validationErrors = new ValidationErrorResponse
            {
                StatusCode = 400,
                Message = "Erro de validação nos dados fornecidos",
                Timestamp = DateTime.UtcNow
            };

            foreach (var modelError in context.ModelState)
            {
                var fieldName = modelError.Key;
                var errors = modelError.Value?.Errors;

                if (errors != null)
                {
                    foreach (var error in errors)
                    {
                        validationErrors.AddError(fieldName, error.ErrorMessage);
                    }
                }
            }

            Log.Warning(
                "Validation failed: {ErrorCount} errors in {FieldCount} fields",
                validationErrors.ErrorCount,
                validationErrors.Errors.Count);

            return new Microsoft.AspNetCore.Mvc.BadRequestObjectResult(validationErrors);
        };
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

        // Add JWT authentication to Swagger
        options.AddSecurityDefinition("Bearer", new Microsoft.OpenApi.Models.OpenApiSecurityScheme
        {
            Description = "JWT Authorization header usando o esquema Bearer. Exemplo: \"Authorization: Bearer {token}\"",
            Name = "Authorization",
            In = Microsoft.OpenApi.Models.ParameterLocation.Header,
            Type = Microsoft.OpenApi.Models.SecuritySchemeType.ApiKey,
            Scheme = "Bearer"
        });

        options.AddSecurityRequirement(new Microsoft.OpenApi.Models.OpenApiSecurityRequirement
        {
            {
                new Microsoft.OpenApi.Models.OpenApiSecurityScheme
                {
                    Reference = new Microsoft.OpenApi.Models.OpenApiReference
                    {
                        Type = Microsoft.OpenApi.Models.ReferenceType.SecurityScheme,
                        Id = "Bearer"
                    }
                },
                Array.Empty<string>()
            }
        });
    });

    // Configure JWT Authentication (User Story 6 - T228)
    var jwtSecretKey = builder.Configuration["Jwt:SecretKey"]
        ?? throw new InvalidOperationException("JWT SecretKey não configurada");
    var jwtIssuer = builder.Configuration["Jwt:Issuer"] ?? "CaixaSeguradora";
    var jwtAudience = builder.Configuration["Jwt:Audience"] ?? "CaixaSeguradora.API";

    builder.Services.AddAuthentication(options =>
    {
        options.DefaultAuthenticateScheme = JwtBearerDefaults.AuthenticationScheme;
        options.DefaultChallengeScheme = JwtBearerDefaults.AuthenticationScheme;
        options.DefaultScheme = JwtBearerDefaults.AuthenticationScheme;
    })
    .AddJwtBearer(options =>
    {
        options.SaveToken = true;
        options.RequireHttpsMetadata = !builder.Environment.IsDevelopment(); // Disable in dev for localhost
        options.TokenValidationParameters = new TokenValidationParameters
        {
            ValidateIssuer = true,
            ValidateAudience = true,
            ValidateLifetime = true,
            ValidateIssuerSigningKey = true,
            ValidIssuer = jwtIssuer,
            ValidAudience = jwtAudience,
            IssuerSigningKey = new SymmetricSecurityKey(Encoding.ASCII.GetBytes(jwtSecretKey)),
            ClockSkew = TimeSpan.Zero // No tolerance for expired tokens
        };

        // Detailed logging for authentication events
        options.Events = new JwtBearerEvents
        {
            OnAuthenticationFailed = context =>
            {
                Log.Warning(
                    "JWT authentication failed: {Exception}",
                    context.Exception.Message);
                return Task.CompletedTask;
            },
            OnTokenValidated = context =>
            {
                var username = context.Principal?.Identity?.Name;
                Log.Debug("JWT token validated successfully for user: {Username}", username);
                return Task.CompletedTask;
            }
        };
    });

    builder.Services.AddAuthorization();

    // Configure Rate Limiting (User Story 6 - T230)
    // Protege a API contra abuso mantendo disponibilidade para usuários legítimos
    builder.Services.AddMemoryCache();

    // Carrega configurações de rate limiting do appsettings.json
    builder.Services.Configure<IpRateLimitOptions>(builder.Configuration.GetSection("IpRateLimiting"));
    builder.Services.Configure<IpRateLimitPolicies>(builder.Configuration.GetSection("IpRateLimitPolicies"));

    // Injeta serviços de rate limiting
    builder.Services.AddSingleton<IIpPolicyStore, MemoryCacheIpPolicyStore>();
    builder.Services.AddSingleton<IRateLimitCounterStore, MemoryCacheRateLimitCounterStore>();
    builder.Services.AddSingleton<IRateLimitConfiguration, RateLimitConfiguration>();
    builder.Services.AddSingleton<IProcessingStrategy, AsyncKeyLockProcessingStrategy>();

    // Register SQL error translator (User Story 2 - T241)
    builder.Services.AddSingleton<SqlErrorTranslator>();

    // Register ReadOnlyDbCommandInterceptor (User Story 2 - T243)
    // Enabled only in production to enforce read-only database access (SC-007)
    var isReadOnlyEnabled = !builder.Environment.IsDevelopment() ||
                           builder.Configuration.GetValue<bool>("Database:EnforceReadOnly", false);
    builder.Services.AddSingleton(sp =>
        new ReadOnlyDbCommandInterceptor(
            sp.GetRequiredService<ILogger<ReadOnlyDbCommandInterceptor>>(),
            isReadOnlyEnabled));

    // Configure DbContext with SQLite and connection pooling (T155 - US5)
    builder.Services.AddDbContext<PremiumReportingDbContext>((sp, options) =>
    {
        // Configure SQLite connection string with pooling and performance optimizations
        var connectionString = builder.Configuration.GetConnectionString("DefaultConnection");

        // SQLite connection string builder for pooling configuration
        var sqliteConnectionStringBuilder = new Microsoft.Data.Sqlite.SqliteConnectionStringBuilder(connectionString!)
        {
            // Connection pooling configuration (T155)
            Pooling = true,                    // Enable connection pooling
            Mode = Microsoft.Data.Sqlite.SqliteOpenMode.ReadWriteCreate,
            Cache = Microsoft.Data.Sqlite.SqliteCacheMode.Shared, // Shared cache for better concurrency

            // Performance optimizations for large datasets (US5)
            // These pragmas improve throughput for cursor-based streaming
            DefaultTimeout = 30                // 30 second timeout for busy database
        };

        options.UseSqlite(
            sqliteConnectionStringBuilder.ToString(),
            sqliteOptions =>
            {
                // Enable command timeout for long-running queries (15k records)
                sqliteOptions.CommandTimeout(300); // 5 minutes for large reports

                // Optimize batch size for bulk operations
                sqliteOptions.MaxBatchSize(100);
            });

        // Add read-only interceptor (SC-007)
        ReadOnlyDbCommandInterceptor readOnlyInterceptor = sp.GetRequiredService<ReadOnlyDbCommandInterceptor>();
        options.AddInterceptors(readOnlyInterceptor);

        // Enable sensitive data logging only in development
        if (builder.Environment.IsDevelopment())
        {
            options.EnableSensitiveDataLogging();
            options.EnableDetailedErrors();
        }

        // Query optimization for streaming scenarios (US5)
        options.UseQueryTrackingBehavior(QueryTrackingBehavior.NoTracking); // Default to no-tracking for read operations

        Log.Information(
            "SQLite DbContext configured with connection pooling (Pooling={Pooling}, Cache={Cache}, CommandTimeout={Timeout}s)",
            true, "Shared", 300);
    });

    // Note: DbContextFactory removed to avoid scoped/singleton DI conflicts
    // Migrations work fine with just AddDbContext above

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

    // Register Phase 7 (US5) repositories for nested cursor patterns
    builder.Services.AddScoped<ICossuranceRepository, CossuranceRepository>();

    // Register Phase 3 repositories (US1 - T051-T055)
    builder.Services.AddScoped<IReportExecutionRepository, ReportExecutionRepository>();

    // Register business logic services (User Story 2)
    builder.Services.AddScoped<IPremiumCalculationService, PremiumCalculationService>();
    builder.Services.AddScoped<ICossuranceService, CossuranceService>();
    builder.Services.AddScoped<IExternalModuleService, ExternalModuleService>();
    builder.Services.AddScoped<IBusinessRuleValidationService, BusinessRuleValidationService>();

    // Register report generation service (User Story 2)
    builder.Services.AddScoped<IReportGenerationService, ReportGenerationService>();

    // Register Phase 3 services (US1 - T062-T063)
    builder.Services.AddScoped<ReportOrchestrationService>();
    builder.Services.AddScoped<IExecutionTrackingService, ExecutionTrackingService>();
    builder.Services.AddScoped<IFileWriterService, FileWriterService>(); // Stub for Phase 3, full impl in Phase 6

    // Register dashboard service (User Story 1)
    builder.Services.AddScoped<IDashboardService, DashboardService>();

    // Register query services (User Story 3)
    builder.Services.AddScoped<IPremiumQueryService, PremiumQueryService>();
    builder.Services.AddScoped<IPolicyQueryService, PolicyQueryService>();

    // Register export services (User Story 3)
    builder.Services.AddScoped<IPremiumExportService, PremiumCsvExportService>();
    builder.Services.AddScoped<PremiumExcelExportService>();
    builder.Services.AddScoped<PremiumPdfExportService>();

    // Register batch job services (Phase 6)
    builder.Services.AddScoped<IBatchJobRepository, BatchJobRepository>();
    builder.Services.AddScoped<IBackgroundJobService, BackgroundJobService>();

    // Register mock data services (User Story 5)
    builder.Services.AddScoped<CsvDataLoader>();
    builder.Services.AddScoped<JsonDataLoader>();
    builder.Services.AddScoped<IMockDataService, MockDataService>();
    builder.Services.AddScoped<IDataValidationService, DataValidationServiceAdapter>();
    builder.Services.AddScoped<DataValidationService>(); // Keep original for other uses

    // Register batch scheduling and notification services (User Story 4 - T177, T179, T181)
    builder.Services.AddScoped<IBatchSchedulingService, BatchSchedulingService>();
    builder.Services.AddScoped<INotificationService, EmailNotificationService>();

    // Register authentication service (User Story 6 - T228)
    builder.Services.AddScoped<IAuthService, AuthService>();

    // Register external service integration (Phase 8 - US6 - T159-T172)
    builder.Services.AddScoped<IReinsuranceCalculationService, ReinsuranceCalculationService>();
    builder.Services.AddScoped<IFormattingService, FormattingService>();
    builder.Services.AddScoped<IExternalValidationService, ExternalValidationService>();

    // Configure Hangfire for batch job scheduling (User Story 4 - T181, T182)
    builder.Services.AddHangfire(config => config
        .SetDataCompatibilityLevel(CompatibilityLevel.Version_180)
        .UseSimpleAssemblyNameTypeSerializer()
        .UseRecommendedSerializerSettings()
        .UseInMemoryStorage()); // Use in-memory storage for development (switch to SQL for production)

    // Add Hangfire server
    builder.Services.AddHangfireServer();

    WebApplication app = builder.Build();

    // CLI Command: Seed sample data (T074 - Phase 3)
    // Usage: dotnet run --seed-data
    if (args.Contains("--seed-data"))
    {
        Log.Information("CLI: Seeding sample data");
        using (var scope = app.Services.CreateScope())
        {
            var context = scope.ServiceProvider.GetRequiredService<PremiumReportingDbContext>();
            var seederLogger = scope.ServiceProvider.GetRequiredService<ILogger<DataSeeder>>();
            var seeder = new DataSeeder(context, seederLogger);

            await seeder.SeedSampleDataAsync();
        }
        Log.Information("CLI: Data seeding completed");
        return; // Exit after seeding
    }

    // Configure the HTTP request pipeline

    // Correlation ID MUST be first to track all requests
    app.UseCorrelationId();

    // Global exception handler MUST be early in the pipeline
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

    // HTTPS Redirection (User Story 6 - T231)
    app.UseHttpsRedirection();

    // HSTS (HTTP Strict Transport Security) - Production only (User Story 6 - T231)
    if (!app.Environment.IsDevelopment())
    {
        var hstsMaxAge = app.Configuration.GetValue<int>("Hsts:MaxAge", 31536000); // 1 year default
        var hstsIncludeSubDomains = app.Configuration.GetValue<bool>("Hsts:IncludeSubDomains", true);

        app.UseHsts();
        Log.Information(
            "HSTS enabled: MaxAge={MaxAge}s, IncludeSubDomains={IncludeSubDomains}",
            hstsMaxAge,
            hstsIncludeSubDomains);
    }

    app.UseCors();

    // Rate limiting deve vir após CORS mas antes de autenticação (User Story 6 - T230)
    // Protege contra ataques de força bruta e abuse da API
    app.UseIpRateLimiting();
    app.UseRateLimitHeaders(); // Adiciona cabeçalhos informativos em todas as respostas

    // Authentication must come before Authorization (User Story 6 - T228)
    app.UseAuthentication();
    app.UseAuthorization();

    // Configure Hangfire Dashboard (User Story 4 - T182)
    app.UseHangfireDashboard("/hangfire", new DashboardOptions
    {
        DashboardTitle = "Caixa Seguradora - Batch Jobs Dashboard",
        // In production, add authorization filter:
        // Authorization = new[] { new HangfireAuthorizationFilter() }
    });

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

// Make Program class accessible to integration tests
public partial class Program { }
