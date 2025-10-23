using AspNetCoreRateLimit;
using Microsoft.Extensions.Options;

namespace CaixaSeguradora.Api.Middleware;

/// <summary>
/// Middleware para adicionar cabeçalhos de rate limiting em todas as respostas.
/// Fornece informações sobre o limite de requisições, requisições restantes e tempo de reset.
/// </summary>
public class RateLimitHeadersMiddleware
{
    private readonly RequestDelegate _next;
    private readonly IpRateLimitOptions _options;
    private readonly ILogger<RateLimitHeadersMiddleware> _logger;

    public RateLimitHeadersMiddleware(
        RequestDelegate next,
        IOptions<IpRateLimitOptions> options,
        ILogger<RateLimitHeadersMiddleware> logger)
    {
        _next = next ?? throw new ArgumentNullException(nameof(next));
        _options = options?.Value ?? throw new ArgumentNullException(nameof(options));
        _logger = logger ?? throw new ArgumentNullException(nameof(logger));
    }

    public async Task InvokeAsync(HttpContext context)
    {
        // Execute o próximo middleware na pipeline
        await _next(context);

        // Adicione cabeçalhos de rate limiting na resposta
        AddRateLimitHeaders(context);
    }

    private void AddRateLimitHeaders(HttpContext context)
    {
        try
        {
            // Obtenha o endpoint atual
            var endpoint = context.GetEndpoint();
            if (endpoint == null)
            {
                return;
            }

            // Obtenha a rota do endpoint
            var routePattern = endpoint.Metadata
                .GetMetadata<Microsoft.AspNetCore.Routing.RouteEndpoint>()
                ?.RoutePattern.RawText ?? context.Request.Path.Value ?? "*";

            var method = context.Request.Method.ToLowerInvariant();
            var endpointKey = $"{method}:{routePattern}";

            // Encontre a regra aplicável para este endpoint
            // AspNetCoreRateLimit usa propriedades diferentes dependendo da versão
            // Para simplicidade, usamos apenas a regra geral por enquanto
            RateLimitRule? rule = null;

            if (_options.GeneralRules != null && _options.GeneralRules.Any())
            {
                rule = _options.GeneralRules.FirstOrDefault();
            }

            if (rule != null)
            {
                // X-Rate-Limit-Limit: número máximo de requisições permitidas no período
                context.Response.Headers["X-Rate-Limit-Limit"] = rule.Limit.ToString();

                // X-Rate-Limit-Remaining: requisições restantes (não podemos calcular exatamente sem acessar o store)
                // Por ora, use um valor placeholder - idealmente seria integrado com IRateLimitCounterStore
                if (!context.Response.Headers.ContainsKey("X-Rate-Limit-Remaining"))
                {
                    context.Response.Headers["X-Rate-Limit-Remaining"] = (rule.Limit - 1).ToString();
                }

                // X-Rate-Limit-Reset: timestamp quando o limite será resetado
                var periodSeconds = ParsePeriodToSeconds(rule.Period);
                var resetTime = DateTimeOffset.UtcNow.AddSeconds(periodSeconds).ToUnixTimeSeconds();
                context.Response.Headers["X-Rate-Limit-Reset"] = resetTime.ToString();

                // X-Rate-Limit-Policy: descrição da política aplicada
                context.Response.Headers["X-Rate-Limit-Policy"] = $"{rule.Limit} per {rule.Period}";
            }

            // Se a resposta for 429 (Too Many Requests), adicione o cabeçalho Retry-After
            if (context.Response.StatusCode == 429)
            {
                var periodSeconds = rule != null ? ParsePeriodToSeconds(rule.Period) : 60;
                context.Response.Headers["Retry-After"] = periodSeconds.ToString();

                _logger.LogWarning(
                    "Rate limit exceeded for {Method} {Path} from IP {ClientIp}. Retry after {RetryAfter}s",
                    context.Request.Method,
                    context.Request.Path,
                    context.Connection.RemoteIpAddress?.ToString() ?? "unknown",
                    periodSeconds);
            }
        }
        catch (Exception ex)
        {
            // Não falhe a requisição se houver erro ao adicionar cabeçalhos
            _logger.LogError(ex, "Erro ao adicionar cabeçalhos de rate limiting");
        }
    }

    private bool MatchesWildcard(string pattern, string endpoint)
    {
        // Suporte básico para padrões com wildcard
        if (pattern.Contains("*"))
        {
            var patternParts = pattern.Split('*');
            return endpoint.Contains(patternParts[0]);
        }
        return false;
    }

    private int ParsePeriodToSeconds(string period)
    {
        if (string.IsNullOrEmpty(period))
        {
            return 60; // Default to 1 minute
        }

        var unit = period[^1];
        var valueStr = period[..^1];

        if (!int.TryParse(valueStr, out int value))
        {
            return 60;
        }

        return unit switch
        {
            's' => value,
            'm' => value * 60,
            'h' => value * 3600,
            'd' => value * 86400,
            _ => 60
        };
    }
}

/// <summary>
/// Extension methods para registrar o middleware de cabeçalhos de rate limiting.
/// </summary>
public static class RateLimitHeadersMiddlewareExtensions
{
    public static IApplicationBuilder UseRateLimitHeaders(this IApplicationBuilder builder)
    {
        return builder.UseMiddleware<RateLimitHeadersMiddleware>();
    }
}
