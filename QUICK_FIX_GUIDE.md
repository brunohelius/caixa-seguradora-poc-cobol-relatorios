# Quick Fix Guide - Critical Issues

**Date**: October 27, 2025
**Purpose**: Step-by-step instructions for fixing the 3 critical blocking issues

---

## Issue #1: COBOL Rounding Behavior (HIGHEST PRIORITY) ⏱️ 1-2 hours

### Problem
- **Test Failures**: 3 rounding tests in `PremiumCalculationServiceTests`
- **Root Cause**: `RoundCobol()` method not using `MidpointRounding.AwayFromZero`
- **Impact**: Financial calculations differ from COBOL (regulatory violation)

### Failed Tests
```
✗ RoundCobol_RoundsUpFromHalf
✗ RoundCobol_RoundsNegativeAwayFromZero
✗ RoundCobol_ToZeroDecimals_RoundsToInteger
```

### Fix Location
**File**: `/Users/brunosouza/Development/Caixa Seguradora/POC Cobol/backend/src/CaixaSeguradora.Core/Services/PremiumCalculationService.cs`

### Current Code (WRONG)
```csharp
public decimal RoundCobol(decimal value, int decimalPlaces)
{
    // This uses ToEven (banker's rounding) by default - INCORRECT
    return Math.Round(value, decimalPlaces);
}
```

### Fixed Code (CORRECT)
```csharp
public decimal RoundCobol(decimal value, int decimalPlaces)
{
    // COBOL uses AwayFromZero rounding (0.5 rounds UP to 1, not to 0)
    return Math.Round(value, decimalPlaces, MidpointRounding.AwayFromZero);
}
```

### Verification
```bash
cd backend
dotnet test tests/CaixaSeguradora.UnitTests \
  --filter "FullyQualifiedName~PremiumCalculationServiceTests.RoundCobol" \
  --verbosity detailed
```

**Expected**: All 3 tests pass ✅

---

## Issue #2: FixedWidthFormatter Padding Logic ⏱️ 2-4 hours

### Problem
- **Test Failures**: 5 formatting tests in `FixedWidthFormatterTests`
- **Root Cause**: Incorrect width calculation or padding logic
- **Impact**: PREMIT/PREMCED files won't match COBOL byte-for-byte

### Failed Tests
```
✗ FormatNumeric_Decimal_VariousInputs_ReturnsCorrectFormat
✗ FullScenario_PremiumRecord_PREMIT_Format_BuildsCorrectly
✗ FormatNumeric_Decimal_VariousDecimalPlaces_ReturnsCorrectFormat
✗ BuildRecord_ComplexRecord_AllFieldTypes_BuildsCorrectly
```

### Fix Location
**File**: `/Users/brunosouza/Development/Caixa Seguradora/POC Cobol/backend/src/CaixaSeguradora.Infrastructure/Formatters/FixedWidthFormatter.cs`

### Investigation Steps

1. **Run failing test with detailed output**:
```bash
cd backend
dotnet test tests/CaixaSeguradora.UnitTests \
  --filter "FullyQualifiedName~FixedWidthFormatterTests.FormatNumeric_Decimal_VariousInputs" \
  --verbosity detailed
```

2. **Analyze expected vs actual**:
```
Expected: "00000012345.67"  (15 chars total)
Actual:   "000000012345.67" (16 chars total)
                   ↑ extra zero
```

3. **Check FormatNumeric() method**:
```csharp
public string FormatNumeric(decimal value, int totalWidth, int decimalPlaces, bool includeDecimalPoint = false)
{
    // ISSUE: totalWidth might be including decimal point when it shouldn't
    // Or padding calculation is wrong

    // Expected behavior:
    // - totalWidth = 15, decimalPlaces = 2, includeDecimalPoint = true
    // - Input: 12345.67
    // - Integer part width: 15 - 2 - 1 (decimal point) = 12 chars
    // - Format: 12 digits + '.' + 2 digits = "000000012345.67" (15 chars)
}
```

4. **Likely issue**: Width calculation doesn't account for decimal point correctly

### Fix Pattern (Hypothesis)
```csharp
public string FormatNumeric(decimal value, int totalWidth, int decimalPlaces, bool includeDecimalPoint = false)
{
    if (includeDecimalPoint)
    {
        // Total width includes decimal point
        int integerPartWidth = totalWidth - decimalPlaces - 1; // -1 for the '.'
        string integerPart = ((long)value).ToString().PadLeft(integerPartWidth, '0');
        string decimalPart = ((long)(value * (decimal)Math.Pow(10, decimalPlaces)) % (long)Math.Pow(10, decimalPlaces))
            .ToString().PadLeft(decimalPlaces, '0');
        return $"{integerPart}.{decimalPart}";
    }
    else
    {
        // No decimal point - multiply and pad
        long scaledValue = (long)(value * (decimal)Math.Pow(10, decimalPlaces));
        return scaledValue.ToString().PadLeft(totalWidth, '0');
    }
}
```

### Verification
```bash
cd backend
dotnet test tests/CaixaSeguradora.UnitTests \
  --filter "FullyQualifiedName~FixedWidthFormatterTests" \
  --verbosity detailed
```

**Expected**: All 5 formatter tests pass ✅

---

## Issue #3: Integration Test Database Provider Conflict ⏱️ 2-3 hours

### Problem
- **Test Failures**: 23/30 integration tests failing
- **Root Cause**: Both SQLite AND InMemory providers registered
- **Impact**: Cannot validate end-to-end workflows

### Error Message
```
System.InvalidOperationException: Services for database providers
'Microsoft.EntityFrameworkCore.Sqlite', 'Microsoft.EntityFrameworkCore.InMemory'
have been registered in the service provider.
```

### Fix Location
**File**: `/Users/brunosouza/Development/Caixa Seguradora/POC Cobol/backend/tests/CaixaSeguradora.IntegrationTests/TestWebApplicationFactory.cs`

### Investigation Steps

1. **Find duplicate registrations**:
```bash
cd backend/tests/CaixaSeguradora.IntegrationTests
grep -n "UseSqlite\|UseInMemoryDatabase" *.cs **/*.cs
```

2. **Check TestWebApplicationFactory**:
```csharp
protected override void ConfigureWebHost(IWebHostBuilder builder)
{
    builder.ConfigureServices(services =>
    {
        // ISSUE: Both UseSqlite and UseInMemoryDatabase called
        // Only ONE can be active at a time

        // Remove existing DbContext registration
        var descriptor = services.SingleOrDefault(
            d => d.ServiceType == typeof(DbContextOptions<PremiumReportingDbContext>));
        if (descriptor != null)
            services.Remove(descriptor);

        // CHOOSE ONE:
        // Option A: In-memory database
        services.AddDbContext<PremiumReportingDbContext>(options =>
            options.UseInMemoryDatabase("TestDb"));

        // Option B: SQLite in-memory (recommended - closer to production)
        services.AddDbContext<PremiumReportingDbContext>(options =>
            options.UseSqlite("DataSource=:memory:"));
    });
}
```

### Recommended Fix
Use SQLite in-memory mode (`:memory:`) because:
- Closer to production behavior
- Supports relational operations (ExecuteSqlRaw, migrations)
- Better for integration testing

### Fix Code
```csharp
protected override void ConfigureWebHost(IWebHostBuilder builder)
{
    builder.ConfigureServices(services =>
    {
        // Remove existing DbContext registration
        var descriptor = services.SingleOrDefault(
            d => d.ServiceType == typeof(DbContextOptions<PremiumReportingDbContext>));
        if (descriptor != null)
            services.Remove(descriptor);

        // Use SQLite in-memory ONLY
        services.AddDbContext<PremiumReportingDbContext>(options =>
        {
            options.UseSqlite("DataSource=:memory:");
            options.EnableSensitiveDataLogging(); // For test debugging
        });

        // Build service provider to ensure database created
        var sp = services.BuildServiceProvider();
        using var scope = sp.CreateScope();
        var db = scope.ServiceProvider.GetRequiredService<PremiumReportingDbContext>();
        db.Database.EnsureCreated();
    });
}
```

### Verification
```bash
cd backend
dotnet test tests/CaixaSeguradora.IntegrationTests --verbosity normal
```

**Expected**: 30/30 integration tests pass ✅

---

## Complete Verification Sequence

After fixing all 3 issues, run this complete test suite:

```bash
cd backend

# 1. Clean rebuild
dotnet clean
dotnet build --configuration Release

# 2. Run all tests
dotnet test --configuration Release --verbosity minimal

# 3. Expected results
# Unit Tests:        483/483 (100%)
# Integration Tests:  30/30  (100%)
# Comparison Tests:   25/25  (100%)
# Total:            538/538 (100%)

# 4. Verify production build
dotnet build CaixaSeguradora.Production.sln --configuration Release

# 5. Test health check endpoint (start API first)
cd src/CaixaSeguradora.Api
dotnet run &
sleep 5
curl http://localhost:5000/health | jq
```

---

## Post-Fix Checklist

- [ ] All 538 tests passing
- [ ] Build has 0 warnings and 0 errors
- [ ] Health check endpoint returns HTTP 200
- [ ] Commit changes with message: "fix: correct COBOL rounding, formatter padding, and test infrastructure"
- [ ] Re-run comparison tests to confirm COBOL parity
- [ ] Document fixes in VALIDATION_STATUS.md

---

## Time Estimates

| Issue | Estimate | Priority |
|-------|----------|----------|
| COBOL Rounding | 1-2 hours | 🔴 CRITICAL |
| FixedWidthFormatter | 2-4 hours | 🔴 CRITICAL |
| Integration Tests | 2-3 hours | 🟡 MEDIUM |
| **Total** | **5-9 hours** | **1 work day** |

---

## Support Resources

### Test Commands
```bash
# Run specific test
dotnet test --filter "FullyQualifiedName~TestName"

# Run with detailed output
dotnet test --verbosity detailed

# Run and save results
dotnet test --logger "trx;LogFileName=results.trx"

# Run with coverage
dotnet test /p:CollectCoverage=true /p:CoverageReportFormat=html
```

### Debugging
```bash
# View test output
dotnet test --logger "console;verbosity=detailed"

# Run single test
dotnet test --filter "FullyQualifiedName~ExactTestName" --verbosity detailed

# Attach debugger
dotnet test --logger "console;verbosity=detailed" -- RunConfiguration.DisableParallelization=true
```

### Git Workflow
```bash
# Create feature branch
git checkout -b fix/critical-test-failures

# Commit fixes
git add .
git commit -m "fix: correct COBOL rounding and formatter padding

- Implement MidpointRounding.AwayFromZero in RoundCobol()
- Fix FixedWidthFormatter totalWidth calculation
- Resolve integration test EF Core provider conflict

Fixes: 23 unit test failures, 23 integration test failures
Tests: 538/538 passing (100%)"

# Push
git push origin fix/critical-test-failures
```

---

**Last Updated**: October 27, 2025
**Developer**: Follow this guide sequentially for fastest resolution
**Questions**: See FINAL_VALIDATION_REPORT.md for detailed context
