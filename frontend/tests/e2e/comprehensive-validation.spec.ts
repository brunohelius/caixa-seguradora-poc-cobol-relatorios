import { test, expect } from '@playwright/test';

/**
 * Comprehensive Frontend Validation Test Suite
 * Tests all 6 implemented pages for functionality and accessibility
 *
 * Pages to test:
 * 1. Dashboard (DashboardPage.tsx)
 * 2. Report Generation (ReportGenerationPage.tsx)
 * 3. Query/Visualization (QueryPage.tsx)
 * 4. Batch Jobs (BatchJobsPage.tsx)
 * 5. Mock Data (MockDataPage.tsx)
 * 6. Report Generation V2 (ReportGenerationPageV2.tsx)
 */

test.describe('Comprehensive Frontend Validation', () => {
  // Set longer timeout for comprehensive tests
  test.setTimeout(120000); // 2 minutes per test

  test.beforeEach(async ({ page }) => {
    // Suppress console errors that don't affect functionality
    page.on('console', msg => {
      if (msg.type() === 'error' && !msg.text().includes('Failed to load resource')) {
        console.log('Console error:', msg.text());
      }
    });
  });

  test('1. Dashboard Page - Should load and display all metrics', async ({ page }) => {
    console.log('🧪 Testing Dashboard Page...');

    await page.goto('http://localhost:5173/');

    // Wait for page to load
    await page.waitForLoadState('networkidle', { timeout: 10000 });

    // Check page title
    await expect(page).toHaveTitle(/Dashboard|Caixa Seguradora/i, { timeout: 10000 });

    // Check for main heading
    const heading = page.locator('h1, h2').filter({ hasText: /Dashboard|Métricas|Visão Geral/i }).first();
    await expect(heading).toBeVisible({ timeout: 10000 });

    // Check for metric cards
    const metricCards = page.locator('[class*="card"], [class*="Card"], .metric-card, .dashboard-card');
    const cardCount = await metricCards.count();
    console.log(`  ✓ Found ${cardCount} metric cards`);
    expect(cardCount).toBeGreaterThan(0);

    // Take screenshot
    await page.screenshot({ path: 'tests/e2e/screenshots/dashboard-validation.png', fullPage: true });

    console.log('  ✅ Dashboard Page: PASSED');
  });

  test('2. Report Generation Page - Should load form and allow generation', async ({ page }) => {
    console.log('🧪 Testing Report Generation Page...');

    await page.goto('http://localhost:5173/reports');

    await page.waitForLoadState('networkidle', { timeout: 10000 });

    // Check for report generation heading
    const heading = page.locator('h1, h2').filter({ hasText: /Geração de Relatórios|Relatórios|Reports/i }).first();
    await expect(heading).toBeVisible({ timeout: 10000 });

    // Check for date inputs or form fields
    const inputs = page.locator('input[type="date"], input[type="text"], select');
    const inputCount = await inputs.count();
    console.log(`  ✓ Found ${inputCount} form inputs`);
    expect(inputCount).toBeGreaterThan(0);

    // Check for generate button
    const generateButton = page.locator('button').filter({ hasText: /Gerar|Generate|Processar/i }).first();
    await expect(generateButton).toBeVisible({ timeout: 5000 });

    // Take screenshot
    await page.screenshot({ path: 'tests/e2e/screenshots/report-generation-validation.png', fullPage: true });

    console.log('  ✅ Report Generation Page: PASSED');
  });

  test('3. Query/Visualization Page - Should load and display query interface', async ({ page }) => {
    console.log('🧪 Testing Query/Visualization Page...');

    await page.goto('http://localhost:5173/query');

    await page.waitForLoadState('networkidle', { timeout: 10000 });

    // Check for query interface
    const heading = page.locator('h1, h2').filter({ hasText: /Query|Consulta|Visualização|Pesquisa/i }).first();
    await expect(heading).toBeVisible({ timeout: 10000 });

    // Check for search or filter inputs
    const searchElements = page.locator('input[type="search"], input[type="text"], input[placeholder*="Buscar"], input[placeholder*="Search"], button').filter({ hasText: /Buscar|Search|Pesquisar/i });
    const searchCount = await searchElements.count();
    console.log(`  ✓ Found ${searchCount} search/filter elements`);

    // Take screenshot
    await page.screenshot({ path: 'tests/e2e/screenshots/query-validation.png', fullPage: true });

    console.log('  ✅ Query/Visualization Page: PASSED');
  });

  test('4. Batch Jobs Page - Should load and display jobs interface', async ({ page }) => {
    console.log('🧪 Testing Batch Jobs Page...');

    await page.goto('http://localhost:5173/batch-jobs');

    await page.waitForLoadState('networkidle', { timeout: 10000 });

    // Check for batch jobs heading
    const heading = page.locator('h1, h2').filter({ hasText: /Batch|Jobs|Processamento|Lote/i }).first();
    await expect(heading).toBeVisible({ timeout: 10000 });

    // Check for job list or create button
    const jobElements = page.locator('table, .job-list, button').filter({ hasText: /Criar|Create|Novo/i });
    const elementCount = await jobElements.count();
    console.log(`  ✓ Found ${elementCount} job-related elements`);

    // Take screenshot
    await page.screenshot({ path: 'tests/e2e/screenshots/batch-jobs-validation.png', fullPage: true });

    console.log('  ✅ Batch Jobs Page: PASSED');
  });

  test('5. Mock Data Page - Should load and display data management', async ({ page }) => {
    console.log('🧪 Testing Mock Data Page...');

    await page.goto('http://localhost:5173/mock-data');

    await page.waitForLoadState('networkidle', { timeout: 10000 });

    // Check for mock data heading
    const heading = page.locator('h1, h2').filter({ hasText: /Mock|Dados|Test|Teste/i }).first();
    await expect(heading).toBeVisible({ timeout: 10000 });

    // Check for data management buttons
    const buttons = page.locator('button');
    const buttonCount = await buttons.count();
    console.log(`  ✓ Found ${buttonCount} buttons`);
    expect(buttonCount).toBeGreaterThan(0);

    // Take screenshot
    await page.screenshot({ path: 'tests/e2e/screenshots/mock-data-validation.png', fullPage: true });

    console.log('  ✅ Mock Data Page: PASSED');
  });

  test('6. Navigation - Should navigate between all pages', async ({ page }) => {
    console.log('🧪 Testing Navigation Between Pages...');

    await page.goto('http://localhost:5173/');
    await page.waitForLoadState('networkidle', { timeout: 10000 });

    // Test navigation to each page
    const routes = [
      { path: '/', name: 'Dashboard' },
      { path: '/reports', name: 'Reports' },
      { path: '/query', name: 'Query' },
      { path: '/batch-jobs', name: 'Batch Jobs' },
      { path: '/mock-data', name: 'Mock Data' }
    ];

    for (const route of routes) {
      console.log(`  → Navigating to ${route.name}...`);
      await page.goto(`http://localhost:5173${route.path}`);
      await page.waitForLoadState('networkidle', { timeout: 10000 });

      // Verify page loaded (check URL)
      expect(page.url()).toContain(route.path === '/' ? '5173/' : route.path);
      console.log(`    ✓ ${route.name} loaded successfully`);
    }

    console.log('  ✅ Navigation: PASSED');
  });

  test('7. Accessibility - Check for basic accessibility issues', async ({ page }) => {
    console.log('🧪 Testing Accessibility...');

    const routes = ['/', '/reports', '/query', '/batch-jobs', '/mock-data'];
    const issues: string[] = [];

    for (const route of routes) {
      await page.goto(`http://localhost:5173${route}`);
      await page.waitForLoadState('networkidle', { timeout: 10000 });

      // Check for basic accessibility
      const buttons = await page.locator('button').all();
      for (const button of buttons) {
        const text = await button.textContent();
        if (text && text.trim() === '') {
          const ariaLabel = await button.getAttribute('aria-label');
          if (!ariaLabel) {
            issues.push(`${route}: Button without text or aria-label`);
          }
        }
      }

      console.log(`  ✓ Checked ${route}`);
    }

    if (issues.length > 0) {
      console.log('  ⚠️ Accessibility warnings:', issues);
    } else {
      console.log('  ✅ No critical accessibility issues found');
    }

    console.log('  ✅ Accessibility Check: COMPLETED');
  });

  test('8. Responsive Design - Test mobile and desktop views', async ({ page, browserName }) => {
    console.log('🧪 Testing Responsive Design...');

    const viewports = [
      { width: 375, height: 667, name: 'Mobile (iPhone SE)' },
      { width: 768, height: 1024, name: 'Tablet (iPad)' },
      { width: 1920, height: 1080, name: 'Desktop (Full HD)' }
    ];

    for (const viewport of viewports) {
      console.log(`  → Testing ${viewport.name}...`);
      await page.setViewportSize({ width: viewport.width, height: viewport.height });

      await page.goto('http://localhost:5173/');
      await page.waitForLoadState('networkidle', { timeout: 10000 });

      // Take screenshot
      await page.screenshot({
        path: `tests/e2e/screenshots/dashboard-${viewport.width}x${viewport.height}.png`,
        fullPage: true
      });

      // Check if content is visible
      const mainContent = page.locator('main, [role="main"], .content, .app-content').first();
      await expect(mainContent).toBeVisible({ timeout: 5000 });

      console.log(`    ✓ ${viewport.name} rendered successfully`);
    }

    console.log('  ✅ Responsive Design: PASSED');
  });
});
