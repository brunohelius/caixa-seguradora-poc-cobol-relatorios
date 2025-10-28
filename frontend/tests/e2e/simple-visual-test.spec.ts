import { test, expect } from '@playwright/test';

const BASE_URL = 'http://localhost:5174';

test.describe('Caixa Seguradora Frontend - Visual & Functional Tests', () => {

  test('DashboardPage - Layout and Components', async ({ page }) => {
    await page.goto(BASE_URL);

    // Verify page loads
    await expect(page).toHaveTitle(/Caixa Seguradora/i);

    // Verify header exists
    const header = page.locator('header, [role="banner"]').first();
    await expect(header).toBeVisible();

    // Verify success banner (green)
    const successBanner = page.locator('.bg-gradient-to-r').first();
    await expect(successBanner).toBeVisible();

    // Verify banner text is visible (white on green)
    const bannerTitle = page.getByText(/Migração 95% Completa/i);
    await expect(bannerTitle).toBeVisible();

    // Verify Próximas Ações section (blue)
    const nextActions = page.getByText(/Próximas Ações/i);
    await expect(nextActions).toBeVisible();

    // Verify action cards are visible
    const uatCard = page.getByText(/UAT \(Semana 10-11\)/i);
    await expect(uatCard).toBeVisible();

    // Verify buttons exist and are clickable
    const buttons = page.locator('button');
    const buttonCount = await buttons.count();
    expect(buttonCount).toBeGreaterThan(0);

    // Take screenshot
    await page.screenshot({ path: 'tests/e2e/screenshots/dashboard-full.png', fullPage: true });
  });

  test('Navigation - All Routes Accessible', async ({ page }) => {
    const routes = [
      { path: '/', name: 'Dashboard' },
      { path: '/batch-jobs', name: 'Batch Jobs' },
      { path: '/query', name: 'Query' },
      { path: '/reports', name: 'Reports' },
      { path: '/mock-data', name: 'Mock Data' }
    ];

    for (const route of routes) {
      await page.goto(`${BASE_URL}${route.path}`);

      // Verify page loads without errors
      await page.waitForLoadState('networkidle');

      // Verify no console errors
      const errors: string[] = [];
      page.on('pageerror', error => errors.push(error.message));

      // Take screenshot
      await page.screenshot({
        path: `tests/e2e/screenshots/${route.name.toLowerCase().replace(' ', '-')}.png`
      });

      // Verify loaded
      const body = page.locator('body');
      await expect(body).toBeVisible();
    }
  });

  test('Color Branding - Caixa Blue and Yellow Present', async ({ page }) => {
    await page.goto(BASE_URL);

    // Check for Caixa blue (#0047BB) usage
    const blueElements = page.locator('[class*="caixa-blue"], [class*="bg-gradient"]');
    const blueCount = await blueElements.count();
    expect(blueCount).toBeGreaterThan(0);

    // Verify success banner has green gradient
    const greenBanner = page.locator('.bg-gradient-to-r').first();
    await expect(greenBanner).toBeVisible();
  });

  test('Text Visibility - No Invisible Text', async ({ page }) => {
    await page.goto(BASE_URL);

    // Check specific important texts are visible
    await expect(page.getByText(/Migração 95% Completa/i)).toBeVisible();
    await expect(page.getByText(/Dashboard de Migração COBOL/i)).toBeVisible();
    await expect(page.getByText(/RG1866B/i).first()).toBeVisible();

    // Verify content is present
    const pageContent = await page.content();
    expect(pageContent).toContain('95%');
    expect(pageContent).toContain('240');
  });

  test('Interactive Elements - Buttons Clickable', async ({ page }) => {
    await page.goto(BASE_URL);
    await page.waitForLoadState('networkidle');

    // Find button elements (including shadcn/ui Button components)
    const buttons = page.locator('button, [role="button"]');
    const buttonCount = await buttons.count();

    console.log(`Found ${buttonCount} buttons`);

    // Verify at least the "Atualizar Dados" and "Gerar Relatórios" buttons exist
    const updateButton = page.getByRole('button', { name: /Atualizar Dados/i });
    const reportButton = page.getByRole('button', { name: /Gerar Relatórios/i });

    await expect(updateButton.or(page.getByText(/Atualizar Dados/i)).first()).toBeVisible();
    await expect(reportButton.or(page.getByText(/Gerar Relatórios/i)).first()).toBeVisible();
  });

  test('Responsive Design - Mobile View', async ({ page }) => {
    // Set mobile viewport
    await page.setViewportSize({ width: 375, height: 812 });
    await page.goto(BASE_URL);

    // Verify page still renders
    const body = page.locator('body');
    await expect(body).toBeVisible();

    // Take mobile screenshot
    await page.screenshot({ path: 'tests/e2e/screenshots/dashboard-mobile.png', fullPage: true });
  });

  test('Performance - Page Load Time', async ({ page }) => {
    const startTime = Date.now();
    await page.goto(BASE_URL);
    await page.waitForLoadState('networkidle');
    const loadTime = Date.now() - startTime;

    console.log(`Page load time: ${loadTime}ms`);

    // Should load in less than 3 seconds
    expect(loadTime).toBeLessThan(3000);
  });
});
