import { test, expect } from '@playwright/test';

/**
 * Comprehensive test suite for all application routes
 * Tests navigation, page loading, and basic functionality
 */

test.describe('Application Routes - Comprehensive Test Suite', () => {
  test.beforeEach(async ({ page }) => {
    // Navigate to home page before each test
    await page.goto('/');
  });

  test('01 - Dashboard route loads successfully', async ({ page }) => {
    await page.goto('/');
    await page.waitForLoadState('networkidle');
    await page.waitForTimeout(3000); // Wait for async data loading

    // Check page title
    await expect(page).toHaveTitle(/Sistema de Relatórios/);

    // Check main heading or body is visible
    await expect(page.locator('body')).toBeVisible();

    // Verify navigation is present
    const nav = page.locator('nav, header');
    await expect(nav.first()).toBeVisible({ timeout: 10000 });

    // Check for Caixa logo
    const logo = page.locator('img[alt*="Caixa"], img[src*="logo"], img[src*="caixa"]');
    if (await logo.count() > 0) {
      await expect(logo.first()).toBeVisible();
    }

    console.log('✓ Dashboard route OK');
  });

  test('02 - Reports route loads successfully', async ({ page }) => {
    await page.goto('/reports');
    await page.waitForLoadState('networkidle');
    await page.waitForTimeout(2000);

    // Check body is visible
    await expect(page.locator('body')).toBeVisible();

    // Check for any heading (h1, h2, h3)
    const headings = page.locator('h1, h2, h3');
    await expect(headings.first()).toBeVisible({ timeout: 15000 });

    // Check for form elements or info card
    const content = page.locator('form, div[class*="bg-"], section, main');
    await expect(content.first()).toBeVisible();

    console.log('✓ Reports route OK');
  });

  test('03 - Query route loads successfully', async ({ page }) => {
    await page.goto('/query');

    // Wait for page to load
    await page.waitForLoadState('networkidle');

    // Check page is loaded
    await expect(page.locator('body')).toBeVisible();

    console.log('✓ Query route OK');
  });

  test('04 - Batch Jobs route loads successfully', async ({ page }) => {
    await page.goto('/batch-jobs');

    // Wait for page to load
    await page.waitForLoadState('networkidle');

    // Check page is loaded
    await expect(page.locator('body')).toBeVisible();

    console.log('✓ Batch Jobs route OK');
  });

  test('05 - Data Management route loads successfully', async ({ page }) => {
    await page.goto('/data-management');

    // Wait for page to load
    await page.waitForLoadState('networkidle');

    // Check page is loaded
    await expect(page.locator('body')).toBeVisible();

    console.log('✓ Data Management route OK');
  });

  test('06 - Navigation menu works correctly', async ({ page }) => {
    await page.goto('/');

    // Test navigation to Reports
    const reportsLink = page.locator('a').filter({ hasText: /Gerar Relatórios/i });
    await expect(reportsLink).toBeVisible();
    await reportsLink.click();
    await page.waitForURL('**/reports');
    expect(page.url()).toContain('/reports');

    // Test navigation to Query
    const queryLink = page.locator('a').filter({ hasText: /Consultar Dados/i });
    await queryLink.click();
    await page.waitForURL('**/query');
    expect(page.url()).toContain('/query');

    // Test navigation back to Dashboard
    const dashboardLink = page.locator('a').filter({ hasText: /Dashboard/i });
    await dashboardLink.click();
    await page.waitForURL(/\/$|\/$/);

    console.log('✓ Navigation menu OK');
  });

  test('07 - 404 page for invalid routes', async ({ page }) => {
    await page.goto('/invalid-route-that-does-not-exist');

    // Wait for page to load
    await page.waitForLoadState('networkidle');

    // Check for 404 content
    const notFoundText = page.locator('text=/404|não encontrada|not found/i');
    await expect(notFoundText.first()).toBeVisible({ timeout: 5000 });

    console.log('✓ 404 page OK');
  });

  test('08 - Check all navigation links are present', async ({ page }) => {
    await page.goto('/');

    const expectedLinks = [
      'Dashboard',
      'Gerar Relatórios',
      'Consultar Dados',
      'Jobs Agendados',
      'Gerenciar Dados'
    ];

    for (const linkText of expectedLinks) {
      const link = page.locator('a').filter({ hasText: new RegExp(linkText, 'i') });
      await expect(link).toBeVisible();
    }

    console.log('✓ All navigation links present');
  });

  test('09 - Header and footer are present on all pages', async ({ page }) => {
    const routes = ['/', '/reports', '/query', '/batch-jobs', '/data-management'];

    for (const route of routes) {
      await page.goto(route);

      // Check header
      const header = page.locator('header');
      await expect(header).toBeVisible();

      // Check footer
      const footer = page.locator('footer');
      await expect(footer).toBeVisible();
    }

    console.log('✓ Header and footer present on all pages');
  });

  test('10 - Page responsiveness - mobile viewport', async ({ page }) => {
    // Set mobile viewport
    await page.setViewportSize({ width: 375, height: 667 });

    await page.goto('/');

    // Check that content is still visible
    await expect(page.locator('body')).toBeVisible();

    // Navigate to reports
    await page.goto('/reports');
    await expect(page.locator('body')).toBeVisible();

    console.log('✓ Mobile responsiveness OK');
  });
});

test.describe('Reports Page - Detailed Tests', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/reports');
    await page.waitForLoadState('networkidle');
  });

  test('Reports page contains expected elements', async ({ page }) => {
    // Check for heading
    const heading = page.locator('h1').filter({ hasText: /Relatórios|SUSEP/i });
    await expect(heading).toBeVisible({ timeout: 10000 });

    // Check for description or info
    const description = page.locator('p, .text-sm, .text-gray-600').first();
    await expect(description).toBeVisible();

    console.log('✓ Reports page elements OK');
  });

  test('Reports page info card is displayed when no job is running', async ({ page }) => {
    // Look for info card
    const infoCard = page.locator('.bg-blue-50, .border-blue-200').first();

    // If info card exists, it should be visible
    const count = await infoCard.count();
    if (count > 0) {
      await expect(infoCard).toBeVisible();
      console.log('✓ Info card displayed');
    } else {
      console.log('✓ No info card (job may be running)');
    }
  });
});

test.describe('API Health Check', () => {
  test('Backend API is accessible', async ({ page }) => {
    // Check if backend is responding - correct endpoint
    const response = await page.request.get('http://localhost:5555/api/Dashboard/health');
    expect(response.status()).toBe(200);

    const data = await response.json();
    expect(data).toHaveProperty('status');

    console.log('✓ Backend API health check OK');
  });

  test('Reports API endpoint is accessible', async ({ page }) => {
    // Check reports history endpoint
    const response = await page.request.get('http://localhost:5555/api/Reports/history?page=1&pageSize=10');
    expect(response.status()).toBe(200);

    const data = await response.json();
    expect(data).toHaveProperty('reports');
    expect(data).toHaveProperty('totalCount');

    console.log('✓ Reports API endpoint OK');
  });
});
