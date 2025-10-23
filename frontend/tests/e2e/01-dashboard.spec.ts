/**
 * E2E Tests for User Story 1: Dashboard and System Overview
 *
 * Validates:
 * - FR-001: Display current COBOL program overview
 * - FR-002: Show migration function points (687 data items, 26 tables)
 * - FR-003: Display system dependencies map
 * - SC-001: Dashboard loads in <2 seconds
 */

import { test, expect } from '@playwright/test';

test.describe('User Story 1: Dashboard Page', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/');
  });

  test('T214.1: Dashboard loads successfully', async ({ page }) => {
    // Verify page title
    await expect(page).toHaveTitle(/Caixa Seguradora/);

    // Verify navigation is visible
    await expect(page.locator('nav')).toBeVisible();

    // Verify main content area exists
    await expect(page.locator('main')).toBeVisible();
  });

  test('T214.2: Program overview card displays COBOL information', async ({ page }) => {
    // Verify program info card is visible
    const programCard = page.locator('[data-testid="program-info-card"]');
    await expect(programCard).toBeVisible();

    // Verify COBOL program name is displayed
    await expect(programCard.locator('text=/RG1866B/i')).toBeVisible();

    // Verify SUSEP Circular 360 is mentioned
    await expect(programCard.locator('text=/SUSEP.*360/i')).toBeVisible();
  });

  test('T214.3: Function points metrics are displayed', async ({ page }) => {
    // Verify data items count (687)
    const dataItemsMetric = page.locator('text=/687.*items/i');
    await expect(dataItemsMetric).toBeVisible();

    // Verify tables count (26)
    const tablesMetric = page.locator('text=/26.*tabelas/i');
    await expect(tablesMetric).toBeVisible();

    // Verify lines of code (5000+)
    const locMetric = page.locator('text=/5.*000.*linhas/i');
    await expect(locMetric).toBeVisible();
  });

  test('T214.4: System dependencies map is displayed', async ({ page }) => {
    // Navigate to dependencies section
    const depsSection = page.locator('[data-testid="dependencies-section"]');
    await expect(depsSection).toBeVisible();

    // Verify database dependencies
    await expect(depsSection.locator('text=/DB2/i')).toBeVisible();

    // Verify file dependencies
    await expect(depsSection.locator('text=/PREMIT/i')).toBeVisible();
    await expect(depsSection.locator('text=/PREMCED/i')).toBeVisible();
  });

  test('T214.5: Dashboard loads within performance requirement (<2s)', async ({ page }) => {
    const startTime = Date.now();

    await page.goto('/');
    await page.waitForLoadState('networkidle');

    const loadTime = Date.now() - startTime;

    // SC-001: Dashboard must load in <2 seconds
    expect(loadTime).toBeLessThan(2000);
  });

  test('T214.6: Navigation menu works correctly', async ({ page }) => {
    // Verify all navigation links are present
    await expect(page.locator('nav >> text=/Dashboard/i')).toBeVisible();
    await expect(page.locator('nav >> text=/Relatórios/i')).toBeVisible();
    await expect(page.locator('nav >> text=/Consultas/i')).toBeVisible();
    await expect(page.locator('nav >> text=/Lotes/i')).toBeVisible();
    await expect(page.locator('nav >> text=/Dados Mock/i')).toBeVisible();
  });

  test('T214.7: Caixa Seguradora branding is applied', async ({ page }) => {
    // Verify logo or brand name is visible
    const header = page.locator('header');
    await expect(header.locator('text=/Caixa Seguradora/i')).toBeVisible();

    // Verify Caixa blue color is used (#0047BB)
    const primaryButton = page.locator('button').first();
    if (await primaryButton.isVisible()) {
      const bgColor = await primaryButton.evaluate((el) => {
        return window.getComputedStyle(el).backgroundColor;
      });
      // RGB equivalent of #0047BB is rgb(0, 71, 187)
      expect(bgColor).toContain('0, 71, 187');
    }
  });

  test('T214.8: Dashboard is responsive on mobile', async ({ page }) => {
    // Set mobile viewport
    await page.setViewportSize({ width: 375, height: 667 });

    await page.goto('/');

    // Verify content is visible and not cut off
    await expect(page.locator('main')).toBeVisible();

    // Verify navigation adapts to mobile (hamburger menu or stacked)
    const nav = page.locator('nav');
    await expect(nav).toBeVisible();
  });
});
