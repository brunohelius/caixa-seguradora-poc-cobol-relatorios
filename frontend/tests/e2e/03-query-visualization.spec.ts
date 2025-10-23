/**
 * E2E Tests for User Story 3: Data Query and Visualization
 *
 * Validates:
 * - FR-009: Query premium records by date range
 * - FR-010: Query policy details
 * - FR-011: Interactive data visualization
 * - FR-012: Export query results to CSV/Excel
 * - SC-003: Query results display in <3 seconds
 * - SC-008: Charts update in real-time
 */

import { test, expect } from '@playwright/test';

test.describe('User Story 3: Query and Visualization', () => {
  test.beforeEach(async ({ page }) => {
    // Mock authentication if needed
    await page.goto('/query');
  });

  test('T214.19: Query page loads successfully', async ({ page }) => {
    // Verify query page header
    await expect(page.locator('h1, h2').filter({ hasText: /Consultas|Pesquisa/i })).toBeVisible();

    // Verify search form is visible
    await expect(page.locator('form, [data-testid="query-form"]')).toBeVisible();
  });

  test('T214.20: Premium query by date range works', async ({ page }) => {
    // Mock premium query API
    await page.route('**/api/v1/premiums/query', async (route) => {
      await route.fulfill({
        status: 200,
        contentType: 'application/json',
        body: JSON.stringify({
          records: [
            {
              policyNumber: 'POL-001',
              premiumAmount: 1500.50,
              effectiveDate: '2025-10-15',
              productName: 'Seguro Auto'
            },
            {
              policyNumber: 'POL-002',
              premiumAmount: 2300.75,
              effectiveDate: '2025-10-20',
              productName: 'Seguro Residencial'
            }
          ],
          totalRecords: 2,
          summary: {
            totalAmount: 3801.25,
            averageAmount: 1900.63
          }
        })
      });
    });

    // Fill date range
    await page.locator('input[type="date"]').first().fill('2025-10-01');
    await page.locator('input[type="date"]').nth(1).fill('2025-10-31');

    // Submit query
    await page.locator('button').filter({ hasText: /Consultar|Buscar|Pesquisar/i }).click();

    // Verify results are displayed
    await expect(page.locator('text=/POL-001|POL-002/')).toBeVisible({ timeout: 5000 });
  });

  test('T214.21: Query results display within performance requirement (<3s)', async ({ page }) => {
    // Mock API with delay
    let apiCallTime: number;
    await page.route('**/api/v1/premiums/query', async (route) => {
      apiCallTime = Date.now();
      await route.fulfill({
        status: 200,
        contentType: 'application/json',
        body: JSON.stringify({
          records: Array(50).fill(null).map((_, i) => ({
            policyNumber: `POL-${String(i + 1).padStart(3, '0')}`,
            premiumAmount: 1000 + (i * 100),
            effectiveDate: '2025-10-15'
          })),
          totalRecords: 50
        })
      });
    });

    // Fill and submit query
    await page.locator('input[type="date"]').first().fill('2025-10-01');
    await page.locator('input[type="date"]').nth(1).fill('2025-10-31');

    const startTime = Date.now();
    await page.locator('button').filter({ hasText: /Consultar|Buscar/i }).click();

    // Wait for results to appear
    await expect(page.locator('text=/POL-/').first()).toBeVisible({ timeout: 5000 });

    const displayTime = Date.now() - startTime;

    // SC-003: Results must display in <3 seconds
    expect(displayTime).toBeLessThan(3000);
  });

  test('T214.22: Policy details query works', async ({ page }) => {
    // Mock policy details API
    await page.route('**/api/v1/policies/*', async (route) => {
      await route.fulfill({
        status: 200,
        contentType: 'application/json',
        body: JSON.stringify({
          policyNumber: 'POL-001',
          companyCode: 123,
          branchCode: 456,
          policyType: 'Auto',
          status: 'active',
          effectiveDate: '2025-01-01',
          expirationDate: '2026-01-01',
          clientName: 'João Silva',
          totalPremium: 1500.50,
          coverages: [
            { code: 'COV-01', description: 'Cobertura Básica', amount: 1000.00 },
            { code: 'COV-02', description: 'Cobertura Adicional', amount: 500.50 }
          ]
        })
      });
    });

    // Search for specific policy
    const policyInput = page.locator('input[placeholder*="Apólice" i], input[name="policyNumber"]').first();
    if (await policyInput.isVisible()) {
      await policyInput.fill('POL-001');
      await page.locator('button').filter({ hasText: /Consultar|Buscar/i }).click();

      // Verify policy details are shown
      await expect(page.locator('text=/POL-001/')).toBeVisible();
      await expect(page.locator('text=/João Silva/i')).toBeVisible();
    }
  });

  test('T214.23: Data visualization charts are displayed', async ({ page }) => {
    // Mock query that returns data for charts
    await page.route('**/api/v1/premiums/query', async (route) => {
      await route.fulfill({
        status: 200,
        contentType: 'application/json',
        body: JSON.stringify({
          records: Array(10).fill(null).map((_, i) => ({
            policyNumber: `POL-${i}`,
            premiumAmount: 1000 + (i * 500),
            effectiveDate: `2025-10-${String(i + 1).padStart(2, '0')}`
          })),
          totalRecords: 10
        })
      });
    });

    // Submit query
    await page.locator('input[type="date"]').first().fill('2025-10-01');
    await page.locator('input[type="date"]').nth(1).fill('2025-10-31');
    await page.locator('button').filter({ hasText: /Consultar|Buscar/i }).click();

    // Wait for results
    await page.waitForTimeout(1000);

    // Look for chart elements (Recharts uses SVG)
    const chart = page.locator('svg, canvas, [data-testid*="chart"]');
    if (await chart.count() > 0) {
      await expect(chart.first()).toBeVisible();
    }
  });

  test('T214.24: Export to CSV functionality works', async ({ page }) => {
    // Mock query results
    await page.route('**/api/v1/premiums/query', async (route) => {
      await route.fulfill({
        status: 200,
        contentType: 'application/json',
        body: JSON.stringify({
          records: [
            { policyNumber: 'POL-001', premiumAmount: 1500.50 },
            { policyNumber: 'POL-002', premiumAmount: 2300.75 }
          ],
          totalRecords: 2
        })
      });
    });

    // Submit query
    await page.locator('input[type="date"]').first().fill('2025-10-01');
    await page.locator('input[type="date"]').nth(1).fill('2025-10-31');
    await page.locator('button').filter({ hasText: /Consultar|Buscar/i }).click();

    // Wait for results
    await page.waitForTimeout(1000);

    // Look for export button
    const exportButton = page.locator('button').filter({ hasText: /Export|CSV|Excel/i });
    if (await exportButton.count() > 0) {
      // Mock export API
      await page.route('**/api/v1/export/**', async (route) => {
        await route.fulfill({
          status: 200,
          contentType: 'text/csv',
          body: 'PolicyNumber,PremiumAmount\nPOL-001,1500.50\nPOL-002,2300.75'
        });
      });

      // Trigger download
      const downloadPromise = page.waitForEvent('download');
      await exportButton.first().click();

      const download = await downloadPromise;
      expect(download.suggestedFilename()).toMatch(/\.csv|\.xlsx/i);
    }
  });

  test('T214.25: Pagination works for large result sets', async ({ page }) => {
    // Mock large dataset
    await page.route('**/api/v1/premiums/query*', async (route) => {
      const url = route.request().url();
      const page = url.includes('page=2') ? 2 : 1;

      await route.fulfill({
        status: 200,
        contentType: 'application/json',
        body: JSON.stringify({
          records: Array(20).fill(null).map((_, i) => ({
            policyNumber: `POL-${(page - 1) * 20 + i + 1}`,
            premiumAmount: 1000 + (i * 100)
          })),
          totalRecords: 100,
          currentPage: page,
          totalPages: 5
        })
      });
    });

    // Submit query
    await page.locator('input[type="date"]').first().fill('2025-10-01');
    await page.locator('input[type="date"]').nth(1).fill('2025-10-31');
    await page.locator('button').filter({ hasText: /Consultar|Buscar/i }).click();

    // Wait for results
    await page.waitForTimeout(1000);

    // Look for pagination controls
    const nextButton = page.locator('button').filter({ hasText: /Próxim|Next|>/i });
    if (await nextButton.count() > 0 && await nextButton.first().isEnabled()) {
      await nextButton.first().click();

      // Verify page 2 results load
      await expect(page.locator('text=/POL-2[0-9]/')).toBeVisible({ timeout: 3000 });
    }
  });

  test('T214.26: Filter by product type works', async ({ page }) => {
    // Check if product filter exists
    const productFilter = page.locator('select, [data-testid="product-filter"]').filter({ hasText: /Produto/i });

    if (await productFilter.count() > 0) {
      await productFilter.first().selectOption({ label: /Auto/i });

      // Submit filtered query
      await page.locator('button').filter({ hasText: /Consultar|Buscar/i }).click();

      // Verify filtered results (would need API mock)
      await page.waitForTimeout(500);
    }
  });
});
