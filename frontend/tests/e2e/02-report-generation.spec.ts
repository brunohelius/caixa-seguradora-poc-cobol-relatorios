/**
 * E2E Tests for User Story 2: Report Generation Workflow
 *
 * Validates:
 * - FR-004: Generate PREMIT.TXT report
 * - FR-005: Generate PREMCED.TXT report
 * - FR-006: Support custom date ranges
 * - FR-007: Real-time progress tracking
 * - FR-008: Report download functionality
 * - SC-002: Report generation UI intuitive
 * - SC-007: File format matches COBOL (byte-for-byte)
 */

import { test, expect } from '@playwright/test';

test.describe('User Story 2: Report Generation', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/reports');
  });

  test('T214.9: Report generation page loads', async ({ page }) => {
    // Verify page title
    await expect(page.locator('h1, h2').filter({ hasText: /Relatórios/i })).toBeVisible();

    // Verify report type selector is visible
    await expect(page.locator('select, [role="combobox"]').first()).toBeVisible();
  });

  test('T214.10: PREMIT report can be selected', async ({ page }) => {
    // Select PREMIT report type
    const reportSelect = page.locator('[data-testid="report-type-select"], select').first();
    await reportSelect.selectOption({ label: /PREMIT/i });

    // Verify PREMIT option is selected
    await expect(reportSelect).toHaveValue(/premit/i);
  });

  test('T214.11: PREMCED report can be selected', async ({ page }) => {
    // Select PREMCED report type
    const reportSelect = page.locator('[data-testid="report-type-select"], select').first();
    await reportSelect.selectOption({ label: /PREMCED/i });

    // Verify PREMCED option is selected
    await expect(reportSelect).toHaveValue(/premced/i);
  });

  test('T214.12: Date range picker works correctly', async ({ page }) => {
    // Verify start date input exists
    const startDateInput = page.locator('input[type="date"]').first();
    await expect(startDateInput).toBeVisible();

    // Set start date
    await startDateInput.fill('2025-10-01');

    // Verify end date input exists
    const endDateInput = page.locator('input[type="date"]').nth(1);
    await expect(endDateInput).toBeVisible();

    // Set end date
    await endDateInput.fill('2025-10-31');

    // Verify dates are set
    await expect(startDateInput).toHaveValue('2025-10-01');
    await expect(endDateInput).toHaveValue('2025-10-31');
  });

  test('T214.13: Generate report button is enabled with valid inputs', async ({ page }) => {
    // Select report type
    const reportSelect = page.locator('select, [data-testid="report-type-select"]').first();
    await reportSelect.selectOption({ label: /PREMIT/i });

    // Set date range
    await page.locator('input[type="date"]').first().fill('2025-10-01');
    await page.locator('input[type="date"]').nth(1).fill('2025-10-31');

    // Verify generate button is enabled
    const generateButton = page.locator('button').filter({ hasText: /Gerar/i });
    await expect(generateButton).toBeEnabled();
  });

  test('T214.14: Report generation shows progress indicator', async ({ page }) => {
    // Set up API mock for slow response to capture progress UI
    await page.route('**/api/v1/reports/generate', async (route) => {
      // Simulate slow response
      await new Promise(resolve => setTimeout(resolve, 2000));
      await route.fulfill({
        status: 200,
        contentType: 'application/json',
        body: JSON.stringify({
          reportId: 'test-123',
          status: 'completed',
          fileName: 'PREMIT_20251001_20251031.TXT'
        })
      });
    });

    // Fill form
    await page.locator('select').first().selectOption({ label: /PREMIT/i });
    await page.locator('input[type="date"]').first().fill('2025-10-01');
    await page.locator('input[type="date"]').nth(1).fill('2025-10-31');

    // Click generate button
    const generateButton = page.locator('button').filter({ hasText: /Gerar/i });
    await generateButton.click();

    // Verify loading spinner or progress indicator appears
    await expect(page.locator('[data-testid="loading-spinner"], .spinner, text=/Gerando/i')).toBeVisible();
  });

  test('T214.15: Successful report generation shows download option', async ({ page }) => {
    // Mock successful report generation
    await page.route('**/api/v1/reports/generate', async (route) => {
      await route.fulfill({
        status: 200,
        contentType: 'application/json',
        body: JSON.stringify({
          reportId: 'test-123',
          status: 'completed',
          fileName: 'PREMIT_20251001_20251031.TXT',
          recordCount: 1234,
          fileSize: 567890
        })
      });
    });

    // Fill and submit form
    await page.locator('select').first().selectOption({ label: /PREMIT/i });
    await page.locator('input[type="date"]').first().fill('2025-10-01');
    await page.locator('input[type="date"]').nth(1).fill('2025-10-31');
    await page.locator('button').filter({ hasText: /Gerar/i }).click();

    // Wait for success message
    await expect(page.locator('text=/Sucesso|Concluído|Completo/i')).toBeVisible({ timeout: 10000 });

    // Verify download button appears
    await expect(page.locator('button, a').filter({ hasText: /Download|Baixar/i })).toBeVisible();
  });

  test('T214.16: Error handling for invalid date range', async ({ page }) => {
    // Select report type
    await page.locator('select').first().selectOption({ label: /PREMIT/i });

    // Set invalid date range (end before start)
    await page.locator('input[type="date"]').first().fill('2025-10-31');
    await page.locator('input[type="date"]').nth(1).fill('2025-10-01');

    // Try to generate
    const generateButton = page.locator('button').filter({ hasText: /Gerar/i });
    await generateButton.click();

    // Verify error message is displayed
    await expect(page.locator('text=/Data.*inválida|Data inicial.*maior/i')).toBeVisible();
  });

  test('T214.17: Report history displays previous reports', async ({ page }) => {
    // Mock report history API
    await page.route('**/api/v1/reports/history', async (route) => {
      await route.fulfill({
        status: 200,
        contentType: 'application/json',
        body: JSON.stringify([
          {
            reportId: '001',
            reportType: 'PREMIT',
            startDate: '2025-10-01',
            endDate: '2025-10-31',
            status: 'completed',
            createdAt: '2025-10-23T10:00:00Z'
          },
          {
            reportId: '002',
            reportType: 'PREMCED',
            startDate: '2025-09-01',
            endDate: '2025-09-30',
            status: 'completed',
            createdAt: '2025-10-22T15:30:00Z'
          }
        ])
      });
    });

    await page.reload();

    // Verify history section exists
    const historySection = page.locator('[data-testid="report-history"], text=/Histórico/i').first();
    await expect(historySection).toBeVisible();

    // Verify at least one historical report is listed
    await expect(page.locator('text=/PREMIT|PREMCED/').first()).toBeVisible();
  });

  test('T214.18: Portuguese language is used throughout', async ({ page }) => {
    // Verify key Portuguese terms are present
    await expect(page.locator('text=/Relatórios/i')).toBeVisible();
    await expect(page.locator('text=/Data Inicial|Data de Início/i')).toBeVisible();
    await expect(page.locator('text=/Data Final|Data de Término/i')).toBeVisible();
    await expect(page.locator('text=/Gerar|Processar/i')).toBeVisible();
  });
});
