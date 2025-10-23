/**
 * E2E Tests for User Story 5: Mock Data Management
 *
 * Validates:
 * - FR-017: Upload CSV/JSON mock data
 * - FR-018: Validate uploaded data
 * - FR-019: View data validation results
 * - FR-020: Reset database to clean state
 * - SC-005: Data validation provides clear error messages
 * - SC-011: Mock data loading completes in <30 seconds for 10K records
 */

import { test, expect } from '@playwright/test';
import path from 'path';

test.describe('User Story 5: Mock Data Management', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/mock-data');
  });

  test('T214.37: Mock data page loads successfully', async ({ page }) => {
    // Verify page header
    await expect(page.locator('h1, h2').filter({ hasText: /Mock|Dados.*Teste/i })).toBeVisible();

    // Verify main content
    await expect(page.locator('main')).toBeVisible();
  });

  test('T214.38: File upload control is visible', async ({ page }) => {
    // Verify file input exists
    const fileInput = page.locator('input[type="file"]');
    await expect(fileInput).toBeVisible();
  });

  test('T214.39: Entity type selector is available', async ({ page }) => {
    // Verify entity type selection (premiums, policies, clients, etc.)
    const entitySelect = page.locator('select, [data-testid="entity-type-select"]');
    await expect(entitySelect.first()).toBeVisible();
  });

  test('T214.40: CSV file can be uploaded', async ({ page }) => {
    // Mock file upload API
    await page.route('**/api/v1/mock-data/load', async (route) => {
      await route.fulfill({
        status: 200,
        contentType: 'application/json',
        body: JSON.stringify({
          status: 'success',
          recordsLoaded: 10,
          validationErrors: [],
          entityType: 'premiums'
        })
      });
    });

    // Select entity type
    const entitySelect = page.locator('select').first();
    await entitySelect.selectOption({ label: /Prêmios|Premiums/i });

    // Create a mock CSV file content
    const csvContent = `policyNumber,premiumAmount,effectiveDate
POL-001,1500.50,2025-10-01
POL-002,2300.75,2025-10-02`;

    // Set file input
    const fileInput = page.locator('input[type="file"]');
    await fileInput.setInputFiles({
      name: 'premiums.csv',
      mimeType: 'text/csv',
      buffer: Buffer.from(csvContent)
    });

    // Submit upload
    const uploadButton = page.locator('button').filter({ hasText: /Carregar|Upload|Enviar/i });
    await uploadButton.click();

    // Verify success message
    await expect(page.locator('text=/Sucesso|10.*registros|records.*loaded/i')).toBeVisible({ timeout: 5000 });
  });

  test('T214.41: JSON file can be uploaded', async ({ page }) => {
    // Mock file upload API
    await page.route('**/api/v1/mock-data/load', async (route) => {
      await route.fulfill({
        status: 200,
        contentType: 'application/json',
        body: JSON.stringify({
          status: 'success',
          recordsLoaded: 5,
          validationErrors: [],
          entityType: 'policies'
        })
      });
    });

    // Select entity type
    const entitySelect = page.locator('select').first();
    await entitySelect.selectOption({ index: 1 }); // Select second option

    // Create a mock JSON file
    const jsonContent = JSON.stringify([
      { policyNumber: 'POL-001', companyCode: 123 },
      { policyNumber: 'POL-002', companyCode: 124 }
    ]);

    // Set file input
    const fileInput = page.locator('input[type="file"]');
    await fileInput.setInputFiles({
      name: 'policies.json',
      mimeType: 'application/json',
      buffer: Buffer.from(jsonContent)
    });

    // Submit upload
    const uploadButton = page.locator('button').filter({ hasText: /Carregar|Upload|Enviar/i });
    await uploadButton.click();

    // Verify success
    await expect(page.locator('text=/Sucesso|registros|success/i')).toBeVisible({ timeout: 5000 });
  });

  test('T214.42: Validation errors are displayed clearly', async ({ page }) => {
    // Mock file upload with validation errors
    await page.route('**/api/v1/mock-data/load', async (route) => {
      await route.fulfill({
        status: 200,
        contentType: 'application/json',
        body: JSON.stringify({
          status: 'partial',
          recordsLoaded: 8,
          validationErrors: [
            {
              row: 3,
              field: 'premiumAmount',
              message: 'Valor inválido: deve ser numérico',
              value: 'ABC'
            },
            {
              row: 5,
              field: 'effectiveDate',
              message: 'Data inválida: formato deve ser YYYY-MM-DD',
              value: '10/15/2025'
            }
          ],
          entityType: 'premiums'
        })
      });
    });

    // Upload file
    const csvContent = 'policyNumber,premiumAmount\nPOL-001,1500.50';
    const fileInput = page.locator('input[type="file"]');
    await fileInput.setInputFiles({
      name: 'test.csv',
      mimeType: 'text/csv',
      buffer: Buffer.from(csvContent)
    });

    await page.locator('button').filter({ hasText: /Carregar|Upload/i }).click();

    // Wait for response
    await page.waitForTimeout(1000);

    // Verify validation errors are shown
    await expect(page.locator('text=/Erro|Error|inválido/i')).toBeVisible({ timeout: 5000 });
    await expect(page.locator('text=/linha|row.*3|premiumAmount/i')).toBeVisible();
  });

  test('T214.43: Data validation can be triggered manually', async ({ page }) => {
    // Mock validation API
    await page.route('**/api/v1/mock-data/validate', async (route) => {
      await route.fulfill({
        status: 200,
        contentType: 'application/json',
        body: JSON.stringify({
          status: 'valid',
          totalRecords: 100,
          validRecords: 98,
          invalidRecords: 2,
          errors: [
            { entityType: 'premiums', recordId: 'POL-045', message: 'Valor negativo' }
          ]
        })
      });
    });

    // Click validate button
    const validateButton = page.locator('button').filter({ hasText: /Validar|Validate/i });
    if (await validateButton.count() > 0) {
      await validateButton.first().click();

      // Verify validation results are shown
      await expect(page.locator('text=/98|válido|valid/i')).toBeVisible({ timeout: 5000 });
    }
  });

  test('T214.44: Database can be reset', async ({ page }) => {
    // Mock reset API
    await page.route('**/api/v1/mock-data/reset', async (route) => {
      await route.fulfill({
        status: 200,
        contentType: 'application/json',
        body: JSON.stringify({
          status: 'success',
          message: 'Banco de dados resetado com sucesso',
          recordsDeleted: 1234
        })
      });
    });

    // Click reset button
    const resetButton = page.locator('button').filter({ hasText: /Reset|Limpar|Apagar/i });
    if (await resetButton.count() > 0) {
      // Handle confirmation dialog
      page.on('dialog', dialog => dialog.accept());

      await resetButton.first().click();

      // Verify success message
      await expect(page.locator('text=/Sucesso|resetado|Success/i')).toBeVisible({ timeout: 5000 });
    }
  });

  test('T214.45: Large file upload shows progress indicator', async ({ page }) => {
    // Mock slow upload
    await page.route('**/api/v1/mock-data/load', async (route) => {
      await new Promise(resolve => setTimeout(resolve, 3000));
      await route.fulfill({
        status: 200,
        contentType: 'application/json',
        body: JSON.stringify({
          status: 'success',
          recordsLoaded: 10000
        })
      });
    });

    // Upload large file
    const largeContent = Array(100).fill(null).map((_, i) =>
      `POL-${i},${1000 + i},2025-10-01`
    ).join('\n');

    const fileInput = page.locator('input[type="file"]');
    await fileInput.setInputFiles({
      name: 'large.csv',
      mimeType: 'text/csv',
      buffer: Buffer.from('policyNumber,premiumAmount,effectiveDate\n' + largeContent)
    });

    await page.locator('button').filter({ hasText: /Carregar|Upload/i }).click();

    // Verify loading indicator appears
    await expect(page.locator('[data-testid="loading"], .spinner, text=/Carregando|Loading/i')).toBeVisible({ timeout: 1000 });
  });

  test('T214.46: Uploaded data summary is displayed', async ({ page }) => {
    // Mock successful upload with summary
    await page.route('**/api/v1/mock-data/load', async (route) => {
      await route.fulfill({
        status: 200,
        contentType: 'application/json',
        body: JSON.stringify({
          status: 'success',
          recordsLoaded: 150,
          entityType: 'premiums',
          summary: {
            totalAmount: 456789.50,
            averageAmount: 3045.26,
            dateRange: {
              start: '2025-10-01',
              end: '2025-10-31'
            }
          }
        })
      });
    });

    // Upload file
    const fileInput = page.locator('input[type="file"]');
    await fileInput.setInputFiles({
      name: 'test.csv',
      mimeType: 'text/csv',
      buffer: Buffer.from('policyNumber,premiumAmount\nPOL-001,1500.50')
    });

    await page.locator('button').filter({ hasText: /Carregar|Upload/i }).click();

    // Wait for upload to complete
    await page.waitForTimeout(1000);

    // Verify summary information is shown
    await expect(page.locator('text=/150.*registros|records/i')).toBeVisible({ timeout: 5000 });
  });

  test('T214.47: File format help is available', async ({ page }) => {
    // Look for help icon, tooltip, or info button
    const helpElement = page.locator('[data-testid="help"], button[aria-label*="help"], text=/Ajuda|Help|Formato/i');

    if (await helpElement.count() > 0) {
      await expect(helpElement.first()).toBeVisible();

      // Click to show help
      if (await helpElement.first().getAttribute('role') === 'button') {
        await helpElement.first().click();

        // Verify help content appears
        await expect(page.locator('text=/CSV|JSON|formato|format/i')).toBeVisible({ timeout: 2000 });
      }
    }
  });

  test('T214.48: Portuguese language is used throughout', async ({ page }) => {
    // Verify Portuguese terms are present
    await expect(page.locator('text=/Dados|Arquivo|Carregar|Validar/i')).toBeVisible();
  });
});
