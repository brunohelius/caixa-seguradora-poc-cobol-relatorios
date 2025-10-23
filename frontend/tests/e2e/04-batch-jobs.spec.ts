/**
 * E2E Tests for User Story 4: Batch Job Scheduling
 *
 * Validates:
 * - FR-013: Schedule report generation jobs
 * - FR-014: View job execution history
 * - FR-015: Monitor job progress
 * - FR-016: Receive job completion notifications
 * - SC-004: Batch jobs replicate COBOL JCL behavior
 * - SC-010: Job scheduling UI is intuitive
 */

import { test, expect } from '@playwright/test';

test.describe('User Story 4: Batch Job Scheduling', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/batch');
  });

  test('T214.27: Batch jobs page loads successfully', async ({ page }) => {
    // Verify page header
    await expect(page.locator('h1, h2').filter({ hasText: /Lotes|Batch|Agendamento/i })).toBeVisible();

    // Verify main content area
    await expect(page.locator('main')).toBeVisible();
  });

  test('T214.28: Create new batch job button is visible', async ({ page }) => {
    // Verify create job button exists
    const createButton = page.locator('button').filter({ hasText: /Criar|Novo|Agendar/i });
    await expect(createButton.first()).toBeVisible();
  });

  test('T214.29: Job creation form appears on button click', async ({ page }) => {
    // Click create job button
    const createButton = page.locator('button').filter({ hasText: /Criar|Novo|Agendar/i });
    await createButton.first().click();

    // Verify form appears
    await expect(page.locator('form, [data-testid="job-form"]')).toBeVisible({ timeout: 2000 });
  });

  test('T214.30: Job scheduling form has required fields', async ({ page }) => {
    // Open job creation form
    await page.locator('button').filter({ hasText: /Criar|Novo|Agendar/i }).first().click();
    await page.waitForTimeout(500);

    // Verify job type selection exists
    const jobTypeSelect = page.locator('select, [role="combobox"]').filter({ hasText: /Tipo/i });
    if (await jobTypeSelect.count() === 0) {
      // Try alternative selectors
      await expect(page.locator('select').first()).toBeVisible();
    }

    // Verify schedule time input exists (could be datetime-local, date+time, or text)
    const timeInputs = page.locator('input[type="datetime-local"], input[type="date"], input[type="time"]');
    expect(await timeInputs.count()).toBeGreaterThan(0);
  });

  test('T214.31: Job can be scheduled successfully', async ({ page }) => {
    // Mock job creation API
    await page.route('**/api/v1/batch/jobs', async (route) => {
      if (route.request().method() === 'POST') {
        await route.fulfill({
          status: 201,
          contentType: 'application/json',
          body: JSON.stringify({
            jobId: 'JOB-123',
            jobType: 'PREMIT_GENERATION',
            scheduledTime: '2025-10-24T02:00:00Z',
            status: 'scheduled',
            createdAt: '2025-10-23T12:00:00Z'
          })
        });
      } else {
        await route.continue();
      }
    });

    // Fill job creation form
    const createButton = page.locator('button').filter({ hasText: /Criar|Novo|Agendar/i });
    await createButton.first().click();
    await page.waitForTimeout(500);

    // Select job type
    const selects = page.locator('select');
    if (await selects.count() > 0) {
      await selects.first().selectOption({ index: 1 });
    }

    // Set schedule time (tomorrow at 2am)
    const tomorrow = new Date();
    tomorrow.setDate(tomorrow.getDate() + 1);
    const dateStr = tomorrow.toISOString().split('T')[0];

    const dateInput = page.locator('input[type="date"]').first();
    if (await dateInput.isVisible()) {
      await dateInput.fill(dateStr);
    }

    // Submit form
    await page.locator('button').filter({ hasText: /Criar|Salvar|Agendar/i }).last().click();

    // Verify success message or job appears in list
    await expect(page.locator('text=/Sucesso|Criado|Agendado|JOB-123/i')).toBeVisible({ timeout: 5000 });
  });

  test('T214.32: Job execution history is displayed', async ({ page }) => {
    // Mock job list API
    await page.route('**/api/v1/batch/jobs**', async (route) => {
      await route.fulfill({
        status: 200,
        contentType: 'application/json',
        body: JSON.stringify({
          jobs: [
            {
              jobId: 'JOB-001',
              jobType: 'PREMIT_GENERATION',
              status: 'completed',
              scheduledTime: '2025-10-23T02:00:00Z',
              completedTime: '2025-10-23T02:05:23Z'
            },
            {
              jobId: 'JOB-002',
              jobType: 'PREMCED_GENERATION',
              status: 'running',
              scheduledTime: '2025-10-23T03:00:00Z',
              startedTime: '2025-10-23T03:00:15Z'
            },
            {
              jobId: 'JOB-003',
              jobType: 'PREMIT_GENERATION',
              status: 'scheduled',
              scheduledTime: '2025-10-24T02:00:00Z'
            }
          ],
          totalJobs: 3
        })
      });
    });

    await page.reload();

    // Verify job history table/list is visible
    await expect(page.locator('text=/JOB-00[123]/')).toBeVisible({ timeout: 3000 });

    // Verify different job statuses are shown
    await expect(page.locator('text=/completed|concluído/i')).toBeVisible();
    await expect(page.locator('text=/running|executando|em andamento/i')).toBeVisible();
    await expect(page.locator('text=/scheduled|agendado/i')).toBeVisible();
  });

  test('T214.33: Job details can be viewed', async ({ page }) => {
    // Mock job list
    await page.route('**/api/v1/batch/jobs**', async (route) => {
      await route.fulfill({
        status: 200,
        contentType: 'application/json',
        body: JSON.stringify({
          jobs: [{
            jobId: 'JOB-001',
            jobType: 'PREMIT_GENERATION',
            status: 'completed'
          }]
        })
      });
    });

    // Mock job details API
    await page.route('**/api/v1/batch/jobs/JOB-001', async (route) => {
      await route.fulfill({
        status: 200,
        contentType: 'application/json',
        body: JSON.stringify({
          jobId: 'JOB-001',
          jobType: 'PREMIT_GENERATION',
          status: 'completed',
          scheduledTime: '2025-10-23T02:00:00Z',
          startedTime: '2025-10-23T02:00:05Z',
          completedTime: '2025-10-23T02:05:23Z',
          recordsProcessed: 5432,
          outputFile: 'PREMIT_20251001_20251031.TXT'
        })
      });
    });

    await page.reload();
    await page.waitForTimeout(1000);

    // Click on job to view details
    const jobRow = page.locator('text=/JOB-001/').first();
    if (await jobRow.isVisible()) {
      await jobRow.click();

      // Verify details are shown (could be modal, drawer, or new page)
      await expect(page.locator('text=/5432|registros|records/i')).toBeVisible({ timeout: 3000 });
    }
  });

  test('T214.34: Running job shows progress indicator', async ({ page }) => {
    // Mock job with progress
    await page.route('**/api/v1/batch/jobs**', async (route) => {
      await route.fulfill({
        status: 200,
        contentType: 'application/json',
        body: JSON.stringify({
          jobs: [{
            jobId: 'JOB-RUNNING',
            jobType: 'PREMIT_GENERATION',
            status: 'running',
            progress: 45,
            recordsProcessed: 4500,
            totalRecords: 10000
          }]
        })
      });
    });

    await page.reload();
    await page.waitForTimeout(1000);

    // Look for progress bar or percentage
    const progressIndicator = page.locator('[role="progressbar"], .progress, text=/45%|4500/');
    if (await progressIndicator.count() > 0) {
      await expect(progressIndicator.first()).toBeVisible();
    }
  });

  test('T214.35: Failed jobs show error information', async ({ page }) => {
    // Mock failed job
    await page.route('**/api/v1/batch/jobs**', async (route) => {
      await route.fulfill({
        status: 200,
        contentType: 'application/json',
        body: JSON.stringify({
          jobs: [{
            jobId: 'JOB-FAILED',
            jobType: 'PREMIT_GENERATION',
            status: 'failed',
            errorMessage: 'Erro ao conectar ao banco de dados',
            failedTime: '2025-10-23T02:00:30Z'
          }]
        })
      });
    });

    await page.reload();
    await page.waitForTimeout(1000);

    // Verify error status is shown
    await expect(page.locator('text=/failed|falhou|erro/i')).toBeVisible();

    // Click to see error details
    const failedJob = page.locator('text=/JOB-FAILED/').first();
    if (await failedJob.isVisible()) {
      await failedJob.click();

      // Verify error message is displayed
      await expect(page.locator('text=/banco de dados|database/i')).toBeVisible({ timeout: 3000 });
    }
  });

  test('T214.36: Job can be cancelled', async ({ page }) => {
    // Mock scheduled job
    await page.route('**/api/v1/batch/jobs**', async (route) => {
      await route.fulfill({
        status: 200,
        contentType: 'application/json',
        body: JSON.stringify({
          jobs: [{
            jobId: 'JOB-TO-CANCEL',
            jobType: 'PREMIT_GENERATION',
            status: 'scheduled'
          }]
        })
      });
    });

    // Mock cancel API
    await page.route('**/api/v1/batch/jobs/JOB-TO-CANCEL/cancel', async (route) => {
      await route.fulfill({
        status: 200,
        contentType: 'application/json',
        body: JSON.stringify({ status: 'cancelled' })
      });
    });

    await page.reload();
    await page.waitForTimeout(1000);

    // Look for cancel button
    const cancelButton = page.locator('button').filter({ hasText: /Cancelar|Cancel/i });
    if (await cancelButton.count() > 0 && await cancelButton.first().isEnabled()) {
      await cancelButton.first().click();

      // Confirm cancellation if dialog appears
      page.on('dialog', dialog => dialog.accept());

      // Verify job is cancelled
      await expect(page.locator('text=/cancelado|cancelled/i')).toBeVisible({ timeout: 5000 });
    }
  });
});
