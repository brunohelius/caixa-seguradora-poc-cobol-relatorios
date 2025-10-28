import { test, expect } from '@playwright/test';

/**
 * E2E tests for Report Generation feature (003-complete-cobol-migration).
 * Tests the complete flow: form submission, status polling, and file download.
 *
 * Prerequisites:
 * - Backend API must be running on http://localhost:5555
 * - Frontend must be running on http://localhost:5173
 * - Database must have sample data for month 202510
 */

test.describe('Report Generation V2 - Complete Flow', () => {
  test.beforeEach(async ({ page }) => {
    // Navigate to report generation page
    await page.goto('http://localhost:5173/reports-v2');
  });

  test('should display page title and description in Portuguese', async ({ page }) => {
    // Check page title
    await expect(page.locator('h1')).toContainText('Geração de Relatórios SUSEP');

    // Check page description
    await expect(page.locator('p')).toContainText('Circular SUSEP 360/2008');
  });

  test('should validate month input format', async ({ page }) => {
    // Find month input
    const monthInput = page.locator('input#month');

    // Try invalid format (too short)
    await monthInput.fill('2025');
    await page.locator('button[type="submit"]').click();

    // Should show validation error
    await expect(page.locator('text=Formato inválido')).toBeVisible();

    // Try invalid format (wrong pattern)
    await monthInput.fill('10-2025');
    await page.locator('button[type="submit"]').click();

    // Should show validation error
    await expect(page.locator('text=Formato inválido')).toBeVisible();
  });

  test('should reject future months', async ({ page }) => {
    // Calculate future month (next year)
    const futureDate = new Date();
    futureDate.setFullYear(futureDate.getFullYear() + 1);
    const futureMonth = `${futureDate.getFullYear()}${String(futureDate.getMonth() + 1).padStart(2, '0')}`;

    // Fill form with future month
    await page.locator('input#month').fill(futureMonth);
    await page.locator('button[type="submit"]').click();

    // Should show validation error
    await expect(page.locator('text=meses futuros')).toBeVisible();
  });

  test('should accept valid month in YYYYMM format', async ({ page }) => {
    // Fill valid month
    const monthInput = page.locator('input#month');
    await monthInput.fill('202510');

    // Should show formatted month name in Portuguese
    await expect(page.locator('text=Outubro 2025')).toBeVisible();

    // Validation error should not appear
    await expect(page.locator('text=Formato inválido')).not.toBeVisible();
  });

  test('should allow selecting report type with radio buttons', async ({ page }) => {
    // Check that "Ambos os Relatórios" is selected by default
    const bothRadio = page.locator('input[value="BOTH"]');
    await expect(bothRadio).toBeChecked();

    // Select PREMIT
    await page.locator('text=PREMIT - Prêmios Emitidos').click();
    const premitRadio = page.locator('input[value="PREMIT"]');
    await expect(premitRadio).toBeChecked();

    // Select PREMCED
    await page.locator('text=PREMCED - Prêmios Cedidos').click();
    const premcedRadio = page.locator('input[value="PREMCED"]');
    await expect(premcedRadio).toBeChecked();
  });

  test('should submit form and start report generation', async ({ page }) => {
    // Fill form
    await page.locator('input#month').fill('202510');
    await page.locator('input[value="BOTH"]').check();

    // Submit form
    await page.locator('button[type="submit"]').click();

    // Button should show "Gerando Relatório..."
    await expect(page.locator('button[type="submit"]')).toContainText('Gerando Relatório...');

    // Wait for status card to appear
    await expect(page.locator('text=Status')).toBeVisible({ timeout: 5000 });

    // Should show either PENDING or RUNNING status
    await expect(
      page.locator('text=Pendente').or(page.locator('text=Processando'))
    ).toBeVisible({ timeout: 10000 });
  });

  test('should poll status and update progress', async ({ page }) => {
    // Submit report
    await page.locator('input#month').fill('202510');
    await page.locator('button[type="submit"]').click();

    // Wait for status card
    await expect(page.locator('text=Status')).toBeVisible({ timeout: 5000 });

    // Progress bar should appear
    await expect(page.locator('text=Progresso')).toBeVisible({ timeout: 5000 });

    // Records processed count should be visible
    await expect(page.locator('text=registros processados')).toBeVisible();

    // Elapsed time should be visible
    await expect(page.locator('text=Tempo Decorrido')).toBeVisible();

    // Wait for progress to update (give it 10 seconds for polling)
    await page.waitForTimeout(10000);

    // Progress percentage should have changed or status completed
    const statusCompleted = page.locator('text=Concluído');
    const statusRunning = page.locator('text=Processando');

    await expect(statusCompleted.or(statusRunning)).toBeVisible();
  });

  test('should show download buttons when completed', async ({ page }) => {
    // Submit report
    await page.locator('input#month').fill('202510');
    await page.locator('button[type="submit"]').click();

    // Wait for completion (max 2 minutes as per spec)
    await expect(page.locator('text=Concluído')).toBeVisible({ timeout: 120000 });

    // Download buttons should appear
    await expect(page.locator('text=Baixar PREMIT.TXT')).toBeVisible();
    await expect(page.locator('text=Baixar PREMCED.TXT')).toBeVisible();

    // "Gerar Novo Relatório" button should appear
    await expect(page.locator('text=Gerar Novo Relatório')).toBeVisible();
  });

  test('should trigger file download when clicking download button', async ({ page }) => {
    // Submit and wait for completion
    await page.locator('input#month').fill('202510');
    await page.locator('button[type="submit"]').click();
    await expect(page.locator('text=Concluído')).toBeVisible({ timeout: 120000 });

    // Set up download listener
    const downloadPromise = page.waitForEvent('download');

    // Click download button
    await page.locator('text=Baixar PREMIT.TXT').click();

    // Wait for download to start
    const download = await downloadPromise;

    // Verify filename
    expect(download.suggestedFilename()).toMatch(/PREMIT_202510\.TXT/);
  });

  test('should display execution history table', async ({ page }) => {
    // Execution history should be visible
    await expect(page.locator('text=Histórico de Execuções')).toBeVisible();

    // Table headers should be in Portuguese
    await expect(page.locator('text=Mês')).toBeVisible();
    await expect(page.locator('text=Status')).toBeVisible();
    await expect(page.locator('text=Data/Hora Início')).toBeVisible();
    await expect(page.locator('text=Registros')).toBeVisible();
    await expect(page.locator('text=Ações')).toBeVisible();
  });

  test('should paginate execution history', async ({ page }) => {
    // Check if pagination exists (only if there are multiple pages)
    const paginationExists = await page.locator('text=Página').isVisible();

    if (paginationExists) {
      // Click next page
      await page.locator('text=Próxima').click();

      // Page number should update
      await expect(page.locator('text=Página 2')).toBeVisible();

      // Click previous page
      await page.locator('text=Anterior').click();

      // Should be back to page 1
      await expect(page.locator('text=Página 1')).toBeVisible();
    }
  });

  test('should download file from execution history', async ({ page }) => {
    // Wait for history to load
    await page.waitForTimeout(2000);

    // Find a completed execution in history
    const downloadButton = page.locator('text=Baixar').first();

    if (await downloadButton.isVisible()) {
      // Set up download listener
      const downloadPromise = page.waitForEvent('download');

      // Click download from history
      await downloadButton.click();

      // Wait for download
      const download = await downloadPromise;

      // Verify it's a .TXT file
      expect(download.suggestedFilename()).toMatch(/\.(TXT|txt)$/);
    }
  });

  test('should show return code and description when completed', async ({ page }) => {
    // Submit and wait for completion
    await page.locator('input#month').fill('202510');
    await page.locator('button[type="submit"]').click();
    await expect(page.locator('text=Concluído')).toBeVisible({ timeout: 120000 });

    // Return code should be visible
    await expect(page.locator('text=Código de Retorno')).toBeVisible();

    // Should show one of the expected return codes
    await expect(
      page.locator('text=0000').or(page.locator('text=0004'))
    ).toBeVisible();

    // Should show Portuguese description
    await expect(
      page.locator('text=Sucesso').or(page.locator('text=avisos'))
    ).toBeVisible();
  });

  test('should reset form when clicking "Gerar Novo Relatório"', async ({ page }) => {
    // Submit and wait for completion
    await page.locator('input#month').fill('202510');
    await page.locator('button[type="submit"]').click();
    await expect(page.locator('text=Concluído')).toBeVisible({ timeout: 120000 });

    // Click "Gerar Novo Relatório"
    await page.locator('text=Gerar Novo Relatório').click();

    // Form should reappear
    await expect(page.locator('input#month')).toBeVisible();
    await expect(page.locator('button[type="submit"]')).toBeVisible();

    // Download buttons should be hidden
    await expect(page.locator('text=Baixar PREMIT.TXT')).not.toBeVisible();
  });

  test('should display instructions when no report is active', async ({ page }) => {
    // Instructions should be visible
    await expect(page.locator('text=Como usar este sistema')).toBeVisible();

    // Should have 6 steps in Portuguese
    await expect(page.locator('text=Preencha o mês de referência')).toBeVisible();
    await expect(page.locator('text=Selecione o tipo de relatório')).toBeVisible();
    await expect(page.locator('text=Clique em')).toBeVisible();
    await expect(page.locator('text=Acompanhe o progresso')).toBeVisible();
    await expect(page.locator('text=Faça download')).toBeVisible();
    await expect(page.locator('text=Acesse relatórios anteriores')).toBeVisible();
  });

  test('should show error message on network failure', async ({ page }) => {
    // Intercept API call and force network error
    await page.route('**/api/v1/reports/generate', route => route.abort('failed'));

    // Fill and submit form
    await page.locator('input#month').fill('202510');
    await page.locator('button[type="submit"]').click();

    // Should show network error message in Portuguese
    await expect(page.locator('text=Erro de conexão')).toBeVisible({ timeout: 5000 });
  });

  test('should update elapsed time every second while processing', async ({ page }) => {
    // Submit report
    await page.locator('input#month').fill('202510');
    await page.locator('button[type="submit"]').click();

    // Wait for processing to start
    await expect(page.locator('text=Tempo Decorrido')).toBeVisible({ timeout: 10000 });

    // Get initial elapsed time
    const elapsedTimeLocator = page.locator('text=Tempo Decorrido').locator('..').locator('span').last();
    const initialTime = await elapsedTimeLocator.textContent();

    // Wait 3 seconds
    await page.waitForTimeout(3000);

    // Elapsed time should have changed
    const updatedTime = await elapsedTimeLocator.textContent();
    expect(updatedTime).not.toBe(initialTime);
  });
});

test.describe('Report Generation V2 - Visual Regression', () => {
  test('should match visual snapshot of form', async ({ page }) => {
    await page.goto('http://localhost:5173/reports-v2');

    // Take screenshot of form
    await expect(page).toHaveScreenshot('report-form-v2.png', {
      fullPage: false,
      maxDiffPixels: 100,
    });
  });

  test('should match visual snapshot of completed report', async ({ page }) => {
    await page.goto('http://localhost:5173/reports-v2');

    // Submit report and wait for completion
    await page.locator('input#month').fill('202510');
    await page.locator('button[type="submit"]').click();
    await expect(page.locator('text=Concluído')).toBeVisible({ timeout: 120000 });

    // Take screenshot
    await expect(page).toHaveScreenshot('report-completed-v2.png', {
      fullPage: true,
      maxDiffPixels: 200,
    });
  });
});
