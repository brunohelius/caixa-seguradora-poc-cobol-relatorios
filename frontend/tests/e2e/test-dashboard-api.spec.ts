import { test, expect } from '@playwright/test';

test.describe('Dashboard API Integration', () => {
  test.beforeEach(async ({ page }) => {
    // Enable console logging
    page.on('console', msg => console.log('BROWSER:', msg.text()));

    // Log network requests
    page.on('request', request => {
      if (request.url().includes('/api/')) {
        console.log('REQUEST:', request.method(), request.url());
      }
    });

    page.on('response', response => {
      if (response.url().includes('/api/')) {
        console.log('RESPONSE:', response.status(), response.url());
      }
    });
  });

  test('Dashboard loads and connects to API successfully', async ({ page }) => {
    // Navigate to dashboard
    await page.goto('http://localhost:5173');

    // Wait for page to load
    await page.waitForLoadState('networkidle');

    // Wait for dashboard content (with longer timeout)
    await page.waitForSelector('h1, h2, [data-testid="dashboard-page"]', {
      timeout: 10000
    });

    // Take screenshot
    await page.screenshot({ path: 'test-results/dashboard-loaded.png', fullPage: true });

    // Check if error message is displayed
    const errorMessage = await page.locator('text=/erro/i').first();
    const hasError = await errorMessage.isVisible().catch(() => false);

    if (hasError) {
      const errorText = await errorMessage.textContent();
      console.error('ERROR ON PAGE:', errorText);

      // Get all text content for debugging
      const bodyText = await page.locator('body').textContent();
      console.log('PAGE CONTENT:', bodyText);
    }

    // Verify no error message is shown
    await expect(errorMessage).not.toBeVisible();

    // Verify dashboard title or content is present
    const dashboardTitle = page.locator('h1, h2').first();
    await expect(dashboardTitle).toBeVisible();

    console.log('✅ Dashboard loaded successfully!');
  });

  test('API endpoints return correct data', async ({ page }) => {
    // Intercept API calls
    const apiResponses: any[] = [];

    page.on('response', async (response) => {
      if (response.url().includes('/api/v1/dashboard/')) {
        const url = response.url();
        const status = response.status();
        let data = null;

        try {
          data = await response.json();
        } catch (e) {
          console.error('Failed to parse JSON:', e);
        }

        apiResponses.push({ url, status, data });
        console.log('API RESPONSE:', { url, status, dataKeys: data ? Object.keys(data) : null });
      }
    });

    // Navigate to dashboard
    await page.goto('http://localhost:5173');

    // Wait for API calls to complete
    await page.waitForLoadState('networkidle');
    await page.waitForTimeout(3000); // Wait extra time for API calls

    // Verify we got API responses
    console.log('Total API responses captured:', apiResponses.length);

    expect(apiResponses.length).toBeGreaterThan(0);

    // Check each API response
    for (const response of apiResponses) {
      console.log('Checking response:', response.url);
      expect(response.status).toBe(200);
      expect(response.data).toBeTruthy();
    }

    console.log('✅ All API endpoints working correctly!');
  });
});
