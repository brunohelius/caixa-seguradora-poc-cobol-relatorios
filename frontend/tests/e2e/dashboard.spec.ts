import { test, expect } from '@playwright/test';

test.describe('Dashboard Page', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('http://localhost:5173');
  });

  test('should load the application', async ({ page }) => {
    // Wait for the page to load
    await page.waitForLoadState('networkidle');

    // Check if the page is loaded
    await expect(page).toHaveTitle(/frontend/);
  });

  test('should display dashboard elements', async ({ page }) => {
    // Wait for content to load
    await page.waitForLoadState('networkidle');
    await page.waitForTimeout(2000); // Give time for React to render

    // Take a screenshot
    await page.screenshot({ path: 'tests/e2e/screenshots/dashboard.png', fullPage: true });
  });

  test('should be responsive', async ({ page }) => {
    // Test mobile viewport
    await page.setViewportSize({ width: 375, height: 667 });
    await page.waitForLoadState('networkidle');
    await page.screenshot({ path: 'tests/e2e/screenshots/dashboard-mobile.png', fullPage: true });

    // Test tablet viewport
    await page.setViewportSize({ width: 768, height: 1024 });
    await page.waitForLoadState('networkidle');
    await page.screenshot({ path: 'tests/e2e/screenshots/dashboard-tablet.png', fullPage: true });

    // Test desktop viewport
    await page.setViewportSize({ width: 1920, height: 1080 });
    await page.waitForLoadState('networkidle');
    await page.screenshot({ path: 'tests/e2e/screenshots/dashboard-desktop.png', fullPage: true });
  });
});

test.describe('API Integration', () => {
  test('should fetch dashboard metrics from API', async ({ page }) => {
    // Intercept API call
    const responsePromise = page.waitForResponse(
      response => response.url().includes('/api/dashboard/metrics') && response.status() === 200
    );

    await page.goto('http://localhost:5173');

    // Wait for the API response
    const response = await responsePromise;
    const data = await response.json();

    // Verify the response structure
    expect(data).toHaveProperty('programInfo');
    expect(data.programInfo).toHaveProperty('programName', 'RG1866B');
    expect(data).toHaveProperty('dataStructure');
    expect(data.dataStructure).toHaveProperty('totalDataItems', 687);
  });
});
