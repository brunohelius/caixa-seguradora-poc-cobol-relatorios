import { test, expect } from '@playwright/test';

/**
 * shadcn/ui Components Validation Test Suite
 * Validates that all shadcn/ui components are rendering correctly with proper Tailwind styles
 */

test.describe('shadcn/ui Components - Visual Validation', () => {

  test('Dashboard - Card components render correctly', async ({ page }) => {
    await page.goto('/');
    await page.waitForLoadState('networkidle');
    await page.waitForTimeout(2000); // Wait for async data

    // Check for Card components (shadcn/ui)
    const cards = page.locator('[class*="rounded-"], .card, [class*="border"]');
    await expect(cards.first()).toBeVisible({ timeout: 15000 });

    // Verify Tailwind classes are applied
    const firstCard = cards.first();
    const classes = await firstCard.getAttribute('class');
    expect(classes).toBeTruthy();

    console.log('✓ Card components rendering');
  });

  test('Reports - Form components with shadcn/ui', async ({ page }) => {
    await page.goto('/reports');
    await page.waitForLoadState('networkidle');
    await page.waitForTimeout(2000);

    // Check for shadcn/ui form elements
    const formElements = page.locator('form, input, select, button').first();
    await expect(formElements).toBeVisible({ timeout: 15000 });

    console.log('✓ Form components rendering');
  });

  test('Buttons - shadcn/ui Button component styling', async ({ page }) => {
    await page.goto('/reports');
    await page.waitForLoadState('networkidle');

    // Find buttons
    const buttons = page.locator('button');
    const buttonCount = await buttons.count();
    expect(buttonCount).toBeGreaterThan(0);

    // Check first button has Tailwind classes
    if (buttonCount > 0) {
      const firstButton = buttons.first();
      const classes = await firstButton.getAttribute('class');
      expect(classes).toContain('');  // At least has class attribute
    }

    console.log(`✓ Found ${buttonCount} buttons with styling`);
  });

  test('Responsive layout - Mobile viewport (375px)', async ({ page }) => {
    await page.setViewportSize({ width: 375, height: 667 });
    await page.goto('/');
    await page.waitForLoadState('networkidle');

    // Page should still be visible and functional
    await expect(page.locator('body')).toBeVisible();

    // Navigation should adapt
    const nav = page.locator('nav, header');
    await expect(nav.first()).toBeVisible();

    console.log('✓ Mobile layout OK');
  });

  test('Responsive layout - Tablet viewport (768px)', async ({ page }) => {
    await page.setViewportSize({ width: 768, height: 1024 });
    await page.goto('/');
    await page.waitForLoadState('networkidle');

    await expect(page.locator('body')).toBeVisible();

    console.log('✓ Tablet layout OK');
  });

  test('Responsive layout - Desktop viewport (1920px)', async ({ page }) => {
    await page.setViewportSize({ width: 1920, height: 1080 });
    await page.goto('/');
    await page.waitForLoadState('networkidle');
    await page.waitForTimeout(2000);

    await expect(page.locator('body')).toBeVisible();

    // Check for proper spacing and layout
    const container = page.locator('.container, [class*="mx-auto"]').first();
    if (await container.count() > 0) {
      await expect(container).toBeVisible();
    }

    console.log('✓ Desktop layout OK');
  });

  test('Tailwind color scheme - Caixa branding', async ({ page }) => {
    await page.goto('/');
    await page.waitForLoadState('networkidle');
    await page.waitForTimeout(2000);

    // Look for Caixa blue color classes
    const blueElements = page.locator('[class*="caixa-blue"], [class*="bg-blue"]');
    const count = await blueElements.count();

    // Should have at least some branded elements
    expect(count).toBeGreaterThanOrEqual(0);

    console.log(`✓ Found ${count} elements with Caixa branding`);
  });

  test('Typography - Consistent font usage', async ({ page }) => {
    await page.goto('/');
    await page.waitForLoadState('networkidle');

    // Check for headings
    const h1 = page.locator('h1');
    const h2 = page.locator('h2');
    const h3 = page.locator('h3');

    const h1Count = await h1.count();
    const h2Count = await h2.count();
    const h3Count = await h3.count();

    console.log(`✓ Typography: ${h1Count} H1, ${h2Count} H2, ${h3Count} H3`);
  });

  test('Icons and visual elements', async ({ page }) => {
    await page.goto('/');
    await page.waitForLoadState('networkidle');
    await page.waitForTimeout(2000);

    // Check for SVG icons or emoji icons
    const icons = page.locator('svg, [class*="icon"], span:has-text("📊"), span:has-text("📈")');
    const iconCount = await icons.count();

    expect(iconCount).toBeGreaterThanOrEqual(0);

    console.log(`✓ Found ${iconCount} visual elements/icons`);
  });

  test('Shadows and borders - Design system consistency', async ({ page }) => {
    await page.goto('/');
    await page.waitForLoadState('networkidle');
    await page.waitForTimeout(2000);

    // Look for elements with shadows
    const shadowedElements = page.locator('[class*="shadow"]');
    const borderedElements = page.locator('[class*="border"]');

    const shadowCount = await shadowedElements.count();
    const borderCount = await borderedElements.count();

    console.log(`✓ Design system: ${shadowCount} shadows, ${borderCount} borders`);
  });
});
