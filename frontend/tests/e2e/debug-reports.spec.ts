import { test } from '@playwright/test';

test('Debug Reports Page', async ({ page }) => {
  const errors: string[] = [];
  const consoleMessages: string[] = [];

  page.on('pageerror', error => {
    errors.push('PAGE ERROR: ' + error.message);
    console.error('Page error:', error);
  });

  page.on('console', msg => {
    const text = msg.text();
    const type = msg.type();
    consoleMessages.push(type + ': ' + text);
    console.log('Console ' + type + ':', text);
  });

  await page.goto('http://localhost:5174/reports');
  await page.waitForTimeout(3000);

  console.log('\n=== ERRORS ===');
  errors.forEach(e => console.log(e));

  console.log('\n=== CONSOLE MESSAGES ===');
  consoleMessages.forEach(m => console.log(m));

  // Take screenshot
  await page.screenshot({ path: 'tests/e2e/screenshots/reports-debug.png', fullPage: true });
});
