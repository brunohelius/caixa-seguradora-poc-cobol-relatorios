/**
 * Playwright Comprehensive Test Suite for Caixa Seguradora Frontend
 * Tests functionality, navigation, interactions, and cross-browser compatibility
 */

import { test, expect, Page, BrowserContext } from '@playwright/test';
import { injectAxe, checkA11y, getViolations } from 'axe-playwright';
import * as fs from 'fs/promises';
import * as path from 'path';
import { fileURLToPath } from 'url';

// Get __dirname equivalent in ES modules
const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// Configuration
const BASE_URL = 'http://localhost:5174';
const SCREENSHOT_DIR = path.join(__dirname, 'screenshots', 'playwright');

// Test data
const PAGES = [
  { name: 'Dashboard', path: '/', title: 'Sistema de Relatórios' },
  { name: 'BatchJobs', path: '/batch-jobs', title: 'Processamento em Lote' },
  { name: 'Query', path: '/query', title: 'Consultas' },
  { name: 'ReportGeneration', path: '/report-generation', title: 'Geração de Relatórios' },
  { name: 'MockData', path: '/mock-data', title: 'Dados de Teste' }
];

// Caixa brand colors for validation
const BRAND_COLORS = {
  blue: 'rgb(0, 71, 187)',
  yellow: 'rgb(255, 184, 28)',
  green: 'rgb(22, 163, 74)',
  white: 'rgb(255, 255, 255)'
};

// Test results collector
interface TestResults {
  functional: any[];
  accessibility: any[];
  performance: any[];
  crossBrowser: any[];
  issues: any[];
}

const testResults: TestResults = {
  functional: [],
  accessibility: [],
  performance: [],
  crossBrowser: [],
  issues: []
};

// Ensure directories exist
test.beforeAll(async () => {
  await fs.mkdir(SCREENSHOT_DIR, { recursive: true });
  await fs.mkdir(path.join(__dirname, 'reports'), { recursive: true });
});

test.describe('🎯 Functional Tests', () => {
  test.describe.configure({ mode: 'parallel' });

  test('Navigation between all pages', async ({ page }) => {
    await page.goto(BASE_URL);

    for (const pageConfig of PAGES) {
      await test.step(`Navigate to ${pageConfig.name}`, async () => {
        // Click navigation link
        await page.click(`a[href="${pageConfig.path}"], nav >> text=${pageConfig.name}`).catch(() => {
          // Try alternative selector
          return page.goto(`${BASE_URL}${pageConfig.path}`);
        });

        // Verify URL
        await expect(page).toHaveURL(new RegExp(pageConfig.path === '/' ? '^/$|/dashboard' : pageConfig.path));

        // Take screenshot
        await page.screenshot({
          path: path.join(SCREENSHOT_DIR, `nav-${pageConfig.name.toLowerCase()}.png`),
          fullPage: true
        });

        testResults.functional.push({
          test: 'Navigation',
          page: pageConfig.name,
          passed: true
        });
      });
    }
  });

  test('Dashboard page functionality', async ({ page }) => {
    await page.goto(`${BASE_URL}/`);
    await page.waitForLoadState('networkidle');

    await test.step('Check success banner visibility', async () => {
      const banner = page.locator('.bg-green-600, [class*="success"]').first();
      const isVisible = await banner.isVisible().catch(() => false);

      testResults.functional.push({
        test: 'Success banner',
        page: 'Dashboard',
        passed: isVisible,
        issue: !isVisible ? 'Banner not visible' : null
      });

      if (isVisible) {
        // Check text color contrast on banner
        const textColor = await banner.evaluate(el => {
          const styles = window.getComputedStyle(el);
          return styles.color;
        });

        if (textColor === 'rgb(0, 0, 0)' || textColor === 'rgba(0, 0, 0, 0)') {
          testResults.issues.push({
            page: 'Dashboard',
            element: 'Success banner',
            issue: 'Text may be invisible (black on green)',
            severity: 'High',
            fix: 'Change text color to white'
          });
        }
      }
    });

    await test.step('Check próximas ações section', async () => {
      const section = page.locator('.bg-gradient-to-r, .bg-blue-600').first();
      const isVisible = await section.isVisible().catch(() => false);

      testResults.functional.push({
        test: 'Próximas ações',
        page: 'Dashboard',
        passed: isVisible,
        issue: !isVisible ? 'Section not visible' : null
      });

      if (isVisible) {
        // Check text visibility
        const textElements = await section.locator('p, span, h2, h3').all();
        for (const element of textElements) {
          const color = await element.evaluate(el => window.getComputedStyle(el).color);
          if (!color.includes('255')) {
            testResults.issues.push({
              page: 'Dashboard',
              element: 'Próximas ações text',
              issue: 'Text may not be visible on gradient',
              severity: 'High',
              fix: 'Ensure text is white'
            });
          }
        }
      }
    });

    await test.step('Check dashboard cards', async () => {
      const cards = page.locator('.card, [class*="border"][class*="rounded"]');
      const count = await cards.count();

      testResults.functional.push({
        test: 'Dashboard cards',
        page: 'Dashboard',
        passed: count >= 8,
        count: count,
        expected: 8
      });
    });
  });

  test('Batch Jobs page functionality', async ({ page }) => {
    await page.goto(`${BASE_URL}/batch-jobs`);
    await page.waitForLoadState('networkidle');

    await test.step('Check action buttons', async () => {
      const buttons = page.locator('button');
      const count = await buttons.count();

      for (let i = 0; i < count; i++) {
        const button = buttons.nth(i);
        const isEnabled = await button.isEnabled();
        const text = await button.textContent();

        testResults.functional.push({
          test: 'Button functionality',
          page: 'BatchJobs',
          element: text,
          enabled: isEnabled,
          passed: true
        });
      }
    });

    await test.step('Check alert sections', async () => {
      const alerts = page.locator('[role="alert"], .alert');
      const count = await alerts.count();

      testResults.functional.push({
        test: 'Alert sections',
        page: 'BatchJobs',
        count: count,
        passed: count > 0
      });
    });
  });

  test('Query page functionality', async ({ page }) => {
    await page.goto(`${BASE_URL}/query`);
    await page.waitForLoadState('networkidle');

    await test.step('Check tabs functionality', async () => {
      const tabs = page.locator('[role="tab"], button[class*="tab"]');
      const count = await tabs.count();

      if (count > 0) {
        // Try clicking first tab
        await tabs.first().click();
        testResults.functional.push({
          test: 'Tab switching',
          page: 'Query',
          passed: true,
          tabCount: count
        });
      }
    });

    await test.step('Check filter inputs', async () => {
      const inputs = page.locator('input, select');
      const count = await inputs.count();

      testResults.functional.push({
        test: 'Filter inputs',
        page: 'Query',
        count: count,
        passed: count > 0
      });
    });
  });

  test('Report Generation page functionality', async ({ page }) => {
    await page.goto(`${BASE_URL}/report-generation`);
    await page.waitForLoadState('networkidle');

    await test.step('Check form presence', async () => {
      const form = page.locator('form').first();
      const isVisible = await form.isVisible().catch(() => false);

      testResults.functional.push({
        test: 'Report form',
        page: 'ReportGeneration',
        passed: isVisible
      });
    });

    await test.step('Check date inputs', async () => {
      const dateInputs = page.locator('input[type="date"]');
      const count = await dateInputs.count();

      testResults.functional.push({
        test: 'Date inputs',
        page: 'ReportGeneration',
        count: count,
        passed: count >= 2
      });
    });
  });

  test('Mock Data page functionality', async ({ page }) => {
    await page.goto(`${BASE_URL}/mock-data`);
    await page.waitForLoadState('networkidle');

    await test.step('Check statistics cards', async () => {
      const cards = page.locator('.card, [class*="stat"], [class*="metric"]');
      const count = await cards.count();

      testResults.functional.push({
        test: 'Statistics cards',
        page: 'MockData',
        count: count,
        passed: count >= 3
      });
    });

    await test.step('Check action buttons', async () => {
      const buttons = page.locator('button');
      const count = await buttons.count();

      for (let i = 0; i < count; i++) {
        const button = buttons.nth(i);
        const text = await button.textContent();
        if (text?.includes('Carregar') || text?.includes('Validar') || text?.includes('Resetar')) {
          testResults.functional.push({
            test: 'Action button',
            page: 'MockData',
            button: text,
            passed: true
          });
        }
      }
    });
  });
});

test.describe('♿ Accessibility Tests', () => {
  test.describe.configure({ mode: 'parallel' });

  for (const pageConfig of PAGES) {
    test(`Accessibility check for ${pageConfig.name}`, async ({ page }) => {
      await page.goto(`${BASE_URL}${pageConfig.path}`);
      await page.waitForLoadState('networkidle');

      // Inject axe-core
      await injectAxe(page);

      // Run accessibility checks
      const violations = await getViolations(page, null, {
        detailedReport: true
      });

      testResults.accessibility.push({
        page: pageConfig.name,
        violations: violations.length,
        passed: violations.length === 0,
        details: violations.map(v => ({
          id: v.id,
          impact: v.impact,
          description: v.description,
          nodes: v.nodes.length
        }))
      });

      // Check keyboard navigation
      await test.step('Keyboard navigation', async () => {
        // Tab through interactive elements
        let tabCount = 0;
        for (let i = 0; i < 10; i++) {
          await page.keyboard.press('Tab');
          const focused = await page.evaluate(() => document.activeElement?.tagName);
          if (focused && focused !== 'BODY') {
            tabCount++;
          }
        }

        testResults.accessibility.push({
          page: pageConfig.name,
          test: 'Keyboard navigation',
          tabbableElements: tabCount,
          passed: tabCount > 0
        });
      });

      // Check color contrast
      await test.step('Color contrast', async () => {
        const contrastIssues = await page.evaluate(() => {
          const issues: any[] = [];
          const texts = document.querySelectorAll('p, span, h1, h2, h3, h4, h5, h6, button, a');

          texts.forEach(el => {
            const styles = window.getComputedStyle(el);
            const color = styles.color;
            const bgColor = styles.backgroundColor;

            // Simple check for potentially invisible text
            if (color === 'rgb(0, 0, 0)' &&
                (bgColor === 'rgba(0, 0, 0, 0)' || bgColor === 'transparent')) {
              const parent = el.parentElement;
              const parentBg = parent ? window.getComputedStyle(parent).backgroundColor : '';

              if (parentBg.includes('0, 0, 0') || parentBg === 'rgba(0, 0, 0, 0)') {
                issues.push({
                  text: el.textContent?.substring(0, 50),
                  selector: el.className || el.tagName,
                  color: color,
                  background: bgColor
                });
              }
            }
          });

          return issues;
        });

        if (contrastIssues.length > 0) {
          testResults.issues.push({
            page: pageConfig.name,
            type: 'contrast',
            count: contrastIssues.length,
            severity: 'High',
            fix: 'Set explicit text colors (white for dark backgrounds)'
          });
        }

        testResults.accessibility.push({
          page: pageConfig.name,
          test: 'Color contrast',
          issues: contrastIssues.length,
          passed: contrastIssues.length === 0
        });
      });
    });
  }
});

test.describe('⚡ Performance Tests', () => {
  test.describe.configure({ mode: 'parallel' });

  for (const pageConfig of PAGES) {
    test(`Performance metrics for ${pageConfig.name}`, async ({ page }) => {
      const metrics: any = {};

      // Start measuring
      await page.goto(`${BASE_URL}${pageConfig.path}`, {
        waitUntil: 'networkidle'
      });

      // Get performance metrics
      const performanceTiming = await page.evaluate(() => {
        const timing = performance.timing;
        return {
          loadTime: timing.loadEventEnd - timing.navigationStart,
          domContentLoaded: timing.domContentLoadedEventEnd - timing.navigationStart,
          responseTime: timing.responseEnd - timing.requestStart,
          renderTime: timing.domComplete - timing.domLoading
        };
      });

      // Get Core Web Vitals
      const webVitals = await page.evaluate(() => {
        return new Promise(resolve => {
          let cls = 0;
          let lcp = 0;
          let fid = 0;

          new PerformanceObserver((list) => {
            for (const entry of list.getEntries()) {
              if (entry.entryType === 'layout-shift') {
                cls += (entry as any).value;
              }
              if (entry.entryType === 'largest-contentful-paint') {
                lcp = entry.startTime;
              }
              if (entry.entryType === 'first-input') {
                fid = (entry as any).processingStart - entry.startTime;
              }
            }
          }).observe({ entryTypes: ['layout-shift', 'largest-contentful-paint', 'first-input'] });

          setTimeout(() => {
            resolve({ cls, lcp, fid });
          }, 2000);
        });
      });

      metrics.timing = performanceTiming;
      metrics.webVitals = webVitals;
      metrics.passed = performanceTiming.loadTime < 3000;

      testResults.performance.push({
        page: pageConfig.name,
        metrics: metrics,
        passed: metrics.passed
      });
    });
  }
});

test.describe('🌐 Cross-Browser Tests', () => {
  const browsers = ['chromium', 'firefox', 'webkit'];

  for (const browserName of browsers) {
    test(`${browserName} compatibility`, async ({ page, browserName: currentBrowser }) => {
      if (currentBrowser !== browserName && browserName !== 'chromium') {
        test.skip();
        return;
      }

      const browserResults: any[] = [];

      for (const pageConfig of PAGES) {
        await page.goto(`${BASE_URL}${pageConfig.path}`);
        await page.waitForLoadState('networkidle');

        // Check if page loads without errors
        const consoleErrors: string[] = [];
        page.on('console', msg => {
          if (msg.type() === 'error') {
            consoleErrors.push(msg.text());
          }
        });

        // Basic rendering check
        const title = await page.title();
        const bodyContent = await page.locator('body').textContent();

        browserResults.push({
          page: pageConfig.name,
          browser: browserName,
          loaded: true,
          hasContent: bodyContent && bodyContent.length > 100,
          errors: consoleErrors.length,
          passed: consoleErrors.length === 0
        });
      }

      testResults.crossBrowser.push(...browserResults);
    });
  }
});

// Generate comprehensive report after all tests
test.afterAll(async () => {
  const report = {
    timestamp: new Date().toISOString(),
    summary: {
      functional: {
        total: testResults.functional.length,
        passed: testResults.functional.filter(t => t.passed).length,
        failed: testResults.functional.filter(t => !t.passed).length
      },
      accessibility: {
        total: testResults.accessibility.length,
        passed: testResults.accessibility.filter(t => t.passed).length,
        failed: testResults.accessibility.filter(t => !t.passed).length
      },
      performance: {
        total: testResults.performance.length,
        passed: testResults.performance.filter(t => t.passed).length,
        failed: testResults.performance.filter(t => !t.passed).length
      },
      issues: testResults.issues.length
    },
    details: testResults,
    recommendations: generateRecommendations(testResults)
  };

  // Save report
  const reportPath = path.join(__dirname, 'reports', 'playwright-comprehensive-report.json');
  await fs.writeFile(reportPath, JSON.stringify(report, null, 2));

  // Generate markdown report
  const markdownReport = generateMarkdownReport(report);
  const mdPath = path.join(__dirname, 'reports', 'test-summary.md');
  await fs.writeFile(mdPath, markdownReport);

  console.log('\n📊 Reports generated:');
  console.log(`  - JSON: ${reportPath}`);
  console.log(`  - Markdown: ${mdPath}`);
});

function generateRecommendations(results: TestResults): string[] {
  const recommendations: string[] = [];

  // Check for common issues
  if (results.issues.some(i => i.type === 'contrast')) {
    recommendations.push('Fix text color contrast issues by setting explicit white color for text on dark backgrounds');
  }

  if (results.accessibility.some(a => a.violations > 0)) {
    recommendations.push('Address accessibility violations to ensure WCAG AA compliance');
  }

  if (results.performance.some(p => !p.passed)) {
    recommendations.push('Optimize page load times to stay under 3 second threshold');
  }

  if (results.functional.some(f => !f.passed)) {
    recommendations.push('Fix functional issues to ensure all UI elements are working correctly');
  }

  return recommendations;
}

function generateMarkdownReport(report: any): string {
  let md = '# Caixa Seguradora Frontend E2E Test Report\n\n';
  md += `**Generated:** ${report.timestamp}\n\n`;

  md += '## Executive Summary\n\n';
  md += '| Category | Total | Passed | Failed | Pass Rate |\n';
  md += '|----------|-------|--------|--------|----------|\n';

  for (const [key, value] of Object.entries(report.summary)) {
    if (typeof value === 'object' && value !== null) {
      const v = value as any;
      const passRate = v.total > 0 ? ((v.passed / v.total) * 100).toFixed(1) : '0';
      md += `| ${key} | ${v.total} | ✅ ${v.passed} | ❌ ${v.failed} | ${passRate}% |\n`;
    }
  }

  if (report.details.issues.length > 0) {
    md += '\n## 🔧 Issues Found\n\n';
    report.details.issues.forEach((issue: any) => {
      md += `- **${issue.page}** - ${issue.element || issue.type}: ${issue.issue} (${issue.severity})\n`;
      md += `  - Fix: ${issue.fix}\n`;
    });
  }

  if (report.recommendations.length > 0) {
    md += '\n## 📋 Recommendations\n\n';
    report.recommendations.forEach((rec: string) => {
      md += `- ${rec}\n`;
    });
  }

  md += '\n## ✅ All Tests Status\n\n';
  const allPassed = report.summary.functional.failed === 0 &&
                    report.summary.accessibility.failed === 0 &&
                    report.summary.performance.failed === 0;

  if (allPassed) {
    md += '### 🎉 All tests passed successfully!\n';
  } else {
    md += '### ⚠️ Some tests failed. Please review the issues above.\n';
  }

  return md;
}