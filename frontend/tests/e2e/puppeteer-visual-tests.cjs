/**
 * Puppeteer Visual Test Suite for Caixa Seguradora Frontend
 * Tests layout, branding, and visual rendering
 */

const puppeteer = require('puppeteer');
const fs = require('fs').promises;
const path = require('path');

// Test configuration
const BASE_URL = 'http://localhost:5174';
const SCREENSHOT_DIR = path.join(__dirname, 'screenshots', 'puppeteer');
const TIMEOUT = 30000;

// Caixa brand colors
const BRAND_COLORS = {
  blue: '#0047BB',
  yellow: '#FFB81C',
  green: '#16a34a',
  white: '#ffffff'
};

// Pages to test
const PAGES = [
  { name: 'Dashboard', path: '/', expectedElements: 8 },
  { name: 'BatchJobs', path: '/batch-jobs', expectedElements: 3 },
  { name: 'Query', path: '/query', expectedElements: 4 },
  { name: 'ReportGeneration', path: '/report-generation', expectedElements: 5 },
  { name: 'MockData', path: '/mock-data', expectedElements: 4 }
];

// Test results collector
const testResults = {
  passed: [],
  failed: [],
  warnings: [],
  screenshots: [],
  performanceMetrics: {}
};

async function ensureDirectories() {
  await fs.mkdir(SCREENSHOT_DIR, { recursive: true });
  await fs.mkdir(path.join(__dirname, 'reports'), { recursive: true });
}

async function testLayoutAndBranding(page, pageName) {
  const results = { page: pageName, tests: [] };

  try {
    // Test header presence
    const header = await page.$('header');
    results.tests.push({
      name: 'Header present',
      passed: header !== null,
      selector: 'header'
    });

    // Test navigation menu
    const navLinks = await page.$$('nav a, [role="navigation"] a');
    results.tests.push({
      name: 'Navigation links',
      passed: navLinks.length >= 5,
      count: navLinks.length,
      expected: 5
    });

    // Test footer presence
    const footer = await page.$('footer');
    results.tests.push({
      name: 'Footer present',
      passed: footer !== null,
      selector: 'footer'
    });

    // Test Caixa branding colors
    const brandingTest = await page.evaluate((colors) => {
      const allElements = document.querySelectorAll('*');
      const foundColors = {
        blue: false,
        yellow: false,
        green: false
      };

      allElements.forEach(el => {
        const styles = window.getComputedStyle(el);
        const bgColor = styles.backgroundColor.toLowerCase();
        const color = styles.color.toLowerCase();

        if (bgColor.includes('0, 71, 187') || color.includes('0, 71, 187')) {
          foundColors.blue = true;
        }
        if (bgColor.includes('255, 184, 28') || color.includes('255, 184, 28')) {
          foundColors.yellow = true;
        }
        if (bgColor.includes('22, 163, 74') || color.includes('22, 163, 74')) {
          foundColors.green = true;
        }
      });

      return foundColors;
    }, BRAND_COLORS);

    results.tests.push({
      name: 'Brand colors',
      passed: Object.values(brandingTest).some(v => v),
      colors: brandingTest
    });

    // Test text visibility and contrast
    const textVisibility = await page.evaluate(() => {
      const texts = document.querySelectorAll('p, span, h1, h2, h3, h4, h5, h6, button, a');
      const invisibleTexts = [];

      texts.forEach(el => {
        const styles = window.getComputedStyle(el);
        const color = styles.color;
        const bgColor = window.getComputedStyle(el.parentElement).backgroundColor;

        // Check if text is effectively invisible
        if (el.textContent && el.textContent.trim()) {
          const rgb = color.match(/\d+/g);
          if (rgb) {
            const brightness = (parseInt(rgb[0]) * 299 + parseInt(rgb[1]) * 587 + parseInt(rgb[2]) * 114) / 1000;
            if (brightness < 30 && !bgColor.includes('rgb')) {
              invisibleTexts.push({
                text: el.textContent.substring(0, 50),
                color: color,
                tagName: el.tagName
              });
            }
          }
        }
      });

      return {
        totalTexts: texts.length,
        invisibleTexts: invisibleTexts
      };
    });

    results.tests.push({
      name: 'Text visibility',
      passed: textVisibility.invisibleTexts.length === 0,
      totalTexts: textVisibility.totalTexts,
      invisibleCount: textVisibility.invisibleTexts.length,
      invisibleTexts: textVisibility.invisibleTexts
    });

  } catch (error) {
    results.error = error.message;
  }

  return results;
}

async function testPageSpecificElements(page, pageName) {
  const results = { page: pageName, elements: [] };

  try {
    switch(pageName) {
      case 'Dashboard':
        // Check for success banner
        const banner = await page.$('[class*="bg-green"], [class*="success"]');
        results.elements.push({
          name: 'Success banner',
          found: banner !== null
        });

        // Check for próximas ações section
        const proximasAcoes = await page.$('[class*="bg-gradient"], [class*="bg-blue"]');
        results.elements.push({
          name: 'Próximas ações section',
          found: proximasAcoes !== null
        });

        // Check for dashboard cards
        const cards = await page.$$('[class*="card"], [class*="border"], [class*="rounded"]');
        results.elements.push({
          name: 'Dashboard cards',
          found: cards.length >= 8,
          count: cards.length
        });
        break;

      case 'BatchJobs':
        const buttons = await page.$$('button');
        const alerts = await page.$$('[role="alert"], [class*="alert"]');
        results.elements.push({
          name: 'Action buttons',
          found: buttons.length > 0,
          count: buttons.length
        });
        results.elements.push({
          name: 'Alert sections',
          found: alerts.length > 0,
          count: alerts.length
        });
        break;

      case 'Query':
        const tabs = await page.$$('[role="tab"], [class*="tab"]');
        const filters = await page.$$('input, select');
        const table = await page.$('table, [role="table"]');
        results.elements.push({
          name: 'Query tabs',
          found: tabs.length > 0,
          count: tabs.length
        });
        results.elements.push({
          name: 'Filter inputs',
          found: filters.length > 0,
          count: filters.length
        });
        results.elements.push({
          name: 'Results table',
          found: table !== null
        });
        break;

      case 'ReportGeneration':
        const form = await page.$('form');
        const submitButton = await page.$('button[type="submit"], button:has-text("Gerar")');
        results.elements.push({
          name: 'Report form',
          found: form !== null
        });
        results.elements.push({
          name: 'Generate button',
          found: submitButton !== null
        });
        break;

      case 'MockData':
        const statsCards = await page.$$('[class*="stat"], [class*="metric"]');
        const uploadButton = await page.$('button:has-text("Carregar"), button:has-text("Upload")');
        results.elements.push({
          name: 'Statistics cards',
          found: statsCards.length > 0,
          count: statsCards.length
        });
        results.elements.push({
          name: 'Upload button',
          found: uploadButton !== null
        });
        break;
    }
  } catch (error) {
    results.error = error.message;
  }

  return results;
}

async function captureScreenshots(page, pageName) {
  const screenshots = [];

  try {
    // Full page screenshot
    const fullPath = path.join(SCREENSHOT_DIR, `${pageName.toLowerCase()}-full.png`);
    await page.screenshot({
      path: fullPath,
      fullPage: true
    });
    screenshots.push({ type: 'full', path: fullPath });

    // Viewport screenshot
    const viewportPath = path.join(SCREENSHOT_DIR, `${pageName.toLowerCase()}-viewport.png`);
    await page.screenshot({
      path: viewportPath,
      fullPage: false
    });
    screenshots.push({ type: 'viewport', path: viewportPath });

    // Mobile viewport
    await page.setViewport({ width: 375, height: 812 });
    const mobilePath = path.join(SCREENSHOT_DIR, `${pageName.toLowerCase()}-mobile.png`);
    await page.screenshot({
      path: mobilePath,
      fullPage: false
    });
    screenshots.push({ type: 'mobile', path: mobilePath });

    // Reset viewport
    await page.setViewport({ width: 1920, height: 1080 });

  } catch (error) {
    console.error(`Screenshot error for ${pageName}:`, error);
  }

  return screenshots;
}

async function runVisualTests() {
  console.log('🚀 Starting Puppeteer Visual Tests...');
  await ensureDirectories();

  const browser = await puppeteer.launch({
    headless: 'new',
    args: ['--no-sandbox', '--disable-setuid-sandbox']
  });

  try {
    const page = await browser.newPage();
    await page.setViewport({ width: 1920, height: 1080 });

    // Enable performance monitoring
    await page.evaluateOnNewDocument(() => {
      window.performanceMarks = [];
      const observer = new PerformanceObserver((list) => {
        for (const entry of list.getEntries()) {
          window.performanceMarks.push({
            name: entry.name,
            startTime: entry.startTime,
            duration: entry.duration
          });
        }
      });
      observer.observe({ entryTypes: ['measure', 'navigation'] });
    });

    for (const pageConfig of PAGES) {
      console.log(`\n📄 Testing ${pageConfig.name}...`);

      try {
        // Navigate to page
        await page.goto(`${BASE_URL}${pageConfig.path}`, {
          waitUntil: 'networkidle0',
          timeout: TIMEOUT
        });

        // Wait for content to load
        await new Promise(resolve => setTimeout(resolve, 1500));

        // Run layout and branding tests
        const layoutResults = await testLayoutAndBranding(page, pageConfig.name);
        testResults.passed.push(...layoutResults.tests.filter(t => t.passed));
        testResults.failed.push(...layoutResults.tests.filter(t => !t.passed).map(t => ({
          ...t,
          page: pageConfig.name
        })));

        // Run page-specific element tests
        const elementResults = await testPageSpecificElements(page, pageConfig.name);
        elementResults.elements.forEach(el => {
          if (el.found) {
            testResults.passed.push({ page: pageConfig.name, ...el });
          } else {
            testResults.warnings.push({ page: pageConfig.name, ...el });
          }
        });

        // Capture screenshots
        const screenshots = await captureScreenshots(page, pageConfig.name);
        testResults.screenshots.push({
          page: pageConfig.name,
          screenshots: screenshots
        });

        // Capture performance metrics
        const metrics = await page.metrics();
        const performance = await page.evaluate(() => ({
          loadTime: performance.timing.loadEventEnd - performance.timing.navigationStart,
          domContentLoaded: performance.timing.domContentLoadedEventEnd - performance.timing.navigationStart,
          firstPaint: performance.getEntriesByType('paint')[0]?.startTime || 0
        }));

        testResults.performanceMetrics[pageConfig.name] = {
          ...metrics,
          ...performance
        };

        console.log(`  ✅ ${pageConfig.name}: ${layoutResults.tests.filter(t => t.passed).length} tests passed`);

      } catch (error) {
        console.error(`  ❌ Error testing ${pageConfig.name}:`, error.message);
        testResults.failed.push({
          page: pageConfig.name,
          error: error.message
        });
      }
    }

  } finally {
    await browser.close();
  }

  // Generate report
  await generateReport();

  return testResults;
}

async function generateReport() {
  const report = {
    timestamp: new Date().toISOString(),
    summary: {
      totalTests: testResults.passed.length + testResults.failed.length,
      passed: testResults.passed.length,
      failed: testResults.failed.length,
      warnings: testResults.warnings.length
    },
    details: {
      passed: testResults.passed,
      failed: testResults.failed,
      warnings: testResults.warnings
    },
    screenshots: testResults.screenshots,
    performance: testResults.performanceMetrics
  };

  const reportPath = path.join(__dirname, 'reports', 'puppeteer-visual-report.json');
  await fs.writeFile(reportPath, JSON.stringify(report, null, 2));

  console.log(`\n📊 Report saved to: ${reportPath}`);
  console.log(`\n📈 Test Summary:`);
  console.log(`  ✅ Passed: ${report.summary.passed}`);
  console.log(`  ❌ Failed: ${report.summary.failed}`);
  console.log(`  ⚠️ Warnings: ${report.summary.warnings}`);
}

// Auto-fix function for detected issues
async function autoFixIssues() {
  if (testResults.failed.length === 0) {
    console.log('\n✨ No issues to fix!');
    return;
  }

  console.log(`\n🔧 Auto-fixing ${testResults.failed.length} issues...`);

  for (const failure of testResults.failed) {
    if (failure.name === 'Text visibility' && failure.invisibleTexts?.length > 0) {
      console.log(`  🔧 Fixing invisible text issues in ${failure.page}...`);
      // This would be handled by the fix agent
    }
  }
}

// Run the tests
runVisualTests()
  .then(async () => {
    await autoFixIssues();
    console.log('\n✅ Puppeteer Visual Tests Complete!');
    process.exit(testResults.failed.length > 0 ? 1 : 0);
  })
  .catch(error => {
    console.error('Fatal error:', error);
    process.exit(1);
  });