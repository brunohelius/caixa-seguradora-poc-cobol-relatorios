#!/usr/bin/env node
/**
 * Auto-Fix Agent for Caixa Seguradora Frontend
 * Monitors test results and automatically fixes detected issues
 */

const fs = require('fs').promises;
const path = require('path');

// Paths
const FRONTEND_SRC = path.join(__dirname, '../../src');
const REPORTS_DIR = path.join(__dirname, 'reports');
const FIXES_LOG = path.join(REPORTS_DIR, 'auto-fixes.log');

// Issue patterns and fixes
const ISSUE_FIXES = {
  // Text visibility issues
  'invisible_text': {
    pattern: /color:\s*['"]?(rgb\(0,\s*0,\s*0\)|black|#000000?)['"]?/gi,
    fix: 'color: "white"',
    description: 'Fixed invisible text by changing color to white'
  },
  'text_on_green': {
    pattern: /(?:bg-green-\d+|bg-success).*?\{[^}]*color:[^;}]*\}/g,
    fix: (match) => match.replace(/color:[^;}]*/, 'color: white'),
    description: 'Fixed text color on green background'
  },
  'text_on_blue': {
    pattern: /(?:bg-blue-\d+|bg-gradient-to-r).*?\{[^}]*color:[^;}]*\}/g,
    fix: (match) => match.replace(/color:[^;}]*/, 'color: white'),
    description: 'Fixed text color on blue gradient'
  },
  // Missing explicit colors
  'missing_text_color': {
    pattern: /className=["'][^"']*(?:bg-green|bg-blue|bg-gradient)[^"']*["'][^>]*>(?!.*(?:text-white|text-gray))/g,
    fix: (match) => match.replace(/className=["']/, 'className="text-white '),
    description: 'Added explicit text-white class'
  }
};

// File patterns to check
const FILE_PATTERNS = [
  'pages/DashboardPage.tsx',
  'pages/BatchJobsPage.tsx',
  'pages/QueryPage.tsx',
  'pages/ReportGenerationPage.tsx',
  'pages/MockDataPage.tsx',
  'components/**/*.tsx',
  'components/**/*.jsx'
];

class AutoFixAgent {
  constructor() {
    this.fixes = [];
    this.filesFixed = new Set();
  }

  async initialize() {
    await fs.mkdir(REPORTS_DIR, { recursive: true });
    console.log('🤖 Auto-Fix Agent initialized');
  }

  async monitorTestResults() {
    const reportFiles = [
      path.join(REPORTS_DIR, 'puppeteer-visual-report.json'),
      path.join(REPORTS_DIR, 'playwright-comprehensive-report.json')
    ];

    const issues = [];

    for (const reportFile of reportFiles) {
      try {
        const content = await fs.readFile(reportFile, 'utf-8');
        const report = JSON.parse(content);

        // Extract issues from report
        if (report.details?.issues) {
          issues.push(...report.details.issues);
        }
        if (report.details?.failed) {
          issues.push(...report.details.failed);
        }
      } catch (error) {
        // Report might not exist yet
        console.log(`  ⏳ Waiting for ${path.basename(reportFile)}...`);
      }
    }

    return issues;
  }

  async fixTextVisibilityIssues() {
    console.log('\n🔧 Fixing text visibility issues...');

    const pagesToFix = [
      {
        file: path.join(FRONTEND_SRC, 'pages/DashboardPage.tsx'),
        fixes: [
          {
            // Fix success banner text
            search: /<div className="bg-green-600[^>]*>([\s\S]*?)<\/div>/g,
            replace: (match, content) => {
              if (!match.includes('text-white')) {
                return match.replace('className="', 'className="text-white ');
              }
              return match;
            },
            description: 'Added text-white to success banner'
          },
          {
            // Fix próximas ações text
            search: /<div className="bg-gradient-to-r[^>]*>([\s\S]*?)<\/div>/g,
            replace: (match, content) => {
              if (!match.includes('text-white')) {
                return match.replace('className="', 'className="text-white ');
              }
              return match;
            },
            description: 'Added text-white to gradient section'
          }
        ]
      }
    ];

    for (const pageConfig of pagesToFix) {
      try {
        let content = await fs.readFile(pageConfig.file, 'utf-8');
        let modified = false;

        for (const fix of pageConfig.fixes) {
          const originalContent = content;
          if (typeof fix.replace === 'function') {
            content = content.replace(fix.search, fix.replace);
          } else {
            content = content.replace(fix.search, fix.replace);
          }

          if (content !== originalContent) {
            modified = true;
            this.fixes.push({
              file: pageConfig.file,
              description: fix.description,
              timestamp: new Date().toISOString()
            });
          }
        }

        if (modified) {
          await fs.writeFile(pageConfig.file, content);
          this.filesFixed.add(pageConfig.file);
          console.log(`  ✅ Fixed: ${path.basename(pageConfig.file)}`);
        }
      } catch (error) {
        console.error(`  ❌ Error fixing ${pageConfig.file}:`, error.message);
      }
    }
  }

  async fixSpecificComponents() {
    console.log('\n🔧 Checking and fixing component issues...');

    // Fix specific known issues in components
    const componentFixes = [
      {
        path: path.join(FRONTEND_SRC, 'components/common/Button.tsx'),
        check: async (content) => {
          // Ensure buttons have proper contrast
          return content.includes('disabled:opacity-50');
        },
        fix: async (content) => {
          if (!content.includes('disabled:cursor-not-allowed')) {
            return content.replace(
              'disabled:opacity-50',
              'disabled:opacity-50 disabled:cursor-not-allowed'
            );
          }
          return content;
        },
        description: 'Added cursor-not-allowed for disabled buttons'
      },
      {
        path: path.join(FRONTEND_SRC, 'components/common/Card.tsx'),
        check: async (content) => {
          // Ensure cards have proper border
          return !content.includes('border-gray-200');
        },
        fix: async (content) => {
          return content.replace(
            'className="rounded-lg',
            'className="rounded-lg border border-gray-200'
          );
        },
        description: 'Added border to cards for better visibility'
      }
    ];

    for (const fix of componentFixes) {
      try {
        const content = await fs.readFile(fix.path, 'utf-8');

        if (await fix.check(content)) {
          const fixedContent = await fix.fix(content);

          if (fixedContent !== content) {
            await fs.writeFile(fix.path, fixedContent);
            this.filesFixed.add(fix.path);
            this.fixes.push({
              file: fix.path,
              description: fix.description,
              timestamp: new Date().toISOString()
            });
            console.log(`  ✅ Fixed: ${path.basename(fix.path)} - ${fix.description}`);
          }
        }
      } catch (error) {
        // File might not exist
        if (error.code !== 'ENOENT') {
          console.error(`  ⚠️ Error checking ${fix.path}:`, error.message);
        }
      }
    }
  }

  async fixTailwindClasses() {
    console.log('\n🔧 Ensuring proper Tailwind classes...');

    const pagesDir = path.join(FRONTEND_SRC, 'pages');
    const pages = await fs.readdir(pagesDir);

    for (const page of pages) {
      if (page.endsWith('.tsx') || page.endsWith('.jsx')) {
        const filePath = path.join(pagesDir, page);
        let content = await fs.readFile(filePath, 'utf-8');
        let modified = false;

        // Fix patterns
        const fixes = [
          {
            // Ensure text on colored backgrounds is visible
            pattern: /className="([^"]*bg-(?:green|blue|indigo|purple|gradient)[^"]*)"(?![^>]*text-white)/g,
            replace: 'className="$1 text-white"',
            condition: (match) => !match.includes('text-white')
          },
          {
            // Fix button hover states
            pattern: /className="([^"]*btn[^"]*)"(?![^>]*hover:)/g,
            replace: 'className="$1 hover:opacity-90"',
            condition: (match) => !match.includes('hover:')
          }
        ];

        for (const fix of fixes) {
          const matches = content.match(fix.pattern);
          if (matches) {
            matches.forEach(match => {
              if (!fix.condition || fix.condition(match)) {
                const replacement = match.replace(fix.pattern, fix.replace);
                content = content.replace(match, replacement);
                modified = true;
              }
            });
          }
        }

        if (modified) {
          await fs.writeFile(filePath, content);
          this.filesFixed.add(filePath);
          this.fixes.push({
            file: filePath,
            description: 'Fixed Tailwind classes for visibility',
            timestamp: new Date().toISOString()
          });
          console.log(`  ✅ Fixed Tailwind classes in: ${page}`);
        }
      }
    }
  }

  async generateReport() {
    console.log('\n📝 Generating fix report...');

    const report = {
      timestamp: new Date().toISOString(),
      summary: {
        totalFixes: this.fixes.length,
        filesModified: this.filesFixed.size
      },
      fixes: this.fixes,
      modifiedFiles: Array.from(this.filesFixed)
    };

    // Save JSON report
    const reportPath = path.join(REPORTS_DIR, 'auto-fixes-report.json');
    await fs.writeFile(reportPath, JSON.stringify(report, null, 2));

    // Generate markdown summary
    let md = '# Auto-Fix Report\n\n';
    md += `**Timestamp:** ${report.timestamp}\n\n`;
    md += `## Summary\n\n`;
    md += `- **Total Fixes Applied:** ${report.summary.totalFixes}\n`;
    md += `- **Files Modified:** ${report.summary.filesModified}\n\n`;

    if (report.fixes.length > 0) {
      md += '## Fixes Applied\n\n';
      report.fixes.forEach((fix, index) => {
        md += `${index + 1}. **${path.basename(fix.file)}**\n`;
        md += `   - ${fix.description}\n`;
        md += `   - Time: ${fix.timestamp}\n\n`;
      });
    }

    md += '## Modified Files\n\n';
    report.modifiedFiles.forEach(file => {
      md += `- ${file.replace(FRONTEND_SRC, 'src')}\n`;
    });

    const mdPath = path.join(REPORTS_DIR, 'auto-fixes-summary.md');
    await fs.writeFile(mdPath, md);

    console.log(`\n✅ Auto-fix complete!`);
    console.log(`  - Fixes applied: ${report.summary.totalFixes}`);
    console.log(`  - Files modified: ${report.summary.filesModified}`);
    console.log(`  - Report saved: ${reportPath}`);
    console.log(`  - Summary saved: ${mdPath}`);
  }

  async run() {
    await this.initialize();

    console.log('🔍 Analyzing codebase for common issues...\n');

    // Apply fixes
    await this.fixTextVisibilityIssues();
    await this.fixSpecificComponents();
    await this.fixTailwindClasses();

    // Generate report
    await this.generateReport();

    return this.fixes.length;
  }
}

// Run the agent
if (require.main === module) {
  const agent = new AutoFixAgent();
  agent.run()
    .then(fixCount => {
      console.log(`\n🎉 Auto-fix agent completed with ${fixCount} fixes!`);
      process.exit(0);
    })
    .catch(error => {
      console.error('❌ Auto-fix agent failed:', error);
      process.exit(1);
    });
}

module.exports = AutoFixAgent;