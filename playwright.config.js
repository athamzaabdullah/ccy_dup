module.exports = {
  testDir: 'playwright/tests',
  timeout: 30 * 1000,
  use: {
    headless: true,
    baseURL: 'http://localhost:3456',
    actionTimeout: 5 * 1000,
    trace: 'on-first-retry'
  },
  projects: [
    { name: 'chromium', use: { browserName: 'chromium' } },
    { name: 'firefox', use: { browserName: 'firefox' } },
    { name: 'webkit', use: { browserName: 'webkit' } }
  ]
};
