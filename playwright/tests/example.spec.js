const { test, expect } = require('@playwright/test');

test('home page loads', async ({ page }) => {
  await page.goto('/');
  // Adjust title check if the Shiny app sets a specific title
  await expect(page).toHaveTitle(/Shiny|Deduplication|ActivityInfo/i);
});
