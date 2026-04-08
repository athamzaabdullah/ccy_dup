# Copilot instructions for deduplication_app_R

Summary
- Purpose: Provide concise, actionable guidance for future Copilot CLI sessions working on this R Shiny deduplication app.
- Location: R Shiny app in `shiny/`, core logic in `shiny/R/`.

Build, test, and lint commands
- Install required R packages (installs all listed deps):
  Rscript shiny/install.R
- Run the app locally from repository root:
  Rscript -e "shiny::runApp('shiny', launch.browser = TRUE)"
- Run the full test suite (testthat):
  Rscript -e "testthat::test_dir('shiny/tests/testthat')"
- Run a single test file (example):
  Rscript -e "testthat::test_file('shiny/tests/testthat/test-preprocess.R')"
  (Change filename to the test to run.)

High-level architecture (big picture)
- The repository hosts a single R Shiny application under `shiny/` used to deduplicate partner-submitted beneficiary lists against an ActivityInfo master database and internally.
- Entry point: `shiny/app.R` (UI + server wiring).
- Modular logic lives in `shiny/R/`:
  - activityinfo.R — fetching & syncing ActivityInfo master data
  - preprocess.R — normalization routines (names, Arabic handling, phones, categories)
  - matching.R — blocking, scoring, exact and fuzzy matching logic
  - jobs.R — background job management (future/promises pattern)
  - mapping.R / export.R / auth.R / config.R — mapping, export/reporting, authentication, configuration helpers
- docs/ contains canonical docs: ARCHITECTURE.md, MATCHING.md, API.md, DEPLOYMENT.md, PARTNER_GUIDE.md, SECURITY.md. Use these when making architectural changes.
- Long-running tasks use future/promises to avoid blocking the Shiny server — do not convert them to synchronous calls.

Key conventions specific to this codebase
- R style: 2-space indentation and snake_case for functions/variables (see docs/ and AGENTS.md). Follow existing style in `shiny/R/` files.
- Modularization: place pure business logic in `shiny/R/*.R` and keep `app.R` responsible for wiring and UI. New helpers should be added to `shiny/R/` and sourced from `app.R`.
- Async jobs: use the `future` + `promises` pattern for background work (jobs.R) — returning reactive-friendly objects and storing job state in `shiny/tmp/` or `shiny/tmp` job store.
- Matching behavior: matching is separated into blocking + scoring steps; modify `matching.R` only after reading MATCHING.md in docs/.
- Tests: tests live under `shiny/tests/` and use testthat. Prefer small, fast unit tests for preprocess/matching functions.
- Security: do not hardcode secrets. Use environment variables and `config.R` patterns. Check SECURITY.md for handling credentials and masking exported results.

Files and AI assistant configs to consult
- AGENTS.md — repository-specific AI assistant guidance (already present).
- GEMINI.md — high-level project context and run/test commands (already present).
- docs/ARCHITECTURE.md and docs/MATCHING.md — authoritative design & matching rules; consult before changing matching logic.

Suggestions for future Copilot sessions
- Before editing matching or preprocessing code, read docs/MATCHING.md and `shiny/R/preprocess.R` and `shiny/R/matching.R` to understand normalization and scoring expectations.
- When adding features that touch ActivityInfo, update docs/API.md and tests under shiny/tests/.
- For UI changes, ensure screenshots or notes are added to PRs as the repo prefers UI screenshots for review.

MCP servers
- Playwright end-to-end tests configured. Files added:
  - package.json (devDependencies: playwright, start-server-and-test)
  - playwright.config.js (baseURL: http://localhost:3456, testDir: playwright/tests)
  - playwright/tests/example.spec.js (example smoke test)

Setup & run
1. Install Node deps at repo root:
   npm install
2. Install Playwright browsers (required once):
   npm run playwright:install-browsers
3. Run tests (app must be running):
   npm run test:playwright
4. Run CI-style test that starts the Shiny app automatically:
   npm run test:playwright:ci

Notes
- The start command used by the CI script is:
  Rscript -e "shiny::runApp('shiny', port=3456, launch.browser = FALSE)"
  Ensure R is installed on CI runners and the required R packages are available.
- Tests expect the app at http://localhost:3456; change playwright.config.js if you prefer a different port.

If you want changes
- Reply with areas to expand (e.g., include specific test-file examples, more guidance on the matching algorithm, or add Playwright MCP server setup), and the file will be updated.
