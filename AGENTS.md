# Repository Guidelines

## Project Structure & Module Organization
- `shiny/` R Shiny application (UI + backend logic).
- `shiny/R/` Application modules (auth, ActivityInfo, preprocessing, matching, jobs).
- `docs/` Technical documentation and decisions.

## Build, Test, and Development Commands
Shiny app (from repo root):
- `Rscript shiny/install.R` to install required R packages.
- `Rscript -e "shiny::runApp('shiny', launch.browser = TRUE)"` to run locally.

## Coding Style & Naming Conventions
- R: 2 spaces, `snake_case` for functions and variables.
- Modules: place helpers in `shiny/R/*.R` and source them from `shiny/app.R`.
- Keep functions small and pure where possible; pass dependencies explicitly.

## Testing Guidelines
- If/when tests are added, place them under `shiny/tests/` using `testthat`.
- Favor fast unit tests for preprocessing and scoring functions.

## Commit & Pull Request Guidelines
- Use Conventional Commits: `feat:`, `fix:`, `docs:`, `chore:`.
- PRs should include a short description, linked issue if applicable, and screenshots for UI changes.

## Security & Configuration
- No secrets in git. Use environment variables for tokens and credentials.
- Master database access is read-only.
- Mask sensitive fields in any exported results.
