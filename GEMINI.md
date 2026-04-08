# Gemini Context: Deduplication App (R Shiny)

A specialized R Shiny application designed for deduplicating beneficiary lists against a master database (ActivityInfo) and within themselves.

## Project Overview

- **Purpose:** To provide a user-friendly interface for matching and deduplicating partner-submitted Excel lists against a central ActivityInfo master database.
- **Tech Stack:** R, Shiny, `bslib` (UI), `dplyr`/`data.table` (Data processing), `stringdist`/`fuzzyjoin` (Matching logic), `future`/`promises` (Async jobs).
- **Core Workflow:**
  1. **Upload:** User uploads an Excel file.
  2. **Mapping:** Suggest and confirm column mappings (e.g., Name, Phone, Age).
  3. **Preprocessing:** Normalize text, Arabic names, phone numbers, and categories.
  4. **Matching:** Execute exact and fuzzy matching rules (internal and external).
  5. **Review & Export:** Generate a formatted Excel report for partner verification.

## Building and Running

### Prerequisites
- R (>= 4.0.0)
- RStudio (optional but recommended)

### Setup
Install all required R packages:
```bash
Rscript shiny/install.R
```

### Running Locally
Run the Shiny application from the repository root:
```bash
Rscript -e "shiny::runApp('shiny', launch.browser = TRUE)"
```

### Testing
Run the test suite using `testthat`:
```bash
Rscript -e "testthat::test_dir('shiny/tests/testthat')"
```

## Directory Structure

- `shiny/`: Main application directory.
  - `app.R`: Application entry point (UI and server).
  - `R/`: Modularized logic (auth, preprocessing, matching, etc.).
  - `tests/`: Unit and integration tests.
  - `tmp/`: Local data snapshots and job state files.
- `docs/`: Technical documentation (Architecture, API, Security).
- `AGENTS.md`: Development guidelines for AI assistants.

## Development Conventions

- **R Style:** Use `snake_case` for functions and variables. Indent with 2 spaces.
- **Modularization:** Place business logic in `shiny/R/*.R` files and source them in `app.R`.
- **Async Processing:** Long-running tasks (fetching data, matching) should use the `future` and `promises` framework to keep the UI responsive.
- **Security:** Never commit API tokens or secrets. Use `.env` files or system environment variables.
- **Commits:** Use Conventional Commits (`feat:`, `fix:`, `docs:`, `chore:`).

## Key Files & Modules

- `shiny/R/matching.R`: Core deduplication logic, including blocking and scoring algorithms.
- `shiny/R/activityinfo.R`: Handles data retrieval from the ActivityInfo API.
- `shiny/R/preprocess.R`: Implements normalization rules for various data types (Arabic text, phones, etc.).
- `shiny/R/jobs.R`: Manages background job execution and status tracking.
- `docs/ARCHITECTURE.md`: Detailed breakdown of the system components and data flow.
