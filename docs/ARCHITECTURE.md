# Architecture Overview

## Stack
- Frontend + orchestration: R Shiny (`shiny/app.R`)
- Core modules: `shiny/R/*.R`
- Master data source: ActivityInfo API (snapshotted locally as `.rds`)
- Export: `openxlsx`

## Main Components
- `shiny/R/activityinfo.R`
  - Fetches ActivityInfo forms in chunks.
  - Supports progress reporting and cancellation.

- `shiny/R/preprocess.R`
  - Canonical column mapping.
  - Text/Arabic/phone/age/category normalization.

- `shiny/R/mapping.R`
  - Upload-to-required-column suggestion with string similarity.

- `shiny/R/matching.R`
  - Blocking candidate generation.
  - Exact duplicate rules.
  - Multi-factor fuzzy scoring and confidence classification.
  - Internal (same-list) and external (list-vs-master) matching.

- `shiny/R/jobs.R`
  - Background job queue/state files.
  - Async matching and master fetch workers.
  - Cancellation and status tracking.

- `shiny/R/export.R`
  - Builds the final single Excel file with required sheet structure and formatting.

## Data Flow
1. User uploads Excel file.
2. System suggests and confirms column mappings.
3. User sets thresholds/tolerance and starts job.
4. Worker loads upload + latest master snapshot.
5. Workflow runs exact/fuzzy matching for:
   - same list vs same list
   - upload vs ActivityInfo master
6. Result object is persisted as job output.
7. Export action writes one formatted workbook for partner review.

## Security & Privacy
- Master access is read-only.
- Snapshot kept locally under `shiny/tmp/`.
- Sensitive master identifiers are masked in exported matching sheets.
- No credential values should be committed.
