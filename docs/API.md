# Interface Contract (Shiny App)

This project uses an R Shiny application, not REST endpoints.

## User workflow
1. Upload partner `.xlsx` file.
2. Review and adjust column mapping suggestions.
3. Configure thresholds/tolerance.
4. Run matching job.
5. Export one Excel workbook.

## Inputs
- Upload file columns are mapped to required canonical fields.
- Settings include:
  - high threshold
  - medium threshold
  - age tolerance
  - max candidate pairs

## Outputs
- Background job result object (`.rds`) with:
  - `info`
  - `summary`
  - `same_list_exact`
  - `same_list_fuzzy`
  - `list_vs_master_exact`
  - `list_vs_master_fuzzy`
- Downloaded Excel workbook with the same structured content.
