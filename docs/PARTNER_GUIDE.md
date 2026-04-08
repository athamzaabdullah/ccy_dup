# Partner Interpretation Guide

## Setup
1. Install packages:
`Rscript shiny/install.R`

2. Run app from repository root:
`Rscript -e "shiny::runApp('shiny', launch.browser = TRUE)"`

3. In **Settings**, set your ActivityInfo token.

## How to read output workbook

## Info
- Shows filename, run time, row counts, thresholds, age tolerance, and matching weights.

## Summary
- Shows counts and percentages for all four matching directions/types.
- Includes average fuzzy scores and warning notes.

## Same List Exact Matching
- High-confidence duplicates inside uploaded file.
- `matched_fields` shows which exact rule was triggered.

## Same List Fuzzy Matching
- Possible duplicates inside uploaded file.
- `match_score` is 0-100.
- `contributing_factors` explains score composition.

## List Vs ActivityInfo Exact
- High-confidence upload-to-master duplicates.
- Master sensitive identifiers are masked.

## List Vs ActivityInfo Fuzzy
- Probable upload-to-master duplicates requiring analyst review.
- Prioritize rows with `confidence = high` and highest `match_score`.

## Confidence bands
- `high`: score >= high threshold
- `medium`: score >= medium threshold and below high
- `low`: excluded from fuzzy sheets

## Assumptions and limitations
- Blocking improves speed but may miss edge cases where key fields are missing.
- Data quality heavily affects fuzzy performance (spelling variants, missing phones/IDs).
- Geography standardization is lexical; local synonym dictionaries can improve recall.
