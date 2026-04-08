# Deduplication Matching Workflow

## Objective
Compare an uploaded partner dataset:
- against itself (internal duplicates)
- against the ActivityInfo master snapshot (external duplicates)

The workflow produces exact and fuzzy matches with confidence levels and explanations, then exports one Excel file with six structured sheets.

## Pipeline
1. Ingestion and validation
- Upload `.xlsx` file.
- Auto-suggest column mapping using string similarity.
- User confirms/adjusts mappings for required fields.

2. Normalization
- Arabic text normalization: trims whitespace, removes diacritics, normalizes Alef/Ya/Ta marbuta variants.
- Digits normalization (Arabic/Extended Arabic to ASCII).
- Phone normalization to digits only.
- Age normalization to integer.
- Category normalization for sex and marital status.
- Geography normalization (trim/lower/spacing cleanup).

3. Candidate blocking (performance)
Blocking keys reduce pairwise comparisons:
- governorate + first letter of name
- governorate + first 3 chars of name
- district + first 3 chars of name
- governorate + phone prefix
- phone suffix (last 4 digits)
- governorate + spouse-name prefix

4. Exact matching rules
A pair is exact duplicate if any rule matches:
- exact `hoh_ID_number`
- exact primary phone
- exact secondary phone
- exact name + governorate
- exact name + ID
- exact phone + age within tolerance

5. Fuzzy scoring
When exact rules fail, composite fuzzy score (0-100) is computed with weighted factors:
- HoH name similarity (Jaro-Winkler + token-sort + Levenshtein)
- Spouse name similarity (same methods)
- Phone similarity (exact or partial)
- Geography hierarchy similarity
- Age proximity (tolerance-based)
- Sex exact match

Default weights (`config$weights`):
- `hoh_arabic_name`: 0.35
- `hoh_spouse_name`: 0.15
- `phone_number`: 0.20
- `geography`: 0.15
- `age`: 0.10
- `sex`: 0.05

6. Confidence thresholds
Configurable in Step 3 UI:
- High confidence: score >= high threshold (default 90)
- Medium confidence: score >= medium threshold (default 75)
- Low confidence: below medium threshold (excluded from fuzzy output)

7. Privacy controls
Master sensitive identifiers are masked in output:
- `master_hoh_ID_number`
- `master_primary_phone_number`
- `master_secondary_phone_number`

## Excel output
Generated workbook includes:
- `Info`
- `Summary`
- `Same List Exact Matching`
- `Same List Fuzzy Matching`
- `List Vs ActivityInfo Exact`
- `List Vs ActivityInfo Fuzzy`

Formatting:
- bold headers
- frozen header row
- column filters
- conditional highlighting for high/medium confidence

## Limitations
- Blocking can miss true matches if key fields are empty or heavily misspelled.
- Arabic/geo normalization is rule-based and may need location-specific synonym dictionaries.
- Very large uploads may require lowering candidate space and running in batches.
