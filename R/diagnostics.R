# R/diagnostics.R
# Pre-Upload Data Hygiene & Quality Diagnostic Engine for CCY Deduplication Platform
# Identifies scientific notation corruption, blank rows, and placeholder anomalies before matching.

check_upload_hygiene <- function(df) {
  warnings <- character(0)
  checks <- list()

  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(list(
      clean_df = df,
      warnings = "Uploaded dataset is empty.",
      issue_count = 1L,
      checks = list(
        list(id = "empty_rows", name = "Blank & Empty Rows", status = "bad", label = "Dataset is empty", detail = "File contains 0 records."),
        list(id = "sci_notation", name = "Scientific Notation Scan", status = "skip", label = "Not checked", detail = "No records to evaluate."),
        list(id = "dup_headers", name = "Column Header Integrity", status = "skip", label = "Not checked", detail = "No columns to evaluate."),
        list(id = "placeholders", name = "Dummy/Placeholder Filter", status = "skip", label = "Not checked", detail = "No records to evaluate.")
      )
    ))
  }

  clean_df <- df

  # 1. Detect and auto-prune completely empty rows
  is_row_empty <- apply(clean_df, 1, function(row) {
    all(is.na(row) | !nzchar(trimws(as.character(row))))
  })
  empty_count <- sum(is_row_empty)
  if (empty_count > 0) {
    clean_df <- clean_df[!is_row_empty, , drop = FALSE]
    msg <- sprintf("Detected and automatically pruned %d empty row(s).", empty_count)
    warnings <- c(warnings, msg)
    checks$empty_rows <- list(
      id = "empty_rows",
      name = "Empty Row Audit",
      status = "info",
      badge = paste0(empty_count, " Cleaned"),
      label = paste0(empty_count, " empty row(s) removed"),
      detail = "Completely blank rows were removed to prevent indexing misalignment."
    )
  } else {
    checks$empty_rows <- list(
      id = "empty_rows",
      name = "Empty Row Audit",
      status = "pass",
      badge = "0 Empty Rows",
      label = "No blank rows detected",
      detail = "Every record contains active beneficiary data."
    )
  }

  # 2. Detect duplicate column headers
  header_names <- trimws(names(clean_df))
  dup_headers <- header_names[duplicated(tolower(header_names))]
  if (length(dup_headers) > 0) {
    msg <- sprintf("Detected duplicate column headers: '%s'. Duplicate names may cause column collision during mapping.", paste(unique(dup_headers), collapse = "', '"))
    warnings <- c(warnings, msg)
    checks$dup_headers <- list(
      id = "dup_headers",
      name = "Column Header Integrity",
      status = "warn",
      badge = paste0(length(unique(dup_headers)), " Duplicates"),
      label = "Duplicate headers detected",
      detail = paste0("Collision risk on: ", paste(unique(dup_headers), collapse = ", "))
    )
  } else {
    checks$dup_headers <- list(
      id = "dup_headers",
      name = "Column Header Integrity",
      status = "pass",
      badge = "All Unique",
      label = "All column headers are unique",
      detail = "Zero column collision risks identified across upload fields."
    )
  }

  # 3. Detect scientific notation corruption in phone and ID columns
  sci_pattern <- "^[0-9]+(\\.[0-9]+)?[eE]\\+[0-9]+$"
  sci_matches_total <- 0L
  sci_cols <- character(0)
  for (col in names(clean_df)) {
    vals <- clean_df[[col]]
    char_vals <- as.character(vals[!is.na(vals)])
    char_vals <- trimws(char_vals[nzchar(char_vals)])
    if (length(char_vals) == 0) next

    sci_m <- sum(grepl(sci_pattern, char_vals))
    if (sci_m > 0) {
      sci_matches_total <- sci_matches_total + sci_m
      sci_cols <- c(sci_cols, col)
      warnings <- c(
        warnings,
        sprintf("Column '%s' contains %d value(s) in scientific notation (e.g., '%s'). Excel often converts 9-digit phones or 11-digit IDs to scientific format. Please format these as 'Text' in Excel to avoid losing digits.",
                col, sci_m, head(char_vals[grepl(sci_pattern, char_vals)], 1))
      )
    }
  }
  if (sci_matches_total > 0) {
    checks$sci_notation <- list(
      id = "sci_notation",
      name = "Scientific Notation Scan",
      status = "warn",
      badge = paste0(sci_matches_total, " Values Corrupted"),
      label = "Scientific format detected (e.g., 7.71E+08)",
      detail = paste0("Detected in columns: ", paste(sci_cols, collapse = ", "), ". Format as Text in Excel.")
    )
  } else {
    checks$sci_notation <- list(
      id = "sci_notation",
      name = "Scientific Notation Scan",
      status = "pass",
      badge = "Clean",
      label = "No scientific format corruption",
      detail = "9-digit phone and 11-digit national ID digits are fully preserved."
    )
  }

  # 4. Detect columns with abnormally high placeholder / dummy values
  dummy_pattern <- "^(0+|1+|2+|3+|4+|5+|6+|7+|8+|9+|12345678|123456789|987654321|00000000|11111111|77777777|99999999)$"
  id_phone_cols <- grep("phone|id|tel|mobile|national", names(clean_df), ignore.case = TRUE, value = TRUE)
  dummy_cols <- character(0)
  for (col in id_phone_cols) {
    vals <- clean_df[[col]]
    char_vals <- as.character(vals[!is.na(vals)])
    char_vals <- trimws(char_vals[nzchar(char_vals)])
    if (length(char_vals) >= 10) {
      dummy_matches <- sum(grepl(dummy_pattern, char_vals))
      rate <- dummy_matches / length(char_vals)
      if (rate >= 0.30) {
        dummy_cols <- c(dummy_cols, col)
        warnings <- c(
          warnings,
          sprintf("Column '%s' has a high proportion (%.0f%%) of generic placeholder values (e.g., '0000', '111111'). The deduplication engine will exclude these placeholders from exact matching.",
                  col, rate * 100)
        )
      }
    }
  }
  if (length(dummy_cols) > 0) {
    checks$placeholders <- list(
      id = "placeholders",
      name = "Placeholder & Dummy Scan",
      status = "warn",
      badge = "Placeholders Flagged",
      label = "High placeholder rate detected",
      detail = paste0("Placeholders found in: ", paste(dummy_cols, collapse = ", "), ". Excluded from false-exact matches.")
    )
  } else {
    checks$placeholders <- list(
      id = "placeholders",
      name = "Placeholder & Dummy Scan",
      status = "pass",
      badge = "Clean",
      label = "No dummy placeholder spikes",
      detail = "ID and phone numbers contain diverse genuine beneficiary sequences."
    )
  }

  list(
    clean_df = clean_df,
    warnings = warnings,
    issue_count = length(warnings),
    checks = checks
  )
}
