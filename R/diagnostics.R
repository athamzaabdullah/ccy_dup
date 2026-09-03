# R/diagnostics.R
# Pre-Upload Data Hygiene & Quality Diagnostic Engine for CCY Deduplication Platform
# Identifies scientific notation corruption, blank rows, and placeholder anomalies before matching.

check_upload_hygiene <- function(df) {
  warnings <- character(0)
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(list(clean_df = df, warnings = "Uploaded dataset is empty.", issue_count = 1L))
  }

  clean_df <- df

  # 1. Detect and auto-prune completely empty rows
  is_row_empty <- apply(clean_df, 1, function(row) {
    all(is.na(row) | !nzchar(trimws(as.character(row))))
  })
  empty_count <- sum(is_row_empty)
  if (empty_count > 0) {
    clean_df <- clean_df[!is_row_empty, , drop = FALSE]
    warnings <- c(warnings, sprintf("Detected and automatically removed %d completely empty row(s).", empty_count))
  }

  # 2. Detect duplicate column headers
  header_names <- trimws(names(clean_df))
  dup_headers <- header_names[duplicated(tolower(header_names))]
  if (length(dup_headers) > 0) {
    warnings <- c(warnings, sprintf("Detected duplicate column headers: '%s'. Duplicate names may cause column collision during mapping.", paste(unique(dup_headers), collapse = "', '")))
  }

  # 3. Detect scientific notation corruption in phone and ID columns
  sci_pattern <- "^[0-9]+(\\.[0-9]+)?[eE]\\+[0-9]+$"
  for (col in names(clean_df)) {
    vals <- clean_df[[col]]
    char_vals <- as.character(vals[!is.na(vals)])
    char_vals <- trimws(char_vals[nzchar(char_vals)])
    if (length(char_vals) == 0) next

    sci_matches <- sum(grepl(sci_pattern, char_vals))
    if (sci_matches > 0) {
      warnings <- c(
        warnings,
        sprintf("Column '%s' contains %d value(s) in scientific notation (e.g., '%s'). Excel often converts 9-digit phones or 11-digit IDs to scientific format. Please format these as 'Text' in Excel to avoid losing digits.",
                col, sci_matches, head(char_vals[grepl(sci_pattern, char_vals)], 1))
      )
    }
  }

  # 4. Detect columns with abnormally high placeholder / dummy values
  dummy_pattern <- "^(0+|1+|2+|3+|4+|5+|6+|7+|8+|9+|12345678|123456789|987654321|00000000|11111111|77777777|99999999)$"
  id_phone_cols <- grep("phone|id|tel|mobile|national", names(clean_df), ignore.case = TRUE, value = TRUE)
  for (col in id_phone_cols) {
    vals <- clean_df[[col]]
    char_vals <- as.character(vals[!is.na(vals)])
    char_vals <- trimws(char_vals[nzchar(char_vals)])
    if (length(char_vals) >= 10) {
      dummy_matches <- sum(grepl(dummy_pattern, char_vals))
      rate <- dummy_matches / length(char_vals)
      if (rate >= 0.30) {
        warnings <- c(
          warnings,
          sprintf("Column '%s' has a high proportion (%.0f%%) of generic placeholder values (e.g., '0000', '111111'). The deduplication engine will exclude these placeholders from exact matching.",
                  col, rate * 100)
        )
      }
    }
  }

  list(
    clean_df = clean_df,
    warnings = warnings,
    issue_count = length(warnings)
  )
}
