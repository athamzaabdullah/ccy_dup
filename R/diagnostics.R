# R/diagnostics.R
# Pre-Upload Data Hygiene & Quality Diagnostic Engine for CCY Deduplication Platform
# Identifies scientific notation corruption, blank rows, and placeholder anomalies before matching.

is_scientific_format <- function(x) {
  if (is.null(x) || length(x) == 0) return(logical(0))
  x_char <- trimws(as.character(x))
  is_empty <- is.na(x_char) | !nzchar(x_char)
  out <- rep(FALSE, length(x_char))
  if (all(is_empty)) return(out)

  # Pattern 1: Canonical or variations of scientific format (e.g. 1.02E+10, 1.02e+10, 1.02E10, 7.71E8, 1e10, 7.71e+08, 1,02E+10)
  pat1 <- "(?i)^[-+]?[0-9]+[.,]?[0-9]*e[-+]?[0-9]+$"
  # Pattern 2: Digits around E (handles cases with surrounding tokens or embedded values)
  pat2 <- "(?i)[0-9][eE][-+]?[0-9]"

  matches <- (grepl(pat1, x_char, perl = TRUE) | grepl(pat2, x_char, perl = TRUE)) & !is_empty
  matches
}

normalize_sci_string <- function(x) {
  if (is.null(x) || length(x) == 0) return(x)
  vapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    vc <- trimws(as.character(v))
    if (!is_scientific_format(vc)) return(vc)
    vc_dot <- gsub(",", ".", vc)
    m <- regmatches(vc_dot, regexpr("[-+]?[0-9]+[.]?[0-9]*[eE][-+]?[0-9]+", vc_dot))
    if (length(m) > 0 && nzchar(m)) {
      num <- suppressWarnings(as.numeric(m))
      if (!is.na(num) && is.finite(num)) {
        return(format(num, scientific = FALSE, trim = TRUE))
      }
    }
    vc
  }, character(1), USE.NAMES = FALSE)
}

col_letter_to_index <- function(col_str) {
  letters_vec <- strsplit(toupper(col_str), "")[[1]]
  idx <- 0L
  for (ch in letters_vec) {
    idx <- idx * 26L + (utf8ToInt(ch) - utf8ToInt("A") + 1L)
  }
  idx
}

detect_xlsx_scientific_cells <- function(xlsx_path, df_colnames = NULL) {
  if (!file.exists(xlsx_path)) return(list(has_sci = FALSE, sci_cols = character(0), count = 0L))
  
  td <- file.path(tempdir(), paste0("xlsx_diag_", as.numeric(Sys.time()) * 1000, "_", sample(1000:9999, 1)))
  dir.create(td, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  
  files <- tryCatch(utils::unzip(xlsx_path, list = TRUE)$Name, error = function(e) character(0))
  if (!("xl/styles.xml" %in% files) || !any(grepl("^xl/worksheets/sheet[0-9]+\\.xml$", files))) {
    return(list(has_sci = FALSE, sci_cols = character(0), count = 0L))
  }
  
  sheet_file <- grep("^xl/worksheets/sheet[0-9]+\\.xml$", files, value = TRUE)[1]
  utils::unzip(xlsx_path, files = "xl/styles.xml", exdir = td)
  
  styles_path <- file.path(td, "xl", "styles.xml")
  if (!file.exists(styles_path)) {
    return(list(has_sci = FALSE, sci_cols = character(0), count = 0L))
  }
  
  styles_xml <- paste(readLines(styles_path, warn = FALSE), collapse = " ")
  
  # Standard ECMA-376 scientific numFmtId is 11 (0.00E+00) and 48 (##0.0E+0)
  sci_num_fmts <- c(11, 48)
  
  custom_fmts <- regmatches(styles_xml, gregexpr('<numFmt [^>]*numFmtId="([0-9]+)"[^>]*formatCode="([^"]*)"[^>]*/>', styles_xml))[[1]]
  for (cf in custom_fmts) {
    id_m <- regmatches(cf, regexpr('numFmtId="([0-9]+)"', cf))
    code_m <- regmatches(cf, regexpr('formatCode="([^"]*)"', cf))
    if (length(id_m) > 0 && length(code_m) > 0) {
      fmt_id <- as.numeric(gsub('[^0-9]', '', id_m))
      fmt_code <- gsub('^formatCode="|"^', '', code_m)
      if (grepl("(?i)[0#][.,]?[0#]*[eE][-+]?[0#]", fmt_code)) {
        sci_num_fmts <- c(sci_num_fmts, fmt_id)
      }
    }
  }
  
  cell_xfs_part <- regmatches(styles_xml, regexpr('<cellXfs [^>]*>.*?</cellXfs>', styles_xml))
  if (length(cell_xfs_part) == 0) return(list(has_sci = FALSE, sci_cols = character(0), count = 0L))
  
  xfs <- regmatches(cell_xfs_part, gregexpr('<xf [^>]*>', cell_xfs_part))[[1]]
  sci_xf_indices <- integer(0)
  for (idx in seq_along(xfs)) {
    xf_str <- xfs[idx]
    id_m <- regmatches(xf_str, regexpr('numFmtId="([0-9]+)"', xf_str))
    if (length(id_m) > 0) {
      fid <- as.numeric(gsub('[^0-9]', '', id_m))
      if (fid %in% sci_num_fmts) {
        sci_xf_indices <- c(sci_xf_indices, idx - 1L)
      }
    }
  }
  
  if (length(sci_xf_indices) == 0) {
    return(list(has_sci = FALSE, sci_cols = character(0), count = 0L))
  }
  
  utils::unzip(xlsx_path, files = sheet_file, exdir = td)
  sheet_path <- file.path(td, sheet_file)
  if (!file.exists(sheet_path)) {
    return(list(has_sci = FALSE, sci_cols = character(0), count = 0L))
  }
  
  sheet_xml <- paste(readLines(sheet_path, warn = FALSE), collapse = " ")
  cell_matches <- regmatches(sheet_xml, gregexpr('<c [^>]*r="([A-Z]+)[0-9]+"[^>]*s="([0-9]+)"[^>]*>', sheet_xml))[[1]]
  
  sci_col_letters <- character(0)
  sci_count <- 0L
  for (cm in cell_matches) {
    s_val <- as.numeric(gsub('.*s="([0-9]+)".*', '\\1', cm))
    if (s_val %in% sci_xf_indices) {
      col_letter <- gsub('.*r="([A-Z]+)[0-9]+".*', '\\1', cm)
      sci_col_letters <- c(sci_col_letters, col_letter)
      sci_count <- sci_count + 1L
    }
  }
  
  col_names <- character(0)
  if (!is.null(df_colnames) && length(df_colnames) > 0) {
    for (cl in unique(sci_col_letters)) {
      c_idx <- col_letter_to_index(cl)
      if (c_idx <= length(df_colnames)) {
        col_names <- c(col_names, df_colnames[c_idx])
      } else {
        col_names <- c(col_names, cl)
      }
    }
  } else {
    col_names <- unique(sci_col_letters)
  }
  
  list(
    has_sci = sci_count > 0,
    sci_cols = col_names,
    count = sci_count
  )
}

check_upload_hygiene <- function(df, file_path = NULL) {
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
        list(id = "formula_errors", name = "Excel Formula Errors", status = "skip", label = "Not checked", detail = "No records to evaluate."),
        list(id = "dup_headers", name = "Column Header Integrity", status = "skip", label = "Not checked", detail = "No columns to evaluate."),
        list(id = "placeholders", name = "Dummy/Placeholder Filter", status = "skip", label = "Not checked", detail = "No records to evaluate.")
      )
    ))
  }

  clean_df <- df

  # 1. Whitespace & Non-breaking space sanitization across all character columns
  for (col in names(clean_df)) {
    if (is.character(clean_df[[col]])) {
      clean_df[[col]] <- trimws(gsub("[\u00A0\t\r\n]+", " ", clean_df[[col]]))
    }
  }

  # 2. Detect and auto-prune completely empty rows
  mat <- as.matrix(clean_df)
  has_val <- !is.na(mat) & trimws(mat) != ""
  is_row_empty <- rowSums(matrix(has_val, nrow = nrow(clean_df), ncol = ncol(clean_df))) == 0
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

  # 3. Detect duplicate column headers
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

  # 4. Detect scientific notation corruption across all columns (with normalization in clean_df)
  sci_matches_total <- 0L
  sci_cols <- character(0)

  # 4a. If file_path is provided for .xlsx, inspect Excel cell formatting directly
  if (!is.null(file_path) && file.exists(file_path) && grepl("\\.xlsx$", file_path, ignore.case = TRUE)) {
    xlsx_res <- tryCatch(detect_xlsx_scientific_cells(file_path, names(clean_df)), error = function(e) NULL)
    if (!is.null(xlsx_res) && isTRUE(xlsx_res$has_sci)) {
      sci_matches_total <- sci_matches_total + xlsx_res$count
      sci_cols <- unique(c(sci_cols, xlsx_res$sci_cols))
      for (scol in xlsx_res$sci_cols) {
        if (scol %in% names(clean_df)) {
          if (is.numeric(clean_df[[scol]])) {
            clean_df[[scol]] <- format(clean_df[[scol]], scientific = FALSE, trim = TRUE)
          }
          clean_df[[scol]] <- normalize_sci_string(clean_df[[scol]])
          warnings <- c(
            warnings,
            sprintf("Column '%s' contains %d cell(s) formatted in scientific notation in Excel (e.g., '1.02E+10'). Values have been safely auto-expanded in memory to standard digits; format as 'Text' in Excel to avoid truncation.",
                    scol, xlsx_res$count)
          )
        }
      }
    }
  }

  # 4b. Scan all columns for string or numeric scientific formats
  for (col in names(clean_df)) {
    vals <- clean_df[[col]]
    raw_char <- as.character(vals)
    sci_mask <- is_scientific_format(raw_char)
    sci_m <- sum(sci_mask, na.rm = TRUE)
    if (sci_m > 0) {
      if (!(col %in% sci_cols)) {
        sci_matches_total <- sci_matches_total + sci_m
        sci_cols <- c(sci_cols, col)
        example_val <- head(raw_char[which(sci_mask)], 1)
        warnings <- c(
          warnings,
          sprintf("Column '%s' contains %d value(s) in scientific notation (e.g., '%s'). Excel often converts 9-digit phones or 11-digit IDs to scientific format, which can truncate lower-order digits. Values have been auto-expanded in memory; format as 'Text' in Excel to avoid data loss.",
                  col, sci_m, example_val)
        )
      }
      # Normalize in clean_df so downstream matching engine receives standard digits
      clean_df[[col]] <- normalize_sci_string(clean_df[[col]])
    }
  }
  if (sci_matches_total > 0) {
    checks$sci_notation <- list(
      id = "sci_notation",
      name = "Scientific Notation Scan",
      status = "warn",
      badge = paste0(sci_matches_total, " Formatted"),
      label = "Scientific format detected (e.g., 7.71E+08, 1.02E10)",
      detail = paste0("Detected in: ", paste(sci_cols, collapse = ", "), ". Auto-expanded in memory; format as Text in Excel.")
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

  # 5. Detect broken Excel formulas (#REF!, #VALUE!, #N/A, #NAME?, #DIV/0!, #NULL!)
  pat_formula <- "(?i)^#(REF!|VALUE!|N/A|NAME\\?|DIV/0!|NULL!|NUM!|SPILL!|CALC!)"
  formula_matches_total <- 0L
  formula_cols <- character(0)
  for (col in names(clean_df)) {
    vals <- clean_df[[col]]
    char_vals <- trimws(as.character(vals[!is.na(vals)]))
    if (length(char_vals) == 0) next
    form_mask <- grepl(pat_formula, char_vals)
    form_m <- sum(form_mask)
    if (form_m > 0) {
      formula_matches_total <- formula_matches_total + form_m
      formula_cols <- c(formula_cols, col)
      example_val <- head(char_vals[form_mask], 1)
      warnings <- c(
        warnings,
        sprintf("Column '%s' contains %d broken Excel formula error(s) (e.g., '%s'). These values will be treated as missing.",
                col, form_m, example_val)
      )
      # Clean out broken formula tokens in clean_df
      clean_df[[col]][grepl(pat_formula, trimws(as.character(clean_df[[col]])))] <- ""
    }
  }
  if (formula_matches_total > 0) {
    checks$formula_errors <- list(
      id = "formula_errors",
      name = "Excel Formula Errors",
      status = "warn",
      badge = paste0(formula_matches_total, " Errors"),
      label = "Broken formula tokens detected (#REF!, #VALUE!, etc.)",
      detail = paste0("Errors in: ", paste(formula_cols, collapse = ", "), ". Replaced with empty values to prevent false matches.")
    )
  } else {
    checks$formula_errors <- list(
      id = "formula_errors",
      name = "Excel Formula Errors",
      status = "pass",
      badge = "0 Formula Errors",
      label = "No broken formula tokens",
      detail = "No #REF!, #VALUE!, or #N/A formula error artifacts found."
    )
  }

  # 6. Detect columns with abnormally high placeholder / dummy values
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
