activityinfo_setup <- function(cfg = config$activityinfo) {
  if (is.null(cfg$token) || cfg$token == "") {
    stop("ActivityInfo token not set")
  }
  activityinfo::activityInfoRootUrl(cfg$base_url)
  activityinfo::activityInfoToken(cfg$token, prompt = FALSE)
}

activityinfo_required_columns <- function() {
  c(
    "X.id",
    "_id",
    "@id",
    "record_id",
    "QA_Code_SN",
    "QA_Code",
    "1.1. Organization Prefix",
    "1.1. Organization_text",
    "Partner Prefix",
    "Main Form Partner Batch Code",
    "1.1. Organization",
    "Partner",
    "organization",
    "1.4. Today's Date",
    "todays_date",
    "system_date",
    "1.2. Interviewer",
    "interviewer",
    "main_ref",
    "3.1. Head of household (HoH) Name (Arabic)",
    "hoh_arabic_name",
    "3.2. Head of HH Marital Status",
    "hoh_marital_status",
    "3.3. Head of HH's Spouse Name",
    "hoh_spouse_name",
    "3.4. Age of the head of the household?",
    "hoh_age",
    "3.5. Head of HH Gender",
    "hoh_sex",
    "3.11 What is the main form of ID that the Head of Household uses?",
    "hoh_id_type",
    "3.12 What is the Head of Household's ID number?",
    "hoh_id_number",
    "family_count",
    "2.1. Primary Phone Number:",
    "primary_phone_number",
    "2.2. Secondary Phone Number:",
    "secondary_phone_number",
    "2.3. Beneficiary Status",
    "beneficiary_status",
    "Dist_Type",
    "Dist_Date_Calc_New",
    "Dist_Date_Calc",
    "1.11. Governorate",
    "Governorate Label",
    "governorate",
    "1.12. District",
    "District Label",
    "district",
    "1.13. Sub-District",
    "Subdistrict Label",
    "sub_district",
    "1.14. Village",
    "village"
  )
}

activityinfo_list_databases <- function(cfg = config$activityinfo) {
  activityinfo_setup(cfg)
  activityinfo::getDatabases()
}

activityinfo_fetch_form <- function(form_id, cfg = config$activityinfo) {
  activityinfo_setup(cfg)
  tryCatch({
    activityinfo::queryTable(
      form = form_id,
      truncateStrings = FALSE,
      makeNames = FALSE
    )
  }, error = function(e) {
    columns <- activityinfo_required_columns()
    activityinfo::queryTable(
      form = form_id,
      columns = setNames(paste0("[", columns, "]"), columns),
      truncateStrings = FALSE,
      makeNames = FALSE
    )
  })
}

activityinfo_fetch_all <- function(cfg = config$activityinfo) {
  form_ids <- activityinfo_resolve_form_ids(cfg)
  records <- lapply(form_ids, function(id) {
    df <- activityinfo_fetch_form(id, cfg = cfg)
    if (nrow(df) == 0) return(NULL)
    df$.source_form_id <- id
    df
  })
  records <- Filter(Negate(is.null), records)
  if (length(records) == 0) return(data.frame())
  dplyr::bind_rows(records)
}

activityinfo_resolve_form_ids <- function(cfg = config$activityinfo) {
  form_ids <- cfg$form_ids
  if (length(form_ids) == 0) {
    stop("No ActivityInfo form IDs configured")
  }
  form_ids
}

activityinfo_fetch_all_progress <- function(cfg = config$activityinfo, form_ids = NULL,
                                            progress_cb = NULL, cancel_cb = NULL) {
  if (is.null(form_ids)) {
    form_ids <- activityinfo_resolve_form_ids(cfg)
  }
  activityinfo_setup(cfg)
  total <- length(form_ids)
  records <- list()
  for (i in seq_along(form_ids)) {
    if (!is.null(cancel_cb) && isTRUE(cancel_cb())) return(NULL)
    form_id <- form_ids[[i]]
    if (!is.null(progress_cb)) progress_cb(i, total, form_id)
    batch_size <- if (!is.null(cfg$batch_size)) cfg$batch_size else 2000
    batch_size <- as.integer(batch_size)
    offset <- as.integer(0)
    chunk_index <- as.integer(0)
    df_list <- list()
    if (!is.null(progress_cb)) {
      progress_cb(i, total, paste0(form_id, " | chunk 0 | rows fetched 0"))
    }
    repeat {
      if (!is.null(cancel_cb) && isTRUE(cancel_cb())) return(NULL)
      chunk <- tryCatch({
        activityinfo::queryTable(
          form = form_id,
          window = as.integer(c(offset, batch_size)),
          truncateStrings = FALSE,
          makeNames = FALSE
        )
      }, error = function(e) {
        columns <- activityinfo_required_columns()
        activityinfo::queryTable(
          form = form_id,
          columns = setNames(paste0("[", columns, "]"), columns),
          window = as.integer(c(offset, batch_size)),
          truncateStrings = FALSE,
          makeNames = FALSE
        )
      })
      if (nrow(chunk) == 0) break
      chunk_index <- chunk_index + 1L
      df_list[[length(df_list) + 1]] <- chunk
      offset <- offset + nrow(chunk)
      if (!is.null(progress_cb)) {
        progress_cb(i, total, paste0(form_id, " | chunk ", chunk_index, " | rows fetched ", offset))
      }
      if (nrow(chunk) < batch_size) break
    }
    df <- if (length(df_list) == 0) data.frame() else dplyr::bind_rows(df_list)
    if (nrow(df) == 0) next
    df$.source_form_id <- form_id
    records[[length(records) + 1]] <- df
  }
  if (length(records) == 0) return(data.frame())
  dplyr::bind_rows(records)
}

extract_lean_master <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(df)

  # 1. Direct and normalized matching against canonical mapping dictionary
  matched_by_dict <- character(0)
  if (exists("get_activityinfo_mapping_dict", mode = "function")) {
    dict <- get_activityinfo_mapping_dict()
    all_dict_terms <- unique(c(names(dict), unlist(dict, use.names = FALSE)))
    clean_str <- function(s) tolower(gsub("[^a-zA-Z0-9]", "", s))
    clean_dict_terms <- clean_str(all_dict_terms)
    clean_df_cols <- clean_str(names(df))
    matched_by_dict <- names(df)[names(df) %in% all_dict_terms | clean_df_cols %in% clean_dict_terms]
  }

  # 2. Comprehensive regex patterns covering standard humanitarian question numbering and system fields
  essential_patterns <- c(
    "^(@|_)?id$", "^record_id$", "^status$",
    "QA_CODE_SN", "QA_Code", "qa_code",
    "Main[ _]Form[ _]Partner[ _]Batch[ _]Code", "batch",
    # Specific section-number anchors starting at the beginning of the column name
    "^(upload_)?3[._]12([._ ]|$)", "id[-_ ]?number", "national[-_ ]?id", "nid", "hoh[-_ ]?id",
    "^(upload_)?3[._]11([._ ]|$)", "id[-_ ]?type", "main[-_ ]?form[-_ ]?of[-_ ]?id",
    "^(upload_)?3[._]1([._ ]|$)", "hoh[-_ ]?arabic[-_ ]?name", "hoh[-_ ]?name",
    "^(upload_)?3[._]2([._ ]|$)", "marital[-_ ]?status",
    "^(upload_)?3[._]3([._ ]|$)", "spouse[-_ ]?name", "hoh[-_ ]?spouse",
    "^(upload_)?3[._]4([._ ]|$)", "hoh[-_ ]?age",
    "^(upload_)?3[._]5([._ ]|$)", "hoh[-_ ]?(sex|gender)",
    "^(upload_)?2[._]1([._ ]|$)", "primary[-_ ]?phone",
    "^(upload_)?2[._]2([._ ]|$)", "secondary[-_ ]?phone",
    "^(upload_)?2[._]3([._ ]|$)", "beneficiary[-_ ]?status",
    "^(upload_)?1[._]1([._ ]|$)", "partner", "organization",
    "^(upload_)?1[._]2([._ ]|$)", "interviewer",
    "^(upload_)?1[._]4([._ ]|$)", "system[-_ ]?date",
    "^(upload_)?1[._]11([._ ]|$)", "governorate",
    "^(upload_)?1[._]12([._ ]|$)", "district",
    "^(upload_)?1[._]13([._ ]|$)", "sub[-_ ]?district",
    "^(upload_)?1[._]14([._ ]|$)", "village",
    "family[-_ ]?size", "household[-_ ]?size",
    "dist[-_ ]?date", "Dist_Date_Calc_New", "Last[ _]Receipt[ _]Date",
    "Dist_Type", "dist_type", "Distribution[ _]Type"
  )
  combined <- paste(essential_patterns, collapse = "|")
  pattern_cols <- grep(combined, names(df), ignore.case = TRUE, value = TRUE)

  keep_cols <- unique(c(matched_by_dict, pattern_cols))
  if (length(keep_cols) > 0) {
    df[, keep_cols, drop = FALSE]
  } else {
    df
  }
}

save_master_snapshot <- function(df, snap_dir = config$paths$master_snap_dir) {
  is_abs <- grepl("^[A-Za-z]:[/\\\\]|^[/\\\\]{2}|^/", snap_dir)
  if (!is_abs) {
    snap_dir <- normalizePath(file.path(getwd(), snap_dir), winslash = "/", mustWork = FALSE)
  } else {
    snap_dir <- normalizePath(snap_dir, winslash = "/", mustWork = FALSE)
  }
  if (!dir.exists(snap_dir)) dir.create(snap_dir, recursive = TRUE)
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  path <- file.path(snap_dir, paste0("master_snapshot_", ts, ".rds"))
  saveRDS(df, path)

  # Companion Lean Master Index for fast low-memory matching
  tryCatch({
    lean_df <- extract_lean_master(df)
    lean_path <- file.path(snap_dir, paste0("master_lean_", ts, ".rds"))
    saveRDS(lean_df, lean_path)
  }, error = function(e) {
    warning("Could not write lean master snapshot: ", conditionMessage(e))
  })

  normalizePath(path, winslash = "/", mustWork = FALSE)
}

