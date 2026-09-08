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

  # Also write Apache Arrow Parquet for full master snapshot
  full_parquet_path <- file.path(snap_dir, paste0("master_snapshot_", ts, ".parquet"))
  tryCatch({
    if (requireNamespace("arrow", quietly = TRUE)) {
      arrow::write_parquet(df, full_parquet_path)
    }
  }, error = function(pe) NULL)

  # Companion Lean Master Index for fast low-memory matching
  tryCatch({
    lean_df <- extract_lean_master(df)
    lean_path <- file.path(snap_dir, paste0("master_lean_", ts, ".rds"))
    saveRDS(lean_df, lean_path)
    
    # Write Apache Arrow Parquet for high-speed projection & low memory
    parquet_path <- file.path(snap_dir, paste0("master_lean_", ts, ".parquet"))
    tryCatch({
      if (requireNamespace("arrow", quietly = TRUE)) {
        arrow::write_parquet(lean_df, parquet_path)
      }
    }, error = function(pe) NULL)
  }, error = function(e) {
    warning("Could not write lean master snapshot: ", conditionMessage(e))
  })

  normalizePath(path, winslash = "/", mustWork = FALSE)
}

load_master_lean <- function(path, columns = NULL) {
  if (is.null(path) || !nzchar(path)) return(NULL)
  
  lean_path <- if (grepl("master_snapshot_", path)) {
    gsub("master_snapshot_", "master_lean_", path)
  } else {
    path
  }
  
  parquet_path <- gsub("\\.rds$", ".parquet", lean_path)
  if (!file.exists(parquet_path) && grepl("master_lean_", parquet_path)) {
    alt_parquet <- gsub("master_lean_", "master_snapshot_", parquet_path)
    if (file.exists(alt_parquet)) parquet_path <- alt_parquet
  }
  
  # 1. Zero-copy memory mapped query via DuckDB
  if (file.exists(parquet_path) && requireNamespace("duckdb", quietly = TRUE) && requireNamespace("DBI", quietly = TRUE)) {
    out <- tryCatch({
      con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
      on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
      escaped_path <- gsub("\\\\", "/", normalizePath(parquet_path, winslash = "/", mustWork = FALSE))
      
      if (!is.null(columns) && length(columns) > 0) {
        cols_info <- DBI::dbGetQuery(con, sprintf("DESCRIBE SELECT * FROM read_parquet('%s')", escaped_path))
        avail <- intersect(columns, cols_info$column_name)
        if (length(avail) > 0) {
          col_sql <- paste(vapply(avail, function(c) paste0('"', c, '"'), character(1)), collapse = ", ")
          res <- DBI::dbGetQuery(con, sprintf("SELECT %s FROM read_parquet('%s')", col_sql, escaped_path))
          return(as.data.frame(res))
        }
      }
      res <- DBI::dbGetQuery(con, sprintf("SELECT * FROM read_parquet('%s')", escaped_path))
      as.data.frame(res)
    }, error = function(e) NULL)
    if (!is.null(out) && nrow(out) > 0) return(out)
  }

  # 2. High-speed projection via Apache Arrow
  if (file.exists(parquet_path) && requireNamespace("arrow", quietly = TRUE)) {
    out <- tryCatch({
      if (!is.null(columns) && length(columns) > 0) {
        schema_names <- arrow::read_parquet(parquet_path, as_data_frame = FALSE)$schema$names
        avail <- intersect(columns, schema_names)
        if (length(avail) > 0) {
          return(as.data.frame(arrow::read_parquet(parquet_path, col_select = dplyr::all_of(avail))))
        }
      }
      as.data.frame(arrow::read_parquet(parquet_path))
    }, error = function(e) NULL)
    if (!is.null(out)) return(out)
  }
  
  # 3. Fallback to RDS
  if (file.exists(lean_path)) {
    return(readRDS(lean_path))
  }
  if (file.exists(path)) {
    return(readRDS(path))
  }
  NULL
}

query_master_duckdb <- function(parquet_path, sql = NULL, columns = NULL, where_clause = NULL) {
  if (!file.exists(parquet_path)) return(NULL)
  if (!requireNamespace("duckdb", quietly = TRUE) || !requireNamespace("DBI", quietly = TRUE)) {
    return(NULL)
  }
  con <- DBI::dbConnect(duckdb::duckdb(shared_home = FALSE))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  escaped_path <- gsub("\\\\", "/", normalizePath(parquet_path, winslash = "/", mustWork = FALSE))
  
  if (!is.null(sql) && nzchar(trimws(sql))) {
    full_sql <- gsub("\\{table\\}|\\{parquet\\}", sprintf("read_parquet('%s')", escaped_path), sql)
    return(as.data.frame(DBI::dbGetQuery(con, full_sql)))
  }
  
  cols_sql <- if (!is.null(columns) && length(columns) > 0) {
    paste(vapply(columns, function(c) paste0('"', c, '"'), character(1)), collapse = ", ")
  } else {
    "*"
  }
  query <- sprintf("SELECT %s FROM read_parquet('%s')", cols_sql, escaped_path)
  if (!is.null(where_clause) && nzchar(trimws(where_clause))) {
    query <- paste(query, "WHERE", where_clause)
  }
  as.data.frame(DBI::dbGetQuery(con, query))
}

activityinfo_find_id_col <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  candidates <- c("@id", "_id", "X.id", "record_id", "QA_Code_SN", "QA_Code")
  for (cand in candidates) {
    if (cand %in% names(df)) return(cand)
  }
  id_match <- grep("^(@|_)?id$|^record_id$", names(df), ignore.case = TRUE, value = TRUE)
  if (length(id_match) > 0) return(id_match[1])
  NULL
}

activityinfo_merge_delta <- function(base_df, delta_df, id_col = NULL) {
  if (is.null(base_df) || nrow(base_df) == 0) {
    return(list(
      data = delta_df,
      n_inserted = if (is.null(delta_df)) 0L else nrow(delta_df),
      n_updated = 0L
    ))
  }
  if (is.null(delta_df) || nrow(delta_df) == 0) {
    return(list(
      data = base_df,
      n_inserted = 0L,
      n_updated = 0L
    ))
  }

  if (is.null(id_col)) {
    id_base <- activityinfo_find_id_col(base_df)
    id_delta <- activityinfo_find_id_col(delta_df)
    if (!is.null(id_base) && id_base %in% names(delta_df)) {
      id_col <- id_base
    } else if (!is.null(id_delta) && id_delta %in% names(base_df)) {
      id_col <- id_delta
    }
  }

  if (is.null(id_col) || !id_col %in% names(base_df) || !id_col %in% names(delta_df)) {
    merged <- dplyr::bind_rows(base_df, delta_df)
    return(list(
      data = merged,
      n_inserted = nrow(delta_df),
      n_updated = 0L
    ))
  }

  base_ids <- as.character(base_df[[id_col]])
  delta_ids <- as.character(delta_df[[id_col]])

  is_existing <- delta_ids %in% base_ids
  updates_df <- delta_df[is_existing, , drop = FALSE]
  inserts_df <- delta_df[!is_existing, , drop = FALSE]

  n_updated <- nrow(updates_df)
  n_inserted <- nrow(inserts_df)

  updated_ids <- as.character(updates_df[[id_col]])
  retained_base <- base_df[!base_ids %in% updated_ids, , drop = FALSE]

  merged <- dplyr::bind_rows(retained_base, updates_df, inserts_df)

  list(
    data = merged,
    n_inserted = n_inserted,
    n_updated = n_updated
  )
}

activityinfo_sync_delta <- function(cfg = config$activityinfo,
                                    base_snapshot_path = NULL,
                                    last_sync_time = NULL,
                                    form_ids = NULL,
                                    progress_cb = NULL,
                                    cancel_cb = NULL) {
  snap_dir <- cfg$paths$master_snap_dir %||% (if (exists("config") && !is.null(config$paths$master_snap_dir)) config$paths$master_snap_dir else "data/master_snapshots")
  is_abs <- grepl("^[A-Za-z]:[/\\\\]|^[/\\\\]{2}|^/", snap_dir)
  if (!is_abs) snap_dir <- normalizePath(file.path(getwd(), snap_dir), winslash = "/", mustWork = FALSE)

  if (is.null(base_snapshot_path) || !file.exists(base_snapshot_path)) {
    candidates <- list.files(snap_dir, pattern = "^master_snapshot_.*\\.rds$", full.names = TRUE)
    if (length(candidates) > 0) {
      mtimes <- file.info(candidates)$mtime
      base_snapshot_path <- candidates[order(mtimes, decreasing = TRUE)][1]
    }
  }

  if (is.null(base_snapshot_path) || !file.exists(base_snapshot_path)) {
    if (!is.null(progress_cb)) progress_cb(10, "No existing master snapshot found. Performing initial full sync...")
    full_df <- activityinfo_fetch_all_progress(cfg = cfg, form_ids = form_ids, progress_cb = progress_cb, cancel_cb = cancel_cb)
    if (is.null(full_df) || (!is.null(cancel_cb) && isTRUE(cancel_cb()))) return(list(canceled = TRUE))
    path <- save_master_snapshot(full_df, snap_dir = snap_dir)
    return(list(
      canceled = FALSE,
      snapshot_path = path,
      rows = nrow(full_df),
      new_records = nrow(full_df),
      updated_records = 0L,
      sync_type = "full_initial",
      synced_at = Sys.time()
    ))
  }

  if (!is.null(progress_cb)) progress_cb(20, "Loading existing master database snapshot...")
  base_df <- readRDS(base_snapshot_path)

  sync_since <- if (!is.null(last_sync_time)) {
    as.POSIXct(last_sync_time)
  } else {
    file.info(base_snapshot_path)$mtime
  }

  if (is.null(form_ids)) {
    form_ids <- activityinfo_resolve_form_ids(cfg)
  }

  if (!is.null(progress_cb)) progress_cb(35, paste0("Querying ActivityInfo delta updates since ", format(sync_since, "%Y-%m-%d %H:%M:%S"), "..."))

  activityinfo_setup(cfg)
  delta_records <- list()
  for (i in seq_along(form_ids)) {
    if (!is.null(cancel_cb) && isTRUE(cancel_cb())) return(list(canceled = TRUE))
    fid <- form_ids[[i]]
    df_delta <- tryCatch({
      activityinfo::queryTable(
        form = fid,
        truncateStrings = FALSE,
        makeNames = FALSE
      )
    }, error = function(e) {
      columns <- activityinfo_required_columns()
      tryCatch({
        activityinfo::queryTable(
          form = fid,
          columns = setNames(paste0("[", columns, "]"), columns),
          truncateStrings = FALSE,
          makeNames = FALSE
        )
      }, error = function(e2) data.frame())
    })
    if (nrow(df_delta) > 0) {
      df_delta$.source_form_id <- fid
      delta_records[[length(delta_records) + 1]] <- df_delta
    }
  }

  delta_df <- if (length(delta_records) == 0) data.frame() else dplyr::bind_rows(delta_records)

  if (!is.null(progress_cb)) progress_cb(75, "Merging delta records into local master index...")
  merge_res <- activityinfo_merge_delta(base_df, delta_df)

  if (!is.null(progress_cb)) progress_cb(90, "Writing updated master snapshot and Parquet index...")
  new_snap_path <- save_master_snapshot(merge_res$data, snap_dir = snap_dir)

  if (!is.null(progress_cb)) progress_cb(100, "Delta sync complete.")

  list(
    canceled = FALSE,
    snapshot_path = new_snap_path,
    rows = nrow(merge_res$data),
    new_records = merge_res$n_inserted,
    updated_records = merge_res$n_updated,
    sync_type = "delta_sync",
    synced_at = Sys.time()
  )
}
