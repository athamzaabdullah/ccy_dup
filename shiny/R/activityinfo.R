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
    "QA_Code_SN",
    "organization",
    "todays_date",
    "interviewer",
    "main_ref",
    "hoh_arabic_name",
    "hoh_marital_status",
    "hoh_spouse_name",
    "hoh_age",
    "hoh_sex",
    "hoh_id_type",
    "hoh_id_number",
    "primary_phone_number",
    "secondary_phone_number",
    "beneficiary_status",
    "Dist_Type",
    "Dist_Date_Calc_New",
    "governorate",
    "district",
    "sub_district",
    "village"
  )
}

activityinfo_list_databases <- function(cfg = config$activityinfo) {
  activityinfo_setup(cfg)
  activityinfo::getDatabases()
}

activityinfo_fetch_form <- function(form_id, cfg = config$activityinfo) {
  activityinfo_setup(cfg)
  columns <- activityinfo_required_columns()
  activityinfo::queryTable(
    form = form_id,
    columns = setNames(paste0("[", columns, "]"), columns),
    truncateStrings = FALSE,
    makeNames = FALSE
  )
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
      columns <- activityinfo_required_columns()
      chunk <- activityinfo::queryTable(
        form = form_id,
        columns = setNames(paste0("[", columns, "]"), columns),
        window = as.integer(c(offset, batch_size)),
        truncateStrings = FALSE,
        makeNames = FALSE
      )
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

save_master_snapshot <- function(df, snap_dir = config$paths$master_snap_dir) {
  is_abs <- grepl("^[A-Za-z]:[/\\\\]|^[/\\\\]{2}|^/", snap_dir)
  if (!is_abs) {
    snap_dir <- normalizePath(file.path(getwd(), snap_dir), winslash = "/", mustWork = FALSE)
  } else {
    snap_dir <- normalizePath(snap_dir, winslash = "/", mustWork = FALSE)
  }
  if (!dir.exists(snap_dir)) dir.create(snap_dir, recursive = TRUE)
  path <- file.path(snap_dir, paste0("master_snapshot_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))
  saveRDS(df, path)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}
