to_abs_path <- function(path) {
  if (is.null(path) || !nzchar(path)) return(path)
  is_abs <- grepl("^[A-Za-z]:[/\\\\]|^[/\\\\]{2}|^/", path)
  if (is_abs) return(normalizePath(path, winslash = "/", mustWork = FALSE))
  normalizePath(file.path(getwd(), path), winslash = "/", mustWork = FALSE)
}

job_root <- to_abs_path(config$paths$master_snap_dir)
job_dir <- file.path(job_root, "jobs")
results_dir <- file.path(job_dir, "results")

ensure_job_dirs <- function() {
  if (!dir.exists(job_dir)) dir.create(job_dir, recursive = TRUE)
  if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)
}

ensure_job_dirs()

job_path <- function(id) file.path(job_dir, paste0(id, ".json"))
result_path <- function(id) file.path(results_dir, paste0(id, ".rds"))

write_job <- function(id, job) {
  ensure_job_dirs()
  path <- job_path(id)
  tmp <- paste0(path, ".tmp_", Sys.getpid(), "_", format(Sys.time(), "%H%M%OS6"))
  jsonlite::write_json(job, tmp, auto_unbox = TRUE)
  if (file.exists(path)) unlink(path)
  ok <- file.rename(tmp, path)
  if (!ok) {
    # Fallback for filesystems where rename can fail across boundaries.
    file.copy(tmp, path, overwrite = TRUE)
    unlink(tmp)
  }
}

read_job <- function(id) {
  path <- job_path(id)
  if (!file.exists(path)) return(NULL)
  for (attempt in seq_len(4)) {
    out <- tryCatch(
      jsonlite::read_json(path, simplifyVector = TRUE),
      error = function(e) NULL
    )
    if (!is.null(out)) return(out)
    Sys.sleep(0.03 * attempt)
  }
  NULL
}

new_job_id <- function() {
  paste0("job_", format(Sys.time(), "%Y%m%d_%H%M%S_"), sample(1000:9999, 1))
}

init_job <- function() {
  id <- new_job_id()
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  job <- list(
    id = id,
    status = "queued",
    progress = 0,
    message = "Queued",
    result_path = NULL,
    error = NULL,
    started_at = now,
    updated_at = now,
    history = c(paste0(now, " — Queued"))
  )
  write_job(id, job)
  id
}

set_job_progress <- function(id, progress, message = NULL) {
  job <- read_job(id)
  if (is.null(job)) return()
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  job$progress <- progress
  if (!is.null(message)) job$message <- message
  job$status <- "running"
  job$updated_at <- now
  if (!is.null(message)) {
    job$history <- c(job$history, paste0(now, " — ", message))
  }
  write_job(id, job)
}

set_job_result <- function(id, result) {
  job <- read_job(id)
  if (is.null(job)) return()
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  saveRDS(result, result_path(id))
  job$status <- "completed"
  job$progress <- 100
  job$message <- "Completed"
  job$result_path <- result_path(id)
  job$updated_at <- now
  job$history <- c(job$history, paste0(now, " — Completed"))
  write_job(id, job)
}

set_job_error <- function(id, error) {
  job <- read_job(id)
  if (is.null(job)) return()
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  job$status <- "failed"
  job$message <- error
  job$error <- error
  job$updated_at <- now
  job$history <- c(job$history, paste0(now, " — Failed: ", error))
  write_job(id, job)
}

format_job_error <- function(e) {
  msg <- conditionMessage(e)
  cls <- paste(class(e), collapse = "/")
  call <- NULL
  if (!is.null(e$call)) {
    call <- paste(deparse(e$call), collapse = " ")
  }
  parts <- c(msg, if (!is.null(call)) paste0("Call: ", call), paste0("Class: ", cls))
  paste(parts, collapse = " | ")
}

set_job_canceled <- function(id, message = "Canceled by user") {
  job <- read_job(id)
  if (is.null(job)) return()
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  job$status <- "canceled"
  job$message <- message
  job$updated_at <- now
  job$history <- c(job$history, paste0(now, " — ", message))
  write_job(id, job)
}

job_is_canceled <- function(id) {
  job <- read_job(id)
  if (is.null(job)) return(FALSE)
  identical(job$status, "canceled")
}

get_job <- function(id) {
  read_job(id)
}

get_job_result <- function(job) {
  if (is.null(job$result_path) || !file.exists(job$result_path)) return(NULL)
  readRDS(job$result_path)
}

enqueue_match_job <- function(upload_df, snapshot_path, mapping = NULL,
                              upload_filename = "uploaded_file.xlsx",
                              upload_time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                              fuzzy_high_threshold = config$thresholds$high,
                              fuzzy_medium_threshold = config$thresholds$medium,
                              age_tolerance = config$age_tolerance,
                              weights = config$weights,
                              match_fields = c(
                                "partner",
                                "hoh_ID_number",
                                "phone_number",
                                "hoh_arabic_name",
                                "hoh_spouse_name",
                                "geography"
                              ),
                              max_candidates = config$max_candidates) {
  id <- init_job()
  set_job_progress(id, 5, "Starting")

  payload_dir <- file.path(job_dir, "payloads")
  if (!dir.exists(payload_dir)) dir.create(payload_dir, recursive = TRUE)
  upload_path <- file.path(payload_dir, paste0(id, "_upload.rds"))
  saveRDS(upload_df, upload_path)

  future::future({
    tryCatch({
      if (job_is_canceled(id)) return(NULL)
      set_job_progress(id, 8, "Worker started")
      if (!file.exists(upload_path)) stop("Uploaded payload not found")
      if (is.null(snapshot_path) || !file.exists(snapshot_path)) {
        stop("Local master snapshot not found. Please fetch a fresh master database in Step 1.")
      }

      set_job_progress(id, 10, "Loading local master snapshot")
      master_df <- readRDS(snapshot_path)

      if (job_is_canceled(id)) return(NULL)
      set_job_progress(id, 30, "Loading upload data")
      upload_df <- readRDS(upload_path)

      if (job_is_canceled(id)) return(NULL)
      set_job_progress(id, 40, "Preparing upload")
      if (!is.null(mapping) && length(mapping) > 0) {
        for (req_col in names(mapping)) {
          src <- mapping[[req_col]]
          if (!req_col %in% names(upload_df) && src %in% names(upload_df)) {
            upload_df[[req_col]] <- upload_df[[src]]
          }
        }
      }

      if (job_is_canceled(id)) return(NULL)
      set_job_progress(id, 70, "Matching records")
      result <- run_dedup(
        upload_df,
        master_df,
        upload_filename = upload_filename,
        upload_time = upload_time,
        fuzzy_high_threshold = fuzzy_high_threshold,
        fuzzy_medium_threshold = fuzzy_medium_threshold,
        age_delta = age_tolerance,
        weights = weights,
        match_fields = match_fields,
        max_candidates = max_candidates
      )

      if (job_is_canceled(id)) return(NULL)
      set_job_progress(id, 90, "Finalizing")
      set_job_result(id, result)
    }, error = function(e) {
      set_job_error(id, format_job_error(e))
    }, finally = {
      if (file.exists(upload_path)) unlink(upload_path)
    })
  })

  id
}

enqueue_master_fetch_job <- function(cfg) {
  id <- init_job()
  set_job_progress(id, 5, "Starting master fetch")

  future::future({
    tryCatch({
      set_job_progress(id, 6, "Worker started")
      if (job_is_canceled(id)) return(NULL)
      set_job_progress(id, 8, "Connecting to ActivityInfo")
      activityinfo_list_databases(cfg = cfg)
      set_job_progress(id, 12, "Connection confirmed")

      if (job_is_canceled(id)) return(NULL)
      set_job_progress(id, 15, "Resolving ActivityInfo forms")
      form_ids <- activityinfo_resolve_form_ids(cfg)
      batch_size <- if (!is.null(cfg$batch_size)) as.integer(cfg$batch_size) else 2000L
      set_job_progress(id, 18, paste0("Found ", length(form_ids), " forms to fetch (chunk size ", batch_size, ")"))

      progress_cb <- function(i, total, msg) {
        if (job_is_canceled(id)) return(NULL)
        chunk_index <- 0L
        rows_fetched <- 0L
        m_chunk <- regexec("chunk ([0-9]+)", msg)
        hit_chunk_list <- regmatches(msg, m_chunk)
        hit_chunk <- character(0)
        if (length(hit_chunk_list) >= 1 && length(hit_chunk_list[[1]]) >= 1) hit_chunk <- hit_chunk_list[[1]] else hit_chunk <- character(0)
        if (length(hit_chunk) >= 2) chunk_index <- as.integer(hit_chunk[2])
        m_rows <- regexec("rows fetched ([0-9]+)", msg)
        hit_rows_list <- regmatches(msg, m_rows)
        hit_rows <- character(0)
        if (length(hit_rows_list) >= 1 && length(hit_rows_list[[1]]) >= 1) hit_rows <- hit_rows_list[[1]] else hit_rows <- character(0)
        if (length(hit_rows) >= 2) rows_fetched <- as.integer(hit_rows[2])

        # Advance progress inside each form so large forms don't appear stuck.
        within_form <- if (chunk_index <= 0) 0 else min(0.95, 1 - exp(-chunk_index / 4))
        frac <- ((i - 1) + within_form) / total
        pct <- 18 + round(frac * 70)
        pct <- max(18, min(88, pct))

        set_job_progress(
          id,
          pct,
          paste0(
            "Fetching form ", i, " of ", total, " (",
            msg, ", chunk size ", batch_size, ")"
          )
        )
      }
      cancel_cb <- function() job_is_canceled(id)

      df <- activityinfo_fetch_all_progress(
        cfg = cfg,
        form_ids = form_ids,
        progress_cb = progress_cb,
        cancel_cb = cancel_cb
      )
      if (job_is_canceled(id)) return(NULL)

      set_job_progress(id, 90, "Saving snapshot")
      path <- save_master_snapshot(df)

      set_job_progress(id, 98, "Finalizing")
      set_job_result(id, list(snapshot_path = path, rows = nrow(df)))
    }, error = function(e) {
      set_job_error(id, format_job_error(e))
    })
  })

  id
}

run_master_fetch_sync <- function(cfg, progress_cb = NULL, cancel_cb = NULL) {
  if (!is.null(progress_cb)) progress_cb(8, "Connecting to ActivityInfo")
  activityinfo_list_databases(cfg = cfg)
  if (!is.null(progress_cb)) progress_cb(15, "Connection confirmed")

  form_ids <- activityinfo_resolve_form_ids(cfg)
  batch_size <- if (!is.null(cfg$batch_size)) as.integer(cfg$batch_size) else 2000L
  if (!is.null(progress_cb)) progress_cb(18, paste0("Found ", length(form_ids), " forms to fetch (chunk size ", batch_size, ")"))

  df <- activityinfo_fetch_all_progress(cfg = cfg, form_ids = form_ids, progress_cb = function(i, total, msg) {
    chunk_index <- 0L
    m_chunk <- regexec("chunk ([0-9]+)", msg)
    hit_chunk_list <- regmatches(msg, m_chunk)
    hit_chunk <- if (length(hit_chunk_list) >= 1) hit_chunk_list[[1]] else character(0)
    if (length(hit_chunk) >= 2) chunk_index <- as.integer(hit_chunk[2])

    within_form <- if (chunk_index <= 0) 0 else min(0.95, 1 - exp(-chunk_index / 4))
    frac <- ((i - 1) + within_form) / total
    pct <- 18 + round(frac * 70)
    pct <- max(18, min(88, pct))

    if (!is.null(progress_cb)) {
      progress_cb(pct, paste0("Fetching form ", i, " of ", total, " (", msg, ", chunk size ", batch_size, ")"))
    }
  }, cancel_cb = cancel_cb)

  if (is.null(df)) return(list(canceled = TRUE))

  if (!is.null(progress_cb)) progress_cb(90, "Saving snapshot")
  path <- save_master_snapshot(df)
  if (!is.null(progress_cb)) progress_cb(98, "Finalizing")
  list(canceled = FALSE, snapshot_path = path, rows = nrow(df))
}
