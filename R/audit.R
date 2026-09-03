# R/audit.R
# Export Audit Logging for CCY Deduplication Platform
# Tracks downloads of beneficiary deduplication dossiers and PII access for donor and humanitarian compliance.

get_audit_log_path <- function() {
  file.path(getwd(), "tmp", "audit_export_log.rds")
}

get_audit_csv_path <- function() {
  file.path(getwd(), "tmp", "audit_export_log.csv")
}

empty_audit_log <- function() {
  data.frame(
    timestamp = character(0),
    user_email = character(0),
    user_role = character(0),
    partner_name = character(0),
    file_name = character(0),
    record_count = integer(0),
    pii_masked = logical(0),
    job_id = character(0),
    stringsAsFactors = FALSE
  )
}

log_export_audit <- function(user_email = "local_user",
                             user_role = "partner_deduplicator",
                             partner_name = "CCY",
                             file_name = "dedup_results.xlsx",
                             record_count = 0L,
                             pii_masked = TRUE,
                             job_id = NA_character_) {
  tryCatch({
    tmp_dir <- file.path(getwd(), "tmp")
    if (!dir.exists(tmp_dir)) dir.create(tmp_dir, recursive = TRUE)

    rds_path <- get_audit_log_path()
    csv_path <- get_audit_csv_path()

    existing <- if (file.exists(rds_path)) {
      tryCatch(readRDS(rds_path), error = function(e) empty_audit_log())
    } else {
      empty_audit_log()
    }

    new_row <- data.frame(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      user_email = as.character(user_email %||% "unknown"),
      user_role = as.character(user_role %||% "unknown"),
      partner_name = as.character(partner_name %||% "All"),
      file_name = as.character(file_name %||% "dedup_results.xlsx"),
      record_count = as.integer(record_count %||% 0L),
      pii_masked = as.logical(pii_masked),
      job_id = as.character(job_id %||% ""),
      stringsAsFactors = FALSE
    )

    updated <- rbind(existing, new_row)
    saveRDS(updated, rds_path)

    # Append to human-readable CSV for external audit compliance
    is_first <- !file.exists(csv_path)
    utils::write.table(
      new_row,
      file = csv_path,
      sep = ",",
      row.names = FALSE,
      col.names = is_first,
      append = !is_first,
      qmethod = "double"
    )

    invisible(TRUE)
  }, error = function(e) {
    warning("Failed to log export audit entry: ", conditionMessage(e))
    invisible(FALSE)
  })
}

get_export_audit_log <- function() {
  rds_path <- get_audit_log_path()
  if (!file.exists(rds_path)) return(empty_audit_log())
  tryCatch({
    df <- readRDS(rds_path)
    if (!is.data.frame(df)) return(empty_audit_log())
    # Order latest events first
    df[order(df$timestamp, decreasing = TRUE), , drop = FALSE]
  }, error = function(e) empty_audit_log())
}
