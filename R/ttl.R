# R/ttl.R
# Automated TTL & Data Retention Cleanup Policy for CCY Deduplication Platform
# Ensures raw beneficiary upload payloads and transient files are purged according to data protection standards.

cleanup_expired_payloads <- function(max_age_days = 14, export_max_age_hours = 24) {
  result <- list(
    payloads_removed = 0L,
    exports_removed = 0L,
    bytes_reclaimed = 0,
    errors = character(0)
  )

  now <- Sys.time()

  # 1. Clean raw upload payloads in tmp/jobs/payloads
  payload_dir <- file.path(getwd(), "tmp", "jobs", "payloads")
  if (dir.exists(payload_dir)) {
    payload_files <- list.files(payload_dir, full.names = TRUE, pattern = "\\.rds$")
    for (f in payload_files) {
      tryCatch({
        fi <- file.info(f)
        if (!is.na(fi$mtime) && difftime(now, fi$mtime, units = "days") > max_age_days) {
          fsize <- fi$size
          if (unlink(f, force = TRUE) == 0) {
            result$payloads_removed <- result$payloads_removed + 1L
            result$bytes_reclaimed <- result$bytes_reclaimed + fsize
          }
        }
      }, error = function(e) {
        result$errors <- c(result$errors, paste0("Failed to unlink payload ", basename(f), ": ", conditionMessage(e)))
      })
    }
  }

  # 2. Clean temporary exported Excel files in tmp/ older than export_max_age_hours
  tmp_dir <- file.path(getwd(), "tmp")
  if (dir.exists(tmp_dir)) {
    export_files <- list.files(tmp_dir, full.names = TRUE, pattern = "\\.(xlsx|csv)$")
    for (f in export_files) {
      tryCatch({
        fi <- file.info(f)
        if (!is.na(fi$mtime) && difftime(now, fi$mtime, units = "hours") > export_max_age_hours) {
          fsize <- fi$size
          if (unlink(f, force = TRUE) == 0) {
            result$exports_removed <- result$exports_removed + 1L
            result$bytes_reclaimed <- result$bytes_reclaimed + fsize
          }
        }
      }, error = function(e) {
        result$errors <- c(result$errors, paste0("Failed to unlink export ", basename(f), ": ", conditionMessage(e)))
      })
    }
  }

  # 3. Log cleanup results if any files were pruned
  if (result$payloads_removed > 0 || result$exports_removed > 0) {
    log_line <- sprintf(
      "[%s] TTL Cleanup: removed %d raw payloads, %d temp exports. Reclaimed %.2f MB\n",
      format(now, "%Y-%m-%d %H:%M:%S"),
      result$payloads_removed,
      result$exports_removed,
      result$bytes_reclaimed / (1024^2)
    )
    cat(log_line, file = file.path(tmp_dir, "ttl_cleanup.log"), append = TRUE)
  }

  invisible(result)
}
