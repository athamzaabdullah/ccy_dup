# R/modules/mod_results.R
# CCY Deduplication Platform - Results Dossier & Export Security Module

compile_export_dossier_meta <- function(upload_path = NULL,
                                       upload_df = NULL,
                                       master_snapshot_path = NULL,
                                       auth_user = list(),
                                       job_result = list()) {
  upload_hash <- if (!is.null(upload_path) && file.exists(upload_path)) {
    compute_file_sha256(upload_path)
  } else if (!is.null(upload_df)) {
    compute_data_sha256(upload_df)
  } else {
    "Not Available"
  }
  
  master_hash <- if (!is.null(master_snapshot_path) && file.exists(master_snapshot_path)) {
    compute_file_sha256(master_snapshot_path)
  } else {
    "Not Available"
  }
  
  manifest_id <- paste0("CCY-AUDIT-", format(Sys.time(), "%Y%m%d%H%M%S-"), sample(1000:9999, 1))
  
  total_recs <- 0L
  if (!is.null(job_result$summary) && !is.null(job_result$summary$total_pairs)) {
    total_recs <- as.integer(job_result$summary$total_pairs)
  } else if (!is.null(job_result$list_vs_master_exact)) {
    total_recs <- as.integer(nrow(as.data.frame(job_result$list_vs_master_exact)))
  }
  
  list(
    upload_sha256 = upload_hash,
    master_sha256 = master_hash,
    manifest_id = manifest_id,
    user_email = auth_user$email %||% "local_user",
    user_role = auth_user$role %||% "partner_deduplicator",
    partner_name = auth_user$partner_name %||% "CCY",
    record_count = total_recs
  )
}
