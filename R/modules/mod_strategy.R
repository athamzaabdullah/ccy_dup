# R/modules/mod_strategy.R
# CCY Deduplication Platform - Matching Strategy & Recency Simulation Module

validate_matching_strategy <- function(high, medium, max_candidates) {
  high_num <- suppressWarnings(as.numeric(high))
  med_num <- suppressWarnings(as.numeric(medium))
  max_cand <- suppressWarnings(as.numeric(max_candidates))
  
  if (is.na(high_num) || is.na(med_num)) {
    return(list(valid = FALSE, message = "Confidence thresholds must be valid numbers."))
  }
  if (med_num >= high_num) {
    return(list(valid = FALSE, message = "High confidence threshold must be strictly greater than medium threshold."))
  }
  if (high_num > 100 || high_num < 50) {
    return(list(valid = FALSE, message = "High confidence threshold must be between 50% and 100%."))
  }
  if (med_num < 0 || med_num >= 100) {
    return(list(valid = FALSE, message = "Medium confidence threshold must be between 0% and 99%."))
  }
  if (is.na(max_cand) || max_cand < 50 || max_cand > 2000) {
    return(list(valid = FALSE, message = "Max candidate pairs must be between 50 and 2,000."))
  }
  
  list(
    valid = TRUE,
    high = high_num,
    medium = med_num,
    max_candidates = as.integer(max_cand)
  )
}

compute_mpca_simulator_metrics <- function(snap_path, window_months = 6, ref_date = Sys.Date()) {
  if (is.null(snap_path) || !file.exists(snap_path)) {
    return(list(ready = FALSE, message = "No valid master snapshot available."))
  }
  
  date_cols <- c("Dist_Date_Calc_New", "dist_date_calc_new", "DIST_DATE_CALC_NEW", "Dist_Date", "dist_date", "system_date", "1.4. Today's Date", "todays_date")
  m_dates <- tryCatch({
    load_master_lean(snap_path, columns = date_cols)
  }, error = function(e) NULL)
  
  sim <- simulate_mpca_window(m_dates, window_months = window_months, ref_date = ref_date)
  list(ready = TRUE, simulation = sim)
}
