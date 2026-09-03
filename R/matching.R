# Helper for exact pair IDs
exact_pair_id <- function(prefix, a, b) {
  paste0(prefix, "_", sprintf("%06d", a), "_", sprintf("%06d", b))
}

safe_char_scalar <- function(x) {
  if (is.null(x) || length(x) == 0) return("")
  # Prefer [1] extraction which works for vectors and lists; guard against NULL/NA
  if (length(x) >= 1) {
    val <- x[1]
  } else {
    val <- NA
  }
  if (is.null(val) || (is.atomic(val) && length(val) == 0) || is.na(val)) return("")
  as.character(val)
}

# Vectorized versions of similarity functions
v_safe_text_sim <- function(a, b, method = "jw") {
  a <- as.character(a)
  b <- as.character(b)

  a[is.na(a)] <- ""
  b[is.na(b)] <- ""

  a_trim <- trimws(a)
  b_trim <- trimws(b)

  # Empty or missing values carry no matching evidence.
  valid <- nzchar(a_trim) & nzchar(b_trim)
  out <- rep(0, length(a_trim))
  if (any(valid)) {
    sim <- stringdist::stringsim(a_trim[valid], b_trim[valid], method = method)
    out[valid] <- round(100 * sim, 1)
  }
  out
}

v_token_sort <- function(x) {
  # This is hard to fully vectorize without a loop or specialized function
  # but we can apply it to unique values for efficiency
  u_x <- unique(x)
  u_res <- vapply(u_x, function(val) {
    if (is.na(val) || !nzchar(val)) return("")
    toks <- unlist(strsplit(val, "\\s+"))
    toks <- toks[nzchar(toks)]
    if (length(toks) == 0) return("")
    paste(sort(toks), collapse = " ")
  }, character(1))
  u_res[x]
}

v_lev_sim <- function(a, b) {
  dist <- stringdist::stringdist(a, b, method = "lv")
  max_len <- pmax(nchar(a), nchar(b))
  sim <- 1 - (dist / pmax(max_len, 1))
  sim[max_len == 0] <- 0
  round(100 * sim, 1)
}

v_name_similarity <- function(a, b) {
  jw <- v_safe_text_sim(a, b, method = "jw")
  ts <- v_safe_text_sim(v_token_sort(a), v_token_sort(b), method = "jw")
  lv <- v_lev_sim(a, b)
  round((jw + ts + lv) / 3, 1)
}

v_phone_similarity <- function(a, b) {
  res <- rep(0, length(a))
  res[a == b & nzchar(a)] <- 100
  
  # Partial matches for non-exact ones
  idx <- res == 0 & nzchar(a) & nzchar(b)
  if (any(idx)) {
    sub_a <- a[idx]
    sub_b <- b[idx]
    min_len <- pmin(nchar(sub_a), nchar(sub_b))
    
    # 7-digit suffix
    idx7 <- min_len >= 7 & substr(sub_a, nchar(sub_a) - 6, nchar(sub_a)) == substr(sub_b, nchar(sub_b) - 6, nchar(sub_b))
    res[idx][idx7] <- 85
    
    # 4-digit suffix (if not already set by 7)
    idx4 <- !idx7 & min_len >= 4 & substr(sub_a, nchar(sub_a) - 3, nchar(sub_a)) == substr(sub_b, nchar(sub_b) - 3, nchar(sub_b))
    res[idx][idx4] <- 65
  }
  res
}

v_age_similarity <- function(a, b, tolerance = 2) {
  d <- abs(a - b)
  res <- rep(0, length(a))
  res[is.na(d)] <- 0
  res[d <= tolerance] <- 100
  res[d > tolerance & d <= (tolerance + 3)] <- 70
  res[d > (tolerance + 3) & d <= (tolerance + 8)] <- 40
  res
}

confidence_from_score <- function(score, high_threshold = 90, medium_threshold = 75) {
  if (is.na(score)) return("low")
  if (score >= high_threshold) return("high")
  if (score >= medium_threshold) return("medium")
  "low"
}

# Blocking Logic
build_block_keys <- function(df) {
  dt <- data.table::as.data.table(df)
  res <- list()
  
  # Priority 1: Exact National ID (Highest precision blocking)
  idx_id <- nzchar(dt$hoh_ID_number_n) & !is_generic_or_invalid_id(dt$hoh_ID_number_n)
  if (any(idx_id)) {
    res[[length(res)+1]] <- data.table::data.table(
      row_id = dt$row_id[idx_id],
      block_key = paste0("id|", dt$hoh_ID_number_n[idx_id]),
      priority = 1L
    )
  }
  
  # Priority 2: Exact Phone Number
  idx_p <- nzchar(dt$phone_number_n) & !is_generic_or_invalid_phone(dt$phone_number_n)
  if (any(idx_p)) {
    res[[length(res)+1]] <- data.table::data.table(
      row_id = dt$row_id[idx_p],
      block_key = paste0("ph|", dt$phone_number_n[idx_p]),
      priority = 2L
    )
    # Priority 3: Phone last 7 digits
    p7_idx <- idx_p & nchar(dt$phone_number_n) >= 7
    if (any(p7_idx)) {
      res[[length(res)+1]] <- data.table::data.table(
        row_id = dt$row_id[p7_idx],
        block_key = paste0("p7|", substr(dt$phone_number_n[p7_idx], nchar(dt$phone_number_n[p7_idx]) - 6, nchar(dt$phone_number_n[p7_idx]))),
        priority = 3L
      )
    }
  }

  # Secondary Phone Number (if present)
  if ("secondary_phone_number_n" %in% names(dt)) {
    idx_sp <- nzchar(dt$secondary_phone_number_n) & !is_generic_or_invalid_phone(dt$secondary_phone_number_n)
    if (any(idx_sp)) {
      res[[length(res)+1]] <- data.table::data.table(
        row_id = dt$row_id[idx_sp],
        block_key = paste0("ph|", dt$secondary_phone_number_n[idx_sp]),
        priority = 2L
      )
    }
  }
  
  # Priority 4: District + Name first 3
  idx_d <- nzchar(dt$district_n) & nzchar(dt$hoh_arabic_name_n) & nchar(dt$hoh_arabic_name_n) >= 3
  if (any(idx_d)) {
    res[[length(res)+1]] <- data.table::data.table(
      row_id = dt$row_id[idx_d],
      block_key = paste0("dn3|", dt$district_n[idx_d], "|", substr(dt$hoh_arabic_name_n[idx_d], 1, 3)),
      priority = 4L
    )
  }
  
  # Priority 5: Governorate + Name first 4
  idx_gn <- nzchar(dt$governorate_n) & nzchar(dt$hoh_arabic_name_n) & nchar(dt$hoh_arabic_name_n) >= 4
  if (any(idx_gn)) {
    res[[length(res)+1]] <- data.table::data.table(
      row_id = dt$row_id[idx_gn],
      block_key = paste0("gn4|", dt$governorate_n[idx_gn], "|", substr(dt$hoh_arabic_name_n[idx_gn], 1, 4)),
      priority = 5L
    )
  }
  
  # Priority 6: Subdistrict + Name first 3 (if subdistrict available)
  idx_sd <- nzchar(dt$subdistrict_n) & nzchar(dt$hoh_arabic_name_n) & nchar(dt$hoh_arabic_name_n) >= 3
  if (any(idx_sd)) {
    res[[length(res)+1]] <- data.table::data.table(
      row_id = dt$row_id[idx_sd],
      block_key = paste0("sdn|", dt$subdistrict_n[idx_sd], "|", substr(dt$hoh_arabic_name_n[idx_sd], 1, 3)),
      priority = 4L
    )
  }
  
  if (length(res) == 0) return(data.table::data.table(row_id = integer(0), block_key = character(0), priority = integer(0)))
  
  data.table::rbindlist(res)[!is.na(block_key) & nzchar(block_key)]
}

build_cross_candidates <- function(u_dt, m_dt, limit = 500L, max_per_record = NULL) {
  u_keys <- build_block_keys(u_dt)
  m_keys <- build_block_keys(m_dt)
  if (nrow(u_keys) == 0 || nrow(m_keys) == 0) return(data.table::data.table())
  
  cand <- merge(u_keys, m_keys, by = "block_key", allow.cartesian = TRUE)
  if (nrow(cand) == 0) return(data.table::data.table())

  if ("priority.x" %in% names(cand) && "priority.y" %in% names(cand)) {
    cand <- cand[, .(priority = min(priority.x, priority.y)), by = .(upload_row_id = row_id.x, master_row_id = row_id.y)]
    data.table::setorder(cand, upload_row_id, priority)
  } else {
    cand <- unique(cand[, .(upload_row_id = row_id.x, master_row_id = row_id.y)])
  }

  # Cap candidates PER upload record so every uploaded row gets fairly evaluated
  per_rec_cap <- if (!is.null(max_per_record) && max_per_record > 0) {
    as.integer(max_per_record)
  } else {
    max(25L, min(100L, as.integer(limit / 10L)))
  }

  cand[, cand_rank := 1:.N, by = upload_row_id]
  cand <- cand[cand_rank <= per_rec_cap, .(upload_row_id, master_row_id)]

  # Global safety limit to prevent memory exhaustion (up to 500k candidate pairs)
  global_limit <- 500000L
  if (nrow(cand) > global_limit) cand <- cand[1:global_limit]
  cand
}

build_self_candidates <- function(dt, limit = 500L, max_per_record = NULL) {
  keys <- build_block_keys(dt)
  if (nrow(keys) == 0) return(data.table::data.table())
  
  cand <- merge(keys, keys, by = "block_key", allow.cartesian = TRUE)
  cand <- cand[row_id.x < row_id.y]
  if (nrow(cand) == 0) return(data.table::data.table())

  if ("priority.x" %in% names(cand) && "priority.y" %in% names(cand)) {
    cand <- cand[, .(priority = min(priority.x, priority.y)), by = .(row_a = row_id.x, row_b = row_id.y)]
    data.table::setorder(cand, row_a, priority)
  } else {
    cand <- unique(cand[, .(row_a = row_id.x, row_b = row_id.y)])
  }

  per_rec_cap <- if (!is.null(max_per_record) && max_per_record > 0) {
    as.integer(max_per_record)
  } else {
    max(25L, min(100L, as.integer(limit / 10L)))
  }

  cand[, cand_rank := 1:.N, by = row_a]
  cand <- cand[cand_rank <= per_rec_cap, .(row_a, row_b)]

  global_limit <- 200000L
  if (nrow(cand) > global_limit) cand <- cand[1:global_limit]
  cand
}

mask_master_columns <- function(df, upload_partner = NULL, partner_org = NULL, user_role = NULL) {
  if (!is.data.frame(df) || nrow(df) == 0) return(df)
  sensitive <- c("master_hoh_ID_number", "master_primary_phone_number", "master_secondary_phone_number", "master_phone_number")
  
  mask_val <- function(x) {
    if (is.na(x) || !nzchar(x)) return("")
    n <- nchar(x)
    if (n <= 3) return(paste0(rep("*", n), collapse = ""))
    paste0(paste0(rep("*", n - 3), collapse = ""), substr(x, n - 2, n))
  }
  
  is_ccy_master <- !is.null(user_role) && identical(tolower(trimws(user_role)), "ccy_master")
  
  n_rows <- nrow(df)
  u_partners <- if (!is.null(upload_partner) && length(upload_partner) == n_rows) {
    as.character(upload_partner)
  } else if ("upload_partner" %in% names(df)) {
    as.character(df$upload_partner)
  } else {
    rep(if (!is.null(partner_org)) as.character(partner_org) else "", n_rows)
  }
  
  m_orgs <- if ("master_organization" %in% names(df)) as.character(df$master_organization) else rep("", n_rows)
  
  clean_u <- tolower(trimws(u_partners))
  clean_m <- tolower(trimws(m_orgs))
  
  # When partners match: both non-empty and identical
  is_same_partner <- nzchar(clean_u) & nzchar(clean_m) & (clean_u == clean_m)
  
  # Fallback to partner_org if u_partner is empty
  if (!is.null(partner_org) && isTRUE(nzchar(partner_org))) {
    fallback_clean <- tolower(trimws(partner_org))
    fallback_match <- !nzchar(clean_u) & nzchar(clean_m) & (clean_m == fallback_clean)
    is_same_partner <- is_same_partner | fallback_match
  }
  
  # When logged in as Consortium Lead (ccy_master), bypass PII masking across all records.
  # Otherwise: if partners differ (!is_same_partner), mask PII.
  should_mask <- if (is_ccy_master) rep(FALSE, n_rows) else !is_same_partner
  
  for (col in sensitive) {
    if (col %in% names(df)) {
      raw_vals <- as.character(df[[col]])
      masked_vals <- vapply(raw_vals, mask_val, character(1))
      df[[col]] <- ifelse(should_mask, masked_vals, raw_vals)
    }
  }
  df
}

run_dedup <- function(upload_df,
                      master_df,
                      upload_filename = "uploaded_file.xlsx",
                      upload_time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                      fuzzy_high_threshold = 90,
                      fuzzy_medium_threshold = 75,
                      weights = config$weights,
                      match_fields = c(
                        "partner",
                        "hoh_ID_number",
                        "phone_number",
                        "hoh_arabic_name",
                        "hoh_spouse_name",
                        "geography"
                      ),
                      max_candidates = 500L,
                      filter_recent_mpca = FALSE,
                      mpca_window_months = 6,
                      mpca_reference_date = Sys.Date(),
                      partner_org = NULL,
                      user_role = NULL,
                      ...) {
  
  # MPCA Assistance Date Filtering (< 6 months window)
  if (isTRUE(filter_recent_mpca) && !is.null(master_df) && nrow(master_df) > 0) {
    date_col <- NULL
    for (candidate_name in c("dist_date_calc_new", "Dist_Date_Calc_New", "DIST_DATE_CALC_NEW", "Dist_Date", "dist_date", "system_date")) {
      if (candidate_name %in% names(master_df)) {
        date_col <- candidate_name
        break
      }
    }
    
    if (!is.null(date_col)) {
      raw_dates <- master_df[[date_col]]
      parsed_dates <- parse_flexible_date(raw_dates)
      ref_date <- if (!is.null(mpca_reference_date)) as.Date(mpca_reference_date) else Sys.Date()
      cutoff_date <- ref_date - round(as.numeric(mpca_window_months) * 30.4375)
      
      # Keep records whose last MPCA distribution date is >= cutoff_date (i.e. received assistance within the last N months)
      is_recent <- !is.na(parsed_dates) & (parsed_dates >= cutoff_date)
      master_df <- master_df[is_recent, , drop = FALSE]
    }
  }

  # 1. Prepare Data
  u_prep <- data.table::as.data.table(prepare_frame(upload_df))
  m_prep <- data.table::as.data.table(prepare_frame(master_df))

  use_partner <- "partner" %in% match_fields
  use_id <- "hoh_ID_number" %in% match_fields
  use_phone <- "phone_number" %in% match_fields
  use_name <- "hoh_arabic_name" %in% match_fields
  use_spouse <- "hoh_spouse_name" %in% match_fields
  use_geo <- "geography" %in% match_fields

  # Disable non-selected features at source so blocking/exact/fuzzy logic all honor Step 2 selection.
  if (!use_partner) {
    u_prep[, partner_n := ""]
    m_prep[, partner_n := ""]
  }
  if (!use_id) {
    u_prep[, hoh_ID_number_n := ""]
    m_prep[, hoh_ID_number_n := ""]
  }
  if (!use_phone) {
    u_prep[, `:=`(phone_number_n = "", secondary_phone_number_n = "")]
    m_prep[, `:=`(phone_number_n = "", secondary_phone_number_n = "")]
  }
  if (!use_name) {
    u_prep[, hoh_arabic_name_n := ""]
    m_prep[, hoh_arabic_name_n := ""]
  }
  if (!use_spouse) {
    u_prep[, hoh_spouse_name_n := ""]
    m_prep[, hoh_spouse_name_n := ""]
  }
  if (!use_geo) {
    u_prep[, `:=`(governorate_n = "", district_n = "", subdistrict_n = "", village_n = "")]
    m_prep[, `:=`(governorate_n = "", district_n = "", subdistrict_n = "", village_n = "")]
  }
  
  u_prep[, row_id := .I]
  m_prep[, row_id := .I]
  
  # Remove internally mapped standard column names from original columns so they don't get duplicated in export
  std_cols <- c("partner", "hoh_ID_number", "phone_number", "secondary_phone_number", 
                "hoh_arabic_name", "hoh_spouse_name", "governorate", "district", "subdistrict", 
                "village", "sex", "age", "row_id")
  orig_cols <- names(upload_df)
  
  # 2. Internal Matching (Same List)
  same_cand <- build_self_candidates(u_prep, limit = max_candidates)
  out_sl <- data.table::data.table(match_score = numeric(0))
  
  if (nrow(same_cand) > 0) {
    # Join with features
    same_cand <- merge(same_cand, u_prep, by.x = "row_a", by.y = "row_id")
    same_cand <- merge(same_cand, u_prep, by.x = "row_b", by.y = "row_id", suffixes = c("_a", "_b"))
    
    # Exact Checks
    same_cand[, exact_id := use_id & nzchar(hoh_ID_number_n_a) & !is_generic_or_invalid_id(hoh_ID_number_n_a) & hoh_ID_number_n_a == hoh_ID_number_n_b]
    same_cand[, exact_phone := use_phone & nzchar(phone_number_n_a) & !is_generic_or_invalid_phone(phone_number_n_a) & phone_number_n_a == phone_number_n_b]
    same_cand[, exact_name_gov := use_name & use_geo & nzchar(hoh_arabic_name_n_a) & hoh_arabic_name_n_a == hoh_arabic_name_n_b &
                                  nzchar(governorate_n_a) & governorate_n_a == governorate_n_b]
    
    same_cand[, is_exact := exact_id | exact_phone | exact_name_gov]
    
    # Fuzzy Scoring for non-exact or all
    same_cand[, name_score := if (use_name) v_name_similarity(hoh_arabic_name_n_a, hoh_arabic_name_n_b) else 0]
    same_cand[, spouse_score := if (use_spouse) v_name_similarity(hoh_spouse_name_n_a, hoh_spouse_name_n_b) else 0]
    same_cand[, phone_score := if (use_phone) {
      sc <- v_phone_similarity(phone_number_n_a, phone_number_n_b)
      ifelse(is_generic_or_invalid_phone(phone_number_n_a) | is_generic_or_invalid_phone(phone_number_n_b), 0, sc)
    } else 0]
    # Household ID score (binary exact match only)
    same_cand[, id_score := if (use_id) {
      ifelse(nzchar(hoh_ID_number_n_a) & !is_generic_or_invalid_id(hoh_ID_number_n_a) & hoh_ID_number_n_a == hoh_ID_number_n_b, 100, 0)
    } else 0]
    # Age-based scoring removed per configuration — no contribution from age
    same_cand[, geo_score := 0]
    if (use_geo) {
      same_cand[governorate_n_a == governorate_n_b & nzchar(governorate_n_a), geo_score := geo_score + 35]
      same_cand[district_n_a == district_n_b & nzchar(district_n_a), geo_score := geo_score + 30]
      same_cand[subdistrict_n_a == subdistrict_n_b & nzchar(subdistrict_n_a), geo_score := geo_score + 20]
      same_cand[village_n_a == village_n_b & nzchar(village_n_a), geo_score := geo_score + 15]
    }
    
    same_cand[, sex_score := ifelse(sex_n_a == sex_n_b & nzchar(sex_n_a), 100, 0)]
    
    same_cand[, match_score := round(
      name_score * weights$hoh_arabic_name +
      spouse_score * weights$hoh_spouse_name +
      phone_score * weights$phone_number +
      id_score * (if (is.null(weights$hoh_ID_number)) 0 else weights$hoh_ID_number) +
      geo_score * weights$geography +
      sex_score * weights$sex, 1)]
    same_cand[exact_id == TRUE | exact_name_gov == TRUE, match_score := 100]
    
    # Filter
    same_results <- same_cand[match_score >= fuzzy_medium_threshold]
    
    # Format results
    if (nrow(same_results) > 0) {
      same_results[, confidence := ifelse(match_score >= fuzzy_high_threshold, "high", "medium")]
      # match_type consolidated into confidence only; exact/fuzzy removed
      same_results[, match_pair_id := exact_pair_id("SL", row_a, row_b), by = 1:nrow(same_results)]
      
      # Select and rename for output
      out_sl <- same_results[, .(
        match_pair_id, match_score, confidence,
        contributing_factors = paste0("name=", name_score, " | phone=", phone_score, " | id=", id_score, " | geo=", geo_score),
        upload_row_id_a = row_a,
        upload_row_id_b = row_b
      )]
      # Add original columns
      for (col in orig_cols) {
        out_sl[[paste0("upload_", col, "_a")]] <- same_results[[paste0(col, "_a")]]
        out_sl[[paste0("upload_", col, "_b")]] <- same_results[[paste0(col, "_b")]]
      }
    } else {
      out_sl <- data.table::data.table()
    }
  } else {
    out_sl <- data.table::data.table()
  }
  
  # 3. External Matching (List vs Master)
  cross_cand <- build_cross_candidates(u_prep, m_prep, limit = max_candidates)
  out_lm <- data.table::data.table(match_score = numeric(0))
  
  if (nrow(cross_cand) > 0) {
    cross_cand <- merge(cross_cand, u_prep, by.x = "upload_row_id", by.y = "row_id")
    cross_cand <- merge(cross_cand, m_prep, by.x = "master_row_id", by.y = "row_id", suffixes = c("_u", "_m"))
    
    cross_cand[, exact_id := use_id & nzchar(hoh_ID_number_n_u) & !is_generic_or_invalid_id(hoh_ID_number_n_u) & hoh_ID_number_n_u == hoh_ID_number_n_m]
    cross_cand[, exact_phone := use_phone & nzchar(phone_number_n_u) & !is_generic_or_invalid_phone(phone_number_n_u) & phone_number_n_u == phone_number_n_m]
    cross_cand[, is_exact := exact_id | exact_phone]
    
    cross_cand[, name_score := if (use_name) v_name_similarity(hoh_arabic_name_n_u, hoh_arabic_name_n_m) else 0]
    cross_cand[, spouse_score := if (use_spouse) v_name_similarity(hoh_spouse_name_n_u, hoh_spouse_name_n_m) else 0]
    cross_cand[, phone_score := if (use_phone) {
      sc <- v_phone_similarity(phone_number_n_u, phone_number_n_m)
      ifelse(is_generic_or_invalid_phone(phone_number_n_u) | is_generic_or_invalid_phone(phone_number_n_m), 0, sc)
    } else 0]
    # Household ID score (binary exact match only)
    cross_cand[, id_score := if (use_id) {
      ifelse(nzchar(hoh_ID_number_n_u) & !is_generic_or_invalid_id(hoh_ID_number_n_u) & hoh_ID_number_n_u == hoh_ID_number_n_m, 100, 0)
    } else 0]
    # Age-based scoring removed per configuration — do not compute age_score
    cross_cand[, geo_score := 0]
    if (use_geo) {
      cross_cand[governorate_n_u == governorate_n_m & nzchar(governorate_n_u), geo_score := geo_score + 35]
      cross_cand[district_n_u == district_n_m & nzchar(district_n_u), geo_score := geo_score + 30]
      cross_cand[subdistrict_n_u == subdistrict_n_m & nzchar(subdistrict_n_u), geo_score := geo_score + 20]
      cross_cand[village_n_u == village_n_m & nzchar(village_n_u), geo_score := geo_score + 15]
    }
    
    cross_cand[, sex_score := ifelse(sex_n_u == sex_n_m & nzchar(sex_n_u), 100, 0)]
    
    cross_cand[, match_score := round(
      name_score * weights$hoh_arabic_name +
      spouse_score * weights$hoh_spouse_name +
      phone_score * weights$phone_number +
      id_score * (if (is.null(weights$hoh_ID_number)) 0 else weights$hoh_ID_number) +
      geo_score * weights$geography +
      sex_score * weights$sex, 1)]
    cross_cand[exact_id == TRUE, match_score := 100]
    
    ext_results <- cross_cand[match_score >= fuzzy_medium_threshold]
    
    if (nrow(ext_results) > 0) {
      ext_results[, confidence := ifelse(match_score >= fuzzy_high_threshold, "high", "medium")]
      # match_type consolidated into confidence only; exact/fuzzy removed
      ext_results[, match_pair_id := exact_pair_id("LM", upload_row_id, master_row_id), by = 1:nrow(ext_results)]
      
      out_lm <- ext_results[, .(
        match_pair_id, match_score, confidence,
        contributing_factors = paste0("name=", name_score, " | phone=", phone_score, " | id=", id_score, " | geo=", geo_score),
        upload_row_id, master_row_id,
        master_organization = partner_m,
        master_governorate = governorate_m,
        master_district = district_m,
        master_sub_district = subdistrict_m,
        master_village = village_m,
        master_hoh_arabic_name = hoh_arabic_name_m,
        master_hoh_age = age_m,
        master_hoh_sex = sex_m,
        master_hoh_ID_number = hoh_ID_number_m,
        master_primary_phone_number = phone_number_m,
        master_secondary_phone_number = secondary_phone_number_m,
        master_dist_date_calc_new = dist_date_calc_new_m
      )]
      # Retrieve Batch Code and QA Code from Master
      master_matched <- master_df[ext_results$master_row_id, , drop = FALSE]
      m_batch_col <- NULL
      for (cand in c("Main Form Partner Batch Code", "main_form_partner_batch_code", "Batch Code", "Batch_ID", "batch_code")) {
        if (cand %in% names(master_matched)) {
          m_batch_col <- cand
          break
        }
      }
      out_lm[["master_Main Form Partner Batch Code"]] <- if (!is.null(m_batch_col)) safe_char(master_matched[[m_batch_col]]) else NA_character_

      m_qa_col <- NULL
      for (cand in c("QA_CODE_SN", "QA_Code_SN", "qa_code_sn", "QA_Code", "qa_code")) {
        if (cand %in% names(master_matched)) {
          m_qa_col <- cand
          break
        }
      }
      out_lm[["master_QA_CODE_SN"]] <- if (!is.null(m_qa_col)) safe_char(master_matched[[m_qa_col]]) else NA_character_

      upload_matched <- upload_df[ext_results$upload_row_id, , drop = FALSE]
      for (col in names(upload_matched)) {
        out_lm[[paste0("upload_", col)]] <- upload_matched[[col]]
      }


      u_partner_col <- NULL
      for (cand in c("partner", "1.1. Organization Prefix", "1.1. Organization_text", "1.1. Organization", "Organization", "Partner")) {
        if (cand %in% names(upload_matched)) {
          u_partner_col <- as.character(upload_matched[[cand]])
          break
        }
      }
      if (is.null(u_partner_col) && "partner_u" %in% names(ext_results)) {
        u_partner_col <- as.character(ext_results$partner_u)
      }

      out_lm <- mask_master_columns(
        as.data.frame(out_lm),
        upload_partner = u_partner_col,
        partner_org = partner_org,
        user_role = user_role
      )
    } else {
      out_lm <- data.table::data.table()
    }
  } else {
    out_lm <- data.table::data.table()
  }
  
  # Final Summaries
  info <- data.frame(
    item = c("filename", "upload_time", "upload_count", "master_count", "mpca_date_filter_active", "mpca_window_months", "generated_at"),
    value = c(
      upload_filename,
      upload_time,
      nrow(upload_df),
      nrow(master_df),
      as.character(isTRUE(filter_recent_mpca)),
      as.character(mpca_window_months),
      format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
  )
  
  subset_by_match_type <- function(df, match_type_value) {
    # Backwards-compatible subset: map 'exact' -> confidence == 'high', 'fuzzy' -> confidence == 'medium'
    x <- as.data.frame(df)
    if (nrow(x) == 0) return(x[0, , drop = FALSE])
    if ("confidence" %in% names(x)) {
      if (match_type_value == "exact") return(x[x$confidence == "high", , drop = FALSE])
      if (match_type_value == "fuzzy") return(x[x$confidence == "medium", , drop = FALSE])
    }
    # Fallback: if original match_type field exists, use it
    if (!"match_type" %in% names(x)) return(x[0, , drop = FALSE])
    x[x$match_type == match_type_value, , drop = FALSE]
  }

  sl_exact <- subset_by_match_type(out_sl, "exact")
  sl_fuzzy <- subset_by_match_type(out_sl, "fuzzy")
  lm_exact <- subset_by_match_type(out_lm, "exact")
  lm_fuzzy <- subset_by_match_type(out_lm, "fuzzy")
  
  summary_df <- summarize_results(
    upload_n = nrow(upload_df),
    master_n = nrow(master_df),
    same_exact = sl_exact,
    same_fuzzy = sl_fuzzy,
    ext_exact = lm_exact,
    ext_fuzzy = lm_fuzzy,
    fuzzy_threshold = fuzzy_high_threshold,
    possible_threshold = fuzzy_medium_threshold
  )
  
  list(
    info = info,
    summary = summary_df,
    same_list_exact = sl_exact,
    same_list_fuzzy = sl_fuzzy,
    list_vs_master_exact = lm_exact,
    list_vs_master_fuzzy = lm_fuzzy
  )
}

summarize_results <- function(upload_n, master_n, same_exact, same_fuzzy, ext_exact, ext_fuzzy, fuzzy_threshold, possible_threshold) {
  df <- data.frame(
    metric = c(
      "Total Records Examined",
      "Same List Exact",
      "Same List Fuzzy",
      "List vs Master Exact",
      "List vs Master Fuzzy",
      "Fuzzy High Threshold",
      "Fuzzy Medium Threshold",
      "Master Record Count"
    ),
    value = c(
      upload_n,
      nrow(same_exact),
      nrow(same_fuzzy),
      nrow(ext_exact),
      nrow(ext_fuzzy),
      fuzzy_threshold,
      possible_threshold,
      master_n
    ),
    stringsAsFactors = FALSE
  )
  
  pct <- c(
    NA,
    round(100 * nrow(same_exact) / max(1, upload_n), 2),
    round(100 * nrow(same_fuzzy) / max(1, upload_n), 2),
    round(100 * nrow(ext_exact) / max(1, upload_n), 2),
    round(100 * nrow(ext_fuzzy) / max(1, upload_n), 2),
    NA, NA, NA
  )
  df$percentage_of_upload <- pct
  df
}
