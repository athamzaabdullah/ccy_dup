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
  
  # Create individual block components
  res <- list()
  
  # gfn: gov + first name char
  idx <- nzchar(dt$governorate_n) & nzchar(dt$hoh_arabic_name_n)
  if (any(idx)) {
    res[[length(res)+1]] <- data.table::data.table(
      row_id = dt$row_id[idx],
      block_key = paste0("gfn|", dt$governorate_n[idx], "|", substr(dt$hoh_arabic_name_n[idx], 1, 1))
    )
  }
  
  # gn3: gov + name first 3
  if (any(idx)) {
    res[[length(res)+1]] <- data.table::data.table(
      row_id = dt$row_id[idx],
      block_key = paste0("gn3|", dt$governorate_n[idx], "|", substr(dt$hoh_arabic_name_n[idx], 1, 3))
    )
  }
  
  # dn3: dist + name first 3
  idx_d <- nzchar(dt$district_n) & nzchar(dt$hoh_arabic_name_n)
  if (any(idx_d)) {
    res[[length(res)+1]] <- data.table::data.table(
      row_id = dt$row_id[idx_d],
      block_key = paste0("dn3|", dt$district_n[idx_d], "|", substr(dt$hoh_arabic_name_n[idx_d], 1, 3))
    )
  }
  
  # ps4: phone last 4
  idx_p <- nzchar(dt$phone_number_n)
  if (any(idx_p)) {
    res[[length(res)+1]] <- data.table::data.table(
      row_id = dt$row_id[idx_p],
      block_key = paste0("ps4|", substr(dt$phone_number_n[idx_p], pmax(1, nchar(dt$phone_number_n[idx_p]) - 3), nchar(dt$phone_number_n[idx_p])))
    )
  }
  
  if (length(res) == 0) return(data.table::data.table(row_id = integer(0), block_key = character(0)))
  
  data.table::rbindlist(res)[!is.na(block_key) & nzchar(block_key)]
}

build_cross_candidates <- function(u_dt, m_dt, limit = 500000L) {
  u_keys <- build_block_keys(u_dt)
  m_keys <- build_block_keys(m_dt)
  if (nrow(u_keys) == 0 || nrow(m_keys) == 0) return(data.table::data.table())
  
  cand <- merge(u_keys, m_keys, by = "block_key", allow.cartesian = TRUE)
  cand <- unique(cand[, .(upload_row_id = row_id.x, master_row_id = row_id.y)])
  if (nrow(cand) > limit) cand <- cand[1:limit]
  cand
}

build_self_candidates <- function(dt, limit = 500000L) {
  keys <- build_block_keys(dt)
  if (nrow(keys) == 0) return(data.table::data.table())
  
  cand <- merge(keys, keys, by = "block_key", allow.cartesian = TRUE)
  cand <- cand[row_id.x < row_id.y]
  cand <- unique(cand[, .(row_a = row_id.x, row_b = row_id.y)])
  if (nrow(cand) > limit) cand <- cand[1:limit]
  cand
}

mask_master_columns <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) return(df)
  sensitive <- c("master_hoh_ID_number", "master_primary_phone_number", "master_secondary_phone_number")
  
  mask_val <- function(x) {
    if (is.na(x) || !nzchar(x)) return("")
    n <- nchar(x)
    if (n <= 3) return(paste0(rep("*", n), collapse = ""))
    paste0(paste0(rep("*", n - 3), collapse = ""), substr(x, n - 2, n))
  }
  
  for (col in sensitive) {
    if (col %in% names(df)) {
      df[[paste0(col, "_masked")]] <- vapply(df[[col]], mask_val, character(1))
      df[[col]] <- NULL
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
                      max_candidates = 500000L) {
  
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
  
  orig_cols <- names(upload_df)
  
  # 2. Internal Matching (Same List)
  same_cand <- build_self_candidates(u_prep, limit = max_candidates)
  out_sl <- data.table::data.table(match_score = numeric(0))
  
  if (nrow(same_cand) > 0) {
    # Join with features
    same_cand <- merge(same_cand, u_prep, by.x = "row_a", by.y = "row_id")
    same_cand <- merge(same_cand, u_prep, by.x = "row_b", by.y = "row_id", suffixes = c("_a", "_b"))
    
    # Exact Checks
    same_cand[, exact_id := use_id & nzchar(hoh_ID_number_n_a) & hoh_ID_number_n_a == hoh_ID_number_n_b]
    same_cand[, exact_phone := use_phone & nzchar(phone_number_n_a) & phone_number_n_a == phone_number_n_b]
    same_cand[, exact_name_gov := use_name & use_geo & nzchar(hoh_arabic_name_n_a) & hoh_arabic_name_n_a == hoh_arabic_name_n_b &
                                  nzchar(governorate_n_a) & governorate_n_a == governorate_n_b]
    
    same_cand[, is_exact := exact_id | exact_phone | exact_name_gov]
    
    # Fuzzy Scoring for non-exact or all
    same_cand[, name_score := if (use_name) v_name_similarity(hoh_arabic_name_n_a, hoh_arabic_name_n_b) else 0]
    same_cand[, spouse_score := if (use_spouse) v_name_similarity(hoh_spouse_name_n_a, hoh_spouse_name_n_b) else 0]
    same_cand[, phone_score := if (use_phone) v_phone_similarity(phone_number_n_a, phone_number_n_b) else 0]
    # Household ID score (binary exact match only)
    same_cand[, id_score := if (use_id) {
      ifelse(nzchar(hoh_ID_number_n_a) & hoh_ID_number_n_a == hoh_ID_number_n_b, 100, 0)
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
    
    same_cand[is_exact == TRUE, match_score := 100]
    
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
    
    cross_cand[, exact_id := use_id & nzchar(hoh_ID_number_n_u) & hoh_ID_number_n_u == hoh_ID_number_n_m]
    cross_cand[, exact_phone := use_phone & nzchar(phone_number_n_u) & phone_number_n_u == phone_number_n_m]
    cross_cand[, is_exact := exact_id | exact_phone]
    
    cross_cand[, name_score := if (use_name) v_name_similarity(hoh_arabic_name_n_u, hoh_arabic_name_n_m) else 0]
    cross_cand[, spouse_score := if (use_spouse) v_name_similarity(hoh_spouse_name_n_u, hoh_spouse_name_n_m) else 0]
    cross_cand[, phone_score := if (use_phone) v_phone_similarity(phone_number_n_u, phone_number_n_m) else 0]
    # Household ID score (binary exact match only)
    cross_cand[, id_score := if (use_id) {
      ifelse(nzchar(hoh_ID_number_n_u) & hoh_ID_number_n_u == hoh_ID_number_n_m, 100, 0)
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
    
    cross_cand[is_exact == TRUE, match_score := 100]
    
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
        master_secondary_phone_number = secondary_phone_number_m
      )]
      for (col in orig_cols) {
        out_lm[[paste0("upload_", col)]] <- ext_results[[paste0(col, "_u")]]
      }
      out_lm <- mask_master_columns(as.data.frame(out_lm))
    } else {
      out_lm <- data.table::data.table()
    }
  } else {
    out_lm <- data.table::data.table()
  }
  
  # Final Summaries
  info <- data.frame(
    item = c("filename", "upload_time", "upload_count", "master_count", "generated_at"),
    value = c(upload_filename, upload_time, nrow(upload_df), nrow(master_df), format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
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
