columns_to_remove <- c(
  "upload_hh_expend_food_a",
  "upload_hh_expend_food_b",
  "upload_hh_expend_wash_items_a",
  "upload_hh_expend_wash_items_b",
  "upload_hh_expend_transportation_a",
  "upload_hh_expend_transportation_b",
  "upload_hh_expend_communication_a",
  "upload_hh_expend_communication_b",
  "upload_hh_expend_cloths_a",
  "upload_hh_expend_cloths_b",
  "upload_hh_expend_healthcare_a",
  "upload_hh_expend_healthcare_b",
  "upload_hh_expend_paid_off_debt_a",
  "upload_hh_expend_paid_off_debt_b",
  "upload_hh_expend_savings_a",
  "upload_hh_expend_savings_b",
  "upload_hh_expend_shelter_material_a",
  "upload_hh_expend_shelter_material_b",
  "upload_hh_expend_household_items_a",
  "upload_hh_expend_household_items_b",
  "upload_hh_expend_education_a",
  "upload_hh_expend_education_b",
  "upload_hh_expend_livelihood_a",
  "upload_hh_expend_livelihood_b",
  "upload_cereals_a",
  "upload_cereals_b",
  "upload_pulses_a",
  "upload_pulses_b",
  "upload_Vegetables_a",
  "upload_Vegetables_b",
  "upload_Fruits_a",
  "upload_Fruits_b",
  "upload_Animal_a",
  "upload_Animal_b",
  "upload_milk_a",
  "upload_milk_b",
  "upload_sugar_a",
  "upload_sugar_b",
  "upload_oil_a",
  "upload_oil_b",
  "upload_condiments_a",
  "upload_condiments_b",
  "upload_check_hoh_age_a",
  "upload_check_hoh_age_b",
  "upload_female_plw_yn_a",
  "upload_female_plw_yn_b",
  "upload_hoh_medical_condition_a",
  "upload_hoh_medical_condition_b",
  "upload_hoh_disability_a",
  "upload_hoh_disability_b",
  "upload_hoh_id_name_match_a",
  "upload_hoh_id_name_match_b",
  "upload_family_count_a",
  "upload_family_count_b",
  "upload_fam_count_0_4_m_a",
  "upload_fam_count_0_4_m_b",
  "upload_fam_count_0_4_f_a",
  "upload_fam_count_0_4_f_b",
  "upload_fam_count_5_17_m_a",
  "upload_fam_count_5_17_m_b",
  "upload_fam_count_5_17_f_a",
  "upload_fam_count_5_17_f_b",
  "upload_fam_count_18_49_m_a",
  "upload_fam_count_18_49_m_b",
  "upload_fam_count_18_49_f_a",
  "upload_fam_count_18_49_f_b",
  "upload_fam_count_50_m_a",
  "upload_fam_count_50_m_b",
  "upload_fam_count_50_f_a",
  "upload_fam_count_50_f_b",
  "upload_family_count_non_hoh_a",
  "upload_family_count_non_hoh_b",
  "upload_stop_note_hh_number_a",
  "upload_stop_note_hh_number_b",
  "upload_fam_count_severe_med_m_a",
  "upload_fam_count_severe_med_m_b",
  "upload_fam_count_severe_med_f_a",
  "upload_fam_count_severe_med_f_b",
  "upload_fam_count_disability_m_a",
  "upload_fam_count_disability_m_b",
  "upload_fam_count_disability_f_a",
  "upload_fam_count_disability_f_b",
  "upload_children_not_school_time_a",
  "upload_children_not_school_time_b",
  "upload_gfd_received_a",
  "upload_gfd_received_b",
  "upload_sam_received_yn_a",
  "upload_sam_received_yn_b",
  "upload_sam_monthly_basis_yn_a",
  "upload_sam_monthly_basis_yn_b",
  "upload_rrm_received_a",
  "upload_rrm_received_b",
  "upload_rrm_received_date_a",
  "upload_rrm_received_date_b",
  "upload_hh_econ_earn_income1_yn_a",
  "upload_hh_econ_earn_income1_yn_b",
  "upload_hh_econ_earn_income_a",
  "upload_hh_econ_earn_income_b",
  "upload_hh_econ_in_debt_a",
  "upload_hh_econ_in_debt_b",
  "upload_hh_econ_in_debt_yes_a",
  "upload_hh_econ_in_debt_yes_b",
  "upload_shelter_type_a",
  "upload_shelter_type_b",
  "upload_share_house_with_another_hh_yn_a",
  "upload_share_house_with_another_hh_yn_b",
  "upload_share_housing_how_many_families_a",
  "upload_share_housing_how_many_families_b",
  "upload_main_water_source_a",
  "upload_main_water_source_b",
  "upload_main_electricity_source_a",
  "upload_main_electricity_source_b",
  "upload_main_energy_source_a",
  "upload_main_energy_source_b",
  "upload_latrine_access_a",
  "upload_latrine_access_b",
  "upload_hh_marginalized_community_yn_a",
  "upload_hh_marginalized_community_yn_b",
  "upload_hh_enough_items_a",
  "upload_hh_enough_items_b",
  "upload_SDC_a",
  "upload_SDC_b",
  "upload_FE_a",
  "upload_FE_b",
  "upload_Fire_T_F_a",
  "upload_Fire_T_F_b",
  "upload_dist_date_calc_a",
  "upload_dist_date_calc_b",
  "upload_dist_date_calc_new_a",
  "upload_dist_date_calc_new_b",
  "upload_Excl_reason_a",
  "upload_Excl_reason_b",
  "upload_status_a",
  "upload_status_b",
  "upload_interview_method_a",
  "upload_interview_method_b",
  "upload_type_of_shock_surveys_a",
  "upload_type_of_shock_surveys_b",
  "upload_flood_any_damage_a",
  "upload_flood_any_damage_b",
  "upload_Main Distribution Donor_a",
  "upload_Main Distribution Donor_b",
  "upload_Main Distribution Donor",
  "upload_partner",
  "upload_partner_a",
  "upload_partner_b"
)

drop_export_columns <- function(df) {
  cols <- names(df)
  drop_exact <- c(
    columns_to_remove,
    "upload_Main Distribution Donor_a",
    "upload_Main Distribution Donor_b",
    "upload_Main Distribution Donor",
    "upload_partner",
    "upload_partner_a",
    "upload_partner_b"
  )
  drop_pattern <- "^upload_(Main[ _]Distribution[ _]Donor(_[ab])?|partner(_[ab])?)$"
  keep <- cols[!cols %in% drop_exact & !grepl(drop_pattern, cols, ignore.case = TRUE)]
  df[, keep, drop = FALSE]
}

find_matching_master_column <- function(u_col, master_remaining) {
  if (length(master_remaining) == 0) return(NA_character_)
  base <- sub("^upload_", "", u_col)
  clean_base <- trimws(sub("^[0-9\\.\\s\\:]+", "", base))
  norm_base <- gsub("[^a-z0-9]", "", tolower(clean_base))
  
  # Organization / Partner
  if (grepl("organization|partner", norm_base, ignore.case = TRUE)) {
    if ("master_organization" %in% master_remaining) return("master_organization")
  }
  
  # Arabic Name (Head of Household)
  if (grepl("arabicname|hoharabicname|hhname|beneficiaryname", norm_base, ignore.case = TRUE) ||
      (grepl("name", norm_base, ignore.case = TRUE) && !grepl("spouse", norm_base, ignore.case = TRUE))) {
    if ("master_hoh_arabic_name" %in% master_remaining) return("master_hoh_arabic_name")
  }
  
  # Spouse Name
  if (grepl("spouse", norm_base, ignore.case = TRUE)) {
    if ("master_hoh_spouse_name" %in% master_remaining) return("master_hoh_spouse_name")
  }
  
  # National ID
  if (grepl("nationalid|idnumber|hohidnumber|nid|identity", norm_base, ignore.case = TRUE) ||
      norm_base %in% c("id", "nid", "idtype")) {
    if ("master_hoh_ID_number" %in% master_remaining) return("master_hoh_ID_number")
  }
  
  # Secondary Phone (check secondary first!)
  if (grepl("secondaryphone|secondphone|altphone", norm_base, ignore.case = TRUE)) {
    if ("master_secondary_phone_number" %in% master_remaining) return("master_secondary_phone_number")
  }
  
  # Primary Phone
  if (grepl("primaryphone|phone|mobile|contact|tel", norm_base, ignore.case = TRUE)) {
    if ("master_primary_phone_number" %in% master_remaining) return("master_primary_phone_number")
    if ("master_phone_number" %in% master_remaining) return("master_phone_number")
  }
  
  # Sub-District (check subdistrict before district!)
  if (grepl("subdistrict|subdist", norm_base, ignore.case = TRUE)) {
    if ("master_sub_district" %in% master_remaining) return("master_sub_district")
    if ("master_subdistrict" %in% master_remaining) return("master_subdistrict")
  }
  
  # District
  if (grepl("district", norm_base, ignore.case = TRUE)) {
    if ("master_district" %in% master_remaining) return("master_district")
  }
  
  # Governorate
  if (grepl("governorate", norm_base, ignore.case = TRUE)) {
    if ("master_governorate" %in% master_remaining) return("master_governorate")
  }
  
  # Village
  if (grepl("village", norm_base, ignore.case = TRUE)) {
    if ("master_village" %in% master_remaining) return("master_village")
  }
  
  # Sex / Gender
  if (grepl("sex|gender", norm_base, ignore.case = TRUE)) {
    if ("master_hoh_sex" %in% master_remaining) return("master_hoh_sex")
    if ("master_sex" %in% master_remaining) return("master_sex")
  }
  
  # Age (exact word match or hohage)
  if (norm_base %in% c("age", "hohage", "headofhhage", "hhage") || grepl("(^|[^a-z])age($|[^a-z])", clean_base, ignore.case = TRUE)) {
    if ("master_hoh_age" %in% master_remaining) return("master_hoh_age")
    if ("master_age" %in% master_remaining) return("master_age")
  }
  
  # Distribution Date / MPCA Date
  if (grepl("distdate|distributiondate|mpcadate", norm_base, ignore.case = TRUE)) {
    if ("master_dist_date_calc_new" %in% master_remaining) return("master_dist_date_calc_new")
  }
  
  # General fuzzy matching against remaining master column names
  norm_m <- gsub("[^a-z0-9]", "", tolower(sub("^master_", "", master_remaining)))
  hit <- which(norm_m == norm_base)
  if (length(hit) > 0) return(master_remaining[hit[1]])
  
  NA_character_
}

reorder_upload_master_columns <- function(df) {
  cols <- names(df)
  upload_cols <- grep("^upload_", cols, value = TRUE)
  master_cols <- grep("^master_", cols, value = TRUE)
  if (length(upload_cols) == 0 || length(master_cols) == 0) return(df)

  fixed_first <- c(
    "match_pair_id", "match_score", "confidence",
    "contributing_factors", "upload_row_id", "master_row_id"
  )
  fixed_first <- fixed_first[fixed_first %in% cols]

  master_remaining <- master_cols
  interleaved <- character(0)

  for (u in upload_cols) {
    interleaved <- c(interleaved, u)
    m <- find_matching_master_column(u, master_remaining)
    if (!is.na(m) && m %in% master_remaining) {
      interleaved <- c(interleaved, m)
      master_remaining <- setdiff(master_remaining, m)
    }
  }

  others <- setdiff(cols, c(fixed_first, upload_cols, master_cols))
  new_order <- c(fixed_first, interleaved, master_remaining, others)
  new_order <- unique(new_order[new_order %in% cols])
  df[, new_order, drop = FALSE]
}

normalize_export_table <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(data.frame(note = "No records found", stringsAsFactors = FALSE))
  }
  df <- drop_export_columns(df)
  df <- reorder_upload_master_columns(df)
  df
}

apply_sheet_format <- function(wb, sheet, df, highlight_score = TRUE) {
  openxlsx::freezePane(wb, sheet = sheet, firstRow = TRUE)
  openxlsx::showGridLines(wb, sheet = sheet, showGridLines = TRUE)

  if (nrow(df) > 0) {
    openxlsx::addFilter(wb, sheet = sheet, rows = 1, cols = seq_len(ncol(df)))
  }

  # CCY Humanitarian Header Style
  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    fontName = "Segoe UI",
    fontSize = 11,
    fontColour = "#FFFFFF",
    fgFill = "#1B5E20", # CCY Forest Green
    halign = "center",
    valign = "center",
    border = "TopBottomLeftRight",
    borderColour = "#14532D",
    wrapText = FALSE
  )
  openxlsx::addStyle(wb, sheet = sheet, style = header_style, rows = 1, cols = seq_len(ncol(df)), gridExpand = TRUE)
  
  # Data cell general style
  body_style <- openxlsx::createStyle(
    fontName = "Segoe UI",
    fontSize = 10,
    border = "TopBottomLeftRight",
    borderColour = "#E2E8F0"
  )
  if (nrow(df) > 0) {
    openxlsx::addStyle(wb, sheet = sheet, style = body_style, rows = 2:(nrow(df) + 1), cols = seq_len(ncol(df)), gridExpand = TRUE)
  }
  
  openxlsx::setColWidths(wb, sheet = sheet, cols = seq_len(ncol(df)), widths = "auto")

  if (!highlight_score || nrow(df) == 0) return(invisible(NULL))

  score_col <- which(names(df) %in% c("match_score", "Score (0-100)"))
  conf_col <- which(names(df) %in% c("confidence", "Confidence"))
  status_col <- which(names(df) %in% c("verification_status", "Verification Status"))

  if (length(score_col) == 1) {
    openxlsx::conditionalFormatting(
      wb,
      sheet = sheet,
      cols = score_col,
      rows = 2:(nrow(df) + 1),
      rule = ">=90",
      style = openxlsx::createStyle(fontColour = "#166534", fgFill = "#DCFCE7", textDecoration = "bold"),
      type = "expression"
    )
    openxlsx::conditionalFormatting(
      wb,
      sheet = sheet,
      cols = score_col,
      rows = 2:(nrow(df) + 1),
      rule = "AND($A1<>\"\",INDIRECT(ADDRESS(ROW(),COLUMN()))>=75,INDIRECT(ADDRESS(ROW(),COLUMN()))<90)",
      style = openxlsx::createStyle(fontColour = "#92400E", fgFill = "#FEF3C7", textDecoration = "bold"),
      type = "expression"
    )
  }

  if (length(conf_col) == 1) {
    openxlsx::conditionalFormatting(
      wb,
      sheet = sheet,
      cols = conf_col,
      rows = 2:(nrow(df) + 1),
      rule = "INDIRECT(ADDRESS(ROW(),COLUMN()))=\"high\"",
      style = openxlsx::createStyle(fontColour = "#166534", fgFill = "#DCFCE7", textDecoration = "bold"),
      type = "expression"
    )
  }

  if (length(status_col) == 1) {
    openxlsx::conditionalFormatting(
      wb,
      sheet = sheet,
      cols = status_col,
      rows = 2:(nrow(df) + 1),
      rule = "INDIRECT(ADDRESS(ROW(),COLUMN()))=\"Confirmed Duplicate\"",
      style = openxlsx::createStyle(fontColour = "#991B1B", fgFill = "#FEE2E2", textDecoration = "bold"),
      type = "expression"
    )
    openxlsx::conditionalFormatting(
      wb,
      sheet = sheet,
      cols = status_col,
      rows = 2:(nrow(df) + 1),
      rule = "INDIRECT(ADDRESS(ROW(),COLUMN()))=\"False Positive\"",
      style = openxlsx::createStyle(fontColour = "#166534", fgFill = "#DCFCE7", textDecoration = "bold"),
      type = "expression"
    )
  }
}

write_formatted_sheet <- function(wb, sheet_name, data, highlight_score = TRUE, tab_color = NULL) {
  df <- normalize_export_table(data)
  if (!is.null(tab_color) && isTRUE(nzchar(tab_color))) {
    openxlsx::addWorksheet(wb, sheet_name, tabColour = tab_color)
  } else {
    openxlsx::addWorksheet(wb, sheet_name)
  }
  openxlsx::writeData(wb, sheet_name, df, colNames = TRUE)
  apply_sheet_format(wb, sheet_name, df, highlight_score = highlight_score)
}

build_summary_sheet <- function(result) {
  summary_df <- normalize_export_table(result$summary)
  if (!"metric" %in% names(summary_df)) return(summary_df)

  total <- suppressWarnings(as.numeric(summary_df$value[summary_df$metric == "Total Records Examined"]))
  total <- ifelse(length(total) == 0 || all(is.na(total)), NA_real_, total[1])

  if (!is.na(total)) {
    row_metrics <- c("Same List Exact", "Same List Fuzzy", "List vs Master Exact", "List vs Master Fuzzy")
    summary_df$percent_of_total <- ""
    for (m in row_metrics) {
      idx <- which(summary_df$metric == m)
      if (length(idx) == 1) {
        v <- suppressWarnings(as.numeric(summary_df$value[[idx]]))
        if (!is.na(v) && total > 0) {
          summary_df$percent_of_total[[idx]] <- sprintf("%.2f%%", 100 * v / total)
        }
      }
    }
  }
  summary_df
}

write_dedup_workbook <- function(result, file, triage_decisions = NULL) {
  wb <- openxlsx::createWorkbook()

  info_df <- normalize_export_table(result$info)
  summary_df <- build_summary_sheet(result)

  write_formatted_sheet(wb, "Info", info_df, highlight_score = FALSE, tab_color = "#0F766E")
  write_formatted_sheet(wb, "Summary", summary_df, highlight_score = FALSE, tab_color = "#0F766E")

  attach_triage <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(data.frame())
    df <- data.table::copy(df)
    if (!is.null(triage_decisions) && is.list(triage_decisions) && length(triage_decisions) > 0 && "match_pair_id" %in% names(df)) {
      status_vec <- character(nrow(df))
      notes_vec <- character(nrow(df))
      reviewer_vec <- character(nrow(df))
      time_vec <- character(nrow(df))
      for (i in seq_len(nrow(df))) {
        pid <- as.character(df$match_pair_id[i])
        dec <- triage_decisions[[pid]]
        if (!is.null(dec) && is.list(dec)) {
          status_vec[i] <- dec$status %||% "Unreviewed"
          notes_vec[i] <- dec$notes %||% ""
          reviewer_vec[i] <- dec$reviewer %||% ""
          time_vec[i] <- if (!is.null(dec$timestamp)) as.character(dec$timestamp) else ""
        } else {
          status_vec[i] <- "Unreviewed"
          notes_vec[i] <- ""
          reviewer_vec[i] <- ""
          time_vec[i] <- ""
        }
      }
      df$verification_status <- status_vec
      df$verification_notes <- notes_vec
      df$verification_reviewer <- reviewer_vec
      df$verification_timestamp <- time_vec
    }
    df
  }

  get_sheet <- function(keys) {
    for (k in keys) {
      if (!is.null(result[[k]])) return(attach_triage(result[[k]]))
    }
    return(data.frame())
  }

  write_formatted_sheet(wb, "Same List (High Confidence)", get_sheet(c("same_list_high", "same_list_exact")), highlight_score = TRUE, tab_color = "#1B5E20")
  write_formatted_sheet(wb, "Same List (Medium Confidence)", get_sheet(c("same_list_medium", "same_list_fuzzy")), highlight_score = TRUE, tab_color = "#D97706")
  write_formatted_sheet(wb, "List vs ActivityInfo (High)", get_sheet(c("list_vs_master_high", "list_vs_master_exact")), highlight_score = TRUE, tab_color = "#1B5E20")
  write_formatted_sheet(wb, "List vs ActivityInfo (Medium)", get_sheet(c("list_vs_master_medium", "list_vs_master_fuzzy")), highlight_score = TRUE, tab_color = "#D97706")

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}
