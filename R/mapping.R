# Bilingual column label mapping for CCY humanitarian datasets
column_bilingual_dictionary <- list(
  partner = list(en = "Partner Organization", ar = "المنظمة الشريكة"),
  record_id = list(en = "Record ID", ar = "رقم السجل"),
  qa_code_sn = list(en = "QA Code / SN", ar = "رمز الجودة / الرقم التسلسلي"),
  system_date = list(en = "System Date", ar = "تاريخ الإدخال"),
  interviewer = list(en = "Interviewer Name", ar = "اسم الباحث الميداني"),
  main_ref = list(en = "Main Reference", ar = "المرجع الأساسي"),
  hoh_ID_number = list(en = "Head of Household ID", ar = "رقم الهوية الوطنية / البطاقة"),
  id_type = list(en = "ID Type", ar = "نوع الهوية"),
  phone_number = list(en = "Primary Phone", ar = "رقم الهاتف الأساسي"),
  secondary_phone_number = list(en = "Secondary Phone", ar = "رقم الهاتف الثانوي"),
  hoh_arabic_name = list(en = "Head of Household Name (Arabic)", ar = "اسم رب الأسرة (رباعي)"),
  marital_status = list(en = "Marital Status", ar = "الحالة الاجتماعية"),
  hoh_spouse_name = list(en = "Spouse Name", ar = "اسم الزوج / الزوجة"),
  age = list(en = "Age", ar = "العمر"),
  household_size = list(en = "Household Size", ar = "حجم الأسرة"),
  sex = list(en = "Sex / Gender", ar = "النوع الاجتماعي"),
  beneficiary_status = list(en = "Beneficiary Status", ar = "حالة المستفيد"),
  dist_type = list(en = "Distribution Type", ar = "نوع التوزيع"),
  dist_date_calc_new = list(en = "Last MPCA Distribution Date (Dist_Date_Calc_New)", ar = "تاريخ آخر توزيع مساعدات نقدية"),
  governorate = list(en = "Governorate", ar = "المحافظة"),
  district = list(en = "District", ar = "المديرية"),
  subdistrict = list(en = "Subdistrict", ar = "العزلة / الحي"),
  village = list(en = "Village / Neighborhood", ar = "القرية / المحلة")
)

get_field_bilingual_label <- function(col_name) {
  info <- column_bilingual_dictionary[[col_name]]
  if (is.null(info)) return(col_name)
  paste0(col_name, " (", info$ar, " - ", info$en, ")")
}

# Mapping Presets Storage & Retrieval
load_mapping_presets <- function() {
  path <- config$paths$mapping_presets
  if (is.null(path) || !file.exists(path)) return(list())
  tryCatch({
    txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "")
    if (!nzchar(trimws(txt))) return(list())
    res <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
    if (is.list(res)) res else list()
  }, error = function(e) list())
}

save_mapping_preset <- function(preset_name, mapping_list) {
  if (is.null(preset_name) || !nzchar(trimws(preset_name))) return(FALSE)
  presets <- load_mapping_presets()
  presets[[trimws(preset_name)]] <- mapping_list

  preset_file <- config$paths$mapping_presets
  dir_path <- dirname(preset_file)
  if (!dir.exists(dir_path)) dir.create(dir_path, recursive = TRUE)

  tryCatch({
    json_txt <- jsonlite::toJSON(presets, pretty = TRUE, auto_unbox = TRUE)
    writeLines(as.character(json_txt), preset_file, useBytes = TRUE)
    TRUE
  }, error = function(e) FALSE)
}

# ==============================================================================
# Field Role & Deduplication Architecture Metadata
# ==============================================================================

field_metadata_dictionary <- list(
  hoh_ID_number = list(
    role = "Primary Blocking Anchor",
    role_ar = "معيار الحجب الأساسي",
    role_type = "primary",
    icon = "🔑",
    description = "Exact national ID deduplication anchor."
  ),
  phone_number = list(
    role = "Primary Blocking Anchor",
    role_ar = "معيار الحجب الأساسي",
    role_type = "primary",
    icon = "📱",
    description = "Cleaned 9-digit mobile phone number blocking."
  ),
  hoh_arabic_name = list(
    role = "Fuzzy / Token Anchor",
    role_ar = "حجب ومطابقة تقريبية",
    role_type = "fuzzy",
    icon = "👤",
    description = "Normalized 4-part Arabic name (Jaro-Winkler & Levenshtein)."
  ),
  hoh_spouse_name = list(
    role = "Secondary Corroboration",
    role_ar = "مطابقة تأكيدية ثانوية",
    role_type = "secondary",
    icon = "👥",
    description = "Cross-spouse verification to resolve candidate ambiguity."
  ),
  governorate = list(
    role = "Spatial Blocking (Admin 1)",
    role_ar = "حجب مكاني (المحافظة)",
    role_type = "spatial",
    icon = "🗺️",
    description = "First administrative tier spatial blocking."
  ),
  district = list(
    role = "Spatial Blocking (Admin 2)",
    role_ar = "حجب مكاني (المديرية)",
    role_type = "spatial",
    icon = "📍",
    description = "Second administrative tier spatial blocking."
  ),
  subdistrict = list(
    role = "Spatial Blocking (Admin 3)",
    role_ar = "حجب مكاني (العزلة / الحي)",
    role_type = "spatial",
    icon = "🏘️",
    description = "Third administrative tier spatial blocking."
  ),
  village = list(
    role = "Spatial Hierarchy (Admin 4)",
    role_ar = "الموقع الجغرافي (القرية)",
    role_type = "spatial",
    icon = "🏡",
    description = "Fourth administrative tier (neighborhood / village)."
  ),
  partner = list(
    role = "Organization Scope",
    role_ar = "نطاق المنظمة",
    role_type = "meta",
    icon = "🏛️",
    description = "Partner code or prefix scope filter."
  ),
  dist_date_calc_new = list(
    role = "Recency Filter",
    role_ar = "تصفية حداثة التوزيع",
    role_type = "date",
    icon = "📅",
    description = "Last MPCA cash distribution timestamp."
  )
)

get_field_meta <- function(col_name) {
  if (!is.null(col_name) && col_name %in% names(field_metadata_dictionary)) {
    return(field_metadata_dictionary[[col_name]])
  }
  list(
    role = "Attribute",
    role_ar = "سمة بيانات",
    role_type = "default",
    icon = "📋",
    description = "Standard dataset attribute."
  )
}

# ==============================================================================
# CCY Humanitarian Standard Column Aliases & Smart Detection
# ==============================================================================

ccy_column_aliases <- list(
  hoh_arabic_name = c(
    "3.1. Head of household (HoH) Name (Arabic)",
    "3.1. Head of household (HoH) Name (Arabic):",
    "3.1. Head of HH Arabic Name",
    "Head of household (HoH) Name (Arabic)",
    "Head of HH Arabic Name",
    "hoh_arabic_name",
    "hoh_name",
    "Head of Household Name",
    "hoh_full_name",
    "اسم رب الاسرة",
    "اسم رب الأسرة",
    "اسم المستفيد"
  ),
  hoh_spouse_name = c(
    "3.3. Head of HH's Spouse Name",
    "3.3. Head of HH's Spouse Name:",
    "Head of HH's Spouse Name",
    "hoh_spouse_name",
    "spouse_name",
    "اسم الزوج",
    "اسم الزوجة",
    "اسم الزوج / الزوجة"
  ),
  id_type = c(
    "3.11 What is the main form of ID that the Head of Household uses?",
    "3.11. What is the main form of ID that the Head of Household uses?",
    "What is the main form of ID that the Head of Household uses?",
    "hoh_id_type",
    "id_type",
    "نوع الهوية"
  ),
  hoh_ID_number = c(
    "3.12 What is the Head of Household's ID number?",
    "3.12. What is the Head of Household's ID number?",
    "What is the Head of Household's ID number?",
    "hoh_id_number",
    "hoh_ID_number",
    "National ID Number",
    "National ID",
    "ID Number",
    "National ID Number:",
    "national_id",
    "id_number",
    "رقم الهوية",
    "رقم البطاقة الشخصية"
  ),
  phone_number = c(
    "2.1. Primary Phone Number:",
    "2.1. Primary Phone Number",
    "Primary Phone Number:",
    "Primary Phone Number",
    "primary_phone_number",
    "phone_number",
    "phone",
    "mobile_number",
    "رقم الهاتف",
    "رقم الجوال"
  ),
  secondary_phone_number = c(
    "2.2. Secondary Phone Number:",
    "2.2. Secondary Phone Number",
    "Secondary Phone Number:",
    "Secondary Phone Number",
    "secondary_phone_number",
    "رقم الهاتف الثانوي"
  ),
  governorate = c(
    "1.11. Governorate",
    "1.11. Governorate:",
    "1.11 Governorate",
    "Governorate Label",
    "governorate_label",
    "Governorate",
    "governorate",
    "المحافظة"
  ),
  district = c(
    "1.12. District",
    "1.12. District:",
    "1.12 District",
    "District Label",
    "district_label",
    "District",
    "district",
    "المديرية"
  ),
  subdistrict = c(
    "1.13. Sub-District",
    "1.13. Sub-District:",
    "1.13 Sub-District",
    "1.13. Sub-district",
    "1.13. Subdistrict",
    "Sub-District",
    "Subdistrict Label...78",
    "Subdistrict Label",
    "Subdistrict Label:",
    "subdistrict_label",
    "sub_district_label",
    "sub_district",
    "subdistrict",
    "Subdistrict",
    "العزلة",
    "الحي"
  ),
  village = c(
    "1.14. Village",
    "1.14. Village:",
    "Village",
    "village",
    "القرية",
    "المحلة"
  ),
  partner = c(
    "1.1. Organization Prefix",
    "1.1. Organization_text",
    "Partner Prefix",
    "partner_prefix",
    "Main Form Partner Batch Code",
    "Partner",
    "1.1. Organization",
    "Main Form Partner",
    "organization",
    "partner",
    "المنظمة",
    "الشريك"
  ),
  dist_date_calc_new = c(
    "Dist_Date_Calc_New",
    "dist_date_calc_new",
    "Dist_Date_Calc",
    "dist_date_calc",
    "Last MPCA Distribution Date",
    "Distribution Date",
    "تاريخ التوزيع"
  )
)

#' Detect best matching column in uploaded dataset
#'
#' @param req_col Canonical column name (e.g. "hoh_ID_number")
#' @param uploaded_cols Vector of column names from uploaded dataset
#' @return Best matching column name or NULL if no match
detect_best_column_match <- function(req_col, uploaded_cols) {
  if (is.null(uploaded_cols) || length(uploaded_cols) == 0) return(NULL)
  
  # 1. Exact string match
  if (req_col %in% uploaded_cols) return(req_col)
  
  # Helper to normalize headers for robust comparison
  clean_header <- function(x) {
    tolower(gsub("[^a-zA-Z0-9\u0621-\u064A]", "", x))
  }
  
  req_clean <- clean_header(req_col)
  up_clean <- clean_header(uploaded_cols)
  
  # 2. Normalized exact match
  exact_idx <- which(up_clean == req_clean)
  if (length(exact_idx) > 0) return(uploaded_cols[exact_idx[1]])
  
  # 3. Known CCY alias dictionary match
  aliases <- ccy_column_aliases[[req_col]]
  if (!is.null(aliases) && length(aliases) > 0) {
    # Check exact alias match in uploaded columns
    alias_exact <- intersect(aliases, uploaded_cols)
    if (length(alias_exact) > 0) return(alias_exact[1])
    
    # Check normalized alias match
    alias_clean <- clean_header(aliases)
    for (ac in alias_clean) {
      match_pos <- which(up_clean == ac)
      if (length(match_pos) > 0) return(uploaded_cols[match_pos[1]])
    }
  }
  
  # 4. Smart heuristic keyword match (prioritizing specific terms to avoid cross-matching)
  patterns <- list(
    hoh_ID_number = c("id_num", "national_id", "idnumber", "hoh_id"),
    phone_number = c("primary_phone", "phone_num", "phonenum", "mobile"),
    hoh_arabic_name = c("hoh_name", "arabic_name", "beneficiary_name", "head_name"),
    hoh_spouse_name = c("spouse_name", "spouse"),
    governorate = c("governorate", "gov_label", "gov"),
    district = c("^district", "district_label"),
    subdistrict = c("subdistrict", "sub_district", "sub_dist"),
    village = c("village", "village_label", "neighborhood"),
    partner = c("partner", "org_prefix", "organization")
  )
  
  if (req_col %in% names(patterns)) {
    for (pat in patterns[[req_col]]) {
      hits <- grep(pat, uploaded_cols, ignore.case = TRUE)
      # Ensure district doesn't accidentally match subdistrict
      if (req_col == "district" && length(hits) > 0) {
        sub_hits <- grep("sub", uploaded_cols[hits], ignore.case = TRUE)
        if (length(sub_hits) > 0) {
          hits <- hits[-sub_hits]
        }
      }
      # Ensure hoh_name doesn't match spouse_name
      if (req_col == "hoh_arabic_name" && length(hits) > 0) {
        sp_hits <- grep("spouse", uploaded_cols[hits], ignore.case = TRUE)
        if (length(sp_hits) > 0) {
          hits <- hits[-sp_hits]
        }
      }
      if (length(hits) > 0) return(uploaded_cols[hits[1]])
    }
  }
  
  NULL
}

#' Extract representative sample value from uploaded dataframe
#'
#' @param df Dataframe from upload
#' @param col_name Selected column name
#' @param max_len Maximum characters to show
#' @return String representation of sample value or NULL
get_sample_preview_value <- function(df, col_name, max_len = 35) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
  if (is.null(col_name) || !nzchar(trimws(col_name)) || !col_name %in% names(df)) return(NULL)
  
  vals <- df[[col_name]]
  # Filter out NA, NULL, and whitespace-only values
  clean_vals <- vals[!is.na(vals) & nzchar(trimws(as.character(vals)))]
  if (length(clean_vals) == 0) return("(all empty / NA)")
  
  first_val <- trimws(as.character(clean_vals[1]))
  if (nchar(first_val) > max_len) {
    first_val <- paste0(substr(first_val, 1, max_len - 3), "...")
  }
  first_val
}
