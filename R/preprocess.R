safe_char <- function(x) {
  normalize_missing <- function(v) {
    t <- tolower(trimws(v))
    t %in% c("na", "n/a", "null", "none", "nan")
  }

  if (is.list(x)) {
    out <- vapply(x, function(v) paste(v, collapse = " "), character(1))
    out[is.na(out) | normalize_missing(out)] <- ""
    return(out)
  }
  out <- as.character(x)
  out[is.na(out) | normalize_missing(out)] <- ""
  out
}

normalize_digits <- function(x) {
  x <- safe_char(x)
  x <- stringi::stri_replace_all_fixed(
    x,
    c("\u0660", "\u0661", "\u0662", "\u0663", "\u0664", "\u0665", "\u0666", "\u0667", "\u0668", "\u0669"),
    c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"),
    vectorize_all = FALSE
  )
  x <- stringi::stri_replace_all_fixed(
    x,
    c("\u06F0", "\u06F1", "\u06F2", "\u06F3", "\u06F4", "\u06F5", "\u06F6", "\u06F7", "\u06F8", "\u06F9"),
    c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"),
    vectorize_all = FALSE
  )
  x
}

normalize_arabic <- function(x) {
  x <- safe_char(x)
  x <- normalize_digits(x)
  x <- stringi::stri_enc_toutf8(x, is_unknown_8bit = TRUE, validate = TRUE)
  x <- stringi::stri_replace_all_regex(x, "[\\u064B-\\u065F\\u0670]", "")
  x <- stringi::stri_replace_all_fixed(
    x,
    c("\u0622", "\u0623", "\u0625", "\u0671"),
    c("\u0627", "\u0627", "\u0627", "\u0627"),
    vectorize_all = FALSE
  )
  x <- stringi::stri_replace_all_fixed(
    x,
    c("\u0649", "\u064A"),
    c("\u064A", "\u064A"),
    vectorize_all = FALSE
  )
  x <- stringi::stri_replace_all_fixed(x, "\u0629", "\u0647", vectorize_all = FALSE)
  x <- stringi::stri_replace_all_regex(x, "[^\\p{Arabic}\\p{Latin}\\p{Nd}\\s]", " ")
  x <- stringi::stri_replace_all_regex(x, "\\s+", " ")
  stringi::stri_trim_both(x)
}

normalize_text <- function(x) {
  x <- safe_char(x)
  x <- normalize_digits(x)
  x <- tolower(trimws(x))
  x <- stringi::stri_replace_all_regex(x, "\\s+", " ")
  x
}

normalize_phone <- function(x) {
  x <- normalize_digits(x)
  x <- stringi::stri_replace_all_regex(x, "[^0-9]", "")
  x
}

normalize_sex <- function(x) {
  x <- normalize_text(x)
  dplyr::case_when(
    x %in% c("m", "male", "man", "\u0630\u0643\u0631", "\u0630\u0643\u0631\u0020") ~ "m",
    x %in% c("f", "female", "woman", "\u0627\u0646\u062b\u0649", "\u0623\u0646\u062b\u0649") ~ "f",
    TRUE ~ ""
  )
}

normalize_marital_status <- function(x) {
  x <- normalize_text(x)
  dplyr::case_when(
    x %in% c("single", "never married", "\u0627\u0639\u0632\u0628") ~ "single",
    x %in% c("married", "\u0645\u062a\u0632\u0648\u062c") ~ "married",
    x %in% c("widowed", "\u0627\u0631\u0645\u0644", "\u0623\u0631\u0645\u0644") ~ "widowed",
    x %in% c("divorced", "\u0645\u0637\u0644\u0642", "\u0645\u0637\u0644\u0642\u0647") ~ "divorced",
    TRUE ~ x
  )
}

normalize_geo <- function(x) {
  x <- normalize_arabic(x)
  x <- tolower(x)
  x <- stringi::stri_replace_all_regex(x, "\\s+", " ")
  trimws(x)
}

normalize_partner <- function(x) {
  normalize_text(x)
}

normalize_age <- function(x) {
  x <- normalize_digits(x)
  x <- suppressWarnings(as.integer(x))
  ifelse(is.na(x), NA_integer_, x)
}

normalize_household_size <- function(x) {
  x <- normalize_digits(x)
  x <- suppressWarnings(as.integer(x))
  ifelse(is.na(x), NA_integer_, x)
}

parse_flexible_date <- function(x) {
  if (is.null(x) || length(x) == 0) return(as.Date(character(0)))
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  
  char_x <- trimws(as.character(x))
  char_x[char_x %in% c("", "NA", "N/A", "null", "NULL", "none", "None")] <- NA_character_
  out <- as.Date(rep(NA_character_, length(char_x)))
  
  # 1. Try numeric Excel serial date numbers (e.g. 44561)
  is_num <- !is.na(suppressWarnings(as.numeric(char_x)))
  if (any(is_num)) {
    num_vals <- suppressWarnings(as.numeric(char_x[is_num]))
    valid_excel <- num_vals > 20000 & num_vals < 70000
    if (any(valid_excel)) {
      out[is_num][valid_excel] <- as.Date(num_vals[valid_excel], origin = "1899-12-30")
    }
  }
  
  # 2. For remaining string dates, try common formats
  rem_idx <- which(is.na(out) & !is.na(char_x))
  if (length(rem_idx) > 0) {
    rem_str <- char_x[rem_idx]
    parsed <- suppressWarnings(as.Date(rem_str, format = "%Y-%m-%d"))
    na_p <- is.na(parsed)
    if (any(na_p)) parsed[na_p] <- suppressWarnings(as.Date(rem_str[na_p], format = "%d/%m/%Y"))
    na_p <- is.na(parsed)
    if (any(na_p)) parsed[na_p] <- suppressWarnings(as.Date(rem_str[na_p], format = "%m/%d/%Y"))
    na_p <- is.na(parsed)
    if (any(na_p)) parsed[na_p] <- suppressWarnings(as.Date(rem_str[na_p], format = "%d-%m-%Y"))
    na_p <- is.na(parsed)
    if (any(na_p)) parsed[na_p] <- suppressWarnings(as.Date(rem_str[na_p], format = "%Y/%m/%d"))
    na_p <- is.na(parsed)
    if (any(na_p)) parsed[na_p] <- suppressWarnings(as.Date(rem_str[na_p], format = "%d.%m.%Y"))
    
    out[rem_idx] <- parsed
  }
  out
}

map_activityinfo_columns <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0 || ncol(df) == 0) return(df)
  
  mapping_dict <- list(
    hoh_arabic_name = c(
      "3.1. Head of household (HoH) Name (Arabic)",
      "3.1. Head of household (HoH) Name (Arabic):",
      "Head of household (HoH) Name (Arabic)",
      "hoh_arabic_name",
      "hoh_name",
      "Head of Household Name"
    ),
    hoh_spouse_name = c(
      "3.3. Head of HH's Spouse Name",
      "3.3. Head of HH's Spouse Name:",
      "Head of HH's Spouse Name",
      "hoh_spouse_name",
      "spouse_name"
    ),
    id_type = c(
      "3.11 What is the main form of ID that the Head of Household uses?",
      "3.11. What is the main form of ID that the Head of Household uses?",
      "What is the main form of ID that the Head of Household uses?",
      "hoh_id_type",
      "id_type"
    ),
    hoh_ID_number = c(
      "3.12 What is the Head of Household's ID number?",
      "3.12. What is the Head of Household's ID number?",
      "What is the Head of Household's ID number?",
      "hoh_id_number",
      "hoh_ID_number"
    ),
    sex = c(
      "3.5. Head of HH Gender",
      "3.5. Head of HH Gender:",
      "Head of HH Gender",
      "hoh_sex",
      "sex",
      "gender"
    ),
    age = c(
      "3.4. Age of the head of the household?",
      "3.4. Age of the head of the household",
      "Age of the head of the household?",
      "Age of the head of the household",
      "hoh_age",
      "age"
    ),
    marital_status = c(
      "3.2. Head of HH Marital Status",
      "3.2. Head of HH Marital Status:",
      "Head of HH Marital Status",
      "hoh_marital_status",
      "marital_status"
    ),
    phone_number = c(
      "2.1. Primary Phone Number:",
      "2.1. Primary Phone Number",
      "Primary Phone Number:",
      "Primary Phone Number",
      "primary_phone_number",
      "phone_number"
    ),
    secondary_phone_number = c(
      "2.2. Secondary Phone Number:",
      "2.2. Secondary Phone Number",
      "Secondary Phone Number:",
      "Secondary Phone Number",
      "secondary_phone_number"
    ),
    governorate = c(
      "Governorate Label",
      "governorate_label",
      "Governorate",
      "governorate"
    ),
    district = c(
      "District Label",
      "district_label",
      "District",
      "district"
    ),
    subdistrict = c(
      "Subdistrict Label",
      "Subdistrict Label:",
      "subdistrict_label",
      "sub_district_label",
      "sub_district",
      "subdistrict",
      "Subdistrict"
    ),
    village = c(
      "1.14. Village",
      "1.14. Village:",
      "Village",
      "village"
    ),
    partner = c(
      "Partner Prefix",
      "partner_prefix",
      "Partner",
      "organization",
      "partner"
    ),
    dist_date_calc_new = c(
      "Dist_Date_Calc_New",
      "dist_date_calc_new",
      "Last MPCA Distribution Date",
      "Distribution Date"
    ),
    dist_type = c(
      "Dist_Type",
      "dist_type",
      "Distribution Type"
    ),
    beneficiary_status = c(
      "beneficiary_status",
      "Beneficiary Status"
    ),
    qa_code_sn = c(
      "QA_Code_SN",
      "qa_code_sn",
      "QA Code SN"
    ),
    system_date = c(
      "todays_date",
      "Today's Date",
      "system_date"
    ),
    interviewer = c(
      "interviewer",
      "Interviewer"
    ),
    main_ref = c(
      "main_ref",
      "Main Reference"
    ),
    record_id = c(
      "X.id",
      "_id",
      "record_id",
      "id"
    )
  )

  curr_names <- names(df)
  new_names <- curr_names
  clean_str <- function(s) tolower(gsub("[^a-zA-Z0-9]", "", s))
  
  assigned <- logical(length(curr_names))
  
  for (canonical in names(mapping_dict)) {
    aliases <- mapping_dict[[canonical]]
    clean_aliases <- clean_str(aliases)
    
    # 1. Exact match
    matched_idx <- which(!assigned & curr_names %in% aliases)
    if (length(matched_idx) == 0) {
      # 2. Case-insensitive / punctuation-stripped match
      matched_idx <- which(!assigned & clean_str(curr_names) %in% clean_aliases)
    }
    
    if (length(matched_idx) > 0) {
      target_idx <- matched_idx[1]
      new_names[target_idx] <- canonical
      assigned[target_idx] <- TRUE
    }
  }
  
  names(df) <- new_names
  df
}

prepare_frame <- function(df) {
  if (!is.null(names(df))) names(df) <- make.unique(names(df))
  df <- map_activityinfo_columns(df)
  if (!is.null(names(df))) names(df) <- make.unique(names(df))

  required <- c(
    "partner",
    "record_id",
    "qa_code_sn",
    "system_date",
    "interviewer",
    "main_ref",
    "hoh_ID_number",
    "id_type",
    "phone_number",
    "secondary_phone_number",
    "hoh_arabic_name",
    "marital_status",
    "hoh_spouse_name",
    "age",
    "household_size",
    "sex",
    "beneficiary_status",
    "dist_type",
    "dist_date_calc_new",
    "governorate",
    "district",
    "subdistrict",
    "village"
  )

  for (col in required) {
    if (!col %in% names(df)) df[[col]] <- NA_character_
  }

  df <- df |>
    dplyr::mutate(
      partner = safe_char(partner),
      record_id = safe_char(record_id),
      qa_code_sn = safe_char(qa_code_sn),
      system_date = safe_char(system_date),
      interviewer = safe_char(interviewer),
      main_ref = safe_char(main_ref),
      hoh_ID_number = safe_char(hoh_ID_number),
      id_type = safe_char(id_type),
      phone_number = safe_char(phone_number),
      secondary_phone_number = safe_char(secondary_phone_number),
      hoh_arabic_name = safe_char(hoh_arabic_name),
      marital_status = safe_char(marital_status),
      hoh_spouse_name = safe_char(hoh_spouse_name),
      age = safe_char(age),
      household_size = safe_char(household_size),
      sex = safe_char(sex),
      beneficiary_status = safe_char(beneficiary_status),
      dist_type = safe_char(dist_type),
      dist_date_calc_new = safe_char(dist_date_calc_new),
      governorate = safe_char(governorate),
      district = safe_char(district),
      subdistrict = safe_char(subdistrict),
      village = safe_char(village)
    ) |>
    dplyr::mutate(
      hoh_ID_number_n = normalize_digits(hoh_ID_number),
      phone_number_n = normalize_phone(phone_number),
      secondary_phone_number_n = normalize_phone(secondary_phone_number),
      partner_n = normalize_partner(partner),
      id_type_n = normalize_text(id_type),
      marital_status_n = normalize_marital_status(marital_status),
      hoh_arabic_name_n = normalize_arabic(hoh_arabic_name),
      hoh_spouse_name_n = normalize_arabic(hoh_spouse_name),
      age_n = normalize_age(age),
      household_size_n = normalize_household_size(household_size),
      sex_n = normalize_sex(sex),
      dist_date_calc_new_n = parse_flexible_date(dist_date_calc_new),
      governorate_n = normalize_geo(governorate),
      district_n = normalize_geo(district),
      subdistrict_n = normalize_geo(subdistrict),
      village_n = normalize_geo(village)
    )

  df
}
