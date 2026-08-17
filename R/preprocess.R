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

map_activityinfo_columns <- function(df) {
  mapping <- c(
    "X.id" = "record_id",
    "QA_Code_SN" = "qa_code_sn",
    "organization" = "partner",
    "todays_date" = "system_date",
    "interviewer" = "interviewer",
    "main_ref" = "main_ref",
    "hoh_arabic_name" = "hoh_arabic_name",
    "hoh_marital_status" = "marital_status",
    "hoh_spouse_name" = "hoh_spouse_name",
    "hoh_age" = "age",
    "hoh_sex" = "sex",
    "hoh_id_type" = "id_type",
    "hoh_id_number" = "hoh_ID_number",
    "primary_phone_number" = "phone_number",
    "secondary_phone_number" = "secondary_phone_number",
    "beneficiary_status" = "beneficiary_status",
    "Dist_Type" = "dist_type",
    "governorate" = "governorate",
    "district" = "district",
    "sub_district" = "subdistrict",
    "subdistrict" = "subdistrict",
    "village" = "village"
  )

  for (old_name in names(mapping)) {
    if (old_name %in% names(df)) {
      names(df)[names(df) == old_name] <- mapping[[old_name]]
    }
  }
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
      governorate_n = normalize_geo(governorate),
      district_n = normalize_geo(district),
      subdistrict_n = normalize_geo(subdistrict),
      village_n = normalize_geo(village)
    )

  df
}
