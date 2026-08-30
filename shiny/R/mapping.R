map_suggestions <- function(upload_cols, required_cols, min_score = config$mapping_min_score) {
  upload_cols <- trimws(upload_cols)
  required_cols <- trimws(required_cols)
  if (length(upload_cols) == 0 || length(required_cols) == 0) {
    return(data.frame(required_column = character(0), candidate_column = character(0), score = numeric(0)))
  }
  grid <- expand.grid(required_column = required_cols, candidate_column = upload_cols, stringsAsFactors = FALSE)
  grid$score <- stringdist::stringsim(grid$required_column, grid$candidate_column, method = "jw")
  grid$score <- round(grid$score * 100, 1)
  grid <- grid[order(grid$required_column, -grid$score), ]
  grid <- grid[grid$score >= min_score, ]
  grid
}

pick_best_mapping <- function(suggestions) {
  if (nrow(suggestions) == 0) return(list())
  best <- suggestions |> dplyr::group_by(required_column) |> dplyr::slice_max(score, n = 1, with_ties = FALSE)
  setNames(best$candidate_column, best$required_column)
}

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
