# R/modules/mod_upload.R
# CCY Deduplication Platform - Upload & Data Hygiene Workflow Module

validate_upload_dataset <- function(df, required_cols = c("hoh_arabic_name", "hoh_ID_number", "phone_number")) {
  if (is.null(df) || !is.data.frame(df)) {
    return(list(valid = FALSE, message = "Uploaded file does not contain a valid tabular dataset."))
  }
  if (nrow(df) == 0) {
    return(list(valid = FALSE, message = "The uploaded file is completely empty (0 rows)."))
  }
  
  col_names <- names(df)
  has_name <- any(grepl("name|arabic|hoh", col_names, ignore.case = TRUE))
  has_id <- any(grepl("id|national|nid|code", col_names, ignore.case = TRUE))
  has_phone <- any(grepl("phone|mobile|tel", col_names, ignore.case = TRUE))
  
  if (!has_name && !has_id && !has_phone) {
    return(list(
      valid = FALSE,
      message = "The dataset does not appear to contain recognizable beneficiary identification fields (name, ID, or phone)."
    ))
  }
  
  list(
    valid = TRUE,
    rows = nrow(df),
    cols = ncol(df),
    col_names = col_names,
    has_name = has_name,
    has_id = has_id,
    has_phone = has_phone
  )
}

read_uploaded_spreadsheet <- function(path, ext = NULL, sheet = NULL) {
  if (!file.exists(path)) stop("File does not exist: ", path)
  
  if (is.null(ext)) {
    ext <- tolower(tools::file_ext(path))
  }
  
  df <- switch(ext,
    "xlsx" = readxl::read_excel(path, sheet = sheet %||% 1, col_types = "text", .name_repair = "minimal"),
    "xls" = readxl::read_excel(path, sheet = sheet %||% 1, col_types = "text", .name_repair = "minimal"),
    "csv" = utils::read.csv(path, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE),
    "tsv" = utils::read.delim(path, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE),
    stop("Unsupported file extension: ", ext)
  )
  
  as.data.frame(df)
}
