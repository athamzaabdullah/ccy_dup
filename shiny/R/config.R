get_env <- function(name, default = NULL) {
  val <- Sys.getenv(name, unset = NA)
  if (is.na(val)) default else val
}

split_csv <- function(x) {
  if (is.null(x) || x == "") return(character(0))
  parts <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
  parts[nzchar(parts)]
}

config <- list(
  app_name = "Deduplication Check",
  required_columns = c(
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
  ),
  weights = list(
    hoh_arabic_name = 0.35,
    hoh_spouse_name = 0.15,
    phone_number = 0.20,
    geography = 0.15,
    age = 0.10,
    sex = 0.05
  ),
  thresholds = list(
    high = 90,
    medium = 75
  ),
  age_tolerance = 2,
  max_candidates = 500000,
  mapping_min_score = 75
)

config$activityinfo <- list(
  base_url = get_env("DEDUP_ACTIVITYINFO_BASE_URL", "https://www.activityinfo.org"),
  token = get_env("DEDUP_ACTIVITYINFO_TOKEN", NULL),
  batch_size = as.integer(get_env("DEDUP_ACTIVITYINFO_BATCH_SIZE", "2000")),
  database_ids = split_csv(get_env("DEDUP_ACTIVITYINFO_DATABASE_IDS", "")),
  form_ids = split_csv(get_env("DEDUP_ACTIVITYINFO_FORM_IDS", ""))
)

config$paths <- list(
  master_snap_dir = "tmp",
  user_store = get_env("DEDUP_USERS_FILE", "users.json"),
  user_tokens = get_env("DEDUP_USER_TOKENS_FILE", "tmp/user_tokens.json"),
  admin_settings = get_env("DEDUP_ADMIN_SETTINGS_FILE", "tmp/admin_settings.json")
)

to_abs_path <- function(path) {
  if (is.null(path) || !nzchar(path)) return(path)
  is_abs <- grepl("^[A-Za-z]:[/\\\\]|^[/\\\\]{2}|^/", path)
  if (is_abs) return(normalizePath(path, winslash = "/", mustWork = FALSE))
  normalizePath(file.path(getwd(), path), winslash = "/", mustWork = FALSE)
}

config$paths <- lapply(config$paths, to_abs_path)

config$sso <- list(
  verify_url = get_env("DEDUP_SSO_VERIFY_URL", NULL)
)
