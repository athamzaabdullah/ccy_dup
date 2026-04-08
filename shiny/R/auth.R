load_user_store <- function(path) {
  if (!file.exists(path)) {
    return(data.frame(email = character(0), password_hash = character(0), role = character(0)))
  }
  raw <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  if (!"users" %in% names(raw)) {
    return(data.frame(email = character(0), password_hash = character(0), role = character(0)))
  }
  df <- as.data.frame(raw$users, stringsAsFactors = FALSE)
  if (!all(c("email", "password_hash", "role") %in% names(df))) {
    return(data.frame(email = character(0), password_hash = character(0), role = character(0)))
  }
  df
}

load_user_tokens <- function(path) {
  if (!file.exists(path)) {
    return(data.frame(email = character(0), token = character(0)))
  }
  raw <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  if (!"tokens" %in% names(raw)) {
    return(data.frame(email = character(0), token = character(0)))
  }
  df <- as.data.frame(raw$tokens, stringsAsFactors = FALSE)
  if (!all(c("email", "token") %in% names(df))) {
    return(data.frame(email = character(0), token = character(0)))
  }
  df
}

save_user_token <- function(path, email, token) {
  dir <- dirname(path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  tokens <- load_user_tokens(path)
  tokens <- tokens[tokens$email != email, , drop = FALSE]
  tokens <- rbind(tokens, data.frame(email = email, token = token, stringsAsFactors = FALSE))
  jsonlite::write_json(list(tokens = tokens), path, auto_unbox = TRUE, pretty = TRUE)
}

get_user_token <- function(path, email) {
  tokens <- load_user_tokens(path)
  row <- tokens[tokens$email == email, , drop = FALSE]
  if (nrow(row) == 0) return("")
  val <- NULL
  if ("token" %in% names(row) && length(row$token) >= 1) val <- row$token[1]
  if (is.null(val) || length(val) == 0 || is.na(val)) return("")
  as.character(val)
}

load_admin_settings <- function(path) {
  if (!file.exists(path)) {
    return(list(activityinfo_form_id = ""))
  }
  raw <- jsonlite::fromJSON(path, simplifyVector = TRUE)
  if (is.null(raw$activityinfo_form_id)) {
    return(list(activityinfo_form_id = ""))
  }
  list(activityinfo_form_id = raw$activityinfo_form_id)
}

save_admin_settings <- function(path, activityinfo_form_id = "") {
  dir <- dirname(path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  payload <- list(activityinfo_form_id = activityinfo_form_id)
  jsonlite::write_json(payload, path, auto_unbox = TRUE, pretty = TRUE)
}

get_admin_form_id <- function(path) {
  settings <- load_admin_settings(path)
  if (is.null(settings$activityinfo_form_id)) return("")
  settings$activityinfo_form_id
}

authenticate_user <- function(email, password, user_store_path) {
  users <- load_user_store(user_store_path)
  if (nrow(users) == 0) {
    return(list(ok = FALSE, error = "User store not configured"))
  }
  row <- users[users$email == email, , drop = FALSE]
  if (nrow(row) == 0) {
    return(list(ok = FALSE, error = "Invalid credentials"))
  }
  pw_hash <- NULL
  if ("password_hash" %in% names(row) && length(row$password_hash) >= 1) pw_hash <- row$password_hash[1]
  if (is.null(pw_hash) || !nzchar(pw_hash) || !bcrypt::checkpw(password, pw_hash)) {
    return(list(ok = FALSE, error = "Invalid credentials"))
  }
  role_val <- if ("role" %in% names(row) && length(row$role) >= 1) as.character(row$role[1]) else NULL
  list(ok = TRUE, user = list(email = email, role = role_val))
}

verify_sso_token <- function(token, verify_url) {
  if (is.null(verify_url) || verify_url == "") {
    return(list(ok = FALSE, error = "SSO not configured"))
  }
  req <- httr2::request(verify_url) |> httr2::req_method("POST")
  req <- httr2::req_body_json(list(token = token))
  resp <- httr2::req_perform(req)
  if (httr2::resp_status(resp) >= 300) {
    return(list(ok = FALSE, error = "SSO verification failed"))
  }
  data <- httr2::resp_body_json(resp)
  if (!all(c("email", "role") %in% names(data))) {
    return(list(ok = FALSE, error = "SSO response missing fields"))
  }
  list(ok = TRUE, user = list(email = data$email, role = data$role))
}
