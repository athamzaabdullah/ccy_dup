user_roles <- c("ccy_master", "partner_admin", "partner_deduplicator")

normalize_role <- function(role) {
  if (length(role) == 0 || is.null(role)) return("partner_deduplicator")
  role <- trimws(tolower(as.character(role)))
  if (!nzchar(role)) return("partner_deduplicator")
  if (role == "admin") return("ccy_master")
  if (role == "user") return("partner_deduplicator")
  if (role %in% user_roles) return(role)
  "partner_deduplicator"
}

default_partner_names <- function() {
  c("NRC", "DRC", "MC", "SI", "ACTED")
}

role_label <- function(role) {
  switch(
    normalize_role(role),
    ccy_master = "CCY master",
    partner_admin = "Partner admin",
    partner_deduplicator = "Partner deduplicator",
    "Partner deduplicator"
  )
}

normalize_partner_name <- function(x) {
  x <- trimws(as.character(x))
  if (!nzchar(x) || is.na(x)) return("")
  x
}

partner_name_key <- function(x) {
  x <- normalize_partner_name(x)
  key <- tolower(x)
  key <- gsub("[^a-z0-9]+", "_", key)
  gsub("^_+|_+$", "", key)
}

normalize_partner_names <- function(x) {
  vals <- unique(vapply(x, normalize_partner_name, character(1)))
  vals[nzchar(vals)]
}

empty_user_store <- function() {
  data.frame(
    email = character(0),
    password_hash = character(0),
    role = character(0),
    partner_name = character(0),
    active = logical(0),
    created_at = character(0),
    updated_at = character(0),
    created_by = character(0),
    stringsAsFactors = FALSE
  )
}

empty_token_store <- function() {
  data.frame(
    scope_type = character(0),
    scope_key = character(0),
    token = character(0),
    stringsAsFactors = FALSE
  )
}

load_user_store <- function(path) {
  if (!file.exists(path)) {
    return(empty_user_store())
  }
  raw <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  if (!"users" %in% names(raw)) {
    return(empty_user_store())
  }
  df <- as.data.frame(raw$users, stringsAsFactors = FALSE)
  if (nrow(df) == 0) {
    return(empty_user_store())
  }

  if (!"email" %in% names(df)) df$email <- character(nrow(df))
  if (!"password_hash" %in% names(df)) df$password_hash <- character(nrow(df))
  if (!"role" %in% names(df)) df$role <- "partner_deduplicator"
  if (!"partner_name" %in% names(df)) {
    if ("partner_code" %in% names(df)) {
      df$partner_name <- as.character(df$partner_code)
    } else {
      df$partner_name <- ""
    }
  }
  if (!"active" %in% names(df)) df$active <- TRUE
  if (!"created_at" %in% names(df)) df$created_at <- ""
  if (!"updated_at" %in% names(df)) df$updated_at <- ""
  if (!"created_by" %in% names(df)) df$created_by <- ""

  df$email <- trimws(tolower(as.character(df$email)))
  df$password_hash <- as.character(df$password_hash)
  df$role <- vapply(df$role, normalize_role, character(1))
  df$partner_name <- vapply(df$partner_name, normalize_partner_name, character(1))
  df$active <- ifelse(is.na(df$active), TRUE, as.logical(df$active))
  df$created_at <- as.character(df$created_at)
  df$updated_at <- as.character(df$updated_at)
  df$created_by <- as.character(df$created_by)
  df$partner_name[df$role == "ccy_master"] <- ""

  df <- df[, c("email", "password_hash", "role", "partner_name", "active", "created_at", "updated_at", "created_by"), drop = FALSE]
  df[order(df$email), , drop = FALSE]
}

save_user_store <- function(path, users) {
  dir <- dirname(path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  users <- users[, c("email", "password_hash", "role", "partner_name", "active", "created_at", "updated_at", "created_by"), drop = FALSE]
  jsonlite::write_json(list(users = users), path, auto_unbox = TRUE, pretty = TRUE)
}

load_user_tokens <- function(path) {
  if (!file.exists(path)) {
    return(empty_token_store())
  }
  raw <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE)
  if (!"tokens" %in% names(raw)) {
    return(empty_token_store())
  }
  df <- as.data.frame(raw$tokens, stringsAsFactors = FALSE)
  if (nrow(df) == 0) {
    return(empty_token_store())
  }

  if (all(c("email", "token") %in% names(df))) {
    out <- data.frame(
      scope_type = rep("user", nrow(df)),
      scope_key = trimws(tolower(as.character(df$email))),
      token = as.character(df$token),
      stringsAsFactors = FALSE
    )
    return(out)
  }

  if (!all(c("scope_type", "scope_key", "token") %in% names(df))) {
    return(empty_token_store())
  }

  df$scope_type <- trimws(tolower(as.character(df$scope_type)))
  df$scope_key <- trimws(tolower(as.character(df$scope_key)))
  df$token <- as.character(df$token)
  df[, c("scope_type", "scope_key", "token"), drop = FALSE]
}

save_user_tokens <- function(path, tokens) {
  dir <- dirname(path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  tokens <- tokens[, c("scope_type", "scope_key", "token"), drop = FALSE]
  jsonlite::write_json(list(tokens = tokens), path, auto_unbox = TRUE, pretty = TRUE)
}

scope_service_name <- function(scope_type, scope_key) {
  paste0("dedup_token_", trimws(tolower(scope_type)), "_", trimws(tolower(scope_key)))
}

save_scope_token <- function(path, scope_type, scope_key, token) {
  scope_type <- trimws(tolower(as.character(scope_type)))
  scope_key <- trimws(tolower(as.character(scope_key)))
  if (!nzchar(scope_type) || !nzchar(scope_key)) return(invisible(FALSE))

  svc <- scope_service_name(scope_type, scope_key)
  if (requireNamespace("keyring", quietly = TRUE)) {
    tryCatch({
      keyring::key_set_with_value(service = svc, username = scope_key, password = token)
    }, error = function(e) {
      # Fall through to file-based storage.
    })
  }

  tokens <- load_user_tokens(path)
  tokens <- tokens[!(tokens$scope_type == scope_type & tokens$scope_key == scope_key), , drop = FALSE]
  tokens <- rbind(tokens, data.frame(scope_type = scope_type, scope_key = scope_key, token = token, stringsAsFactors = FALSE))
  save_user_tokens(path, tokens)
  invisible(TRUE)
}

get_scope_token <- function(path, scope_type, scope_key) {
  scope_type <- trimws(tolower(as.character(scope_type)))
  scope_key <- trimws(tolower(as.character(scope_key)))
  if (!nzchar(scope_type) || !nzchar(scope_key)) return("")

  svc <- scope_service_name(scope_type, scope_key)
  if (requireNamespace("keyring", quietly = TRUE)) {
    tryCatch({
      val <- keyring::key_get(service = svc, username = scope_key)
      if (!is.null(val) && nzchar(val)) return(val)
    }, error = function(e) {
      # Fall back to file.
    })
  }

  tokens <- load_user_tokens(path)
  row <- tokens[tokens$scope_type == scope_type & tokens$scope_key == scope_key, , drop = FALSE]
  if (nrow(row) == 0) return("")
  val <- row$token[1]
  if (is.null(val) || length(val) == 0 || is.na(val)) return("")
  as.character(val)
}

delete_scope_token <- function(path, scope_type, scope_key) {
  scope_type <- trimws(tolower(as.character(scope_type)))
  scope_key <- trimws(tolower(as.character(scope_key)))
  if (!nzchar(scope_type) || !nzchar(scope_key)) return(invisible(FALSE))

  svc <- scope_service_name(scope_type, scope_key)
  if (requireNamespace("keyring", quietly = TRUE)) {
    tryCatch({
      keyring::key_delete(service = svc, username = scope_key)
    }, error = function(e) {
      # Ignore and continue with file cleanup.
    })
  }

  tokens <- load_user_tokens(path)
  tokens <- tokens[!(tokens$scope_type == scope_type & tokens$scope_key == scope_key), , drop = FALSE]
  save_user_tokens(path, tokens)
  invisible(TRUE)
}

save_user_token <- function(path, email, token) {
  save_scope_token(path, "user", email, token)
}

get_user_token <- function(path, email) {
  get_scope_token(path, "user", email)
}

get_effective_token <- function(path, email, partner_name = "", role = "partner_deduplicator") {
  role <- normalize_role(role)
  partner_key <- partner_name_key(partner_name)
  if (role %in% c("partner_admin", "partner_deduplicator") && nzchar(partner_key)) {
    val <- get_scope_token(path, "partner", partner_key)
    if (nzchar(val)) return(val)
  }
  get_scope_token(path, "user", email)
}

save_effective_token <- function(path, email, partner_name = "", role = "partner_deduplicator", token) {
  role <- normalize_role(role)
  partner_key <- partner_name_key(partner_name)
  if (role %in% c("partner_admin", "partner_deduplicator") && nzchar(partner_key)) {
    return(save_scope_token(path, "partner", partner_key, token))
  }
  save_scope_token(path, "user", email, token)
}

load_admin_settings <- function(path) {
  if (!file.exists(path)) {
    return(list(activityinfo_form_id = "", partner_names = default_partner_names()))
  }
  raw <- jsonlite::fromJSON(path, simplifyVector = TRUE)
  partner_names <- if ("partner_names" %in% names(raw)) raw$partner_names else default_partner_names()
  list(
    activityinfo_form_id = if (!is.null(raw$activityinfo_form_id)) raw$activityinfo_form_id else "",
    partner_names = normalize_partner_names(partner_names)
  )
}

save_admin_settings <- function(path, activityinfo_form_id = "", partner_names = default_partner_names()) {
  dir <- dirname(path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  payload <- list(
    activityinfo_form_id = activityinfo_form_id,
    partner_names = normalize_partner_names(partner_names)
  )
  jsonlite::write_json(payload, path, auto_unbox = TRUE, pretty = TRUE)
}

get_admin_form_id <- function(path) {
  settings <- load_admin_settings(path)
  if (is.null(settings$activityinfo_form_id)) return("")
  settings$activityinfo_form_id
}

get_partner_names <- function(path) {
  settings <- load_admin_settings(path)
  partners <- normalize_partner_names(settings$partner_names)
  if (length(partners) == 0) default_partner_names() else partners
}

save_partner_names <- function(path, partner_names) {
  settings <- load_admin_settings(path)
  save_admin_settings(
    path,
    activityinfo_form_id = if (!is.null(settings$activityinfo_form_id)) settings$activityinfo_form_id else "",
    partner_names = partner_names
  )
}

add_partner_name <- function(path, partner_name) {
  partner_name <- normalize_partner_name(partner_name)
  if (!nzchar(partner_name)) stop("Partner name is required")
  partners <- get_partner_names(path)
  if (partner_name_key(partner_name) %in% vapply(partners, partner_name_key, character(1))) {
    stop("Partner name already exists")
  }
  save_partner_names(path, c(partners, partner_name))
  invisible(TRUE)
}

remove_partner_name <- function(path, partner_name, user_store_path = NULL) {
  partner_name <- normalize_partner_name(partner_name)
  if (!nzchar(partner_name)) stop("Partner name is required")
  partners <- get_partner_names(path)
  keep <- vapply(partners, partner_name_key, character(1)) != partner_name_key(partner_name)
  if (all(keep)) stop("Partner name not found")
  if (!is.null(user_store_path)) {
    users <- load_user_store(user_store_path)
    used <- users$role != "ccy_master" & vapply(users$partner_name, partner_name_key, character(1)) == partner_name_key(partner_name)
    if (any(used)) stop("Partner name is still assigned to one or more users")
  }
  save_partner_names(path, partners[keep])
  invisible(TRUE)
}

get_user_record <- function(path, email) {
  users <- load_user_store(path)
  row <- users[users$email == trimws(tolower(as.character(email))), , drop = FALSE]
  if (nrow(row) == 0) return(NULL)
  row[1, , drop = FALSE]
}

list_manageable_users <- function(path, actor_role, actor_partner = "", include_inactive = TRUE) {
  users <- load_user_store(path)
  actor_role <- normalize_role(actor_role)
  actor_partner_key <- partner_name_key(actor_partner)

  if (actor_role == "ccy_master") {
    out <- users
  } else if (actor_role == "partner_admin") {
    out <- users[vapply(users$partner_name, partner_name_key, character(1)) == actor_partner_key & users$role == "partner_deduplicator", , drop = FALSE]
  } else {
    out <- empty_user_store()
  }

  if (!isTRUE(include_inactive)) {
    out <- out[out$active, , drop = FALSE]
  }
  out
}

count_active_partner_users <- function(path, partner_name) {
  users <- load_user_store(path)
  partner_key <- partner_name_key(partner_name)
  rows <- users[vapply(users$partner_name, partner_name_key, character(1)) == partner_key & users$role == "partner_deduplicator" & users$active, , drop = FALSE]
  nrow(rows)
}

upsert_user <- function(path, email, role, partner_name = "", password = NULL, active = TRUE, actor_email = "") {
  users <- load_user_store(path)
  email <- trimws(tolower(as.character(email)))
  role <- normalize_role(role)
  partner_name <- normalize_partner_name(partner_name)

  if (!nzchar(email)) stop("Email is required")
  if (!grepl("@", email, fixed = TRUE)) stop("Enter a valid email address")
  if (!role %in% user_roles) stop("Invalid role")
  if (role != "ccy_master" && !nzchar(partner_name)) stop("Partner name is required for partner users")
  if (role == "ccy_master") partner_name <- ""

  idx <- which(users$email == email)
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  if (length(idx) == 0 && (is.null(password) || !nzchar(password))) {
    stop("Password is required for a new user")
  }

  password_hash <- NULL
  if (!is.null(password) && nzchar(password)) {
    password_hash <- bcrypt::hashpw(password)
  }

  if (length(idx) == 0) {
    users <- rbind(users, data.frame(
      email = email,
      password_hash = if (is.null(password_hash)) "" else password_hash,
      role = role,
      partner_name = partner_name,
      active = isTRUE(active),
      created_at = now,
      updated_at = now,
      created_by = actor_email,
      stringsAsFactors = FALSE
    ))
  } else {
    i <- idx[1]
    users$role[i] <- role
    users$partner_name[i] <- partner_name
    users$active[i] <- isTRUE(active)
    users$updated_at[i] <- now
    if (!is.null(password_hash)) users$password_hash[i] <- password_hash
  }

  save_user_store(path, users)
  invisible(TRUE)
}

set_user_active <- function(path, email, active = TRUE) {
  users <- load_user_store(path)
  email <- trimws(tolower(as.character(email)))
  idx <- which(users$email == email)
  if (length(idx) == 0) stop("User not found")
  users$active[idx[1]] <- isTRUE(active)
  users$updated_at[idx[1]] <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  save_user_store(path, users)
  invisible(TRUE)
}

delete_user <- function(path, email, token_path = NULL) {
  users <- load_user_store(path)
  email <- trimws(tolower(as.character(email)))
  row <- users[users$email == email, , drop = FALSE]
  if (nrow(row) == 0) stop("User not found")
  users <- users[users$email != email, , drop = FALSE]
  save_user_store(path, users)
  if (!is.null(token_path)) {
    delete_scope_token(token_path, "user", email)
  }
  invisible(TRUE)
}

ensure_backup_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
}

create_user_backup <- function(backup_dir, user_store_path, token_path, admin_settings_path, actor_email = "") {
  ensure_backup_dir(backup_dir)
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  file <- file.path(backup_dir, paste0("user_backup_", stamp, ".json"))
  payload <- list(
    created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    created_by = actor_email,
    users = load_user_store(user_store_path),
    tokens = load_user_tokens(token_path),
    admin_settings = load_admin_settings(admin_settings_path)
  )
  jsonlite::write_json(payload, file, auto_unbox = TRUE, pretty = TRUE)
  file
}

list_user_backups <- function(backup_dir) {
  if (!dir.exists(backup_dir)) return(character(0))
  files <- list.files(backup_dir, pattern = "^user_backup_.*\\.json$", full.names = TRUE)
  files[order(files, decreasing = TRUE)]
}

restore_user_backup <- function(backup_file, user_store_path, token_path, admin_settings_path) {
  if (!file.exists(backup_file)) stop("Backup file not found")
  raw <- jsonlite::fromJSON(backup_file, simplifyDataFrame = TRUE)

  users <- if ("users" %in% names(raw)) as.data.frame(raw$users, stringsAsFactors = FALSE) else empty_user_store()
  if (nrow(users) == 0) {
    users <- empty_user_store()
  } else {
    tmp <- tempfile(fileext = ".json")
    jsonlite::write_json(list(users = users), tmp, auto_unbox = TRUE, pretty = TRUE)
    users <- load_user_store(tmp)
    unlink(tmp)
  }

  tokens <- if ("tokens" %in% names(raw)) as.data.frame(raw$tokens, stringsAsFactors = FALSE) else empty_token_store()
  if (nrow(tokens) == 0) {
    tokens <- empty_token_store()
  } else {
    tmp <- tempfile(fileext = ".json")
    jsonlite::write_json(list(tokens = tokens), tmp, auto_unbox = TRUE, pretty = TRUE)
    tokens <- load_user_tokens(tmp)
    unlink(tmp)
  }

  admin_settings <- if ("admin_settings" %in% names(raw)) raw$admin_settings else list(activityinfo_form_id = "")

  save_user_store(user_store_path, users)
  save_user_tokens(token_path, tokens)
  save_admin_settings(
    admin_settings_path,
    activityinfo_form_id = if (!is.null(admin_settings$activityinfo_form_id)) admin_settings$activityinfo_form_id else "",
    partner_names = if (!is.null(admin_settings$partner_names)) admin_settings$partner_names else default_partner_names()
  )
  invisible(TRUE)
}

authenticate_user <- function(email, password, user_store_path) {
  users <- load_user_store(user_store_path)
  if (nrow(users) == 0) {
    return(list(ok = FALSE, error = "User store not configured"))
  }
  email <- trimws(tolower(as.character(email)))
  row <- users[users$email == email, , drop = FALSE]
  if (nrow(row) == 0 || !isTRUE(row$active[1])) {
    return(list(ok = FALSE, error = "Invalid credentials"))
  }
  pw_hash <- row$password_hash[1]
  if (is.null(pw_hash) || !nzchar(pw_hash) || !bcrypt::checkpw(password, pw_hash)) {
    return(list(ok = FALSE, error = "Invalid credentials"))
  }
  list(ok = TRUE, user = list(
    email = email,
    role = as.character(row$role[1]),
    partner_name = as.character(row$partner_name[1]),
    active = isTRUE(row$active[1])
  ))
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
  list(ok = TRUE, user = list(
    email = trimws(tolower(as.character(data$email))),
    role = normalize_role(data$role),
    partner_name = if ("partner_name" %in% names(data)) {
      normalize_partner_name(data$partner_name)
    } else if ("partner_code" %in% names(data)) {
      normalize_partner_name(data$partner_code)
    } else {
      ""
    }
  ))
}
