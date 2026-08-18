rm(list = ls())

## install.packages(c("shiny", "dplyr", "DT", "readxl", "openxlsx", "promises", "future", "blastula", "sendmailR", "otp"), dependencies = TRUE)

library(dplyr)
library(bslib)
library(DT)
library(readxl)
library(openxlsx)
library(shiny)
library(promises)
library(future)

# setwd is not needed in Shiny apps and breaks cloud deployment
# setwd("D:/OneDrive - Danish Refugee Council/02. Projects/06_deduplication_app_R/shiny/")

plan(multisession)

# Log Shiny server errors to tmp/shiny_error.log for diagnostics
if (!dir.exists("tmp")) dir.create("tmp", recursive = TRUE)
# Prefer full errors and traces in logs to identify root causes
options(shiny.sanitize.errors = FALSE)
options(shiny.maxRequestSize = 50*1024^2)
options(shiny.error = function(e = NULL, ...) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  sess_info <- paste(capture.output(sessionInfo()), collapse = "\n")
  if (is.null(e)) {
    # Shiny may call the handler without passing the condition. Fall back to geterrmessage().
    msg_body <- paste0("unknown error; geterrmessage: ", geterrmessage())
    extra <- ""
    # sys.calls provides a reliable call stack in all environments
    tb <- paste(capture.output(sys.calls()), collapse = "\n")
  } else {
    msg_body <- conditionMessage(e)
    extra <- paste(capture.output(print(e)), collapse = "\n")
    # Capture call stack for the error context
    tb <- paste(capture.output(sys.calls()), collapse = "\n")
  }
  msg <- paste0(ts, " — ", msg_body, "\n", extra, "\nCallStack:\n", tb, "\nSessionInfo:\n", sess_info, "\n")
  cat(msg, file = file.path(getwd(), "tmp", "shiny_error.log"), append = TRUE)
})

source("R/config.R")
source("R/auth.R")
source("R/mfa.R")
source("R/activityinfo.R")
source("R/preprocess.R")
source("R/mapping.R")
source("R/matching.R")
source("R/export.R")
source("R/jobs.R")
source("R/ui_helpers.R")

theme <- bs_theme(
  version = 5,
  primary = "#53AD32",
  secondary = "#81BD59",
  success = "#A7CD83",
  base_font = "Segoe UI",
  heading_font = "Segoe UI",
  code_font = "Consolas"
)

ui <- fluidPage(
  theme = theme,
  tags$head(
    includeCSS("www/custom.css")
  ),
  tags$script(HTML("
    // Allow Enter to submit the login modal but ensure inputs are committed to Shiny first.
    $(document).on('keydown', function(e) {
      if (e.key !== 'Enter') return;
      if ($('.modal:visible').length === 0) return;
      // only target the login submit when visible
      var loginBtn = $('#login_submit:visible');
      if (loginBtn.length) {
        // delay to allow browser to update input focus/values before clicking
        setTimeout(function() { loginBtn.click(); }, 120);
      }
    });
    Shiny.addCustomMessageHandler('export_ready', function(message) {
      var btn = $('#export_results');
      if (btn.length) {
        btn.prop('disabled', false).removeClass('disabled');
      }
    });
    $(document).on('click', '#export_results', function() {
      var btn = $(this);
      if (btn.prop('disabled')) return;
      btn.prop('disabled', true).addClass('disabled');
    });
  ")),
  div(
    class = "app-shell",
    uiOutput("app_ui")
  )
)

server <- function(input, output, session) {
  auth <- reactiveValues(logged_in = FALSE, role = NULL, email = NULL, partner_name = NULL)
  upload_df <- reactiveVal(NULL)
  # upload_error holds validation messages related to the uploaded file
  upload_error <- reactiveVal(NULL)
  mapping_suggestions <- reactiveVal(data.frame())
  master_mapping_suggestions <- reactiveVal(data.frame())
  current_job <- reactiveVal(NULL)
  current_step <- reactiveVal("upload")
  master_fetch_status <- reactiveVal("Not fetched yet.")
  last_master_snapshot <- reactiveVal(NULL)
  last_master_notify <- reactiveVal(NULL)
  last_master_mtime <- reactiveVal(NULL)
  export_in_progress <- reactiveVal(FALSE)
  export_status <- reactiveVal("")
  settings_username <- reactiveVal("")
  settings_token <- reactiveVal("")
  master_job <- reactiveVal(NULL)
  admin_form_id <- reactiveVal("")
  admin_user_refresh <- reactiveVal(0)
  admin_backup_refresh <- reactiveVal(0)
  partner_name_refresh <- reactiveVal(0)
  fuzzy_high_threshold <- reactiveVal(config$thresholds$high)
  fuzzy_medium_threshold <- reactiveVal(config$thresholds$medium)
  max_candidates <- reactiveVal(config$max_candidates)
  match_fields <- reactiveVal(c(
    "partner",
    "hoh_ID_number",
    "phone_number",
    "hoh_arabic_name",
    "hoh_spouse_name",
    "geography"
  ))
  last_job_notify <- reactiveVal(NULL)
  
  # MFA temp state: pending user after password verification and email OTP cache
  mfa_pending <- reactiveVal(NULL) # list(email=..., user=...)
  mfa_email_codes <- reactiveVal(list())

  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0) y else x
  }

  # Helper: safe wrapper around DT::datatable to prevent crashes when DT internals fail
  safe_datatable <- function(df, opts = list(pageLength = 5)) {
    tryCatch({
      # For large data, enable client-side performance helpers (deferRender) instead of server flag
      is_large <- !is.null(df) && is.data.frame(df) && nrow(df) > 500
      if (is_large) {
        opts <- modifyList(opts, list(pageLength = 10, deferRender = TRUE))
      }
      DT::datatable(df, options = opts, rownames = FALSE)
    }, error = function(e) {
      msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " — datatable render error: ", conditionMessage(e), "\n")
      if (!dir.exists("tmp")) dir.create("tmp", recursive = TRUE)
      cat(msg, file = file.path(getwd(), "tmp", "shiny_error.log"), append = TRUE)
      DT::datatable(data.frame(Error = conditionMessage(e)), options = list(pageLength = 5), rownames = FALSE)
    })
  }

  get_latest_master_snapshot <- function() {
    snap_dir <- config$paths$master_snap_dir
    if (!dir.exists(snap_dir)) return(NULL)
    files <- list.files(snap_dir, pattern = "^master_snapshot_.*\\.rds$", full.names = TRUE)
    if (length(files) == 0) return(NULL)
    info <- file.info(files)
    files[[which.max(info$mtime)]]
  }

  snapshot_is_fresh <- function(path, max_age_hours = 24) {
    if (is.null(path) || !file.exists(path)) return(FALSE)
    mtime <- file.info(path)$mtime
    if (is.na(mtime)) return(FALSE)
    age_hours <- as.numeric(difftime(Sys.time(), mtime, units = "hours"))
    age_hours <= max_age_hours
  }

  observe({
    path <- get_latest_master_snapshot()
    if (!is.null(path)) {
      last_master_snapshot(path)
      last_master_mtime(file.info(path)$mtime)
      stamp <- format(file.info(path)$mtime, "%Y-%m-%d %H:%M:%S")
      if (snapshot_is_fresh(path)) {
        master_fetch_status(paste0("Last fetched: ", stamp, " (cached snapshot)"))
      } else {
        master_fetch_status(paste0("Snapshot is older than 24 hours (", stamp, "). Please fetch a fresh master database."))
      }
    }
  })

  is_ccy_master <- reactive({
    identical(normalize_role(auth$role), "ccy_master")
  })

  is_partner_admin <- reactive({
    identical(normalize_role(auth$role), "partner_admin")
  })

  can_open_admin_workspace <- reactive({
    isTRUE(is_ccy_master()) || isTRUE(is_partner_admin())
  })

  can_edit_token <- reactive({
    isTRUE(is_ccy_master()) || isTRUE(is_partner_admin())
  })

  can_edit_form_id <- reactive({
    isTRUE(is_ccy_master())
  })

  can_fetch_master <- reactive({
    isTRUE(is_ccy_master()) || isTRUE(is_partner_admin())
  })

  admin_button_label <- reactive({
    if (isTRUE(is_partner_admin())) "Users" else "Admin"
  })

  partner_names <- reactive({
    partner_name_refresh()
    get_partner_names(config$paths$admin_settings)
  })

  output$app_ui <- renderUI({
    if (!auth$logged_in) {
      login_ui(config$app_name)
    } else {
      main_ui(
        config$app_name,
        show_admin = isTRUE(can_open_admin_workspace()),
        admin_label = admin_button_label(),
        show_settings = !identical(normalize_role(auth$role), "partner_deduplicator")
      )
    }
  })

  output$app_title <- renderUI({
    name <- settings_username()
    if (is.null(name) || !isTRUE(nzchar(name))) return(tags$span(config$app_name))
    tags$span(paste0(config$app_name, " - ", name))
  })

  observeEvent(input$open_login, {
    showModal(modalDialog(
      size = "m",
      div(style = "display:grid; gap:12px; width:420px; max-width:90%; margin: 8px auto;",
        div(style = "display:flex; align-items:center; justify-content:flex-start;",
          tags$h4("Log in", style = "margin:0; font-size:18px; font-weight:600; color:#111827;")
        ),
        div(style = "display:grid; gap:8px;",
          textInput("login_email", "Email", width = "100%"),
          passwordInput("login_password", "Password", width = "100%")
        ),
        div(style = "display:flex; gap:8px; justify-content:flex-end; align-items:center;",
          actionButton("login_submit", "Log in", class = "btn-primary")
        )
      ),
      easyClose = FALSE,
      footer = NULL
    ))
  })


  # Rate-limiting: track failed attempts per email in an in-memory list
  failed_attempts <- reactiveVal(list())
  max_attempts <- 5
  lockout_secs <- 300 # 5 minutes

  observeEvent(input$login_submit, {
    email <- trimws(tolower(isolate(input$login_email)))
    password <- isolate(input$login_password)
    if (!isTRUE(nzchar(email)) || !isTRUE(nzchar(password))) {
      showNotification("Enter email and password.", type = "error")
      return()
    }

    # Check lockout
    fa <- failed_attempts()
    if (!is.null(fa[[email]])) {
      rec <- fa[[email]]
      if (!is.null(rec$locked_until) && Sys.time() < rec$locked_until) {
        remaining <- round(as.numeric(difftime(rec$locked_until, Sys.time(), units = "secs")))
        showNotification(paste0("Account locked due to repeated failed logins. Try again in ", remaining, " seconds."), type = "error")
        return()
      }
    }

    res <- authenticate_user(email, password, config$paths$user_store)
    if (isTRUE(res$ok)) {
      # Reset failed attempts on success
      fa[[email]] <- NULL
      failed_attempts(fa)

      # Successful login: MFA is optional and can be enabled from Settings.
      auth$logged_in <- TRUE
      auth$email <- email
      auth$role <- res$user$role
      auth$partner_name <- res$user$partner_name
      settings_username(res$user$email)
      settings_token(get_effective_token(config$paths$user_tokens, email, res$user$partner_name, res$user$role))
      admin_form_id(get_admin_form_id(config$paths$admin_settings))
      # Close the login modal so the app is fully usable
      removeModal()
      showNotification("Login successful. MFA is optional and can be managed from Settings.", type = "message")
    } else {
      # Record failed attempt
      now <- Sys.time()
      rec <- fa[[email]]
      if (is.null(rec)) rec <- list(count = 0, last = as.POSIXct(0), locked_until = NULL)
      rec$count <- rec$count + 1
      rec$last <- now
      if (rec$count >= max_attempts) {
        rec$locked_until <- now + lockout_secs
        showNotification(paste0("Too many failed attempts. Account locked for ", lockout_secs, " seconds."), type = "error")
      } else {
        attempts_left <- max_attempts - rec$count
        showNotification(paste0(res$error, " You have ", attempts_left, " attempts remaining."), type = "error")
      }
      fa[[email]] <- rec
      failed_attempts(fa)
    }
  })

  # MFA modal actions: send email OTP, verify code, cancel
  observeEvent(input$send_mfa_email, {
    pending <- mfa_pending()
    if (is.null(pending) || is.null(pending$email)) {
      showNotification("No MFA authentication in progress.", type = "error")
      return()
    }
    email <- pending$email
    # generate 6-digit code and store with expiry
    code <- sprintf("%06d", sample(0:999999, 1))
    expires <- Sys.time() + 300 # 5 minutes
    codes <- mfa_email_codes()
    codes[[email]] <- list(code = code, expires = expires)
    mfa_email_codes(codes)

    # Send code via SMTP using environment variables
    smtp_host <- Sys.getenv("SMTP_HOST", "")
    smtp_port <- as.integer(Sys.getenv("SMTP_PORT", ""))
    smtp_user <- Sys.getenv("SMTP_USER", "")
    smtp_pass <- Sys.getenv("SMTP_PASS", "")
    smtp_tls <- tolower(Sys.getenv("SMTP_USE_TLS", "false")) == "true"
    sender <- Sys.getenv("SENDER_EMAIL", "no-reply@example.com")

    send_ok <- FALSE
    if (nzchar(smtp_host) && nzchar(smtp_user) && nzchar(smtp_pass)) {
      # try blastula
      if (requireNamespace("blastula", quietly = TRUE)) {
        tryCatch({
          email_msg <- blastula::compose_email(
            body = blastula::md(paste0("Your login code is **", code, "**. It expires in 5 minutes."))
          )
          smtp <- blastula::smtp_server(host = smtp_host, port = smtp_port, username = smtp_user, password = smtp_pass, use_ssl = smtp_tls)
          blastula::smtp_send(email_msg, from = sender, to = email, subject = "Your login code", credentials = smtp)
          send_ok <- TRUE
        }, error = function(e) {
          send_ok <<- FALSE
        })
      } else if (requireNamespace("sendmailR", quietly = TRUE)) {
        tryCatch({
          body <- sprintf("Your login code is %s. It expires in 5 minutes.", code)
          sendmailR::sendmail(from = sender, to = email, subject = "Your login code", msg = body, control = list(smtpServer = smtp_host))
          send_ok <- TRUE
        }, error = function(e) {
          send_ok <<- FALSE
        })
      }
    }

    if (isTRUE(send_ok)) {
      showNotification("A verification code was sent to your email.", type = "message")
    } else {
      showNotification("Failed to send verification email. Ensure SMTP environment variables are configured on the server.", type = "error")
    }
  })

  observeEvent(input$verify_mfa, {
    pending <- mfa_pending()
    if (is.null(pending) || is.null(pending$email)) {
      showNotification("No MFA authentication in progress.", type = "error")
      return()
    }
    email <- pending$email
    code <- isolate(input$mfa_code)
    # First try TOTP if secret present and mfa installed
    totp_ok <- FALSE
    try({ totp_ok <- verify_totp_code(email, code) }, silent = TRUE)
    email_ok <- FALSE
    codes <- mfa_email_codes()
    if (!is.null(codes[[email]])) {
      rec <- codes[[email]]
      if (Sys.time() <= rec$expires && identical(as.character(code), as.character(rec$code))) email_ok <- TRUE
    }

    if (isTRUE(totp_ok) || isTRUE(email_ok)) {
      # finalize login
      auth$logged_in <- TRUE
      auth$email <- pending$email
      auth$role <- pending$user$role
      auth$partner_name <- pending$user$partner_name
      settings_username(pending$user$email)
      settings_token(get_effective_token(config$paths$user_tokens, pending$user$email, pending$user$partner_name, pending$user$role))
      admin_form_id(get_admin_form_id(config$paths$admin_settings))
      mfa_pending(NULL)
      # clear used email code
      codes[[email]] <- NULL
      mfa_email_codes(codes)
      removeModal()
      showNotification("Two-factor authentication successful.", type = "message")
    } else {
      showNotification("Invalid or expired code.", type = "error")
    }
  })

  observeEvent(input$mfa_cancel, {
    mfa_pending(NULL)
    removeModal()
  })

  perform_logout <- function() {
    auth$logged_in <- FALSE
    auth$email <- NULL
    auth$role <- NULL
    auth$partner_name <- NULL
    current_job(NULL)
    upload_df(NULL)
    current_step("upload")
    settings_username("")
    settings_token("")
    admin_form_id("")
  }

  # Cleanup when the Shiny session ends: cancel background jobs and clear sensitive values
  session$onSessionEnded(function() {
    tryCatch({
      id <- NULL
      try({ id <- current_job() }, silent = TRUE)
      if (!is.null(id)) {
        try({ set_job_canceled(id) }, silent = TRUE)
      }

      mid <- NULL
      try({ mid <- master_job() }, silent = TRUE)
      if (!is.null(mid)) {
        try({ set_job_canceled(mid) }, silent = TRUE)
      }

      # Clear sensitive reactive values from memory (do not delete persistent keyring entries)
      try({ settings_token("") }, silent = TRUE)
      try({ settings_username("") }, silent = TRUE)
      try({ admin_form_id("") }, silent = TRUE)
      try({ auth$logged_in <- FALSE; auth$email <- NULL; auth$role <- NULL; auth$partner_name <- NULL }, silent = TRUE)

      # Ensure job references are cleared
      try({ current_job(NULL); master_job(NULL) }, silent = TRUE)
    }, error = function(e) {
      if (!dir.exists("tmp")) dir.create("tmp", recursive = TRUE)
      msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " — session cleanup error: ", conditionMessage(e), "\n")
      cat(msg, file = file.path(getwd(), "tmp", "shiny_error.log"), append = TRUE)
    })
  })

  observeEvent(input$logout, {
    job <- job_status()
    fetch_job <- if (!is.null(master_job())) get_job(master_job()) else NULL
    if (!is.null(job) && job$status %in% c("queued", "running")) {
      showModal(modalDialog(
        title = "Job in progress",
        p("A deduplication job is still running. Logging out will stop it."),
        footer = tagList(
          actionButton("logout_confirm", "Log out anyway", class = "btn-danger"),
          actionButton("logout_cancel", "Stay signed in", class = "btn-secondary")
        ),
        easyClose = TRUE
      ))
      return()
    }
    if (!is.null(fetch_job) && fetch_job$status %in% c("queued", "running")) {
      showModal(modalDialog(
        title = "Master fetch in progress",
        p("Master data is still being fetched. Logging out will stop it."),
        footer = tagList(
          actionButton("logout_confirm_fetch", "Log out anyway", class = "btn-danger"),
          actionButton("logout_cancel_fetch", "Stay signed in", class = "btn-secondary")
        ),
        easyClose = TRUE
      ))
      return()
    }
    perform_logout()
  })

  observeEvent(input$logout_confirm, {
    removeModal()
    id <- current_job()
    if (!is.null(id)) set_job_canceled(id)
    perform_logout()
  })

  observeEvent(input$logout_cancel, removeModal())

  observeEvent(input$logout_confirm_fetch, {
    removeModal()
    id <- master_job()
    if (!is.null(id)) set_job_canceled(id, "Master fetch canceled")
    perform_logout()
  })

  observeEvent(input$logout_cancel_fetch, removeModal())

  observeEvent(input$open_settings, {
    updateTextInput(session, "settings_username", value = settings_username())
    if (isTRUE(can_edit_token())) {
      updateTextInput(session, "settings_token", value = settings_token())
    }
    if (isTRUE(can_edit_form_id())) {
      updateTextInput(session, "settings_form_id", value = admin_form_id())
    }
    current_step("settings")
  })

  # Manage MFA from settings
  observeEvent(input$manage_mfa, {
    if (!isTRUE(auth$logged_in)) {
      showNotification("Please log in to manage MFA.", type = "error")
      return()
    }

    email <- auth$email
    secret <- ""
    try({ secret <- get_mfa_secret(email) }, silent = TRUE)
    if (!is.null(secret) && nzchar(secret)) {
      # already enrolled
      showModal(modalDialog(
        title = "Manage MFA",
        p("TOTP (authenticator app) is currently enabled for your account."),
        p("You can disable MFA or regenerate your secret (this will require re-enrollment)."),
        div(style = "display:flex; gap:8px; justify-content:flex-end;",
          actionButton("mfa_disable", "Disable MFA", class = "btn-danger"),
          actionButton("mfa_regen", "Regenerate secret", class = "btn-secondary")
        ),
        easyClose = TRUE
      ))
    } else {
      # show enrollment flow — generate a secret instruction (TOTP requires 'otp')
      can_otp <- requireNamespace("otp", quietly = TRUE)
      showModal(modalDialog(
        title = "Enroll in Authenticator App (TOTP)",
        if (can_otp) {
          tagList(
            p("Your account is not enrolled for TOTP. Follow these steps:"),
            tags$ol(
              tags$li("Open your authenticator app and choose to add an account."),
              tags$li("Scan the QR code or enter the secret shown below."),
              tags$li("Enter the 6-digit code from your app to verify.")
            ),
            # Placeholder: we do not auto-generate secret without otp helper; instruct admin
            tags$p("TOTP enrollment requires the 'otp' package. Install it on the server to enable TOTP enrollment."),
            tags$p("You may still use email OTP fallback which sends codes to your email.")
          )
        } else {
          tagList(
            p("TOTP enrollment is not available because the 'otp' package is not installed on the server."),
            p("You can continue to use email one-time passwords as a second factor.")
          )
        },
        footer = tagList(actionButton("send_otp_enroll_email", "Enable email OTP", class = "btn-primary")),
        easyClose = TRUE
      ))
    }
  })

  observeEvent(input$mfa_disable, {
    if (!isTRUE(auth$logged_in)) return()
    email <- auth$email
    ok <- delete_mfa_secret(email)
    removeModal()
    if (isTRUE(ok)) showNotification("MFA disabled for your account.", type = "message") else showNotification("Failed to disable MFA.", type = "error")
  })

  observeEvent(input$mfa_regen, {
    # For simplicity, disable then prompt to re-enroll
    if (!isTRUE(auth$logged_in)) return()
    email <- auth$email
    ok <- delete_mfa_secret(email)
    removeModal()
    if (isTRUE(ok)) showNotification("MFA secret removed; please enroll again.", type = "message") else showNotification("Failed to remove MFA secret.", type = "error")
  })

  observeEvent(input$send_mfa_enroll_email, {
    # This enables email OTP only; no persistent secret created
    if (!isTRUE(auth$logged_in)) {
      showNotification("Please log in to enable email OTP.", type = "error")
      return()
    }
    email <- auth$email
    # generate code and send like send_mfa_email
    code <- sprintf("%06d", sample(0:999999, 1))
    expires <- Sys.time() + 300
    codes <- mfa_email_codes()
    codes[[email]] <- list(code = code, expires = expires)
    mfa_email_codes(codes)
    # try send via SMTP as earlier
    smtp_host <- Sys.getenv("SMTP_HOST", "")
    smtp_port <- as.integer(Sys.getenv("SMTP_PORT", ""))
    smtp_user <- Sys.getenv("SMTP_USER", "")
    smtp_pass <- Sys.getenv("SMTP_PASS", "")
    smtp_tls <- tolower(Sys.getenv("SMTP_USE_TLS", "false")) == "true"
    sender <- Sys.getenv("SENDER_EMAIL", "no-reply@example.com")
    send_ok <- FALSE
    if (nzchar(smtp_host) && nzchar(smtp_user) && nzchar(smtp_pass)) {
      if (requireNamespace("blastula", quietly = TRUE)) {
        tryCatch({
          email_msg <- blastula::compose_email(body = blastula::md(paste0("Your enrollment code is **", code, "**. It expires in 5 minutes.")))
          smtp <- blastula::smtp_server(host = smtp_host, port = smtp_port, username = smtp_user, password = smtp_pass, use_ssl = smtp_tls)
          blastula::smtp_send(email_msg, from = sender, to = email, subject = "MFA enrollment code", credentials = smtp)
          send_ok <- TRUE
        }, error = function(e) { send_ok <<- FALSE })
      }
    }
    if (isTRUE(send_ok)) showNotification("Enrollment code sent to your email.", type = "message") else showNotification("Failed to send enrollment email. Configure SMTP.", type = "error")
  })

  observeEvent(input$close_settings, {
    current_step("upload")
  })

  observeEvent(input$save_settings, {
    tryCatch({
      settings_username(auth$email)
      if (isTRUE(can_edit_token()) && !is.null(input$settings_token) && isTRUE(nzchar(input$settings_token))) {
        settings_token(input$settings_token)
        if (!is.null(auth$email) && isTRUE(nzchar(auth$email))) {
          save_effective_token(config$paths$user_tokens, auth$email, auth$partner_name, auth$role, input$settings_token)
        }
      }
      if (isTRUE(can_edit_form_id())) {
        form_id <- input$settings_form_id
        if (!is.null(form_id) && isTRUE(nzchar(form_id))) {
          admin_form_id(form_id)
          save_admin_settings(config$paths$admin_settings, form_id)
        }
      }
      showNotification("Settings saved for this session.", type = "message")
      current_step("upload")
    }, error = function(e) {
      # Log full error to tmp for diagnostics and show brief notification
      if (!dir.exists("tmp")) dir.create("tmp", recursive = TRUE)
      ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      msg <- paste0(ts, " — settings save error: ", conditionMessage(e), "\n")
      cat(msg, file = file.path(getwd(), "tmp", "shiny_error.log"), append = TRUE)
      # Log some inputs to help debug
      cat(paste0("settings_username=", input$settings_username, "\n"), file = file.path(getwd(), "tmp", "shiny_error.log"), append = TRUE)
      cat(paste0("settings_token_present=", (!is.null(input$settings_token) && nzchar(as.character(input$settings_token))), "\n"), file = file.path(getwd(), "tmp", "shiny_error.log"), append = TRUE)
      if (!is.null(input$settings_form_id)) cat(paste0("settings_form_id=", input$settings_form_id, "\n"), file = file.path(getwd(), "tmp", "shiny_error.log"), append = TRUE)
      showNotification("Failed to save settings (error logged).", type = "error")
    })
  })

  observeEvent(input$cancel_job, {
    job <- job_status()
    if (!is.null(job) && job$status %in% c("queued", "running")) {
      showModal(modalDialog(
        title = "Stop current run?",
        p("This will cancel the running job and clear the uploaded data. You will need to start over."),
        footer = tagList(
          actionButton("cancel_confirm", "Yes, stop and reset", class = "btn-danger"),
          actionButton("cancel_abort", "Keep running", class = "btn-secondary")
        ),
        easyClose = TRUE
      ))
      return()
    }
    current_job(NULL)
    upload_df(NULL)
    mapping_suggestions(data.frame())
    current_step("upload")
  })

  observeEvent(input$cancel_confirm, {
    removeModal()
    id <- current_job()
    if (!is.null(id)) {
      set_job_canceled(id)
    }
    current_job(NULL)
    upload_df(NULL)
    mapping_suggestions(data.frame())
    current_step("upload")
  })

  observeEvent(input$cancel_abort, removeModal())

  observeEvent(input$admin_open, {
    if (!isTRUE(can_open_admin_workspace())) {
      showNotification("Your role cannot manage users.", type = "error")
      return()
    }
    current_step("admin")
  })

  manageable_users <- reactive({
    admin_user_refresh()
    if (!isTRUE(auth$logged_in)) return(empty_user_store())
    list_manageable_users(
      config$paths$user_store,
      actor_role = auth$role,
      actor_partner = auth$partner_name,
      include_inactive = TRUE
    )
  })

  manageable_user_choices <- reactive({
    users <- manageable_users()
    if (nrow(users) == 0) {
      return(c("No users available in your scope" = ""))
    }
    labels <- paste0(
      users$email, " (", vapply(users$role, role_label, character(1)),
      ifelse(nzchar(users$partner_name), paste0(" / ", users$partner_name), ""),
      ifelse(users$active, "", " / inactive"),
      ")"
    )
    stats::setNames(users$email, labels)
  })

  selected_manageable_user <- reactive({
    email <- trimws(tolower(as.character(input$admin_selected_user %||% "")))
    users <- manageable_users()
    if (!nzchar(email) || nrow(users) == 0) return(NULL)
    row <- users[users$email == email, , drop = FALSE]
    if (nrow(row) == 0) return(NULL)
    row[1, , drop = FALSE]
  })

  output$admin_access_summary <- renderUI({
    if (!isTRUE(can_open_admin_workspace())) return(NULL)
    tagList(
      p(
        if (isTRUE(is_ccy_master())) {
          "CCY master access: create, edit, deactivate, delete, back up, and restore all users."
        } else {
          paste0(
            "Partner admin access for partner \"", auth$partner_name,
            "\": manage up to ", config$limits$partner_users_max,
            " deduplicator users and maintain the shared ActivityInfo token."
          )
        }
      )
    )
  })

  output$admin_workspace_ui <- renderUI({
    if (!isTRUE(can_open_admin_workspace())) {
      return(tags$p(style = "color:#b91c1c;", "User administration is not available for your role."))
    }

    role_choices <- setNames(user_roles, vapply(user_roles, role_label, character(1)))
    tagList(
      selectInput(
        "admin_selected_user",
        "Existing user",
        choices = c("Select a user" = "", manageable_user_choices()),
        selected = "",
        selectize = FALSE
      ),
      if (nrow(manageable_users()) == 0) {
        p(style = "color:#6b7280; margin-top:-8px;", "No users are currently available in your management scope.")
      },
      textInput("admin_user_email", "User email"),
      passwordInput("admin_user_password", "Password (required for new users)"),
      if (isTRUE(is_ccy_master())) {
        selectInput("admin_user_role", "Role", choices = role_choices, selected = "partner_deduplicator")
      } else {
        selectInput("admin_user_role", "Role", choices = setNames("partner_deduplicator", "Partner deduplicator"), selected = "partner_deduplicator")
      },
      if (isTRUE(is_ccy_master())) {
        selectInput("admin_user_partner", "Partner name", choices = partner_names(), selected = if (length(partner_names()) >= 1) partner_names()[1] else character(0), selectize = FALSE)
      } else {
        tagList(
          tags$label(class = "control-label", "Partner name"),
          tags$p(
            style = "margin-bottom:12px; color:#475569;",
            paste0("Fixed for your account: ", auth$partner_name)
          )
        )
      },
      checkboxInput("admin_user_active", "Active", value = TRUE),
      div(
        style = "display:flex; gap:8px; flex-wrap:wrap;",
        actionButton("admin_save_user", "Create or update user", class = "btn-primary"),
        actionButton("admin_clear_user_form", "Clear form", class = "btn-secondary"),
        actionButton("admin_toggle_user", "Activate or deactivate", class = "btn-ghost"),
        actionButton("admin_delete_user", "Delete user", class = "btn-danger")
      ),
      tags$hr(),
      tags$strong("Managed users"),
      DT::DTOutput("admin_users_table"),
      uiOutput("partner_registry_ui"),
      uiOutput("admin_backup_ui")
    )
  })

  output$admin_users_table <- renderDT({
    req(can_open_admin_workspace())
    users <- manageable_users()
    if (nrow(users) == 0) {
      return(safe_datatable(data.frame(note = "No users in scope"), opts = list(pageLength = 5)))
    }
    show_df <- users[, c("email", "role", "partner_name", "active", "updated_at"), drop = FALSE]
    show_df$role <- vapply(show_df$role, role_label, character(1))
    names(show_df) <- c("Email", "Role", "Partner name", "Active", "Updated at")
    safe_datatable(show_df, opts = list(pageLength = 10))
  })

  output$partner_registry_ui <- renderUI({
    if (!isTRUE(is_ccy_master())) return(NULL)
    partners <- partner_names()
    tagList(
      tags$hr(),
      tags$strong("Partner names"),
      p(style = "color:#475569;", "CCY master can manage the partner name list used for user scope and shared partner tokens."),
      div(
        style = "display:flex; gap:8px; flex-wrap:wrap; align-items:flex-end;",
        div(style = "min-width:240px; flex:1 1 240px;",
          textInput("partner_name_new", "Add partner name")
        ),
        actionButton("partner_name_add", "Add partner", class = "btn-primary"),
        div(style = "min-width:240px; flex:1 1 240px;",
          selectInput("partner_name_remove", "Existing partner names", choices = partners, selected = if (length(partners) >= 1) partners[1] else character(0), selectize = FALSE)
        ),
        actionButton("partner_name_remove_btn", "Remove partner", class = "btn-danger")
      )
    )
  })

  output$admin_backup_ui <- renderUI({
    if (!isTRUE(is_ccy_master())) return(NULL)
    admin_backup_refresh()
    backups <- list_user_backups(config$paths$user_backups)
    choices <- setNames(backups, basename(backups))
    tagList(
      tags$hr(),
      tags$strong("Backup and restore"),
      p(style = "color:#475569;", "Backups include users, scoped tokens, and admin settings."),
      div(
        class = "admin-backup-grid",
        div(
          class = "admin-backup-cell admin-backup-action",
          tags$label(class = "control-label", "Create backup"),
          actionButton("admin_create_backup", "Create backup", class = "btn-secondary w-100")
        ),
        div(
          class = "admin-backup-cell admin-backup-select",
          selectInput("admin_restore_backup", "Available backups", choices = choices, selected = if (length(backups) >= 1) backups[1] else character(0), selectize = FALSE)
        ),
        div(
          class = "admin-backup-cell admin-backup-action",
          tags$label(class = "control-label", "Restore selected backup"),
          actionButton("admin_restore_backup_btn", "Restore backup", class = "btn-danger w-100")
        )
      )
    )
  })

  observeEvent(selected_manageable_user(), {
    row <- selected_manageable_user()
    if (is.null(row)) return()
    updateTextInput(session, "admin_user_email", value = row$email[1])
    if (isTRUE(is_ccy_master())) {
      updateSelectInput(session, "admin_user_partner", choices = partner_names(), selected = row$partner_name[1])
    }
    updateSelectInput(session, "admin_user_role", selected = row$role[1])
    updateCheckboxInput(session, "admin_user_active", value = isTRUE(row$active[1]))
    updateTextInput(session, "admin_user_password", value = "")
  }, ignoreInit = TRUE)

  observeEvent(input$admin_clear_user_form, {
    updateSelectInput(session, "admin_selected_user", selected = "")
    updateTextInput(session, "admin_user_email", value = "")
    updateTextInput(session, "admin_user_password", value = "")
    updateSelectInput(session, "admin_user_role", selected = "partner_deduplicator")
    if (isTRUE(is_ccy_master())) {
      updateSelectInput(session, "admin_user_partner", choices = partner_names(), selected = if (length(partner_names()) >= 1) partner_names()[1] else character(0))
    }
    updateCheckboxInput(session, "admin_user_active", value = TRUE)
  })

  observeEvent(input$admin_save_user, {
    req(can_open_admin_workspace())
    selected_email <- trimws(tolower(as.character(input$admin_selected_user %||% "")))
    email <- trimws(tolower(as.character(input$admin_user_email %||% "")))
    if (!nzchar(email)) {
      showNotification("Enter a user email first.", type = "error")
      return()
    }
    action_label <- if (nzchar(selected_email)) "update" else "create"
    showModal(modalDialog(
      title = if (action_label == "update") "Confirm user update" else "Confirm user creation",
      p(
        if (action_label == "update") {
          paste0("Update the user account for ", email, "?")
        } else {
          paste0("Create a new user account for ", email, "?")
        }
      ),
      footer = tagList(
        actionButton("admin_save_user_cancel", "Cancel", class = "btn-secondary"),
        actionButton("admin_save_user_confirm", "Confirm", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$admin_save_user_cancel, {
    removeModal()
  })

  observeEvent(input$admin_save_user_confirm, {
    req(can_open_admin_workspace())
    removeModal()
    tryCatch({
      selected_email <- trimws(tolower(as.character(input$admin_selected_user %||% "")))
      email <- trimws(tolower(as.character(input$admin_user_email %||% "")))
      password <- as.character(input$admin_user_password %||% "")
      role <- if (isTRUE(is_ccy_master())) as.character(input$admin_user_role %||% "partner_deduplicator") else "partner_deduplicator"
      partner_name <- if (isTRUE(is_ccy_master())) as.character(input$admin_user_partner %||% "") else auth$partner_name
      partner_name <- normalize_partner_name(partner_name)
      active <- isTRUE(input$admin_user_active)

      if (nzchar(selected_email) && selected_email != email) {
        stop("Email cannot be changed for an existing user. Create a new user instead.")
      }
      if (identical(normalize_role(role), "ccy_master") && !isTRUE(is_ccy_master())) {
        stop("Only the CCY master can create or edit this role.")
      }
      if (identical(normalize_role(role), "ccy_master")) {
        partner_name <- ""
      } else {
        partner_keys <- vapply(partner_names(), partner_name_key, character(1))
        if (!partner_name_key(partner_name) %in% partner_keys) {
          stop("Select a valid partner name from the managed partner list.")
        }
      }

      existing <- get_user_record(config$paths$user_store, email)
      if (!is.null(existing) && !nzchar(selected_email) && !isTRUE(is_ccy_master())) {
        stop("That email already exists.")
      }

      if (!isTRUE(is_ccy_master())) {
        if (!identical(normalize_role(role), "partner_deduplicator")) {
          stop("Partner admins can only manage deduplicator users.")
        }
        if (is.null(existing) || !identical(partner_name_key(existing$partner_name[1]), partner_name_key(auth$partner_name)) || !identical(existing$role[1], "partner_deduplicator")) {
          existing <- NULL
        }
        current_count <- count_active_partner_users(config$paths$user_store, auth$partner_name)
        existing_active <- !is.null(existing) && isTRUE(existing$active[1])
        if (isTRUE(active) && !existing_active && current_count >= config$limits$partner_users_max) {
          stop(paste0("Partner user limit reached (", config$limits$partner_users_max, ")."))
        }
      }

      upsert_user(
        config$paths$user_store,
        email = email,
        role = role,
        partner_name = partner_name,
        password = password,
        active = active,
        actor_email = auth$email
      )
      admin_user_refresh(admin_user_refresh() + 1)
      showNotification("User saved.", type = "message")
      updateTextInput(session, "admin_user_password", value = "")
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = 8)
    })
  })

  observeEvent(input$admin_toggle_user, {
    req(can_open_admin_workspace())
    row <- selected_manageable_user()
    if (is.null(row)) {
      showNotification("Select a user first.", type = "error")
      return()
    }
    new_active <- !isTRUE(row$active[1])
    showModal(modalDialog(
      title = if (new_active) "Confirm user activation" else "Confirm user deactivation",
      p(paste0(if (new_active) "Activate " else "Deactivate ", row$email[1], "?")),
      footer = tagList(
        actionButton("admin_toggle_user_cancel", "Cancel", class = "btn-secondary"),
        actionButton("admin_toggle_user_confirm", if (new_active) "Activate" else "Deactivate", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$admin_toggle_user_cancel, {
    removeModal()
  })
  
  observeEvent(input$admin_toggle_user_confirm, {
    req(can_open_admin_workspace())
    removeModal()
    row <- selected_manageable_user()
    if (is.null(row)) {
      showNotification("Select a user first.", type = "error")
      return()
    }
    tryCatch({
      new_active <- !isTRUE(row$active[1])
      if (!isTRUE(is_ccy_master()) && new_active) {
        current_count <- count_active_partner_users(config$paths$user_store, auth$partner_name)
        if (current_count >= config$limits$partner_users_max) {
          stop(paste0("Partner user limit reached (", config$limits$partner_users_max, ")."))
        }
      }
      set_user_active(config$paths$user_store, row$email[1], active = new_active)
      admin_user_refresh(admin_user_refresh() + 1)
      updateCheckboxInput(session, "admin_user_active", value = new_active)
      showNotification(if (new_active) "User activated." else "User deactivated.", type = "message")
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = 8)
    })
  })

  observeEvent(input$admin_delete_user, {
    req(can_open_admin_workspace())
    row <- selected_manageable_user()
    if (is.null(row)) {
      showNotification("Select a user first.", type = "error")
      return()
    }
    if (identical(row$email[1], auth$email)) {
      showNotification("You cannot delete your own account while signed in.", type = "error")
      return()
    }
    showModal(modalDialog(
      title = "Confirm user deletion",
      p(paste0("Delete the user account for ", row$email[1], "? This cannot be undone except by restoring a backup.")),
      footer = tagList(
        actionButton("admin_delete_user_cancel", "Cancel", class = "btn-secondary"),
        actionButton("admin_delete_user_confirm", "Delete user", class = "btn-danger")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$admin_delete_user_cancel, {
    removeModal()
  })

  observeEvent(input$admin_delete_user_confirm, {
    req(can_open_admin_workspace())
    removeModal()
    row <- selected_manageable_user()
    if (is.null(row)) {
      showNotification("Select a user first.", type = "error")
      return()
    }
    if (identical(row$email[1], auth$email)) {
      showNotification("You cannot delete your own account while signed in.", type = "error")
      return()
    }
    tryCatch({
      delete_user(config$paths$user_store, row$email[1], token_path = config$paths$user_tokens)
      admin_user_refresh(admin_user_refresh() + 1)
      updateSelectInput(session, "admin_selected_user", selected = "")
      updateTextInput(session, "admin_user_email", value = "")
      updateTextInput(session, "admin_user_password", value = "")
      showNotification("User deleted.", type = "message")
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = 8)
    })
  })

  observeEvent(input$admin_create_backup, {
    req(is_ccy_master())
    tryCatch({
      file <- create_user_backup(
        config$paths$user_backups,
        config$paths$user_store,
        config$paths$user_tokens,
        config$paths$admin_settings,
        actor_email = auth$email
      )
      admin_backup_refresh(admin_backup_refresh() + 1)
      showNotification(paste0("Backup created: ", basename(file)), type = "message", duration = 6)
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = 8)
    })
  })

  observeEvent(input$partner_name_add, {
    req(is_ccy_master())
    tryCatch({
      add_partner_name(config$paths$admin_settings, input$partner_name_new)
      partner_name_refresh(partner_name_refresh() + 1)
      updateTextInput(session, "partner_name_new", value = "")
      showNotification("Partner name added.", type = "message")
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = 8)
    })
  })

  observeEvent(input$partner_name_remove_btn, {
    req(is_ccy_master())
    partner_name <- as.character(input$partner_name_remove %||% "")
    if (!nzchar(partner_name)) {
      showNotification("Select a partner name first.", type = "error")
      return()
    }
    showModal(modalDialog(
      title = "Confirm partner name removal",
      p(paste0("Remove partner name '", partner_name, "'? This may affect user scoping and tokens.")),
      footer = tagList(
        actionButton("partner_name_remove_cancel", "Cancel", class = "btn-secondary"),
        actionButton("partner_name_remove_confirm", "Remove partner", class = "btn-danger")
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$partner_name_remove_cancel, {
    removeModal()
  })
  
  observeEvent(input$partner_name_remove_confirm, {
    req(is_ccy_master())
    removeModal()
    partner_name <- as.character(input$partner_name_remove %||% "")
    if (!nzchar(partner_name)) {
      showNotification("Select a partner name first.", type = "error")
      return()
    }
    tryCatch({
      remove_partner_name(config$paths$admin_settings, partner_name, user_store_path = config$paths$user_store)
      partner_name_refresh(partner_name_refresh() + 1)
      showNotification("Partner name removed.", type = "message")
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = 8)
    })
  })

  observeEvent(input$admin_restore_backup_btn, {
    req(is_ccy_master())
    backup_file <- as.character(input$admin_restore_backup %||% "")
    if (!nzchar(backup_file)) {
      showNotification("Select a backup to restore.", type = "error")
      return()
    }
    showModal(modalDialog(
      title = "Confirm backup restore",
      p(paste0("Restore backup '", basename(backup_file), "'? This will overwrite current users, tokens, and admin settings.")),
      footer = tagList(
        actionButton("admin_restore_backup_cancel", "Cancel", class = "btn-secondary"),
        actionButton("admin_restore_backup_confirm", "Restore backup", class = "btn-danger")
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$admin_restore_backup_cancel, {
    removeModal()
  })
  
  observeEvent(input$admin_restore_backup_confirm, {
    req(is_ccy_master())
    removeModal()
    backup_file <- as.character(input$admin_restore_backup %||% "")
    if (!nzchar(backup_file)) {
      showNotification("Select a backup to restore.", type = "error")
      return()
    }
    tryCatch({
      restore_user_backup(
        backup_file,
        config$paths$user_store,
        config$paths$user_tokens,
        config$paths$admin_settings
      )
      admin_user_refresh(admin_user_refresh() + 1)
      admin_backup_refresh(admin_backup_refresh() + 1)
      partner_name_refresh(partner_name_refresh() + 1)
      current_user <- get_user_record(config$paths$user_store, auth$email)
      if (is.null(current_user) || !isTRUE(current_user$active[1])) {
        perform_logout()
      } else {
        auth$role <- current_user$role[1]
        auth$partner_name <- current_user$partner_name[1]
        settings_token(get_effective_token(config$paths$user_tokens, auth$email, auth$partner_name, auth$role))
        admin_form_id(get_admin_form_id(config$paths$admin_settings))
      }
      showNotification("Backup restored.", type = "message", duration = 6)
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "error", duration = 8)
    })
  })

  observeEvent(input$admin_back, {
    current_step("upload")
  })

  observeEvent(input$upload_file, {
    req(input$upload_file)
    file_info <- input$upload_file

    # Enforce max file size (10 MB)
    max_bytes <- 10 * 1024^2
    if (is.null(file_info$size) || file_info$size > max_bytes) {
      upload_error("Upload rejected: file too large. Maximum allowed size is 10 MB.")
      upload_df(NULL)
      return()
    }

    # Validate extension
    fname <- file_info$name
    ext <- tolower(tools::file_ext(fname))
    allowed_exts <- c("xlsx", "xls", "csv")
    if (!(ext %in% allowed_exts)) {
      upload_error("Upload rejected: invalid file type. Accepted types: xlsx, xls, csv.")
      upload_df(NULL)
      return()
    }

    # Validate mime when available (best-effort)
    mime <- tolower(ifelse(is.null(file_info$type), "", file_info$type))
    allowed_mimes <- c("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", "application/vnd.ms-excel", "text/csv", "text/plain")
    if (nzchar(mime) && !(mime %in% allowed_mimes)) {
      # allow if extension matches but warn
      upload_error(paste0("Upload rejected: unexpected MIME type (", mime, "). Accepted: xlsx, xls, csv."))
      upload_df(NULL)
      return()
    }

    # Attempt to read file depending on extension, with strict checks
    df <- NULL
    read_error <- NULL
    tryCatch({
      if (ext == "csv") {
        # read CSV with readr if available, fallback to base
        if (requireNamespace("readr", quietly = TRUE)) {
          df <- readr::read_csv(file_info$datapath, show_col_types = FALSE)
        } else {
          df <- utils::read.csv(file_info$datapath, stringsAsFactors = FALSE, check.names = FALSE)
        }
      } else {
        df <- readxl::read_excel(file_info$datapath)
      }
    }, error = function(e) {
      read_error <<- paste0("Unable to read uploaded file: ", conditionMessage(e))
    })

    if (!is.null(read_error) || is.null(df)) {
      upload_error(ifelse(is.null(read_error), "Uploaded file could not be read or is empty.", read_error))
      upload_df(NULL)
      return()
    }

    # Validate column names: must be present and non-empty
    cn <- names(df)
    if (is.null(cn) || length(cn) == 0 || any(is.na(cn)) || any(!nzchar(as.character(cn)))) {
      upload_error("Upload rejected: spreadsheet must contain non-empty column headers.")
      upload_df(NULL)
      return()
    }

    # Validation: require between 10 and 20 columns
    ncols <- ncol(df)
    if (ncols < 10 || ncols > 20) {
      upload_error(paste0("Upload rejected: spreadsheet has ", ncols, " columns; must have between 10 and 20 columns."))
      upload_df(NULL)
      return()
    }

    # Passed validation — clear any previous error and store the dataframe
    upload_error(NULL)
    upload_df(df)
  })

  observeEvent(input$fetch_master, {
    if (!isTRUE(can_fetch_master())) {
      showNotification("Your role cannot fetch the master database.", type = "error")
      return()
    }
    if (!is.null(master_job()) && !is.null(get_job(master_job())) &&
      get_job(master_job())$status %in% c("queued", "running")) {
      showNotification("Master fetch is already running.", type = "message")
      return()
    }
    showModal(modalDialog(
      title = "Fetch master database",
      p("This will fetch the latest master database from ActivityInfo. It may take a couple of minutes depending on your internet connection."),
      footer = tagList(
        actionButton("cancel_fetch_master", "Cancel", class = "btn-secondary"),
        actionButton("confirm_fetch_master", "Start fetch", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$cancel_fetch_master, {
    removeModal()
  })

  observeEvent(input$confirm_fetch_master, {
    if (!isTRUE(can_fetch_master())) {
      showNotification("Your role cannot fetch the master database.", type = "error")
      removeModal()
      return()
    }
    removeModal()
    token <- settings_token()
    if (is.null(token) || !isTRUE(nzchar(token))) {
      master_fetch_status("Fetch failed: ActivityInfo token not set. Add it in Settings.")
      showNotification("ActivityInfo token not set. Add it in Settings.", type = "error")
      return()
    }
    cfg <- config$activityinfo
    cfg$token <- token
    if (isTRUE(nzchar(admin_form_id()))) cfg$form_ids <- admin_form_id()
    fetch_chunk_size <- if (!is.null(cfg$batch_size)) as.integer(cfg$batch_size) else 2000L
    master_fetch_status(paste0("Fetching master database in chunks of ", fetch_chunk_size, " rows..."))
    job_id <- enqueue_master_fetch_job(cfg)
    master_job(job_id)
  })

  output$fetch_status <- renderUI({
    tags$p(style = "color:#475569; margin-top:8px;", master_fetch_status())
  })

  output$fetch_feedback_ui <- renderUI({
    job <- master_job_status()
    if (is.null(job)) {
      return(tags$p(style = "color:#6b7280; margin-top:8px;", "Fetch is idle."))
    }
    progress_value <- suppressWarnings(as.numeric(job$progress))
    if (!is.finite(progress_value)) progress_value <- 0
    job_status_value <- if (!is.null(job$status) && isTRUE(nzchar(job$status))) job$status else "queued"
    job_message_value <- if (!is.null(job$message) && isTRUE(nzchar(job$message))) job$message else "Waiting for fetch status..."

    stage_defs <- list(
      list(label = "Starting", min_progress = 5),
      list(label = "Worker started", min_progress = 6),
      list(label = "Connecting to ActivityInfo", min_progress = 8),
      list(label = "Connection confirmed", min_progress = 12),
      list(label = "Resolving ActivityInfo forms", min_progress = 15),
      list(label = "Fetching forms", min_progress = 18),
      list(label = "Saving snapshot", min_progress = 90),
      list(label = "Finalizing", min_progress = 98),
      list(label = "Completed", min_progress = 100)
    )

    mins <- vapply(stage_defs, function(s) s$min_progress, numeric(1))
    reached <- which(progress_value >= mins)
    current_idx <- if (length(reached) == 0) 1 else max(reached)

    status_style <- function(status) {
      switch(status,
        done = "color:#166534;",
        running = "color:#1d4ed8;",
        failed = "color:#b91c1c;",
        canceled = "color:#b45309;",
        "color:#6b7280;"
      )
    }

    status_label <- function(status) {
      switch(status,
        done = "Done",
        running = "In progress",
        failed = "Failed",
        canceled = "Canceled",
        "Pending"
      )
    }

    tagList(
      tags$div(
        class = "mt-2",
        tags$strong("Fetch feedback"),
        tags$ul(
          lapply(seq_along(stage_defs), function(i) {
            if (job_status_value == "completed") {
              st <- "done"
            } else if (job_status_value == "failed" && i == current_idx) {
              st <- "failed"
            } else if (job_status_value == "canceled" && i == current_idx) {
              st <- "canceled"
            } else if (i < current_idx) {
              st <- "done"
            } else if (i == current_idx && job_status_value %in% c("queued", "running")) {
              st <- "running"
            } else {
              st <- "pending"
            }
            tags$li(
              tags$span(stage_defs[[i]]$label),
              tags$span(
                style = paste0("margin-left:8px;", status_style(st)),
                paste0("[", status_label(st), "]")
              )
            )
          })
        ),
        tags$p(style = "color:#475569; margin-top:8px;", paste("Current:", job_message_value))
      ),
      if (job_status_value == "failed") {
        tags$p(style = "color:#b91c1c; margin-top:8px;", paste("Error:", job_message_value))
      },
      if (job_status_value == "canceled") {
        tags$p(style = "color:#b45309; margin-top:8px;", "Fetch was canceled.")
      }
    )
  })

  output$fetch_progress_ui <- renderUI({
    job <- master_job_status()
    if (is.null(job)) return(NULL)
    progress_value <- suppressWarnings(as.numeric(job$progress))
    if (!is.finite(progress_value)) progress_value <- 0
    job_message_value <- if (!is.null(job$message) && isTRUE(nzchar(job$message))) job$message else "Waiting for fetch status..."
    updated_at <- if (!is.null(job$updated_at)) as.POSIXct(job$updated_at) else NA
    elapsed <- if (!is.na(updated_at)) difftime(Sys.time(), updated_at, units = "secs") else NA
    stale <- !is.na(elapsed) && elapsed > 30
    queued_too_long <- !is.na(elapsed) && elapsed > 15 && identical(job$status, "queued")
    tagList(
      p(job_message_value),
      if (isTRUE(queued_too_long)) p(style = "color:#b91c1c;", "Background worker did not start. Restart RStudio or run fetch again."),
      if (isTRUE(stale)) p(style = "color:#b91c1c;", "No update in the last 30 seconds. The fetch may still be running."),
      div(style = "background: #e2e8f0; height: 8px; border-radius: 6px;",
        div(style = paste0("width:", progress_value, "%; height: 8px; background:#0b1b2b; border-radius: 6px;"))
      )
    )
  })

  output$fetch_log_ui <- renderUI({
    job <- master_job_status()
    if (is.null(job) || is.null(job$history)) return(NULL)
    entries <- rev(tail(job$history, 10))
    tags$div(
      class = "mt-2",
      tags$strong("Fetch log (latest 10)"),
      tags$ul(lapply(entries, tags$li))
    )
  })

  output$cancel_fetch_button <- renderUI({
    job <- master_job_status()
    running <- !is.null(job) && job$status %in% c("queued", "running")
    class <- if (running) "btn-danger ms-2" else "btn-danger ms-2 disabled"
    actionButton("cancel_fetch", "Stop fetch", class = class, disabled = !running)
  })

  observeEvent(input$cancel_fetch, {
    job <- master_job_status()
    if (!is.null(job) && job$status %in% c("queued", "running")) {
      showModal(modalDialog(
        title = "Stop master fetch?",
        p("This will cancel the master data fetch. You can start it again any time."),
        footer = tagList(
          actionButton("cancel_fetch_confirm", "Yes, stop fetch", class = "btn-danger"),
          actionButton("cancel_fetch_abort", "Keep fetching", class = "btn-secondary")
        ),
        easyClose = TRUE
      ))
      return()
    }
    master_fetch_status("Fetch canceled.")
  })

  observeEvent(input$cancel_fetch_confirm, {
    removeModal()
    id <- master_job()
    if (!is.null(id)) {
      set_job_canceled(id, "Master fetch canceled")
      master_fetch_status("Fetch canceled.")
    }
  })

  observeEvent(input$cancel_fetch_abort, removeModal())

  master_timer <- reactiveTimer(1000)
  master_job_status <- reactive({
    id <- master_job()
    if (is.null(id)) return(NULL)
    job <- get_job(id)
    if (is.null(job) || is.null(job$status) || job$status %in% c("queued", "running")) {
      master_timer()
    }
    if (is.null(job)) {
      return(list(
        id = id,
        status = "queued",
        progress = 0,
        message = "Starting master fetch...",
        history = character(0),
        started_at = NA_character_,
        updated_at = NA_character_
      ))
    }
    job
  })

  observeEvent(master_job_status(), {
    job <- master_job_status()
    if (is.null(job)) return()
    if (job$status %in% c("queued", "running")) {
      master_fetch_status(job$message)
    }
    if (job$status == "completed") {
      res <- get_job_result(job)
      if (!is.null(res$snapshot_path)) last_master_snapshot(res$snapshot_path)
      if (!is.null(res$snapshot_path)) last_master_mtime(file.info(res$snapshot_path)$mtime)
      stamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      rows <- if (!is.null(res$rows)) res$rows else 0
      master_fetch_status(paste0("Last fetched: ", stamp, " (", rows, " rows)"))
      if (!identical(last_master_notify(), job$id)) {
        showNotification("Master database fetched successfully.", type = "message")
        last_master_notify(job$id)
      }
    }
    if (job$status == "failed") {
      master_fetch_status(paste("Fetch failed:", job$message))
      if (!identical(last_master_notify(), job$id)) {
        showNotification(job$message, type = "error")
        last_master_notify(job$id)
      }
    }
    if (job$status == "canceled") {
      master_fetch_status("Fetch canceled.")
      if (!identical(last_master_notify(), job$id)) {
        last_master_notify(job$id)
      }
    }
  })

  master_ready <- function() {
    path <- last_master_snapshot()
    !is.null(path) && file.exists(path) && snapshot_is_fresh(path)
  }

  master_block_message <- function() {
    path <- last_master_snapshot()
    if (!is.null(path) && file.exists(path)) {
      stamp <- format(file.info(path)$mtime, "%Y-%m-%d %H:%M:%S")
      return(paste0("Master snapshot is older than 24 hours (", stamp, "). Please fetch a fresh master database before running deduplication."))
    }
    "Master database has not been fetched yet. Click \"Fetch master database\" and confirm it completes successfully before running deduplication."
  }

  observeEvent(input$match_fields, {
    if (!is.null(input$match_fields)) {
      match_fields(input$match_fields)
    }
  })

  required_columns <- reactive({
    selected <- match_fields()
    req(length(selected) > 0)
    cols <- c()
    if ("hoh_ID_number" %in% selected) cols <- c(cols, "hoh_ID_number")
    if ("phone_number" %in% selected) cols <- c(cols, "phone_number")
    if ("partner" %in% selected) cols <- c(cols, "partner")
    if ("hoh_arabic_name" %in% selected) cols <- c(cols, "hoh_arabic_name")
    if ("hoh_spouse_name" %in% selected) cols <- c(cols, "hoh_spouse_name")
    if ("geography" %in% selected) {
      cols <- c(cols, "governorate", "district", "subdistrict", "village")
    }
    unique(cols)
  })

  observeEvent(list(upload_df(), match_fields(), last_master_snapshot()), {
    req(upload_df())
    # Suggestions used to pre-fill mapping (required fields -> upload columns)
    cols <- required_columns()
    suggestions <- map_suggestions(names(upload_df()), cols, config$mapping_min_score)
    mapping_suggestions(suggestions)

    # Suggestions table should show master <-> upload column similarity only
    snap <- last_master_snapshot()
    if (!is.null(snap) && file.exists(snap)) {
      master_df <- tryCatch(readRDS(snap), error = function(e) NULL)
      if (!is.null(master_df)) {
        master_cols <- names(master_df)
        # map_suggestions(upload_cols, required_cols) -> required_column will be master column here
        master_sugg <- map_suggestions(names(upload_df()), master_cols, config$mapping_min_score)
        master_mapping_suggestions(master_sugg)
      } else {
        master_mapping_suggestions(data.frame())
      }
    } else {
      master_mapping_suggestions(data.frame())
    }
  })

  output$upload_preview <- renderDT({
    req(upload_df())
    safe_datatable(head(upload_df(), 10), opts = list(pageLength = 5))
  })

  output$upload_validation <- renderUI({
    msg <- upload_error()
    if (!is.null(msg) && isTRUE(nzchar(msg))) {
      tags$div(style = "color:#b91c1c; margin-top:8px; font-weight:600;", msg)
    } else {
      NULL
    }
  })

  observeEvent(input$confirm_upload, {
    if (is.null(upload_df())) {
      showNotification("Upload a file before continuing.", type = "error")
      return()
    }
    if (!master_ready()) {
      showNotification(master_block_message(), type = "error", duration = 8)
      return()
    }
    current_step("mapping")
  })

  output$mapping_ui <- renderUI({
    req(upload_df())
    cols <- names(upload_df())
    suggestions <- mapping_suggestions()
    default_map <- pick_best_mapping(suggestions)

    tagList(lapply(required_columns(), function(req_col) {
      sel_val <- ""
      if (!is.null(default_map) && length(default_map) > 0 && !is.null(names(default_map)) && req_col %in% names(default_map)) {
        sel_val <- default_map[[req_col]]
        if (is.null(sel_val)) sel_val <- ""
      }
      selectInput(paste0("map_", req_col), paste0("Map ", req_col), choices = c("", cols), selected = sel_val)
    }))
  })

  output$mapping_table <- renderDT({
    # Show master <-> upload suggestions in the suggestions table. If none available, show upload-only suggestions as fallback.
    master_sugg <- master_mapping_suggestions()
    if (!is.null(master_sugg) && nrow(master_sugg) > 0) {
      return(safe_datatable(master_sugg, opts = list(pageLength = 10)))
    }
    req(mapping_suggestions())
    safe_datatable(mapping_suggestions(), opts = list(pageLength = 10))
  })

  observeEvent(input$confirm_mapping, {
    req(upload_df())
    selected <- input$match_fields
    if (is.null(selected) || length(selected) == 0) {
      showNotification("Select at least one field to match.", type = "error")
      return()
    }
    if (!any(c("hoh_ID_number", "phone_number", "hoh_arabic_name", "geography") %in% selected)) {
      showNotification("Select ID, phone, name, or geography for blocking.", type = "error")
      return()
    }
    match_fields(selected)
    mapping <- list()
    for (req_col in required_columns()) {
      input_id <- paste0("map_", req_col)
      selected <- input[[input_id]]
      if (!req_col %in% names(upload_df()) && (is.null(selected) || selected == "")) {
        showNotification(paste0("Missing required column: ", req_col), type = "error")
        return()
      }
      if (!is.null(selected) && selected != "") mapping[[req_col]] <- selected
    }
    current_step("strategy")
  })

  observeEvent(input$run_match, {
    req(upload_df())
    mapping <- list()
    selected <- input$match_fields
    if (is.null(selected) || length(selected) == 0) {
      showNotification("Select at least one field to match.", type = "error")
      return()
    }
    if (!any(c("hoh_ID_number", "phone_number", "hoh_arabic_name", "geography") %in% selected)) {
      showNotification("Select ID, phone, name, or geography for blocking.", type = "error")
      return()
    }
    if (!master_ready()) {
      showNotification(master_block_message(), type = "error", duration = 8)
      return()
    }
    match_fields(selected)
    for (req_col in required_columns()) {
      input_id <- paste0("map_", req_col)
      selected <- input[[input_id]]
      if (!req_col %in% names(upload_df()) && (is.null(selected) || selected == "")) {
        showNotification(paste0("Missing required column: ", req_col), type = "error")
        return()
      }
      if (!is.null(selected) && selected != "") mapping[[req_col]] <- selected
    }

    snapshot_path <- last_master_snapshot()
    if (is.null(snapshot_path) || !file.exists(snapshot_path)) {
      showNotification("No cached master snapshot found. Please fetch the master database first.", type = "error", duration = 8)
      return()
    }
    job_id <- enqueue_match_job(
      upload_df(),
      snapshot_path = snapshot_path,
      mapping = mapping,
      upload_filename = if (!is.null(input$upload_file$name)) input$upload_file$name else "uploaded_file.xlsx",
      upload_time = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      fuzzy_high_threshold = fuzzy_high_threshold(),
      fuzzy_medium_threshold = fuzzy_medium_threshold(),
      weights = config$weights,
      match_fields = match_fields(),
      max_candidates = max_candidates()
    )
    current_job(job_id)
    current_step("matching")
  })

  observeEvent(input$confirm_strategy, {
    high <- as.numeric(input$threshold_high)
    medium <- as.numeric(input$threshold_medium)
    max_cand <- as.numeric(input$max_candidates)

    if (is.na(high) || is.na(medium) || medium >= high || high > 100 || medium < 0) {
      showNotification("Set valid thresholds where high > medium and both are within 0-100.", type = "error")
      return()
    }
    if (is.na(max_cand) || max_cand < 1000) {
      showNotification("Set max candidate pairs to at least 1000.", type = "error")
      return()
    }

    fuzzy_high_threshold(high)
    fuzzy_medium_threshold(medium)
    max_candidates(as.integer(max_cand))
    current_step("matching")
  })

  # Breadcrumb navigation: replaces individual "Back" buttons. When a job is running,
  # navigation is disabled and the cancel button is used to interrupt.
  output$breadcrumb_nav <- renderUI({
    steps <- list(upload = "Upload", mapping = "Map", strategy = "Configure", matching = "Match", results = "Results")
    cur <- current_step()
    job <- job_status()
    running <- !is.null(job) && job$status %in% c("queued", "running")
    cur_idx <- match(cur, names(steps))

    tags$div(class = "breadcrumb-nav", lapply(seq_along(steps), function(i) {
      step_key <- names(steps)[i]
      label <- steps[[i]]
      # Defensive check: only allow navigation if cur_idx is valid and the job is not running
      can_navigate <- !is.na(cur_idx) && i <= cur_idx && !isTRUE(running)
      el <- if (step_key == cur) {
        tags$span(tags$strong(label))
      } else if (can_navigate) {
        actionLink(paste0("crumb_", step_key), label)
      } else {
        tags$span(style = "color:#9ca3af;", label)
      }
      if (i < length(steps)) list(el, tags$span(" » ")) else el
    }))
  })

  observeEvent(input$crumb_upload, {
    job <- job_status()
    if (!is.null(job) && job$status %in% c("queued", "running")) {
      showNotification("Cannot navigate while matching is running.", type = "message")
      return()
    }
    current_step("upload")
  })

  observeEvent(input$crumb_mapping, {
    job <- job_status()
    if (!is.null(job) && job$status %in% c("queued", "running")) {
      showNotification("Cannot navigate while matching is running.", type = "message")
      return()
    }
    current_step("mapping")
  })

  observeEvent(input$crumb_strategy, {
    job <- job_status()
    if (!is.null(job) && job$status %in% c("queued", "running")) {
      showNotification("Cannot navigate while matching is running.", type = "message")
      return()
    }
    current_step("strategy")
  })

  observeEvent(input$crumb_matching, {
    job <- job_status()
    if (!is.null(job) && job$status %in% c("queued", "running")) {
      showNotification("Cannot navigate while matching is running.", type = "message")
      return()
    }
    current_step("matching")
  })

  observeEvent(input$crumb_results, {
    job <- job_status()
    if (!is.null(job) && job$status %in% c("queued", "running")) {
      showNotification("Cannot navigate while matching is running.", type = "message")
      return()
    }
    current_step("results")
  })

  job_timer <- reactiveTimer(1000)
  job_status <- reactive({
    id <- current_job()
    if (is.null(id)) return(NULL)
    job <- get_job(id)
    if (!is.null(job) && job$status %in% c("queued", "running")) {
      job_timer()
      job <- get_job(id)
    }
    job
  })

  output$progress_ui <- renderUI({
    job <- job_status()
    if (is.null(job)) return(p("No job running."))
    updated_at <- if (!is.null(job$updated_at)) as.POSIXct(job$updated_at) else NA
    elapsed <- if (!is.na(updated_at)) difftime(Sys.time(), updated_at, units = "secs") else NA
    stale <- !is.na(elapsed) && elapsed > 30
    tagList(
      p(job$message),
      if (isTRUE(stale)) p(style = "color:#b91c1c;", "No update in the last 30 seconds. The job may still be running."),
      div(style = "background: #e2e8f0; height: 10px; border-radius: 6px;",
        div(style = paste0("width:", job$progress, "%; height: 10px; background:#0f172a; border-radius: 6px;"))
      )
    )
  })

  output$matching_feedback_ui <- renderUI({
    job <- job_status()
    if (is.null(job)) {
      return(tags$p(style = "color:#6b7280; margin-top:8px;", "Matching has not started yet."))
    }

    stage_defs <- list(
      list(label = "Starting", min_progress = 5),
      list(label = "Worker started", min_progress = 8),
      list(label = "Loading local master snapshot", min_progress = 10),
      list(label = "Loading upload data", min_progress = 30),
      list(label = "Preparing upload", min_progress = 40),
      list(label = "Matching records", min_progress = 70),
      list(label = "Finalizing", min_progress = 90),
      list(label = "Completed", min_progress = 100)
    )

    mins <- vapply(stage_defs, function(s) s$min_progress, numeric(1))
    reached <- which(job$progress >= mins)
    current_idx <- if (length(reached) == 0) 1 else max(reached)

    status_style <- function(status) {
      switch(status,
        done = "color:#166534;",
        running = "color:#1d4ed8;",
        "color:#6b7280;"
      )
    }

    status_label <- function(status) {
      switch(status,
        done = "Done",
        running = "In progress",
        "Pending"
      )
    }

    tagList(
      tags$div(
        class = "mt-3",
        tags$strong("Matching feedback"),
        tags$ul(
          lapply(seq_along(stage_defs), function(i) {
            if (job$status == "completed") {
              st <- "done"
            } else if (i < current_idx) {
              st <- "done"
            } else if (i == current_idx && job$status %in% c("queued", "running")) {
              st <- "running"
            } else {
              st <- "pending"
            }
            tags$li(
              tags$span(stage_defs[[i]]$label),
              tags$span(
                style = paste0("margin-left:8px;", status_style(st)),
                paste0("[", status_label(st), "]")
              )
            )
          })
        ),
        tags$p(style = "color:#475569; margin-top:8px;", paste("Current:", job$message))
      ),
      if (job$status == "failed") {
        tags$p(style = "color:#b91c1c; margin-top:8px;", paste("Error:", job$message))
      },
      if (job$status == "canceled") {
        tags$p(style = "color:#b45309; margin-top:8px;", "Matching was canceled.")
      }
    )
  })

  output$cancel_button <- renderUI({
    job <- job_status()
    running <- !is.null(job) && job$status %in% c("queued", "running")
    class <- if (running) "btn-danger ms-2" else "btn-danger ms-2 disabled"
    actionButton("cancel_job", "Stop & start over", class = class, disabled = !running)
  })

  output$status_ui <- renderUI({
    job <- job_status()
    if (is.null(job)) return(NULL)
    started_at <- if (!is.null(job$started_at)) job$started_at else "unknown"
    updated_at <- if (!is.null(job$updated_at)) job$updated_at else "unknown"
    history <- if (!is.null(job$history)) rev(tail(job$history, 4)) else character(0)
    tagList(
      p(paste("Status:", job$status)),
      p(paste("Started:", started_at)),
      p(paste("Last update:", updated_at)),
      if (length(history) > 0) {
        tags$div(
          class = "mt-2",
          tags$strong("Recent events"),
          tags$ul(lapply(history, tags$li))
        )
      }
    )
  })

  output$results_table <- renderDT({
    job <- job_status()
    req(job)
    if (job$status != "completed") {
      return(safe_datatable(data.frame(), opts = list(pageLength = 5)))
    }
    res <- get_job_result(job)
    if (is.list(res) && !is.null(res$summary)) {
      return(safe_datatable(res$summary, opts = list(pageLength = 5)))
    }
    safe_datatable(res, opts = list(pageLength = 10))
  })

  output$export_button <- renderUI({
    job <- job_status()
    ready <- !is.null(job) && job$status == "completed"
    class <- if (ready) "btn-secondary" else "btn-secondary disabled"
    downloadButton("export_results", "Export results", class = class, disabled = !ready)
  })

  output$export_status_ui <- renderUI({
    job <- job_status()
    if (is.null(job) || job$status != "completed") {
      return(tags$p(style = "color:#6b7280; margin-top:8px;", "Export will be enabled once matching completes."))
    }
    status <- export_status()
    if (!isTRUE(nzchar(status))) return(NULL)
    tags$p(style = "color:#475569; margin-top:8px;", status)
  })

  output$export_results <- downloadHandler(
    filename = function() {
      paste0("dedup_results_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      job <- job_status()
      req(!is.null(job) && job$status == "completed")
      if (isTRUE(export_in_progress())) {
        showNotification("Export already in progress. Please wait...", type = "message", duration = 6)
        return()
      }
      export_in_progress(TRUE)
      export_status("Preparing export...")
      on.exit({
        export_in_progress(FALSE)
        export_status("")
        session$sendCustomMessage("export_ready", list())
      }, add = TRUE)
      res <- get_job_result(job)
      tryCatch({
        withProgress(message = "Preparing export...", value = 0, {
          incProgress(0.35, detail = "Collecting results")
          write_dedup_workbook(res, file)
          incProgress(1, detail = "Done")
        })
        showNotification("Export ready. Download should begin shortly.", type = "message", duration = 6)
      }, error = function(e) {
        msg <- conditionMessage(e)
        if (grepl("memory allocation error", msg, ignore.case = TRUE)) {
          showNotification("Export failed due to memory limits. Try reducing match fields or max candidates, or export in smaller batches.", type = "error", duration = 10)
        } else {
          showNotification(paste("Export failed:", msg), type = "error", duration = 8)
        }
        stop(e)
      })
    }
  )

  observeEvent(job_status(), {
    job <- job_status()
    if (is.null(job)) return()
    if (job$status == "completed") current_step("results")
    if (job$status == "canceled") current_step("upload")
    if (job$status == "failed" && !identical(last_job_notify(), job$id)) {
      showNotification(job$message, type = "error")
      last_job_notify(job$id)
    }
  })

  output$step_label <- renderUI({
    step <- current_step()
    labels <- c(
      upload = "Step 1 of 5",
      mapping = "Step 2 of 5",
      strategy = "Step 3 of 5",
      matching = "Step 4 of 5",
      results = "Step 5 of 5",
      settings = "Settings",
      admin = "User Administration"
    )
    # Defensive: ensure step is a valid name
    if (is.null(step) || !is.character(step) || length(step) != 1 || !(step %in% names(labels))) {
      step <- "upload"
    }
    tags$span(labels[[step]])
  })

  output$step_ui <- renderUI({
    step <- current_step()
    valid_steps <- c("upload", "mapping", "strategy", "matching", "results", "settings", "admin")
    if (is.null(step) || !is.character(step) || length(step) != 1 || !(step %in% valid_steps)) {
      step <- "upload"
    }
    switch(step,
      upload = upload_step_ui(can_fetch_master = isTRUE(can_fetch_master())),
      mapping = mapping_step_ui(),
      strategy = strategy_step_ui(),
      matching = matching_step_ui(),
      settings = settings_step_ui(
        can_edit_token = isTRUE(can_edit_token()),
        can_edit_form_id = isTRUE(can_edit_form_id())
      ),
      admin = admin_step_ui(),
      results = results_step_ui()
    )
  })
}

shinyApp(ui, server)
