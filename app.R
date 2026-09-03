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
  primary = "#52B32D",
  secondary = "#BFDCA8",
  success = "#0F8B8D",
  base_font = "Inter",
  heading_font = "Inter",
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
    // Adjust DataTables columns when changing tabs to prevent header misalignment
    $(document).on('shown.bs.tab', 'a[data-bs-toggle=\"tab\"]', function (e) {
      if ($.fn.dataTable) {
        $.fn.dataTable.tables({ visible: true, api: true }).columns.adjust();
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
    // Immediate visual feedback when Proceed is clicked in the confirmation modal
    $(document).on('click', '#confirm_start_matching_btn', function() {
      var $btn = $(this);
      if ($btn.hasClass('disabled') || $btn.prop('disabled')) return;
      $btn.addClass('disabled btn-matching-running').prop('disabled', true);
      $btn.html('<span class=\"spinner-border spinner-border-sm me-2\" role=\"status\" aria-hidden=\"true\"></span>Starting deduplication...');
      $('#matching_feedback_status').html('<span class=\"matching-pulse-hint\"><span class=\"spinner-grow spinner-grow-sm\" role=\"status\" aria-hidden=\"true\"></span> Deduplication job initialized in background...</span>');
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
  filter_recent_mpca <- reactiveVal(FALSE)
  mpca_window_months <- reactiveVal(6)
  match_fields <- reactiveVal(c(
    "partner",
    "hoh_ID_number",
    "phone_number",
    "hoh_arabic_name",
    "hoh_spouse_name",
    "geography"
  ))
  last_job_notify <- reactiveVal(NULL)
  current_lang <- reactiveVal("en")
  presets_trigger <- reactiveVal(0)
  triage_records <- reactiveValues()
  triage_update_trigger <- reactiveVal(0)
  active_modal_pair_id <- reactiveVal(NULL)
  
  # MFA temp state: pending user after password verification and email OTP cache
  mfa_pending <- reactiveVal(NULL) # list(email=..., user=...)
  mfa_email_codes <- reactiveVal(list())

  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0) y else x
  }

  # Helper: safe wrapper around DT::datatable to prevent crashes when DT internals fail
  safe_datatable <- function(df, opts = list(pageLength = 5), selection = "none", ...) {
    tryCatch({
      # For large data, enable client-side performance helpers (deferRender) instead of server flag
      is_large <- !is.null(df) && is.data.frame(df) && nrow(df) > 500
      if (is_large) {
        opts <- modifyList(opts, list(pageLength = 10, deferRender = TRUE))
      }
      DT::datatable(df, options = opts, rownames = FALSE, selection = selection, ...)
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

  output$master_freshness_pill <- renderUI({
    snap <- last_master_snapshot()
    if (!is.null(snap) && file.exists(snap)) {
      fi <- file.info(snap)
      diff_hrs <- round(as.numeric(difftime(Sys.time(), fi$mtime, units = "hours")), 1)
      time_txt <- if (diff_hrs < 0.1) "Just now" else if (diff_hrs < 1) paste(round(diff_hrs * 60), "m ago") else if (diff_hrs < 24) paste(diff_hrs, "h ago") else paste(round(diff_hrs / 24, 1), "d ago")
      tags$div(
        class = "status-pill status-pill-ready",
        title = paste("Master snapshot cached on disk:", format(fi$mtime, "%Y-%m-%d %H:%M:%S")),
        tags$span(paste0("🟢 Master DB Synced: ", time_txt, " (Offline Ready)"))
      )
    } else {
      tags$div(
        class = "status-pill status-pill-warn",
        title = "No master database snapshot is cached on this server.",
        tags$span("🟡 Master DB: Snapshot Needed")
      )
    }
  })

  output$topbar_actions <- renderUI({
    step <- current_step()
    is_settings_or_admin <- step %in% c("settings", "admin")
    show_settings <- !identical(normalize_role(auth$role), "partner_deduplicator")
    show_admin <- isTRUE(can_open_admin_workspace())
    
    tagList(
      actionLink("toggle_lang", uiOutput("lang_toggle_btn_ui"), class = "lang-toggle-btn me-2"),
      if (is_settings_or_admin) {
        actionButton("topbar_back_workflow", "Back to workflow", class = "btn-ghost")
      } else {
        tagList(
          if (show_settings) actionButton("open_settings", "Settings", class = "btn-ghost") else NULL,
          if (show_admin) actionButton("admin_open", admin_button_label(), class = "btn-ghost") else NULL
        )
      },
      actionButton("logout", "Log out", class = "btn-danger")
    )
  })

  output$lang_toggle_btn_ui <- renderUI({
    if (identical(current_lang(), "en")) "عربي (AR)" else "English (EN)"
  })

  observeEvent(input$toggle_lang, {
    current_lang(if (identical(current_lang(), "en")) "ar" else "en")
    showNotification(
      if (identical(current_lang(), "ar")) "تم تفعيل اللغة العربية والمصطلحات المزدوجة" else "English language mode active",
      type = "message"
    )
  })
  
  observeEvent(input$topbar_back_workflow, {
    current_step("upload") # Or whatever the logic is to go back to workflow. Wait, we should restore previous step if possible.
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
          modalButton("Cancel"),
          actionButton("login_submit", "Log in", class = "btn-primary")
        )
      ),
      easyClose = TRUE,
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
    
    msg <- "Are you sure you want to log out?"
    
    if (!is.null(job) && job$status %in% c("queued", "running")) {
      msg <- "A deduplication job is still running. Logging out will cancel it."
    } else if (!is.null(fetch_job) && fetch_job$status %in% c("queued", "running")) {
      msg <- "Master data is still being fetched. Logging out will cancel it."
    }
    
    showModal(modalDialog(
      size = "s",
      div(style = "padding: 16px;",
        tags$h4("Confirm Log Out", style = "margin-top:0; margin-bottom:16px; color:var(--app-forest); font-weight:600;"),
        p(msg, style = "margin-bottom: 0;")
      ),
      footer = div(style = "display:flex; gap:8px; justify-content:flex-end;",
        modalButton("Cancel"),
        actionButton("logout_confirm_general", "Log out", class = "btn-danger")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$logout_confirm_general, {
    removeModal()
    # Cancel any running jobs
    id <- current_job()
    if (!is.null(id)) set_job_canceled(id)
    
    fid <- master_job()
    if (!is.null(fid)) set_job_canceled(fid, "Master fetch canceled")
    
    perform_logout()
  })

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

  observeEvent(input$close_settings_body, {
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
        style = "display:flex; gap:10px; margin-top:12px; margin-bottom:16px; flex-wrap:wrap;",
        actionButton("admin_save_user", "💾 Save User", class = "btn-primary"),
        actionButton("admin_clear_user_form", "🔄 Clear Form", class = "btn-secondary"),
        actionButton("admin_toggle_user", "⚡ Toggle Active", class = "btn-ghost"),
        actionButton("admin_delete_user", "🗑️ Delete User", class = "btn-danger")
      ),
      tags$hr(style = "margin: 20px 0; border-color: var(--app-border);"),
      tags$h5(style = "font-weight: 700; color: var(--app-forest); margin-bottom: 8px;", "👥 Managed Users Directory"),
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
      tags$hr(style = "margin: 24px 0; border-color: var(--app-border);"),
      tags$h5(style = "color: var(--app-forest); font-weight: 700; margin-bottom: 4px;", "🏢 Partner Organization Registry"),
      tags$p(style = "color: #64748B; font-size: 0.85rem; margin-bottom: 16px;", "CCY Master administrators can manage the partner organization directory used for partner scopes and automated mapping presets."),
      layout_columns(
        col_widths = c(6, 6),
        tags$div(
          class = "app-card",
          style = "padding: 16px; border: 1px solid var(--app-border); border-radius: 6px; background: #FFFFFF;",
          tags$strong(style = "font-size: 0.85rem; color: var(--app-forest);", "➕ Register New Partner Organization"),
          tags$div(
            style = "margin-top: 10px;",
            textInput("partner_name_new", "Partner Organization Name", placeholder = "e.g. ACF, DRC, NRC, SCI, CARE", width = "100%"),
            actionButton("partner_name_add", "➕ Add Partner to Registry", class = "btn-primary btn-sm mt-2")
          )
        ),
        tags$div(
          class = "app-card",
          style = "padding: 16px; border: 1px solid var(--app-border); border-radius: 6px; background: #FFFFFF;",
          tags$strong(style = "font-size: 0.85rem; color: #DC2626;", "🗑️ Remove Existing Partner"),
          tags$div(
            style = "margin-top: 10px;",
            selectInput("partner_name_remove", "Select Partner to Remove", choices = partners, selected = if (length(partners) >= 1) partners[1] else character(0), selectize = FALSE, width = "100%"),
            actionButton("partner_name_remove_btn", "🗑️ Remove Selected Partner", class = "btn-danger btn-sm mt-2")
          )
        )
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

  output$download_template <- downloadHandler(
    filename = function() { "Deduplication_Template.xlsx" },
    content = function(file) {
      template_df <- data.frame(
        partner = character(),
        hoh_ID_number = character(),
        phone_number = character(),
        hoh_arabic_name = character(),
        hoh_spouse_name = character(),
        geography = character(),
        stringsAsFactors = FALSE
      )
      openxlsx::write.xlsx(template_df, file)
    }
  )

  observeEvent(input$upload_file, {
    req(input$upload_file)
    file_info <- input$upload_file

    # Enforce max file size (25 MB)
    max_bytes <- 25 * 1024^2
    if (is.null(file_info$size) || file_info$size > max_bytes) {
      upload_error("Upload rejected: file exceeds maximum allowed size of 25 MB.")
      upload_df(NULL)
      return()
    }

    # Validate file extension
    fname <- file_info$name
    ext <- tolower(tools::file_ext(fname))
    allowed_exts <- c("xlsx", "xls", "csv")
    if (!(ext %in% allowed_exts)) {
      upload_error(paste0("Upload rejected: unsupported file format '.", ext, "'. Accepted formats are .xlsx, .xls, and .csv."))
      upload_df(NULL)
      return()
    }

    # Attempt to read file
    df <- NULL
    read_error <- NULL
    tryCatch({
      if (ext == "csv") {
        if (requireNamespace("readr", quietly = TRUE)) {
          df <- as.data.frame(readr::read_csv(file_info$datapath, show_col_types = FALSE, guess_max = 5000))
        } else {
          df <- utils::read.csv(file_info$datapath, stringsAsFactors = FALSE, check.names = FALSE)
        }
      } else {
        df <- as.data.frame(readxl::read_excel(file_info$datapath))
      }
    }, error = function(e) {
      read_error <<- paste0("Could not parse file structure: ", conditionMessage(e))
    })

    if (!is.null(read_error) || is.null(df)) {
      upload_error(ifelse(is.null(read_error), "Uploaded spreadsheet could not be read or is empty.", read_error))
      upload_df(NULL)
      return()
    }

    # Validate column names: must be non-empty
    cn <- names(df)
    if (is.null(cn) || length(cn) == 0 || any(is.na(cn)) || any(!nzchar(trimws(as.character(cn))))) {
      upload_error("Upload rejected: spreadsheet contains missing or blank column headers. Please ensure every column has a header title.")
      upload_df(NULL)
      return()
    }

    # Validation: must contain at least 2 columns and 1 row
    ncols <- ncol(df)
    nrows <- nrow(df)
    if (ncols < 2) {
      upload_error(paste0("Upload rejected: spreadsheet contains only ", ncols, " column. Deduplication requires at least 2 columns (e.g., Name, Phone, ID)."))
      upload_df(NULL)
      return()
    }
    if (nrows < 1) {
      upload_error("Upload rejected: spreadsheet contains column headers but zero data rows.")
      upload_df(NULL)
      return()
    }

    # Passed validation — clear any error and store dataframe
    upload_error(NULL)
    upload_df(df)
    showNotification(paste0("Spreadsheet verified: ", format(nrows, big.mark = ","), " records loaded successfully."), type = "message", duration = 4)
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
    class <- if (running) "btn-danger" else "btn-danger disabled"
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

    # Suggestions table should show master <-> upload column similarity only
    snap <- last_master_snapshot()
    if (!is.null(snap) && file.exists(snap)) {
      master_df <- tryCatch(readRDS(snap), error = function(e) NULL)
      if (!is.null(master_df)) {
        master_cols <- names(master_df)
        # map_suggestions(upload_cols, required_cols) -> required_column will be master column here
        } else {
        }
    } else {
    }
  })

  output$upload_data_health_and_preview_ui <- renderUI({
    err <- upload_error()
    if (!is.null(err) && isTRUE(nzchar(err))) {
      return(
        tags$div(
          class = "empty-state-card",
          style = "border: 2px dashed #EF4444; background: #FEF2F2; padding: 28px 20px;",
          tags$div(class = "empty-state-icon", tags$span(style = "font-size: 2.6rem; color: #DC2626;", "⚠️")),
          tags$h5(style = "color: #991B1B; font-weight: 700; margin-top: 6px;", "Spreadsheet Verification Issue"),
          tags$p(style = "color: #7F1D1D; font-size: 0.88rem; max-width: 55ch; margin-bottom: 16px; font-weight: 500;", err),
          tags$div(
            class = "empty-state-features",
            tags$div(class = "feature-pill", style = "background:#FFF; color:#991B1B; border-color:#FCA5A5;", "💡 Tip: Ensure file has clear header columns (e.g., Name, Phone, ID, District)"),
            tags$div(class = "feature-pill", style = "background:#FFF; color:#991B1B; border-color:#FCA5A5;", "💡 Tip: Supported file extensions are .xlsx, .xls, and .csv"),
            tags$div(class = "feature-pill", style = "background:#FFF; color:#991B1B; border-color:#FCA5A5;", "💡 Tip: Download and compare against the CCY standard template on the left")
          )
        )
      )
    }

    df <- upload_df()
    if (is.null(df) || nrow(df) == 0) {
      return(
        tags$div(
          class = "empty-state-card",
          tags$div(class = "empty-state-icon", tags$span(style = "font-size: 2.2rem; color: var(--app-sea);", "📁")),
          tags$h5("No spreadsheet uploaded yet"),
          tags$p("Upload an Excel (.xlsx/.xls) or CSV partner list on the left to verify record health and preview rows."),
          tags$div(
            class = "empty-state-features",
            tags$div(class = "feature-pill", tags$strong("⚡ Instant Health Check: "), "Coverage analysis for IDs & phone numbers"),
                        tags$div(class = "feature-pill", tags$strong("🔒 Data Protection: "), "Zero external transmission; processed in-memory")
          )
        )
      )
    }

    n_records <- nrow(df)
    cols <- names(df)
    fname <- if (!is.null(input$upload_file$name)) input$upload_file$name else "Uploaded Spreadsheet"

    # 1. Identify National ID column candidates
    id_col_candidates <- cols[grepl("id|national|nid|identity|card", cols, ignore.case = TRUE)]
    id_col <- if ("hoh_ID_number" %in% cols) "hoh_ID_number" else if (length(id_col_candidates) > 0) id_col_candidates[1] else NULL
    
    id_count <- 0
    id_pct <- 0
    dup_ids <- 0
    if (!is.null(id_col)) {
      id_vals <- trimws(as.character(df[[id_col]]))
      id_valid <- !is.na(id_vals) & nzchar(id_vals) & tolower(id_vals) != "na"
      id_count <- sum(id_valid)
      id_pct <- round(100 * id_count / max(1, n_records), 1)
      dup_ids <- sum(duplicated(id_vals[id_valid]))
    }

    # 2. Identify Phone column candidates
    phone_col_candidates <- cols[grepl("phone|mobile|cell|contact|tel", cols, ignore.case = TRUE)]
    phone_col <- if ("phone_number" %in% cols) "phone_number" else if (length(phone_col_candidates) > 0) phone_col_candidates[1] else NULL
    
    phone_count <- 0
    phone_pct <- 0
    short_phones <- 0
    if (!is.null(phone_col)) {
      phone_vals <- trimws(as.character(df[[phone_col]]))
      phone_valid <- !is.na(phone_vals) & nzchar(phone_vals) & tolower(phone_vals) != "na"
      phone_count <- sum(phone_valid)
      phone_pct <- round(100 * phone_count / max(1, n_records), 1)
      clean_digits <- gsub("[^0-9]", "", phone_vals[phone_valid])
      short_phones <- sum(nchar(clean_digits) > 0 & nchar(clean_digits) < 9)
    }

    tagList(
      tags$div(
        style = "display: flex; justify-content: space-between; align-items: center; padding: 10px 14px; background: #F0FDF4; border: 1px solid #BBF7D0; border-radius: var(--app-radius-xs); margin-bottom: 12px;",
        tags$div(
          tags$strong(style = "color: #166534; font-size: 0.88rem;", paste0("📄 ", fname)),
          tags$span(style = "color: #15803D; font-size: 0.78rem; margin-left: 8px;", paste("(", format(n_records, big.mark = ","), " rows × ", length(cols), " columns)"))
        ),
        tags$span(class = "status-pill status-pill-ready", "✓ File Verified")
      ),

      tags$div(
        class = "health-kpi-grid",
        tags$div(
          class = "health-kpi-chip kpi-good",
          tags$span(class = "kpi-label", "Total Records"),
          tags$span(class = "kpi-value", format(n_records, big.mark = ","))
        ),
        tags$div(
          class = paste("health-kpi-chip", if (id_pct >= 85) "kpi-good" else "kpi-warn"),
          tags$span(class = "kpi-label", "National ID Coverage"),
          tags$span(class = "kpi-value", if (!is.null(id_col)) paste0(id_pct, "%") else "Unmapped")
        ),
        tags$div(
          class = paste("health-kpi-chip", if (phone_pct >= 80) "kpi-good" else "kpi-warn"),
          tags$span(class = "kpi-label", "Phone Coverage"),
          tags$span(class = "kpi-value", if (!is.null(phone_col)) paste0(phone_pct, "%") else "Unmapped")
        ),
        tags$div(
          class = paste("health-kpi-chip", if (dup_ids == 0) "kpi-good" else "kpi-warn"),
          tags$span(class = "kpi-label", "Raw Duplicate IDs"),
          tags$span(class = "kpi-value", if (dup_ids == 0) "0 (Clean)" else paste(dup_ids, "Found"))
        )
      ),

      # Health Alert Banners
      if (dup_ids > 0) {
        tags$div(
          class = "health-alert health-alert-warning",
          tags$strong("⚠️ Warning:"),
          tags$span(paste(dup_ids, "duplicate National ID(s) detected within the raw upload file. Internal duplicates will be identified during matching."))
        )
      },
      if (short_phones > 0) {
        tags$div(
          class = "health-alert health-alert-info",
          tags$strong("ℹ️ Notice:"),
          tags$span(paste(short_phones, "phone number(s) have fewer than 9 digits and will be normalized."))
        )
      },
      if (!is.null(id_col) && id_pct < 80) {
        tags$div(
          class = "health-alert health-alert-warning",
          tags$strong("⚠️ Advisory:"),
          tags$span(paste0("National ID coverage is low (", id_pct, "%). The deduplication engine will prioritize Name and Phone fuzzy matching."))
        )
      },
      if (dup_ids == 0 && (is.null(id_col) || id_pct >= 80) && (is.null(phone_col) || phone_pct >= 80)) {
        tags$div(
          class = "health-alert health-alert-success",
          tags$strong("✓ Quality Check:"),
          tags$span("High field coverage detected. Dataset is healthy and ready for column mapping.")
        )
      },

      tags$div(
        style = "display: flex; justify-content: flex-end; margin-top: 16px;",
        actionButton("confirm_upload_health_btn", "Proceed to Step 2: Confirm Mapping ➔", class = "btn-primary")
      )
    )
  })

  output$upload_validation <- renderUI({
    msg <- upload_error()
    if (!is.null(msg) && isTRUE(nzchar(msg))) {
      tags$div(style = "color:#b91c1c; margin-top:8px; font-weight:600; font-size:0.85rem;", paste("⚠️", msg))
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

  observeEvent(input$confirm_upload_health_btn, {
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

  # Mapping Presets Logic
  all_presets <- reactive({
    presets_trigger()
    load_mapping_presets()
  })

  output$load_preset_ui <- renderUI({
    presets <- all_presets()
    choices <- c("Select Saved Preset..." = "", names(presets))
    selectInput("selected_preset", "Mapping Preset:", choices = choices, selected = "", width = "220px")
  })

  observeEvent(input$selected_preset, {
    preset_name <- input$selected_preset
    req(nzchar(preset_name))
    presets <- all_presets()
    if (!preset_name %in% names(presets)) return()
    map_data <- presets[[preset_name]]
    req_cols <- required_columns()
    for (rc in req_cols) {
      if (rc %in% names(map_data)) {
        updateSelectInput(session, paste0("map_", rc), selected = map_data[[rc]])
      }
    }
    showNotification(paste("Applied mapping preset:", preset_name), type = "message")
  })

  observeEvent(input$save_preset, {
    default_name <- if (!is.null(auth$partner_name) && isTRUE(nzchar(auth$partner_name))) auth$partner_name else ""
    showModal(modalDialog(
      title = "Save Mapping Preset",
      textInput("preset_name_input", "Preset Name (e.g. Partner Name):", value = default_name),
      tags$p(style = "color:#64748B; font-size:0.85rem;", "Saves the current column mapping configuration so it can be quickly auto-loaded for future files from this partner."),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_preset_confirm", "Save Preset", class = "btn-primary")
      )
    ))
  })

  observeEvent(input$save_preset_confirm, {
    name <- trimws(input$preset_name_input)
    if (!nzchar(name)) {
      showNotification("Please provide a preset name.", type = "error")
      return()
    }
    mapping <- list()
    for (req_col in required_columns()) {
      val <- input[[paste0("map_", req_col)]]
      if (!is.null(val) && isTRUE(nzchar(val))) {
        mapping[[req_col]] <- val
      }
    }
    saved <- save_mapping_preset(name, mapping)
    if (isTRUE(saved)) {
      presets_trigger(presets_trigger() + 1)
      removeModal()
      showNotification(paste0("Mapping preset '", name, "' saved successfully."), type = "message")
    } else {
      showNotification("Failed to save preset to file.", type = "error")
    }
  })

  output$mapping_ui <- renderUI({
    req(upload_df())
    cols <- names(upload_df())
    default_map <- list()

    # Smart auto-application: If logged-in partner has a preset, auto-populate from it
    partner <- auth$partner_name
    presets <- all_presets()
    if (!is.null(partner) && isTRUE(nzchar(partner)) && partner %in% names(presets)) {
      default_map <- presets[[partner]]
    }

    render_field_select <- function(req_col) {
      sel_val <- ""
      if (!is.null(default_map) && length(default_map) > 0 && req_col %in% names(default_map)) {
        sel_val <- default_map[[req_col]]
        if (is.null(sel_val)) sel_val <- ""
      }
      selectInput(
        paste0("map_", req_col),
        get_field_bilingual_label(req_col),
        choices = c("", cols),
        selected = sel_val,
        width = "100%"
      )
    }

    groups <- list(
      list(
        id = "grp_identity",
        title = "👤 Personal Identity & Demographics",
        title_ar = "الهوية والبيانات الديموغرافية",
        cols = c("hoh_arabic_name", "hoh_spouse_name", "hoh_ID_number", "id_type", "sex", "age", "marital_status", "household_size")
      ),
      list(
        id = "grp_contact",
        title = "📞 Contact Information",
        title_ar = "بيانات التواصل",
        cols = c("phone_number", "secondary_phone_number")
      ),
      list(
        id = "grp_geo",
        title = "📍 Geographic Hierarchy",
        title_ar = "الموقع الجغرافي",
        cols = c("governorate", "district", "subdistrict", "village")
      ),
      list(
        id = "grp_admin",
        title = "🏛️ Administrative & Project Metadata",
        title_ar = "البيانات الإدارية والمشروع",
        cols = c("partner", "record_id", "qa_code_sn", "system_date", "interviewer", "main_ref", "beneficiary_status", "dist_type", "dist_date_calc_new")
      )
    )

    tagList(
      lapply(groups, function(grp) {
        req_subset <- grp$cols[grp$cols %in% required_columns()]
        if (length(req_subset) == 0) return(NULL)
        
        tags$div(
          class = "mapping-category-card",
          tags$div(
            class = "mapping-category-header",
            tags$span(paste0(grp$title, " (", grp$title_ar, ")")),
            tags$span(class = "category-badge-chip", paste(length(req_subset), "fields"))
          ),
          tags$div(
            class = "mapping-category-body",
            lapply(req_subset, render_field_select)
          )
        )
      })
    )
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
      selected_val <- input[[input_id]]
      if (!req_col %in% names(upload_df()) && (is.null(selected_val) || selected_val == "")) {
        showNotification(paste0("Missing required column mapping: ", req_col), type = "error")
        return()
      }
      if (!is.null(selected_val) && selected_val != "") mapping[[req_col]] <- selected_val
    }

    snapshot_path <- last_master_snapshot()
    if (is.null(snapshot_path) || !file.exists(snapshot_path)) {
      showNotification("No cached master snapshot found. Please fetch the master database first.", type = "error", duration = 8)
      return()
    }

    n_records <- format(nrow(upload_df()), big.mark = ",")
    selected_fields_str <- paste(selected, collapse = ", ")
    high_th <- fuzzy_high_threshold()
    med_th <- fuzzy_medium_threshold()
    max_cand <- max_candidates()

    showModal(modalDialog(
      title = tags$div(
        style = "display: flex; align-items: center; gap: 8px; color: var(--app-forest); font-weight: 700;",
        tags$span(style = "font-size: 1.3rem;", "⚠️"),
        tags$span("Confirm Deduplication Launch (تأكيد بدء المطابقة)")
      ),
      size = "m",
      easyClose = FALSE,
      tags$div(
        tags$div(
          class = "health-alert health-alert-warning mb-3",
          tags$div(
            tags$strong("Resource-Intensive Processing Notice:"),
            tags$p(
              style = "margin: 4px 0 0 0; font-size: 0.825rem;",
              "The deduplication engine executes comprehensive multi-pass phonetic, fuzzy token comparison (Jaro-Winkler & Levenshtein), and spatial blocking across your uploaded list and the central master database. This operation is resource-intensive and processing time depends on dataset size."
            )
          )
        ),
        tags$div(
          style = "background: var(--app-mist); border: 1px solid var(--app-border); border-radius: 6px; padding: 12px 14px; margin-bottom: 12px;",
          tags$strong(style = "font-size: 0.8rem; color: var(--app-forest); text-transform: uppercase; letter-spacing: 0.03em;", "Execution Parameters Summary:"),
          tags$div(
            style = "display: grid; grid-template-columns: 1fr 1fr; gap: 8px; margin-top: 8px; font-size: 0.825rem;",
            tags$div(tags$strong("Upload Size: "), tags$span(paste(n_records, "records"))),
            tags$div(tags$strong("Max Pairs: "), tags$span(paste(max_cand, "candidates"))),
            tags$div(tags$strong("High Threshold: "), tags$span(paste0(high_th, "%"))),
            tags$div(tags$strong("Medium Threshold: "), tags$span(paste0(med_th, "%"))),
            tags$div(
              style = "grid-column: span 2;",
              tags$strong("📅 MPCA Recency Filter: "),
              if (isTRUE(filter_recent_mpca())) {
                tags$span(style = "color: #166534; font-weight: 700;", paste0("Active (< ", mpca_window_months(), " months via Dist_Date_Calc_New)"))
              } else {
                tags$span(style = "color: #64748B;", "Disabled (All Historical Records)")
              }
            )
          ),
          tags$div(
            style = "margin-top: 6px; font-size: 0.825rem;",
            tags$strong("Match Fields: "), tags$span(selected_fields_str)
          )
        ),
        tags$p(
          style = "font-size: 0.8rem; color: #64748B; margin-bottom: 0;",
          "You can halt or cancel a running job at any time using the 'Stop & start over' control on the matching screen."
        )
      ),
      footer = tagList(
        modalButton("Halt & Go Back (إلغاء)"),
        actionButton("confirm_start_matching_btn", "🚀 Proceed & Start Matching (بدء المطابقة)", class = "btn-primary")
      )
    ))
  })

  observeEvent(input$confirm_start_matching_btn, {
    removeModal()
    req(upload_df())
    mapping <- list()
    for (req_col in required_columns()) {
      input_id <- paste0("map_", req_col)
      selected_val <- input[[input_id]]
      if (!is.null(selected_val) && selected_val != "") mapping[[req_col]] <- selected_val
    }

    showNotification("Matching initiated. Preparing dataset...", type = "message", duration = 3)
    snapshot_path <- last_master_snapshot()
    if (is.null(snapshot_path) || !file.exists(snapshot_path)) {
      showNotification("No cached master snapshot found. Please fetch the master database first.", type = "error", duration = 8)
      return()
    }
    detected_partner <- NULL
    if (!is.null(mapping$partner) && mapping$partner %in% names(upload_df())) {
      vals <- na.omit(upload_df()[[mapping$partner]])
      if (length(vals) > 0 && nzchar(trimws(as.character(vals[1])))) detected_partner <- trimws(as.character(vals[1]))
    }
    if (is.null(detected_partner) && "1.1. Organization Prefix" %in% names(upload_df())) {
      vals <- na.omit(upload_df()[["1.1. Organization Prefix"]])
      if (length(vals) > 0 && nzchar(trimws(as.character(vals[1])))) detected_partner <- trimws(as.character(vals[1]))
    }
    partner_org_val <- if (!is.null(detected_partner)) detected_partner else (auth$partner_name %||% NULL)

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
      max_candidates = max_candidates(),
      filter_recent_mpca = isTRUE(filter_recent_mpca()),
      mpca_window_months = mpca_window_months(),
      partner_org = partner_org_val,
      user_role = auth$role
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
    if (is.na(max_cand) || max_cand < 50 || max_cand > 2000) {
      showNotification("Set max candidate pairs between 50 and 2000.", type = "error")
      return()
    }

    fuzzy_high_threshold(high)
    fuzzy_medium_threshold(medium)
    max_candidates(as.integer(max_cand))
    filter_recent_mpca(isTRUE(input$filter_recent_mpca))
    mpca_window_months(as.numeric(input$mpca_window_months %||% 6))
    current_step("matching")
  })

  # Interactive Guided Stepper: replaces simple breadcrumb links with a rich visual stepper.
  # When a job is running, navigation is locked and the cancel button is used to interrupt.
  output$breadcrumb_nav <- renderUI({
    steps <- list(
      upload = list(title = "Upload & Verify", desc = "Data Hygiene"),
      mapping = list(title = "Map Columns", desc = "Field Matching"),
      strategy = list(title = "Configure", desc = "Rules & Thresholds"),
      matching = list(title = "Run Matching", desc = "Pairwise Engine"),
      results = list(title = "Results", desc = "Review & Export")
    )
    cur <- current_step()
    job <- job_status()
    running <- !is.null(job) && job$status %in% c("queued", "running")
    cur_idx <- match(cur, names(steps))

    # If in settings or admin, show clear administration header
    if (is.na(cur_idx)) {
      label <- if (cur == "admin") "User Administration" else "System Settings"
      return(
        tags$div(
          class = "d-flex align-items-center justify-content-between",
          tags$div(
            tags$strong(style = "color: var(--app-forest); font-size: 1rem;", label),
            tags$span(style = "color: #64748B; font-size: 0.85rem; margin-left: 8px;", "— Administration Area")
          ),
          actionButton("close_settings", "← Back to Workflow", class = "btn-secondary btn-sm")
        )
      )
    }

    n_steps <- length(steps)
    tags$ul(
      class = "stepper-progress",
      lapply(seq_along(steps), function(i) {
        step_key <- names(steps)[i]
        step_info <- steps[[i]]
        is_completed <- i < cur_idx
        is_active <- i == cur_idx
        can_navigate <- is_completed && !isTRUE(running)

        status_class <- if (is_active) "active" else if (is_completed) "completed" else "pending"
        badge_text <- if (is_active) "Current" else if (is_completed) "Done" else "Pending"
        indicator_content <- if (is_completed) "✓" else as.character(i)

        content <- tagList(
          tags$span(class = "step-indicator", indicator_content),
          tags$div(
            class = "step-label-group",
            tags$span(class = "step-title-text", step_info$title),
            tags$span(class = "step-badge", badge_text)
          )
        )

        item_el <- if (can_navigate) {
          actionLink(paste0("crumb_", step_key), content, class = "step-btn")
        } else {
          tags$div(class = "step-static", content)
        }

        connector <- if (i < n_steps) {
          tags$div(class = paste("step-connector", if (i < cur_idx) "completed" else ""))
        } else {
          NULL
        }

        tags$li(
          class = paste("stepper-item", status_class),
          item_el,
          connector
        )
      })
    )
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
    if (is.null(job)) {
      filename <- if (!is.null(input$upload_file$name)) input$upload_file$name else "Uploaded dataset"
      n_rows <- if (!is.null(upload_df())) nrow(upload_df()) else 0
      n_fields <- length(match_fields())
      max_pairs <- max_candidates()

      return(
        tags$div(
          class = "pre-match-readiness-card mb-3",
          tags$div(
            class = "readiness-header",
            tags$span(class = paste("readiness-badge", if (master_ready()) "ready" else "pending"), if (master_ready()) "✓ Ready to Run" else "Waiting for Master DB"),
            tags$strong("Matching Pipeline Readiness")
          ),
          tags$div(
            class = "readiness-grid mt-2",
            tags$div(class = "readiness-item", tags$span(class = "item-label", "Master Database:"), tags$span(class = "item-val", if (master_ready()) "Connected & Cached" else "Fetch required in Step 1")),
            tags$div(class = "readiness-item", tags$span(class = "item-label", "Uploaded File:"), tags$span(class = "item-val", paste0(filename, " (", format(n_rows, big.mark = ","), " rows)"))),
            tags$div(class = "readiness-item", tags$span(class = "item-label", "Active Rules:"), tags$span(class = "item-val", paste(n_fields, "match fields"))),
            tags$div(class = "readiness-item", tags$span(class = "item-label", "Candidate Cap:"), tags$span(class = "item-val", paste(format(max_pairs, big.mark = ","), "pairs max")))
          )
        )
      )
    }
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
      return(tags$p(style = "color:#64748B; font-size:0.85rem; margin-top:8px;", "Click 'Run matching' above to begin deduplicating records across the master database and within the upload."))
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

  output$run_match_button_ui <- renderUI({
    job <- job_status()
    running <- !is.null(job) && job$status %in% c("queued", "running")
    completed <- !is.null(job) && job$status == "completed"

    if (running) {
      tags$button(
        id = "run_match",
        type = "button",
        class = "btn btn-primary disabled btn-matching-running",
        disabled = "disabled",
        tags$span(class = "spinner-border spinner-border-sm me-2", role = "status", `aria-hidden` = "true"),
        "Matching in progress..."
      )
    } else if (completed) {
      actionButton("run_match", "Re-run matching", class = "btn-primary")
    } else {
      actionButton("run_match", "Run matching", class = "btn-primary")
    }
  })

  output$cancel_button <- renderUI({
    job <- job_status()
    running <- !is.null(job) && job$status %in% c("queued", "running")
    class <- if (running) "btn-danger" else "btn-danger disabled"
    actionButton("cancel_job", "Stop & start over", class = class, disabled = !running)
  })

  output$status_ui <- renderUI({
    job <- job_status()
    if (is.null(job)) return(NULL)
    started_at <- if (!is.null(job$started_at)) job$started_at else "unknown"
    updated_at <- if (!is.null(job$updated_at)) job$updated_at else "unknown"
    history <- if (!is.null(job$history)) rev(tail(job$history, 4)) else character(0)
    tagList(
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

  # Results Reactive Helpers
  results_data <- reactive({
    job <- job_status()
    req(job)
    if (job$status != "completed") return(NULL)
    get_job_result(job)
  })

  high_conf_raw <- reactive({
    res <- results_data()
    if (is.null(res)) return(data.frame())
    exact <- if (is.data.frame(res$list_vs_master_exact)) res$list_vs_master_exact else data.frame()
    fuzzy_high <- if (is.data.frame(res$list_vs_master_fuzzy) && "confidence" %in% names(res$list_vs_master_fuzzy)) {
      res$list_vs_master_fuzzy[res$list_vs_master_fuzzy$confidence == "high", ]
    } else data.frame()
    
    if (nrow(exact) > 0 && nrow(fuzzy_high) > 0) {
      rbind(exact, fuzzy_high)
    } else if (nrow(exact) > 0) {
      exact
    } else {
      fuzzy_high
    }
  })

  medium_conf_raw <- reactive({
    res <- results_data()
    if (is.null(res) || !is.data.frame(res$list_vs_master_fuzzy) || !"confidence" %in% names(res$list_vs_master_fuzzy)) {
      return(data.frame())
    }
    res$list_vs_master_fuzzy[res$list_vs_master_fuzzy$confidence == "medium", ]
  })

  internal_dups_raw <- reactive({
    res <- results_data()
    if (is.null(res)) return(data.frame())
    sl_exact <- if (is.data.frame(res$same_list_exact)) res$same_list_exact else data.frame()
    sl_fuzzy <- if (is.data.frame(res$same_list_fuzzy)) res$same_list_fuzzy else data.frame()
    if (nrow(sl_exact) > 0 && nrow(sl_fuzzy) > 0) {
      rbind(sl_exact, sl_fuzzy)
    } else if (nrow(sl_exact) > 0) {
      sl_exact
    } else {
      sl_fuzzy
    }
  })

  # Triage Decision Helpers
  get_pair_triage <- function(pair_id) {
    triage_update_trigger()
    dec <- triage_records[[pair_id]]
    if (is.null(dec) || !is.list(dec)) {
      list(status = "Unreviewed", notes = "", reviewer = "", timestamp = NULL)
    } else {
      dec
    }
  }

  set_pair_triage <- function(pair_id, status, notes) {
    reviewer <- if (!is.null(auth$email) && nzchar(auth$email)) auth$email else "MEAL Reviewer"
    triage_records[[pair_id]] <- list(
      status = status,
      notes = notes,
      reviewer = reviewer,
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
    triage_update_trigger(triage_update_trigger() + 1)
  }

  analyze_arabic_name_tokens <- function(name_u, name_m) {
    tokens_u <- unlist(strsplit(trimws(as.character(name_u %||% "")), "\\s+"))
    tokens_m <- unlist(strsplit(trimws(as.character(name_m %||% "")), "\\s+"))
    
    labels <- c("1st (Given / الاسم الأول)", "2nd (Father / اسم الأب)", "3rd (Grandfather / اسم الجد)", "4th (Family / اللقب)")
    max_len <- min(4, max(length(tokens_u), length(tokens_m)))
    if (max_len == 0) return(list())
    
    lapply(seq_len(max_len), function(i) {
      tu <- if (i <= length(tokens_u)) tokens_u[i] else ""
      tm <- if (i <= length(tokens_m)) tokens_m[i] else ""
      is_match <- nzchar(tu) && nzchar(tm) && (tu == tm || stringdist::stringsim(tu, tm, method = "jw") >= 0.85)
      list(
        part = if (i <= length(labels)) labels[i] else paste0("Part ", i),
        val_u = tu,
        val_m = tm,
        match = is_match
      )
    })
  }

  # Modal Inspector for Candidate Comparison
  show_candidate_diff_modal <- function(row_data, is_internal = FALSE) {
    if (is.null(row_data) || nrow(row_data) == 0) return()
    
    pair_id <- if ("match_pair_id" %in% names(row_data)) as.character(row_data$match_pair_id[1]) else "Match Detail"
    active_modal_pair_id(pair_id)
    score <- if ("match_score" %in% names(row_data)) as.numeric(row_data$match_score[1]) else NA
    conf <- if ("confidence" %in% names(row_data)) as.character(row_data$confidence[1]) else "medium"
    factors <- if ("contributing_factors" %in% names(row_data)) as.character(row_data$contributing_factors[1]) else ""
    cur_triage <- get_pair_triage(pair_id)
    
    # 1. Arabic Name 4-Part Analysis
    name_u <- if (is_internal) as.character(row_data$upload_hoh_arabic_name_a[1] %||% "") else as.character(row_data$upload_hoh_arabic_name[1] %||% "")
    name_m <- if (is_internal) as.character(row_data$upload_hoh_arabic_name_b[1] %||% "") else as.character(row_data$master_hoh_arabic_name[1] %||% "")
    name_tokens <- analyze_arabic_name_tokens(name_u, name_m)
    
    token_cards_html <- if (length(name_tokens) > 0) {
      tags$div(
        class = "name-token-container",
        tags$div(class = "name-token-header", "🔍 Arabic 4-Part Name Token Analysis:"),
        tags$div(
          class = "name-token-grid",
          lapply(name_tokens, function(tok) {
            card_class <- if (tok$match) "token-card token-match" else "token-card token-diff"
            status_symbol <- if (tok$match) "✓ Match" else "≠ Diff"
            tags$div(
              class = card_class,
              tags$span(class = "token-part-label", paste(tok$part, "-", status_symbol)),
              tags$div(class = "token-val-pair", paste0("U: ", if (nzchar(tok$val_u)) tok$val_u else "—")),
              tags$div(class = "token-val-pair", paste0("M: ", if (nzchar(tok$val_m)) tok$val_m else "—"))
            )
          })
        )
      )
    } else {
      NULL
    }

    # 2. Field Comparison Table
    fields_to_compare <- list(
      list(label = "Head of Household Name (اسم رب الأسرة)", u = "hoh_arabic_name", m = "hoh_arabic_name"),
      list(label = "Spouse Name (اسم الزوج / الزوجة)", u = "hoh_spouse_name", m = "hoh_spouse_name"),
      list(label = "National ID Number (رقم الهوية)", u = "hoh_ID_number", m = "hoh_ID_number"),
      list(label = "ID Type (نوع الهوية)", u = "id_type", m = "id_type"),
      list(label = "Primary Phone (الهاتف الأساسي)", u = "phone_number", m = "primary_phone_number"),
      list(label = "Secondary Phone (الهاتف الثانوي)", u = "secondary_phone_number", m = "secondary_phone_number"),
      list(label = "Partner (المنظمة الشريكة)", u = "partner", m = "organization"),
      list(label = "Governorate (المحافظة)", u = "governorate", m = "governorate"),
      list(label = "District (المديرية)", u = "district", m = "district"),
      list(label = "Subdistrict (العزلة / الحي)", u = "subdistrict", m = "sub_district"),
      list(label = "Village (القرية)", u = "village", m = "village"),
      list(label = "Sex / Gender (النوع)", u = "sex", m = "hoh_sex"),
      list(label = "Age (العمر)", u = "age", m = "hoh_age"),
      list(label = "Household Size (حجم الأسرة)", u = "household_size", m = "household_size"),
      list(label = "Last MPCA Distribution Date (تاريخ آخر توزيع)", u = "dist_date_calc_new", m = "dist_date_calc_new")
    )
    
    rows_html <- lapply(fields_to_compare, function(f) {
      if (is_internal) {
        val_a <- if (paste0("upload_", f$u, "_a") %in% names(row_data)) as.character(row_data[[paste0("upload_", f$u, "_a")]][1]) else ""
        val_b <- if (paste0("upload_", f$m, "_b") %in% names(row_data)) as.character(row_data[[paste0("upload_", f$m, "_b")]][1]) else ""
      } else {
        val_a <- if (paste0("upload_", f$u) %in% names(row_data)) as.character(row_data[[paste0("upload_", f$u)]][1]) else ""
        m_col <- paste0("master_", f$m)
        val_b <- if (m_col %in% names(row_data)) {
          as.character(row_data[[m_col]][1])
        } else if (paste0("master_", f$u) %in% names(row_data)) {
          as.character(row_data[[paste0("master_", f$u)]][1])
        } else ""
      }
      if (is.na(val_a)) val_a <- ""
      if (is.na(val_b)) val_b <- ""
      
      clean_a <- trimws(tolower(val_a))
      clean_b <- trimws(tolower(val_b))
      
      has_data <- nzchar(clean_a) || nzchar(clean_b)
      if (!has_data) return(NULL)
      
      is_match <- nzchar(clean_a) && nzchar(clean_b) && clean_a == clean_b
      row_class <- if (is_match) "diff-row-match" else "diff-row-mismatch"
      tag_el <- if (is_match) tags$span(class = "diff-tag-match", "✓ Match") else tags$span(class = "diff-tag-mismatch", "≠ Diff")
      
      tags$tr(
        class = row_class,
        tags$td(tags$strong(f$label)),
        tags$td(if (nzchar(val_a)) val_a else tags$span(style = "color:#94a3b8; font-style:italic;", "Blank")),
        tags$td(if (nzchar(val_b)) val_b else tags$span(style = "color:#94a3b8; font-style:italic;", "Blank")),
        tags$td(tag_el)
      )
    })
    
    col_a_header <- if (is_internal) "Uploaded Record (Row A)" else "Uploaded Record (Partner File)"
    col_b_header <- if (is_internal) "Uploaded Record (Row B)" else "Master Database Record"
    
    badge_class <- if (conf == "high") "badge-conf-high" else "badge-conf-medium"
    badge_text <- if (conf == "high") paste0("🚨 High Confidence (", score, "%)") else paste0("🔍 Medium Review (", score, "%)")
    
    audit_snippet_text <- paste0(
      "[MEAL AUDIT] Pair #", pair_id,
      " | Score: ", score, "%",
      " | Upload: \"", name_u, "\"",
      " | Master: \"", name_m, "\"",
      " | Location: ", row_data$upload_governorate[1] %||% "", "/", row_data$upload_district[1] %||% "",
      " | Decision: ", cur_triage$status,
      " | Verified by: ", if (nzchar(cur_triage$reviewer)) cur_triage$reviewer else "MEAL Reviewer"
    )

    partner_u <- if (is_internal) {
      as.character(row_data$upload_partner_a[1] %||% "")
    } else {
      as.character(row_data$upload_partner[1] %||% row_data[["upload_1.1. Organization Prefix"]][1] %||% auth$partner_name %||% "")
    }
    partner_m <- if (is_internal) as.character(row_data$upload_partner_b[1] %||% "") else as.character(row_data$master_organization[1] %||% "")
    
    is_cross_agency <- !is_internal && nzchar(partner_m) && nzchar(partner_u) && tolower(trimws(partner_m)) != tolower(trimws(partner_u))
    
    cross_agency_banner <- if (is_cross_agency) {
      tags$div(
        class = "health-alert health-alert-warning mb-3",
        style = "display: flex; align-items: flex-start; gap: 10px; border-left: 4px solid #D97706;",
        tags$span(style = "font-size: 1.4rem; line-height: 1;", "🏢"),
        tags$div(
          tags$strong(paste0("Cross-Agency Overlap: ", toupper(partner_m))),
          tags$p(
            style = "margin: 3px 0 0 0; font-size: 0.8rem;",
            paste0("This household is registered in the central database by ", toupper(partner_m), 
                   ". In accordance with CCY Consortium SOPs, coordinate with the ", toupper(partner_m), 
                   " MEAL/IM focal point before disbursing cash assistance.")
          )
        )
      )
    } else if (!is_internal && nzchar(partner_m)) {
      tags$div(
        class = "health-alert health-alert-info mb-3",
        style = "display: flex; align-items: flex-start; gap: 10px; border-left: 4px solid #0F766E;",
        tags$span(style = "font-size: 1.4rem; line-height: 1;", "🏢"),
        tags$div(
          tags$strong(paste0("Same-Agency Historical Record: ", toupper(partner_m))),
          tags$p(style = "margin: 3px 0 0 0; font-size: 0.8rem;", "This record matches a previous registration within your own organization.")
        )
      )
    } else {
      NULL
    }

    showModal(modalDialog(
      title = tags$div(
        class = "diff-modal-header",
        tags$div(
          tags$h5(style = "margin:0; font-weight:700; color:var(--app-forest);", paste("Candidate Comparator — Pair:", pair_id)),
          tags$span(style = "font-size:0.8rem; color:#64748B;", if (is_internal) "Internal Duplicate Comparison (Same List)" else "Partner Upload vs Central Master Database")
        ),
        tags$span(class = badge_class, badge_text)
      ),
      size = "l",
      easyClose = TRUE,
      tags$div(
        cross_agency_banner,
        if (nzchar(factors)) {
          tags$div(
            class = "diff-factors-bar",
            tags$strong("Scoring Factors: "),
            tags$span(factors)
          )
        },
        token_cards_html,
        tags$div(
          style = "overflow-x:auto; border:1px solid var(--app-border); border-radius:4px;",
          tags$table(
            class = "diff-table",
            tags$thead(
              tags$tr(
                tags$th(style = "width:30%;", "Attribute / Field"),
                tags$th(style = "width:32%;", col_a_header),
                tags$th(style = "width:32%;", col_b_header),
                tags$th(style = "width:6%;", "Status")
              )
            ),
            tags$tbody(rows_html)
          )
        ),

        # 3. Interactive Triage Workspace
        tags$div(
          class = "triage-workspace-bar",
          tags$strong(style = "color:var(--app-forest); font-size:0.85rem;", "⚡ IM / MEAL Verification & Triage Decision:"),
          tags$div(
            style = "display:grid; grid-template-columns: 1fr 1fr; gap: 12px; margin-top: 8px;",
            selectInput(
              "triage_status_choice",
              "Set Status:",
              choices = c(
                "Unreviewed" = "Unreviewed",
                "Confirmed Duplicate (مؤكد مكرر)" = "Confirmed Duplicate",
                "False Positive / Whitelist (تطابق خاطئ)" = "False Positive",
                "Needs Field Verification (يحتاج تحقق ميداني)" = "Needs Field Verification"
              ),
              selected = cur_triage$status,
              width = "100%"
            ),
            textInput(
              "triage_notes_choice",
              "Reviewer Notes / Justification:",
              value = cur_triage$notes,
              placeholder = "e.g. Field verification conducted on 2026-08...",
              width = "100%"
            )
          ),
          tags$div(
            style = "display:flex; justify-content:space-between; align-items:center; margin-top: 8px;",
            tags$span(style = "font-size:0.75rem; color:#64748B;", if (nzchar(cur_triage$reviewer)) paste("Last updated by:", cur_triage$reviewer, "at", cur_triage$timestamp) else "No previous triage record"),
            actionButton("save_triage_decision_btn", "💾 Save Triage Decision", class = "btn-primary btn-sm")
          )
        ),

        # 4. MEAL Audit Snippet
        tags$div(
          class = "audit-snippet-container",
          tags$span("📋 MEAL Audit Trail Snippet:"),
          tags$div(style = "margin-top:4px; word-break:break-all;", audit_snippet_text)
        )
      ),
      footer = tagList(
        modalButton("Close Inspector")
      )
    ))
  }

  observeEvent(input$save_triage_decision_btn, {
    pair_id <- active_modal_pair_id()
    req(pair_id)
    new_status <- input$triage_status_choice
    new_notes <- trimws(input$triage_notes_choice %||% "")
    set_pair_triage(pair_id, new_status, new_notes)
    showNotification(paste0("Triage decision saved for pair: ", pair_id), type = "message")
  })

  output$results_dossier_ui <- renderUI({
    job <- job_status()
    if (is.null(job) || job$status != "completed") {
      return(
        tags$div(
          class = "empty-state-card",
          tags$div(class = "empty-state-icon", tags$span(style = "font-size: 2.2rem; color: var(--app-forest);", "📊")),
          tags$h5("No matching results available"),
          tags$p("Matching has not been run yet. Return to Step 4 and click 'Run matching' to generate duplicate analysis.")
        )
      )
    }

    n_upload <- if (!is.null(upload_df())) nrow(upload_df()) else 0
    n_high <- nrow(high_conf_raw())
    n_med <- nrow(medium_conf_raw())
    n_internal <- nrow(internal_dups_raw())
    total_dups <- n_high + n_med
    dedup_rate <- if (n_upload > 0) round(100 * total_dups / n_upload, 1) else 0

    triage_update_trigger()
    all_triages <- reactiveValuesToList(triage_records)
    n_triaged <- sum(vapply(all_triages, function(x) !is.null(x$status) && x$status != "Unreviewed", logical(1)))

    tagList(
      tabsetPanel(
        id = "results_tabset",
        tabPanel(
          "📊 Executive Summary",
          tags$div(
            class = "health-kpi-grid mt-3 mb-3",
            tags$div(
              class = "health-kpi-chip kpi-good",
              tags$span(class = "kpi-label", "Upload Records Examined"),
              tags$span(class = "kpi-value", format(n_upload, big.mark = ","))
            ),
            tags$div(
              class = paste("health-kpi-chip", if (n_high > 0) "kpi-warn" else "kpi-good"),
              tags$span(class = "kpi-label", "High Confidence Duplicates"),
              tags$span(class = "kpi-value", format(n_high, big.mark = ","))
            ),
            tags$div(
              class = paste("health-kpi-chip", if (n_med > 0) "kpi-warn" else "kpi-good"),
              tags$span(class = "kpi-label", "Medium Review Queue"),
              tags$span(class = "kpi-value", format(n_med, big.mark = ","))
            ),
            tags$div(
              class = paste("health-kpi-chip", if (n_internal > 0) "kpi-warn" else "kpi-good"),
              tags$span(class = "kpi-label", "Internal Same-List Duplicates"),
              tags$span(class = "kpi-value", format(n_internal, big.mark = ","))
            ),
            tags$div(
              class = "health-kpi-chip kpi-good",
              tags$span(class = "kpi-label", "Deduplication Rate"),
              tags$span(class = "kpi-value", paste0(dedup_rate, "%"))
            )
          ),
          tags$div(
            style = "margin-top: 16px;",
            tags$strong(style = "font-size:0.85rem; color:var(--app-forest);", "Deduplication Metrics Table:"),
            DT::DTOutput("results_summary_dt")
          )
        ),
        tabPanel(
          paste0("🚨 High Confidence (", n_high, ")"),
          tags$div(
            class = "health-alert health-alert-warning mt-3 mb-3",
            tags$strong("🚨 High-Confidence Matches:"),
            tags$span("Records below have high similarity or exact national ID/phone matches with ActivityInfo. Click any row to inspect field-by-field differences and record triage decisions.")
          ),
          DT::DTOutput("results_high_dt")
        ),
        tabPanel(
          paste0("🔍 Medium Review (", n_med, ")"),
          tags$div(
            class = "health-alert health-alert-info mt-3 mb-3",
            tags$strong("🔍 Medium Review Queue:"),
            tags$span("Candidate pairs with moderate similarity flagged for MEAL verification. Click any row to inspect field-by-field differences and record triage decisions.")
          ),
          DT::DTOutput("results_medium_dt")
        ),
        tabPanel(
          paste0("📋 Internal Duplicates (", n_internal, ")"),
          tags$div(
            class = "health-alert health-alert-info mt-3 mb-3",
            tags$strong("📋 Internal Duplicates:"),
            tags$span("Duplicate records identified internally within the uploaded spreadsheet. Click any row to inspect field-by-field differences and record triage decisions.")
          ),
          DT::DTOutput("results_internal_dt")
        )
      )
    )
  })

  output$results_summary_dt <- renderDT({
    res <- results_data()
    req(res)
    if (is.list(res) && !is.null(res$summary)) {
      return(safe_datatable(res$summary, opts = list(pageLength = 8)))
    }
    safe_datatable(data.frame())
  })

  output$results_high_dt <- renderDT({
    df <- high_conf_raw()
    triage_update_trigger()
    if (nrow(df) == 0) {
      return(safe_datatable(data.frame("Status" = "No high-confidence duplicate records detected.")))
    }
    
    triage_statuses <- vapply(df$match_pair_id, function(pid) {
      get_pair_triage(as.character(pid))$status
    }, character(1))

    master_phones <- if ("master_primary_phone_number" %in% names(df)) df$master_primary_phone_number else (df$master_phone_number %||% "")
    master_orgs <- if ("master_organization" %in% names(df)) df$master_organization else ""

    display <- data.frame(
      "Pair ID" = df$match_pair_id,
      "Triage Status" = triage_statuses,
      "Score" = paste0(df$match_score, "%"),
      "Agency Overlap" = ifelse(nzchar(master_orgs), toupper(master_orgs), "Central Master"),
      "Upload Name" = df$upload_hoh_arabic_name %||% "",
      "Master Name" = df$master_hoh_arabic_name %||% "",
      "Upload Phone" = df$upload_phone_number %||% "",
      "Master Phone" = master_phones,
      "Upload ID" = df$upload_hoh_ID_number %||% "",
      "Master ID" = df$master_hoh_ID_number %||% "",
      "Location" = paste0(df$upload_governorate %||% "", " / ", df$upload_district %||% ""),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    safe_datatable(display, opts = list(pageLength = 8, scrollX = TRUE), selection = "single")
  })

  observeEvent(input$results_high_dt_rows_selected, {
    idx <- input$results_high_dt_rows_selected
    req(length(idx) == 1)
    df <- high_conf_raw()
    req(nrow(df) >= idx)
    show_candidate_diff_modal(df[idx, , drop = FALSE], is_internal = FALSE)
  })

  output$results_medium_dt <- renderDT({
    df <- medium_conf_raw()
    triage_update_trigger()
    if (nrow(df) == 0) {
      return(safe_datatable(data.frame("Status" = "No medium-confidence candidate pairs flagged for review.")))
    }
    
    triage_statuses <- vapply(df$match_pair_id, function(pid) {
      get_pair_triage(as.character(pid))$status
    }, character(1))

    master_phones <- if ("master_primary_phone_number" %in% names(df)) df$master_primary_phone_number else (df$master_phone_number %||% "")
    master_orgs <- if ("master_organization" %in% names(df)) df$master_organization else ""

    display <- data.frame(
      "Pair ID" = df$match_pair_id,
      "Triage Status" = triage_statuses,
      "Score" = paste0(df$match_score, "%"),
      "Agency Overlap" = ifelse(nzchar(master_orgs), toupper(master_orgs), "Central Master"),
      "Upload Name" = df$upload_hoh_arabic_name %||% "",
      "Master Name" = df$master_hoh_arabic_name %||% "",
      "Upload Phone" = df$upload_phone_number %||% "",
      "Master Phone" = master_phones,
      "Upload ID" = df$upload_hoh_ID_number %||% "",
      "Master ID" = df$master_hoh_ID_number %||% "",
      "Location" = paste0(df$upload_governorate %||% "", " / ", df$upload_district %||% ""),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    safe_datatable(display, opts = list(pageLength = 8, scrollX = TRUE), selection = "single")
  })

  observeEvent(input$results_medium_dt_rows_selected, {
    idx <- input$results_medium_dt_rows_selected
    req(length(idx) == 1)
    df <- medium_conf_raw()
    req(nrow(df) >= idx)
    show_candidate_diff_modal(df[idx, , drop = FALSE], is_internal = FALSE)
  })

  output$results_internal_dt <- renderDT({
    df <- internal_dups_raw()
    triage_update_trigger()
    if (nrow(df) == 0) {
      return(safe_datatable(data.frame("Status" = "No internal duplicates detected within the uploaded spreadsheet.")))
    }
    
    triage_statuses <- vapply(df$match_pair_id, function(pid) {
      get_pair_triage(as.character(pid))$status
    }, character(1))

    display <- data.frame(
      "Pair ID" = df$match_pair_id,
      "Triage Status" = triage_statuses,
      "Score" = paste0(df$match_score, "%"),
      "Confidence" = toupper(df$confidence %||% "medium"),
      "Name (Row A)" = df$upload_hoh_arabic_name_a %||% "",
      "Name (Row B)" = df$upload_hoh_arabic_name_b %||% "",
      "Phone (Row A)" = df$upload_phone_number_a %||% "",
      "Phone (Row B)" = df$upload_phone_number_b %||% "",
      "ID (Row A)" = df$upload_hoh_ID_number_a %||% "",
      "ID (Row B)" = df$upload_hoh_ID_number_b %||% "",
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    safe_datatable(display, opts = list(pageLength = 8, scrollX = TRUE), selection = "single")
  })

  observeEvent(input$results_internal_dt_rows_selected, {
    idx <- input$results_internal_dt_rows_selected
    req(length(idx) == 1)
    df <- internal_dups_raw()
    req(nrow(df) >= idx)
    show_candidate_diff_modal(df[idx, , drop = FALSE], is_internal = TRUE)
  })

  output$results_export_summary_ui <- renderUI({
    job <- job_status()
    if (is.null(job) || job$status != "completed") {
      return(tags$p(style = "color:#64748B; font-size:0.85rem;", "Results summary will be generated once matching is completed."))
    }
    filename <- if (!is.null(input$upload_file$name)) input$upload_file$name else "Uploaded dataset"
    n_high <- nrow(high_conf_raw())
    n_med <- nrow(medium_conf_raw())
    n_internal <- nrow(internal_dups_raw())

    tags$div(
      tags$p(style = "font-size: 0.85rem; margin-bottom: 6px;", tags$strong("File: "), filename),
      tags$p(style = "font-size: 0.85rem; margin-bottom: 6px;", tags$strong("Completed: "), format(Sys.time(), "%Y-%m-%d %H:%M")),
      tags$p(style = "font-size: 0.85rem; margin-bottom: 6px;", tags$strong("Total Identified: "), paste(n_high + n_med + n_internal, "potential duplicates")),
      tags$p(style = "font-size: 0.78rem; color:#64748B;", "The exported Excel dossier contains all executive summary metrics, high confidence pairs, medium review queue, internal duplicates, and run metadata.")
    )
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
          triage_list <- reactiveValuesToList(triage_records)
          write_dedup_workbook(res, file, triage_decisions = triage_list)
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

  observeEvent(input$restart_dedup_btn, {
    upload_df(NULL)
    upload_error(NULL)
    current_job(NULL)
    filter_recent_mpca(FALSE)
    mpca_window_months(6)
    export_status("")
    export_in_progress(FALSE)
    active_modal_pair_id(NULL)
    current_step("upload")
    showNotification("Workflow reset. Ready for a new deduplication run.", type = "message")
  })

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
