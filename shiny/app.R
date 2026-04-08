rm(list = ls())

library(dplyr)
library(bslib)
library(DT)
library(readxl)
library(openxlsx)
library(shiny)
library(promises)
library(future)

setwd("D:/OneDrive/02. Projects/06_deduplication_app_R/shiny/")

plan(multisession)

# Log Shiny server errors to tmp/shiny_error.log for diagnostics
if (!dir.exists("tmp")) dir.create("tmp", recursive = TRUE)
options(shiny.error = function(e = NULL, ...) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  if (is.null(e)) {
    # Shiny may call the handler without passing the condition. Fall back to geterrmessage().
    msg_body <- paste0("unknown error; geterrmessage: ", geterrmessage())
    extra <- ""
  } else {
    msg_body <- conditionMessage(e)
    extra <- paste(capture.output(print(e)), collapse = "\n")
  }
  msg <- paste0(ts, " — ", msg_body, "\n", extra, "\n")
  cat(msg, file = file.path(getwd(), "tmp", "shiny_error.log"), append = TRUE)
})

source("R/config.R")
source("R/auth.R")
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
    $(document).on('keydown', function(e) {
      if (e.key !== 'Enter') return;
      if ($('.modal:visible').length === 0) return;
      var loginBtn = $('#login_submit:visible');
      if (loginBtn.length) {
        loginBtn.click();
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
  auth <- reactiveValues(logged_in = FALSE, role = NULL, email = NULL)
  upload_df <- reactiveVal(NULL)
  # upload_error holds validation messages related to the uploaded file
  upload_error <- reactiveVal(NULL)
  mapping_suggestions <- reactiveVal(data.frame())
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
  fuzzy_high_threshold <- reactiveVal(config$thresholds$high)
  fuzzy_medium_threshold <- reactiveVal(config$thresholds$medium)
  age_tolerance <- reactiveVal(config$age_tolerance)
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

  # Helper: safe wrapper around DT::datatable to prevent crashes when DT internals fail
  safe_datatable <- function(df, opts = list(pageLength = 5)) {
    tryCatch({
      DT::datatable(df, options = opts)
    }, error = function(e) {
      msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " — datatable render error: ", conditionMessage(e), "\n")
      if (!dir.exists("tmp")) dir.create("tmp", recursive = TRUE)
      cat(msg, file = file.path(getwd(), "tmp", "shiny_error.log"), append = TRUE)
      DT::datatable(data.frame(Error = conditionMessage(e)), options = list(pageLength = 5))
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

  output$app_ui <- renderUI({
    if (!auth$logged_in) {
      login_ui(config$app_name)
    } else {
      main_ui(config$app_name, show_admin = auth$role == "admin")
    }
  })

  output$app_title <- renderUI({
    name <- settings_username()
    if (is.null(name) || !nzchar(name)) return(tags$span(config$app_name))
    tags$span(paste0(config$app_name, " - ", name))
  })

  observeEvent(input$open_login, {
    showModal(modalDialog(
      title = "Log in",
      size = "m",
      div(
        class = "login-modal-form",
        textInput("login_email", "Email"),
        passwordInput("login_password", "Password")
      ),
      footer = div(
        class = "login-modal-actions",
        actionButton("close_login", "Cancel", class = "btn-secondary"),
        actionButton("login_submit", "Log in", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$close_login, removeModal())

  observeEvent(input$login_submit, {
    email <- trimws(tolower(isolate(input$login_email)))
    password <- isolate(input$login_password)
    if (!nzchar(email) || !nzchar(password)) {
      showNotification("Enter email and password.", type = "error")
      return()
    }
    res <- authenticate_user(email, password, config$paths$user_store)
    if (isTRUE(res$ok)) {
      auth$logged_in <- TRUE
      auth$email <- res$user$email
      auth$role <- res$user$role
      settings_username(res$user$email)
      settings_token(get_user_token(config$paths$user_tokens, res$user$email))
      admin_form_id(get_admin_form_id(config$paths$admin_settings))
      removeModal()
    } else {
      showNotification(res$error, type = "error")
    }
  })

  perform_logout <- function() {
    auth$logged_in <- FALSE
    auth$email <- NULL
    auth$role <- NULL
    current_job(NULL)
    upload_df(NULL)
    current_step("upload")
    settings_username("")
    settings_token("")
    admin_form_id("")
  }

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
    updateTextInput(session, "settings_token", value = settings_token())
    if (auth$role == "admin") {
      updateTextInput(session, "settings_form_id", value = admin_form_id())
    }
    current_step("settings")
  })

  observeEvent(input$close_settings, {
    current_step("upload")
  })

  observeEvent(input$save_settings, {
    settings_username(input$settings_username)
    if (!is.null(input$settings_token) && nzchar(input$settings_token)) {
      settings_token(input$settings_token)
      if (!is.null(auth$email) && nzchar(auth$email)) {
        save_user_token(config$paths$user_tokens, auth$email, input$settings_token)
      }
    }
    if (auth$role == "admin") {
      form_id <- input$settings_form_id
      if (!is.null(form_id) && nzchar(form_id)) {
        admin_form_id(form_id)
        save_admin_settings(config$paths$admin_settings, form_id)
      }
    }
    showNotification("Settings saved for this session.", type = "message")
    current_step("upload")
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
    showNotification("Admin workspace is not enabled yet.", type = "message")
  })

  observeEvent(input$upload_file, {
    req(input$upload_file)
    # Attempt to read the uploaded Excel file and validate column count
    df <- NULL
    read_error <- NULL
    tryCatch({
      df <- readxl::read_excel(input$upload_file$datapath)
    }, error = function(e) {
      read_error <<- paste0("Unable to read Excel file: ", e$message)
    })

    if (!is.null(read_error)) {
      upload_error(read_error)
      upload_df(NULL)
      return()
    }

    if (is.null(df)) {
      upload_error("Uploaded file could not be read or is empty.")
      upload_df(NULL)
      return()
    }

    ncols <- ncol(df)
    if (is.null(ncols) || ncols == 0) {
      upload_error("Uploaded file has no columns. Please provide a valid Excel file.")
      upload_df(NULL)
      return()
    }

    # Validation: require between 10 and 20 columns
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
    if (!is.null(master_job()) && !is.null(get_job(master_job())) &&
      get_job(master_job())$status %in% c("queued", "running")) {
      showNotification("Master fetch is already running.", type = "message")
      return()
    }
    token <- settings_token()
    if (is.null(token) || !nzchar(token)) {
      master_fetch_status("Fetch failed: ActivityInfo token not set. Add it in Settings.")
      showNotification("ActivityInfo token not set. Add it in Settings.", type = "error")
      return()
    }
    cfg <- config$activityinfo
    cfg$token <- token
    if (nzchar(admin_form_id())) cfg$form_ids <- admin_form_id()
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
    reached <- which(job$progress >= mins)
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
            if (job$status == "completed") {
              st <- "done"
            } else if (job$status == "failed" && i == current_idx) {
              st <- "failed"
            } else if (job$status == "canceled" && i == current_idx) {
              st <- "canceled"
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
        tags$p(style = "color:#b45309; margin-top:8px;", "Fetch was canceled.")
      }
    )
  })

  output$fetch_progress_ui <- renderUI({
    job <- master_job_status()
    if (is.null(job)) return(NULL)
    updated_at <- if (!is.null(job$updated_at)) as.POSIXct(job$updated_at) else NA
    elapsed <- if (!is.na(updated_at)) difftime(Sys.time(), updated_at, units = "secs") else NA
    stale <- !is.na(elapsed) && elapsed > 30
    queued_too_long <- !is.na(elapsed) && elapsed > 15 && identical(job$status, "queued")
    tagList(
      p(job$message),
      if (isTRUE(queued_too_long)) p(style = "color:#b91c1c;", "Background worker did not start. Restart RStudio or run fetch again."),
      if (isTRUE(stale)) p(style = "color:#b91c1c;", "No update in the last 30 seconds. The fetch may still be running."),
      div(style = "background: #e2e8f0; height: 8px; border-radius: 6px;",
        div(style = paste0("width:", job$progress, "%; height: 8px; background:#0b1b2b; border-radius: 6px;"))
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
    if (!is.null(job) && job$status %in% c("queued", "running")) {
      master_timer()
      job <- get_job(id)
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

  observeEvent(list(upload_df(), match_fields()), {
    req(upload_df())
    cols <- required_columns()
    suggestions <- map_suggestions(names(upload_df()), cols, config$mapping_min_score)
    mapping_suggestions(suggestions)
  })

  output$upload_preview <- renderDT({
    req(upload_df())
    safe_datatable(head(upload_df(), 10), opts = list(pageLength = 5))
  })

  output$upload_validation <- renderUI({
    msg <- upload_error()
    if (!is.null(msg) && nzchar(msg)) {
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
      selected <- if (!is.null(default_map[[req_col]])) default_map[[req_col]] else ""
      selectInput(paste0("map_", req_col), paste0("Map ", req_col), choices = c("", cols), selected = selected)
    }))
  })

  output$mapping_table <- renderDT({
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
      age_tolerance = age_tolerance(),
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
    age_tol <- as.numeric(input$age_tolerance)
    max_cand <- as.numeric(input$max_candidates)

    if (is.na(high) || is.na(medium) || medium >= high || high > 100 || medium < 0) {
      showNotification("Set valid thresholds where high > medium and both are within 0-100.", type = "error")
      return()
    }
    if (is.na(age_tol) || age_tol < 0) {
      showNotification("Set a valid non-negative age tolerance.", type = "error")
      return()
    }
    if (is.na(max_cand) || max_cand < 1000) {
      showNotification("Set max candidate pairs to at least 1000.", type = "error")
      return()
    }

    fuzzy_high_threshold(high)
    fuzzy_medium_threshold(medium)
    age_tolerance(age_tol)
    max_candidates(as.integer(max_cand))
    current_step("matching")
  })

  observeEvent(input$back_to_strategy, {
    job <- job_status()
    if (!is.null(job) && job$status %in% c("queued", "running")) {
      showNotification("Cannot go back while matching is running.", type = "message")
      return()
    }
    current_step("strategy")
  })

  observeEvent(input$back_to_upload, {
    # Allow user to navigate back to the upload step from mapping
    job <- job_status()
    if (!is.null(job) && job$status %in% c("queued", "running")) {
      showNotification("Cannot go back while matching is running.", type = "message")
      return()
    }
    # Keep uploaded data intact so user can re-map or replace file
    current_step("upload")
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
    if (!nzchar(status)) return(NULL)
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
      settings = "Settings"
    )
    tags$span(labels[[step]])
  })

  output$step_ui <- renderUI({
    step <- current_step()
    if (step == "upload") return(upload_step_ui())
    if (step == "mapping") return(mapping_step_ui())
    if (step == "strategy") return(strategy_step_ui())
    if (step == "matching") return(matching_step_ui())
    if (step == "settings") return(settings_step_ui(show_admin = auth$role == "admin"))
    results_step_ui()
  })
}

shinyApp(ui, server)