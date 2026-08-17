login_ui <- function(app_name) {
  div(
    class = "login-screen",
    div(
      class = "hero app-card p-4",
      h1(app_name),
      p(
        class = "hero-sub",
        "Review partner records, auto-map fields, and confirm duplicates in minutes."
      ),
      div(
        class = "hero-actions",
        actionButton("open_login", "Log in", class = "btn-primary"),
        tags$a("See workflow", href = "#workflow", class = "btn-secondary")
      ),
      div(
        id = "workflow",
        class = "callout mt-4 text-start",
        tags$strong("Three steps, one run:"),
        tags$ul(
          tags$li("Upload the partner extract."),
          tags$li("Confirm suggested field mapping."),
          tags$li("Run matching and export results.")
        )
      )
    ),
    div(
      class = "footer",
      tags$a("Contact Support", href = "mailto:hamzaabdullahmoh@gmail.com"),
      tags$span(" | "),
      tags$a("Privacy", href = "mailto:hamzaabdullahmoh@gmail.com"),
      tags$span(" | "),
      tags$a("Terms", href = "mailto:hamzaabdullahmoh@gmail.com")
    )
  )
}

main_ui <- function(app_name, show_admin = FALSE, admin_label = "Admin", show_settings = TRUE) {
  tagList(
    div(
      class = "app-topbar",
      div(class = "app-title", uiOutput("app_title")),
      div(
        class = "app-actions",
        if (isTRUE(show_settings)) actionButton("open_settings", "Settings", class = "btn-ghost") else NULL,
        if (isTRUE(show_admin)) actionButton("admin_open", admin_label, class = "btn-ghost") else NULL,
        actionButton("logout", "Log out", class = "btn-danger")
      )
    ),
    div(
      class = "stepper",
      uiOutput("step_label"),
      uiOutput("breadcrumb_nav")
    ),
    div(class = "step-content", uiOutput("step_ui"))
  )
}

upload_step_ui <- function(can_fetch_master = TRUE) {
  layout_columns(
    col_widths = c(5, 7),
    card(
      class = "app-card",
      card_header(
        div(class = "step-title"),
        tags$h4("Upload & Fetch Data")
      ),
      card_body(
        fileInput("upload_file", "Upload spreadsheet", accept = c('.xlsx', '.xls', '.csv', 'text/csv', 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', 'application/vnd.ms-excel')),
        tags$div(style = "font-size:0.9em; color:#6b7280; margin-top:6px;", "Accepted file types: .xlsx, .xls, .csv — Max size: 10 MB."),
        uiOutput("upload_validation"),

        div(
          class = "mt-3",
          if (isTRUE(can_fetch_master)) actionButton("fetch_master", "Fetch master database", class = "btn-ghost") else NULL,
          if (isTRUE(can_fetch_master)) uiOutput("cancel_fetch_button") else NULL
        ),
        uiOutput("fetch_status"),
        uiOutput("fetch_feedback_ui"),
        uiOutput("fetch_progress_ui"),
        uiOutput("fetch_log_ui"),
        actionButton("confirm_upload", "Confirm upload", class = "btn-primary mt-3")
      )
    ),
    card(
      class = "app-card",
      card_header(
        div(class = "step-title"),
        tags$h4("Verify Spreadsheet")
      ),
      card_body(
        DT::DTOutput("upload_preview")
      )
    )
  )
}

mapping_step_ui <- function() {
  layout_columns(
    col_widths = c(5, 7),
    card(
      class = "app-card",
      card_header(
        div(class = "step-title"),
        tags$h4("Confirm Fields Mapping")
      ),
      card_body(
        checkboxGroupInput(
          "match_fields",
          "Fields to use for matching:",
          choices = c(
            "Partner" = "partner",
            "ID number" = "hoh_ID_number",
            "Phone number" = "phone_number",
            "Head of household name" = "hoh_arabic_name",
            "Spouse name" = "hoh_spouse_name",
            "Geography (governorate/district/subdistrict/village)" = "geography"
          ),
          selected = c(
            "partner",
            "hoh_ID_number",
            "phone_number",
            "hoh_arabic_name",
            "hoh_spouse_name",
            "geography"
          )
        ),
        uiOutput("mapping_ui"),
        actionButton("confirm_mapping", "Confirm mapping", class = "btn-primary mt-3")
      )
    ),
    card(
      class = "app-card",
      card_header(
        div(class = "step-title"),
        tags$h4("Suggested Matches")
      ),
      card_body(
        DT::DTOutput("mapping_table")
      )
    )
  )
}

strategy_step_ui <- function() {
  layout_columns(
    col_widths = c(5, 7),
    card(
      class = "app-card",
      card_header(
        div(class = "step-title"),
        tags$h4("Configure Matching Parameters")
      ),
      card_body(
        numericInput("threshold_high", "High confidence threshold:", value = 90, min = 50, max = 100, step = 1),
        numericInput("threshold_medium", "Medium confidence threshold:", value = 75, min = 50, max = 99, step = 1),
        numericInput("max_candidates", "Max candidate pairs:", value = 5000, min = 1000, max = 5000, step = 100),

        actionButton("confirm_strategy", "Continue", class = "btn-primary mt-3")
      )
    ),
    card(
      class = "app-card",
      card_header(
        div(class = "step-title"),
        tags$h4("Results Output")
      ),
      card_body(
        tags$ul(
          tags$li("One Excel file with Info + Summary + 4 matching sheets."),
          tags$li("Exact and fuzzy matching within upload and against master."),
          tags$li("Confidence levels, explainability fields, and masked master identifiers.")
        )
      )
    )
  )
}

matching_step_ui <- function() {
  card(
    class = "app-card",
    card_header(
      div(class = "step-title"),
      tags$h4("Run Matching")
    ),
    card_body(
      # Describe what happens when matching is initiated
      tags$div(
        class = "matching-description",
        tags$strong("When you run matching:"),
        tags$ul(
          tags$li("The app will load the latest master snapshot and preprocess the uploaded file."),
          tags$li("Candidate pairs will be generated using the selected match fields (capped at the configured max candidates)."),
          tags$li("Each candidate pair is scored and classified into High / Medium confidence."),
          tags$li("A running job can be stopped using \"Stop & start over\"; stopping will cancel the job and clear uploaded data."),
          tags$li("Results are saved and an export button will be enabled when matching completes.")
        )
      ),
      tags$div(style = "display:flex; gap:8px; align-items:center;",
        actionButton("run_match", "Run matching", class = "btn-primary"),
        uiOutput("cancel_button")
      ),
      tags$div(class = "mt-3", uiOutput("progress_ui")),
      uiOutput("matching_feedback_ui"),
      uiOutput("status_ui")
    )
  )
}

results_step_ui <- function() {
  layout_columns(
    col_widths = c(8, 4),
    card(
      class = "app-card",
      card_header(tags$h4("Matching Results")),
      card_body(
        DT::DTOutput("results_table")
      )
    ),
    card(
      class = "app-card",
      card_header(tags$h4("Export")),
      card_body(
        uiOutput("export_button"),
        uiOutput("export_status_ui"),
        uiOutput("status_ui")
      )
    )
  )
}

settings_step_ui <- function(can_edit_token = FALSE, can_edit_form_id = FALSE) {
  div(
    class = "settings-wrap",
    card(
      class = "app-card",
      card_header(tags$h4("Settings")),
      card_body(
        textInput("settings_username", "Signed-in email"),
        if (isTRUE(can_edit_token)) passwordInput("settings_token", "ActivityInfo token") else p(style = "color:#6b7280;", "Token changes are restricted for your role."),
        if (isTRUE(can_edit_form_id)) textInput("settings_form_id", "ActivityInfo table (form) ID") else NULL,
        actionButton("save_settings", "Save settings", class = "btn-primary"),
        actionButton("manage_mfa", "Manage MFA", class = "btn-ghost ms-2"),
        actionButton("close_settings", "Back to workflow", class = "btn-secondary ms-2")
      )
    )
  )
}

admin_step_ui <- function() {
  div(
    class = "settings-wrap",
    card(
      class = "app-card",
      card_header(tags$h4("User Administration")),
      card_body(
        uiOutput("admin_access_summary"),
        uiOutput("admin_workspace_ui"),
        actionButton("admin_back", "Back to workflow", class = "btn-secondary mt-3")
      )
    )
  )
}
