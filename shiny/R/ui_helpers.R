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
      tags$a("Contact Support", href = "#support"),
      tags$span(" | "),
      tags$a("Privacy", href = "#privacy"),
      tags$span(" | "),
      tags$a("Terms", href = "#terms")
    )
  )
}

main_ui <- function(app_name, show_admin = FALSE) {
  tagList(
    div(
      class = "app-topbar",
      div(class = "app-title", uiOutput("app_title")),
      div(
        class = "app-actions",
        actionButton("open_settings", "Settings", class = "btn-ghost"),
        if (show_admin) actionButton("admin_open", "Admin", class = "btn-ghost"),
        actionButton("logout", "Log out", class = "btn-danger")
      )
    ),
    div(
      class = "stepper",
      uiOutput("step_label"),
      tags$span(class = "stepper-title", "Upload -> Map -> Configure -> Match -> Results")
    ),
    div(class = "step-content", uiOutput("step_ui"))
  )
}

upload_step_ui <- function() {
  layout_columns(
    col_widths = c(5, 7),
    card(
      class = "app-card",
      card_header(
        div(class = "step-title", "Step 1"),
        tags$h4("Upload partner data")
      ),
      card_body(
        fileInput("upload_file", "Excel file (.xlsx)", accept = ".xlsx"),
        uiOutput("upload_validation"),
        div(
          class = "callout mt-3",
          tags$strong("Tip:"),
          " Ensure required columns are present before upload."
        ),
        div(
          class = "mt-3",
          actionButton("fetch_master", "Fetch master database", class = "btn-ghost"),
          uiOutput("cancel_fetch_button")
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
        div(class = "step-title", "Preview"),
        tags$h4("Verify the spreadsheet")
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
        div(class = "step-title", "Step 2"),
        tags$h4("Confirm field mapping")
      ),
      card_body(
        checkboxGroupInput(
          "match_fields",
          "Fields to use for matching",
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
        actionButton("confirm_mapping", "Confirm mapping", class = "btn-primary mt-3"),
        actionButton("back_to_upload", "Back to upload", class = "btn-secondary ms-2"),
        uiOutput("cancel_button")
      )
    ),
    card(
      class = "app-card",
      card_header(
        div(class = "step-title", "Suggestions"),
        tags$h4("Suggested matches")
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
        div(class = "step-title", "Step 3"),
        tags$h4("Configure matching")
      ),
      card_body(
        numericInput("threshold_high", "High confidence threshold", value = 90, min = 50, max = 100, step = 1),
        numericInput("threshold_medium", "Medium confidence threshold", value = 75, min = 50, max = 99, step = 1),
        numericInput("max_candidates", "Max candidate pairs", value = 500000, min = 1000, max = 2000000, step = 1000),
        div(
          class = "callout mt-3",
          tags$strong("Matching model:"),
          "Exact rules: ID/phones/name+geo/name+ID/phone+age. Fuzzy score uses name+spouse+phone+geography+age+sex with explainable factors."
        ),
        actionButton("confirm_strategy", "Continue", class = "btn-primary mt-3"),
        uiOutput("cancel_button")
      )
    ),
    card(
      class = "app-card",
      card_header(
        div(class = "step-title", "What you will get"),
        tags$h4("Results output")
      ),
      card_body(
        tags$ul(
          tags$li("One Excel file with Info + Summary + 4 matching sheets"),
          tags$li("Exact and fuzzy matching within upload and against master"),
          tags$li("Confidence levels, explainability fields, and masked master identifiers")
        )
      )
    )
  )
}

matching_step_ui <- function() {
  card(
    class = "app-card",
    card_header(
      div(class = "step-title", "Step 4"),
      tags$h4("Run matching")
    ),
    card_body(
      actionLink("back_to_strategy", "Back to step 3"),
      tags$br(),
      actionButton("run_match", "Run matching", class = "btn-primary"),
      uiOutput("cancel_button"),
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
      card_header(tags$h4("Matching results")),
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

settings_step_ui <- function(show_admin = FALSE) {
  div(
    class = "settings-wrap",
    card(
      class = "app-card",
      card_header(tags$h4("Settings")),
      card_body(
        textInput("settings_username", "User name"),
        passwordInput("settings_token", "ActivityInfo token"),
        if (show_admin) textInput("settings_form_id", "ActivityInfo table (form) ID"),
        actionButton("save_settings", "Save settings", class = "btn-primary"),
        actionButton("close_settings", "Back to workflow", class = "btn-secondary ms-2")
      )
    )
  )
}
