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
        actionButton("open_login", "Log in", class = "btn-primary")
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
      tags$span(" · "),
      tags$a("Privacy", href = "mailto:hamzaabdullahmoh@gmail.com"),
      tags$span(" · "),
      tags$a("Terms", href = "mailto:hamzaabdullahmoh@gmail.com")
    )
  )
}

main_ui <- function(app_name, show_admin = FALSE, admin_label = "Admin", show_settings = TRUE) {
  tagList(
    div(
      class = "app-topbar",
      div(class = "app-title", uiOutput("app_title")),
      div(class = "app-topbar-center", uiOutput("master_freshness_pill")),
      div(
        class = "app-actions",
        uiOutput("topbar_actions")
      )
    ),
    div(
      class = "stepper-container",
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
        div(
          style = "display: flex; justify-content: space-between; align-items: baseline; margin-bottom: 0.5rem;",
          tags$label("Upload spreadsheet", style = "font-weight: 500;"),
          downloadLink("download_template", "Download template", style = "font-size: 0.85em; text-decoration: none; color: var(--app-sea);")
        ),
        fileInput("upload_file", NULL, accept = c('.xlsx', '.xls', '.csv', 'text/csv', 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', 'application/vnd.ms-excel')),
        tags$div(style = "font-size:0.85em; color:#6b7280; margin-top:-6px; margin-bottom:12px;", "Accepted file types: .xlsx, .xls, .csv — Max size: 10 MB."),
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
        actionButton("confirm_upload", "Confirm upload & continue", class = "btn-primary mt-3")
      )
    ),
    card(
      class = "app-card",
      card_header(
        div(class = "step-title"),
        tags$h4("Data Health & Verification")
      ),
      card_body(
        uiOutput("upload_data_health_and_preview_ui")
      )
    )
  )
}

mapping_step_ui <- function() {
  layout_columns(
    col_widths = c(6, 6),
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
        div(
          style = "display: flex; gap: 10px; margin-bottom: 16px; align-items: flex-end; flex-wrap: wrap;",
          uiOutput("load_preset_ui"),
          actionButton("save_preset", "💾 Save as Preset", class = "btn-secondary")
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
    col_widths = c(7, 5),
    card(
      class = "app-card",
      card_header(
        div(class = "step-title"),
        tags$h4("Configure Matching Parameters")
      ),
      card_body(
        tags$div(
          class = "slider-group",
          sliderInput("threshold_high", "High confidence threshold:", min = 50, max = 100, value = 90, step = 1, post = "%", width = "100%"),
          tags$div(class = "slider-helper-text", "Pairs scoring at or above this threshold are classified as high-confidence matches.")
        ),
        tags$div(
          class = "slider-group",
          sliderInput("threshold_medium", "Medium confidence threshold:", min = 30, max = 99, value = 75, step = 1, post = "%", width = "100%"),
          tags$div(class = "slider-helper-text", "Pairs scoring between medium and high thresholds are flagged for manual review.")
        ),
        tags$div(
          class = "slider-group",
          sliderInput("max_candidates", "Max candidate pairs:", min = 50, max = 2000, value = 500, step = 50, width = "100%"),
          tags$div(class = "slider-helper-text", "Caps the number of candidate comparisons evaluated per block (maximum 2,000 pairs).")
        ),

        tags$div(
          class = "slider-group",
          style = "background: #F8FAFC; border: 1px solid var(--app-border); border-radius: 6px; padding: 14px; margin-top: 16px; margin-bottom: 16px;",
          tags$div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            tags$strong(style = "color: var(--app-forest); font-size: 0.85rem;", "📅 MPCA Last Distribution Date Filter (تصفية تاريخ آخر توزيع)"),
            tags$span(class = "category-badge-chip", style = "background: #E0E7FF; color: #3730A3;", "Dist_Date_Calc_New")
          ),
          tags$div(
            style = "margin-top: 8px;",
            checkboxInput(
              "filter_recent_mpca",
              tags$span(style = "font-weight: 600; font-size: 0.85rem; color: var(--app-text);", "Deduplicate only against beneficiaries with MPCA distribution in < 6 months"),
              value = FALSE
            )
          ),
          tags$p(
            class = "slider-helper-text",
            style = "margin: 4px 0 0 0; font-size: 0.78rem; color: #64748B;",
            "When checked, the engine filters the central master database to only match against beneficiaries whose last MPCA distribution date (Dist_Date_Calc_New) was received within the last 6 months (180 days). Beneficiaries assisted earlier are excluded from the check."
          ),
          conditionalPanel(
            condition = "input.filter_recent_mpca == true",
            tags$div(
              style = "margin-top: 12px; padding-top: 8px; border-top: 1px dashed var(--app-border);",
              sliderInput(
                "mpca_window_months",
                "Assistance Recency Window (Months / نافذة الأشهر):",
                min = 1,
                max = 12,
                value = 6,
                step = 1,
                post = " months",
                width = "100%"
              )
            )
          )
        ),

        actionButton("confirm_strategy", "Continue to matching", class = "btn-primary mt-2")
      )
    ),
    card(
      class = "app-card",
      card_header(
        div(class = "step-title"),
        tags$h4("Strategy & Deduplication Guide")
      ),
      card_body(
        tags$div(
          class = "health-alert health-alert-info mb-3",
          tags$strong("CCY Consortium Matching SOPs:"),
          tags$p(style = "margin: 4px 0 0 0; font-size: 0.8rem;", "Standard deduplication combines exact national ID/phone checks with weighted 4-part Arabic name decomposition (Jaro-Winkler + Levenshtein).")
        ),
        tags$ul(
          style = "font-size: 0.825rem; color: #475569; padding-left: 18px;",
          tags$li(tags$strong("High Confidence (≥90%): "), "Confirmed duplicates requiring immediate action or partner reconciliation."),
          tags$li(tags$strong("Medium Review (75%–89%): "), "Probable matches queued for field verification."),
          tags$li(tags$strong("MPCA Date Filter: "), "Allows targeting beneficiaries with recent assistance (<6 months) to avoid re-assisting within the active MPCA cycle while enabling re-eligibility after 6 months.")
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
      tags$div(
        class = "matching-actions-bar",
        uiOutput("run_match_button_ui"),
        uiOutput("cancel_button"),
        tags$div(id = "matching_feedback_status", class = "matching-instant-feedback")
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
      card_header(
        div(class = "step-title"),
        tags$h4("Deduplication Results Dossier")
      ),
      card_body(
        uiOutput("results_dossier_ui")
      )
    ),
    card(
      class = "app-card",
      card_header(
        div(class = "step-title"),
        tags$h4("Export & Actions")
      ),
      card_body(
        uiOutput("results_export_summary_ui"),
        div(
          style = "margin-top: 16px; display: flex; flex-direction: column; gap: 8px;",
          uiOutput("export_button"),
          actionButton("restart_dedup_btn", "🔄 Start New Deduplication Run", class = "btn-secondary")
        ),
        uiOutput("export_status_ui"),
        tags$hr(style = "margin: 20px 0; border-color: var(--app-border);"),
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
        div(style = "display:flex; gap:12px; margin-top:24px; flex-wrap:wrap;",
          actionButton("save_settings", "Save settings", class = "btn-primary"),
          actionButton("manage_mfa", "Manage MFA", class = "btn-ghost"),
          actionButton("close_settings_body", "Back to workflow", class = "btn-secondary")
        )
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
        div(style = "display:flex; gap:12px; margin-top:24px; flex-wrap:wrap;",
          actionButton("admin_back", "Back to workflow", class = "btn-secondary")
        )
      )
    )
  )
}
