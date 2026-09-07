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
    uiOutput("stepper_container_ui"),
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
        uiOutput("fetch_feedback_ui"),
        uiOutput("fetch_log_ui"),
        tags$div(
          id = "confirm_upload_btn_container",
          class = "shiny-html-output d-block mt-3",
          tags$button(
            id = "confirm_upload",
            type = "button",
            class = "btn btn-primary disabled",
            disabled = "disabled",
            style = "cursor: not-allowed; opacity: 0.65; pointer-events: none;",
            title = "Upload a valid spreadsheet to continue",
            `aria-disabled` = "true",
            "Confirm upload & continue"
          )
        )
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

data_health_skeleton <- function() {
  tags$div(
    class = "data-health-skeleton-container",
    # 1. Accessible status banner
    tags$div(
      class = "mapping-skeleton-banner mb-3",
      role = "status",
      `aria-live` = "polite",
      `aria-busy` = "true",
      tags$span(class = "spinner-border spinner-border-sm text-success flex-shrink-0", role = "status", `aria-hidden` = "true"),
      tags$div(
        tags$span(class = "banner-title", "Verifying Data Health: "),
        tags$span(class = "banner-text", "Reading file rows, verifying column structures, and running automated pre-upload quality audits. Please wait...")
      )
    ),
    # 2. Skeleton File Summary Bar
    tags$div(
      class = "d-flex justify-content-between align-items-center p-2 mb-3 border rounded",
      style = "background: #F8FAFC; border-color: var(--app-border);",
      tags$div(
        class = "d-flex align-items-center gap-2",
        tags$div(class = "skeleton-shimmer", style = "width: 180px; height: 18px; border-radius: 4px;"),
        tags$div(class = "skeleton-shimmer", style = "width: 120px; height: 14px; border-radius: 4px;")
      ),
      tags$div(class = "skeleton-shimmer", style = "width: 90px; height: 22px; border-radius: 12px;")
    ),
    # 3. Skeleton KPI Grid (4 chips)
    tags$div(
      class = "health-kpi-grid mb-3",
      lapply(1:4, function(i) {
        tags$div(
          class = "health-kpi-chip",
          tags$div(class = "skeleton-shimmer", style = "width: 70px; height: 10px; margin-bottom: 6px; border-radius: 3px;"),
          tags$div(class = "skeleton-shimmer", style = "width: 50px; height: 24px; border-radius: 4px;")
        )
      })
    ),
    # 4. Skeleton Hygiene Box
    tags$div(
      class = "hygiene-box mb-3",
      tags$div(
        class = "hygiene-header",
        tags$div(class = "skeleton-shimmer", style = "width: 220px; height: 16px; border-radius: 4px;"),
        tags$div(class = "skeleton-shimmer", style = "width: 140px; height: 20px; border-radius: 10px;")
      ),
      tags$div(
        class = "hygiene-grid",
        lapply(1:5, function(i) {
          tags$div(
            class = "hygiene-card",
            tags$div(
              class = "hygiene-card-header",
              tags$div(class = "skeleton-shimmer", style = "width: 100px; height: 14px; border-radius: 3px;"),
              tags$div(class = "skeleton-shimmer", style = "width: 50px; height: 16px; border-radius: 8px;")
            ),
            tags$div(class = "skeleton-shimmer mb-1", style = "width: 130px; height: 12px; border-radius: 3px;"),
            tags$div(class = "skeleton-shimmer", style = "width: 160px; height: 10px; border-radius: 3px;")
          )
        })
      )
    ),
    # 5. Skeleton Proceed Button
    tags$div(
      style = "display: flex; justify-content: flex-end; margin-top: 16px;",
      tags$button(
        type = "button",
        class = "btn btn-primary disabled",
        disabled = "disabled",
        style = "cursor: not-allowed; opacity: 0.65; pointer-events: none;",
        `aria-disabled` = "true",
        `aria-busy` = "true",
        tags$span(class = "spinner-border spinner-border-sm me-2", role = "status", `aria-hidden` = "true"),
        "Verifying Spreadsheet Health..."
      )
    )
  )
}

mapping_workbench_skeleton <- function() {
  render_skeleton_row <- function() {
    tags$div(
      class = "mapping-skeleton-row",
      # 1. Target Column Skeleton
      tags$div(
        class = "mapping-col-target",
        tags$div(class = "skeleton-shimmer skeleton-pill"),
        tags$div(class = "skeleton-shimmer skeleton-title"),
        tags$div(class = "skeleton-shimmer skeleton-desc")
      ),
      # 2. Flow Arrow Skeleton
      tags$div(
        class = "mapping-col-flow skeleton-arrow",
        tags$span("⟵", `aria-hidden` = "true")
      ),
      # 3. Source Dropdown Skeleton
      tags$div(
        class = "mapping-col-source",
        tags$div(class = "skeleton-shimmer skeleton-input")
      ),
      # 4. Preview / Chip Skeleton
      tags$div(
        class = "mapping-col-preview",
        tags$div(class = "skeleton-shimmer skeleton-badge")
      )
    )
  }

  groups <- list(
    list(title = "👤 Personal Identity & Demographics", title_ar = "الهوية والبيانات الديموغرافية", rows = 3),
    list(title = "📞 Contact Information", title_ar = "بيانات التواصل", rows = 2),
    list(title = "📍 Geographic Hierarchy", title_ar = "الموقع الجغرافي", rows = 2),
    list(title = "🏛️ Administrative & Project Metadata", title_ar = "البيانات الإدارية والمشروع", rows = 2)
  )

  tags$div(
    class = "mapping-skeleton-container",
    tags$div(
      class = "mapping-skeleton-banner",
      role = "status",
      `aria-live` = "polite",
      `aria-busy` = "true",
      tags$span(class = "spinner-border spinner-border-sm text-success flex-shrink-0", role = "status", `aria-hidden` = "true"),
      tags$div(
        tags$span(class = "banner-title", "Initializing Column Alignment Workbench: "),
        tags$span(class = "banner-text", "Analyzing spreadsheet headers and auto-aligning standard CCY fields. Please wait...")
      )
    ),
    tags$div(
      class = "mapping-matrix-header d-none d-md-grid",
      tags$div(class = "header-target", tags$strong("Target CCY Master Field (الحقل المعياري)")),
      tags$div(class = "header-flow text-center", tags$strong("")),
      tags$div(class = "header-source", tags$strong("Source Uploaded Column (العمود المرفوع)")),
      tags$div(class = "header-preview", tags$strong("Alignment Status & Live Sample (المعاينة الحية)"))
    ),
    lapply(groups, function(grp) {
      tags$div(
        class = "mapping-category-group mb-3",
        tags$div(
          class = "mapping-category-header",
          tags$div(
            class = "d-flex align-items-center gap-2",
            tags$span(paste0(grp$title, " (", grp$title_ar, ")"))
          ),
          tags$span(class = "category-badge-chip", "Loading...")
        ),
        tags$div(
          class = "mapping-category-body",
          lapply(seq_len(grp$rows), function(i) render_skeleton_row())
        )
      )
    })
  )
}

mapping_step_ui <- function() {
  div(
    class = "mapping-step-wrapper",
    # Section 1: Match Engine Scope & Blocking Criteria
    card(
      class = "app-card mapping-scope-card",
      card_header(
        div(
          class = "section-header-bar",
          div(
            class = "section-header-content",
            tags$h4(
              class = "section-header-title",
              tags$span(class = "section-header-icon", "🎯"),
              "1. Match Engine Scope & Criteria Configuration"
            ),
            tags$p(
              class = "section-header-desc",
              "Select the criteria to evaluate. Only selected criteria will require column mapping below."
            )
          ),
          div(
            class = "section-header-actions",
            actionButton("select_all_fields_btn", "Select All", class = "btn-ghost btn-sm section-header-btn"),
            actionButton("reset_std_fields_btn", "Reset Standard", class = "btn-ghost btn-sm section-header-btn")
          )
        )
      ),
      card_body(
        div(
          class = "match-criteria-container",
          checkboxGroupInput(
            "match_fields",
            label = tags$span(class = "visually-hidden", "Match engine criteria selection"),
            width = "100%",
            choiceNames = list(
              div(
                class = "match-field-item",
                div(
                  class = "match-field-header",
                  div(
                    class = "match-field-title-group",
                    tags$span(class = "match-field-icon", "🔑"),
                    tags$strong(class = "match-field-title", "National ID Number")
                  ),
                  tags$span(class = "badge-role-primary", "Primary Blocking")
                ),
                div(class = "match-field-desc", "Exact national ID matching (رقم الهوية الوطنية / البطاقة)")
              ),
              div(
                class = "match-field-item",
                div(
                  class = "match-field-header",
                  div(
                    class = "match-field-title-group",
                    tags$span(class = "match-field-icon", "📱"),
                    tags$strong(class = "match-field-title", "Phone Number")
                  ),
                  tags$span(class = "badge-role-primary", "Primary Blocking")
                ),
                div(class = "match-field-desc", "Normalized 9-digit mobile phone matching (رقم الهاتف الأساسي)")
              ),
              div(
                class = "match-field-item",
                div(
                  class = "match-field-header",
                  div(
                    class = "match-field-title-group",
                    tags$span(class = "match-field-icon", "👤"),
                    tags$strong(class = "match-field-title", "Head of Household Name")
                  ),
                  tags$span(class = "badge-role-fuzzy", "Fuzzy / Token Anchor")
                ),
                div(class = "match-field-desc", "Normalized 4-part Arabic name decomposition (اسم رب الأسرة)")
              ),
              div(
                class = "match-field-item",
                div(
                  class = "match-field-header",
                  div(
                    class = "match-field-title-group",
                    tags$span(class = "match-field-icon", "👥"),
                    tags$strong(class = "match-field-title", "Spouse Name")
                  ),
                  tags$span(class = "badge-role-secondary", "Secondary Verification")
                ),
                div(class = "match-field-desc", "Cross-spouse verification to resolve candidate ambiguity (اسم الزوج / الزوجة)")
              ),
              div(
                class = "match-field-item",
                div(
                  class = "match-field-header",
                  div(
                    class = "match-field-title-group",
                    tags$span(class = "match-field-icon", "📍"),
                    tags$strong(class = "match-field-title", "Geographic Hierarchy")
                  ),
                  tags$span(class = "badge-role-geo", "Spatial Blocking")
                ),
                div(class = "match-field-desc", "Admin Levels 1–4: Governorate, District, Subdistrict, Village (الموقع الجغرافي)")
              ),
              div(
                class = "match-field-item",
                div(
                  class = "match-field-header",
                  div(
                    class = "match-field-title-group",
                    tags$span(class = "match-field-icon", "🏛️"),
                    tags$strong(class = "match-field-title", "Partner Organization")
                  ),
                  tags$span(class = "badge-role-meta", "Metadata Scope")
                ),
                div(class = "match-field-desc", "Deduplicate within or across consortium partner organizations (المنظمة الشريكة)")
              )
            ),
            choiceValues = list(
              "hoh_ID_number",
              "phone_number",
              "hoh_arabic_name",
              "hoh_spouse_name",
              "geography",
              "partner"
            ),
            selected = c(
              "hoh_ID_number",
              "phone_number",
              "hoh_arabic_name",
              "hoh_spouse_name",
              "geography",
              "partner"
            )
          )
        )
      )
    ),

    # Section 2: Field Alignment & Mapping Workbench
    card(
      class = "app-card mapping-workbench-card",
      card_header(
        div(
          class = "section-header-bar",
          div(
            class = "section-header-content",
            tags$h4(
              class = "section-header-title",
              tags$span(class = "section-header-icon", "📋"),
              "2. Column Alignment Workbench"
            ),
            tags$p(
              class = "section-header-desc",
              "Align your uploaded spreadsheet headers with CCY master canonical fields."
            )
          ),
          uiOutput("mapping_progress_pill")
        )
      ),
      card_body(
        div(
          class = "mapping-toolbar-bar d-flex justify-content-between align-items-center flex-wrap gap-2 mb-3 pb-3 border-bottom",
          div(
            class = "d-flex align-items-center gap-2 flex-wrap",
            uiOutput("load_preset_ui"),
            actionButton("save_preset", "💾 Save Preset", class = "btn-secondary btn-sm", style = "padding: 6px 12px; font-size: 0.825rem;")
          ),
          div(
            class = "d-flex align-items-center gap-2 flex-wrap",
            uiOutput("auto_map_btn_container", inline = TRUE),
            actionButton("clear_mapping_btn", "↺ Clear All", class = "btn-ghost btn-sm", style = "padding: 6px 10px; font-size: 0.8rem;")
          )
        ),
        uiOutput("auto_map_status_banner"),
        tags$div(
          id = "mapping_ui",
          class = "shiny-html-output",
          `aria-live` = "polite",
          `aria-busy` = "true",
          mapping_workbench_skeleton()
        ),
        div(
          class = "mapping-footer-bar mt-4 pt-3 border-top d-flex justify-content-between align-items-center flex-wrap gap-3",
          actionButton("back_to_upload_btn", "← Back to Upload", class = "btn-secondary"),
          div(
            class = "d-flex align-items-center gap-3 flex-wrap",
            tags$div(
              id = "mapping_validation_hint",
              class = "shiny-html-output",
              tags$div(
                class = "mapping-hint-text text-muted d-flex align-items-center gap-1",
                tags$span(class = "spinner-grow spinner-grow-sm text-primary", role = "status", `aria-hidden` = "true"),
                tags$span("Aligning spreadsheet headers and checking criteria...")
              )
            ),
            tags$div(
              id = "confirm_mapping_btn_container",
              class = "shiny-html-output d-inline-block",
              tags$button(
                id = "confirm_mapping",
                type = "button",
                class = "btn btn-primary disabled",
                disabled = "disabled",
                style = "cursor: not-allowed; opacity: 0.65; pointer-events: none;",
                `aria-disabled` = "true",
                `aria-busy` = "true",
                tags$span(class = "spinner-border spinner-border-sm me-2", role = "status", `aria-hidden` = "true"),
                "Loading Column Alignment..."
              )
            )
          )
        )
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
    class = "admin-wrap",
    div(
      class = "admin-header-bar",
      div(
        class = "admin-header-title",
        tags$h3(tags$span(style = "color: var(--app-forest);", "⚙️"), "System Administration & Control Center"),
        tags$p("Manage authorized user accounts, partner directories, compliance audit trails, and system backups.")
      ),
      div(
        class = "d-flex gap-2 align-items-center",
        actionButton("admin_back", "← Back to Deduplication Workflow", class = "btn-secondary")
      )
    ),
    uiOutput("admin_access_summary"),
    uiOutput("admin_workspace_ui")
  )
}
