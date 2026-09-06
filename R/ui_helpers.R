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

upload_step_ui <- function(can_fetch_master = TRUE, lang = "en") {
  layout_columns(
    col_widths = c(5, 7),
    card(
      class = "app-card",
      card_header(
        div(class = "step-title"),
        tags$h4(tr("card_upload_title", lang = lang))
      ),
      card_body(
        div(
          style = "display: flex; justify-content: space-between; align-items: baseline; margin-bottom: 0.5rem;",
          tags$label(tr("label_upload_file", lang = lang), style = "font-weight: 500;"),
          downloadLink("download_template", tr("link_download_template", lang = lang), style = "font-size: 0.85em; text-decoration: none; color: var(--app-sea);")
        ),
        fileInput("upload_file", NULL, accept = c('.xlsx', '.xls', '.csv', 'text/csv', 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet', 'application/vnd.ms-excel')),
        tags$div(style = "font-size:0.85em; color:#6b7280; margin-top:-6px; margin-bottom:12px;", tr("hint_accepted_files", lang = lang)),
        uiOutput("upload_validation"),

        div(
          class = "mt-3",
          if (isTRUE(can_fetch_master)) actionButton("fetch_master", tr("btn_fetch_master", lang = lang), class = "btn-ghost") else NULL,
          if (isTRUE(can_fetch_master)) uiOutput("cancel_fetch_button") else NULL
        ),
                uiOutput("fetch_feedback_ui"),
                uiOutput("fetch_log_ui"),
        actionButton("confirm_upload", tr("btn_confirm_upload", lang = lang), class = "btn-primary mt-3")
      )
    ),
    card(
      class = "app-card",
      card_header(
        div(class = "step-title"),
        tags$h4(tr("card_health_title", lang = lang))
      ),
      card_body(
        uiOutput("upload_data_health_and_preview_ui")
      )
    )
  )
}

mapping_step_ui <- function(lang = "en", selected_fields = NULL) {
  field_labels <- c(
    "partner" = tr("field_partner", lang = lang),
    "hoh_ID_number" = tr("field_id", lang = lang),
    "phone_number" = tr("field_phone", lang = lang),
    "hoh_arabic_name" = tr("field_hoh_name", lang = lang),
    "hoh_spouse_name" = tr("field_spouse_name", lang = lang),
    "geography" = tr("field_geography", lang = lang)
  )
  choices <- setNames(names(field_labels), field_labels)
  selected <- if (!is.null(selected_fields)) selected_fields else c(
    "partner",
    "hoh_ID_number",
    "phone_number",
    "hoh_arabic_name",
    "hoh_spouse_name",
    "geography"
  )

  layout_columns(
    col_widths = 12,
    card(
      class = "app-card",
      card_header(
        div(class = "step-title"),
        tags$h4(tr("card_mapping_title", lang = lang))
      ),
      card_body(
        checkboxGroupInput(
          "match_fields",
          tr("label_match_fields", lang = lang),
          choices = choices,
          selected = selected
        ),
        div(
          class = "d-flex align-items-end flex-wrap", style = "gap: 10px; margin-bottom: 16px;",
          uiOutput("load_preset_ui"),
          actionButton("save_preset", tr("btn_save_preset", lang = lang), class = "btn-secondary")
        ),
        uiOutput("mapping_ui"),
        actionButton("confirm_mapping", tr("btn_confirm_mapping", lang = lang), class = "btn-primary mt-3")
      )
    )
  )
}

strategy_step_ui <- function(lang = "en", high_val = 90, med_val = 75, max_cand_val = 500, filter_mpca_val = FALSE, mpca_months_val = 6) {
  layout_columns(
    col_widths = c(7, 5),
    card(
      class = "app-card",
      card_header(
        div(class = "step-title"),
        tags$h4(tr("card_strategy_title", lang = lang))
      ),
      card_body(
        tags$div(
          class = "slider-group",
          sliderInput("threshold_high", tr("slider_threshold_high", lang = lang), min = 50, max = 100, value = high_val, step = 1, post = "%", width = "100%"),
          tags$div(class = "slider-helper-text", tr("helper_threshold_high", lang = lang))
        ),
        tags$div(
          class = "slider-group",
          sliderInput("threshold_medium", tr("slider_threshold_medium", lang = lang), min = 30, max = 99, value = med_val, step = 1, post = "%", width = "100%"),
          tags$div(class = "slider-helper-text", tr("helper_threshold_medium", lang = lang))
        ),
        tags$div(
          class = "slider-group",
          sliderInput("max_candidates", tr("slider_max_candidates", lang = lang), min = 50, max = 2000, value = max_cand_val, step = 50, width = "100%"),
          tags$div(class = "slider-helper-text", tr("helper_max_candidates", lang = lang))
        ),

        tags$div(
          class = "slider-group",
          style = "background: #F8FAFC; border: 1px solid var(--app-border); border-radius: 6px; padding: 14px; margin-top: 16px; margin-bottom: 16px;",
          tags$div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            tags$strong(style = "color: var(--app-forest); font-size: 0.85rem;", tr("mpca_filter_title", lang = lang)),
            tags$span(class = "category-badge-chip", style = "background: #E0E7FF; color: #3730A3;", "Dist_Date_Calc_New")
          ),
          tags$div(
            style = "margin-top: 8px;",
            checkboxInput(
              "filter_recent_mpca",
              tags$span(style = "font-weight: 600; font-size: 0.85rem; color: var(--app-text);", tr("mpca_filter_checkbox", lang = lang)),
              value = isTRUE(filter_mpca_val)
            )
          ),
          tags$p(
            class = "slider-helper-text",
            style = "margin: 4px 0 0 0; font-size: 0.78rem; color: #64748B;",
            tr("mpca_filter_desc", lang = lang)
          ),
          conditionalPanel(
            condition = "input.filter_recent_mpca == true",
            tags$div(
              style = "margin-top: 12px; padding-top: 8px; border-top: 1px dashed var(--app-border);",
              sliderInput(
                "mpca_window_months",
                tr("slider_mpca_window", lang = lang),
                min = 1,
                max = 12,
                value = mpca_months_val,
                step = 1,
                post = if (identical(lang, "ar")) " أشهر" else " months",
                width = "100%"
              )
            )
          )
        ),

        actionButton("confirm_strategy", tr("btn_confirm_strategy", lang = lang), class = "btn-primary mt-2")
      )
    ),
    card(
      class = "app-card",
      card_header(
        div(class = "step-title"),
        tags$h4(tr("card_guide_title", lang = lang))
      ),
      card_body(
        tags$div(
          class = "health-alert health-alert-info mb-3",
          tags$strong(tr("guide_sops_title", lang = lang)),
          tags$p(style = "margin: 4px 0 0 0; font-size: 0.8rem;", tr("guide_sops_desc", lang = lang))
        ),
        tags$ul(
          style = "font-size: 0.825rem; color: #475569; padding-left: 18px;",
          tags$li(tags$strong(tr("guide_high_label", lang = lang)), tr("guide_high_desc", lang = lang)),
          tags$li(tags$strong(tr("guide_medium_label", lang = lang)), tr("guide_medium_desc", lang = lang)),
          tags$li(tags$strong(tr("guide_mpca_label", lang = lang)), tr("guide_mpca_desc", lang = lang))
        )
      )
    )
  )
}

matching_step_ui <- function(lang = "en") {
  card(
    class = "app-card",
    card_header(
      div(class = "step-title"),
      tags$h4(tr("card_matching_title", lang = lang))
    ),
    card_body(
      tags$div(
        class = "matching-description",
        tags$strong(tr("matching_when_run", lang = lang)),
        tags$ul(
          tags$li(tr("matching_bullet_1", lang = lang)),
          tags$li(tr("matching_bullet_2", lang = lang)),
          tags$li(tr("matching_bullet_3", lang = lang)),
          tags$li(tr("matching_bullet_4", lang = lang)),
          tags$li(tr("matching_bullet_5", lang = lang))
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

results_step_ui <- function(lang = "en") {
  layout_columns(
    col_widths = c(8, 4),
    card(
      class = "app-card",
      card_header(
        div(class = "step-title"),
        tags$h4(tr("card_results_dossier", lang = lang))
      ),
      card_body(
        uiOutput("results_dossier_ui")
      )
    ),
    card(
      class = "app-card",
      card_header(
        div(class = "step-title"),
        tags$h4(tr("card_export_actions", lang = lang))
      ),
      card_body(
        uiOutput("results_export_summary_ui"),
        div(
          style = "margin-top: 16px; display: flex; flex-direction: column; gap: 8px;",
          uiOutput("export_button"),
          actionButton("restart_dedup_btn", tr("btn_restart_dedup", lang = lang), class = "btn-secondary")
        ),
        uiOutput("export_status_ui"),
        tags$hr(style = "margin: 20px 0; border-color: var(--app-border);"),
        uiOutput("status_ui")
      )
    )
  )
}

settings_step_ui <- function(can_edit_token = FALSE, can_edit_form_id = FALSE, lang = "en") {
  div(
    class = "settings-wrap",
    card(
      class = "app-card",
      card_header(tags$h4(tr("settings_header", lang = lang))),
      card_body(
        textInput("settings_username", tr("label_signed_in_email", lang = lang)),
        if (isTRUE(can_edit_token)) passwordInput("settings_token", tr("label_ai_token", lang = lang)) else p(style = "color:#6b7280;", if (identical(lang, "ar")) "تعديل الرمز محظور على هذا الدور." else "Token changes are restricted for your role."),
        if (isTRUE(can_edit_form_id)) textInput("settings_form_id", tr("label_ai_form_id", lang = lang)) else NULL,
        div(style = "display:flex; gap:12px; margin-top:24px; flex-wrap:wrap;",
          actionButton("save_settings", tr("btn_save_settings", lang = lang), class = "btn-primary"),
          actionButton("manage_mfa", tr("btn_manage_mfa", lang = lang), class = "btn-ghost"),
          actionButton("close_settings_body", tr("btn_back_to_workflow", lang = lang), class = "btn-secondary")
        )
      )
    )
  )
}

admin_step_ui <- function(lang = "en") {
  div(
    class = "admin-wrap",
    div(
      class = "admin-header-bar",
      div(
        class = "admin-header-title",
        tags$h3(tags$span(style = "color: var(--app-forest);", "⚙️"), tr("admin_header_title", lang = lang)),
        tags$p(tr("admin_header_desc", lang = lang))
      ),
      div(
        class = "d-flex gap-2 align-items-center",
        actionButton("admin_back", if (identical(lang, "ar")) "← العودة إلى مسار العمل" else "← Back to Deduplication Workflow", class = "btn-secondary")
      )
    ),
    uiOutput("admin_access_summary"),
    uiOutput("admin_workspace_ui")
  )
}
