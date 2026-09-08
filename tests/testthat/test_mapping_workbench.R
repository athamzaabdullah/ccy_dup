testthat::context("mapping-workbench")

source(file.path("..", "..", "R", "mapping.R"))

test_that("get_field_meta returns correct metadata and roles", {
  id_meta <- get_field_meta("hoh_ID_number")
  expect_equal(id_meta$role_type, "primary")
  expect_equal(id_meta$icon, "🔑")
  expect_true(nzchar(id_meta$role))
  expect_true(nzchar(id_meta$description))

  phone_meta <- get_field_meta("phone_number")
  expect_equal(phone_meta$role_type, "primary")
  expect_equal(phone_meta$icon, "📱")

  name_meta <- get_field_meta("hoh_arabic_name")
  expect_equal(name_meta$role_type, "fuzzy")

  spouse_meta <- get_field_meta("hoh_spouse_name")
  expect_equal(spouse_meta$role_type, "secondary")

  geo_meta <- get_field_meta("governorate")
  expect_equal(geo_meta$role_type, "spatial")

  # Fallback for unknown field
  unk_meta <- get_field_meta("completely_unknown_col")
  expect_equal(unk_meta$role_type, "default")
  expect_equal(unk_meta$icon, "📋")
})

test_that("detect_best_column_match performs exact and normalized matching", {
  cols <- c("record_id", "hoh_ID_number", "phone_number", "Full_Name")
  expect_equal(detect_best_column_match("hoh_ID_number", cols), "hoh_ID_number")
  expect_equal(detect_best_column_match("phone_number", cols), "phone_number")

  # Normalized case-insensitive and underscore variations
  cols_upper <- c("RECORD_ID", "HOH_ID_NUMBER", "PHONE_NUMBER")
  expect_equal(detect_best_column_match("hoh_ID_number", cols_upper), "HOH_ID_NUMBER")
  expect_equal(detect_best_column_match("phone_number", cols_upper), "PHONE_NUMBER")
})

test_that("detect_best_column_match matches standard CCY aliases", {
  ccy_cols <- c(
    "1.1. Organization Prefix",
    "3.12 What is the Head of Household's ID number?",
    "2.1. Primary Phone Number:",
    "3.1. Head of household (HoH) Name (Arabic)",
    "3.3. Head of HH's Spouse Name",
    "Governorate Label",
    "District Label",
    "Subdistrict Label...78",
    "1.14. Village"
  )

  expect_equal(detect_best_column_match("partner", ccy_cols), "1.1. Organization Prefix")
  expect_equal(detect_best_column_match("hoh_ID_number", ccy_cols), "3.12 What is the Head of Household's ID number?")
  expect_equal(detect_best_column_match("phone_number", ccy_cols), "2.1. Primary Phone Number:")
  expect_equal(detect_best_column_match("hoh_arabic_name", ccy_cols), "3.1. Head of household (HoH) Name (Arabic)")
  expect_equal(detect_best_column_match("hoh_spouse_name", ccy_cols), "3.3. Head of HH's Spouse Name")
  expect_equal(detect_best_column_match("governorate", ccy_cols), "Governorate Label")
  expect_equal(detect_best_column_match("district", ccy_cols), "District Label")
  expect_equal(detect_best_column_match("subdistrict", ccy_cols), "Subdistrict Label...78")
  expect_equal(detect_best_column_match("village", ccy_cols), "1.14. Village")
})

test_that("detect_best_column_match avoids false positives between similar fields", {
  # District vs Subdistrict
  mixed_geo <- c("subdistrict_name", "district_name")
  expect_equal(detect_best_column_match("district", mixed_geo), "district_name")
  expect_equal(detect_best_column_match("subdistrict", mixed_geo), "subdistrict_name")

  # HoH Name vs Spouse Name
  names_cols <- c("spouse_name", "hoh_name")
  expect_equal(detect_best_column_match("hoh_arabic_name", names_cols), "hoh_name")
  expect_equal(detect_best_column_match("hoh_spouse_name", names_cols), "spouse_name")
})

test_that("detect_best_column_match handles edge cases gracefully", {
  expect_null(detect_best_column_match("hoh_ID_number", character(0)))
  expect_null(detect_best_column_match("hoh_ID_number", NULL))
  expect_null(detect_best_column_match("nonexistent_field", c("col1", "col2")))
})

test_that("get_sample_preview_value extracts representative samples", {
  df <- data.frame(
    id = c(NA, "  100234567  ", "200345678"),
    empty_col = c(NA, "", "   "),
    long_col = c("This is a very long descriptive text value that should exceed the limit", "b", "c"),
    stringsAsFactors = FALSE
  )

  # Normal trimmed sample extraction
  expect_equal(get_sample_preview_value(df, "id"), "100234567")

  # All empty or NA column
  expect_equal(get_sample_preview_value(df, "empty_col"), "(all empty / NA)")

  # Long value truncation
  truncated <- get_sample_preview_value(df, "long_col", max_len = 20)
  expect_true(nchar(truncated) <= 20)
  expect_true(grepl("\\.\\.\\.$", truncated))

  # Edge cases
  expect_null(get_sample_preview_value(NULL, "id"))
  expect_null(get_sample_preview_value(df, "nonexistent"))
  expect_null(get_sample_preview_value(df, ""))
})

test_that("get_field_bilingual_label formats bilingual strings correctly", {
  label <- get_field_bilingual_label("hoh_ID_number")
  expect_true(grepl("Head of Household ID", label))
  expect_true(grepl("رقم الهوية", label))

  # Unrecognized field returns original name
  expect_equal(get_field_bilingual_label("custom_field_xyz"), "custom_field_xyz")
})

test_that("mapping_workbench_skeleton renders accessible loading skeleton", {
  library(shiny)
  library(bslib)
  source(file.path("..", "..", "R", "ui_helpers.R"))

  skeleton <- mapping_workbench_skeleton()
  skel_html <- as.character(skeleton)

  expect_true(grepl("mapping-skeleton-container", skel_html))
  expect_true(grepl("mapping-skeleton-banner", skel_html))
  expect_true(grepl('role="status"', skel_html))
  expect_true(grepl('aria-live="polite"', skel_html))
  expect_true(grepl('aria-busy="true"', skel_html))
  expect_true(grepl("skeleton-shimmer", skel_html))
  expect_true(grepl("mapping-skeleton-row", skel_html))
  expect_true(grepl("Personal Identity", skel_html))
  expect_true(grepl("Contact Information", skel_html))
  expect_true(grepl("Geographic Hierarchy", skel_html))
  expect_true(grepl("Administrative.*Project Metadata", skel_html))
})

test_that("mapping_step_ui initializes with loading skeleton and disabled confirm button", {
  library(shiny)
  library(bslib)
  source(file.path("..", "..", "R", "ui_helpers.R"))

  step_html <- as.character(mapping_step_ui())

  # 1. mapping_ui output container has embedded skeleton
  expect_true(grepl('id="mapping_ui"', step_html))
  expect_true(grepl("mapping-skeleton-container", step_html))
  expect_true(grepl('aria-busy="true"', step_html))

  # 2. confirm_mapping button is initially disabled with loading spinner
  expect_true(grepl('id="confirm_mapping_btn_container"', step_html))
  expect_true(grepl('id="confirm_mapping"', step_html))
  expect_true(grepl('disabled="disabled"', step_html))
  expect_true(grepl('aria-disabled="true"', step_html))
  expect_true(grepl('aria-busy="true"', step_html))
  expect_true(grepl("spinner-border", step_html))
  expect_true(grepl("Loading Column Alignment[…\\.]", step_html))

  # 3. mapping_validation_hint has initial loading progress indicator
  expect_true(grepl('id="mapping_validation_hint"', step_html))
  expect_true(grepl("Aligning spreadsheet headers and checking criteria", step_html))
})

test_that("data_health_skeleton renders accessible loading skeleton with KPI and hygiene placeholders", {
  library(shiny)
  library(bslib)
  source(file.path("..", "..", "R", "ui_helpers.R"))

  skeleton <- data_health_skeleton()
  skel_html <- as.character(skeleton)

  expect_true(grepl("data-health-skeleton-container", skel_html))
  expect_true(grepl("mapping-skeleton-banner", skel_html))
  expect_true(grepl('role="status"', skel_html))
  expect_true(grepl('aria-live="polite"', skel_html))
  expect_true(grepl('aria-busy="true"', skel_html))
  expect_true(grepl("Verifying Data Health", skel_html))
  expect_true(grepl("health-kpi-grid", skel_html))
  expect_true(grepl("health-kpi-chip", skel_html))
  expect_true(grepl("hygiene-box", skel_html))
  expect_true(grepl("hygiene-grid", skel_html))
  expect_true(grepl("hygiene-card", skel_html))
  expect_equal(lengths(regmatches(skel_html, gregexpr('class="hygiene-card"', skel_html))), 5)
  expect_true(grepl("skeleton-shimmer", skel_html))
  expect_true(grepl('disabled="disabled"', skel_html))
  expect_true(grepl("Verifying Spreadsheet Health[…\\.]", skel_html))
})

test_that("upload_step_ui initializes with disabled confirm upload button container and skeleton holder", {
  library(shiny)
  library(bslib)
  source(file.path("..", "..", "R", "ui_helpers.R"))

  step_html <- as.character(upload_step_ui())

  expect_true(grepl('id="confirm_upload_btn_container"', step_html))
  expect_true(grepl('id="confirm_upload"', step_html))
  expect_true(grepl('disabled="disabled"', step_html))
  expect_true(grepl('aria-disabled="true"', step_html))
  expect_true(grepl("Confirm upload &amp; continue", step_html))
  expect_true(grepl('id="upload_data_health_and_preview_ui"', step_html))
  expect_true(grepl('id="data_health_skeleton_holder"', step_html))
  expect_true(grepl('display: none;', step_html))
})

test_that("mapping_step_ui contains mapping_skeleton_holder for instant client-side transitions", {
  library(shiny)
  library(bslib)
  source(file.path("..", "..", "R", "ui_helpers.R"))

  step_html <- as.character(mapping_step_ui())
  expect_true(grepl('id="mapping_skeleton_holder"', step_html))
  expect_true(grepl('display: none;', step_html))
})

test_that("results_dossier_skeleton renders accessible loading skeleton with KPI and table placeholders", {
  library(shiny)
  library(bslib)
  source(file.path("..", "..", "R", "ui_helpers.R"))

  skeleton <- results_dossier_skeleton()
  skel_html <- as.character(skeleton)

  expect_true(grepl("results-skeleton-container", skel_html))
  expect_true(grepl("mapping-skeleton-banner", skel_html))
  expect_true(grepl('role="status"', skel_html))
  expect_true(grepl('aria-live="polite"', skel_html))
  expect_true(grepl('aria-busy="true"', skel_html))
  expect_true(grepl("Compiling Deduplication Dossier", skel_html))
  expect_true(grepl("health-kpi-grid", skel_html))
  expect_true(grepl("skeleton-shimmer", skel_html))
})

test_that("custom.css guarantees continuous revolving button spinners and GPU-composited skeleton shimmer", {
  css_path <- file.path("..", "..", "www", "custom.css")
  expect_true(file.exists(css_path))
  css_content <- paste(readLines(css_path, warn = FALSE), collapse = "\n")

  # 1. Spinner rotation animation and keyframes exist
  expect_true(grepl("\\.spinner-border", css_content))
  expect_true(grepl("spinner-border-rotate", css_content))
  expect_true(grepl("@keyframes spinner-border-rotate", css_content))
  expect_true(grepl("@-webkit-keyframes spinner-border-rotate", css_content))
  expect_true(grepl("rotate\\(360deg\\)", css_content))
  expect_true(grepl("infinite", css_content))

  # 2. Disabled buttons preserve spinner rotation
  expect_true(grepl("\\.btn:disabled \\.spinner-border", css_content))
  expect_true(grepl("button\\[disabled\\] \\.spinner-border", css_content))

  # 3. Skeleton shimmer sweep keyframes exist with GPU compositing
  expect_true(grepl("@keyframes skeleton-shimmer-sweep", css_content))
  expect_true(grepl("translateX\\(100%\\)", css_content))
  expect_true(grepl("will-change: transform", css_content))

  # 4. Reduced motion layer does NOT hide skeletons or freeze spinners
  reduced_motion_match <- regmatches(css_content, regexpr("@media \\(prefers-reduced-motion: reduce\\)[^}]+}[^}]+}", css_content))
  expect_true(length(reduced_motion_match) > 0)
  rm_block <- reduced_motion_match[1]
  expect_false(grepl("\\.skeleton-shimmer::after\\s*\\{[^}]*display:\\s*none", rm_block))
  expect_true(grepl("not\\(\\.spinner-border\\)", rm_block))
  expect_true(grepl("not\\(\\.skeleton-shimmer\\)", rm_block))
})

test_that("custom.css guarantees unrestricted viewport scrolling and modal scroll recovery", {
  css_path <- file.path("..", "..", "www", "custom.css")
  expect_true(file.exists(css_path))
  css_content <- paste(readLines(css_path, warn = FALSE), collapse = "\n")

  # 1. Root html and body scrolling rules
  expect_true(grepl("html\\s*\\{[^}]*overflow-y:\\s*auto\\s*!important", css_content))
  expect_true(grepl("body\\s*\\{[^}]*overflow-y:\\s*visible\\s*!important", css_content))
  expect_true(grepl("body:not\\(\\.modal-open\\)\\s*\\{[^}]*overflow-y:\\s*visible\\s*!important", css_content))

  # 2. Main containers allow unconstrained overflow
  expect_true(grepl("\\.container-fluid\\s*\\{[^}]*overflow:\\s*visible\\s*!important", css_content))
  expect_true(grepl("\\.app-shell\\s*\\{[^}]*overflow:\\s*visible\\s*!important", css_content))

  # 3. Mapping cards do not clip dropdowns or scroll
  expect_true(grepl("\\.mapping-workbench-card\\s*\\{[^}]*overflow:\\s*visible\\s*!important", css_content))

  # 4. Modal scroll watchdog and cleanup are present in app.R
  app_r_path <- file.path("..", "..", "app.R")
  expect_true(file.exists(app_r_path))
  app_r_content <- paste(readLines(app_r_path, warn = FALSE), collapse = "\n")
  expect_true(grepl("hidden\\.bs\\.modal", app_r_content))
  expect_true(grepl("modal-open", app_r_content))
})

test_that("button dimensions, spinner animations, and reset handlers prevent UI shrinkage and freezes", {
  css_path <- file.path("..", "..", "www", "custom.css")
  expect_true(file.exists(css_path))
  css_content <- paste(readLines(css_path, warn = FALSE), collapse = "\n")

  # 1. Global button dimension hardening to prevent collapsed pills
  expect_true(grepl("\\.btn\\s*\\{[^}]*min-height:\\s*38px\\s*!important", css_content))
  expect_true(grepl("\\.btn\\s*\\{[^}]*min-width:\\s*max-content", css_content))
  expect_true(grepl("\\.btn\\s*\\{[^}]*white-space:\\s*nowrap\\s*!important", css_content))
  expect_true(grepl("\\.btn\\s*\\{[^}]*flex-shrink:\\s*0\\s*!important", css_content))
  expect_true(grepl("\\.btn\\s*\\{[^}]*box-sizing:\\s*border-box\\s*!important", css_content))

  # 2. Button size variants (sm/lg) have guaranteed minimum heights
  expect_true(grepl("\\.btn-sm\\s*\\{[^}]*min-height:\\s*32px\\s*!important", css_content))
  expect_true(grepl("\\.btn-lg\\s*\\{[^}]*min-height:\\s*44px\\s*!important", css_content))

  # 3. Spinner border dimensions, flex protection, and high-contrast color preservation
  expect_true(grepl("\\.spinner-border\\s*\\{[^}]*display:\\s*inline-block\\s*!important", css_content))
  expect_true(grepl("\\.spinner-border\\s*\\{[^}]*flex-shrink:\\s*0\\s*!important", css_content))
  expect_true(grepl("\\.spinner-border\\s*\\{[^}]*box-sizing:\\s*border-box\\s*!important", css_content))
  expect_true(grepl("\\.btn-primary\\s+\\.spinner-border[^}]*border-color:\\s*#FFFFFF\\s*!important", css_content))

  # 4. Reusable button reset message handler registered in Shiny JS
  app_r_path <- file.path("..", "..", "app.R")
  expect_true(file.exists(app_r_path))
  app_r_content <- paste(readLines(app_r_path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  expect_true(grepl('Shiny\\.addCustomMessageHandler\\("reset_button"', app_r_content))

  # 5. Asynchronous graceful timer for upload_verifying in app.R
  expect_true(grepl("upload_verifying\\(\\)", app_r_content))
  expect_true(grepl("upload_verify_time\\(\\)", app_r_content))
  expect_true(grepl("invalidateLater", app_r_content))

  # 6. Button reset dispatched on validation early-exits
  expect_true(grepl('reset_button".*confirm_upload_health_btn', app_r_content))
  expect_true(grepl('reset_button".*confirm_mapping', app_r_content))
  expect_true(grepl('reset_button".*confirm_strategy', app_r_content))
})



