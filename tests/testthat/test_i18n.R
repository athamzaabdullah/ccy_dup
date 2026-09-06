testthat::context("i18n-localization")

library(shiny)
library(bslib)
source("../../R/i18n.R")
source("../../R/ui_helpers.R")

test_that("tr function returns correct translations and handles fallbacks", {
  # English lookup
  expect_equal(tr("app_name", lang = "en"), "CCY Deduplication Platform")
  expect_equal(tr("btn_logout", lang = "en"), "Log out")

  # Arabic lookup
  expect_equal(tr("app_name", lang = "ar"), "منصة مطابقة وتدقيق البيانات - CCY")
  expect_equal(tr("btn_logout", lang = "ar"), "تسجيل الخروج")

  # Fallback for unknown keys
  expect_equal(tr("non_existent_key", lang = "en"), "non_existent_key")
  expect_equal(tr("non_existent_key", lang = "ar"), "non_existent_key")

  # Null and empty inputs
  expect_equal(tr(NULL), "")
  expect_equal(tr(character(0)), "")

  # Sprintf parameter interpolation
  expect_match(tr("time_mins_ago", lang = "en", 15), "15 m ago")
  expect_match(tr("time_mins_ago", lang = "ar", 15), "منذ 15 دقيقة")
  expect_match(tr("found_count", lang = "ar", 3), "3 تكرار")
})

test_that("format_relative_time_i18n formats timestamps correctly in EN and AR", {
  now <- Sys.time()
  
  # Just now (< 0.1 hours)
  expect_equal(format_relative_time_i18n(now, lang = "en"), "Just now")
  expect_equal(format_relative_time_i18n(now, lang = "ar"), "الآن")

  # 30 mins ago
  t_30m <- now - 1800
  expect_match(format_relative_time_i18n(t_30m, lang = "en"), "\\d+ m ago")
  expect_match(format_relative_time_i18n(t_30m, lang = "ar"), "منذ \\d+ دقيقة")

  # 2 hours ago
  t_2h <- now - 7200
  expect_match(format_relative_time_i18n(t_2h, lang = "en"), "2(\\.\\d)? h ago")
  expect_match(format_relative_time_i18n(t_2h, lang = "ar"), "منذ 2(\\.\\d)? ساعة")

  # 2 days ago
  t_2d <- now - 172800
  expect_match(format_relative_time_i18n(t_2d, lang = "en"), "2(\\.\\d)? d ago")
  expect_match(format_relative_time_i18n(t_2d, lang = "ar"), "منذ 2(\\.\\d)? يوم")
})

test_that("all I18N_DICT entries have valid non-empty en and ar translations", {
  expect_true(length(I18N_DICT) > 20)
  for (key in names(I18N_DICT)) {
    entry <- I18N_DICT[[key]]
    expect_true(!is.null(entry$en), info = paste("Missing EN for key:", key))
    expect_true(nchar(entry$en) > 0, info = paste("Empty EN for key:", key))
    expect_true(!is.null(entry$ar), info = paste("Missing AR for key:", key))
    expect_true(nchar(entry$ar) > 0, info = paste("Empty AR for key:", key))
  }
})

test_that("step UI helper functions render without error for both English and Arabic", {
  # Upload step
  ui_up_en <- upload_step_ui(can_fetch_master = TRUE, lang = "en")
  expect_true(inherits(ui_up_en, "shiny.tag"))
  ui_up_ar <- upload_step_ui(can_fetch_master = TRUE, lang = "ar")
  expect_true(inherits(ui_up_ar, "shiny.tag"))

  # Mapping step
  ui_map_en <- mapping_step_ui(lang = "en")
  expect_true(inherits(ui_map_en, "shiny.tag"))
  ui_map_ar <- mapping_step_ui(lang = "ar")
  expect_true(inherits(ui_map_ar, "shiny.tag"))

  # Strategy step
  ui_strat_en <- strategy_step_ui(lang = "en")
  expect_true(inherits(ui_strat_en, "shiny.tag"))
  ui_strat_ar <- strategy_step_ui(lang = "ar")
  expect_true(inherits(ui_strat_ar, "shiny.tag"))

  # Matching step
  ui_match_en <- matching_step_ui(lang = "en")
  expect_true(inherits(ui_match_en, "shiny.tag"))
  ui_match_ar <- matching_step_ui(lang = "ar")
  expect_true(inherits(ui_match_ar, "shiny.tag"))

  # Results step
  ui_res_en <- results_step_ui(lang = "en")
  expect_true(inherits(ui_res_en, "shiny.tag"))
  ui_res_ar <- results_step_ui(lang = "ar")
  expect_true(inherits(ui_res_ar, "shiny.tag"))

  # Settings step
  ui_set_en <- settings_step_ui(can_edit_token = TRUE, can_edit_form_id = TRUE, lang = "en")
  expect_true(inherits(ui_set_en, "shiny.tag"))
  ui_set_ar <- settings_step_ui(can_edit_token = TRUE, can_edit_form_id = TRUE, lang = "ar")
  expect_true(inherits(ui_set_ar, "shiny.tag"))

  # Admin step
  ui_adm_en <- admin_step_ui(lang = "en")
  expect_true(inherits(ui_adm_en, "shiny.tag"))
  ui_adm_ar <- admin_step_ui(lang = "ar")
  expect_true(inherits(ui_adm_ar, "shiny.tag"))
})
