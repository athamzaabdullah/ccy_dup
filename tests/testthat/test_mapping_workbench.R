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
