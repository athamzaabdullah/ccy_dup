library(testthat)

# Source files needed for mapping tests
if (file.exists("../../R/config.R")) source("../../R/config.R")
if (file.exists("../../R/mapping.R")) source("../../R/mapping.R")

test_that("get_field_bilingual_label returns expected bilingual formatting", {
  lbl <- get_field_bilingual_label("hoh_arabic_name")
  expect_true(grepl("اسم رب الأسرة", lbl))
  expect_true(grepl("Head of Household Name", lbl))

  lbl_phone <- get_field_bilingual_label("phone_number")
  expect_true(grepl("الهاتف الأساسي", lbl_phone))

  lbl_unknown <- get_field_bilingual_label("custom_field_xyz")
  expect_equal(lbl_unknown, "custom_field_xyz")
})

test_that("mapping presets can be saved and loaded accurately", {
  tmp_preset_file <- file.path(tempdir(), "test_mapping_presets.json")
  
  old_path <- config$paths$mapping_presets
  config$paths$mapping_presets <<- tmp_preset_file
  on.exit({
    config$paths$mapping_presets <<- old_path
    if (file.exists(tmp_preset_file)) unlink(tmp_preset_file)
  }, add = TRUE)

  expect_equal(load_mapping_presets(), list())

  mapping_to_save <- list(
    hoh_arabic_name = "full_name_ar",
    phone_number = "mobile_no",
    hoh_ID_number = "national_identity_card"
  )
  
  res <- save_mapping_preset("Partner_A", mapping_to_save)
  expect_true(res)

  loaded <- load_mapping_presets()
  expect_true("Partner_A" %in% names(loaded))
  expect_equal(loaded$Partner_A$hoh_arabic_name, "full_name_ar")
  expect_equal(loaded$Partner_A$phone_number, "mobile_no")
  expect_equal(loaded$Partner_A$hoh_ID_number, "national_identity_card")

  mapping_b <- list(hoh_arabic_name = "Beneficiary_Name")
  res2 <- save_mapping_preset("Partner_B", mapping_b)
  expect_true(res2)

  loaded2 <- load_mapping_presets()
  expect_equal(length(loaded2), 2)
  expect_true(all(c("Partner_A", "Partner_B") %in% names(loaded2)))
})
