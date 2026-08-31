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

test_that("map_activityinfo_columns maps exact ActivityInfo form labels and codes accurately", {
  if (file.exists("../../R/preprocess.R")) source("../../R/preprocess.R")
  
  raw_ai_df <- data.frame(
    `3.1. Head of household (HoH) Name (Arabic)` = "محمد علي أحمد",
    `3.3. Head of HH's Spouse Name` = "فاطمة حسن",
    `3.11 What is the main form of ID that the Head of Household uses?` = "National ID",
    `3.12 What is the Head of Household's ID number?` = "12345678901",
    `3.5. Head of HH Gender` = "Male",
    `3.4. Age of the head of the household?` = "45",
    `3.2. Head of HH Marital Status` = "Married",
    `2.1. Primary Phone Number:` = "777123456",
    `Governorate Label` = "Marib",
    `District Label` = "Marib City",
    `Subdistrict Label` = "City Center",
    `1.14. Village` = "Al-Rawdah",
    `Partner Prefix` = "DRC",
    `Dist_Date_Calc_New` = "2024-05-15",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  mapped <- map_activityinfo_columns(raw_ai_df)
  
  expect_true("hoh_arabic_name" %in% names(mapped))
  expect_equal(mapped$hoh_arabic_name[1], "محمد علي أحمد")
  
  expect_true("hoh_spouse_name" %in% names(mapped))
  expect_equal(mapped$hoh_spouse_name[1], "فاطمة حسن")
  
  expect_true("phone_number" %in% names(mapped))
  expect_equal(mapped$phone_number[1], "777123456")
  
  expect_true("hoh_ID_number" %in% names(mapped))
  expect_equal(mapped$hoh_ID_number[1], "12345678901")
  
  expect_true("id_type" %in% names(mapped))
  expect_true("sex" %in% names(mapped))
  expect_true("age" %in% names(mapped))
  expect_true("marital_status" %in% names(mapped))
  expect_true("partner" %in% names(mapped))
  expect_equal(mapped$partner[1], "DRC")
  
  expect_true("governorate" %in% names(mapped))
  expect_true("district" %in% names(mapped))
  expect_true("subdistrict" %in% names(mapped))
  expect_true("village" %in% names(mapped))
  expect_true("dist_date_calc_new" %in% names(mapped))
})

