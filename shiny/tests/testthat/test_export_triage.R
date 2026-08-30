library(testthat)

source("../../R/config.R")
source("../../R/preprocess.R")
source("../../R/matching.R")
source("../../R/export.R")

test_that("write_dedup_workbook creates valid workbook with triage decisions", {
  upload_data <- data.frame(
    partner = c("DRC", "DRC"),
    hoh_arabic_name = c("محمد علي احمد", "محمد علي احمد"),
    hoh_ID_number = c("1234567890", "1234567890"),
    phone_number = c("777111222", "777111222"),
    governorate = c("Marib", "Marib"),
    district = c("City", "City"),
    stringsAsFactors = FALSE
  )
  
  master_data <- data.frame(
    partner = c("DRC"),
    hoh_arabic_name = c("محمد علي احمد"),
    hoh_ID_number = c("1234567890"),
    phone_number = c("777111222"),
    governorate = c("Marib"),
    district = c("City"),
    stringsAsFactors = FALSE
  )

  res <- run_dedup(
    upload_df = upload_data,
    master_df = master_data,
    fuzzy_high_threshold = 90,
    fuzzy_medium_threshold = 75
  )

  triage_decisions <- list(
    "LM_1_1" = list(status = "Confirmed Duplicate", notes = "Field team confirmed duplicate household", reviewer = "im_officer@drc.ngo", timestamp = "2026-08-30 12:00:00")
  )

  tmp_file <- file.path(tempdir(), "test_export_with_triage.xlsx")
  on.exit(if (file.exists(tmp_file)) unlink(tmp_file), add = TRUE)

  expect_error(write_dedup_workbook(res, tmp_file, triage_decisions = triage_decisions), NA)
  expect_true(file.exists(tmp_file))
  expect_gt(file.info(tmp_file)$size, 1000)
})
