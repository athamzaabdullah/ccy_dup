# tests/testthat/test_inversion_displacement_audit.R
context("inversion-displacement-audit")

source("../../R/config.R")
source("../../R/preprocess.R")
source("../../R/matching.R")
source("../../R/export.R")
source("../../R/activityinfo.R")
source("../../R/ttl.R")
source("../../R/audit.R")
source("../../R/diagnostics.R")

test_that("HoH and Spouse inversion is detected and scored accurately", {
  # Synthetic inverted household:
  # Upload: HoH is Husband, Spouse is Wife
  # Master: HoH is Wife, Spouse is Husband
  u <- data.frame(
    partner = "DRC",
    governorate = "Sanaa",
    district = "Ma'ain",
    subdistrict = "Rawdah",
    village = "Village 1",
    hoh_arabic_name = "محمد علي صالح العمري",
    hoh_spouse_name = "فاطمة أحمد حسن",
    phone_number = "771122334",
    hoh_ID_number = "1002003004",
    stringsAsFactors = FALSE
  )

  m <- data.frame(
    partner = "SCI",
    governorate = "Sanaa",
    district = "Ma'ain",
    subdistrict = "Rawdah",
    village = "Village 1",
    hoh_arabic_name = "فاطمة أحمد حسن",
    hoh_spouse_name = "محمد علي صالح العمري",
    phone_number = "771122334",
    hoh_ID_number = "1002003004",
    stringsAsFactors = FALSE
  )

  res <- run_dedup(upload_df = u, master_df = m)
  lm <- res$list_vs_master_exact
  expect_true(nrow(lm) >= 1)
  expect_true(lm$match_score[1] >= 85)
  expect_true(grepl("Inverted", lm$contributing_factors[1], ignore.case = TRUE))
})

test_that("IDP displacement is detected and geo penalty is mitigated", {
  # Exact matching national ID and Phone, but different governorate/district
  u <- data.frame(
    partner = "DRC",
    governorate = "Hodeidah",
    district = "Al-Hali",
    subdistrict = "Sub 1",
    village = "Village 1",
    hoh_arabic_name = "سالم عبده يحيى",
    phone_number = "773344556",
    hoh_ID_number = "5099887766",
    stringsAsFactors = FALSE
  )

  m <- data.frame(
    partner = "NRC",
    governorate = "Sanaa",
    district = "Ma'ain",
    subdistrict = "Sub 2",
    village = "Village 2",
    hoh_arabic_name = "سالم عبده يحيى",
    phone_number = "773344556",
    hoh_ID_number = "5099887766",
    stringsAsFactors = FALSE
  )

  res <- run_dedup(upload_df = u, master_df = m)
  lm <- res$list_vs_master_exact
  expect_true(nrow(lm) >= 1)
  expect_equal(lm$match_score[1], 100) # exact ID match
  expect_true(grepl("Displaced", lm$contributing_factors[1], ignore.case = TRUE))
})

test_that("Multi-shock assistance type is captured and exported beside Last Receipt Date", {
  df <- data.frame(
    match_pair_id = "LM_000001_000001",
    match_score = 95,
    confidence = "high",
    upload_row_id = 1,
    master_row_id = 1,
    "upload_1.1. Organization" = "DRC",
    master_organization = "NRC",
    master_dist_date_calc_new = "2026-04-10",
    master_dist_type = "Flood 1 Month",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  norm <- normalize_export_table(df)
  cols <- names(norm)

  expect_true("Last Receipt Date" %in% cols)
  expect_true("Assistance Type" %in% cols)
  expect_false("master_dist_date_calc_new" %in% cols)
  expect_false("master_dist_type" %in% cols)

  # Check that Assistance Type immediately follows or is adjacent to Last Receipt Date
  idx_date <- which(cols == "Last Receipt Date")
  idx_type <- which(cols == "Assistance Type")
  expect_true(abs(idx_type - idx_date) == 1)
})

test_that("extract_lean_master creates lean dataframe with only essential fields", {
  raw <- data.frame(
    "@id" = "rec_001",
    "record_id" = "rec_001",
    "QA_CODE_SN" = "QA-101",
    "organization" = "DRC",
    "Main Form Partner Batch Code" = "B_01",
    "hoh_arabic_name" = "علي أحمد",
    "phone_number" = "771111111",
    "governorate" = "Sanaa",
    "district" = "Ma'ain",
    "Dist_Type" = "Flood 1 Month",
    "Random_Survey_Question_99" = "Irrelevant text",
    "Food_Consumption_Detail_1" = "Bread",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  lean <- extract_lean_master(raw)
  expect_true("hoh_arabic_name" %in% names(lean))
  expect_true("Dist_Type" %in% names(lean))
  expect_true("QA_CODE_SN" %in% names(lean))
  expect_false("Random_Survey_Question_99" %in% names(lean))
  expect_false("Food_Consumption_Detail_1" %in% names(lean))
})

test_that("cleanup_expired_payloads correctly deletes expired files and preserves fresh files", {
  tmp_dir <- file.path(getwd(), "tmp", "jobs", "payloads")
  if (!dir.exists(tmp_dir)) dir.create(tmp_dir, recursive = TRUE)

  old_file <- file.path(tmp_dir, "test_old_upload.rds")
  fresh_file <- file.path(tmp_dir, "test_fresh_upload.rds")

  saveRDS(data.frame(x = 1), old_file)
  saveRDS(data.frame(x = 2), fresh_file)

  # Artificially age old_file to 20 days ago
  past_time <- Sys.time() - (20 * 24 * 3600)
  Sys.setFileTime(old_file, past_time)

  res <- cleanup_expired_payloads(max_age_days = 14)
  expect_false(file.exists(old_file))
  expect_true(file.exists(fresh_file))

  # Cleanup fresh test file
  if (file.exists(fresh_file)) unlink(fresh_file)
})

test_that("log_export_audit logs transactions and get_export_audit_log reads them", {
  unique_job <- paste0("test_audit_job_", as.integer(runif(1, 10000, 99999)))
  log_export_audit(
    user_email = "meal_officer@drc.ngo",
    user_role = "partner_deduplicator",
    partner_name = "DRC",
    file_name = "test_export.xlsx",
    record_count = 150L,
    pii_masked = TRUE,
    job_id = unique_job
  )

  audit_log <- get_export_audit_log()
  expect_true(nrow(audit_log) >= 1)
  expect_true(unique_job %in% audit_log$job_id)
  row <- audit_log[audit_log$job_id == unique_job, ][1, ]
  expect_equal(row$user_email, "meal_officer@drc.ngo")
  expect_true(row$pii_masked)
  expect_equal(row$record_count, 150L)
})

test_that("check_upload_hygiene detects scientific notation, blank rows, and placeholders", {
  dirty_df <- data.frame(
    "Full Name" = c("Ahmed Ali", "", "Khaled Salem"),
    "Phone" = c("7.71E+08", "", "771234567"),
    "National ID" = c("5010454023", "", "11111111"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  res <- check_upload_hygiene(dirty_df)
  expect_equal(nrow(res$clean_df), 2) # Empty row removed
  expect_true(res$issue_count >= 1)
  warn_text <- paste(res$warnings, collapse = " ")
  expect_true(grepl("scientific notation", warn_text, ignore.case = TRUE))
  expect_true(grepl("empty row", warn_text, ignore.case = TRUE))

  # Test structured checks list
  expect_true(is.list(res$checks))
  expect_equal(res$checks$sci_notation$status, "warn")
  expect_equal(res$checks$empty_rows$status, "info")
  expect_equal(res$checks$dup_headers$status, "pass")

  # Clean dataset checks
  clean_df <- data.frame(
    "Full Name" = c("Ahmed Ali", "Khaled Salem"),
    "Phone" = c("771234567", "779876543"),
    "National ID" = c("5010454023", "1020304050"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  clean_res <- check_upload_hygiene(clean_df)
  expect_equal(clean_res$issue_count, 0)
  expect_equal(clean_res$checks$sci_notation$status, "pass")
  expect_equal(clean_res$checks$empty_rows$status, "pass")
  expect_equal(clean_res$checks$dup_headers$status, "pass")
  expect_equal(clean_res$checks$placeholders$status, "pass")
})

