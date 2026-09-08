source("../../R/preprocess.R")
source("../../R/matching.R")
source("../../R/audit.R")
source("../../R/activityinfo.R")
source("../../R/modules/mod_upload.R")
source("../../R/modules/mod_strategy.R")
source("../../R/modules/mod_results.R")

test_that("validate_upload_dataset rejects invalid or unrecognizable tables", {
  expect_false(validate_upload_dataset(NULL)$valid)
  expect_false(validate_upload_dataset(data.frame())$valid)
  
  unrec_df <- data.frame(color = c("red", "blue"), price = c(10, 20))
  expect_false(validate_upload_dataset(unrec_df)$valid)
  
  rec_df <- data.frame(hoh_arabic_name = "علي محمد", phone_number = "777123456", stringsAsFactors = FALSE)
  v_res <- validate_upload_dataset(rec_df)
  expect_true(v_res$valid)
  expect_equal(v_res$rows, 1)
  expect_true(v_res$has_name)
  expect_true(v_res$has_phone)
})

test_that("validate_matching_strategy enforces strict parameter bounds", {
  # high <= medium
  res1 <- validate_matching_strategy(80, 85, 500)
  expect_false(res1$valid)
  
  # high out of bounds
  res2 <- validate_matching_strategy(105, 75, 500)
  expect_false(res2$valid)
  
  # medium out of bounds
  res3 <- validate_matching_strategy(90, -5, 500)
  expect_false(res3$valid)
  
  # max_candidates out of bounds
  res4 <- validate_matching_strategy(90, 75, 40)
  expect_false(res4$valid)
  res5 <- validate_matching_strategy(90, 75, 3000)
  expect_false(res5$valid)
  
  # valid configuration
  res_ok <- validate_matching_strategy("90", "75", "500")
  expect_true(res_ok$valid)
  expect_equal(res_ok$high, 90)
  expect_equal(res_ok$medium, 75)
  expect_equal(res_ok$max_candidates, 500L)
})

test_that("compile_export_dossier_meta builds compliant cryptographic metadata", {
  mock_res <- list(
    summary = list(total_pairs = 15L),
    same_list_high = data.frame(id = 1:15)
  )
  mock_auth <- list(
    email = "lead.architect@ccy.org",
    role = "ccy_master",
    partner_name = "DRC"
  )
  mock_upload <- data.frame(x = 1:10)
  
  meta <- compile_export_dossier_meta(
    upload_df = mock_upload,
    auth_user = mock_auth,
    job_result = mock_res
  )
  
  expect_true(grepl("^CCY-AUDIT-", meta$manifest_id))
  expect_equal(meta$user_email, "lead.architect@ccy.org")
  expect_equal(meta$user_role, "ccy_master")
  expect_equal(meta$partner_name, "DRC")
  expect_equal(meta$record_count, 15L)
  expect_equal(nchar(meta$upload_sha256), 64)
  expect_equal(meta$master_sha256, "Not Available")
})
