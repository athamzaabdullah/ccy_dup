test_that("parse_flexible_date correctly parses dates from multiple formats", {
  source("../../R/preprocess.R")
  
  # Standard ISO
  d1 <- parse_flexible_date(c("2024-05-15", "2023-12-01"))
  expect_equal(as.character(d1), c("2024-05-15", "2023-12-01"))
  
  # Slash formats (DD/MM/YYYY)
  d2 <- parse_flexible_date(c("15/05/2024", "01/12/2023"))
  expect_equal(as.character(d2), c("2024-05-15", "2023-12-01"))
  
  # Excel serial numbers
  d3 <- parse_flexible_date(c("45427")) # 2024-05-15 in Excel serial
  expect_equal(as.character(d3), "2024-05-15")
  
  # Missing / NA
  d4 <- parse_flexible_date(c("", "NA", "N/A", "null", NA))
  expect_true(all(is.na(d4)))
})

test_that("run_dedup filters master records by MPCA recency window when filter_recent_mpca is TRUE", {
  source("../../R/config.R")
  source("../../R/preprocess.R")
  source("../../R/matching.R")
  
  ref_date <- as.Date("2024-06-01")
  
  # Upload DF with 2 beneficiaries
  upload_df <- data.frame(
    hoh_ID_number = c("12345678901", "98765432109"),
    hoh_arabic_name = c("محمد علي أحمد حسن", "فاطمة صالح عمر سعيد"),
    phone_number = c("777123456", "771987654"),
    stringsAsFactors = FALSE
  )
  
  # Master DF with 2 matching beneficiaries:
  # Beneficiary 1: MPCA distribution was 2 months ago (2024-04-01) -> within 6 months
  # Beneficiary 2: MPCA distribution was 8 months ago (2023-10-01) -> > 6 months ago
  master_df <- data.frame(
    hoh_ID_number = c("12345678901", "98765432109"),
    hoh_arabic_name = c("محمد علي أحمد حسن", "فاطمة صالح عمر سعيد"),
    phone_number = c("777123456", "771987654"),
    Dist_Date_Calc_New = c("2024-04-01", "2023-10-01"),
    stringsAsFactors = FALSE
  )
  
  # Without filter: both beneficiaries match
  res_all <- run_dedup(
    upload_df = upload_df,
    master_df = master_df,
    filter_recent_mpca = FALSE,
    match_fields = c("hoh_ID_number", "hoh_arabic_name", "phone_number")
  )
  expect_equal(nrow(res_all$list_vs_master_exact), 2)
  
  # With MPCA filter enabled (< 6 months window relative to 2024-06-01):
  # Beneficiary 2 (> 6 months ago) must be excluded from master before matching!
  res_filtered <- run_dedup(
    upload_df = upload_df,
    master_df = master_df,
    filter_recent_mpca = TRUE,
    mpca_window_months = 6,
    mpca_reference_date = ref_date,
    match_fields = c("hoh_ID_number", "hoh_arabic_name", "phone_number")
  )
  
  expect_equal(nrow(res_filtered$list_vs_master_exact), 1)
  expect_equal(res_filtered$list_vs_master_exact$upload_hoh_ID_number[1], "12345678901")
})
