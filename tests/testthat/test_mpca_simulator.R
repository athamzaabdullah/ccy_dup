source("../../R/preprocess.R")
source("../../R/matching.R")

test_that("simulate_mpca_window handles empty or NULL datasets gracefully", {
  res_null <- simulate_mpca_window(NULL)
  expect_equal(res_null$total_records, 0)
  expect_equal(res_null$in_window, 0)
  expect_equal(res_null$out_of_window, 0)
  
  res_empty <- simulate_mpca_window(data.frame())
  expect_equal(res_empty$total_records, 0)
})

test_that("simulate_mpca_window handles dataset without date column", {
  df <- data.frame(id = 1:5, name = letters[1:5], stringsAsFactors = FALSE)
  res <- simulate_mpca_window(df, window_months = 6)
  expect_equal(res$total_records, 5)
  expect_equal(res$in_window, 5)
  expect_equal(res$out_of_window, 0)
  expect_true(is.na(res$date_field))
})

test_that("simulate_mpca_window correctly segments cohorts and windows", {
  today <- as.Date("2026-06-01")
  
  df <- data.frame(
    id = 1:6,
    Dist_Date_Calc_New = c(
      "2026-05-15", # ~0.5 months ago (c0_3, in-window for 6m)
      "2026-04-01", # ~2 months ago (c0_3, in-window for 6m)
      "2026-01-15", # ~4.5 months ago (c4_6, in-window for 6m)
      "2025-11-01", # ~7 months ago (c7_12, OUT of 6m window, IN for 9m)
      "2025-08-01", # ~10 months ago (c7_12, OUT of 6m window, OUT for 9m)
      "2024-01-01"  # > 2 years ago (c_over12, OUT of all windows)
    ),
    stringsAsFactors = FALSE
  )
  
  # Standard 6-month CCY MPCA cycle
  sim6 <- simulate_mpca_window(df, window_months = 6, ref_date = today)
  expect_equal(sim6$total_records, 6)
  expect_equal(sim6$records_with_date, 6)
  expect_equal(sim6$in_window, 3)
  expect_equal(sim6$out_of_window, 3)
  expect_equal(sim6$pct_in_window, 50.0)
  expect_equal(sim6$pct_out_of_window, 50.0)
  
  # Check cohorts
  expect_equal(sim6$cohorts$c0_3, 2)
  expect_equal(sim6$cohorts$c4_6, 1)
  expect_equal(sim6$cohorts$c7_12, 2)
  expect_equal(sim6$cohorts$c_over12, 1)
  
  # 3-month restricted lockout window
  sim3 <- simulate_mpca_window(df, window_months = 3, ref_date = today)
  expect_equal(sim3$in_window, 2)
  expect_equal(sim3$out_of_window, 4)
  
  # 9-month extended window
  sim9 <- simulate_mpca_window(df, window_months = 9, ref_date = today)
  expect_equal(sim9$in_window, 4)
  expect_equal(sim9$out_of_window, 2)
})
