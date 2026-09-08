testthat::context("parquet-cache")

source(file.path("..", "..", "R", "config.R"))
source(file.path("..", "..", "R", "activityinfo.R"))

test_that("load_master_lean reads parquet and supports column projection", {
  tmp_dir <- tempfile("snap_test")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE))

  df <- data.frame(
    record_id = c("rec_1", "rec_2"),
    hoh_ID_number = c("12345678901", "98765432109"),
    hoh_arabic_name = c("محمد علي", "احمد حسن"),
    district = c("Ma'rib", "Atta"),
    extra_col = c("unneeded_1", "unneeded_2"),
    stringsAsFactors = FALSE
  )

  snap_path <- save_master_snapshot(df, snap_dir = tmp_dir)
  expect_true(file.exists(snap_path))

  parquet_files <- list.files(tmp_dir, pattern = "\\.parquet$", full.names = TRUE)
  expect_true(length(parquet_files) >= 1)

  # Full read
  loaded_full <- load_master_lean(snap_path)
  expect_equal(nrow(loaded_full), 2)
  expect_true("record_id" %in% names(loaded_full))

  # Column projected read
  loaded_proj <- load_master_lean(snap_path, columns = c("record_id", "hoh_ID_number"))
  expect_equal(nrow(loaded_proj), 2)
  expect_true(all(c("record_id", "hoh_ID_number") %in% names(loaded_proj)))
  expect_false("extra_col" %in% names(loaded_proj))
})
