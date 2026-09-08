source("../../R/activityinfo.R")

test_that("activityinfo_find_id_col detects standard ID columns", {
  expect_equal(activityinfo_find_id_col(data.frame(`@id` = "1", x = 2, check.names = FALSE)), "@id")
  expect_equal(activityinfo_find_id_col(data.frame(`_id` = "1", x = 2, check.names = FALSE)), "_id")
  expect_equal(activityinfo_find_id_col(data.frame(record_id = "1", x = 2)), "record_id")
  expect_equal(activityinfo_find_id_col(data.frame(QA_Code_SN = "1", x = 2)), "QA_Code_SN")
  expect_equal(activityinfo_find_id_col(data.frame(X.id = "1", x = 2)), "X.id")
  expect_null(activityinfo_find_id_col(data.frame(foo = 1, bar = 2)))
})

test_that("activityinfo_merge_delta handles empty datasets gracefully", {
  df <- data.frame(record_id = c("A1", "A2"), val = c(10, 20), stringsAsFactors = FALSE)
  
  # Empty delta
  res1 <- activityinfo_merge_delta(df, data.frame())
  expect_equal(nrow(res1$data), 2)
  expect_equal(res1$n_inserted, 0)
  expect_equal(res1$n_updated, 0)
  
  # Empty base
  res2 <- activityinfo_merge_delta(data.frame(), df)
  expect_equal(nrow(res2$data), 2)
  expect_equal(res2$n_inserted, 2)
  expect_equal(res2$n_updated, 0)
})

test_that("activityinfo_merge_delta correctly updates existing and inserts new rows", {
  base_df <- data.frame(
    record_id = c("REC_001", "REC_002", "REC_003"),
    name = c("Ali Ahmed", "Fatima Saleh", "Hassan Omar"),
    phone = c("777111222", "733444555", "711999888"),
    stringsAsFactors = FALSE
  )
  
  # Delta modifies REC_002 and adds REC_004
  delta_df <- data.frame(
    record_id = c("REC_002", "REC_004"),
    name = c("Fatima Saleh Updated", "Zaid Yahya"),
    phone = c("733999999", "777000111"),
    stringsAsFactors = FALSE
  )
  
  merge_res <- activityinfo_merge_delta(base_df, delta_df)
  expect_equal(merge_res$n_updated, 1)
  expect_equal(merge_res$n_inserted, 1)
  expect_equal(nrow(merge_res$data), 4)
  
  # Check that REC_002 is updated
  row_002 <- merge_res$data[merge_res$data$record_id == "REC_002", ]
  expect_equal(row_002$name, "Fatima Saleh Updated")
  expect_equal(row_002$phone, "733999999")
  
  # Check that REC_001 and REC_003 are retained
  expect_true("REC_001" %in% merge_res$data$record_id)
  expect_true("REC_003" %in% merge_res$data$record_id)
  
  # Check that REC_004 is added
  expect_true("REC_004" %in% merge_res$data$record_id)
})

test_that("activityinfo_merge_delta integrates with save_master_snapshot & load_master_lean", {
  tmp_dir <- tempfile("snap_test_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  base_df <- data.frame(
    record_id = c("REC_1", "REC_2"),
    hoh_arabic_name = c("محمد علي", "أحمد سالم"),
    hoh_id_number = c("10101", "20202"),
    governorate = c("Sana'a", "Aden"),
    district = c("Ma'ain", "Craiter"),
    stringsAsFactors = FALSE
  )
  
  initial_snap <- save_master_snapshot(base_df, snap_dir = tmp_dir)
  expect_true(file.exists(initial_snap))
  
  delta_df <- data.frame(
    record_id = c("REC_2", "REC_3"),
    hoh_arabic_name = c("أحمد سالم باحاج", "عمر خالد"),
    hoh_id_number = c("20202", "30303"),
    governorate = c("Aden", "Taizz"),
    district = c("Craiter", "Al-Qahirah"),
    stringsAsFactors = FALSE
  )
  
  loaded_base <- readRDS(initial_snap)
  merged <- activityinfo_merge_delta(loaded_base, delta_df)
  new_snap <- save_master_snapshot(merged$data, snap_dir = tmp_dir)
  
  expect_true(file.exists(new_snap))
  
  # Test loading via lean Parquet loader with column projection
  lean <- load_master_lean(new_snap, columns = c("record_id", "hoh_arabic_name"))
  expect_equal(nrow(lean), 3)
  expect_true("REC_3" %in% lean$record_id)
  
  rec2 <- lean[lean$record_id == "REC_2", ]
  expect_equal(rec2$hoh_arabic_name, "أحمد سالم باحاج")
})
