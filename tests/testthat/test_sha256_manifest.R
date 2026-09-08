source("../../R/audit.R")
source("../../R/export.R")

test_that("compute_file_sha256 computes correct 64-character hash", {
  tmp <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp), add = TRUE)
  writeLines("Cash Consortium of Yemen - Deduplication Platform", tmp)
  
  hash <- compute_file_sha256(tmp)
  expect_true(is.character(hash))
  expect_equal(nchar(hash), 64)
  expect_true(grepl("^[0-9a-f]{64}$", hash))
  
  # Determinism
  hash2 <- compute_file_sha256(tmp)
  expect_equal(hash, hash2)
})

test_that("compute_data_sha256 produces deterministic hash for data frames", {
  df1 <- data.frame(a = 1:5, b = c("A", "B", "C", "D", "E"), stringsAsFactors = FALSE)
  df2 <- data.frame(a = 1:5, b = c("A", "B", "C", "D", "E"), stringsAsFactors = FALSE)
  
  h1 <- compute_data_sha256(df1)
  h2 <- compute_data_sha256(df2)
  expect_equal(h1, h2)
  expect_equal(nchar(h1), 64)
})

test_that("build_audit_manifest_sheet populates expected fields", {
  res <- list(
    same_list_high = data.frame(x = 1),
    list_vs_master_high = data.frame(x = 1:2),
    same_list_medium = data.frame(),
    list_vs_master_medium = data.frame(x = 1)
  )
  meta <- list(
    upload_sha256 = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
    master_sha256 = "ca978112ca1bbdcafac231b39a23dc4da78608149204e8800ff8e6324cc3e244",
    manifest_id = "CCY-TEST-MANIFEST-101",
    user_email = "architect@ccyemen.org",
    user_role = "ccy_master",
    partner_name = "DRC"
  )
  
  manifest <- build_audit_manifest_sheet(res, meta)
  expect_true(is.data.frame(manifest))
  expect_equal(names(manifest), c("Property", "Value"))
  
  prop_map <- stats::setNames(manifest$Value, manifest$Property)
  expect_equal(prop_map[["Audit Manifest ID"]], "CCY-TEST-MANIFEST-101")
  expect_equal(prop_map[["Upload Dataset SHA-256 Digest"]], meta$upload_sha256)
  expect_equal(prop_map[["Master Snapshot SHA-256 Digest"]], meta$master_sha256)
  expect_equal(prop_map[["Executed By (User)"]], "architect@ccyemen.org")
  expect_equal(prop_map[["Security Role"]], "ccy_master")
  expect_equal(prop_map[["Partner Organization"]], "DRC")
  expect_equal(prop_map[["Total High Confidence Duplicates"]], "3")
  expect_equal(prop_map[["Total Medium Review Duplicates"]], "1")
})

test_that("write_dedup_workbook writes Audit_Manifest sheet", {
  res <- list(
    info = data.frame(Field = "Test", Value = "Value"),
    same_list_high = data.frame(id = 1, hoh_name = "Test", confidence = "HIGH"),
    same_list_medium = data.frame(),
    list_vs_master_high = data.frame(),
    list_vs_master_medium = data.frame()
  )
  meta <- list(
    upload_sha256 = "abc123456789",
    master_sha256 = "def987654321",
    manifest_id = "CCY-AUDIT-TEST"
  )
  
  tmp_xlsx <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp_xlsx), add = TRUE)
  
  write_dedup_workbook(res, tmp_xlsx, audit_meta = meta)
  expect_true(file.exists(tmp_xlsx))
  
  sheets <- openxlsx::getSheetNames(tmp_xlsx)
  expect_true("Audit_Manifest" %in% sheets)
  
  manifest_read <- openxlsx::read.xlsx(tmp_xlsx, sheet = "Audit_Manifest")
  expect_true(nrow(manifest_read) >= 10)
  expect_true("Upload Dataset SHA-256 Digest" %in% manifest_read$Property)
})

test_that("log_export_audit logs sha256 and manifest metadata", {
  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  old_wd <- getwd()
  setwd(tmp_dir)
  on.exit(setwd(old_wd), add = TRUE)
  
  log_export_audit(
    user_email = "test@ccy.org",
    user_role = "partner_deduplicator",
    partner_name = "NRC",
    file_name = "test_export.xlsx",
    record_count = 42L,
    upload_sha256 = "feedbeef1234",
    master_sha256 = "deadcafe5678",
    manifest_id = "CCY-MANIFEST-999"
  )
  
  log <- get_export_audit_log()
  expect_equal(nrow(log), 1)
  expect_equal(log$upload_sha256, "feedbeef1234")
  expect_equal(log$master_sha256, "deadcafe5678")
  expect_equal(log$manifest_id, "CCY-MANIFEST-999")
})
