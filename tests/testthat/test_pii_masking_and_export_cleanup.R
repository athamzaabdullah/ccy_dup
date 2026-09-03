context("pii-masking-and-export-cleanup")

source("../../R/config.R")
source("../../R/preprocess.R")
source("../../R/matching.R")
source("../../R/export.R")

test_that("smart PII masking masks when partners differ and unmasks when partners match", {
  u <- data.frame(
    organization = c("DRC", "SCI"),
    governorate = c("Sanaa", "Aden"),
    district = c("D1", "D2"),
    sub_district = c("SD1", "SD2"),
    village = c("V1", "V2"),
    hoh_ID_number = c("1234567890", "9876543210"),
    phone_number = c("771112233", "774445566"),
    secondary_phone_number = c("770001122", "770003344"),
    hoh_arabic_name = c("محمد علي صالح", "أحمد سالم حسن"),
    stringsAsFactors = FALSE
  )

  m <- data.frame(
    organization = c("DRC", "DRC"), # Master records are both DRC
    governorate = c("Sanaa", "Aden"),
    district = c("D1", "D2"),
    sub_district = c("SD1", "SD2"),
    village = c("V1", "V2"),
    hoh_ID_number = c("1234567890", "9876543210"),
    primary_phone_number = c("771112233", "774445566"),
    secondary_phone_number = c("770001122", "770003344"),
    hoh_arabic_name = c("محمد علي صالح", "أحمد سالم حسن"),
    stringsAsFactors = FALSE
  )

  res <- run_dedup(upload_df = u, master_df = m)
  lm <- res$list_vs_master_exact

  expect_true(nrow(lm) == 2)
  # Row 1: DRC vs DRC (same partner) -> UNMASKED
  row_drc <- lm[lm$upload_row_id == 1, ]
  expect_equal(row_drc$master_hoh_ID_number, "1234567890")
  expect_equal(row_drc$master_primary_phone_number, "771112233")
  expect_equal(row_drc$master_secondary_phone_number, "770001122")

  # Row 2: SCI vs DRC (different partner) -> MASKED
  row_sci <- lm[lm$upload_row_id == 2, ]
  expect_equal(row_sci$master_hoh_ID_number, "*******210")
  expect_equal(row_sci$master_primary_phone_number, "******566")
  expect_equal(row_sci$master_secondary_phone_number, "******344")
})

test_that("export cleanup removes forbidden columns from excel sheets", {
  u <- data.frame(
    organization = c("DRC", "DRC"),
    governorate = c("Sanaa", "Sanaa"),
    district = c("D1", "D1"),
    sub_district = c("SD1", "SD1"),
    village = c("V1", "V1"),
    hoh_ID_number = c("1234567890", "1234567890"),
    phone_number = c("771112233", "771112233"),
    hoh_arabic_name = c("محمد علي صالح", "محمد علي صالح"),
    "Main Distribution Donor" = c("ECHO", "ECHO"),
    "Main Form Partner Batch Code" = c("B01", "B01"),
    "3.1. Head of household (HoH) Name (Arabic)" = c("محمد علي صالح", "محمد علي صالح"),
    "3.3. Head of HH's Spouse Name" = c("فاطمة أحمد", "فاطمة أحمد"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  res <- run_dedup(upload_df = u, master_df = u)
  tmp_file <- tempfile(fileext = ".xlsx")
  write_dedup_workbook(res, tmp_file)

  forbidden <- c(
    "upload_Main Distribution Donor_a",
    "upload_Main Distribution Donor_b",
    "upload_Main Distribution Donor",
    "upload_partner",
    "upload_Main Form Partner Batch Code",
    "upload_Main Form Partner Batch Code_a",
    "upload_Main Form Partner Batch Code_b",
    "upload_3.1. Head of household (HoH) Name (Arabic)",
    "upload_3.1. Head of household (HoH) Name (Arabic)_a",
    "upload_3.1. Head of household (HoH) Name (Arabic)_b",
    "upload_3.3. Head of HH's Spouse Name",
    "upload_3.3. Head of HH's Spouse Name_a",
    "upload_3.3. Head of HH's Spouse Name_b"
  )

  for (s in openxlsx::getSheetNames(tmp_file)) {
    sheet_data <- openxlsx::read.xlsx(tmp_file, sheet = s)
    for (col_name in forbidden) {
      expect_false(col_name %in% names(sheet_data), info = paste("Column", col_name, "found in sheet", s))
    }
  }

  if (file.exists(tmp_file)) unlink(tmp_file)
})

test_that("generic and invalid IDs and phone numbers are excluded from matching", {
  # Generic IDs
  expect_true(is_generic_or_invalid_id("1111"))
  expect_true(is_generic_or_invalid_id("111111"))
  expect_true(is_generic_or_invalid_id("00000000"))
  expect_true(is_generic_or_invalid_id("123456789"))
  expect_true(is_generic_or_invalid_id("100000"))
  expect_true(is_generic_or_invalid_id("2023"))
  expect_true(is_generic_or_invalid_id("لايوجد"))
  expect_true(is_generic_or_invalid_id("تعريف"))
  expect_true(is_generic_or_invalid_id("0"))
  expect_true(is_generic_or_invalid_id("123"))

  # Real IDs
  expect_false(is_generic_or_invalid_id("5010454023"))
  expect_false(is_generic_or_invalid_id("05010373576"))
  expect_false(is_generic_or_invalid_id("11010159768"))
  expect_false(is_generic_or_invalid_id("6900520"))

  # Generic Phones
  expect_true(is_generic_or_invalid_phone("777777777"))
  expect_true(is_generic_or_invalid_phone("711111111"))
  expect_true(is_generic_or_invalid_phone("111111111"))
  expect_true(is_generic_or_invalid_phone("733333333"))
  expect_true(is_generic_or_invalid_phone("700000000"))
  expect_true(is_generic_or_invalid_phone("000000000"))
  expect_true(is_generic_or_invalid_phone("123456789"))
  expect_true(is_generic_or_invalid_phone("777777771"))
  expect_true(is_generic_or_invalid_phone("700000"))
  expect_true(is_generic_or_invalid_phone("100000"))

  # Real Phones
  expect_false(is_generic_or_invalid_phone("771234509"))
  expect_false(is_generic_or_invalid_phone("738531332"))
  expect_false(is_generic_or_invalid_phone("715315606"))
  expect_false(is_generic_or_invalid_phone("+967-774981581"))

  # Matching test: records with identical dummy ID or dummy phone but different names DO NOT MATCH
  u_dummy <- data.frame(
    organization = c("DRC", "DRC"),
    governorate = c("Sanaa", "Aden"),
    district = c("D1", "D2"),
    sub_district = c("SD1", "SD2"),
    village = c("V1", "V2"),
    hoh_ID_number = c("1111", "111111"),
    phone_number = c("777777777", "00000000"),
    hoh_arabic_name = c("خالد أحمد منصور", "فاطمة سعيد ناصر"),
    stringsAsFactors = FALSE
  )
  m_dummy <- data.frame(
    organization = c("SCI", "NRC"),
    governorate = c("Taiz", "Ibb"),
    district = c("D3", "D4"),
    sub_district = c("SD3", "SD4"),
    village = c("V3", "V4"),
    hoh_ID_number = c("1111", "111111"),
    primary_phone_number = c("777777777", "00000000"),
    hoh_arabic_name = c("عمر يحيى القاسمي", "زينب عبد الله"),
    stringsAsFactors = FALSE
  )

  res_dummy <- run_dedup(upload_df = u_dummy, master_df = m_dummy)
  lm_matches <- rbind(res_dummy$list_vs_master_exact, res_dummy$list_vs_master_fuzzy)
  expect_equal(nrow(lm_matches), 0)
})

test_that("ccy_master bypasses masking while partner roles enforce masking on cross-partner records", {
  u <- data.frame(
    organization = "DRC",
    governorate = "Sanaa",
    district = "D1",
    sub_district = "SD1",
    village = "V1",
    hoh_ID_number = "5010454023",
    phone_number = "771234567",
    hoh_arabic_name = "محمد علي صالح",
    stringsAsFactors = FALSE
  )
  m <- data.frame(
    organization = "SCI", # Different partner!
    governorate = "Sanaa",
    district = "D1",
    sub_district = "SD1",
    village = "V1",
    hoh_ID_number = "5010454023",
    primary_phone_number = "771234567",
    hoh_arabic_name = "محمد علي صالح",
    stringsAsFactors = FALSE
  )

  # 1. ccy_master role: BYPASSES masking
  res_master <- run_dedup(upload_df = u, master_df = m, user_role = "ccy_master")
  lm_master <- res_master$list_vs_master_exact
  expect_true(nrow(lm_master) >= 1)
  expect_equal(lm_master$master_hoh_ID_number[1], "5010454023")
  expect_equal(lm_master$master_primary_phone_number[1], "771234567")

  # 2. Partner role: MASKS cross-partner PII
  res_partner <- run_dedup(upload_df = u, master_df = m, user_role = "partner_user")
  lm_partner <- res_partner$list_vs_master_exact
  expect_true(nrow(lm_partner) >= 1)
  expect_equal(lm_partner$master_hoh_ID_number[1], "*******023")
  expect_equal(lm_partner$master_primary_phone_number[1], "******567")
})

test_that("export sorts columns by relativity placing related upload and master columns side by side", {
  df <- data.frame(
    match_pair_id = "LM_1_1",
    match_score = 100,
    confidence = "high",
    upload_row_id = 1,
    master_row_id = 1,
    "upload_1.1. Organization" = "DRC",
    "upload_1.11. Governorate" = "Sanaa",
    "upload_1.12. District" = "Ma'ain",
    "upload_1.13. Sub-District" = "Al-Rawdah",
    "upload_1.14. Village" = "Village A",
    "upload_hoh_arabic_name" = "محمد علي صالح",
    "upload_3.5. Head of HH Gender" = "Male",
    "upload_hoh_ID_number" = "5010454023",
    "upload_phone_number" = "771234567",
    "upload_Batch_ID" = "B99",
    master_organization = "SCI",
    master_governorate = "Sanaa",
    master_district = "Ma'ain",
    master_sub_district = "Al-Rawdah",
    master_village = "Village A",
    master_hoh_arabic_name = "محمد علي صالح",
    master_hoh_sex = "Male",
    master_hoh_ID_number = "5010454023",
    master_primary_phone_number = "771234567",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  norm_df <- normalize_export_table(df)
  cols <- names(norm_df)

  # Check that upload_3.5. Head of HH Gender is immediately followed by master_hoh_sex
  idx_u_gender <- which(cols == "upload_3.5. Head of HH Gender")
  idx_m_sex <- which(cols == "master_hoh_sex")
  expect_equal(idx_m_sex, idx_u_gender + 1)

  # Check that upload_1.1. Organization is immediately followed by master_organization
  idx_u_org <- which(cols == "upload_1.1. Organization")
  idx_m_org <- which(cols == "master_organization")
  expect_equal(idx_m_org, idx_u_org + 1)

  # Check that upload_hoh_ID_number is immediately followed by master_hoh_ID_number
  idx_u_id <- which(cols == "upload_hoh_ID_number")
  idx_m_id <- which(cols == "master_hoh_ID_number")
  expect_equal(idx_m_id, idx_u_id + 1)

  # Check that other upload fields (like upload_Batch_ID) are placed at the end
  idx_u_batch <- which(cols == "upload_Batch_ID")
  expect_true(idx_u_batch > idx_m_sex)
})

