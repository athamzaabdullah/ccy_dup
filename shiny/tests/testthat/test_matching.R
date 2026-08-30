context("matching-workflow")

# Assuming tests are run from the shiny/ folder
source("../../R/config.R")
source("../../R/preprocess.R")
source("../../R/matching.R")

build_upload <- function() {
  data.frame(
    organization = c("Org A", "Org A", "Org B"),
    governorate = c("Sana'a", "Sanaa", "Aden"),
    district = c("D1", "D1", "D2"),
    sub_district = c("SD1", "SD1", "SD2"),
    village = c("V1", "V1", "V2"),
    hoh_ID_number = c("12345", "12345", "99999"),
    primary_phone_number = c("+967-777111222", "777111222", "700000000"),
    secondary_phone_number = c("", "", ""),
    hoh_arabic_name = c("???? ???", "????  ???", "???? ????"),
    hoh_age = c("40", "41", "30"),
    hoh_sex = c("male", "m", "male"),
    hoh_marital_status = c("married", "married", "single"),
    hoh_spouse_name = c("?????", "?????", ""),
    stringsAsFactors = FALSE
  )
}

build_master <- function() {
  data.frame(
    organization = c("Org A", "Org C"),
    governorate = c("Sanaa", "Aden"),
    district = c("D1", "D2"),
    sub_district = c("SD1", "SD2"),
    village = c("V1", "V2"),
    hoh_ID_number = c("12345", "22222"),
    primary_phone_number = c("777111222", "701234567"),
    secondary_phone_number = c("", ""),
    hoh_arabic_name = c("???? ???", "???? ???????"),
    hoh_age = c("40", "35"),
    hoh_sex = c("male", "male"),
    hoh_marital_status = c("married", "married"),
    hoh_spouse_name = c("?????", "?????"),
    stringsAsFactors = FALSE
  )
}

test_that("exact matches are detected for same-list and list-vs-master", {
  res <- run_dedup(
    upload_df = build_upload(),
    master_df = build_master(),
    fuzzy_high_threshold = 90,
    fuzzy_medium_threshold = 75
  )

  expect_true(is.data.frame(res$same_list_exact))
  expect_true(nrow(res$same_list_exact) >= 1)
  expect_true(is.data.frame(res$list_vs_master_exact))
  expect_true(nrow(res$list_vs_master_exact) >= 1)
})

test_that("fuzzy scores and confidence tiers follow thresholds", {
  u <- build_upload()
  m <- build_master()
  u$hoh_ID_number[1] <- ""
  u$hoh_ID_number[2] <- ""

  res <- run_dedup(
    upload_df = u,
    master_df = m,
    fuzzy_high_threshold = 88,
    fuzzy_medium_threshold = 70
  )

  expect_true(is.data.frame(res$list_vs_master_fuzzy))
  if (nrow(res$list_vs_master_fuzzy) > 0) {
    expect_true(all(res$list_vs_master_fuzzy$match_score >= 70))
    expect_true(all(res$list_vs_master_fuzzy$confidence %in% c("high", "medium")))
  }
})

test_that("threshold boundaries separate high and medium", {
  expect_identical(confidence_from_score(90, high_threshold = 90, medium_threshold = 75), "high")
  expect_identical(confidence_from_score(75, high_threshold = 90, medium_threshold = 75), "medium")
  expect_identical(confidence_from_score(74.9, high_threshold = 90, medium_threshold = 75), "low")
})

test_that("normalization handles malformed values", {
  malformed <- data.frame(
    hoh_arabic_name = c(NA, "  ?????  "),
    hoh_spouse_name = c("", NA),
    hoh_ID_number = c("???", "abc"),
    primary_phone_number = c("+967 777-000-111", NA),
    secondary_phone_number = c(NA, ""),
    governorate = c("  Sanaa ", NA),
    district = c(NA, "D"),
    sub_district = c(NA, "S"),
    village = c(NA, "V"),
    hoh_age = c("35", "not-a-number"),
    hoh_sex = c("male", NA),
    organization = c("Org", "Org"),
    hoh_marital_status = c("married", "single"),
    stringsAsFactors = FALSE
  )

  out <- prepare_frame(malformed)
  expect_true("hoh_arabic_name_n" %in% names(out))
  expect_true("phone_number_n" %in% names(out))
  expect_true(nrow(out) == 2)
})

test_that("config and default max_candidates are set to 500 and capped at 2000", {
  expect_equal(config$max_candidates, 500)
  expect_true(config$max_candidates <= 2000)
})
