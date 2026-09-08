testthat::context("arabic-compounds-and-tribal")

source(file.path("..", "..", "R", "config.R"))
source(file.path("..", "..", "R", "preprocess.R"))
source(file.path("..", "..", "R", "matching.R"))

test_that("bond_arabic_compounds bonds religious compound names correctly", {
  expect_equal(bond_arabic_compounds("عبد الله"), "عبدالله")
  expect_equal(bond_arabic_compounds("عبد الرحمن"), "عبدالرحمن")
  expect_equal(bond_arabic_compounds("عبد العزيز"), "عبدالعزيز")
  expect_equal(bond_arabic_compounds("سيف الدين"), "سيفالدين")
  expect_equal(bond_arabic_compounds("نور الدين"), "نورالدين")
  expect_equal(bond_arabic_compounds("صلاح الدين"), "صلاحالدين")
  expect_equal(bond_arabic_compounds("جار الله"), "جارالله")
  expect_equal(bond_arabic_compounds("فضل الله"), "فضلالله")
})

test_that("normalize_arabic produces identical tokens for separated and bonded compounds", {
  expect_equal(normalize_arabic("عبد الله محمد"), normalize_arabic("عبدالله محمد"))
  expect_equal(normalize_arabic("أمة الرحمن علي"), normalize_arabic("امةالرحمن علي"))
  expect_equal(normalize_arabic("سيف الدين قاسم"), normalize_arabic("سيفالدين قاسم"))
})

test_that("strip_tribal_prefixes strips Al- and Bin/Ibn prefixes from family names", {
  expect_equal(strip_tribal_prefixes("محمد علي الشرعبي"), "محمد علي شرعبي")
  expect_equal(strip_tribal_prefixes("احمد بن علي الحوثي"), "احمد علي حوثي")
  expect_equal(strip_tribal_prefixes("صالح آل جابر"), "صالح جابر")
  expect_equal(strip_tribal_prefixes("يحيى ابن قاسم الصنعاني"), "يحيى قاسم صنعاني")
})

test_that("v_name_similarity gives 100 score to compound variations and tribal variations", {
  sim_compound <- v_name_similarity("عبد الله محمد", "عبدالله محمد")
  expect_equal(sim_compound, 100)

  sim_tribal <- v_name_similarity("محمد علي الشرعبي", "محمد علي شرعبي")
  expect_equal(sim_tribal, 100)

  sim_bin <- v_name_similarity("احمد بن علي الحوثي", "احمد علي حوثي")
  expect_equal(sim_bin, 100)
})
