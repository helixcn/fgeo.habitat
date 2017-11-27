context("test-getkrigedsoil.R")

df <- soilkrig::soil_random[1:10, ]
result <- suppressWarnings(GetKrigedSoil(df, var="M3Al"))


test_that("GetKrigedSoil() passes regression test", {
  expect_equal_to_reference(result, "ref-GetKrigedSoil.rds")
})
# MORE REGRESSION TESTS
# The following two tests are regression tests. They are to confirm that
# the old and new versions of the functions output the same. These test are
# dissabled by default because, before they can run, you must place in R/ the
# file: "C:\Users\LeporeM\Dropbox\git_repos\krig\data-raw\from_Graham_Zemunik/
#   Kriging functions".
#
# test_that("GetKrigedSoil() outputs the same as the original kriging funs", {
#   original <- suppressWarnings(GetKrigedSoil_2(df, var="M3Al"))
#   expect_equal(result, original)
# })
# test_that("GetKrigedSoil() outputs equal with geoR::ksline and krig_ksline", {
#   edited <- GetKrigedSoil(df, var = "M3Al")
#   original <- suppressWarnings(GetKrigedSoil_2(df, var = "M3Al"))
#   expect_equal(edited, original)
# })

test_that("GetKrigedSoil() returns the expected value.", {
  expect_type(result, "list")
  nms <- c("df", "df.poly", "lambda", "vg", "vm")
  expect_named(result, nms)
  expect_is(result$df, "data.frame")
  expect_is(result$df.poly, "data.frame")
  expect_is(result$lambda, "numeric")
  expect_type(result$vg, "list")
  expect_named(result$vg)
  expect_is(result$vm, "variomodel")
  expect_is(result$vm, "variofit")
})
