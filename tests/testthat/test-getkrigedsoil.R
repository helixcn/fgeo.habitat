context("test-getkrigedsoil.R")

df <- krig::soil_random[1:10, ]
result <- suppressWarnings(GetKrigedSoil(df, var="M3Al"))

test_that("GetKrigedSoil() outputs no warning.", {
  expect_error(
    expect_warning(GetKrigedSoil(df, var="M3Al"))
  )
})

test_that("GetKrigedSoil() passes regression test", {
  expect_equal_to_reference(result, "ref-GetKrigedSoil.rds")
})

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

test_that("GetKrigedSoil() outputs equal with geoR::ksline and krig_ksline", {
  source("getkrigedsoil_original.R")
  edited <- GetKrigedSoil(df, var = "M3Al")
  original <- suppressWarnings(GetKrigedSoil_original(df, var = "M3Al"))
  expect_equal(edited, original)
})
