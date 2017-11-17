context("test-getkrigedsoil.R")

df <- krig::soil_random[1:10, ]

test_that("GetKrigedSoil() returns silently.", {
  expect_silent(GetKrigedSoil(df, var="M3Al"))
})

test_that("GetKrigedSoil() passes regression test", {
  res <- suppressWarnings(GetKrigedSoil(df, var="M3Al"))
  expect_known_output(res, "ref-GetKrigedSoil.rds")
})
