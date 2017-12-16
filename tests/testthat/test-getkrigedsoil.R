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

# Checks ------------------------------------------------------------------

test_that("check_GetKrigSoil() fails with wrong input", {
  numeric_input <- as.matrix(df)
  expect_error(GetKrigedSoil(numeric_input, var="M3Al"))

  rnm <- stats::setNames(df, c("wrong_x", "wrong_gy", "M3Al"))
  expect_error(
    GetKrigedSoil(rnm, var = "M3Al"),
    "Ensure your data set has these variables:"
  )
  cero_row <- data.frame(gx = numeric(0), gy = numeric(0))
  expect_error(
    GetKrigedSoil(cero_row, var ="M3Al"),
    "Ensure `df.soil` has one or more rows"
  )
  expect_error(
    GetKrigedSoil(df, var = "non-existent-var"),
    "The variable-name passed to `var` isn't in your data"
  )
  expect_error(GetKrigedSoil(df, var = 888))
  expect_error(
    GetKrigedSoil(df),
    "argument \"var\" is missing"
  )
  expect_error(GetKrigedSoil(df, var = "M3Al", gridSize = "3"))
  expect_error(GetKrigedSoil(df, var = "M3Al", xSize = "3"))
  expect_error(GetKrigedSoil(df, var = "M3Al", ySize = "3"))
  wrong_type <- 1
  expect_error(GetKrigedSoil(df, var = "M3Al", krigeParams = wrong_type))
  bad_not_a_number <- "a"
  expect_error(GetKrigedSoil(df, var = "M3Al", breaks = bad_not_a_number))
  bad_not_logical <- "a"
  expect_error(GetKrigedSoil(df, var = "M3Al", useKsLine = bad_not_logical))
})
