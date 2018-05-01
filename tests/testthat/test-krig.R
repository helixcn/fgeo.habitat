context("test-krig.R")

df <- fgeo.habitat::soil_random[1:10, ]
result <- suppressWarnings(krig(df, var = "M3Al"))

test_that("krig() passes regression test", {
  expect_equal_to_reference(result, "ref-krig.rds")
  expect_known_output(result, "ref-krig", print = TRUE, update = TRUE)
})

test_that("krig() returns the expected value.", {
  expect_type(result, "list")
  nms <- c("df", "df.poly", "lambda", "vg", "vm")
  expect_named(result, nms)
  expect_is(result, "krig")
  expect_is(result, "list")
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
  expect_error(krig(numeric_input, var = "M3Al"))

  rnm <- stats::setNames(df, c("wrong_x", "wrong_gy", "M3Al"))
  expect_error(
    krig(rnm, var = "M3Al"),
    "Ensure your data set has these variables:"
  )
  cero_row <- data.frame(gx = numeric(0), gy = numeric(0))
  expect_error(
    krig(cero_row, var = "M3Al"),
    "Ensure `df.soil` has one or more rows"
  )
  expect_error(
    krig(df, var = "non-existent-var"),
    "The variable-name passed to `var` isn't in your data"
  )
  expect_error(krig(df, var = 888))
  expect_error(
    krig(df),
    "argument \"var\" is missing"
  )
  expect_error(krig(df, var = "M3Al", gridSize = "3"))
  expect_error(krig(df, var = "M3Al", xSize = "3"))
  expect_error(krig(df, var = "M3Al", ySize = "3"))
  wrong_type <- 1
  expect_error(krig(df, var = "M3Al", params = wrong_type))
  bad_not_a_number <- "a"
  expect_error(krig(df, var = "M3Al", breaks = bad_not_a_number))
  bad_not_logical <- "a"
  expect_error(krig(df, var = "M3Al", useKsLine = bad_not_logical))
})
