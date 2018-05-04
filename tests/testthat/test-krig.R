context("test-krig.R")

library(tidyverse)

# Small dataset that still preserves plot dimensions via guess_plotdim()
df0 <- fgeo.habitat::soil_random
df_gx <- tail(arrange(df0, gx))
df_gy <- tail(arrange(df0, gy))
df <- rbind(df_gx, df_gy)

test_that("plotdimensions are guessed correctly", {
  expect_equal(guess_plotdim(df), c(1000, 500))
})

result <- suppressMessages(krig(df, var = "m3al"))

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

test_that("print suppressable message", {
  expect_message(krig(df, var = "m3al"))
  expect_silent(suppressMessages(krig(df, var = "m3al")))
})

# Checks ------------------------------------------------------------------

test_that("outputs the same with plotdim given directly or via guess_plotdim", {
  expect_equal(
    suppressMessages(
      krig(soil_random, "m3al", plotdim = c(1000, 500))
    ),
    suppressMessages(
      krig(soil_random, "m3al", plotdim = guess_plotdim(soil_random))
    )
  )
})

test_that("check_GetKrigSoil() fails with wrong input", {
  numeric_input <- as.matrix(df)
  expect_error(krig(numeric_input, var = "m3al"))

  rnm <- stats::setNames(df, c("wrong_x", "wrong_gy", "m3al"))
  expect_error(
    krig(rnm, var = "m3al"),
    "Ensure your data set has these variables:"
  )
  cero_row <- data.frame(gx = numeric(0), gy = numeric(0))
  expect_error(
    krig(cero_row, var = "m3al"),
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
  expect_error(krig(df, var = "m3al", gridSize = "3"))
  expect_error(krig(df, var = "m3al", xSize = "3"))
  expect_error(krig(df, var = "m3al", ySize = "3"))
  wrong_type <- 1
  expect_error(krig(df, var = "m3al", params = wrong_type))
  bad_not_a_number <- "a"
  expect_error(krig(df, var = "m3al", breaks = bad_not_a_number))
  bad_not_logical <- "a"
  expect_error(krig(df, var = "m3al", useKsLine = bad_not_logical))
})
