context("extract_from_habitat")

test_that("with habitats = luquillo_habitat gridsize value is 20", {
  expect_equal(extract_gridsize(luquillo_habitat), 20)
  expect_equal(extract_gridsize(luquillo_habitat), 20)
})
test_that("with habitats = luquillo_habitat plotdim value is c(320,  500)", {
  expect_equal(extract_plotdim(luquillo_habitat)[[1]], 320)
  expect_equal(extract_plotdim(luquillo_habitat)[[2]], 500)
})
test_that("value are of correct type", {
  expect_type(extract_gridsize(luquillo_habitat), "integer")
  expect_type(extract_plotdim(luquillo_habitat), "integer")
})
test_that("Output of extract_plotdim is unnamed", {
  expect_null(names(extract_plotdim(luquillo_habitat)))
})
test_that("output is of correct lengh", {
  expect_length(extract_gridsize(luquillo_habitat), 1)
  expect_length(extract_plotdim(luquillo_habitat), 2)
})

test_that("fails with wrong names", {
  expect_error(
    extract_gridsize(data.frame(wrong_name = 1)),
    "Ensure your data set has these variables"
  )
  expect_error(
    extract_plotdim(data.frame(wrong_name = 1)),
    "Ensure your data set has these variables"
  )
})

test_that("warns if habitat data contains NA", {
  dfm <- dplyr::tibble(
    x = c(0, 999, NA),
    y = c(0, 499, NA),
    habitats = c(1, 2, 3)
  )
  expect_silent(extract_plotdim(dfm[1:2, ]))
  expect_warning(extract_plotdim(dfm), "Detected missing values in: x, y")
  expect_silent(extract_gridsize(dfm[1:2, ]))
  expect_warning(extract_gridsize(dfm), "Detected missing values in: x, y")
})
