context("extract_plot_dimensions")

library(fgeo.data)

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
