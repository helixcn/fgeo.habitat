context("test-abund_index.R")

library(tidyverse)

cns <- luquillo_tree6_random
pdm <- c(1000, 500)
gsz <- 20

test_that("outputs equal to abundanceperquad()", {

  old <- fgeo.habitat:::abundanceperquad2(
    censdata = cns, mindbh = 0, plotdim = pdm, gridsize = gsz
  )$abund
  new <- abund_index(cns, pdm, gsz)

  expect_equal(old, new)
})

test_that("is faster than abundanceperquad()", {
  skip_if_not_installed("microbenchmark")
  
  # source("tests/testthat/ref-abundanceperquad.R")
  source("ref-abundanceperquad.R")
  new <- microbenchmark::microbenchmark(abund_index(cns, pdm, gsz), times = 3)
  old <- microbenchmark::microbenchmark(
    abundanceperquad(
      censdata = cns, mindbh = 0, plotdim = pdm, gridsize = gsz
    )$abund,
    times = 10
  )
  expect_true(mean(new$time) < mean(old$time))
})



context("abundanceperquadrat2")

test_that("outputs equal to abundanceperquad()", {
  
  out1 <- fgeo.habitat:::abundanceperquad2(
    censdata = cns, mindbh = 0, plotdim = pdm, gridsize = gsz
  )$abund
  out2 <- abundanceperquad(
    censdata = cns, mindbh = 0, plotdim = pdm, gridsize = gsz
  )$abund
  expect_equal(out1, out2)
})
