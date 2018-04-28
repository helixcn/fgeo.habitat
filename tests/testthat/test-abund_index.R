context("test-abund_index.R")

test_that("outputs equal to abundanceperquad()", {
  cns <- luquillo_tree6_random
  pdm <- c(1000, 500)
  gsz <- 20

  old <- fgeo.habitat:::abundanceperquad(
    censdata = cns, mindbh = 0, plotdim = pdm, gridsize = gsz
  )$abund
  new <- abund_index(cns, pdm, gsz)

  expect_equal(old, new)
})
