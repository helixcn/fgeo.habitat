context("data")

library(fgeo.habitat)
library(dplyr)

test_that("expected datasets", {
  expect <- sort(
    c(
      "luquillo_elevation",
      "luquillo_habitat",
      "luquillo_stem6_random",
      "luquillo_top3_sp",
      "luquillo_tree6_random",
      "soil_random",
      "soil_fake"
    )
  )
  actual <- sort(fgeo.base::find_datasets("fgeo.habitat"))
  expect_equal(expect, actual)
})
