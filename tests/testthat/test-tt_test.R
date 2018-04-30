context("tt_test.R")

# Ensure consistent values accross runs
set.seed(123)
library(dplyr)
library(fgeo.habitat)

# Small dataset from Luquillo
cns_luq <- luquillo_top3_sp
sp_top3_luq <- unique(cns_luq$sp)
hab_luq <- luquillo_habitat
sp_top1_luq <- first(sp_top3_luq)

test_that("outputs the expected tibble", {
  out <- expect_silent(tt_test(sp_top1_luq, cns_luq, hab_luq))
  expect_true(any(grepl("data.frame", class(out))))
  expect_equal(dim(out), c(24, 3))
  expect_equal(names(out), c("metric", "sp", "value"))
  expect_equal(sp_top1_luq, unique(out$sp))
})

pdim_luq <- c(320, 500)
gsize_luq <- 20

abnd <- abund_index(cns_luq, pdim_luq, gsize_luq)
out_tt_one <- tt_test_one(
  species = sp_top1_luq,
  hab.index20 = hab_luq,
  allabund20 = abnd,
  plotdim = pdim_luq,
  gridsize = gsize_luq
)

test_that("outputs expected values", {
  out <- tt_test(sp_top1_luq, cns_luq, hab_luq)$value
  expect_equal(out, as.vector(out_tt_one))
})

test_that("species may be factor or character", {
  expect_true(
    identical(
      tt_test(as.factor(sp_top1_luq), cns_luq, hab_luq), 
      tt_test(sp_top1_luq, cns_luq, hab_luq)
    )
  )
})

test_that("fails with informative message", {
  expect_error(
    tt_test(1, cns_luq, hab_luq),
    "`sp` must be of class character or factor"
  )
  expect_error(tt_test("a", cns_luq, hab_luq), "All `sp` must be present")
  expect_error(
    tt_test(c("SLOBER", "odd"), cns_luq, hab_luq), 
    "odd"
  )
  expect_error(
    tt_test(c("SLOBER", "PREMON"), census = 1, hab_luq), 
    "is not TRUE"
  )
  expect_error(tt_test(c("SLOBER", "PREMON"), cns_luq, 1), "is not TRUE")
  expect_error(tt_test(c("SLOBER"), cns_luq, hab_luq, 1), "is not TRUE")
  expect_error(
    tt_test(c("SLOBER"), cns_luq, hab_luq, pdim_luq, "a"), 
    "is not TRUE"
  )
  expect_warning(
    tt_test(c("SLOBER"), cns_luq, hab_luq, pdim_luq, 12), 
    "Uncommon `gridsize`"
  )
})



context("tt_df")

test_that("Fails with known error", {
  expect_error(tt_df(character()), "Can't deal with data")
})
