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

test_that("outputs the expected list", {
  out <- expect_silent(tt_test_lst(cns_luq, sp_top1_luq, hab_luq))
  expect_equal(class(out), c("tt_lst", "list"))
  expect_equal(dim(out[[1]]), c(1, 24))
  expect_equal(sp_top1_luq, rownames(out[[1]]))
})

pdim_luq <- c(320, 500)
gsize_luq <- 20

abnd <- abund_index(cns_luq, pdim_luq, gsize_luq)
out_tt <- tt_test(
  sp = sp_top1_luq,
  habitat = hab_luq,
  abundance = abnd,
  plotdim = pdim_luq,
  gridsize = gsize_luq
)

test_that("outputs expected values", {
  out_lst <- tt_test_lst(cns_luq, sp_top1_luq, hab_luq)
  expect_equal(out_lst[[1]], out_tt)
})





test_that("species may be factor or character", {
  expect_true(
    identical(
      tt_test_lst(cns_luq, as.factor(sp_top1_luq), hab_luq), 
      tt_test_lst(cns_luq, sp_top1_luq, hab_luq)
    )
  )
})

test_that("fails with informative message", {
  expect_error(
    tt_test_lst(cns_luq, 1, hab_luq),
    "`sp` must be of class character or factor"
  )
  expect_error(tt_test_lst(cns_luq, "a", hab_luq), "All `sp` must be present")
  expect_error(
    tt_test_lst(cns_luq, c("SLOBER", "odd"), hab_luq), 
    "odd"
  )
  expect_error(
    tt_test_lst(census = 1, c("SLOBER", "PREMON"), hab_luq), 
    "is not TRUE"
  )
  expect_error(tt_test_lst(cns_luq, c("SLOBER", "PREMON"), 1), "is not TRUE")
  expect_error(tt_test_lst(cns_luq, c("SLOBER"), hab_luq, 1), "is not TRUE")
  expect_error(
    tt_test_lst(cns_luq, c("SLOBER"), hab_luq, pdim_luq, "a"), 
    "is not TRUE"
  )
  expect_warning(
    tt_test_lst(cns_luq, c("SLOBER"), hab_luq, pdim_luq, 12), 
    "Uncommon `gridsize`"
  )
})
