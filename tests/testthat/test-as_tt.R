context("as_tt.R")

habitat <- luquillo_habitat
census <- luquillo_top3_sp
species <- unique(census$sp)
plotdim <- c(320, 500)
gridsize <- 20
abundance <- abund_index(census, plotdim, gridsize)

tt_mat <- torusonesp.all(species[[1]],
  hab.index20 = habitat,
  allabund20 = abundance,
  plotdim = plotdim,
  gridsize = gridsize
)

test_that("adds expected class and behaviour", {
  expect_is(tt_mat, "matrix")
  tt <- as_tt(tt_mat)
  expect_true(any(grepl("tt", class(tt))))
  expect_is(as_df(tt), "data.frame")
})

test_that("fails with informative message", {
  expect_error(as_tt(1), "Don't know how to coerce")
  
  wrong_dim <- matrix(1:4, 2)
  expect_error(as_tt(wrong_dim), "`.x` must have 24 columns")
})



context("as_tt_lst")

test_that("adds expected class and behaviour", {
  lst <- lapply(species, torusonesp.all, habitat, abundance, plotdim, gridsize)
  expect_is(lst, "list")
  tt_lst <- as_tt_lst(lst)
  expect_true(any(grepl("tt_lst", class(tt_lst))))
  expect_is(as_df(tt_lst), "data.frame")
})

test_that("fails with informative message", {
  expect_error(as_tt_lst(1), "Don't know how to coerce")
  
  wrong_dim <- matrix(1:4, 2)
  expect_error(as_tt_lst(list(wrong_dim)), "`.x` must have 24 columns")
})
