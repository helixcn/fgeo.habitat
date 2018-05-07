context("test-as_df.R")

# krig --------------------------------------------------------------------

vars <- c("c", "p")
out_lst <- krig_lst(soil_fake, vars, quiet = TRUE)
out_df <- as_df(out_lst)

test_that("passes silently with data of correct class", {
  expect_silent(head(out_df))
})

test_that("fails with unknown class", {
  expect_error(as_df(character(1)), "Can't deal with data of class")
  expect_error(as_df(data.frame(1)), "Can't deal with data of class")
  expect_error(as_df(list(1)), "Can't deal with data of class")
  
  expect_error(as_df(out_lst, name = 1))
  expect_error(as_df(out_lst, item = 1))
  expect_error(as_df(out_lst, item = "bad_item"))
  expect_error(as_df(out_lst, item = c("df", "df.poly")))
})

test_that("outputs object of no-longer class krig_lst", {
  expect_false(any("krig_lst" %in% class(as_df(out_lst))))
})



# tt ----------------------------------------------------------------------

context("as_df.tt_lst")

cns <- luquillo_top3_sp
spp1 <- unique(cns$sp)[[1]]
hab_luq <- luquillo_habitat
tt_lst <- tt_test_lst(cns, spp1, hab_luq)

test_that("outputs the expected dataframe", {
  expect_equal(class(tt_lst), c("tt_lst", "list"))
  out <- expect_silent(as_df(tt_lst))
  expect_equal(class(out), "data.frame")
})



context("as_df.tt")

pdim <- c(320, 500)
gsz <- 20

abnd <- abund_index(cns, pdim, gsz)
tt <- tt_test(
  sp = spp1,
  habitat = hab_luq,
  abundance = abnd,
  plotdim = pdim,
  gridsize = gsz
)

test_that("outputs the expected dataframe", {
  expect_equal(class(tt), c("tt", "matrix"))
  tt_df <- expect_silent(as_df(tt))
  expect_equal(class(tt_df), "data.frame")
})

