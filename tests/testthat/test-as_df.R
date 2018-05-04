context("test-as_df.R")

vars <- c("c", "p")
out_lst <- krig_lst(vars, soil_fake, quiet = TRUE)
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
