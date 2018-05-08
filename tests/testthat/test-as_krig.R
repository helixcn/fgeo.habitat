context("as_krig")

out <- GetKrigedSoil(soil_fake, "c")

test_that("adds expected class and behaviour", {
  expect_is(out, "list")
  kg <- as_krig(out)
  expect_true(any(grepl("krig", class(kg))))
  expect_output(summary(kg), "'data.frame':	1250 obs.")
})

test_that("fails with informative message", {
  expect_error(as_krig(1), "Don't know how to coerce")
  
  expect_error(as_krig(out[1:2]), "The length of `.x` must be 5")
  names(out)[[1]] <- "bad_name"
  expect_error(as_krig(out), "has wrong names")
})



context("as_krig_lst")

out <- lapply(c("c", "p"), function(var) GetKrigedSoil(soil_fake, var))

test_that("adds expected class and behaviour", {
  expect_is(out, "list")
  kg <- as_krig_lst(out)
  expect_true(any(grepl("krig_lst", class(kg))))
  expect_is(to_df(kg), "data.frame")
})

test_that("fails with informative message", {
  expect_error(as_krig_lst(1), "Don't know how to coerce")
  
  expect_error(as_krig_lst(out[[1]][4:5]), "The length of `.x` must be 5")
  names(out[[1]])[[1]] <- "bad_name"
  expect_error(as_krig_lst(out), "has wrong names")
})
