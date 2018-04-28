context("mat_enframe.R")

test_that("outputs expected values", {
  mat <- matrix(1:6, 2, dimnames = list(LETTERS[1:2], letters[1:3]))
  out <- mat_enframe(mat)
  expect_equal(out$value, 1:6)
  expect_equal(out$rownames, rep(c("A", "B"), 3))
  expect_equal(out$colnames, rep(c("a", "b", "c"), each = 2))
})

test_that("outputs expected format", {
  mat <- matrix(1:6, 2, dimnames = list(LETTERS[1:2], letters[1:3]))
  out <- mat_enframe(mat, "a", "b", "c")
  expect_equal(names(out), c("a", "b", "c"))
  expect_true(any(grepl("data.frame", class(out))))
})



context("mat_enframe_ls.R")

test_that("output is of expected format", {
  mat <- matrix(1:6, 2, dimnames = list(LETTERS[1:2], letters[1:3]))
  out <- mat_enframe_ls(mat)
  expect_type(out, "list")
  nms <- unique(Reduce(c, lapply(out, names)))
  expect_equal(nms, c("rownames", "colnames", "value"))

  out2 <- mat_enframe_ls(mat, "metric", "sp")
  nms2 <- unique(Reduce(c, lapply(out2, names)))
  expect_equal(nms2, c("metric", "sp", "value"))

  mat <- matrix(1:6, 2)
  out3 <- mat_enframe_ls(mat)
  expect_equal(names(out3), paste0("X", 1:3))
})
