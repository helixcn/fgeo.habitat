context("dist_in_torus")

x <- data.frame(
  a = runif(10, min = 3, max = 5), 
  b = runif(10, min = 13, max = 15)
)



test_that("works as in example", {
  d1 <- dist_in_torus(x)
  expect_type(d1, "double")
  expect_true("matrix" %in%  class(d1))
  
  d2 <-  dist_in_torus(x, lower = c(3, 13), upper = c(5, 15))
  expect_type(d2, "double")
  expect_true("matrix" %in%  class(d2))
})


test_that("returns equal with dataframe and matrix", {
  expect_equal(dist_in_torus(x), dist_in_torus(as.matrix(x)))
})

