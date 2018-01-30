#' A general function to calculate distances in a n-dimensional toroid.
#' 
#' By default, this function returns the distance in the Euclidean space
#' by assuming borders infinitely apart (i.e. points in a small portion
#' of an infinitely large toroid). 
#' \[xxx What happens if you change the default?\]
#' 
#' The shortest distance in the toroid is the hypotenuse of the smallest
#' hyper-triangle. The 'internal' distance is the typical distance based on the
#' coordinates, as in the Euclidean space. The 'external' distance is crossing
#' borders, going around. There are only two ways of measuring distance along
#' each dimension.
#' 
#' @param x The matrix with the coordinates of the points. \[xxx The data
#'   sould contain exclusively the coordinates, right? If so it may be better
#'   to restrict the data to a matrix -- dataframes can mix vectors of different
#'   types which may fail.\]
#' @param lower,upper The minimum and maximum possible values of the coordinates
#'   along each dimension. \[xxx should these always be vectors of length 2? --
#'   as shown in the example?\]
#'   
#' @author Gabriel Arellano
#'
#' @return A numeric matrix.
#' @export
#'
#' @examples
#' # \[xxx can you add\] an example in more than 2 dimensions?\]
#' 
#' x <- data.frame(runif(10, min = 3, max = 5), runif(10, min = 13, max = 15))
#' 
#' # Euclidean distances
#' d0 <- dist(x) 
#' # default behaviour
#' d1 <- dist_in_torus(x) 
#' # distances in the toroid
#' d2 <- dist_in_torus(x, lower = c(3, 13), upper = c(5, 15))
#' 
#' par(mfrow = c(1, 3))
#' plot(x, xlim = c(3, 5), ylim = c(13, 15), xlab = "x", ylab = "y")
#' plot(c(d0), c(as.dist(d1)), main = "default = Euclidean = infinite toroid")
#' abline(0, 1)
#' plot(c(d0), c(as.dist(d2)), main = "finite toroid")
#' abline(0, 1)
#' 
#' 
#' # \[xxx Is this behaviour OK?\]
#' # # Passes
#' # dist_in_torus(matrix(c(NaN, NaN)))
#' # dist_in_torus(matrix(c(NA, NA)))
#' # dist_in_torus(matrix(c(Inf, Inf)))
#' # dist_in_torus(matrix(c(1, 1)))
#' # dist_in_torus(matrix(c(-1, -1)))
#' # dist_in_torus(matrix(c(1, 1)))
#' # dist_in_torus(data.frame(a = c(1, 1)))
#' # 
#' # # Fails
#' # dist_in_torus(matrix(a = c(1)))
#' # dist_in_torus(matrix(c(NULL, NULL)))
#' # 
#' # # \[xxx What known outputs would be useful for tests? For example:
#' # library(testthat)
#' # v1 <- c(0, 1, 1, 0)
#' # expect_equal(c(dist_in_torus(matrix(c(1, 2)))), v1)
#' # expect_equal(c(dist_in_torus(matrix(c(0, 1)))), v1)
#' # # ...
#' 
dist_in_torus <- function(x,
                          lower = rep(-Inf, ncol(x)),
                          upper = rep(Inf, ncol(x))) {
  # Number of dimensions
  n <- ncol(x)
  # Size of the n-dimensional space considered
  ranges <- upper - lower

  # Internal and external cathetuses along each dimension:
  internal_cats <- sapply(
    1:n, function(i) abs(outer(x[, i], x[, i], "-")), simplify = "array"
  )
  external_cats <- sapply(1:n, function(i) ranges[i] - internal_cats[, , i])

  # The shortest cathetuses along each dimension define the smallest
  # hyper-triangle:
  shortest_cats <- pmin(internal_cats, external_cats)

  # Application of the Pythagorean theorem across layers:
  hypo <- sqrt(rowSums(shortest_cats ^ 2, dims = 2))
  hypo
}
