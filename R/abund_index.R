<<<<<<< HEAD
#' Count rows by quadrat-index (conservative wrapper of `abundanceperquad()`).
=======
#' Count rows by quadrat-index (conservative warpper of `abundanceperquad()`).
>>>>>>> b830130d4e7d59c66ab77b8d5b9bb356575e1003
#'
#' Count rows by quadrat-index. This is a conservative wrapper around the
#' function `abundanceperquad()` of the CTFSRPackage. Its output is always
#' abundance (not basal area nor agb) and includes all available rows. If you
#' want to exclude trees of some particular dbh range you need to do it before
#' using this function.
#'
#' @inheritParams ctfs::abundanceperquad
#'
#' @return A dataframe where each quadrat-index is a column and each species
#' is a rowname.
#' @export
#'
#' @seealso `fgeo.tool::add_index()`.
#'
#' @examples
#' cns <- luquillo_tree6_random
#' pdm <- c(1000, 500)
#' gsz <- 20
#' abund_index(cns, pdm, gsz)
abund_index <- function(censdata, plotdim, gridsize) {
  stopifnot(!missing(censdata), !missing(plotdim), !missing(gridsize))
  abundanceperquad(
    censdata = censdata, plotdim = plotdim, gridsize = gridsize, mindbh = 0
  )$abund
}
