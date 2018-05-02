#' Summary of `krig()` results.
#' 
#' A cleaner version of `str()` for the result of `krig()`.
#' 
#' @param object The result of `krig()`.
#' @inheritDotParams base::summary
#'
#' @return Prints a cleaner version of `str()` and returns its input invisibly.
#' @export
#'
#' @examples
#' # Example dataset
#' soil <- soil_fake
#' 
#' res1 <- GetKrigedSoil(soil, var = "mg")
#' 
#' summary(res1)
#' 
#' res2 <- suppressMessages(krig(soil, var = "mg"))
#' summary(res2)
#' 
#' identical(unclass(res1), unclass(res2))
summary.krig <- function(object, ...) {
  cat(
    hdr(object$df, "df"),
    cat("\n"),
    hdr(object$df.poly, "df.poly"),
    cat("\n"),
    hdr2(object$lambda, "lambda"),
    cat("\n"),
    hdr2(object$vg, "vg"),
    cat("\n"),
    hdr2(object$vm, "vm")
  )
  invisible(object)
}

hdr <- function(.data, ...) {
  cat(
    cat(... , "\n", sep = ""),
    cat(utils::str(.data, give.attr = FALSE))
  )
}

hdr2 <- function(x, nm) {
  hdr(x, nm, "\n'", commas(class(x), "'"))
}
