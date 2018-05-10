#' Summary results of `krig()`.
#' 
#' A cleaner version of `str()` for the result of `krig().
#' 
#' @param object The result of `krig()`.
#' @inheritDotParams base::summary
#'
#' @return Prints a cleaner version of `str()` and returns its input invisibly.
#' @export
#'
#' @examples
#' result <- krig(soil_fake, c("c", "p"), quiet = TRUE)
#' summary(result)
#' str(result)
summary.krig_lst <- function(object, ...) {
  nms <- names(object)
  for (i in seq_along(nms)) {
    cat("var:", nms[[i]], "\n")
    summarize_krig(object[[i]])
  }
  invisible(object)
}

summarize_krig <- function(object, ...) {
  cat(
    hdr(object$df, "df"),
    cat("\n"),
    hdr(object$df.poly, "df.poly"),
    cat("\n"),
    hdr2(object$lambda, "lambda"),
    cat("\n"),
    hdr2(object$vg, "vg"),
    cat("\n"),
    hdr2(object$vm, "vm"),
    cat("\n")
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
