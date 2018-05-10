#' Summary of `krig_lst()` results.
#' 
#' A cleaner version of `str()` for the result of `krig_lst()`.
#' 
#' @param object The result of `krig_lst()`.
#' @inheritDotParams base::summary
#'
#' @return Prints a cleaner version of `str()` and returns its input invisibly.
#' @export
#'
#' @examples
#' multiple_results <- krig_lst(soil_fake, c("c", "p"), quiet = TRUE)
#' summary(multiple_results)
#' 
#' single_result <- multiple_results[[1]]
#' summary(single_result)
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

#' @export
summary.krig_lst <- function(object, ...) {
  nms <- names(object)
  for (i in nms) {
    summary.krig(object[[i]])
  }
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
