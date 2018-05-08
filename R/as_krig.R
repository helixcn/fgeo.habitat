#' Coerce to class krig and krig_lst
#' 
#' Usually you would use this function to coerce the output of `GetKrigedSoil()`
#' and because you want to use the methods available for objects of class krig
#' or krig_lst.
#' 
#' @param .x Output formatted as that of `krig()`.
#' @param ... Other arguments passed to methods.
#' 
#' @seealso [summary.krig()], [as_df.krig_lst()].
#'
#' @return An object of class krig.
#' @export
#'
#' @examples
#' out <- GetKrigedSoil(soil_fake, "c")
#' summary(out)
#' summary(as_krig(out))
#' 
#' out_lst <- lapply(c("c", "p"), function(var) GetKrigedSoil(soil_fake, var))
#' head(as_df(as_krig_lst(out_lst)))
as_krig <- function(.x, ...) {
  UseMethod("as_krig")
}

#' @export
as_krig.krig <- function(.x, ...) .x

#' @export
as_krig.list <- function(.x, ...) {
  validate_krig(new_krig(.x))
}

#' @export
as_krig.default <- function(.x, ...) {
  stop(
    "Don't know how to coerce object of class ", 
    paste(class(.x), collapse = "/"), " into krig", 
    call. = FALSE
  )
}

new_krig <- function(.x) {
  stopifnot(is.list(.x))
  structure(.x, class = c("krig", class(.x)))
}

validate_krig <- function(.x) {
  if (!length(.x) == 5) {
    rlang::abort(paste("The length of `.x` must be 5 but is", length(.x)))
  }
  
  expect <- c("df", "df.poly", "lambda", "vg", "vm")
  if (!all(names(.x) %in% expect)) {
    rlang::abort(paste(
      "`.x` has wrong names:\n",
      "* Expected: ", commas(expect), "\n",
      "* Actual: ", commas(names(.x))
    ))
  }
  .x
}



#' @export
#' @rdname as_krig
as_krig_lst <- function(.x, ...) {
  UseMethod("as_krig_lst")
}

#' @export
#' @rdname as_krig
as_krig_lst.krig_lst <- function(.x, ...) .x

#' @export
#' @rdname as_krig
as_krig_lst.list <- function(.x, ...) {
  validate_krig_lst(new_krig_lst(.x))
}

#' @export
#' @rdname as_krig
as_krig_lst.default <- function(.x, ...) {
  stop(
    "Don't know how to coerce object of class ", 
    paste(class(.x), collapse = "/"), " into krig_lst", 
    call. = FALSE
  )
}

new_krig_lst <- function(.x) {
  stopifnot(is.list(.x))
  structure(.x, class = c("krig_lst", class(.x)))
}

validate_krig_lst <- function(.x) {
  validate_krig(.x[[1]])
  .x
}
