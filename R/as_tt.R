#' Coerce to class tt and tt_list.
#'
#' @param .x Output structured as that of [tt_test()] or [tt_test_lst()].
#' @param ... Other arguments passed to methods.
#'
#' @seealso [as_df()].
#'
#' @return An object of class tt or tt_lst.
#' @export
#'
#' @examples
#' # For easier data wranging
#' library(dplyr)
#' 
#' habitat <- luquillo_habitat
#' census <- luquillo_top3_sp
#' 
#' # Pick alive trees, of 10 mm or more
#' pick <- filter(census, status == "A", dbh >= 10)
#' # Pick sufficiently abundant trees
#' pick <- add_count(pick, sp)
#' pick <- filter(pick, n > 50)
#' 
#' species <- unique(pick$sp)
#' plotdim <- c(320, 500)
#' gridsize <- 20
#' abundance <- abund_index(pick, plotdim, gridsize)
#' 
#' tt_mat <- torusonesp.all(species[[1]],
#'   hab.index20 = habitat,
#'   allabund20 = abundance,
#'   plotdim = plotdim,
#'   gridsize = gridsize
#' )
#' 
#' head(as_df(as_tt(tt_mat)))
#' 
#' # Iterate over multiple species
#' tt_lst <- lapply(species, tt_test, habitat, abundance, plotdim, gridsize)
#' head(as_df(as_tt_lst(tt_lst)))
as_tt <- function(.x, ...) {
  UseMethod("as_tt")
}

#' @export
as_tt.matrix <- function(.x, ...) {
  validate_tt(new_tt(.x))
}

#' @export
as_tt.tt <- function(.x, ...) .x

#' @export
as_tt.default <- function(.x, ...) {
  stop(
    "Don't know how to coerce object of class ", 
    paste(class(.x), collapse = "/"), " into tt", 
    call. = FALSE
  )
}

new_tt <- function(.x) {
  stopifnot(is.matrix(.x))
  structure(.x, class = c("tt", class(.x)))
}

validate_tt <- function(.x) {
  if (!dim(.x)[[2]] == 24) {
    rlang::abort(paste("`.x` must have 24 columns but has", dim(.x)[[2]]))
  }
  .x
}

#' @rdname as_tt
#' @export
as_tt_lst <- function(.x, ...) {
  UseMethod("as_tt_lst")
}

#' @export
as_tt_lst.tt_lst <- function(.x, ...) .x

#' @export
as_tt_lst.list <- function(.x, ...) {
  validate_tt_lst(new_tt_lst(.x))
}

#' @export
as_tt_lst.default <- function(.x, ...) {
  stop(
    "Don't know how to coerce object of class ", 
    paste(class(.x), collapse = "/"), " into tt", 
    call. = FALSE
  )
}

new_tt_lst <- function(.x) {
  stopifnot(is.list(.x))
  structure(.x, class = c("tt_lst", class(.x)))
}

validate_tt_lst <- function(.x) {
  validate_tt(.x[[1]])
  .x
}
