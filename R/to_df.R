#' Restructure data as a dataframe.
#'
#' The goal of this (generic) function (with methods for multiple fgeo classes)
#' is to produce a dataframe that helps you to work fluently with other general
#' purpose tools such as __dplyr__ and __ggplot2__.
#'
#' @param .x An fgeo object of supported class.
#' @param ... Other arguments passed to methods.
#'
#' @seealso [to_df.krig_lst()], [to_df.tt_lst()].
#'
#' @return A dataframe.
#' @export
to_df <- function(.x, ...) {
  UseMethod("to_df")
}

#' @export
to_df.default <- function(.x, ...) {
  rlang::abort(paste0("Can't deal with data of class ", class(.x)))
}

#' Dataframe the output of `krig()`.
#'
#' @param .x The output of [krig()].
#' @param name Name for the column to hold soil variable-names.
#' @param item Character string; either "df" or "df.poly".
#' @inheritDotParams to_df
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' vars <- c("c", "p")
#' krig <- krig(soil_fake, vars, quiet = TRUE)
#' head(to_df(krig))
to_df.krig_lst <- function(.x, name = "var", item = "df", ...) {
  stopifnot(is.character(name), is.character(item))
  stopifnot(length(item) == 1, item == "df" || item == "df.poly")

  dfs <- lapply(.x, "[[", item)
  out <- Reduce(rbind, fgeo.base::name_df_lst(dfs, name = name))
  out[c(name, setdiff(names(out), name))]
}

#' Dataframe the output of tt_test().
#'
#' @param .x An object of class tt_lst.
#' @param ... Other arguments passed to [to_df()].
#'
#' @return A dataframe.
#'
#' @export
#' @examples
#' cns <- fgeo.habitat::luquillo_top3_sp
#' spp <- unique(cns$sp)
#' hab <- luquillo_habitat
#'
#' tt_lst <- tt_test(cns, spp, hab)
#' tt_df <- to_df(tt_lst)
#' head(tt_df)
#'
#' tail(tt_df)
to_df.tt_lst <- function(.x, ...) {
  flip <- t(Reduce(rbind, .x))
  fgeo.base::gather_matrix(flip, "metric", "sp", "value")
}
