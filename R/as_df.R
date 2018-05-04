#' Restructure data as a dataframe.
#' 
#' The goal of this (generic) function (with methods for multiple fgeo classes)
#' is to produce a dataframe that helps you to work fluently with other general 
#' purpose tools such as __dplyr__ and __ggplot2__.
#'
#' @param x A data object of supported class.
#' @param ... Other arguments passed to methods.
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' # Method for class "krig_lst"
#' vars <- c("c", "p")
#' krig_lst <- krig_lst(vars, soil_fake, quiet = TRUE)
#' as_df(krig_lst)
as_df <- function(x, ...) {
  UseMethod("as_df")
}



#' Dataframe the output of `krig_lst()`.
#'
#' @param krig_lst The output of [krig_lst()].
#' @param name Name for the column to hold soil variable-names.
#' @param item Character string; either "df" or "df.poly".
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' vars <- c("c", "p")
#' krig_lst <- krig_lst(vars, soil_fake, quiet = TRUE)
#' as_df(krig_lst)
as_df.krig_lst <- function(.x, name = "var", item = "df") {
  stopifnot(is.character(name), is.character(item))
  stopifnot(length(item) == 1, item == "df" || item == "df.poly")
  
  dfs <- lapply(.x, "[[", item)
  out <- Reduce(rbind, name_df_lst(dfs, name = name))
  out[c(name, setdiff(names(out), name))]
}

#' @export
as_df.default <- function(.x, ...) {
  rlang::abort(paste0("Can't deal with data of class ", class(.x)))
}



# TODO add as_df.tt_one

#' @export
as_df.tt_one <- function(x) {
  mat_enframe(t(x), "metric", "sp", "value")
}

as_df.tt <- function(.x, ...) {
  flip <- t(Reduce(rbind, .x))
  mat_enframe(flip, "metric", "sp", "value")
}
