#' Restructure data as a dataframe.
#' 
#' The goal of this (generic) function (with methods for multiple fgeo classes)
#' is to produce a dataframe that helps you to work fluently with other general 
#' purpose tools such as __dplyr__ and __ggplot2__.
#' 
#' @param .x An fgeo object of supported class.
#' @param ... Other arguments passed to methods.
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' # Method for class "krig_lst"
#' vars <- c("c", "p")
#' krig_lst <- krig_lst(soil_fake, vars, quiet = TRUE)
#' as_df(krig_lst)
as_df <- function(.x, ...) {
  UseMethod("as_df")
}



#' Dataframe the output of `krig_lst()`.
#'
#' @param .x The output of [krig_lst()].
#' @param name Name for the column to hold soil variable-names.
#' @param item Character string; either "df" or "df.poly".
#'
#' @return A dataframe.
#' @export
#'
#' @examples
#' vars <- c("c", "p")
#' krig_lst <- krig_lst(soil_fake, vars, quiet = TRUE)
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




#' Dataframe objects of class tt_*.
#'
#' @param .x An object of class tt_*.
#' @param ... Other arguments passed to [as_df()].
#'
#' @return A dataframe.
#'
#' @examples
#' # Class tt_lst
#' cns <- fgeo.habitat::luquillo_top3_sp
#' spp <- unique(cns$sp)
#' hab <- luquillo_habitat
#' 
#' tt_lst <- tt_test_lst(spp, cns, hab)
#' tt_df <- as_df(tt_lst)
#' head(tt_df)
#' 
#' tail(tt_df)
#' 
#' # Class tt
#' 
#' pdim <- c(320, 500)
#' gsz <- 20
#' abnd <- abund_index(cns, pdim, gsz)
#' spp1 <- spp[[1]]
#' 
#' tt <- tt_test(
#'   species = spp1,
#'   hab.index20 = hab,
#'   allabund20 = abnd,
#'   plotdim = pdim,
#'   gridsize = gsz
#' )
#' 
#' tt_df <- as_df(tt)
#' head(tt_df)
#' tail(tt_df)
#' @name as_df_ttl
NULL

#' @rdname  as_df_tt
#' @export
as_df.tt <- function(.x, ...) {
  mat_enframe(t(.x), "metric", "sp", "value")
}

#' @rdname  as_df_tt
#' @export
as_df.tt_lst <- function(.x, ...) {
  flip <- t(Reduce(rbind, .x))
  mat_enframe(flip, "metric", "sp", "value")
}
