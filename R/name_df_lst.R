# Move to fgeo.base. Replaces fgeo.tool::ls_name_df().

# Takes list of dataframes and adds a column in each with its name
name_df_lst <- function(df_lst, name = "name") {
  stopifnot(is.list(df_lst), is.character(name))
  
  df_lst <- fill_missing_names(df_lst)
  lst_nms <- names(df_lst)
  for (i in seq_along(lst_nms)) {
    df_nms <- c(names(df_lst[[i]]), name)
    df_lst[[i]] <- stats::setNames(cbind(df_lst[[i]], lst_nms[[i]]), df_nms)
  }
  df_lst
}

# Fill names of an unnamed list
# fill_missing_names(c(named = 1), "vector")
# fill_missing_names(c(1), "vector")
fill_missing_names <- function(x, prefix = "df") {
  filler_names <- paste0(prefix, seq_along(x))
  if (is.null(names(x))) {
    names(x) <- filler_names
  }
  missing_names <- names(x) %in% ""
  if (any(missing_names)) {
    names(x)[missing_names] <- filler_names[missing_names]
  }
  x
}
