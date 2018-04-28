#' Convert a matrices to data frames.
#'
#' These functions convert a matrix to three-column data frames, where columns
<<<<<<< HEAD
#' store row names, column names, and values of the original matrix.
=======
#' sotre row names, column names, and values of the original matrix.
>>>>>>> b830130d4e7d59c66ab77b8d5b9bb356575e1003
#' [mat_enframe()] outputs a long-format dataframe and [mat_enframe_ls()]
#' returns a list of dataframes where each element of the list maps to a column
#' of the original matrix.
#'
#' These functions are a matrix implementation of the `enframe()` function of
#' the __tibble__ package.
#'
#' @param mat A matrix.
#' @param rownm,colnm,value Names of the columns that store the row names, the
#'   column names and the values.
#' @param ... Arguments passed to mat_enframe_ls.
#'
#' @seealso `enframe()` (__tibble__ package).
#'
#' @return
#' * [mat_enframe()]: A dataframe.
#' * [mat_enframe_ls()]: A list of dataframes.
#'
#' @examples
#' mat <- matrix(1:6, 2, dimnames = list(LETTERS[1:2], letters[1:3]))
#' mat_enframe_ls(mat)
#' mat_enframe_ls(mat, "metric", "sp")
#'
#' mat <- matrix(1:6, 2)
#' mat_enframe_ls(mat)
mat_enframe <- function(...) {
  Reduce(rbind, mat_enframe_ls(...))
}

#' @rdname mat_enframe
#' @export
mat_enframe_ls <- function(mat,
                           rownm = "rownames",
                           colnm = "colnames",
                           value = "value") {
  stopifnot(!is.null(mat))

  if (rlang::is_null(rownames(mat))) {
    rownames(mat) <- 1:nrow(mat)
  }
  if (rlang::is_null(colnames(mat))) {
    colnames(mat) <- 1:ncol(mat)
  }
  rvar <- rownames(mat)
  cvar <- colnames(mat)
  cols <- lapply(data.frame(mat), function(x) x)
  for (i in seq_along(cvar)) {
    df <- list(rvar, cvar[[i]], unname(mat[, cvar[[i]]]))
    names(df) <- c(rownm, colnm, value)
    cols[[i]] <- as.data.frame(df, stringsAsFactors = FALSE)
  }
  cols
}
