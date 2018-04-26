collapse <- function(...) {
  paste0(..., collapse = ", ")
}

check_crucial_names <- function (x, nms) {
  stopifnot(rlang::is_named(x))
  stopifnot(is.character(nms))
  are_names_expected <- all(nms %in% names(x))
  if (are_names_expected) {
    return(invisible(x))
  }
  else {
    stop("Ensure your data set has these variables:\n", paste0(nms, 
      collapse = ", "), call. = FALSE)
  }
}

get_datasets <- function (package) {
  dinfo <- utils::data(package = package)
  dinfo[["results"]][, "Item"]
}

#' data.frame version of tibble::enframe 
enframe <- function (x, name = "name", value = "value") {
  if (is.null(x)) 
    x <- logical()
  if (is_null(names(x))) {
    df <- list(seq_along(x), x)
  }
  else {
    df <- list(names(x), unname(x))
  }
  names(df) <- c(name, value)
  as.data.frame(df)
}
