commas <- function(...) {
  paste0(..., collapse = ", ")
}

get_datasets <- function(package) {
  dinfo <- utils::data(package = package)
  dinfo[["results"]][, "Item"]
}
