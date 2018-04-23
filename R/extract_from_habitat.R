#' Extract plot dimensions from habitat data.
#'
#' @param habitats Data frame giving the habitat designation for each 20x20
#'   quadrat.
#' @name extract_from_habitat
#'
#' @return
#' * [extract_plotdim()]: `plotdim` (vector of length 2);
#' * [extract_gridsize()]: `gridsize` (scalar).
#'
#' @examples
#' extract_plotdim(bci_habitat)
#' extract_gridsize(bci_habitat)
NULL

#' @rdname extract_from_habitat
#' @export
extract_gridsize <- function(habitats) {
  # assert_are_names_matching(habitats, c("x", "y"))
  
  grid_x <- difference_among_grid_steps(habitats$x)
  grid_y <- difference_among_grid_steps(habitats$y)
  gridsize <- unique(grid_x, grid_y)
  gridsize
}

#' @rdname extract_from_habitat
#' @export
extract_plotdim <- function(habitats) {
  # assert_are_names_matching(habitats, c("x", "y"))
  
  gridsize <- extract_gridsize(habitats)
  plotdim <- unlist(
    lapply(habitats[c("x", "y")], function(.x){max(.x) + gridsize})
  )
  unname(plotdim)
}

#' From x and y columns of habitat data, get difference between grid steps.
#'
#' @param habitat_x_or_y Column x or y of habitat data, e.g. bci_habitat$x.
#'
#' @return A non negative scalar
#' @noRd
difference_among_grid_steps <- function(habitat_x_or_y) {
  # assertive::assert_is_non_empty(habitat_x_or_y)
  # assertive::assert_is_vector(habitat_x_or_y)
  # assertive::assert_all_are_non_negative(habitat_x_or_y)
  
  grid_steps <- unique(habitat_x_or_y)
  difference_among_grid_steps <- unique(diff(grid_steps))
  
  difference_among_grid_steps
}
