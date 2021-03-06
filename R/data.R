#' Datasets from __fgeo.data__.
#'
#' @seealso `fgeo.data::luquillo_habitat`, `fgeo.data::luquillo_elevation`,
#'   `fgeo.data::luquillo_tree6_random`, `fgeo.data::luquillo_stem6_random`.
#'
#' @examples
#' str(luquillo_habitat, give.attr = FALSE)
#' str(luquillo_elevation, give.attr = FALSE)
#' str(luquillo_tree6_random, give.attr = FALSE)
#' str(luquillo_stem6_random, give.attr = FALSE)
#' @name luquillo
NULL

#' @rdname luquillo
"luquillo_habitat"
#' @rdname luquillo
"luquillo_elevation"
#' @rdname luquillo
"luquillo_tree6_random"
#' @rdname luquillo
"luquillo_stem6_random"



#' A tiny dataset from Luquillo of the three most abundant species.
#'
#' @examples
#' str(luquillo_top3_sp)
"luquillo_top3_sp"



#' Soil data for examples.
#'
#' This data set is based on data from Barro Colorado Island, but the variable
#' `m3al` has been randomized. It is useful for examples but not for research.
#'
#' @source Graham Zemunik (via https://goo.gl/2EwLQJ).
#'
#' @examples
#' str(soil_random, give.attr = FALSE)
"soil_random"



#' Fake soil data with multiple soil variables.
#'
#' @examples
#' str(soil_fake)
"soil_fake"
