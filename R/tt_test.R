#' Torus Translation Test to determine habitat associations of tree species.
#'
#' Torus Translation Test (TT test) to determine habitat associations of tree
#' species.
#'
#' You should only try to determine the habitat association for sufficiently
#' abundant species - in a 50-ha plot, a minimum abundance of 50 trees/species
#' has been used.
#'
#' `tt_test_lst()` uses `abundanceperquad()` -- via `abund_index()` -- which is
#' slow. You may calculate abundance per quadrat independently, feed it to the
#' argument `allabund20` of `tt_test()`, and reformat the output with
#' `tt_df()`. See Examples to iterate over multiple species.
#'
#' @param sp,species Character sting giving species names. `tt_test()` can
#'   take only one species; `tt_test_lst()` can take any number of species.
#' @param census A dataframe; a ForestGEO census.
#' @param habitat,hab.index20 Object giving the habitat designation for each
#'   plot partition defined by `gridsize`.
#' @param plotdim Plot dimensions.
#' @param gridsize Grid size. If using `tt_test()`, ensure it matches the
#'   gridsize on which the habitats are defined and the abundances were
#'   calculated.
#' @param allabund20 The output of `abund_index()`.
#'
#' @author Sabrina Russo, Daniel Zuleta, Matteo Detto, and Kyle Harms.
#'
#' @seealso [tt_df()].
#'
#' @return 
#' * `tt_test_lst()`: A dataframe.
#' * `tt_test()`: A numeric matrix.
#'
#' @export
#' @examples
#' # Setup
#'
#' # For easier data wranging
#' library(dplyr)
#'
#' # Example data
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
#'
#'
#'
#' # Test with the wrapper tt_test_lst()
#'
#' out <- tt_test_lst(species, census, habitat)
#' # Try also: View(out)
#' head(out)
#' tail(out)
#'
#'
#'
#' # Test without the wrapper, i.e. with tt_test()
#'
#' # Setup
#' plotdim <- c(320, 500)
#' gridsize <- 20
#' abundance <- abund_index(pick, plotdim, gridsize)
#'
#' # One species
#' out2 <- tt_test(unique(pick$sp)[[2]],
#'   hab.index20 = habitat,
#'   allabund20 = abundance,
#'   plotdim = plotdim,
#'   gridsize = gridsize
#' )
#' out2
#' # Alternative data structure; try also View(tt_df(out2))
#' head(tt_df(out2))
#'
#' # Multiple species
#' out3 <- lapply(
#'   species, tt_test,
#'   hab.index20 = habitat,
#'   allabund20 = abundance,
#'   plotdim = plotdim,
#'   gridsize = gridsize
#' )
#' out3
#' # Alternative data structure; try also View(tt_df(out3))
#' head(tt_df(out3))
tt_test_lst <- function(sp,
                    census,
                    habitat,
                    plotdim = extract_plotdim(habitat),
                    gridsize = extract_gridsize(habitat)) {
  check_tt(sp = sp, census = census, habitat = habitat, plotdim = plotdim, 
    gridsize = gridsize
  )
  abundance <- abund_index(census, plotdim, gridsize)
  tt_mat <- lapply(
    X = sp,
    FUN = tt_test,
    allabund20 = abundance,
    hab.index20 = habitat,
    plotdim = plotdim,
    gridsize = gridsize
  )
  # TODO: Output not a df but a list of matrices
  # tt_lst <- tt_df(tt_mat)
  tt_lst <- tt_mat
  structure(tt_lst, class = c("tt_lst", class(tt_lst)))
}


check_tt <- function(sp, census, habitat, plotdim, gridsize) {
  stopifnot(
    is.data.frame(census), 
    is.data.frame(habitat),
    is.numeric(plotdim), 
    length(plotdim) == 2,
    is.numeric(gridsize), 
    length(gridsize) == 1
  )
  
  common_gridsize <- gridsize  %in% c(5, 10, 20)
  if (!common_gridsize) {
    rlang::warn(paste("Uncommon `gridsize`:", gridsize, "\nIs this expected?"))
  }
  
  if (!any(is.character(sp) || is.factor(sp))) {
    msg <- paste0(
      "`sp` must be of class character or factor but is of class ",
      class(sp), "."
    )
    rlang::abort(msg)
  }
  
  valid_sp <- sp %in% unique(census$sp)
  if (!all(valid_sp)) {
    msg <- paste0(
      "All `sp` must be present in `census`.\n",
      "Odd: ", commas(sp[!valid_sp])
    )
    abort(msg)
  }
}

#' @rdname tt_test_lst
#' @export
tt_test <- function(species, hab.index20, allabund20, plotdim, gridsize) {
  plotdimqx <- plotdim[1] / gridsize # Calculates no. of x-axis quadrats of plot. (x is the long axis of plot in the case of Pasoh)
  plotdimqy <- plotdim[2] / gridsize # Calculates no. of y-axis quadrats of plot.
  num.habs <- length(unique(hab.index20$habitats)) # Determines tot. no. of habitat types.

  GrLsEq <- matrix(0, 1, num.habs * 6) # Creates empty matrix for output.
  rownames(GrLsEq) <- species # Names single row of output matrix.


  for (i in 1:num.habs) # Creates names for columns of output matrix.
  {
    if (i == 1) {
      cols <- c(paste("N.Hab.", i, sep = ""), paste("Gr.Hab.", i, sep = ""), paste("Ls.Hab.", i, sep = ""), paste("Eq.Hab.", i, sep = ""), paste("Rep.Agg.Neut.", i, sep = ""), paste("Obs.Quantile.", i, sep = ""))
    }
    if (i > 1) {
      cols <- c(cols, paste("N.Hab.", i, sep = ""), paste("Gr.Hab.", i, sep = ""), paste("Ls.Hab.", i, sep = ""), paste("Eq.Hab.", i, sep = ""), paste("Rep.Agg.Neut.", i, sep = ""), paste("Obs.Quantile.", i, sep = ""))
    }
  }
  colnames(GrLsEq) <- cols # Names columns of output matrix.


  # CALCULATIONS FOR OBSERVED RELATIVE DENSITIES ON THE TRUE HABITAT MAP

  allabund20.sp <- allabund20[which(rownames(allabund20) == species), ] # pulls out the abundance by quad data for the focal species
  spmat <- matrix(as.numeric(allabund20.sp), nrow = plotdimqy, plotdimqx, byrow = F) # Fills a matrix, with no. rows = plotdimqy (dim 2) and no. columns = plotdimqx (dim 1), with the indiv. counts per quadrat of one species.
  totmat <- matrix(apply(allabund20, MARGIN = 2, FUN = "sum"), plotdimqy, plotdimqx, byrow = F) # calculates total number of stems in each quad for all species and puts in matrix

  habmat <- matrix(hab.index20$habitats, nrow = plotdimqy, ncol = plotdimqx, byrow = F) # fills matrix with habitat types, oriented in the same way as the species and total matrices above

  spstcnthab <- numeric() # Creates empty vector for stem counts per sp. per habitat.
  totstcnthab <- numeric() # Creates empty vector for tot. stem counts per habitat.

  for (i in 1:num.habs)
  {
    totstcnthab[i] <- sum(totmat[habmat == i]) # Determines tot. no. stems per habitat of the true map.
    spstcnthab[i] <- sum(spmat[habmat == i]) # Determines tot. no. stems for focal sp. per habitat of the true map.
  }

  spprophab <- spstcnthab / totstcnthab # Calculates observed relative stem density of focal sp. per habitat of the true map.

  # CALCULATIONS FOR RELATIVE DENSITIES ON THE TORUS-BASED HABITAT MAPS
  habmat.template <- habmat

  for (j in 1:4) {
    # apply rotations and mirrors
    # if j==1 do nothing

    if (j == 2) habmat <- apply(habmat.template, 2, rev)
    if (j == 3) habmat <- t(apply(habmat.template, 1, rev))
    if (j == 4) habmat <- t(apply(apply(habmat.template, 2, rev), 1, rev))


    # CALCULATIONS FOR RELATIVE DENSITIES ON THE TORUS-BASED HABITAT MAPS

    for (x in 0:(plotdimqx - 1)) # Opens "for loop" through all 20-m translations along x-axis.
    {
      for (y in 0:(plotdimqy - 1)) # Opens "for loop" through all 20-m translations along y-axis.
      {
        newhab <- matrix(0, plotdimqy, plotdimqx) # Creates empty matrix of quadrats' habitat designations.


        # The following "if" statements create the x,y torus-translation of the habitat map.

        if (y == 0 & x == 0) {
          newhab <- habmat
        }

        if (y == 0 & x > 0) {
          newhab <- habmat[c(1:plotdimqy), c((plotdimqx - x + 1):plotdimqx, 1:(plotdimqx - x))]
        }

        if (x == 0 & y > 0) {
          newhab <- habmat[c((plotdimqy - y + 1):plotdimqy, 1:(plotdimqy - y)), c(1:plotdimqx)]
        }

        if (x > 0 & y > 0) {
          newhab <- habmat[c((plotdimqy - y + 1):plotdimqy, 1:(plotdimqy - y)), c((plotdimqx - x + 1):plotdimqx, 1:(plotdimqx - x))]
        }


        Torspstcnthab <- numeric() # Creates empty vector for stem counts per sp. per habitat in torus-based maps.
        Tortotstcnthab <- numeric() # Creates empty vector for tot. stem counts per habitat in torus-based maps.

        for (i in 1:num.habs)
        {
          Tortotstcnthab[i] <- sum(totmat[newhab == i]) # Determines tot. no. stems per habitat of the focal torus-based map.
          Torspstcnthab[i] <- sum(spmat[newhab == i]) # Determines tot. no. stems for focal sp. per habitat of the focal torus-based map.
        }

        Torspprophab <- Torspstcnthab / Tortotstcnthab # Calculates relative stem density of focal sp. per habitat of the focal torus-based map.

        for (i in 1:num.habs)
        {
          if (is.na(spprophab[i] > Torspprophab[i])) {
            warn_invalid_comparison(spprophab[i], Torspprophab[i])
          }
          if (spprophab[i] > Torspprophab[i]) { # If rel. dens. of focal sp. in focal habitat of true map is greater than rel. dens. of focal sp. in focal habitat of torus-based map, then add one to "greater than" count.
            GrLsEq[1, (6 * i) - 4] <- GrLsEq[1, (6 * i) - 4] + 1
          }

          if (spprophab[i] < Torspprophab[i]) { # If rel. dens. of focal sp. in focal habitat of true map is less than rel. dens. of focal sp. in focal habitat of torus-based map, then add one to "less than" count.
            GrLsEq[1, (6 * i) - 3] <- GrLsEq[1, (6 * i) - 3] + 1
          }

          if (spprophab[i] == Torspprophab[i]) { # If rel. dens. of focal sp. in focal habitat of true map is equal to rel. dens. of focal sp. in focal habitat of torus-based map, then add one to "equal to" count.
            GrLsEq[1, (6 * i) - 2] <- GrLsEq[1, (6 * i) - 2] + 1
          }
        }
      } # Closes "for loop" through all 20-m translations along x-axis.
    } # Closes "for loop" through all 20-m translations along y-axis.
  } # Closes for loop through mirrors and rotations (j)


  for (i in 1:num.habs)
  {
    GrLsEq[1, (6 * i) - 5] <- spstcnthab[i] # add counts of No. stems in each habitat

    if (GrLsEq[1, (6 * i) - 4] / (4 * (plotdimqx * plotdimqy)) <= 0.025) GrLsEq[1, (6 * i) - 1] <- -1 # if rel.dens. of sp in true map is greater than rel. dens. in torus map less than 2.5% of the time, then repelled
    if (GrLsEq[1, (6 * i) - 4] / (4 * (plotdimqx * plotdimqy)) >= 0.975) GrLsEq[1, (6 * i) - 1] <- 1 # if rel.dens. of sp in true map is greater than rel. dens. in torus map more than 97.5% of the time, then aggregated
    if (GrLsEq[1, (6 * i) - 4] / (4 * (plotdimqx * plotdimqy)) < 0.975 & GrLsEq[1, (6 * i) - 4] / (plotdimqx * plotdimqy) > 0.025) GrLsEq[1, (6 * i) - 1] <- 0 # otherwise it's neutral (not different from random dist)

    GrLsEq[1, (6 * i)] <- GrLsEq[1, (6 * i) - 4] / (4 * (plotdimqx * plotdimqy)) # quantile in the TT distribtution of relative densities of the true relative density
  }

  # return(GrLsEq)
  structure(GrLsEq, class = c("tt", class(GrLsEq)))
}

#' Warns that a comparison is invalid. Results from a division `NaN = 0/0`
#' @noRd
warn_invalid_comparison <- function(spp, torus) {
  msg <- "Values can't be compared:\n"
  value <- paste0(
    "spprophab = ", spp, " vs. ",
    "Torspprophab = ", torus, "\n"
  )
  rlang::warn(paste0(msg, value))
}

#' Gather the output of `tt_test()`.
#'
#' Dataframe the output of `tt_test()`. For examples see [tt_test_lst()].
#'
#' @param ttt Output of `tt_test()`; Either a single matrix or a list of
#'   matrices.
#'
#' @return A dataframe.
#' @export
tt_df <- function(ttt) {
  UseMethod("tt_df")
}

#' @export
tt_df.list <- function(ttt) {
  flip <- t(Reduce(rbind, ttt))
  mat_enframe(flip, "metric", "sp", "value")
}
#' @export
tt_df.matrix <- function(ttt) {
  flip <- t(ttt)
  mat_enframe(flip, "metric", "sp", "value")
}
#' @export
tt_df.default <- function(ttt) {
  rlang::abort(paste0("Can't deal with data of class ", class(ttt), "."))
}
