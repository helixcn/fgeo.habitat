#' Count rows by quadrat-index.
#'
#' Count rows by quadrat-index. This is a conservative wrapper around the
#' function `abundanceperquad()` of the CTFSRPackage. Its output is always
#' abundance (not basal area nor agb) and includes all available rows. If you
#' want to exclude trees of some particular dbh range you need to do it before
#' using this function.
#' 
#' This function is softly deprecated. Better alternavies to count rows by
#' groups are available in __dplyr__ and __janitor__(see `group_by()` and
#' `count()` in __dplyr__ and `tabyl()` in __janitor__). Those alternatives are
#' better tested and considerably faster.
#'
#' @inheritParams ctfs::abundanceperquad
#'
#' @return A dataframe where each quadrat-index is a column and each species
#' is a rowname.
#' @export
#'
#' @seealso `fgeo.tool::add_index()`.
#'
#' @examples
#' cns <- luquillo_tree6_random
#' pdm <- c(1000, 500)
#' gsz <- 20
#' abund_index(cns, pdm, gsz)
abund_index <- function(censdata, plotdim, gridsize) {
  stopifnot(!missing(censdata), !missing(plotdim), !missing(gridsize))
  abundanceperquad2(
    censdata = censdata, plotdim = plotdim, gridsize = gridsize, mindbh = 0
  )$abund
}



# From ctfs ---------------------------------------------------------------

#' A simpler, faster version of abundanceperquad(), for abundance exclusively.
#'
#' @inheritParams ctfs::abundanceperquad
#'
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
#' @noRd
abundanceperquad2 <- function(censdata,
  mindbh = 10,
  plotdim = c(1000, 500),
  gridsize = 100,
  type = "abund",
  dbhunit = "mm") {
  sp <- censdata$sp
  quadno <- gxgy.to.index(censdata$gx, censdata$gy,
    gridsize = gridsize,
    plotdim = plotdim
  )
  result <- abundance2(censdata,
    type = type, mindbh = mindbh,
    dbhunit = dbhunit, split1 = sp, split2 = quadno
  )
  allspp <- unique(censdata$sp)
  maxquad <- floor(plotdim[1] / gridsize) * floor(plotdim[2] / gridsize)
  allquad <- 1:maxquad
  if (dim(result[[type]])[1] < length(allspp) | dim(result[[type]])[2] <
      length(allquad)) {
    result[[type]] <- fill.dimension(result[[type]],
      class1 = allspp,
      class2 = allquad, fill = 0
    )
  }
  return(result)
}

#' A faster version of abundance() targeted to only counts (not ba or agb)
#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
#' @noRd
abundance2 <- function(censdata,
  type = "abund",
  alivecode = c("A"),
  mindbh = NULL,
  dbhunit = "mm",
  split1 = NULL,
  split2 = NULL) {
  if (!type == "abund") {
    stop(
      "`type` must be 'abund'; other types are deprecated.\n",
      "Maybe you want `abundance()` of the original CTFSRPackage?",
      call. = FALSE
    )
  }
  
  if (is.null(split1)) {
    split1 <- rep("all", dim(censdata)[1])
  }
  if (is.null(split2)) {
    split2 <- rep("all", dim(censdata)[1])
  }
  if (!is.null(mindbh)) {
    inc <- censdata$dbh >= mindbh
  } else {
    inc <- rep(TRUE, length(censdata$dbh))
  }
  alive <- rep(FALSE, length(censdata$dbh))
  for (i in 1:length(alivecode)) alive[censdata$status == alivecode[i]] <- TRUE
  class1 <- sort(unique(split1))
  class2 <- sort(unique(split2))
  groupvar <- list(split1[alive & inc], split2[alive & inc])
  abund <- tapply(censdata$dbh[alive & inc], groupvar, length)
  abund <- fill.dimension(abund, class1, class2)
  result <- list(abund = abund)
  return(result)
}

#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
#' @noRd
basum <- function(dbh, mindbh = 10, dbhunit = "mm") {
  if (!is.null(mindbh)) {
    dbh <- dbh[dbh >= mindbh]
  }
  if (length(dbh) == 0) {
    return(0)
  }
  return(sum(ba(dbh, dbhunit = dbhunit), na.rm = TRUE))
}

#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
#' @noRd
gxgy.to.index <- function(gx, gy, gridsize = 20, plotdim = c(1000, 500)) {
  badgxgy <- (gx < 0 | gy < 0 | gx >= plotdim[1] | gy >= plotdim[2] |
      is.na(gx) | is.na(gy))
  colno <- 1 + floor(gx / gridsize)
  rowno <- 1 + floor(gy / gridsize)
  if (length(badgxgy[badgxgy > 0])) {
    colno[badgxgy] <- rowno[badgxgy] <- NA
  }
  return(rowcol.to.index(rowno, colno, gridsize, plotdim))
}

#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
#' @noRd
fill.dimension <- function(dataarray, class1, class2, fill = 0) {
  result <- data.frame(matrix(fill, nrow = length(class1), ncol = length(class2)))
  rownames(result) <- class1
  colnames(result) <- class2
  result[rownames(dataarray), colnames(dataarray)] <- dataarray
  result[is.na(result)] <- fill
  return(result)
}

#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
#' @noRd
ba <- function(dbh, dbhunit = "mm") {
  if (dbhunit == "mm") {
    return(pi * (dbh / 2000)^2)
  }
  if (dbhunit == "cm") {
    return(pi * (dbh / 200)^2)
  }
}

#' @family functions from http://ctfs.si.edu/Public/CTFSRPackage/
#' @keywords internal
#' @noRd
rowcol.to.index <- function(rowno, colno, gridsize = 20, plotdim = c(1000, 500)) {
  badrc <- (rowno <= 0 | colno <= 0 | rowno > plotdim[2] / gridsize |
      colno > plotdim[1] / gridsize)
  rowno <- rowno - 1
  colno <- colno - 1
  maxrow <- floor(plotdim[2] / gridsize)
  index <- colno * maxrow + rowno + 1
  if (length(badrc[badrc > 0])) {
    index[badrc] <- NA
  }
  return(index)
}
