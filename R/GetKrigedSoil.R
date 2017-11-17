# Kriging functionality for CTFS



#' Krige soil data following the methodology of the John et al. (2007).
#'
#' @description
#' If called without inputs, `GetKrigedSoil()` calculates a "best" semivariogram
#' to use and krige the soil data. By default the best kriging parameters (found
#' via variogram functions in the __geoR__ package) are used, but specified
#' parameters can be used via the `krigeParams` argument. __geoR__ has two main
#' kriging functions: [geoR::ksline()] and [geoR::krige.conv()]. The argument
#' `useKsLine` specifies whether to use the [geoR::ksline()] function or not.
#'
#' @param df.soil The data frame with the points, coords specified in the
#'   columns `gx`, `gy`.
#' @param var The variable/column in `df.soil` to krige.
#' @param gridSize Points are kriged to the centre points of a grid of this
#'   size.
#' @param krigeParams If you want to pass specified kriging parameters; see
#'   [GetAutomatedKrigeParams()] for each parameter.
#' @param xSize,ySize X and Y size/length of the plot.
#' @param breaks Breaks/intervals used to calculate the semivariogram, which
#'   only happens if `krigeParams = NULL` (default).
#' @param useKsLine See above.
#' @return A list with the following items:
#'   * `df`: Data frame of kriged values (column z) at each grid point (x, y).
#'   * `df.poly`: Data frame of the polynomial surface fitted to the raw data.
#'   * `lambda`: The "lambda" value used in the Box-Cox transform of the raw
#'     data.
#'   * `vg`: A list giving the variogram parameters used for the kriging.
#'   * `vm`: Minimum loss value returned from [geoR::variofit()].
#' @seealso [geoR::variofit()], [geoR::variog()], [geoR::as.geodata()],
#'   [geoR::ksline()], [geoR::krige.conv()], [geoR::krige.control()].
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # Use randomized data set for examples (invalid for reaserch).
#' df <- krig::soil_random
#' str(df)
#'
#' # Krige with automated parameters
#' kriged <- GetKrigedSoil( df, var="M3Al" )
#' str(kriged)
#'
#' # Krige with specified parameters - these example params are rather arbitrary
#' # but are loosely based on what was chosen from the automated kriging
#' params <- list( model="circular", range=100, nugget=1000, sill=46000, kappa=0.5 )
#' kriged.2 <- GetKrigedSoil( df, var="M3Al", krigeParams=params )
#'
#' # Have a look at the differences
#' g <- ggplot( kriged$df, aes( x=x, y=y, fill=z ) )
#' g <- g + geom_tile() + coord_equal()
#' g2 <- ggplot( kriged.2$df, aes( x=x, y=y, fill=z ) )
#' g2 <- g2 + geom_tile() + coord_equal()
#' g
#' g2
#' }
#' @export
GetKrigedSoil <- function(df.soil,
                          var = "P",
                          gridSize = 20,
                          krigeParams = NULL,
                          xSize = 1000,
                          ySize = 500,
                          breaks = ExpList(2, 320, 30),
                          useKsLine = TRUE) {
  df <- df.soil[ , c("gx", "gy", var)]
  names(df)[3] <- "z"

  # This follows a similar methodology to the John 2007 paper:
  # 1. the data is transformed based on the optimal boxcox transform
  # 2. a polynomial regression curve is fitted - a second order
  #    polynomial is used
  # 3. the residuals from the polynomial fit are kriged: two steps, a)
  #    get a variogram, b) do the kriging
  # 4. the kriged values are added to the polynomial fit and they
  #    are backtranformed as required

  # The lambda for the box-cox transform is restricted to 0, 0.5 and 1.
  # Data with 0's in there are handled by the addition of a small constant
  # in the regression
  bc <- BoxCoxTransformSoil(df)
  df <- bc$df

  polyfit <- GetPolynomialFit(
    df, gridSize = gridSize, xSize = xSize, ySize = ySize
  )

  # Get the variogram parameters
  # If a polynomial fit was possible then the parameters come from
  # the residuals of the polynomial fit
  # Otherwise, the raw data is used (a failure to fit the polynomial
  # most likely indicates that no trend exists, so trend removal isn't
  # necessary)
  if (is.null(polyfit$mod)) {
    df.krig <- df
  } else {
    df.krig <- cbind(polyfit$df.orig[, c("gx", "gy")], z = resid(polyfit$mod))
  }

  geod <- geoR::as.geodata(df.krig)
  if (is.null(krigeParams)) {
    params <- GetAutomatedKrigeParams(geod, breaks = breaks)
  } else {
    params <- krigeParams
    if (!("kappa" %in% names(params)))
      params$kappa <- 0
  }

  # Do the kriging
  if (useKsLine) {
    krig <- geoR::ksline(
      geod,
      locations = polyfit$df.interpolated[, c("gx", "gy")],
      cov.pars = c(params$sill, params$range),
      cov.model = params$model,
      nugget = params$nugget,
      kappa = params$kappa,
      lambda = 1
    )
  } else {
    krig <- geoR::krige.conv(
      geod,
      locations = polyfit$df.interpolated[, c("gx", "gy")],
      krige = geoR::krige.control(
        cov.pars = c(params$sill, params$range),
        cov.model = params$model,
        nugget = params$nugget,
        kappa = params$kappa,
        lambda = 1,
        aniso.pars = NULL
      )
    )
  }
  # Add the kriged results to the trend if necessary
  df.pred <- polyfit$df.interpolated
  if (is.null(polyfit$mod)) {
    df.pred$z <- krig$predict
  } else {
    df.pred$z <- df.pred$z + krig$predict
  }

  # Back transform (if required)
  df.pred <- InvBoxCoxTransformSoil(df.pred, bc$lambda, bc$delta)

  names(df.pred) <- c("x", "y", "z")

  # Return all useful data
  list(
    df = df.pred,
    df.poly = polyfit$df.interpolated,
    lambda = bc$lambda,
    vg = params$vg,
    vm = params$minVM
  )
}

#' List of n values which exponentially increases from first to last.
#'
#' @param first Xxx.
#' @param last Xxx.
#' @param n Xxx.
#'
#' @noRd
#' @keywords internal
ExpList <- function(first, last, n) {
  v <- vector()
  m <- 1 / (n - 1)
  quotient <- (last / first) ^ m

  v[1] <- first
  for (i in 2:n)
    v[i] <- v[i - 1] * quotient

  v
}

#' Find the "best" variogram parameters for a given geodata object.
#'
#' Uses the [geoR::variofit()] with a range of variogram models.
#'
#' @return A list of the best fitted variogram parameters:
#'   * nugget: Xxxx.
#'   * sill: Xxxx.
#'   * range: Xxxx.
#'   * kappa: Xxxx.
#'   * model: Xxxx.
#'   * minVM: The minimum fit error.
#'   * vg: The variogram.
#' @noRd
#' @keywords internal
GetAutomatedKrigeParams <- function(geod,
                                    trend = "cte",
                                    breaks = ExpList(2, 320, 30)) {
  # The default breaks argument is set to have more points where the curve
  # rises the most and exponentially fewer at large distances
  # This means that the curve fitting is not overly biased by points
  # beyond the effective maximum range

  # Several different models are tested; the one with the lowest least
  # squares error is chosen
  vg <- geoR::variog(geod, breaks = breaks, pairs.min = 5, trend = trend)
  varModels <- c("exponential", "circular", "cauchy", "gaussian") #, "wave" )
  minValue <- NULL
  minVM <- NULL
  startRange <- max(breaks) / 2
  initialVal <- max(vg$v) / 2
  for (i in 1:length(varModels)) {
    vm <- geoR::variofit(
      vg,
      ini.cov.pars = c(initialVal, startRange),
      nugget = initialVal,
      cov.model = varModels[i]
    )
    if (is.null(minValue) || vm$value < minValue) {
      minValue <- vm$value
      minVM <- vm
    }
  }
  list(
    nugget = minVM$nugget,
    sill = minVM$cov.pars[1],
    range = minVM$cov.pars[2],
    kappa = minVM$kappa,
    model = minVM$cov.model,
    minVM = minVM,
    vg = vg
  )
}

#' Fit a 2D polynomial surface.
#'
#' Fit a 2D polynomial surface from the following inputs:
#' * A dataframe with columns specifying the x, y coordinates and a quantity at
#'   each coord; and
#' * A grid of locations (specified by gridSize), which are interpolated using
#'   nls.
#'
#' @return A list of the original df, the interpolated values at each grid
#'   point, and the nls model, if the nls fit succeeded. No interpolated
#'   locations are returned if nls failed to model the surface, in which case
#'   the model attribute is set to `NULL.`
#'
#' @param df Xxxx.
#' @param gridSize Xxxx.
#' @param xSize Xxxx.
#' @param ySize Xxxx.
#'
#' @noRd
#' @keywords internal
GetPolynomialFit <- function(df, gridSize = 20, xSize = 1000, ySize = 500) {
  # The data frame is assumed to be x, y, z
  names(df) <- c("gx", "gy", "z")

  model <- NULL
  tryCatch(
    model <- nls(
      z ~ PolynomialSurfaceOrder2(gx, gy, a, b, c, d, e, f),
      data = df,
      start = list(
        a = 0.1,
        b = 0.1,
        c = 0.1,
        d = 0.1,
        e = 0.1,
        f = 0.1
      ),
      trace = FALSE,
      control = nls.control(maxiter = 200, minFactor = 1 / 4096)
    ),
    error = function(e) {
    }
  )

  halfGrid <- gridSize / 2
  df.locations <- expand.grid(
    gx = seq(halfGrid, xSize - halfGrid, by = gridSize),
    gy = seq(halfGrid, ySize - halfGrid, by = gridSize)
  )
  if (!is.null(model)) {
    df.locations$z <- predict(model, newdata = df.locations)
  }

  list(df.orig = df, df.interpolated = df.locations, mod = model)
}



#' Return a polynomial 2nd order surface (x,y) defined by the parameters a to f
#'
#' @param x Xxx.
#' @param y Xxx.
#' @param a Xxx.
#' @param b Xxx.
#' @param c Xxx.
#' @param d Xxx.
#' @param e Xxx.
#' @param f Xxx.
#'
#' @keywords internal
#' @noRd
PolynomialSurfaceOrder2 <- function( x, y, a, b, c, d, e, f )
{
  a + b*x + c*y + d*x*y + e*x^2 + f*y^2
}


#' Find the optimal Box-Cox transform parameters.
#'
#' Finds the optimal Box-Cox transform parameters for the data in data frame,
#' df, with columns specifying the x, y coordinates and a quantity at each
#' coord, whilst restricting the lambda value to 0, 0.5 and 1. Only data >=0 can
#' be transformed. Values = 0 are handled by adding a small value, delta, which
#' if used is returned as the delta argument.
#'
#' @return A list of the original df, the delta value and the the delta value.
#'
#' @param df Xxxx.
#'
#' @keywords internal
#' @noRd
BoxCoxTransformSoil <- function( df )
{
  lambda <- 1
  delta <- 0

  if ( ncol(df) >= 3 ) {
    # Sanity checking and enforcement of the structure
    if ( !identical( names(df)[1:3], c("gx", "gy", "z") ) ) {
      names(df)[1:3] <- c("gx", "gy", "z")
    }

    if ( min( df$z ) >= 0 ) { # boxcox will complain about -ve values
      if ( min( df$z ) == 0 ) {
        # add a small amount to allow transforms to work
        delta = 0.00001
        df$z <- df$z + delta
      }
      bc <- MASS::boxcox( z ~ gx + gy, data=df, lambda=c(0, 0.5, 1), plotit=F)
      lambda <- bc$x[ which( bc$y == max(bc$y) ) ]
      lambda <- round( lambda / 0.5 ) * 0.5  # Get lambda in multiples of 0.5
      if ( lambda == 0 ) {
        df$z <- log( df$z )
      } else if ( lambda == 0.5 ) df$z <- sqrt( df$z )
    }
  }
  # Return the data and the parameters
  list( df=df, lambda=lambda, delta=delta )
}

#' Perform the inverse of the Box-Cox transform.
#'
#' Performed the inverse of the Box-Cox transform given the data, df,
#' the lambda value and and delta added to the data
#'
#' @param lambda Xxx.
#' @param df Xxx.
#' @param delta Xxx.
#'
#' @return The df with the transformed data, from the z column.
#'
#' @keywords internal
#' @noRd
InvBoxCoxTransformSoil <- function( df, lambda, delta )
{
  if ( lambda == 0 ) {
    df$z <- exp( df$z )
  } else if ( lambda == 0.5 ) df$z <- df$z ^ 2
  # Take away the delta offset
  df$z <- df$z - delta

  df
}
