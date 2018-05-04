#' Krige soil data following the methodology of the John et al. (2007).
#'
#' Krige soil data following the methodology of the John et al. (2007):
#' * `krig()` has an interface consistent with other functions of ForestGEO. It
#' outputs a list of subclass "krig", which has a `summary()` method (see
#' examples). `krig()` also tries to guess `plotdim` and informs both the
#' guessed `plotdim` and the `gridsize` provided. `krig()` allows you to
#' suppress all messages with `suppressMessages()`. You should carefully read
#' the messages before you suppress them.
#' * `GetKrigedSoil()` remains to preserve the original interface but it is
#' softly deprecated. Its result is the same as `krig()` but its interface is
#' different and lacks many of `krig()`'s features.
#' 
#' @param soil,df.soil The data frame with the points, coords specified in the
#'   columns `gx`, `gy`.
#' @param var A text string giving the variable/column in `df.soil` to krige.
#' @param gridsize,gridSize Points are kriged to the center points of a grid of
#'   this size.
#' @param params,krigeParams If you want to pass specified kriging parameters;
#'   see [krig_auto_params()] for each parameter.
#' @param plotdim,xSize,ySize Numeric vectors giving x and y dimensions of the
#'   plot: 
#'   * `plotdim`: Must be of lenght 2 with the format `c(x, y)`.
#'   * `xSize`, `ySize`: Each must be of lenght 1.
#' @param breaks Breaks/intervals used to calculate the semivariogram, which
#'   only happens if `krigeParams = NULL` (default).
#' @param use_ksline,useKsLine Use the [geoR::ksline()] function? Use `TRUE` to
#'   calculate a "best" semivariogram based on default parameters via
#'   `geoR::variogram()`]. Use `FALSE` to base calculation on parameters passed
#'   to `params`.
#' 
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
#' @author Graham Zemunik (grah.zem@@gmail.com).
#'
#' @export
#' 
#' @examples
#' \dontrun{
#' # Example data
#' soil <- soil_random
#' 
#' # Krige with automated parameters
#' auto <- krig(soil, var = "m3al")
#' # For a nice view in RStudio use: View(auto)
#' summary(auto)
#' 
#' # Custom params (arbitrary but based on automated kriging params)
#' params <- list(
#'   model = "circular", range = 100, nugget = 1000, sill = 46000, kappa = 0.5
#' )
#' custom <- krig(soil, var = "m3al", params = params)
#' # For a nice view in RStudio use: View(custom)
#' summary(custom)
#' 
#' # Compare
#' if (!requireNamespace("ggplot2", quietly = TRUE)) {
#'   stop("Install ggplot2 for this section to work.", call. = FALSE)
#' }
#' library(ggplot2)
#' 
#' title_auto <- "Automated parameters"
#' ggplot(auto$df, aes(x = x, y = y, fill = z)) +
#'   geom_tile() + 
#'   coord_equal() +
#'   labs(title = title_auto)
#' title_custom <- "Custom parameters"
#' 
#' ggplot(custom$df, aes(x = x, y = y, fill = z)) +
#'   geom_tile() + 
#'   coord_equal() +
#'   labs(title = title_custom)
#' }
krig <- function(soil,
                 var,
                 params = NULL,
                 gridsize = 20,
                 plotdim = guess_plotdim(soil),
                 breaks = krig_breaks(2, 320, 30),
                 use_ksline = TRUE) {
  message("Using: gridsize = ", gridsize)
  
  # Prints as message to enable muting via suppressMessages()
  krig_with_message <- krig_quietly(GetKrigedSoil)
  
  krig <- krig_with_message(
    df.soil = soil,
    var = var,
    gridSize = gridsize,
    krigeParams = params,
    xSize = plotdim[[1]],
    ySize = plotdim[[2]],
    breaks = breaks,
    useKsLine = use_ksline
  )
  
  message(krig$output)
  # Enable summary.krig()
  structure(krig$result, class = c("krig", class(krig$result)))
}

#' @rdname krig
#' @export
GetKrigedSoil <- function(df.soil,
                          var,
                          gridSize = 20,
                          krigeParams = NULL,
                          xSize = 1000,
                          ySize = 500,
                          breaks = krig_breaks(2, 320, 30),
                          useKsLine = TRUE) {
  check_krig(
    df.soil = df.soil, var = var, gridSize = gridSize, xSize = xSize,
    ySize = ySize, breaks = breaks, krigeParams = krigeParams,
    useKsLine = useKsLine
  )

  df <- df.soil[, c("gx", "gy", var)]
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
    df,
    gridSize = gridSize, xSize = xSize, ySize = ySize
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

  geod <- as.geodata(df.krig)
  if (is.null(krigeParams)) {
    params <- krig_auto_params(geod, breaks = breaks)
  } else {
    params <- krigeParams
    if (!("kappa" %in% names(params))) {
      params$kappa <- 0
    }
  }

  # Do the kriging
  if (useKsLine) {
    krig <- krig_ksline(
      geod,
      locations = polyfit$df.interpolated[, c("gx", "gy")],
      cov.pars = c(params$sill, params$range),
      cov.model = params$model,
      nugget = params$nugget,
      kappa = params$kappa,
      lambda = 1
    )
  } else {
    krig <- krige.conv(
      geod,
      locations = polyfit$df.interpolated[, c("gx", "gy")],
      krige = krige.control(
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

#' Find "best" variogram parameters. 
#' 
#' Find the "best" variogram parameters for a given geodata object.
#' 
#' The default breaks argument is set to have more points where the curve rises
#' the most and exponentially and fewer at large distances. This means that the
#' curve fitting is not overly biased by points beyond the effective maximum
#' range. 
#' 
#' Several different models are tested; the one with the lowest least
#' squares error is chosen.
#'
#' @inheritParams geoR::variog
#' 
#' @seealso [geoR::variog].
#' 
#' @return A list of the best fitted variogram parameters. The following
#'   description is adapted from [geoR::variog()] -- which you should see
#'   for more details:
#'   * nugget: value of the nugget parameter. An estimated value if 
#'   `fix.nugget = FALSE` or a fixed value if `fix.nugget = TRUE`.
#'   * sill and range: First and second elements of cov.pars` -- a two elements
#'   vector with estimated values of the covariance parameters sigma^2 and phi,
#'   respectively.
#'   * kappa: Fixed value of the smoothness parameter.
#'   * model: Name of the correlation function (see` cov.model`).
#'   * minVM: The minimum fit error.
#'   * vg: The variogram.
#'
#' @author Graham Zemunik (grah.zem@@gmail.com).
#' 
#' @export
krig_auto_params <- function(geodata,
                             trend = "cte",
                             breaks = krig_breaks(2, 320, 30)) {
  vg <- variog(geodata, breaks = breaks, pairs.min = 5, trend = trend)
  varModels <- c("exponential", "circular", "cauchy", "gaussian")
  minValue <- NULL
  minVM <- NULL
  startRange <- max(breaks) / 2
  initialVal <- max(vg$v) / 2
  for (i in 1:length(varModels)) {
    vm <- variofit(
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
#' @author Graham Zemunik (grah.zem@@gmail.com).
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

#' Return a polynomial 2nd order surface (x,y) defined by the parameters a to f.
#'
#' @author Graham Zemunik (grah.zem@@gmail.com).
#'
#' @keywords internal
#' @noRd
PolynomialSurfaceOrder2 <- function(x, y, a, b, c, d, e, f) {
  a + b * x + c * y + d * x * y + e * x^2 + f * y^2
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
#' @author Graham Zemunik (grah.zem@@gmail.com).
#'
#' @keywords internal
#' @noRd
BoxCoxTransformSoil <- function(df) {
  lambda <- 1
  delta <- 0

  if (ncol(df) >= 3) {
    # Sanity checking and enforcement of the structure
    if (!identical(names(df)[1:3], c("gx", "gy", "z"))) {
      names(df)[1:3] <- c("gx", "gy", "z")
    }

    if (min(df$z) >= 0) {
      # boxcox will complain about -ve values
      if (min(df$z) == 0) {
        # add a small amount to allow transforms to work
        delta <- 0.00001
        df$z <- df$z + delta
      }
      bc <- boxcox(
        z ~ gx + gy,
        data = df,
        lambda = c(0, 0.5, 1),
        plotit = F
      )
      lambda <- bc$x[which(bc$y == max(bc$y))]
      lambda <- round(lambda / 0.5) * 0.5 # Get lambda in multiples of 0.5
      if (lambda == 0) {
        df$z <- log(df$z)
      } else {
        if (lambda == 0.5) {
          df$z <- sqrt(df$z)
        }
      }
    }
  }
  # Return the data and the parameters
  list(df = df, lambda = lambda, delta = delta)
}

#' Perform the inverse of the Box-Cox transform.
#'
#' Performed the inverse of the Box-Cox transform given the data, df,
#' the lambda value and and delta added to the data
#'
#' @return The df with the transformed data, from the z column.
#'
#' @author Graham Zemunik (grah.zem@@gmail.com).
#'
#' @keywords internal
#' @noRd
InvBoxCoxTransformSoil <- function(df, lambda, delta) {
  if (lambda == 0) {
    df$z <- exp(df$z)
  } else {
    if (lambda == 0.5) {
      df$z <- df$z^2
    }
  }
  # Take away the delta offset
  df$z <- df$z - delta

  df
}

krig_quietly <- function(krg) {
  function(...) {
    output <- utils::capture.output({
      result <- krg(...)
    })
    output <- strsplit(paste0(output, collapse = "\n"), "\n\\$df\n")[[1]][1]
    list(
      output = output, 
      result = result
    )
  }
}

check_krig <- function(df.soil,
                       var,
                       gridSize,
                       krigeParams,
                       xSize,
                       ySize,
                       breaks,
                       useKsLine) {
  stopifnot(is.data.frame(df.soil))
  stopifnot(!is.null(df.soil))
  if (!dim(df.soil)[[1]] > 0) {
    stop(
      "df.soil has cero rows\n",
      "  * Ensure `df.soil` has one or more rows",
      call. = FALSE
    )
  }
  check_crucial_names(df.soil, c("gx", "gy"))
  stopifnot(is.character(var))
  stopifnot(length(var) != 0)
  stopifnot(!missing(var))

  if (!var %in% names(df.soil)) {
    stop(
      "The variable-name passed to `var` isn't in your data\n",
      "  * Check the possible options with `names(your_data)`",
      call. = FALSE
    )
  }
  stopifnot(is.numeric(gridSize))
  stopifnot(is.numeric(xSize))
  stopifnot(is.numeric(ySize))
  stopifnot(is.numeric(breaks))
  if (!is.null(krigeParams)) {
    stopifnot(is.list(krigeParams))
  }
  stopifnot(is.logical(useKsLine))
}

check_crucial_names <- function(x, nms) {
  are_names_expected <- all(nms %in% names(x))
  if (are_names_expected) {
    return(invisible())
  } else {
    stop(
      "Ensure your data set has these variables:\n",
      paste0(nms, collapse = ", "),
      call. = FALSE
    )
  }
}
