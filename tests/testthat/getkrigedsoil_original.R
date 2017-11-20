GetKrigedSoil_original <- function(df.soil,
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
