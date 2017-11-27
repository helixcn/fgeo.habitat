check_GetKrigedSoil <- function(df.soil,
                                var,
                                gridSize,
                                krigeParams,
                                xSize,
                                ySize,
                                breaks,
                                useKsLine) {
  assertive::assert_is_data.frame(df.soil)
  assertive::assert_is_not_null(df.soil)
  if (!dim(df.soil)[[1]] > 0){
    stop(
      "df.soil has cero rows\n",
      "  * Ensure `df.soil` has one or more rows",
      call. = FALSE
    )
  }
  check_crucial_names(df.soil, c("gx", "gy"))
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
