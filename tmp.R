library(fgeo.habitat)

quiet_krig <- function(krg) {
  function(...) {
    output <- capture.output({
      result <- krg(...)
    })
    output <- strsplit(paste0(output, collapse = "\n"), "\n\\$df\n")[[1]][1]
    message(output)
    result
  }
}





# Develop generic as_df to format fgeo output as data frame
# rename tt_df by as_df
# rename mat_enframe_ls as list_columns() or gather(<matrix>)
# raname mat_enframe() of class "tt" as as_df(<tt_df>)
# krig_df() pull only the df component and rearrange arguments to 
# krig_df(var, ...) for convenient lapply().

# Make krig safer and more consistent with other functios. 
# 1. Remove defaults of gridsize and plotdim. And check with Graham if
# krig_breaks are special for plotdim c(1000, 500) or in general.
# 2. transform plotdim_x and _y in plotdim.
# Develop a function to extract gridsize and plotdim from soil data, similar to 
# fgeo.tool::guess_plotdim().

plotdim_x <- plotdim[1]
plotdim_y <- plotdim[2]

# Consider:
# extract_gridsize_hab
# extract_gridsize_cns
# extract_gridsize_soil

# release()

time_it <- function(f) {
  force(f)
  function(...) {
    system.time(f(...))
  }
}


# List of functions to compare
library(tidyverse)
library(fgeo.habitat)
compare <- list(
  GetKrigedSoil = function(var) suppressMessages(GetKrigedSoil(soil_fake, var)),
  krig = function(var)          suppressMessages(krig(soil_fake, var))
)

call_fun <- function(f, ...) f(...)
lapply(compare, time_it(call_fun), var = "c")





