# Remove dependency on purrr::quietly, only function used.
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
