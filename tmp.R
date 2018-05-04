# Make krig safer and more consistent with other functions. 

# use guess_plotdim() from fgeo.base
# use gridsize = 20, warn.

# 1. Remove defaults of gridsize and plotdim. And check with Graham if
# krig_breaks are special for plotdim c(1000, 500) or in general.

# Develop generic as_df to format fgeo output as data frame
# rename tt_df by as_df
# krig_df() pull only the df component and rearrange arguments to 
# krig_df(var, ...) for convenient lapply().

# Consider:
# extract_gridsize_hab
# extract_gridsize_cns
# extract_gridsize_soil

# rename mat_enframe_ls as list_columns() or gather(<matrix>)
# raname mat_enframe() of class "tt" as as_df(<tt_df>)

# release()
