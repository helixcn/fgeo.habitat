# Make krig safer and more consistent with other functions. 

# Develop generic as_df to format fgeo output as data frame
# rename tt_df by as_df
# krig_df() pull only the df component and rearrange arguments to 
# krig_df(var, ...) for convenient lapply().

# Consider moving these functions to fgeo.base, and doing these changes:
#   rename mat_enframe_ls as list_columns() or gather(<matrix>)
#   raname mat_enframe() of class "tt" as as_df(<tt_df>)

# release()

# Vectorize() over var?


x <- (data.frame(a = 1, b = 2))

data.frame(out = list(x))

dfm <- data.frame(name = "nm")
dfm$df <- list(x)

View(dfm)


list
