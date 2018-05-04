cns <- luquillo_top3_sp
spp <- unique(cns$sp)[[1]]
pdim <- c(1000, 500)
abnd <- abund_index(cns, pdim, 20)
out <- tt_test_one(spp, luquillo_habitat, abnd, pdim, 20)

as_df(out)




# Add krig_lst to vignettes

# move to fgeo.base name_df_list, mat_enframe?

# Make krig safer and more consistent with other functions. 

# Develop generic as_df to format fgeo output as data frame
# rename tt_df by as_df
# krig_df() pull only the df component and rearrange arguments to 
# krig_df(var, ...) for convenient lapply().


vars <- c("c", "p")
krig_lst <- krig_lst(vars, soil_fake, quiet = TRUE)
out <- as_df(krig_lst)
head(out)
tail(out)






# Consider moving these functions to fgeo.base, and doing these changes:
#   rename mat_enframe_ls as list_columns() or gather(<matrix>)
#   raname mat_enframe() of class "tt" as as_df(<tt_df>)

# release()
