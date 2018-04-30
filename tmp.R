# TODO

# Test that tt_test() fails with informative messages. 
# Improve function names in all the package
# Adress FIXME
# Improve the output of soil functions

# OTHER PACKAGES
# map: Write function to map raster
# abundance: Write function to calculate abundance per quadrat



library(tidyverse)
library(janitor)
library(fgeo.tool)
library(fgeo.habitat)

tiny <- fgeo.data::luquillo_stem_random_tiny
tiny

pdm <- extract_plotdim(luquillo_habitat)
gsz <- extract_gridsize(luquillo_habitat)
abund <- abund_index(tiny, pdm, gsz)
some <- 1:6
abund[some, some]

cns <- tiny %>% 
  filter(CensusID == 6) %>% 
  add_index(plotdim = pdm) %>% 
  select(index, quadrat, sp, tag, treeID, stemID, status, dbh, gx, gy) %>% 
  arrange(index, sp, tag) %>% 
  unique()

n_ctfs <- cns %>% abund_index(pdm, gsz)
n_janitor <- tabyl(cns, index, sp)


idx <- 1:400

n_dplyr <- cns %>% 
# fgeo.tool::add_index(plotdim = plot_dimensions) %>%
  count(index, sp) %>% 
  right_join(tibble(index = idx)) %>%
  tidyr::spread(sp, n, fill = 0) %>% 
  arrange(index) %>% 
  select(-index, -`<NA>`) %>% 
  t() %>% 
  as.data.frame() %>% 
  rlang::set_names(idx)


# Make them integers, not doubles.

n_ctfs <- n_ctfs[order(rownames(n_ctfs)), ] %>% 
  map_df(as.integer) %>% 
  as.data.frame()
n_dplyr <- n_dplyr[order(rownames(n_dplyr)), ] %>% 
  map_df(as.integer) %>% 
  as.data.frame()


compare(n_dplyr, n_ctfs)



list(n_ctfs, n_dplyr) %>% lapply("[[", 177) %>% reduce(all.equal)
list(n_ctfs, n_dplyr) %>% lapply(function(x) typeof(pull(x)))
