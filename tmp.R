# TODO

# Test to brake abund_index.
# Remove bciex and use fgeo.data instead.
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

tiny %>% 
  filter(CensusID == 6) %>% 
  add_index(plotdim = pdm) %>% 
  select(index, quadrat, sp, tag, treeID, stemID, status, dbh, gx, gy) %>% 
  arrange(index, sp, tag) %>% 
  unique() %>% 
  abund_index(pdm, gsz)




