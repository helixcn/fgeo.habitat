# Avoid dependency on fgeo.data
luquillo_habitat <- fgeo.data::luquillo_habitat
use_data(luquillo_habitat, overwrite = TRUE)
luquillo_elevation <- fgeo.data::luquillo_elevation
use_data(luquillo_elevation, overwrite = TRUE)
luquillo_tree6_random <- fgeo.data::luquillo_tree6_random
use_data(luquillo_tree6_random, overwrite = TRUE)
luquillo_stem6_random <- fgeo.data::luquillo_stem6_random
use_data(luquillo_stem6_random, overwrite = TRUE)

cns <- fgeo.data::luquillo_tree6_random
top3_sp <- cns %>%
  dplyr::count(sp) %>%
  dplyr::arrange(desc(n)) %>%
  dplyr::top_n(3) %>%
  dplyr::pull(sp)
luquillo_top3_sp <- dplyr::filter(cns, sp  %in% top3_sp)
use_data(luquillo_top3_sp, overwrite = TRUE)
