library(dplyr)

cns <- fgeo.data::luquillo_tree6_1ha

cns %>% 
  filter(status == "A") %>% 
  count(sp) %>% 
  arrange(desc(n)) %>% 
  top_n(3) %>% 
  pull(sp)

cns %>% count(sp) %>% fgeo.tool::row_top(n, 3)

cns %>% fgeo.tool::row_top("sp", 3) %>% select(sp)


test_that("regression with Luquillo", {
  # skip_if_not_installed("fgeo.data")
  cns <- fgeo.data::luquillo_tree6_random
  hab <- fgeo.data::luquillo_habitat
  # Via extract_plotdim()
  # See also https://forestgeo.si.edu/sites/north-america/luquillo
  pdim <- c(320, 500)
  # Via extract_gridsize()
  gsize <- 5
  abun_quad <- abundanceperquad(
    cns,
    plotdim = pdim,
    gridsize = gsize,
    type = 'abund'
  )$abund
  
  sp_alive_3most_abun <- c("PREMON", "CASARB", "SLOBER")
  sp_alive_1most_abun <- sp_alive_abundant[[1]]
  out_onesp <- torusonesp.all(
    species = "PREMON",
    hab.index20 = hab,
    allabund20 = abun_quad,
    plotdim = pdim,
    gridsize = gsize
  )
  
  # one_sp_pasoh <- t(out_onesp)
  # expect_known_output(
  #   one_sp_pasoh,
  #   "ref-one_sp_pasoh",
  #   print = TRUE,
  #   update = TRUE
  # )
})
