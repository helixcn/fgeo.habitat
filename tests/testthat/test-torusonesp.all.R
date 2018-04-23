context("torusonesp.all.R")

library(fgeo.habitat)
library(dplyr)

test_that("regression with Pasoh", {
  # Setup
  skip_if_not_installed("pasoh")
  set.seed(123)
  total_abundance <- 50
  cns <- pasoh::pasoh_3spp
  cns_n50 <- cns %>% 
    as_tibble() %>% 
    filter(status == "A") %>% 
    group_by(sp) %>% 
    sample_n(total_abundance) %>% 
    ungroup() 
  hab <- pasoh::pasoh_hab_index20
  pdim <- c(1000, 500)
  gsize <- 20

  abun_quad <- abundanceperquad(
    cns_n50,
    plotdim = pdim,
    gridsize = gsize,
    type = 'abund'
  )$abund
  
  this_sp <- "GIROPA"
  out_onesp <- torusonesp.all(
    species = this_sp,
    hab.index20 = hab,
    allabund20 = abun_quad,
    plotdim = pdim,
    gridsize = gsize
  )
  one_sp_pasoh <- t(out_onesp)
  
  expect_known_output(
    one_sp_pasoh,
    "ref-one_sp_n50_pasoh",
    print = TRUE,
    update = TRUE
  )
})








