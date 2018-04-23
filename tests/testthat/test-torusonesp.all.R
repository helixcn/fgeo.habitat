context("torusonesp.all.R")

library(fgeo.habitat)
library(dplyr)

# Regression --------------------------------------------------------------

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



# Too few species ---------------------------------------------------------

test_that("fails with a one-species dataset", {
  # Setup
  skip_if_not_installed("pasoh")
  total_abundance <- 50
  cns_too_few_sp <- pasoh::pasoh_3spp %>% 
    filter(sp  == "GIROPA") %>% 
    filter(status == "A") %>% 
    group_by(sp) %>% 
    sample_n(total_abundance) %>% 
    ungroup() 
  hab <- pasoh::pasoh_hab_index20
  pdim <- c(1000, 500)
  gsize <- 20
  
  abun_quad <- abundanceperquad(
    cns_too_few_sp,
    plotdim = pdim,
    gridsize = gsize,
    type = 'abund'
  )$abund
  
  this_sp <- "GIROPA"
  expect_error(
    torusonesp.all(
      species = this_sp,
      hab.index20 = hab,
      allabund20 = abun_quad,
      plotdim = pdim,
      gridsize = gsize
    ), 
    "two or more species"
  )
})



test_that("passes with a two-species dataset", {
  # FIXME: Reduce duplication.
  # Setup
  skip_if_not_installed("pasoh")
  total_abundance <- 50
  cns_too_few_sp <- pasoh::pasoh_3spp %>% 
    filter(sp   %in% c("XERONO", "GIROPA")) %>% 
    filter(status == "A") %>% 
    group_by(sp) %>% 
    sample_n(total_abundance) %>% 
    ungroup() 
  hab <- pasoh::pasoh_hab_index20
  pdim <- c(1000, 500)
  gsize <- 20
  
  abun_quad <- abundanceperquad(
    cns_too_few_sp,
    plotdim = pdim,
    gridsize = gsize,
    type = 'abund'
  )$abund
  
  this_sp <- "XERONO"
  expect_silent(
    torusonesp.all(
      species = this_sp,
      hab.index20 = hab,
      allabund20 = abun_quad,
      plotdim = pdim,
      gridsize = gsize
    )
  )
})

