context("torusonesp.all.R")

# Ensure consistent values accross runs
set.seed(123)

library(fgeo.habitat)
library(dplyr)

# Make a tiny dataset to run test fast
cns <- pasoh::pasoh_3spp
cns_tiny <- cns %>% 
  as_tibble() %>% 
  filter(status == "A") %>% 
  group_by(sp) %>% 
  sample_n(50) %>% 
  ungroup() 

hab <- pasoh::pasoh_hab_index20
pdim <- c(1000, 500)
gsize <- 20
this_sp <- "GIROPA"



# Regression --------------------------------------------------------------

test_that("regression with Pasoh", {
  skip_if_not_installed("pasoh")

  abun_quad <- abundanceperquad(
    cns_tiny, plotdim = pdim, gridsize = gsize, type = 'abund'
  )$abund
  
  out_onesp <- torusonesp.all(
    species = this_sp,
    hab.index20 = hab,
    allabund20 = abun_quad,
    plotdim = pdim,
    gridsize = gsize
  )
  # Transpose for better display
  one_sp_pasoh <- t(out_onesp)
  
  expect_known_output(
    one_sp_pasoh, "ref-one_sp_n50_pasoh", print = TRUE, update = TRUE
  )
})



# Minimum number of species in census -------------------------------------

test_that("fails with a one-species dataset", {
  skip_if_not_installed("pasoh")
  
  # If datasets has only one species, the test fails -- this makes sense but
  # should confirm with Russo et al
  cns_1sp <- cns_tiny %>% 
    filter(sp  == this_sp)

  abun_quad <- abundanceperquad(
    cns_1sp, plotdim = pdim, gridsize = gsize, type = 'abund'
  )$abund
  
  expect_error(
    torusonesp.all(
      species = this_sp,
      hab.index20 = hab,
      allabund20 = abun_quad,
      plotdim = pdim,
      gridsize = gsize
    ), 
    "Invalid stem density of focal sp per habitat of focal map"
  )
})

test_that("passes with a two-species dataset", {
  skip_if_not_installed("pasoh")
  
  cns_2sp <- cns_tiny %>% 
    filter(sp %in% sample(cns_tiny$sp, 2))
  
  abun_quad <- abundanceperquad(
    cns_2sp, plotdim = pdim, gridsize = gsize, type = 'abund'
  )$abund
  
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



# Datasets other than Pasoh -----------------------------------------------

# Small dataset from BCI
cns_bci <- bciex::bci12t7mini
sp_top3 <- cns_bci %>%
  count(sp) %>%
  arrange(desc(n)) %>%
  top_n(3) %>%
  pull(sp)

cns_3sp_bci <- filter(cns_bci, status == "A", sp  %in% sp_top3)
this_sp_bci <- first(sp_top3)

pdim_bci <- c(1000, 500)
gsize_bci <- 20

test_that("passes habitat data in bci package but FIXME not with bciex", {
  skip_if_not_installed("bci")
  skip_if_not_installed("bciex")

  abun_quad_bci <- abundanceperquad(
    cns_3sp_bci, plotdim = pdim_bci, gridsize = gsize_bci, type = 'abund'
  )$abund
  
  expect_silent(
    torusonesp.all(
      species = this_sp_bci,
      hab.index20 = bci::bci_habitat,
      allabund20 = abun_quad_bci,
      plotdim = pdim_bci,
      gridsize = gsize_bci
    )
  )
  
  # FIXME: This should pass but fails
  hab_bciex <- rename(bciex::bci_habitat, habitats = habitat) %>% 
    mutate(habitats = as.integer(as.factor(habitats)))
  expect_silent(
    torusonesp.all(
      species = this_sp_bci,
      hab.index20 = hab_bciex,
      allabund20 = abun_quad_bci,
      plotdim = pdim_bci,
      gridsize = gsize_bci
    )
  )
  
  hab_random <- mutate(
    hab_bciex, habitats = sample(1:7, length(habitats), replace = TRUE)
  )
  expect_silent(
    torusonesp.all(
      species = this_sp_bci,
      hab.index20 = hab_random,
      allabund20 = abun_quad_bci,
      plotdim = pdim_bci,
      gridsize = gsize_bci
    )
  )
})
