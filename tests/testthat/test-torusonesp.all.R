context("torusonesp.all.R")

# Ensure consistent values accross runs
set.seed(123)
library(dplyr)

# Small dataset from Luquillo
cns_luq <- fgeo.data::luquillo_tree6_random
sp_top3 <- cns_luq %>%
  count(sp) %>%
  arrange(desc(n)) %>%
  top_n(3) %>%
  pull(sp)
cns_3sp_luq <- filter(cns_luq, status == "A", sp  %in% sp_top3)
this_sp_luq <- first(sp_top3)
hab_luq <- fgeo.data::luquillo_habitat
pdim_luq <- c(320, 500)
gsize_luq <- 20

abun_quad_luq <- abundanceperquad(
  cns_3sp_luq, plotdim = pdim_luq, gridsize = gsize_luq, type = 'abund'
)$abund

test_that("works with luquillo", {
  expect_silent({
    out <- torusonesp.all(
      species = this_sp_luq,
      hab.index20 = hab_luq,
      allabund20 = abun_quad_luq,
      plotdim = pdim_luq,
      gridsize = gsize_luq
    )
  })
  expect_known_output(out, "ref-luq_top3_premon", print = TRUE, update = TRUE)
})

# Pasoh -------------------------------------------------------------------

test_that("tests with Pasoh", {
  skip_if_not_installed("pasoh")
  pdim <- c(1000, 500)
  gsize <- 20
  this_sp <- "GIROPA"
  # Dependencies on Pasoh
  cns <- pasoh::pasoh_3spp
  cns_tiny <- cns %>% 
    as_tibble() %>% 
    filter(status == "A") %>% 
    group_by(sp) %>% 
    # To run test fast
    sample_n(50) %>% 
    ungroup() 
  hab <- pasoh::pasoh_hab_index20

  
  
  # REGRESSION
  
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


  
  # FAILS WITH A ONE-SPECIES DATASET
  
  # If datasets has only one species, the test fails -- this makes sense but
  # should confirm with Russo et al
  cns_1sp <- cns_tiny %>% 
    filter(sp  == this_sp)

  abun_quad <- abundanceperquad(
    cns_1sp, plotdim = pdim, gridsize = gsize, type = 'abund'
  )$abund
  
  expect_warning(
    torusonesp.all(
      species = this_sp,
      hab.index20 = hab,
      allabund20 = abun_quad,
      plotdim = pdim,
      gridsize = gsize
    ), 
    "Values can't be compared:"
  )
  
  
  
  # PASSES WITH A TWO-SPECIES DATASET
  
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




# BCI ---------------------------------------------------------------------

test_that("passes with different habitat datasets from BCI", {
  skip_if_not_installed("bci")
  skip_if_not_installed("bciex")
  
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

  abun_quad_bci <- abundanceperquad(
    cns_3sp_bci, plotdim = pdim_bci, gridsize = gsize_bci, type = 'abund'
  )$abund
  
  # With bci, all is good
  expect_silent(
    torusonesp.all(
      species = this_sp_bci,
      hab.index20 = bci::bci_habitat,
      allabund20 = abun_quad_bci,
      plotdim = pdim_bci,
      gridsize = gsize_bci
    )
  )
  
  # With bciex, which habitat data is based from elevation data, it's unclear
  # why there are many habitats with cero species.
  hab_bciex <- rename(bciex::bci_habitat, habitats = habitat) %>%
    mutate(habitats = as.integer(as.factor(habitats)))
  expect_warning(
    torusonesp.all(
      species = this_sp_bci,
      hab.index20 = hab_bciex,
      allabund20 = abun_quad_bci,
      plotdim = pdim_bci,
      gridsize = gsize_bci
    )
  )
  
  # With randomly generated habitats, all is good.
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


