context("tt_test_one.R")

# Ensure consistent values accross runs
set.seed(123)
library(dplyr)
library(fgeo.habitat)

# Small dataset from Luquillo
cns_luq <- luquillo_top3_sp
sp_top3_luq <- unique(cns_luq$sp)
hab_luq <- luquillo_habitat
pdim_luq <- c(320, 500)
gsize_luq <- 20
sp_top1_luq <- first(sp_top3_luq)

# Reduce duplication
abundance_sp <- function(n) {
  .cns <- filter(cns_luq, status == "A", sp  %in% sp_top3_luq[1:n])
  abundanceperquad(
    .cns, plotdim = pdim_luq, gridsize = gsize_luq, type = 'abund'
  )$abund
}

test_that("regression: outputs equal to original function", {
  source("ref-torusonesp.all.R")
  ref <- torusonesp.all(
    species = sp_top1_luq,
    hab.index20 = hab_luq,
    allabund20 = abundance_sp(1),
    plotdim = pdim_luq,
    gridsize = gsize_luq
  )
  
  now <- tt_test_one(
    species = sp_top1_luq,
    hab.index20 = hab_luq,
    allabund20 = abundance_sp(1),
    plotdim = pdim_luq,
    gridsize = gsize_luq
  )
  expect_equal(ref, now)
})



# Reduce duplication
expect_silent_with_n <- function(n) {
  expect_silent({
    tt_test_one(
      species = sp_top1_luq,
      hab.index20 = hab_luq,
      allabund20 = abundance_sp(n),
      plotdim = pdim_luq,
      gridsize = gsize_luq
    )
  })
}

test_that("works with luquillo", {
  # Use data with 3 species but get torus translation for only one.
  out <- expect_silent_with_n(3)
  expect_true(is.numeric(out))
  expect_true(is.matrix(out))
})

test_that("outputs silently with a 1- and 2- species dataset from Luquillo", {
  expect_silent_with_n(1)
  expect_silent_with_n(2)
})

test_that("regression: outputs equal to known output", {
  # One species
  out_one <- expect_silent_with_n(3)
  expect_known_output(
    out_one, "ref-luq_top3_premon", print = TRUE, update = TRUE
  )
  
  # Multiple species
  census_data <- luquillo_top3_sp
  alive_trees <- census_data[census_data$status == "A", ]
  habitat_data <- luquillo_habitat
  plot_dimensions <- c(320, 500)
  grid_size <- 20
  abundance_per_quadrat <- abundanceperquad(
    alive_trees,
    plotdim = plot_dimensions,
    gridsize = grid_size,
    type = 'abund'
  )$abund
  all_species <- unique(census_data$sp)
  out_all <- lapply(
    X = all_species,
    FUN = tt_test_one,
    # Other arguments passed to tt_test_one
    hab.index20 = habitat_data,
    allabund20 = abundance_per_quadrat,
    plotdim = plot_dimensions,
    gridsize = grid_size
  )
  out_all <- Reduce(rbind, out_all)
  expect_known_output(out_all, "ref-luq_3sp", print = TRUE, update = TRUE)
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
  out_onesp <- tt_test_one(
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
  cns_1sp <- cns_tiny %>% filter(sp  == this_sp)
  abun_quad <- abundanceperquad(
    cns_1sp, plotdim = pdim, gridsize = gsize, type = 'abund'
  )$abund

  expect_error({
    expect_warning(
      tt_test_one(
        species = this_sp,
        hab.index20 = hab,
        allabund20 = abun_quad,
        plotdim = pdim,
        gridsize = gsize
      ),
      "Values can't be compared:"
    )
  })
  
  # PASSES WITH A TWO-SPECIES DATASET
  cns_2sp <- cns_tiny %>% filter(sp %in% sample(cns_tiny$sp, 2))
  abun_quad <- abundanceperquad(
    cns_2sp, plotdim = pdim, gridsize = gsize, type = 'abund'
  )$abund
  expect_silent(
    tt_test_one(
      species = this_sp,
      hab.index20 = hab,
      allabund20 = abun_quad,
      plotdim = pdim,
      gridsize = gsize
    )
  )
})
