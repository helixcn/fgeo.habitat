context("tt_test_one.R")

# Ensure consistent values accross runs
set.seed(123)
library(dplyr)
library(fgeo.habitat)

# Luquillo ----------------------------------------------------------------

# Small dataset from Luquillo
cns_luq <- luquillo_top3_sp
sp_top3_luq <- unique(cns_luq$sp)
hab_luq <- luquillo_habitat
pdim_luq <- c(320, 500)
gsize_luq <- 20
sp_top1_luq <- first(sp_top3_luq)

# Reduce duplication
abundance_sp <- function(n) {
  .cns <- filter(cns_luq, status == "A", sp %in% sp_top3_luq[1:n])
  abund_index(.cns, pdim_luq, gsize_luq)
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
  
  expect_failure(
    expect_equal(ref, now), "Classes differ: matrix is not tt_one/matrix"
  )
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
    out_one, "ref-luq_top3_premon",
    print = TRUE, update = TRUE
  )

  # Multiple species
  census_data <- luquillo_top3_sp
  alive_trees <- census_data[census_data$status == "A", ]
  habitat_data <- luquillo_habitat
  pdim <- c(320, 500)
  gsize <- 20
  all_species <- unique(census_data$sp)
  out_all <- lapply(
    X = all_species,
    FUN = tt_test_one,
    # Other arguments passed to tt_test_one
    hab.index20 = habitat_data,
    allabund20 = abund_index(alive_trees, pdim, gsize),
    plotdim = pdim,
    gridsize = gsize
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
  out_onesp <- tt_test_one(
    species = this_sp,
    hab.index20 = hab,
    allabund20 = abund_index(cns_tiny, pdim, gsize),
    plotdim = pdim,
    gridsize = gsize
  )
  # Transpose for better display
  one_sp_pasoh <- t(out_onesp)
  expect_known_output(
    one_sp_pasoh, "ref-one_sp_n50_pasoh",
    print = TRUE, update = TRUE
  )

  # FAILS WITH A ONE-SPECIES DATASET
  # If datasets has only one species, the test fails -- this makes sense but
  # should confirm with Russo et al
  cns_1sp <- cns_tiny %>% filter(sp == this_sp)
  expect_error({
    expect_warning(
      tt_test_one(
        species = this_sp,
        hab.index20 = hab,
        allabund20 = abund_index(cns_1sp, pdim, gsize),
        plotdim = pdim,
        gridsize = gsize
      ),
      "Values can't be compared:"
    )
  })

  # PASSES WITH A TWO-SPECIES DATASET
  cns_2sp <- cns_tiny %>% filter(sp %in% sample(cns_tiny$sp, 2))
  expect_silent(
    tt_test_one(
      species = this_sp,
      hab.index20 = hab,
      allabund20 = abund_index(cns_2sp, pdim, gsize),
      plotdim = pdim,
      gridsize = gsize
    )
  )
})



# BCI ---------------------------------------------------------------------

test_that("outputs silently with good habitat data from BCI", {
  skip_if_not_installed("bciex")
  skip_if_not_installed("fgeo.tool")

  bci_elev <- list(
    col = bciex::bci_elevation,
    xdim = 1000,
    ydim = 500
  )
  bci_hab <- fgeo.tool::create_habitat(bci_elev, 20, 4)
  bci_cns <- bciex::bci12t7mini %>%
    filter(status == "A", dbh >= 10) %>%
    add_count(sp) %>%
    filter(n > 50)
  bci_pdim <- c(1000, 500)
  bci_gsz <- 20
  bci_sp <- unique(bci_cns$sp)[[1]]

  expect_silent(
    tt_test_one(
      species = bci_sp,
      hab.index20 = bci_hab,
      allabund20 = abund_index(bci_cns, bci_pdim, bci_gsz),
      plotdim = bci_pdim,
      gridsize = bci_gsz
    )
  )
})
