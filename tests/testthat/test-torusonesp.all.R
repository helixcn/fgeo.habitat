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
hab_luq <- fgeo.data::luquillo_habitat
pdim_luq <- c(320, 500)
gsize_luq <- 20
this_sp_luq <- first(sp_top3)

abundance_sp <- function(n) {
  .cns <- filter(cns_luq, status == "A", sp  %in% sp_top3[1:n])
  abundanceperquad(
    .cns, plotdim = pdim_luq, gridsize = gsize_luq, type = 'abund'
  )$abund
}
expect_silent_with_n <- function(n) {
  expect_silent({
    torusonesp.all(
      species = this_sp_luq,
      hab.index20 = hab_luq,
      allabund20 = abundance_sp(n),
      plotdim = pdim_luq,
      gridsize = gsize_luq
    )
  })
}
test_that("works with luquillo", {
  out <- expect_silent_with_n(3)
  expect_known_output(out, "ref-luq_top3_premon", print = TRUE, update = TRUE)
})

test_that("outputs silently with a 1- and 2- species dataset from Luquillo", {
  expect_silent_with_n(1)
  expect_silent_with_n(2)
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
  cns_1sp <- cns_tiny %>% filter(sp  == this_sp)
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
  cns_2sp <- cns_tiny %>% filter(sp %in% sample(cns_tiny$sp, 2))
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
