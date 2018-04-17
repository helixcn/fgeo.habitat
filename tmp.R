# Census data
# For a small example, choosing only three species from BCI
census <- bciex::bci12t7mini
some_species <- c("hybapr", "faraoc", "des2pa")
census <- with(census, census[sp %in% some_species, ])
head(census)

# Habitat data
habitat <- bciex::bci_habitat
# Small fix to unexpected name in the example dataset
names(habitat)[3] <- "habitats"
head(habitat)

# Abundance data
abundance_per_quadrat <- abundanceperquad(
  census,
  plotdim = c(1000, 500),
  gridsize = 20,
  type = 'abund'
)$abund

# Better
library(dplyr)
census <- filter(
  census, 
  # Alive trees
  status == "A", 
  # drop missing values of dbh
  !is.na(dbh), 
  # Keep trees at or over the minimum size
  dbh >= 10
)

abundance <- count(census, quadrat, sp)
abundance <- abundance %>% 
  arrange(desc(n)) %>% 
  filter(sp == "faraoc") %>% 
  pull(n)

abundance_per_quadrat[1:10]
dim(abundance_per_quadrat)

result <- torusonesp.all(
  species = "faraoc",
  hab.index20 = habitat,
  # allabund20 = abundance,
  allabund20 = abundance_per_quadrat,
  plotdim = c(1000, 500),
  gridsize = 20
)

# Iterate over all (or a subset of) species
all_species <- unique(census$sp)

lapply(
  all_species, 
  torusonesp.all,
  hab.index20 = habitat,
  allabund20 = abundance_per_quadrat,
  plotdim = c(1000, 500),
  gridsize = 20
)











census <- pasoh::pasoh_3spp
head(census)

habitat <- pasoh::pasoh_hab_index20
head(habitat)

abundance_per_quadrat <- ctfs::abundanceperquad(
  census,
  plotdim = c(1000, 500),
  gridsize = 20,
  type = 'abund'
)$abund

abundance_per_quadrat[1:10]
dim(abundance_per_quadrat)

result_one <- torusonesp.all(
  species = "GIROPA",
  hab.index20 = habitat,
  allabund20 = abundance_per_quadrat,
  plotdim = c(1000, 500),
  gridsize = 20
)
result_one

# Iterate over all (or a subset of) species
all_species <- unique(census$sp)

result_all <- lapply(
  all_species, 
  torusonesp.all,
  hab.index20 = habitat,
  allabund20 = abundance_per_quadrat,
  plotdim = c(1000, 500),
  gridsize = 20
)

# Make the output easier to view
Reduce(cbind, lapply(result_all, t))
