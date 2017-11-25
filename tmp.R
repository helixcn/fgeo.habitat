library(ggplot2)

# Use randomized data set for examples (invalid for reaserch).
df <- krig::soil_random
head(df)

# Krige with automated parameters
kriged <- GetKrigedSoil(df, var = "M3Al")
str(kriged)

# Krige with specified parameters - these example params are rather arbitrary
# but are loosely based on what was chosen from the automated kriging
params <- list(
  model = "circular",
  range = 100,
  nugget = 1000,
  sill = 46000,
  kappa = 0.5
)
kriged.2 <- GetKrigedSoil(df, var = "M3Al", krigeParams = params)

# Have a look at the differences
g <- ggplot(kriged$df, aes(x = x, y = y, fill = z)) +
  geom_tile() +
  coord_equal()
g2 <- ggplot(kriged.2$df, aes(x = x, y = y, fill = z)) +
  geom_tile() +
  coord_equal()
g
g2
