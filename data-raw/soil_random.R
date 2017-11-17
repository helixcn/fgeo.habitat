# Some soil data from Barro Colorado Island.
# Source: Graham Zemunik (via https://goo.gl/2EwLQJ).

library(readr)
example_soil <- read_csv(
  "data-raw/from_Graham_Zemunik/from_Graham_Zemunik/example soil.csv"
)
use_data(example_soil)

# Randomize data
soil_random <- example_soil
var <- example_soil$M3Al
soil_random$M3Al <- sample(var, length(var))

# Expect FALSE
identical(soil_random, example_soil)
use_data(soil_random)
