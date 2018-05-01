
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="https://i.imgur.com/m8FNhQR.png" align="right" height=88 /> Analize soils and tree-habitat associations

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/forestgeo/fgeo.habitat.svg?branch=master)](https://travis-ci.org/forestgeo/fgeo.habitat)
[![Coverage
status](https://coveralls.io/repos/github/forestgeo/fgeo.habitat/badge.svg)](https://coveralls.io/r/forestgeo/fgeo.habitat?branch=master)
[![CRAN
status](http://www.r-pkg.org/badges/version/fgeo.habitat)](https://cran.r-project.org/package=fgeo.habitat)

## Installation

``` r
# install.packages("remotes")
remotes::install_github("forestgeo/fgeo.habitat")
```

For details on how to install packages from GitHub, see [this
article](https://goo.gl/dQKEeg).

## Example

``` r
library(fgeo.habitat)
```

Habitat-species associations.

``` r
# Setup

# For easier data wranging
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

# Example data
habitat <- luquillo_habitat
census <- luquillo_top3_sp

# Pick alive trees, of 10 mm or more
pick <- filter(census, status == "A", dbh >= 10)
# Pick sufficiently abundant trees
pick <- add_count(pick, sp)
pick <- filter(pick, n > 50)

species <- unique(pick$sp)

out <- tt_test(species, census, habitat)
# Try also: View(out)
head(out)
#>           metric     sp       value
#> 1        N.Hab.1 CASARB   25.000000
#> 2       Gr.Hab.1 CASARB 1489.000000
#> 3       Ls.Hab.1 CASARB  109.000000
#> 4       Eq.Hab.1 CASARB    2.000000
#> 5 Rep.Agg.Neut.1 CASARB    0.000000
#> 6 Obs.Quantile.1 CASARB    0.930625
tail(out)
#>            metric     sp       value
#> 67        N.Hab.4 SLOBER   17.000000
#> 68       Gr.Hab.4 SLOBER 1151.000000
#> 69       Ls.Hab.4 SLOBER  440.000000
#> 70       Eq.Hab.4 SLOBER    9.000000
#> 71 Rep.Agg.Neut.4 SLOBER    0.000000
#> 72 Obs.Quantile.4 SLOBER    0.719375
```

Krige soil data.

``` r
# Randomized data -- not for research. See ?soil_random.
df <- soil_random

result <- krig(df, var = "M3Al")
#> variog: computing omnidirectional variogram
#> variofit: covariance model used is exponential 
#> variofit: weights used: npairs 
#> variofit: minimisation function used: optim 
#> variofit: covariance model used is circular 
#> variofit: weights used: npairs 
#> variofit: minimisation function used: optim 
#> variofit: covariance model used is cauchy 
#> variofit: weights used: npairs 
#> variofit: minimisation function used: optim 
#> variofit: covariance model used is gaussian 
#> variofit: weights used: npairs 
#> variofit: minimisation function used: optim 
#> ksline: kriging location:  1 out of 1250 
#> ksline: kriging location:  101 out of 1250 
#> ksline: kriging location:  201 out of 1250 
#> ksline: kriging location:  301 out of 1250 
#> ksline: kriging location:  401 out of 1250 
#> ksline: kriging location:  501 out of 1250 
#> ksline: kriging location:  601 out of 1250 
#> ksline: kriging location:  701 out of 1250 
#> ksline: kriging location:  801 out of 1250 
#> ksline: kriging location:  901 out of 1250 
#> ksline: kriging location:  1001 out of 1250 
#> ksline: kriging location:  1101 out of 1250 
#> ksline: kriging location:  1201 out of 1250 
#> ksline: kriging location:  1250 out of 1250 
#> Kriging performed using global neighbourhood
```

``` r
# The result is a complex list
names(result)
#> [1] "df"      "df.poly" "lambda"  "vg"      "vm"
```

``` r
summary(result)
#> df
#> 'data.frame':    1250 obs. of  3 variables:
#>  $ x: num  10 30 50 70 90 110 130 150 170 190 ...
#>  $ y: num  10 10 10 10 10 10 10 10 10 10 ...
#>  $ z: num  825 826 826 827 828 ...
#> 
#> df.poly
#> 'data.frame':    1250 obs. of  3 variables:
#>  $ gx: num  10 30 50 70 90 110 130 150 170 190 ...
#>  $ gy: num  10 10 10 10 10 10 10 10 10 10 ...
#>  $ z : num  825 826 827 828 829 ...
#> 
#> nm
#> 'numeric'
#>  num 1
#> 
#> nm
#> 'variogram'
#> List of 20
#>  $ u               : num [1:15] 25.4 30.3 36 42.9 51.1 ...
#>  $ v               : num [1:15] 52791 54636 65742 58004 49142 ...
#>  $ n               : num [1:15] 66 1110 73 1101 61 ...
#>  $ sd              : num [1:15] 64847 75753 89509 79157 63291 ...
#>  $ bins.lim        : num [1:31] 1.00e-12 2.00 2.38 2.84 3.38 ...
#>  $ ind.bin         : logi [1:30] FALSE FALSE FALSE FALSE FALSE FALSE ...
#>  $ var.mark        : num 55269
#>  $ beta.ols        : num -1.71e-08
#>  $ output.type     : chr "bin"
#>  $ max.dist        : num 320
#>  $ estimator.type  : chr "classical"
#>  $ n.data          : int 625
#>  $ lambda          : num 1
#>  $ trend           : chr "cte"
#>  $ pairs.min       : num 5
#>  $ nugget.tolerance: num 1e-12
#>  $ direction       : chr "omnidirectional"
#>  $ tolerance       : chr "none"
#>  $ uvec            : num [1:30] 1 2.19 2.61 3.11 3.7 ...
#>  $ call            : language variog(geodata = geodata, breaks = breaks, trend = trend, pairs.min = 5)
#> 
#> nm
#> 'variomodel', variofit'
#> List of 17
#>  $ nugget               : num 56607
#>  $ cov.pars             : num [1:2] 40008 1533
#>  $ cov.model            : chr "cauchy"
#>  $ kappa                : num 0.5
#>  $ value                : num 6.38e+10
#>  $ trend                : chr "cte"
#>  $ beta.ols             : num -1.71e-08
#>  $ practicalRange       : num 30614
#>  $ max.dist             : num 320
#>  $ minimisation.function: chr "optim"
#>  $ weights              : chr "npairs"
#>  $ method               : chr "WLS"
#>  $ fix.nugget           : logi FALSE
#>  $ fix.kappa            : logi TRUE
#>  $ lambda               : num 1
#>  $ message              : chr "optim convergence code: 0"
#>  $ call                 : language variofit(vario = vg, ini.cov.pars = c(initialVal, startRange), cov.model = varModels[i],      nugget = initialVal)
```

## Information

  - [Getting help](SUPPORT.md).
  - [Contributing](CONTRIBUTING.md).
  - [Contributor Code of Conduct](CODE_OF_CONDUCT.md).

## Acknowledgements

Thanks to all partners of ForestGEO who shared their ideas and code.
Functions’ authors include Graham Zemunik, Sabrina Russo, Daniel Zuleta,
Matteo Detto, Kyle Harms, Gabriel Arellano.
