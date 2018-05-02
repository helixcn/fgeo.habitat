
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
```

Habitat-species associations.

``` r
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
soil <- soil_random
str(soil_random, give.attr = FALSE)
#> Classes 'tbl_df', 'tbl' and 'data.frame':    100 obs. of  3 variables:
#>  $ gx  : int  229 49 569 689 229 949 489 709 489 829 ...
#>  $ gy  : int  210 270 30 270 170 330 230 370 150 330 ...
#>  $ m3al: num  945 1051 450 431 867 ...

out <- suppressMessages(
  krig(soil, var = "m3al")
)
summary(out)
#> df
#> 'data.frame':    1250 obs. of  3 variables:
#>  $ x: num  10 30 50 70 90 110 130 150 170 190 ...
#>  $ y: num  10 10 10 10 10 10 10 10 10 10 ...
#>  $ z: num  1018 1010 1002 995 987 ...
#> 
#> df.poly
#> 'data.frame':    1250 obs. of  3 variables:
#>  $ gx: num  10 30 50 70 90 110 130 150 170 190 ...
#>  $ gy: num  10 10 10 10 10 10 10 10 10 10 ...
#>  $ z : num  31.9 31.8 31.7 31.5 31.4 ...
#> 
#> lambda
#> 'numeric'
#>  num 0.5
#> 
#> vg
#> 'variogram'
#> List of 20
#>  $ u               : num [1:11] 30.3 42.9 60.9 86.5 103 ...
#>  $ v               : num [1:11] 21.2 32.2 21.3 22.1 25.3 ...
#>  $ n               : num [1:11] 28 22 81 105 42 154 181 222 306 506 ...
#>  $ sd              : num [1:11] 29.2 55.8 29.9 26.5 42.3 ...
#>  $ bins.lim        : num [1:31] 1.00e-12 2.00 2.38 2.84 3.38 ...
#>  $ ind.bin         : logi [1:30] FALSE FALSE FALSE FALSE FALSE FALSE ...
#>  $ var.mark        : num 20.6
#>  $ beta.ols        : num 1.34e-09
#>  $ output.type     : chr "bin"
#>  $ max.dist        : num 320
#>  $ estimator.type  : chr "classical"
#>  $ n.data          : int 100
#>  $ lambda          : num 1
#>  $ trend           : chr "cte"
#>  $ pairs.min       : num 5
#>  $ nugget.tolerance: num 1e-12
#>  $ direction       : chr "omnidirectional"
#>  $ tolerance       : chr "none"
#>  $ uvec            : num [1:30] 1 2.19 2.61 3.11 3.7 ...
#>  $ call            : language variog(geodata = geodata, breaks = breaks, trend = trend, pairs.min = 5)
#> 
#> vm
#> 'variomodel', variofit'
#> List of 17
#>  $ nugget               : num 20.5
#>  $ cov.pars             : num [1:2] 2360 15360
#>  $ cov.model            : chr "gaussian"
#>  $ kappa                : num 0.5
#>  $ value                : num 10619
#>  $ trend                : chr "cte"
#>  $ beta.ols             : num 1.34e-09
#>  $ practicalRange       : num 26586
#>  $ max.dist             : num 320
#>  $ minimisation.function: chr "optim"
#>  $ weights              : chr "npairs"
#>  $ method               : chr "WLS"
#>  $ fix.nugget           : logi FALSE
#>  $ fix.kappa            : logi TRUE
#>  $ lambda               : num 1
#>  $ message              : chr "optim convergence code: 1"
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
