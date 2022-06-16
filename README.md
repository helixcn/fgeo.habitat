
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
remotes::install_github("helixcn/fgeo.habitat")
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

### Habitat-species associations.

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

tt <- tt_test(census, species, habitat)
tt
#> [[1]]
#>        N.Hab.1 Gr.Hab.1 Ls.Hab.1 Eq.Hab.1 Rep.Agg.Neut.1 Obs.Quantile.1
#> CASARB      25     1489      109        2              0       0.930625
#>        N.Hab.2 Gr.Hab.2 Ls.Hab.2 Eq.Hab.2 Rep.Agg.Neut.2 Obs.Quantile.2
#> CASARB      12      168     1431        1              0          0.105
#>        N.Hab.3 Gr.Hab.3 Ls.Hab.3 Eq.Hab.3 Rep.Agg.Neut.3 Obs.Quantile.3
#> CASARB      14      567     1029        4              0       0.354375
#>        N.Hab.4 Gr.Hab.4 Ls.Hab.4 Eq.Hab.4 Rep.Agg.Neut.4 Obs.Quantile.4
#> CASARB      15      934      661        5              0        0.58375
#> 
#> [[2]]
#>        N.Hab.1 Gr.Hab.1 Ls.Hab.1 Eq.Hab.1 Rep.Agg.Neut.1 Obs.Quantile.1
#> PREMON      59      389     1208        3              0       0.243125
#>        N.Hab.2 Gr.Hab.2 Ls.Hab.2 Eq.Hab.2 Rep.Agg.Neut.2 Obs.Quantile.2
#> PREMON      75     1562       37        1              1        0.97625
#>        N.Hab.3 Gr.Hab.3 Ls.Hab.3 Eq.Hab.3 Rep.Agg.Neut.3 Obs.Quantile.3
#> PREMON      56      632      963        5              0          0.395
#>        N.Hab.4 Gr.Hab.4 Ls.Hab.4 Eq.Hab.4 Rep.Agg.Neut.4 Obs.Quantile.4
#> PREMON      44      222     1375        3              0        0.13875
#> 
#> [[3]]
#>        N.Hab.1 Gr.Hab.1 Ls.Hab.1 Eq.Hab.1 Rep.Agg.Neut.1 Obs.Quantile.1
#> SLOBER      14      492     1092       16              0         0.3075
#>        N.Hab.2 Gr.Hab.2 Ls.Hab.2 Eq.Hab.2 Rep.Agg.Neut.2 Obs.Quantile.2
#> SLOBER      16      473     1125        2              0       0.295625
#>        N.Hab.3 Gr.Hab.3 Ls.Hab.3 Eq.Hab.3 Rep.Agg.Neut.3 Obs.Quantile.3
#> SLOBER      19     1181      415        4              0       0.738125
#>        N.Hab.4 Gr.Hab.4 Ls.Hab.4 Eq.Hab.4 Rep.Agg.Neut.4 Obs.Quantile.4
#> SLOBER      17     1151      440        9              0       0.719375
#> 
#> attr(,"class")
#> [1] "tt_lst" "list"
```

``` r
tt_df <- to_df(tt)
# Try also: View(tt_df)
head(tt_df)
#>           metric     sp       value
#> 1        N.Hab.1 CASARB   25.000000
#> 2       Gr.Hab.1 CASARB 1489.000000
#> 3       Ls.Hab.1 CASARB  109.000000
#> 4       Eq.Hab.1 CASARB    2.000000
#> 5 Rep.Agg.Neut.1 CASARB    0.000000
#> 6 Obs.Quantile.1 CASARB    0.930625

tail(tt_df)
#>            metric     sp       value
#> 67        N.Hab.4 SLOBER   17.000000
#> 68       Gr.Hab.4 SLOBER 1151.000000
#> 69       Ls.Hab.4 SLOBER  440.000000
#> 70       Eq.Hab.4 SLOBER    9.000000
#> 71 Rep.Agg.Neut.4 SLOBER    0.000000
#> 72 Obs.Quantile.4 SLOBER    0.719375
```

### Krige soil data.

``` r
result <- krig(soil_fake, var = c("c", "p"), quiet = TRUE)
head(to_df(result))
#>   var   x  y        z
#> 1   c  10 10 2.134696
#> 2   c  30 10 2.119651
#> 3   c  50 10 2.104591
#> 4   c  70 10 2.089517
#> 5   c  90 10 2.074427
#> 6   c 110 10 2.059322

tail(to_df(result))
#>      var   x   y        z
#> 2295   p 890 450 5.835048
#> 2296   p 910 450 5.826698
#> 2297   p 930 450 5.819219
#> 2298   p 950 450 5.812612
#> 2299   p 970 450 5.806876
#> 2300   p 990 450 5.802012
```

``` r
summary(result)
#> var: c 
#> df
#> 'data.frame':    1150 obs. of  3 variables:
#>  $ x: num  10 30 50 70 90 110 130 150 170 190 ...
#>  $ y: num  10 10 10 10 10 10 10 10 10 10 ...
#>  $ z: num  2.13 2.12 2.1 2.09 2.07 ...
#> 
#> df.poly
#> 'data.frame':    1150 obs. of  3 variables:
#>  $ gx: num  10 30 50 70 90 110 130 150 170 190 ...
#>  $ gy: num  10 10 10 10 10 10 10 10 10 10 ...
#>  $ z : num  2.13 2.12 2.1 2.09 2.07 ...
#> 
#> lambda
#> 'numeric'
#>  num 1
#> 
#> vg
#> 'variogram'
#> List of 20
#>  $ u               : num [1:9] 60.9 86.5 103 122.7 146.1 ...
#>  $ v               : num [1:9] 0.284 0.422 0.882 0.543 0.211 ...
#>  $ n               : num [1:9] 7 9 10 10 18 19 36 34 38
#>  $ sd              : num [1:9] 0.414 0.48 0.633 0.501 0.405 ...
#>  $ bins.lim        : num [1:31] 1.00e-12 2.00 2.38 2.84 3.38 ...
#>  $ ind.bin         : logi [1:30] FALSE FALSE FALSE FALSE FALSE FALSE ...
#>  $ var.mark        : num 0.317
#>  $ beta.ols        : num 1.36e-09
#>  $ output.type     : chr "bin"
#>  $ max.dist        : num 320
#>  $ estimator.type  : chr "classical"
#>  $ n.data          : int 30
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
#>  $ nugget               : num 0.352
#>  $ cov.pars             : num [1:2] 0 160
#>  $ cov.model            : chr "exponential"
#>  $ kappa                : num 0.5
#>  $ value                : num 4.64
#>  $ trend                : chr "cte"
#>  $ beta.ols             : num 1.36e-09
#>  $ practicalRange       : num 480
#>  $ max.dist             : num 320
#>  $ minimisation.function: chr "optim"
#>  $ weights              : chr "npairs"
#>  $ method               : chr "WLS"
#>  $ fix.nugget           : logi FALSE
#>  $ fix.kappa            : logi TRUE
#>  $ lambda               : num 1
#>  $ message              : chr "optim convergence code: 0"
#>  $ call                 : language variofit(vario = vg, ini.cov.pars = c(initialVal, startRange), cov.model = varModels[i],      nugget = initialVal)
#> 
#> var: p 
#> df
#> 'data.frame':    1150 obs. of  3 variables:
#>  $ x: num  10 30 50 70 90 110 130 150 170 190 ...
#>  $ y: num  10 10 10 10 10 10 10 10 10 10 ...
#>  $ z: num  6.37 6.35 6.33 6.31 6.29 ...
#> 
#> df.poly
#> 'data.frame':    1150 obs. of  3 variables:
#>  $ gx: num  10 30 50 70 90 110 130 150 170 190 ...
#>  $ gy: num  10 10 10 10 10 10 10 10 10 10 ...
#>  $ z : num  6.37 6.35 6.33 6.31 6.29 ...
#> 
#> lambda
#> 'numeric'
#>  num 1
#> 
#> vg
#> 'variogram'
#> List of 20
#>  $ u               : num [1:9] 60.9 86.5 103 122.7 146.1 ...
#>  $ v               : num [1:9] 0.396 0.4 0.11 0.402 0.385 ...
#>  $ n               : num [1:9] 7 9 10 10 18 19 36 34 38
#>  $ sd              : num [1:9] 0.592 0.414 0.133 0.409 0.488 ...
#>  $ bins.lim        : num [1:31] 1.00e-12 2.00 2.38 2.84 3.38 ...
#>  $ ind.bin         : logi [1:30] FALSE FALSE FALSE FALSE FALSE FALSE ...
#>  $ var.mark        : num 0.267
#>  $ beta.ols        : num -3.14e-09
#>  $ output.type     : chr "bin"
#>  $ max.dist        : num 320
#>  $ estimator.type  : chr "classical"
#>  $ n.data          : int 30
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
#>  $ nugget               : num 0.305
#>  $ cov.pars             : num [1:2] 0 160
#>  $ cov.model            : chr "exponential"
#>  $ kappa                : num 0.5
#>  $ value                : num 0.818
#>  $ trend                : chr "cte"
#>  $ beta.ols             : num -3.14e-09
#>  $ practicalRange       : num 479
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
