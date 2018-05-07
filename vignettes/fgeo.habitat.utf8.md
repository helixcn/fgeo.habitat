---
title: "Get started"
author: "Mauro Lepore"
date: "2018-05-07"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Get started}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---




```r
library(fgeo.habitat)
library(tidyverse)
#> -- Attaching packages -------------------------------- tidyverse 1.2.1 --
#> v ggplot2 2.2.1     v purrr   0.2.4
#> v tibble  1.4.2     v dplyr   0.7.4
#> v tidyr   0.8.0     v stringr 1.3.0
#> v readr   1.1.1     v forcats 0.3.0
#> -- Conflicts ----------------------------------- tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
```

## Habitat-species associations

Torus translation test to determine habitat associations of tree species: `tt_test_lst()`, by Sabrina Russo, Daniel Zuleta, Matteo Detto, and Kyle Harms.


```r
# Example data
habitat <- luquillo_habitat
census <- luquillo_top3_sp

# Pick alive trees, of 10 mm or more
pick <- filter(census, status == "A", dbh >= 10)
# Pick sufficiently abundant trees
pick <- add_count(pick, sp)
pick <- filter(pick, n > 50)

species <- unique(pick$sp)

out <- tt_test_lst(census, species, habitat)
# Try also: View(out)
head(out)
#> [[1]]
#>        N.Hab.1 Gr.Hab.1 Ls.Hab.1 Eq.Hab.1 Rep.Agg.Neut.1 Obs.Quantile.1
#> CASARB      25     1489      109        2              0       0.930625
#>        N.Hab.2 Gr.Hab.2 Ls.Hab.2 Eq.Hab.2 Rep.Agg.Neut.2 Obs.Quantile.2
#> CASARB      12      168     1431        1              0          0.105
#>        N.Hab.3 Gr.Hab.3 Ls.Hab.3 Eq.Hab.3 Rep.Agg.Neut.3 Obs.Quantile.3
#> CASARB      14      567     1029        4              0       0.354375
#>        N.Hab.4 Gr.Hab.4 Ls.Hab.4 Eq.Hab.4 Rep.Agg.Neut.4 Obs.Quantile.4
#> CASARB      15      934      661        5              0        0.58375
#> attr(,"class")
#> [1] "tt"     "matrix"
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
#> attr(,"class")
#> [1] "tt"     "matrix"
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
#> attr(,"class")
#> [1] "tt"     "matrix"

tail(out)
#> [[1]]
#>        N.Hab.1 Gr.Hab.1 Ls.Hab.1 Eq.Hab.1 Rep.Agg.Neut.1 Obs.Quantile.1
#> CASARB      25     1489      109        2              0       0.930625
#>        N.Hab.2 Gr.Hab.2 Ls.Hab.2 Eq.Hab.2 Rep.Agg.Neut.2 Obs.Quantile.2
#> CASARB      12      168     1431        1              0          0.105
#>        N.Hab.3 Gr.Hab.3 Ls.Hab.3 Eq.Hab.3 Rep.Agg.Neut.3 Obs.Quantile.3
#> CASARB      14      567     1029        4              0       0.354375
#>        N.Hab.4 Gr.Hab.4 Ls.Hab.4 Eq.Hab.4 Rep.Agg.Neut.4 Obs.Quantile.4
#> CASARB      15      934      661        5              0        0.58375
#> attr(,"class")
#> [1] "tt"     "matrix"
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
#> attr(,"class")
#> [1] "tt"     "matrix"
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
#> attr(,"class")
#> [1] "tt"     "matrix"
```

## Krige soil data

Krige soil data with `krig()`, by Graham Zemunik.

### Compare parameters


```r
# Example data
soil <- soil_random
str(soil)
#> Classes 'tbl_df', 'tbl' and 'data.frame':	100 obs. of  3 variables:
#>  $ gx  : int  9 9 29 29 29 49 69 69 69 89 ...
#>  $ gy  : int  110 270 130 290 370 390 90 130 330 190 ...
#>  $ m3al: num  927 716 809 1115 419 ...
```

Krige with automated parameters.


```r
auto <- krig(soil, var = "m3al")
#> 
#> var: m3alUsing: gridsize = 20
#> Gessing: plotdim = c(1000, 500)
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


```r
summary(auto)
#> df
#> 'data.frame':	1250 obs. of  3 variables:
#>  $ x: num  10 30 50 70 90 110 130 150 170 190 ...
#>  $ y: num  10 10 10 10 10 10 10 10 10 10 ...
#>  $ z: num  950 944 939 933 928 ...
#> 
#> df.poly
#> 'data.frame':	1250 obs. of  3 variables:
#>  $ gx: num  10 30 50 70 90 110 130 150 170 190 ...
#>  $ gy: num  10 10 10 10 10 10 10 10 10 10 ...
#>  $ z : num  949 943 938 933 927 ...
#> 
#> lambda
#> 'numeric'
#>  num 1
#> 
#> vg
#> 'variogram'
#> List of 20
#>  $ u               : num [1:12] 30.3 42.9 51.1 60.9 86.5 ...
#>  $ v               : num [1:12] 60522 54239 174166 55226 44193 ...
#>  $ n               : num [1:12] 21 25 5 81 94 57 155 167 247 288 ...
#>  $ sd              : num [1:12] 96683 58618 214627 60326 52526 ...
#>  $ bins.lim        : num [1:31] 1.00e-12 2.00 2.38 2.84 3.38 ...
#>  $ ind.bin         : logi [1:30] FALSE FALSE FALSE FALSE FALSE FALSE ...
#>  $ var.mark        : num 55039
#>  $ beta.ols        : num 2.62e-08
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
#>  $ nugget               : num 56634
#>  $ cov.pars             : num [1:2] 87783 2105
#>  $ cov.model            : chr "gaussian"
#>  $ kappa                : num 0.5
#>  $ value                : num 1.49e+11
#>  $ trend                : chr "cte"
#>  $ beta.ols             : num 2.62e-08
#>  $ practicalRange       : num 3644
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

Try also:

```R
View(auto)
```

<img src="https://i.imgur.com/fTDjckj.png" align="center" height=150 />


```r
str(auto$df, give.attr = FALSE)
#> 'data.frame':	1250 obs. of  3 variables:
#>  $ x: num  10 30 50 70 90 110 130 150 170 190 ...
#>  $ y: num  10 10 10 10 10 10 10 10 10 10 ...
#>  $ z: num  950 944 939 933 928 ...
```

With custom parameters (arbitrary but based on automated kriging parameters).


```r
params <- list(
  model = "circular", range = 100, nugget = 1000, sill = 46000, kappa = 0.5
)
# Quietly
custom <- krig(soil, var = "m3al", params = params, quiet = TRUE)
```


```r
str(custom$df, give.attr = FALSE)
#> 'data.frame':	1250 obs. of  3 variables:
#>  $ x: num  10 30 50 70 90 110 130 150 170 190 ...
#>  $ y: num  10 10 10 10 10 10 10 10 10 10 ...
#>  $ z: num  933 955 984 968 919 ...
```

Compare.


```r
title_auto <- "Automated parameters"
ggplot(auto$df, aes(x = x, y = y, fill = z)) +
  geom_tile() + 
  coord_equal() +
  labs(title = title_auto)

title_custom <- "Custom parameters"
ggplot(custom$df, aes(x = x, y = y, fill = z)) +
  geom_tile() + 
  coord_equal() +
  labs(title = title_custom)
```

<img src="fgeo.habitat_files/figure-html/unnamed-chunk-9-1.png" width="45%" /><img src="fgeo.habitat_files/figure-html/unnamed-chunk-9-2.png" width="45%" />

### Iterate over multiple soil variables


```r
soil <- soil_fake
soil
#> # A tibble: 30 x 5
#>       gx    gy    mg     c     p
#>    <int> <int> <dbl> <dbl> <dbl>
#>  1    40   193  0.67  1.75  6.5 
#>  2    56    30  0.5   2.25  5.90
#>  3    61   102  0.65  2.05  6.40
#>  4    67   110  0.5   2.35  6.40
#>  5   113    16  0.56  1.45  6.20
#>  6   173   134  0.74  1.55  7   
#>  7   239   252  0.47  0.85  5.90
#>  8   257   442  0.52  2.25  6.20
#>  9   283   288  0.45  0.45  6.7 
#> 10   294   181  0.7   2.45  7.1 
#> # ... with 20 more rows
```

Scale soil data to make their values fall within the same range. In the following section, this will ease visual comparison.


```r
soil_vars <- c("mg", "c", "p")
soil_fake[soil_vars] <- map_df(soil_fake[soil_vars], scale)
soil_fake
#> # A tibble: 30 x 5
#>       gx    gy     mg      c       p
#>    <int> <int>  <dbl>  <dbl>   <dbl>
#>  1    40   193  0.720  0.177  0.613 
#>  2    56    30 -0.876  1.03  -0.479 
#>  3    61   102  0.532  0.691  0.431 
#>  4    67   110 -0.876  1.21   0.431 
#>  5   113    16 -0.313 -0.337  0.0668
#>  6   173   134  1.38  -0.166  1.52  
#>  7   239   252 -1.16  -1.37  -0.479 
#>  8   257   442 -0.689  1.03   0.0668
#>  9   283   288 -1.35  -2.05   0.977 
#> 10   294   181  1.00   1.38   1.71  
#> # ... with 20 more rows
```


```r
# Compared to krig(), notice that the order of first and second arguments is
# reversed. This helps you to first find soil variables and then feed them to
# krig_lst().
vars <- c("c", "p")
out_lst <- krig_lst(soil_fake, vars, quiet = TRUE)
out_df <- as_df(out_lst, name = "soil_var", item = "df")
head(out_df)
#>   soil_var   x  y         z
#> 1        c  10 10 0.8365533
#> 2        c  30 10 0.8107645
#> 3        c  50 10 0.7849500
#> 4        c  70 10 0.7591098
#> 5        c  90 10 0.7332437
#> 6        c 110 10 0.7073519

tail(out_df)
#>      soil_var   x   y          z
#> 2295        p 890 450 -0.5976629
#> 2296        p 910 450 -0.6128642
#> 2297        p 930 450 -0.6264792
#> 2298        p 950 450 -0.6385080
#> 2299        p 970 450 -0.6489505
#> 2300        p 990 450 -0.6578067
```

### Visualize multiple soil variables

The structure of this dataframe allows faceting with __gglot2__.


```r
ggplot(out_df, aes(x, y, fill = z)) +
  geom_tile() +
  facet_wrap("soil_var") +
  coord_equal()
```

<img src="fgeo.habitat_files/figure-html/unnamed-chunk-13-1.png" width="90%" style="display: block; margin: auto;" />

