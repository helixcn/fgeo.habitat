Torus translation test
================
Mauro Lepore
2018-04-17

This article shows how to use the function `torusonesp.all()`, by
Sabrina Russo, Daniel Zuleta, Matteo Detto. For more information see
`?torusonesp.all()`. Although the core of this function will remain
largely the same, the interface to this function is in development and
you can expect it to change rapidly.

Setup.

``` r
# Install the development branch 23_ttt of the package fgeo.habitat
# install.packages("remotes")
remotes::install_github("forestgeo/fgeo.habitat@23_ttt")
```

For details on how to install packages from GitHub, see [this
article](https://goo.gl/dQKEeg).

``` r
library(fgeo.habitat)
```

Example dataset from Pasoh (you should use your own data).

``` r
census <- pasoh::pasoh_3spp
str(census)
#> 'data.frame':    26286 obs. of  21 variables:
#>  $ treeID   : int  10 11 65 154 158 214 228 231 238 261 ...
#>  $ stemID   : int  10 11 65 154 158 214 228 231 238 261 ...
#>  $ tag      : chr  "11" "12" "67" "157" ...
#>  $ StemTag  : chr  "" "" "" "" ...
#>  $ sp       : chr  "XERONO" "ANAXJA" "GIROPA" "GIROPA" ...
#>  $ quadrat  : chr  "0000" "0000" "0000" "0000" ...
#>  $ gx       : num  0.9 1.3 3.2 14.7 14.6 ...
#>  $ gy       : num  3.5 3.6 16.3 2.6 0.5 ...
#>  $ DBHID    : int  46 51 321 766 786 1066 1136 1151 1186 1301 ...
#>  $ CensusID : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ dbh      : num  10 10 25 10 10 10 79 15 35 20 ...
#>  $ pom      : chr  "1.3" "1.3" "1.3" "1.3" ...
#>  $ hom      : num  1.3 1.3 1.3 1.3 1.3 ...
#>  $ ExactDate: chr  "1986-02-04" "1986-02-04" "1986-02-04" "1986-02-04" ...
#>  $ DFstatus : chr  "alive" "alive" "alive" "alive" ...
#>  $ codes    : chr  NA NA NA NA ...
#>  $ nostems  : num  1 1 1 1 1 1 1 1 1 1 ...
#>  $ date     : num  9531 9531 9531 9531 9531 ...
#>  $ status   : chr  "A" "A" "A" "A" ...
#>  $ agb      : num  0.000172 0.000118 0.000858 0.000103 0.000118 ...
#>  $ index5   : num  1 1 4 201 201 304 303 303 303 301 ...

habitat <- pasoh::pasoh_hab_index20
head(habitat)
#>    x   y habitats index5 index20
#> 1  0   0        2      1       1
#> 5  0  20        2      5       2
#> 9  0  40        2      9       3
#> 13 0  60        2     13       4
#> 17 0  80        2     17       5
#> 21 0 100        2     21       6
```

To use YOUR OWN DATA, you may run something like this:

    load("<PATH>/<CENSUS_DATA>.rdata")
    census_data <- <CENSUS_DATA>
    
    load("<PATH>/<HABITAT_DATA>.rdata")
    habitat_data <- <HABITAT_DATA>

Abundance per quadrat.

``` r
abundance_per_quadrat <- abundanceperquad(
  census,
  plotdim = c(1000, 500),
  gridsize = 20,
  type = 'abund'
)$abund

abundance_per_quadrat[1:10]
#>        1  2  3  4  5  6  7  8  9 10
#> XERONO 5 11  8 10  6  8  6 21 15 22
#> ANAXJA 2 14 25 34 25 16 15 30 13  1
#> GIROPA 3  5  2  1  1  3  4  7  6  9
dim(abundance_per_quadrat)
#> [1]    3 1250
```

Torus translation for one species.

``` r
result_one <- torusonesp.all(
  species = "GIROPA",
  hab.index20 = habitat,
  allabund20 = abundance_per_quadrat,
  plotdim = c(1000, 500),
  gridsize = 20
)
result_one
#>        N.Hab.1 Gr.Hab.1 Ls.Hab.1 Eq.Hab.1 Rep.Agg.Neut.1 Obs.Quantile.1
#> GIROPA      78        0     4999        1             -1              0
#>        N.Hab.2 Gr.Hab.2 Ls.Hab.2 Eq.Hab.2 Rep.Agg.Neut.2 Obs.Quantile.2
#> GIROPA    1572       15     4984        1             -1          0.003
#>        N.Hab.3 Gr.Hab.3 Ls.Hab.3 Eq.Hab.3 Rep.Agg.Neut.3 Obs.Quantile.3
#> GIROPA    1085     4984       15        1              1         0.9968
#>        N.Hab.4 Gr.Hab.4 Ls.Hab.4 Eq.Hab.4 Rep.Agg.Neut.4 Obs.Quantile.4
#> GIROPA    1224     4999        0        1              1         0.9998
```

Iterate over all (or a subset of) species.

``` r
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
t(Reduce(rbind, result_all))
#>                   XERONO    ANAXJA    GIROPA
#> N.Hab.1        1197.0000  783.0000   78.0000
#> Gr.Hab.1       4994.0000 2964.0000    0.0000
#> Ls.Hab.1          5.0000 2035.0000 4999.0000
#> Eq.Hab.1          1.0000    1.0000    1.0000
#> Rep.Agg.Neut.1    1.0000    0.0000   -1.0000
#> Obs.Quantile.1    0.9988    0.5928    0.0000
#> N.Hab.2        5405.0000 5711.0000 1572.0000
#> Gr.Hab.2        383.0000 4995.0000   15.0000
#> Ls.Hab.2       4616.0000    4.0000 4984.0000
#> Eq.Hab.2          1.0000    1.0000    1.0000
#> Rep.Agg.Neut.2    0.0000    1.0000   -1.0000
#> Obs.Quantile.2    0.0766    0.9990    0.0030
#> N.Hab.3        1335.0000  366.0000 1085.0000
#> Gr.Hab.3       4064.0000   57.0000 4984.0000
#> Ls.Hab.3        935.0000 4942.0000   15.0000
#> Eq.Hab.3          1.0000    1.0000    1.0000
#> Rep.Agg.Neut.3    0.0000    0.0000    1.0000
#> Obs.Quantile.3    0.8128    0.0114    0.9968
#> N.Hab.4        1025.0000  217.0000 1224.0000
#> Gr.Hab.4       1027.0000  109.0000 4999.0000
#> Ls.Hab.4       3972.0000 4890.0000    0.0000
#> Eq.Hab.4          1.0000    1.0000    1.0000
#> Rep.Agg.Neut.4    0.0000    0.0000    1.0000
#> Obs.Quantile.4    0.2054    0.0218    0.9998
```
