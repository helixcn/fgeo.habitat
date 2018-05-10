# fgeo.habitat 0.0.0.9005

This release removes non essential components to reduce complexity.

* Remove class tt and krig.
* Remove krig_lst() and tt_test_lst().
* Remove as_*().

# fgeo.habitat 0.0.0.9004

* Add a `summary()` method for class "krig".
* Add generic `to_df()` for classes "tt", "tt_lst" and "krig_lst".
* New generic to coerce results of `GetKrigedSoil()` to "krig" and "krig_lst".
* New generic to coerce results of `torusonesp.all()` to "tt" and "tt_lst".
* Split vignette fgeo.habitat.Rmd into tt_test.Rmd and krig.Rmd.

# fgeo.habitat 0.0.0.9003

* Improve performance of tt_test() and abund_index().

# fgeo.habitat 0.0.0.9002

* New function `tt_test()` and friends, by Sabrina Russo et al.
* Rename to __fgeo.habitat__

# soilkrig 0.0.0.9000

* Update website for consistency with other packages of __fgeo__
* Added a `NEWS.md` file to track changes to the package.
