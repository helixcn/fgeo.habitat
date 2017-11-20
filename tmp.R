op <- options(warn = 2)
options("warn")

library(krig)
df <- krig::soil_random
GetKrigedSoil( df, var="M3Al" )

options(op)



# xxxcont

* check with other `m0` arguments.
* confirm regression tests.
* close issue
