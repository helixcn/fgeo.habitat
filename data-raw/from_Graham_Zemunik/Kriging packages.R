# Script to load in the required packages for the soil kriging functionality
#
# This script handles installing packages that are used and that may not have been
# installed.

# I use a modified version of a function from stackoverflow

InstallPackages <- function( requiredPackages )
{
  remaining <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]
  
  if ( length( remaining ) ) {
    install.packages( remaining, dependencies=T )
  }
}

# These are the packages that are needed
requiredPackages = c( "geoR", "MASS" )

# Install the packages if need be
InstallPackages( requiredPackages )
