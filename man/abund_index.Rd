% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/abund_index.R
\name{abund_index}
\alias{abund_index}
\title{Count rows by quadrat-index.}
\usage{
abund_index(censdata, plotdim, gridsize)
}
\arguments{
\item{censdata}{A table of plot census data.}

\item{plotdim}{The x and y dimensions of the plot.}

\item{gridsize}{Side of the square quadrat.}
}
\value{
A dataframe where each quadrat-index is a column and each species
is a rowname.
}
\description{
Count rows by quadrat-index. This is a conservative wrapper around the
function \code{abundanceperquad()} of the CTFSRPackage. Its output is always
abundance (not basal area nor agb) and includes all available rows. If you
want to exclude trees of some particular dbh range you need to do it before
using this function.
}
\details{
This function is softly deprecated. Better alternatives to count rows by
groups are available in \strong{dplyr} and \strong{janitor}(see \code{group_by()} and
\code{count()} in \strong{dplyr} and \code{tabyl()} in \strong{janitor}). Those alternatives are
better tested and considerably faster.
}
\examples{
cns <- luquillo_tree6_random
pdm <- c(1000, 500)
gsz <- 20
abund_index(cns, pdm, gsz)
}
\seealso{
\code{fgeo.tool::add_index()}.
}
