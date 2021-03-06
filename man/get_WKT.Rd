% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_WKT.R
\name{get_WKT}
\alias{get_WKT}
\title{Get WKT (Well Known Text) of \link[sf]{sf} object}
\usage{
get_WKT(sf_obj)
}
\arguments{
\item{sf_obj}{object of class \link[sf]{sf}}
}
\value{
character scalar or string of WKT text depending on number
  of features in \code{sf_obj}
}
\description{
Not well-tested beyond POLYGON and MULTIPOLYGON objects.
}
\examples{
nc = sf::st_read(system.file("shape/nc.shp", package="sf"))
get_WKT(nc[22, ])
}
