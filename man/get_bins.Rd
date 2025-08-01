% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_geelite.R
\name{get_bins}
\alias{get_bins}
\title{Get H3 Bins for Shapes}
\usage{
get_bins(shapes, resol)
}
\arguments{
\item{shapes}{[mandatory] (sf) A simple features object containing
geometries used for generating H3 bins.}

\item{resol}{[mandatory] (integer) An integer specifying the resolution of
the H3 grid.}
}
\value{
A data frame containing the H3 bins with columns for region ISO
  3166 codes, bin IDs, and geometry.
}
\description{
Generates H3 bins for the provided shapes at the specified resolution.
}
\keyword{internal}
