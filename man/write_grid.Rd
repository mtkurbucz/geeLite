% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_geelite.R
\name{write_grid}
\alias{write_grid}
\title{Write Grid to Database}
\usage{
write_grid(grid)
}
\arguments{
\item{grid}{[mandatory] (sf) Simple features object containing the grid data
to be written into the database.}
}
\description{
Writes the H3 grid to the specified SQLite database (\code{data/geelite.db}).
}
\keyword{internal}
