% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_geelite.R
\name{remove_tables}
\alias{remove_tables}
\title{Remove Tables from the Database}
\usage{
remove_tables(tables_drop)
}
\arguments{
\item{tables_drop}{[mandatory] (character) A character vector of tables to
be deleted.}
}
\description{
Removes tables from the database if their corresponding dataset is initially
marked for deletion ('-').
}
\keyword{internal}
