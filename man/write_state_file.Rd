% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_geelite.R
\name{write_state_file}
\alias{write_state_file}
\title{Write State File}
\usage{
write_state_file(task, regions, source_for_state)
}
\arguments{
\item{task}{[mandatory] (list) Session task specifying parameters for data
collection.}

\item{regions}{[mandatory] (character) A vector containing ISO 3166-2 region
codes. Country codes are two characters long, while state codes contain
additional characters.}

\item{source_for_state}{[mandatory] (list) A list containing information
regarding the collected data.}
}
\description{
Writes the state file to the specified directory within the generated
database (\code{state/state.json}).
}
\keyword{internal}
