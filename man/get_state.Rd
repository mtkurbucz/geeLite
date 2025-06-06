% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_json.R
\name{get_state}
\alias{get_state}
\title{Print the State File}
\usage{
get_state(path)
}
\arguments{
\item{path}{[mandatory] (character) The path to the root directory of the
generated database.}
}
\value{
A character string representing the formatted JSON content of the
  state file.
}
\description{
Reads and prints the state file from the database's root directory in a
human-readable format.
}
\examples{
# Example: Printing the state file
\dontrun{
  get_state(path = "path/to/db")
}

}
