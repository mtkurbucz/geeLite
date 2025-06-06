% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_json.R
\name{get_json}
\alias{get_json}
\title{Print JSON File}
\usage{
get_json(path, file_path)
}
\arguments{
\item{path}{[mandatory] (character) The path to the root directory of the
generated database.}

\item{file_path}{[mandatory] (character) The relative path to the JSON file
from the root directory.}
}
\value{
A character string representing the formatted JSON content of the
  specified file.
}
\description{
Reads and prints a specified JSON file from the provided root directory in a
human-readable format.
}
\keyword{internal}
