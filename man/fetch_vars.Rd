% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/access_db.R
\name{fetch_vars}
\alias{fetch_vars}
\title{Fetch Variable Information from an SQLite Database}
\usage{
fetch_vars(
  path,
  format = c("data.frame", "markdown", "latex", "html", "pipe", "simple", "rst")
)
}
\arguments{
\item{path}{[mandatory] (character) Path to the root directory of the
generated database.}

\item{format}{[mandatory] (character) A character string. Possible values
are \code{"data.frame"} (default) to return a \code{data.frame} object,
or one of \code{"markdown"}, \code{"latex"}, \code{"html"}, \code{"pipe"}
(Pandoc's pipe tables), \code{"simple"} (Pandoc's simple tables), and
\code{"rst"} to be passed on to knitr for formatting.}
}
\value{
Returns the variable information in the selected format. If
  \code{format = "data.frame"}, a \code{data.frame} is returned. For other
  formats, the output is printed in the specified format and \code{NULL} is
}
\description{
Displays information on the available variables in the SQLite database
(\code{data/geelite.db}).
}
\examples{
# Example: Printing the available variables
\dontrun{
  fetch_vars(path = "path/to/db")
}
}
