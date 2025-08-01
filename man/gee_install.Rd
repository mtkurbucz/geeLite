% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gee_install.R
\name{gee_install}
\alias{gee_install}
\title{Install and Configure a Conda Environment for 'rgee'}
\usage{
gee_install(conda = "rgee", python_version = "3.10", force_recreate = FALSE)
}
\arguments{
\item{conda}{[optional] (character) Name of the Conda environment to create
or use. Defaults to \code{"rgee"}.}

\item{python_version}{[optional] (character) Python version to use when
creating the Conda environment. Defaults to \code{"3.10"}.}

\item{force_recreate}{[optional] (logical) If \code{TRUE}, deletes and
recreates the Conda environment even if it already exists. Defaults to
\code{FALSE}.}
}
\value{
Invisibly returns the name of the Conda environment used or created.
}
\description{
Sets up a Conda environment with all required Python and R dependencies
for using the \code{rgee} package, including a specific version of the
\code{earthengine-api}. If Conda is not available, the user will be prompted
to install Miniconda. The created environment is automatically registered
for use with \code{rgee}.
}
\note{
Even after installation, users must manually accept the Conda Terms of
Service (ToS) using the `conda tos accept` command before package
installation can proceed. Clear instructions will be provided if ToS
acceptance is needed.
}
\examples{
# Example: Creating a Conda environment with 'rgee' dependencies
\dontrun{
  gee_install()
}
}
