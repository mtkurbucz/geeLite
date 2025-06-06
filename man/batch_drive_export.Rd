% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_geelite.R
\name{batch_drive_export}
\alias{batch_drive_export}
\title{Perform a Single Drive Export for Multiple Geometry Chunks}
\usage{
batch_drive_export(
  sf_list,
  imgs,
  stat_fun,
  band,
  stat,
  scale,
  folder = ".geelite_tmp_drive",
  user = NULL,
  description = "geelite_export",
  verbose = FALSE
)
}
\arguments{
\item{sf_list}{[mandatory] (list) A list of sf data.frames representing
geometry chunks to be processed.}

\item{imgs}{[mandatory] (ee$ImageCollection) The Earth Engine image
collection to extract statistics from.}

\item{stat_fun}{[mandatory] (ee$Reducer) The reducer function to apply
to extract statistics.}

\item{band}{[mandatory] (character) The band name from the image collection
(e.g., "NDVI").}

\item{stat}{[mandatory] (character) The statistical function name (e.g.,
"mean").}

\item{scale}{[mandatory] (numeric) The spatial resolution in meters for
'reduceRegions'.}

\item{folder}{[optional] (character) Name of the Google Drive folder
where the export will be stored. Default is \code{".geelite_tmp_drive"}.}

\item{user}{[optional] (character) If multiple rgee user profiles exist,
specify the user profile directory.}

\item{description}{[optional] (character) A custom description for the
export task. Default is \code{"geelite_export"}.}

\item{verbose}{[optional] (logical) If \code{TRUE}, progress messages will
be printed. Defaults to \code{FALSE}.}
}
\value{
(data.frame) A data frame containing extracted statistics with
  columns \code{id}, \code{band}, \code{zonal_stat}, and date-based values.
}
\description{
Exports multiple geometry chunks to Google Drive in a single batch task.
The function processes spatial data using Google Earth Engine (GEE) and
exports results in CSV format.
}
\keyword{internal}
