% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_geelite.R
\name{extract_drive_stats}
\alias{extract_drive_stats}
\title{Extract Large-Scale Statistics in Drive Mode with Fewer Tasks}
\usage{
extract_drive_stats(
  sf_chunks,
  imgs,
  band,
  stat,
  stat_fun,
  scale,
  folder = ".geelite_tmp_drive",
  user = NULL,
  pb,
  pb_step
)
}
\arguments{
\item{sf_chunks}{[mandatory] (list) A list of sf data frames representing
geometry chunks.}

\item{imgs}{[mandatory] (ee$ImageCollection) The Earth Engine image
collection to extract statistics from.}

\item{band}{[mandatory] (character) The band name (e.g., "NDVI").}

\item{stat}{[mandatory] (character) The statistical function to apply
(e.g., "mean").}

\item{stat_fun}{[mandatory] (ee$Reducer) The Earth Engine reducer function.}

\item{scale}{[mandatory] (numeric) The spatial resolution in meters for
reduceRegions.}

\item{folder}{[optional] (character) Name of the Google Drive folder where
exports will be stored. Defaults to \code{".geelite_tmp_drive"}.}

\item{user}{[optional] (character) GEE user profile name, if applicable.}

\item{pb}{[mandatory] (Progress bar object) A progress bar instance from
\code{progress::progress_bar} or similar package. Used to track task
progress.}

\item{pb_step}{[mandatory] (numeric) The step size for updating the
progress bar.}
}
\value{
(data.frame) A merged data frame containing extracted statistics
  from all Drive exports.
}
\description{
Batches multiple geometry chunks into fewer \code{ee_table_to_drive} tasks,
reducing overhead and leveraging Google Earth Engine's parallel processing.
}
\keyword{internal}
