#' @title Setting Up the Configuration File
#'
#' @description This function creates the configuration file
#' (\code{path/config/config.json}).
#'
#' @param path [mandatory] (character) Path to the root directory of the
#' generated database.
#'
#' @param regions [mandatory] (character) ISO 3166-2 codes of the regions of
#' interest (two letters for countries and additional characters for states).
#'
#' @param source [mandatory] (list) Description of GEE datasets of interest. It
#' is a nested list with three levels:
#' \describe{
#'   \item{names}{(list) Names of the GEE datasets of interest (e.g.,
#'   \code{"MODIS/061/MOD13A1"}).
#'     \describe{
#'       \item{bands}{(list) Bands of interest (e.g., \code{"NDVI"}).
#'         \describe{
#'           \item{stats}{(character) Statistics of interest
#'           (options: \code{"mean"}, \code{"median"}, \code{"min"},
#'           \code{"max"}, \code{"sd"}).}
#'         }
#'       }
#'     }
#'   }
#' }
#'
#' @param resol [mandatory] (integer) Resolution of the H3 bin.
#'
#' @param scale [optional] (integer) Scale of images before processing
#' (default: \code{NULL}).
#'
#' @param start [optional] (date) First date of the data collection
#' (default: \code{"2000-01-01"}).
#'
#' @param limit [optional] (integer) Limit on concurrent zonal statistics
#' calculations. \code{Limit - 1} bins will be processed at the same time
#' (default: \code{10000}).
#'
#' @param crs [optional] (integer) CRS to be assigned to the dataset (default
#' is the shapefile's CRS: \code{NULL}).
#'
#' @param verbose [optional] (logical) Display computation status and messages
#' (default: \code{TRUE}).
#'
#' @export
#'
#' @examples
#' # Example: Setting up the configuration file
#' \dontrun{
#' set_config(path = "path",
#'            regions = c("SO", "YM"),
#'            source = list(
#'             "MODIS/006/MOD13A2" = list(
#'               "NDVI" = c("mean", "sd")
#'             )
#'            ),
#'            start = "2010-01-01",
#'            resol = 3)
#' }
#'
#' @importFrom cli cli_alert_info
#' @importFrom jsonlite write_json
#'
set_config <- function(path, regions, source, start = "2000-01-01", resol,
                       scale = NULL, limit = 10000, crs = NULL,
                       verbose = TRUE) {

  config <- list(
    regions = regions,
    source = source,
    start = start,
    resol = resol,
    scale = scale,
    limit = limit,
    crs = crs
  )

  dir.create(file.path(path, "config"), showWarnings = FALSE)
  write_json(config, file.path(path, "config", "config.json"), pretty = TRUE)
  if (verbose) {
    cat("\n")
    cli_alert_info("Config file generated: 'config/config.json'.")
    cat("\n")
  }
}
