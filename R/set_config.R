# Main Function ----------------------------------------------------------------

#' Initialize the Configuration File
#'
#' Creates the configuration file in the specified directory of the generated
#' database (\code{config/config.json}).
#' @param path [mandatory] (character) The path to the root directory of the
#' generated database.
#' @param regions [mandatory] (character) ISO 3166-2 codes of the regions of
#' interest (two letters for countries and additional characters for states).
#' @param source [mandatory] (list) Description of Google Earth Engine (GEE)
#' datasets of interest. It is a nested list with three levels:
#' \describe{
#'   \item{names}{(list) Datasets of interest (e.g.,
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
#' @param resol [mandatory] (integer) Resolution of the H3 bin.
#' @param scale [optional] (integer) Scale of images before processing
#' (default: \code{NULL}).
#' @param start [optional] (date) First date of the data collection
#' (default: \code{"2010-01-01"}).
#' @param limit [optional] (integer) Limit on concurrent zonal statistics
#' calculations. \code{Limit - 1} bins will be processed at the same time
#' (default: \code{10000}).
#' @param crs [optional] (integer) CRS to be assigned to the dataset (default
#' is the shapefile's original CRS: \code{NULL}). Note: Reprojection is not
#' recommended unless essential.
#' @param verbose [optional] (logical) Display messages (default: \code{TRUE}).
#' @export
#' @examples
#' # Example: Setting up the configuration file
#' \dontrun{
#'   set_config(path = "path/to/root/directory",
#'              regions = c("SO", "YM"),
#'              source = list(
#'               "MODIS/006/MOD13A2" = list(
#'                 "NDVI" = c("mean", "sd")
#'              )
#'             ),
#'             resol = 3)
#' }
#' @importFrom cli cli_alert_info
#' @importFrom jsonlite write_json
#'
set_config <- function(path, regions, source, start = "2010-01-01", resol,
                       scale = NULL, limit = 10000, crs = NULL,
                       verbose = TRUE) {

  # Validate parameters
  params <- list(path = path, regions = regions, source = source, start = start,
                 resol = resol, scale = scale, limit = limit, crs = crs,
                 verbose = verbose)
  validate_params(params)

  # Create configuration list
  config <- params[!names(params) %in% c("path", "verbose")]

  # Create 'config' directory if it doesn't exist
  dir.create(file.path(path, "config"), showWarnings = FALSE)

  # Write configuration list to JSON file
  write_json(config, file.path(path, "config/config.json"), pretty = TRUE)

  # Output information if 'verbose' is TRUE
  output_message("Config file generated: 'config/config.json'.", verbose)

}
