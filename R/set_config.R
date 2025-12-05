# Main Function ----------------------------------------------------------------

#' Initialize the Configuration File
#'
#' Creates a configuration file in the specified directory of the generated
#' database (\code{config/config.json}). If the specified directory does not
#' exist but its parent directory does, it will be created.
#' @param path [mandatory] (character) The path to the root directory of the
#'   generated database. This must be a writable, non-temporary directory.
#'   Avoid using the home directory (~), the current working directory, or the
#'   package directory.
#' @param regions [mandatory] (character) ISO 3166-1 alpha-2 country codes or
#'   ISO 3166-2 subdivision codes.
#' @param source [mandatory] (list) Description of Google Earth Engine (GEE)
#'   datasets of interest (the complete data catalog of GEE is accessible at:
#'   \url{https://developers.google.com/earth-engine/datasets/catalog}). It is
#'   a nested list with three levels:
#' \describe{
#'   \item{names}{(list) Datasets of interest (e.g.,
#'   \code{"MODIS/061/MOD13A1"}).
#'     \describe{
#'       \item{bands}{(list) Bands of interest (e.g., \code{"NDVI"}).
#'         \describe{
#'           \item{zonal_stats}{(character) Statistics of interest
#'           (options: \code{"mean"}, \code{"sum"}, \code{"median"},
#'           \code{"min"}, \code{"max"}, \code{"sd"}).}
#'         }
#'       }
#'     }
#'   }
#' }
#' @param resol [mandatory] (integer) Resolution of the H3 bin.
#' @param scale [optional] (integer) Specifies the nominal resolution
#'   (in meters) for image processing. If left as \code{NULL} (the default), a
#'   resolution of 1000 is used.
#' @param start [optional] (date) First date of the data collection
#'   (default: \code{"2020-01-01"}).
#' @param limit [optional] (integer) In \code{"local"} mode, `limit / dates`
#'   sets batch size; in \code{"drive"} mode, `limit` is the max features per
#'   export (default: \code{10000}).
#' @param verbose [optional] (logical) Display messages (default: \code{TRUE}).
#' @return No return value, called for side effects.
#' @export
#' @examples
#' \dontrun{
#'   set_config(path = tempdir(),
#'              regions = c("SO", "YM"),
#'              source = list(
#'               "MODIS/061/MOD13A1" = list(
#'                 "NDVI" = c("mean", "sd")
#'              )
#'             ),
#'             resol = 3)
#' }
#' @importFrom cli cli_alert_info
#' @importFrom jsonlite write_json
#'
set_config <- function(path, regions, source, start = "2020-01-01", resol,
                       scale = NULL, limit = 10000, verbose = TRUE) {

  # Convert to absolute path and check existence
  path <- normalizePath(path, mustWork = FALSE)

  # Validate all parameters except 'path'
  params <- list(regions = regions, source = source, start = start,
                 resol = resol, scale = scale, limit = limit,
                 verbose = verbose)
  validate_params(params)

  # Extract the parent directory from the given 'path'
  parent_dir <- dirname(path)

  # Check if the parent directory exists
  if (!dir.exists(parent_dir)) {
    stop("The parent directory is not accessible: ", parent_dir)
  }

  # Check if the path itself exists
  if (!dir.exists(path)) {
    # Create the directory if it does not exist
    dir.create(path, recursive = TRUE)
  }

  # Create configuration list
  config <- params[!names(params) %in% c("path", "verbose")]

  # Create 'config' directory if it doesn't exist
  dir.create(file.path(path, "config"), showWarnings = FALSE)

  # Write the configuration list to a JSON file
  write_json(config, file.path(path, "config/config.json"), pretty = TRUE)

  # Display a message if 'verbose' is TRUE
  output_message("Config file generated: 'config/config.json'.", verbose)

}
