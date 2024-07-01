# Main Function ----------------------------------------------------------------

#' @title Modify Configuration File
#'
#' @description Modifies the configuration file at the specified path by
#' updating values associated with specified keys.
#'
#' @param path [mandatory] (character) Path to the root directory of the
#' generated database.
#'
#' @param keys [mandatory] (list) List specifying the path to values in the
#' configuration file that need updating.
#'
#' @param new_values [mandatory] (list) List of new values to replace the
#' original values specified by 'keys'.
#'
#' @param verbose [optional] (logical) Display messages (default: \code{TRUE}).
#'
#' @export
#'
#' @examples
#' # Example: Modify configuration file
#' \dontrun{
#'   modify_config(
#'     path = "path/to/root/directory",
#'     keys = list("limit", c("source", "MODIS/006/MOD13A2", "NDVI")),
#'     new_values = list(1000, "mean")
#'   )
#' }
#'
#' @importFrom purrr map
#' @importFrom cli cli_alert_info
#' @importFrom jsonlite fromJSON write_json
#'
modify_config <- function(path, keys, new_values, verbose = TRUE) {

  # Validate parameters
  call <- match.call()
  params <- generate_params(call)
  validate_params(params)

  # Read configuration file
  config_path <- file.path(path, "config/config.json")
  config <- fromJSON(config_path)

  # Modify configuration settings
  for (i in seq_along(keys)) {
    config <- modify_in(config, keys[[i]], ~ new_values[[i]])
  }

  # Write updated configuration to file
  write_json(config, config_path, pretty = TRUE)

  # Output information if verbose mode is enabled
  output_info("Config file updated: 'config/config.json'.", verbose)

}
