# Main Functions ---------------------------------------------------------------

#' @title Modify Configuration File
#'
#' @description Modifies the configuration file at the specified path by
#' updating values associated with specified keys.
#'
#' @param path [mandatory] (character) Path to the root directory of the
#' generated database.
#' @param keys [mandatory] (list) List specifying the path to values in the
#' configuration file that need updating.
#' @param new_values [mandatory] (list) List of new values to replace the
#' original values specified by 'keys'.
#' @param verbose [optional] (logical) Display messages (default: \code{TRUE}).
#' @export
#' @examples
#' # Example: Modify configuration file
#' \dontrun{
#' modify_config(path = "path/to/root/directory",
#'               keys = list("limit", c("source", "MODIS/006/MOD13A2", "NDVI")),
#'               new_values = list(1000, "mean"))
#' }
#' @importFrom purrr map
#' @importFrom cli cli_alert_info
#' @importFrom jsonlite fromJSON write_json
#'
modify_config <- function(path, keys, new_values, verbose = TRUE) {

  # Validate 'path' parameter
  check_directory_validity(path)
  config_path <- file.path(path, "config/config.json")
  check_file_validity(config_path)

  # Validate 'keys' parameter
  if (!is.list(keys) || length(keys) == 0) {
    stop("'keys' must be a non-empty list.")
  }

  valid_keys <- c("regions", "source", "limit")
  invalid_keys <- setdiff(as.character(map(keys, 1)), valid_keys)
  if(length(invalid_keys) > 0) {
    stop(sprintf("Invalid keys specified: %s", invalid_keys))
  }

  # Validate 'new_values' parameter
  if (!is.list(new_values) || length(new_values) != length(keys)) {
    stop("'new_values' must be a list with the same length as 'keys'.")
  }

  # Read configuration file
  config <- fromJSON(config_path)

  # Modify configuration settings
  for (i in seq_along(keys)) {
    config <- modify_in(config, keys[[i]], ~ new_values[[i]])
  }

  # Write updated configuration to file
  write_json(config, config_path, pretty = TRUE)

  # Output information if verbose mode is enabled
  if (verbose) {
    cat("\n")
    cli_alert_info("Config file updated: 'config/config.json'.")
    cat("\n")
  }

}
