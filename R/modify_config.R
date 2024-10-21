# Main Function ----------------------------------------------------------------

#' Modify Configuration File
#'
#' Modifies the configuration file located in the specified root directory of
#' the generated database (\code{config/config.json}) by updating values
#' corresponding to the specified keys.
#' @param path [mandatory] (character) The path to the root directory of the
#'   generated database.
#' @param keys [mandatory] (list) A list specifying the path to the values in
#'   the configuration file that need updating. Each path should correspond to
#'   a specific element in the configuration.
#' @param new_values [mandatory] (list) A list of new values to replace the
#'   original values at the locations specified by 'keys'. The length of
#'   \code{new_values} must match the length of \code{keys}.
#' @param verbose [optional] (logical) If \code{TRUE}, displays messages about
#'   the updates made (default: \code{TRUE}).
#' @export
#' @examples
#' # Example: Modifying the configuration file
#' \dontrun{
#'   modify_config(
#'     path = "path/to/db",
#'     keys = list("limit", c("source", "MODIS/061/MOD13A2", "NDVI")),
#'     new_values = list(1000, "mean")
#'   )
#' }
#' @importFrom cli cli_alert_info
#' @importFrom purrr map modify_in
#' @importFrom jsonlite fromJSON write_json
#'
modify_config <- function(path, keys, new_values, verbose = TRUE) {

  # Validate input parameters
  params <- list(path = path, file_path = "config/config.json", keys = keys,
                 new_values = new_values, verbose = verbose)
  validate_params(params)

  # Read the configuration file
  config_path <- file.path(path, "config/config.json")
  config <- fromJSON(config_path)

  # Modify the configuration settings based on the provided keys and new values
  for (i in seq_along(keys)) {
    config <- modify_in(config, keys[[i]], ~ new_values[[i]])
  }

  # Write the updated configuration back to the file
  write_json(config, config_path, pretty = TRUE)

  # Display an update message if 'verbose' is TRUE
  output_message(list("Config file updated: 'config/config.json'."), verbose)

}
