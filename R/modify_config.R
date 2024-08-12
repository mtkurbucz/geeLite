# Main Function ----------------------------------------------------------------

#' Modify Configuration File
#'
#' Modifies the configuration file in the specified directory of the generated
#' database (\code{config/config.json}) by updating values associated with the
#' specified keys.
#' @param path [mandatory] (character) Path to the root directory of the
#' generated database.
#' @param keys [mandatory] (list) Specifying the path to values in the
#' configuration file that need updating.
#' @param new_values [mandatory] (list) New values to replace the original
#' values specified by 'keys'.
#' @param verbose [optional] (logical) Display messages (default: \code{TRUE}).
#' @export
#' @examples
#' # Example: Modifying the configuration file
#' \dontrun{
#'   modify_config(
#'     path = "path/to/root/directory",
#'     keys = list("limit", c("source", "MODIS/061/MOD13A2", "NDVI")),
#'     new_values = list(1000, "mean")
#'   )
#' }
#' @importFrom cli cli_alert_info
#' @importFrom purrr map modify_in
#' @importFrom jsonlite fromJSON write_json
#'
modify_config <- function(path, keys, new_values, verbose = TRUE) {

  # Validate parameters
  params <- list(path = path, file_path = "config/config.json", keys = keys,
                 new_values = new_values, verbose = verbose)
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

  # Output information if 'verbose' is TRUE
  output_message(list("Config file updated: 'config/config.json'."), verbose)

}
