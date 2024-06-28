#' @title Print the Configuration File
#'
#' @description This function reads and prints the configuration file in a
#' human-readable JSON format from the specified root directory of the generated database.
#'
#' @param path [mandatory] (character) Path to the root directory of the generated database.
#' Must be a valid directory path.
#'
#' @return A character string representing the formatted configuration JSON.
#'
#' @export
#'
#' @examples
#' # Example: Printing the configuration file
#' \dontrun{
#'   config <- get_config(path = "path/to/root/directory")
#'   cat(config)
#' }
#'
#' @importFrom jsonlite fromJSON toJSON
#'
get_config <- function(path) {
  
  # Validate 'path' parameter
  if (!is.character(path) || length(path) != 1 || path == "") {
    stop("'path' must be a non-empty character string.")
  }
  
  config_file <- file.path(path, "config", "config.json")
  
  # Check if the config file exists
  if (!file.exists(config_file)) {
    stop(sprintf("Config file does not exist at path: %s", config_file))
  }
  
  # Read and format the JSON configuration
  config <- jsonlite::fromJSON(config_file)
  formatted_config <- jsonlite::toJSON(config, pretty = TRUE)
  
  return(formatted_config)
}

