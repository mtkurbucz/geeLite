#' @title Printing the Configuration File
#'
#' @description This function prints the configuration file.
#'
#' @param path [mandatory] (character) Path to the root directory of the
#' generated database.
#'
#' @return Configuration object.
#'
#' @export
#'
#' @examples
#' # Example: Printing the configuration file
#' \dontrun{
#'   get_config(path = "path")
#' }
#'
#' @importFrom jsonlite fromJSON toJSON
#'
get_config <- function(path) {

  config_file <- file.path(path, "config", "config.json")
  if (!file.exists(config_file)) {stop("Config file does not exist.")}
  config <- toJSON(fromJSON(config_file), pretty = TRUE)

  return(config)
}
