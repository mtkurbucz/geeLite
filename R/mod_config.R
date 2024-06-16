#' @title Modifying Configuration File
#'
#' @description This function modifies the configuration file.
#'
#' @param path [mandatory] (character) Path to the root directory of the
#' generated database.
#'
#' @param target [mandatory] (list) Specifies the path to the values to be
#' replaced.
#'
#' @param values [mandatory] (list) New values to replace the original ones.
#'
#' @export
#'
#' @examples
#' # Example: Modify configuration file
#' \dontrun{
#' mod_config(path = "/path/to/custom/directory",
#'            list("limit", c("source", "MODIS/006/MOD13A2", "NDVI")),
#             list(1000, "mean")
#' }
#'
#' @importFrom purrr map modify_in
#' @importFrom jsonlite fromJSON write_json
#'
mod_config <- function(path, target, values) {

  config_file <- file.path(path, "config", "config.json")
  if (!file.exists(config_file)) {
    stop("Config file does not exist.")
  }

  config <- fromJSON(config_file)

  permitted <- c("regions", "source", "limit")
  if(!all(as.character(map(target, 1)) %in% permitted)) {
    stop("Non-modifiable route selected.")
  }

  for (i in seq_along(target)) {
    config <- modify_in(config, target[[i]], ~ values[[i]])
  }

  write_json(config, config_file, pretty = TRUE)

}
