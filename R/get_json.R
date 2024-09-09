# Main Functions ---------------------------------------------------------------

#' Print the Configuration File
#'
#' Reads and prints the configuration file in a human-readable format.
#' @param path [mandatory] (character) Path to the root directory of the
#' generated database.
#' @return A character string representing the formatted configuration JSON.
#' @export
#' @examples
#' # Example: Printing the configuration file
#' \dontrun{
#'   get_config(path = "path/to/db")
#' }
#'
get_config <- function(path) {
  get_json(path, "config/config.json")
}

# ------------------------------------------------------------------------------

#' Print the State File
#'
#' Reads and prints the state file in a human-readable format.
#' @param path [mandatory] (character) Path to the root directory of the
#' generated database.
#' @return A character string representing the formatted state JSON.
#' @export
#' @examples
#' # Example: Printing the state file
#' \dontrun{
#'   get_state(path = "path/to/db")
#' }
#'
get_state <- function(path) {
  get_json(path, "state/state.json")
}

# Internal Function ------------------------------------------------------------

#' Print JSON File
#'
#' Reads and prints a specified JSON file in a human-readable format from the
#' specified root directory of the generated database.
#' @param path [mandatory] (character) Path to the root directory of the
#' generated database.
#' @param file_path [mandatory] (character) Relative path to the JSON file from
#' the root directory.
#' @return A character string representing the formatted JSON of the specified
#' file.
#' @keywords internal
#' @importFrom jsonlite fromJSON toJSON
#'
get_json <- function(path, file_path) {

  # Validate parameters
  params <- list(path = path, file_path = file_path)
  validate_params(params)

  # Read and format the JSON file
  file_path_full <- file.path(path, file_path)
  file_content <- fromJSON(file_path_full)
  formatted_content <- toJSON(file_content, pretty = TRUE)

  return(formatted_content)
}
