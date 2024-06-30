# Main Functions ---------------------------------------------------------------

#' @title Print the Configuration File
#'
#' @description Reads and prints the configuration file in a human-readable
#' format.
#'
#' @param path (character) Path to the root directory of the generated database.
#' @return A character string representing the formatted configuration JSON.
#' @export
#' @examples
#' # Example: Printing the configuration file
#' \dontrun{
#'   config <- get_config(path = "path/to/root/directory")
#'   cat(config)
#' }
#'
get_config <- function(path) {
  get_json(path, "config/config.json")
}

# ------------------------------------------------------------------------------

#' @title Print the State File
#'
#' @description Reads and prints the state file in a human-readable format.
#'
#' @param path (character) Path to the root directory of the generated database.
#' @return A character string representing the formatted state JSON.
#' @export
#' @examples
#' # Example: Printing the state file
#' \dontrun{
#'   state <- get_state(path = "path/to/root/directory")
#'   cat(state)
#' }
#'
get_state <- function(path) {
  get_json(path, "state/state.json")
}

# Internal Function ------------------------------------------------------------

#' @title Print JSON File
#'
#' @description Reads and prints a specified JSON file in a human-readable
#' format from the specified root directory of the generated database.
#'
#' @param path (character) Path to the root directory of the generated database.
#' Must be a valid directory path.
#' @param file_path (character) Relative path to the JSON file from the root
#' directory.
#' @return A character string representing the formatted JSON of the specified
#' file.
#' @importFrom jsonlite fromJSON toJSON
#'
get_json <- function(path, file_path) {

  # Validate 'path' parameter
  check_directory_validity(path)

  full_file_path <- file.path(path, file_path)

  # Validate 'file_path' parameter
  check_file_validity(full_file_path)

  # Check if the specified file exists
  if (!file.exists(full_file_path)) {
    stop(sprintf("File does not exist at path: %s", full_file_path))
  }

  # Read and format the JSON file
  file_content <- jsonlite::fromJSON(full_file_path)
  formatted_content <- jsonlite::toJSON(file_content, pretty = TRUE)

  return(formatted_content)
}
