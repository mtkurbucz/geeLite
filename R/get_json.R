# Main Functions ---------------------------------------------------------------

#' @title Print the Configuration File
#'
#' @description Reads and prints the configuration file in a human-readable
#' format.
#'
#' @param path [mandatory] (character) Path to the root directory of the
#' generated database.
#'
#' @return A character string representing the formatted configuration JSON.
#'
#' @export
#'
#' @examples
#' # Example: Printing the configuration file
#' \dontrun{
#'   get_config(path = "path/to/root/directory")
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
#' @param path [mandatory] (character) Path to the root directory of the
#' generated database.
#'
#' @return A character string representing the formatted state JSON.
#'
#' @export
#'
#' @examples
#' # Example: Printing the state file
#' \dontrun{
#'   get_state(path = "path/to/root/directory")
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
#' @param path [mandatory] (character) Path to the root directory of the
#' generated database.
#'
#' @param file_path [mandatory] (character) Relative path to the JSON file from
#' the root directory.
#'
#' @return A character string representing the formatted JSON of the specified
#' file.
#'
#' @keywords internal
#'
#' @importFrom jsonlite fromJSON toJSON
#'
get_json <- function(path, file_path) {

  # Validate parameters
  call <- match.call()
  params <- generate_params(call)
  validate_params(params)

  # Read and format the JSON file
  file_path_full <- file.path(path, file_path)
  # Check if the file exists
  if (!file.exists(file_path_full)) {
    stop("The specified file does not exist: ", file_path_full, 
      "\nIf this error was the result of a get_config() call, then ensure that a configuration file has been created. Try ??set_config for help.",
      "\nIf this error was the result of a get_state() call, then ensure that a state file has been created. Try ??pull_data for help.")
  }
  file_content <- jsonlite::fromJSON(file_path_full)
  formatted_content <- jsonlite::toJSON(file_content, pretty = TRUE)

  return(formatted_content)
}
