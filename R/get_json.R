# Main Functions ---------------------------------------------------------------

#' Print the Configuration File
#'
#' Reads and prints the configuration file from the database's root directory
#' in a human-readable format.
#' @param path [mandatory] (character) The path to the root directory of the
#'   generated database.
#' @return A character string representing the formatted JSON content of the
#'   configuration file.
#' @export
#' @examples
#' # Example: Printing the configuration file
#' \dontrun{
#'   get_config(path = "path/to/db")
#' }
#'
get_config <- function(path) {
  # Convert to absolute path and check existence
  path <- normalizePath(path, mustWork = FALSE)
  get_json(path, "config/config.json")
}

# ------------------------------------------------------------------------------

#' Print the State File
#'
#' Reads and prints the state file from the database's root directory in a
#' human-readable format.
#' @param path [mandatory] (character) The path to the root directory of the
#'   generated database.
#' @return A character string representing the formatted JSON content of the
#'   state file.
#' @export
#' @examples
#' # Example: Printing the state file
#' \dontrun{
#'   get_state(path = "path/to/db")
#' }
#'
get_state <- function(path) {
  # Convert to absolute path and check existence
  path <- normalizePath(path, mustWork = FALSE)
  get_json(path, "state/state.json")
}

# Internal Function ------------------------------------------------------------

#' Print JSON File
#'
#' Reads and prints a specified JSON file from the provided root directory in a
#' human-readable format.
#' @param path [mandatory] (character) The path to the root directory of the
#'   generated database.
#' @param file_path [mandatory] (character) The relative path to the JSON file
#'   from the root directory.
#' @return A character string representing the formatted JSON content of the
#'   specified file.
#' @keywords internal
#' @importFrom jsonlite fromJSON toJSON
#'
get_json <- function(path, file_path) {

  # Validate input parameters
  params <- list(path = path, file_path = file_path)
  validate_params(params)

  # Construct the full file path and read the JSON content
  file_path_full <- file.path(path, file_path)
  file_content <- fromJSON(file_path_full)

  # Format the JSON content in a human-readable format
  formatted_content <- toJSON(file_content, pretty = TRUE)

  return(formatted_content)
}
