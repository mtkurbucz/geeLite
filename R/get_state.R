#' @title Print the State File
#'
#' @description This function reads and prints the state file in a
#' human-readable JSON format from the specified root directory of the generated database.
#'
#' @param path [mandatory] (character) Path to the root directory of the generated database.
#' Must be a valid directory path.
#'
#' @return A character string representing the formatted state JSON.
#'
#' @export
#'
#' @examples
#' # Example: Printing the state file
#' \dontrun{
#'   state <- get_state(path = "path/to/root/directory")
#'   cat(state)
#' }
#'
#' @importFrom jsonlite fromJSON toJSON
#'
get_state <- function(path) {
  
  # Validate 'path' parameter
  if (!is.character(path) || length(path) != 1 || path == "") {
    stop("'path' must be a non-empty character string.")
  }
  
  state_file <- file.path(path, "state", "state.json")
  
  # Check if the state file exists
  if (!file.exists(state_file)) {
    stop(sprintf("State file does not exist at path: %s", state_file))
  }
  
  # Read and format the JSON state
  state <- jsonlite::fromJSON(state_file)
  formatted_state <- jsonlite::toJSON(state, pretty = TRUE)
  
  return(formatted_state)
}