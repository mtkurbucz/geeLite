#' @title Printing the State File
#'
#' @description This function prints the state file.
#'
#' @param path [mandatory] (character) Path to the root directory of the
#' generated database.
#'
#' @return State object.
#'
#' @export
#'
#' @examples
#' # Example: Printing the state file
#' \dontrun{
#'   get_state(path = "path")
#' }
#'
#' @importFrom jsonlite fromJSON toJSON
#'
get_state <- function(path) {

  state_file <- file.path(path, "state", "state.json")
  if (!file.exists(state_file)) {stop("State file does not exist.")}
  state <- toJSON(fromJSON(state_file), pretty = TRUE)

  return(state)
}
