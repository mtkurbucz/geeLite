#' @title Custom Target Path Generation
#'
#' @description This function prompts the user to enter a valid target path. If
#' the user cancels the input or provides an invalid path, the function will ask
#' again until a valid path is provided.
#'
#' @return The selected \code{target.path}.
#'
#' @export
#'
#' @examples
#' # Example: Run custom path generation
#' \dontrun{
#' result <- GeneratePath()
#' print(result)
#' }
#'
#' @importFrom cli cli_alert_info cli_alert_danger
#'
GeneratePath <- function() {

  # ----------------------------------------------------------------------------
  # S1: Ensure target path validity and prompt for correction if invalid
  # ----------------------------------------------------------------------------

  valid.path <- FALSE

  while (!valid.path) {

    # If the path is not provided or not valid, prompt the user
    target.path <- readline(prompt = cat(paste0("Enter the path (or press ",
                                                "Enter to use the current ",
                                                "path, or 'Cancel' to abort)",
                                                ":\n")))
    cat("\n")

    # Check if the user canceled the input
    if (tolower(target.path) == "cancel") {
      stop("User canceled operation. Stopping.")
    } else if (length(target.path) == 0L || target.path[1] == "") {
      target.path <- getwd()
    }

    # If the user did not enter a path or the path is not valid, ask again
    if (!file.exists(target.path)) {
      cli_alert_danger(paste0("Invalid path.\n\n"))
    } else {
      valid.path <- TRUE
      cli_alert_info(paste0("Selected path: ", target.path))
    }
  }

  return(target.path)
}
