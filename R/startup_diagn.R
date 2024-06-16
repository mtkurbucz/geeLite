#' @title Start-Up Diagnostics
#'
#' @description This function performs start-up diagnostics for the GFIData package. It
#' generates the \code{target.path} and \code{operation.code} global variables
#' and optionally sets up the configuration file.
#'
#' @param target.path [Optional] Path to the directory. If not provided, the
#' user is prompted to enter a path. Default value is \code{NULL}.
#'
#' @param operation.code [Optional] For updating, use \code{1}, and for
#' (re)building, use \code{2}. If not provided, the user is prompted to enter a
#' path. The default value is \code{NULL}.
#'
#' @param quiet [Optional] If \code{TRUE}, suppresses prompts or user
#' interactions. Default value is \code{FALSE}.
#'
#' @return The selected \code{operation.code}. For updating, it is \code{1};
#' for (re)building, it is \code{2}.
#'
#' @export
#'
#' @examples
#' # Example 1: Run start-up diagnostics without providing a target.path
#' \dontrun{
#' result <- StartupDiagn()
#' print(result)
#' }
#'
#' # Example 2: Run start-up diagnostics with a target.path
#' \dontrun{
#' result <- StartupDiagn(target.path = "/path/to/custom/directory")
#' print(result)
#' }
#'
#' @importFrom utils menu
#' @importFrom cli cli_ul cli_li cli_end
#'
StartupDiagn <- function(target.path = NULL, operation.code = NULL,
                         quiet = FALSE) {

  # ----------------------------------------------------------------------------
  # S1: Parameter test
  # ----------------------------------------------------------------------------

  # If 'target.path' is not provided or is invalid, prompt the user
  if (is.null(target.path) || !file.exists(target.path)) {
    target.path <- GeneratePath()
  }

  # Check if 'operation.code' is NULL, 1, or 2
  if (!is.null(operation.code) && !operation.code %in% c(1L, 2L)) {
    stop("'operation.code' must be NULL, 1, or 2. Stopping.")
  }

  # Check if 'quiet' is TRUE and operation code is NULL
  if (quiet && is.null(operation.code)) {
    stop("If 'quiet' is TRUE, 'operation.code' must be specified. Stopping.")
  }

  # Check if 'quiet is logical
  if (!is.logical(quiet)) {
    stop("'quiet' parameter must be a logical value. Stopping.")
  }

  setwd(target.path)

  # ----------------------------------------------------------------------------
  # S2: Check the contents of the target path
  # ----------------------------------------------------------------------------

  # Preliminary user inputs and diagnostics
  available <- vector()
  files.to.check <- c("config/config.json", "state/state.json",
                      "data/GFIData.db")

  cat("Start-up diagnostics: \n\n")
  ulid <- cli_ul()
  for (i in 1:length(files.to.check)) {
    available[i] <- file.exists(files.to.check[i])
    symbol <- ifelse(available[i], "\033[1;32m\xE2\x9C\x93\033[0m",
                     "\033[1;31m\xE2\x9C\x98\033[0m")
    cli_li(paste0(files.to.check[i], " is available: ", symbol), "\n")
  }
  cli_end(ulid)
  cat("\n")

  # Check if 'quiet' is TRUE and 'operation.code' is missing or empty
  if (quiet && available[1]) {
    stop(paste0("If 'quiet' is TRUE and config file is missing, configuration ",
                "parameters must be specified. Stopping."))
  }

  # ----------------------------------------------------------------------------
  # S3: Define operation mode and generate a configuration file if necessary
  # ----------------------------------------------------------------------------

  # Check if 'operation.code' is NULL
  if (is.null(operation.code)) {

    # Selecting operation mode
    if (all(available)) {
      operation.code <- menu(c("Update", "Rebuild", "Cancel"),
                          title = "Select an option:")
    } else {
      operation.code <- menu(c("Build", "Cancel"),
                             title = "Select an option:") + 1L
    }

    # Stop if "cancel"
    if (operation.code == 3L) {
      stop("User canceled operation.")
    } else if (operation.code == 2L) {
      cat("\n")
      new.config <- menu(c("Yes", "No"), title = paste0("Would you like to ",
                                                        "create a new ",
                                                        "configuration file?"))
      if (new.config == 1L) {

        # Generate configuration file
        suppressMessages({
          GenerateConfig(target.path)
        })
      }
    }
  }

  return(operation.code)
}
