#' @title Collecting and Updating Global Food Insecurity Data
#'
#' @description This function performs the entire update or (re)build operation.
#'
#' @param target.path [Optional] Path to the directory. If not provided, the
#' user is prompted to enter a path. Default value is \code{NULL}.
#'
#' @param operation.code [Optional] For updating, use \code{1}, and for
#' (re)building, use \code{2}. If not provided, the user is prompted to enter a
#' path. The default value is \code{NULL}.
#'
#' @param conda.env [Optional] Conda environment. If \code{NULL} is selected,
#' the user can choose the environment from a list. Default value is
#' \code{"rgee"}.
#'
#' @param quiet [Optional] If \code{TRUE}, suppresses prompts or user
#' interactions. Default value is \code{FALSE}.
#'
#' @export
#'
#' @examples
#' # Example 1: Run update or (re)build operation without providing a custom
#' # path
#' \dontrun{
#'   RunGFIData()
#' }
#'
#' # Example 2: Run update or (re)build operation with a custom path
#' \dontrun{
#'   RunGFIData(target.path = "/path/to/custom/directory", conda.env = "rgee")
#' }
#'
#' @importFrom jsonlite toJSON
#' @importFrom utils packageVersion
#' @importFrom cli cli_h1 cli_alert_info
#'
RunGFIData <- function(target.path = NULL, operation.code = NULL,
                       conda.env = "rgee", quiet = FALSE) {

  # ----------------------------------------------------------------------------
  # S1: Parameter test, start-up diagnosis, and task definition
  # ----------------------------------------------------------------------------

  # If 'target.path' is not provided or is invalid, prompt the user
  if (is.null(target.path) || !file.exists(target.path)) {
    cat("\n")
    target.path <- GeneratePath()
  }

  # Print header with the version number
  version <- as.character(packageVersion("GFIData"))
  cli_h1("")
  cat("\033[1mGlobal Food Insecurity Database - Version:", version, "\033[0m")
  cli_h1("")
  cat("\n")

  # Start-up diagnostics and optional configuration file generation
  operation.code <- StartupDiagn(target.path, operation.code, quiet)

  # Task and post-state generation
  results <- GenerateTask(target.path, operation.code)
  task <- results$task
  post.state <- results$post.state

  # ----------------------------------------------------------------------------
  # S2: Activate Conda enviroment
  # ----------------------------------------------------------------------------

  # Set-up Conda enviroment
  ActivateConda(conda.env)

  # ----------------------------------------------------------------------------
  # S3: Data collection and storage, based on the task
  # ----------------------------------------------------------------------------

  s <- 1L

  # Check if the operation requires the database
  if(any(sub("\\*", "", task$MainDatasets) %in% c("NDVI", "Precipitation"))){

    cli_h1(paste0(s,": Collecting Data from Google Earth Engine"))
    cat("\n")

    # Collect dataset from Global Earth Engine
    gee.data <- GetGEEData(task)
    s <- s + 1L

    cli_h1("")
  }

  # Write data to SQLite
  WriteToSQLite(gee.data, "gee", operation.code, target.path)

  # ----------------------------------------------------------------------------
  # S4: Generate (update) state.json, log.txt, and cli.R files
  # ----------------------------------------------------------------------------

  # Update state.json file
  json.data <- toJSON(as.list(post.state), pretty = TRUE)
  writeLines(json.data, "state/state.json")

  cli_alert_info("Database state updated. Check 'state/state.json'.")

  # Create the log file if it is missing; log the operation
  task$StartDate <- as.character(as.Date(unlist(task$StartDate)))
  task$EndDate <- as.character(as.Date(unlist(task$EndDate)))

  log.message <- paste0("[Timestamp]: ", format(Sys.time(), "%Y-%m-%d %H:%M"),
                        ", [Operation Code]: ", operation.code, ", [Task]: ",
                        paste(names(task),
                              sapply(task, function(x) if (is.character(x))
                                paste0("\"", x, "\"") else x),
                                sep = ": ", collapse = ", "))

  if (file.exists("log/log.txt")) {

    # Append to existing file
    cat(sprintf("%s\n", log.message), file = "log/log.txt", append = TRUE)
  } else {

    # Create and initialize a new file
    cat(sprintf("%s\n", log.message), file = "log/log.txt")
  }

  cli_alert_info("Log file updated. Check 'log/log.txt'.")

  # Create command-line script if it is missing
  cli.path <- file.path(target.path,"cli/cli.R")
  if (!file.exists(cli.path)) {

    # Read complete command-line script
    script.path <- system.file("cli/complete_cli.R", package = "GFIData")
    cli.content <- readLines(script.path)

    # Modify rows for removing path selection option
    cli.content[4] <- 'pkgs <- c("devtools", "optparse")'
    cli.content[40] <- "# Select target path"
    cli.content[41] <- paste0("selected$path <- ","'", target.path,"'")

    # Remove specified rows
    cli.content <- cli.content[-c(11:12, 25:26, 41:57)]

    # Write modified script to the target path
    writeLines(cli.content, con = cli.path)

    cli_alert_info("Command-line script generated. Check 'cli/cli.R'.")
  }

  cat("\n")
}
