# Main Functions ---------------------------------------------------------------

#' @title Set Up CLI Files
#'
#' @description Creates R scripts to enable main functions to be callable
#' through the Command Line Interface (CLI).
#'
#' @param path [mandatory] (character) The path to the root directory of the
#' generated database.
#' @param verbose [optional] (logical) Display messages (default: \code{TRUE}).
#' @export
#' @examples
#' # Example: Setting up CLI files
#' \dontrun{
#'   set_cli(path = "path/to/root/directory")
#' }
#' @importFrom cli cli_alert_info
#'
set_cli <- function(path, verbose = TRUE) {

  # Validate 'verbose' and the 'cli' directory within the specified path
  cli_dir <- file.path(path, "cli")
  params <- list(path = cli_dir, verbose = verbose)
  validate_params(params)

  # Directory containing source files
  src_dir <- file.path(system.file(package = "geeLite"), "cli")

  # Retrieve all source files, excluding "set_cli.R" script
  src_files <- setdiff(dir(src_dir, full.names = TRUE),
                       file.path(src_dir, "set_cli.R"))

  # Iterate through source files
  for (src_file in src_files) {

    # Get the base name of the source file
    src_name <- basename(src_file)

    # Read the content of the source file
    src_content <- readLines(src_file)

    # Modify the content for each file except "fetch_regions.R"
    if (src_name != "fetch_regions.R") {
      src_content[1] <- paste0("path <- '", path, "'")
    }

    # Write modified content to the CLI directory
    writeLines(src_content, con = file.path(cli_dir, src_name))

  }

  # Output information if verbose mode is enabled
  if (verbose) {
    cat("\n")
    cli_alert_info("CLI files generated: 'cli/...'.")
    cat("\n")
  }
}
