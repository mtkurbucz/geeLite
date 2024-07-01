# Main Function ----------------------------------------------------------------

#' @title Set Up CLI Files
#'
#' @description Creates R scripts to enable main functions to be callable
#' through the Command Line Interface (CLI).
#'
#' @param path [mandatory] (character) The path to the root directory of the
#' generated database.
#'
#' @param verbose [optional] (logical) Display messages (default: \code{TRUE}).
#'
#' @export
#'
#' @examples
#' # Example: Setting up CLI files
#' \dontrun{
#'   set_cli(path = "path/to/root/directory")
#' }
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

  # Process each source file
  process_source_files(src_files, path)

  # Output information if verbose mode is enabled
  output_info("CLI files generated: 'cli/...'.", verbose)

}

# Internal Functions -----------------------------------------------------------

#' @title Process Source Files
#'
#' @description Processes multiple source files by iterating through them.
#'
#' @param src_files [mandatory] (character) A vector of source file paths.
#'
#' @param path [mandatory] (character) The path to the root directory of the
#' generated database.
#'
#' @keywords internal
#'
process_source_files <- function(src_files, path) {
  for (src_file in src_files) {
    process_single_file(src_file, path)
  }
}

# ------------------------------------------------------------------------------

#' @title Process a Single Source File
#'
#' @description Processes an individual source file.
#'
#' @param src_file [mandatory] (character) The path of the source file.
#'
#' @param path [mandatory] (character) The path to the root directory of the
#' generated database.
#'
#' @keywords internal
#'
process_single_file <- function(src_file, path) {
  src_name <- basename(src_file)
  src_content <- readLines(src_file)
  if (src_name != "fetch_regions.R") {
    src_content[1] <- paste0("path <- '", path, "'")
  }
  writeLines(src_content, con = file.path(file.path(path, "cli"), src_name))
}
