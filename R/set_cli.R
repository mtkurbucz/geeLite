# Main Function ----------------------------------------------------------------

#' Initialize CLI Files
#'
#' Creates R scripts to enable main functions to be callable through the
#' Command Line Interface (CLI). These scripts are stored within the specified
#' directory of the generated database (\code{cli/...}).
#' @param path [mandatory] (character) Path to the root directory of the
#' generated database.
#' @param verbose [optional] (logical) Display messages (default: \code{TRUE}).
#' @export
#' @examples
#' # Example: Setting up CLI files
#' \dontrun{
#'   set_cli(path = "path/to/db")
#' }
#'
set_cli <- function(path, verbose = TRUE) {

  # Validate 'verbose' and the 'cli' directory within the specified path
  cli_dir_path <- file.path(path, "cli")
  params <- list(path = cli_dir_path, verbose = verbose)
  validate_params(params)

  # Directory containing source files
  src_dir_path <- file.path(system.file(package = "geeLite"), "cli")

  # Retrieve all source files, excluding 'set_cli.R' script
  src_files_path <- setdiff(dir(src_dir_path, full.names = TRUE),
                            file.path(src_dir_path, "set_cli.R"))

  # Process each source file
  process_source_files(src_files_path, path)

  # Output message if 'verbose' is TRUE
  output_message(list("CLI files generated: 'cli/...'."), verbose)

}

# Internal Functions -----------------------------------------------------------

#' Process Source Files
#'
#' Processes multiple source files by iterating through them.
#' @param src_files_path [mandatory] (character) A vector of source file paths.
#' @param path [mandatory] (character) The path to the root directory of the
#' generated database.
#' @keywords internal
#'
process_source_files <- function(src_files_path, path) {
  for (src_file_path in src_files_path) {
    process_single_file(src_file_path, path)
  }
}

# ------------------------------------------------------------------------------

#' Process a Single Source File
#'
#' Processes an individual source file by specifying the 'path' within the
#' script, and writes the output to the CLI directory of the database.
#' @param src_file_path [mandatory] (character) Path of the source file.
#' @param path [mandatory] (character) The path to the root directory of the
#' generated database.
#' @keywords internal
#'
process_single_file <- function(src_file_path, path) {
  src_name <- basename(src_file_path)
  src_content <- readLines(src_file_path)
  if (!src_name %in% c("fetch_regions.R", "gee_install.R")) {
    src_content[1] <- paste0("path <- '", path, "'")
  }
  writeLines(src_content, con = file.path(path, "cli", src_name))
}
