# Main Function ----------------------------------------------------------------

#' Initialize CLI Files
#'
#' Creates R scripts to enable the main functions to be called through the
#' Command Line Interface (CLI). These scripts are stored in the \code{cli/}
#' directory of the generated database.
#' @param path [mandatory] (character) The path to the root directory of the
#'   generated database. This must be a writable, non-temporary directory.
#'   Avoid using the home directory (~), the current working directory, or the
#'   package directory.
#' @param verbose [optional] (logical) Whether to display messages (default:
#'   \code{TRUE}).
#' @return No return value, called for side effects.
#' @export
#' @examples
#' \dontrun{
#'   set_cli(path = tempdir())
#' }
#'
set_cli <- function(path, verbose = TRUE) {

  # Convert to absolute path and check existence
  path <- normalizePath(path, mustWork = FALSE)

  # Check if the directory ('path') exists, and create it if necessary
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  # Define the path for the cli subdirectory
  cli_dir_path <- file.path(path, "cli")

  # Check if the cli subdirectory exists, and create it if necessary
  if (!dir.exists(cli_dir_path)) {
    dir.create(cli_dir_path)
  }

  # Validate the 'verbose' parameter and ensure the 'cli' directory exists
  params <- list(path = cli_dir_path, verbose = verbose)
  validate_params(params)

  # Directory containing source files for CLI scripts
  src_dir_path <- file.path(system.file(package = "geeLite"), "cli")

  # Retrieve all source files, excluding 'set_cli.R'
  src_files_path <- setdiff(dir(src_dir_path, full.names = TRUE),
                            file.path(src_dir_path, "set_cli.R"))

  # Process each source file to create CLI scripts
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
#'   generated database.
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
#' Processes an individual source file by updating the file with the specified
#' 'path' and writing the updated file to the \code{cli/} directory of the
#' database.
#' @param src_file_path [mandatory] (character) The path of the source file to
#'   process.
#' @param path [mandatory] (character) The path to the root directory of the
#'   generated database.
#' @keywords internal
#'
process_single_file <- function(src_file_path, path) {
  src_name <- basename(src_file_path)
  src_content <- readLines(src_file_path)

  # Modify the first line of the script to set the 'path' if required
  if (!src_name %in% c("fetch_regions.R", "gee_install.R")) {
    path <- normalizePath(path, winslash = "/", mustWork = FALSE)
    src_content[3] <- paste0("path <- '", path, "'")
  }

  # Write the updated file to the 'cli/' directory
  writeLines(src_content, con = file.path(path, "cli", src_name))
}
