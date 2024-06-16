#' @title Printing the Configuration File
#'
#' @description This function prints the configuration file.
#'
#' @param path [mandatory] (character) Path to the root directory of the
#' generated database.
#'
#' @return Configuration object.
#'
#' @export
#'
#' @examples
#' # Example: Printing the configuration file
#' \dontrun{
#'   set_cli(path = "path")
#' }
#'
set_cli <- function(path) {

  folder <- file.path(path, "cli")
  if (!dir.exists(folder)) {
    dir.create(folder)
  }

  cli_path <- file.path(system.file(package = "geeLite"), "cli")
  cli_files <- setdiff(dir(cli_path, full.names = TRUE),
                       file.path(cli_path, "set_cli.R"))

  for (cli_file in cli_files) {
    if (cli_file != "get_fips.R") {
      cli_content <- readLines(cli_file)
      cli_content[1] <- paste0("path <- '", path, "'")
      writeLines(cli_content, con = file.path(folder, basename(cli_file)))
    }
  }
}
