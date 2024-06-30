#' @title Check Directory Path Validity
#'
#' @description Throws an error if the directory path is invalid.
#'
#' @param directory_path A character string specifying the path to the
#' directory.
#' @return Invisible NULL if the directory exists.
#'
check_directory_validity <- function(directory_path) {
  if (!dir.exists(directory_path)) {
    stop(sprintf("Directory not found: %s", directory_path))
  }
  invisible(NULL)
}

# ------------------------------------------------------------------------------

#' @title Check File Path Validity
#'
#' @description Throws an error if the file path is invalid.
#'
#' @param file_path A character string specifying the path to the file.
#' @return Invisible NULL if the file exists.
#'
check_file_validity <- function(file_path) {
  if (!file.exists(file_path)) {
    stop(sprintf("File not found: %s", file_path))
  }
  invisible(NULL)
}
