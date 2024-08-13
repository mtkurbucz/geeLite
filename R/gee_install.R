# Main Function ----------------------------------------------------------------

#' Create a Conda Environment with 'rgee' Dependencies
#'
#' This function creates an isolated Conda environment with all necessary
#' dependencies for the \code{rgee} package, including the specific version of
#' \code{earthengine-api} required by the current version of \code{geeLite}. It
#' is based on the \code{ee_install} function from the \code{rgee} package.
#' @param conda [optional] (character) Name of the virtual Conda environment
#' installed and used by the \code{rgee} package (default: \code{"rgee"}).
#' @export
#' @examples
#' # Example: Creating a Conda environment with 'rgee' dependencies
#' \dontrun{
#'   gee_install()
#' }
#' @importFrom rgee ee_install ee_install_upgrade
#' @importFrom utils installed.packages install.packages
#'
gee_install <- function(conda = "rgee") {

  # Install any missing required packages
  pkgs <- c("geojsonio", "rnaturalearthdata")
  if (length(pkgs <- setdiff(pkgs, rownames(installed.packages()))))
  install.packages(pkgs)
  rm(pkgs)

  # Install 'rgee' dependencies and specify the 'earthengine-api' version
  ee_install(py_env = conda, confirm = FALSE)
  ee_install_upgrade(version = "0.1.370")
}
