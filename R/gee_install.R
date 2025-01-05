# Main Function ----------------------------------------------------------------

#' Create a Conda Environment with 'rgee' Dependencies
#'
#' Sets up an isolated Conda environment with all the necessary dependencies
#' for the \code{rgee} package, including the specific version of the
#' \code{earthengine-api} required by the current version of \code{geeLite}. It
#' is based on the \code{ee_install} function from the \code{rgee} package and
#' ensures that other essential packages, like \code{geojsonio} and
#' \code{rnaturalearthdata}, are installed and updated if needed.
#' @param conda [optional] (character) The name of the virtual Conda
#'   environment used by the \code{rgee} package (default: \code{"rgee"}).
#' @export
#' @examples
#' # Example: Creating a Conda environment with 'rgee' dependencies
#' \dontrun{
#'   gee_install()
#' }
#' @importFrom rgee ee_install ee_install_upgrade
#' @importFrom utils old.packages install.packages installed.packages
#'
gee_install <- function(conda = "rgee") {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  install_package_updates <- NULL

  # Install or update required R packages ('geojsonio' and 'rnaturalearthdata')
  pkgs <- c("geojsonio", "rnaturalearthdata")

  # Function to install or update a list of R packages
  install_package_updates <- function(packages) {

    # Check if a package is already installed
    is_installed <- function(pkg) {
      is.element(pkg, installed.packages()[, "Package"])
    }

    # Detach a package if it is loaded in the current session
    detach_package <- function(pkg) {
      pkg_name <- paste("package:", pkg, sep = "")
      if (pkg_name %in% search()) {
        detach(pkg_name, unload = TRUE, character.only = TRUE)
        message(sprintf("Package '%s' has been detached.", pkg))
      }
    }

    # Install or update an individual package
    install_or_update <- function(pkg) {
      if (!is_installed(pkg)) {
        # Install the package if it's not already installed
        install.packages(pkg, dependencies = TRUE)
        message(sprintf("Package '%s' has been installed.", pkg))
      } else {
        # Detach the package if it's loaded
        detach_package(pkg)

        # Check for available updates
        update_needed <- old.packages()
        if (!is.null(update_needed) && pkg %in% rownames(update_needed)) {
          # Update the package if a new version is available
          install.packages(pkg, dependencies = TRUE)
          message(sprintf("Package '%s' has been updated.", pkg))
        } else {
          message(sprintf("Package '%s' is already up-to-date.", pkg))
        }
      }
    }

    # Apply the installation or update process to all specified packages
    sapply(packages, install_or_update)

    # Reload installed or updated packages
    lapply(packages, library, character.only = TRUE)
  }

  # Install or update the required packages
  install_package_updates(pkgs)

  # Install 'rgee' dependencies in the specified Conda environment
  ee_install(py_env = conda, confirm = FALSE)

  # Downgrade 'earthengine-api' to the specified version
  ee_install_upgrade(version = "0.1.370", earthengine_env = conda)
}
