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
  #if (length(pkgs <- setdiff(pkgs, rownames(installed.packages()))))
  #install.packages(pkgs)
  #rm(pkgs)
  install.package.updates <- function(packages) {
    # Function to check if a package is installed
    is_installed <- function(pkg) {
      is.element(pkg, installed.packages()[, "Package"])
    }
    
    # Function to detach a package if it is loaded
    detach_package <- function(pkg) {
      pkg_name <- paste("package:", pkg, sep = "")
      if (pkg_name %in% search()) {
        detach(pkg_name, unload = TRUE, character.only = TRUE)
        message(sprintf("Package '%s' has been detached.", pkg))
      }
    }
    
    # Function to install or update a package
    install_or_update <- function(pkg) {
      if (!is_installed(pkg)) {
        # Install package if not installed
        install.packages(pkg, dependencies = TRUE)
        message(sprintf("Package '%s' has been installed.", pkg))
      } else {
        # Detach the package if it is loaded
        detach_package(pkg)
        
        # Check for available updates
        update_needed <- old.packages()
        if (!is.null(update_needed) && pkg %in% rownames(update_needed)) {
          # Update package if a new version is available
          install.packages(pkg, dependencies = TRUE)
          message(sprintf("Package '%s' has been updated.", pkg))
        } else {
          message(sprintf("Package '%s' is already up-to-date.", pkg))
        }
      }
    }
    
    # Apply the function to all packages
    sapply(packages, install_or_update)
    
    # Reload the packages
    lapply(packages, library, character.only = TRUE)
  }
  install.packages.updates(pkgs)
  
  # Install 'rgee' dependencies and specify the 'earthengine-api' version
  ee_install(py_env = conda, confirm = FALSE)
  ee_install_upgrade(version = "0.1.370")
}
