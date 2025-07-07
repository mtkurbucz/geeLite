# Main Function ----------------------------------------------------------------

#' Install and Configure a Conda Environment for 'rgee'
#'
#' Sets up a Conda environment with all required Python and R dependencies
#' for using the \code{rgee} package, including a specific version of the
#' \code{earthengine-api}. If Conda is not available, Miniconda will be
#' installed. The created environment is automatically registered for use with
#' \code{rgee}.
#' @param conda [optional] (character) Name of the Conda environment to create
#' or use. Defaults to \code{"rgee"}.
#' @param python_version [optional] (character) Python version to use when
#' creating the Conda environment. Defaults to \code{"3.10"}.
#' @param force_recreate [optional] (logical) If \code{TRUE}, deletes and
#' recreates the Conda environment even if it already exists. Defaults to
#' \code{FALSE}.
#' @return Invisibly returns the name of the Conda environment used or created.
#' @export
#' @examples
#' # Example: Creating a Conda environment with 'rgee' dependencies
#' \dontrun{
#'   gee_install()
#' }
#' @importFrom rgee ee_install_set_pyenv
#' @importFrom rstudioapi isAvailable restartSession
#' @importFrom utils old.packages install.packages installed.packages
#' @importFrom reticulate conda_binary conda_create conda_list conda_python
#' conda_remove install_miniconda py_install
#'
gee_install <- function(conda = "rgee", python_version = "3.10",
                         force_recreate = FALSE) {
  # Required R packages
  pkgs <- c("geojsonio", "rnaturalearthdata")

  install_or_update_r_packages <- function(packages) {
    is_installed <- function(pkg) pkg %in% rownames(installed.packages())
    detach_package <- function(pkg) {
      if (paste0("package:", pkg) %in% search()) {
        detach(paste0("package:", pkg), unload = TRUE, character.only = TRUE)
      }
    }
    for (pkg in packages) {
      if (!is_installed(pkg)) {
        install.packages(pkg, dependencies = TRUE)
      } else {
        detach_package(pkg)
        updates <- old.packages()
        if (!is.null(updates) && pkg %in% rownames(updates)) {
          install.packages(pkg, dependencies = TRUE)
        }
      }
      if (is_installed(pkg)) {
        suppressPackageStartupMessages(library(pkg, character.only = TRUE))
      }
    }
  }

  # Ensure conda is available or install Miniconda
  conda_path <- tryCatch(conda_binary(), error = function(e) NULL)
  if (is.null(conda_path) || !file.exists(conda_path)) {
    message("Conda not found. Installing Miniconda via reticulate...")
    install_miniconda()
    conda_path <- conda_binary()
    if (is.null(conda_path) || !file.exists(conda_path)) {
      stop("Failed to install Miniconda. Please install it manually.")
    }
  }

  # Update/install required R packages
  suppressMessages(suppressWarnings(
    install_or_update_r_packages(pkgs)
  ))

  # Check for existing Conda environment
  conda_envs <- tryCatch(conda_list(), error = function(e) NULL)
  env_exists <- !is.null(conda_envs) && conda %in% conda_envs$name

  # Remove and recreate if needed
  if (force_recreate && env_exists) {
    message("Removing existing Conda environment: ", conda)
    conda_remove(envname = conda)
    env_exists <- FALSE
  }

  # Create the environment if needed
  if (!env_exists) {
    message("Creating Conda environment: ", conda)
    conda_create(envname = conda, python_version = python_version)
    message("Conda environment created.")
  } else {
    message("Using existing Conda environment: ", conda)
  }

  # Install Python dependencies
  message("Installing Python packages: earthengine-api, ee_extra, numpy")
  tryCatch({
    py_install(
      packages = c("earthengine-api==0.1.370", "ee_extra", "numpy"),
      envname = conda,
      method = "conda"
    )
    message("Python packages installed.")
  }, error = function(e) {
    stop("Python package installation failed: ", e$message)
  })

  # Register with rgee
  py_path <- conda_python(envname = conda)
  if (!file.exists(py_path) || file.access(py_path, 1) != 0) {
    stop("Python executable not found or not accessible: ", py_path)
  }
  ee_install_set_pyenv(py_path = py_path, py_env = conda, confirm = FALSE)

  # Prompt to restart R
  if (interactive()) {
    resp <- readline("Restart R now? [Y/n]: ")
    restart <- tolower(resp) %in% c("", "y", "yes")
    if (restart && isAvailable()) {
      restartSession()
    } else {
      message("Please restart R manually.")
    }
  }

  invisible(conda)
}
