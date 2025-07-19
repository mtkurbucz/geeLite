# Main Function ----------------------------------------------------------------

#' Install and Configure a Conda Environment for 'rgee'
#'
#' Sets up a Conda environment with all required Python and R dependencies
#' for using the \code{rgee} package, including a specific version of the
#' \code{earthengine-api}. If Conda is not available, the user will be prompted
#' to install Miniconda. The created environment is automatically registered
#' for use with \code{rgee}.
#' @param conda [optional] (character) Name of the Conda environment to create
#' or use. Defaults to \code{"rgee"}.
#' @param python_version [optional] (character) Python version to use when
#' creating the Conda environment. Defaults to \code{"3.10"}.
#' @param force_recreate [optional] (logical) If \code{TRUE}, deletes and
#' recreates the Conda environment even if it already exists. Defaults to
#' \code{FALSE}.
#' @return Invisibly returns the name of the Conda environment used or created.
#' @note Even after installation, users must manually accept the Conda Terms of
#' Service (ToS) using the `conda tos accept` command before package
#' installation can proceed. Clear instructions will be provided if ToS
#' acceptance is needed.
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

  # Always prompt the user before setup
  message(
    "\nPreparing to configure the Conda environment: ", sQuote(conda), "\n",
    "This will install the required Python packages."
  )

  if (interactive()) {
    resp <- readline("Continue with setup? [y/N]: ")
    if (!tolower(resp) %in% c("y", "yes")) {
      message("Environment setup aborted by user.")
      return(invisible(NULL))
    }
  } else {
    stop("gee_install() must be run in an interactive session.")
  }

  # Define conda_path before referencing it
  conda_path <- tryCatch(reticulate::conda_binary(), error = function(e) NULL)

  if (is.null(conda_path) || !file.exists(conda_path)) {
    message("Conda not found. Installing Miniconda via reticulate...")
    install_miniconda()
    conda_path <- conda_binary()
    if (is.null(conda_path) || !file.exists(conda_path)) {
      stop(
        "Conda not found. Please install Miniconda manually: ",
        "https://docs.conda.io/en/latest/miniconda.html\n",
        "Accept terms during install, add it to PATH if prompted, ",
        "then restart R and rerun `gee_install()`."
      )
    }
  }

  # Check for existing Conda environment
  conda_envs <- tryCatch(reticulate::conda_list(), error = function(e) NULL)
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
    tryCatch({
      conda_create(envname = conda, python_version = python_version)
      message("Conda environment created.")
    }, error = function(e) {
      msg <- e$message
      message(paste0("Error creating Conda environment '", conda, "':\n", msg))

      # Suggest ToS fix, since reticulate may hide the root cause
      message(paste0(
        "\nThis may happen if you have not yet accepted the Conda ",
        "Terms of Service (ToS).\n\n",
        "To accept them, run the following commands in a terminal ",
        "where `conda` is available:\n\n",
        "conda tos accept --override-channels --channel ",
        "https://repo.anaconda.com/pkgs/main\n",
        "conda tos accept --override-channels --channel ",
        "https://repo.anaconda.com/pkgs/r\n",
        "conda tos accept --override-channels --channel ",
        "https://repo.anaconda.com/pkgs/msys2\n\n",
        "Once completed, restart your R session and rerun ",
        "`gee_install()`.\n"
      ))

      return(invisible(NULL))
    })
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
