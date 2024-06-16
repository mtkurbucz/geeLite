#' @title Activate Conda Environment
#'
#' @description This function provides users with the flexibility to either
#' specify a Conda environment or choose from a list of available environments
#' if none is explicitly defined.
#'
#' @param conda.env [Optional] Conda environment. If \code{NULL} is selected,
#' the user can choose the environment from a list. Default value is
#' \code{"rgee"}.
#'
#' @export
#'
#' @examples
#' # Example 1: Activate Conda environment without providing an environment name
#' \dontrun{
#' ActivateConda()
#' }
#'
#' # Example 2: Activate Conda environment with providing an environment name
#' \dontrun{
#' ActivateConda(conda.env = "rgee")
#' }
#'
#' @importFrom reticulate conda_list use_condaenv
#' @importFrom cli cli_ul cli_li cli_end cli_alert_success
#'
ActivateConda <- function(conda.env = "rgee") {

  # ----------------------------------------------------------------------------
  # S1: If 'conda.env' is NULL, prompt the user to make a selection
  # ----------------------------------------------------------------------------

  if (is.null(conda.env)) {

    # List Conda environments
    conda.envs <- conda_list()

    # Check if any Conda environments are found
    if (is.null(conda.envs)) {
      stop("No Conda environments found.")
    }

    # Let the user choose a Conda environment
    cat("Available Conda environments:\n")

    ulid <- cli_ul()
    for (i in seq_len(nrow(conda.envs))) {
      cli_li(sprintf("[%d] %s\n", i, conda.envs$name[i]))
    }
    cli_end(ulid)

    cat("\n")
    selected.index <- as.integer(readline(paste0("Select the desired Conda ",
    "environment: ")))

    if (selected.index < 1L || selected.index > length(conda.envs)) {
      stop("Invalid selection.")
    }

    conda.env <- conda.envs$name[selected.index]
  }

  # ----------------------------------------------------------------------------
  # S2: Activate selected Conda enviroment
  # ----------------------------------------------------------------------------

  # Activate the Conda environment
  use_condaenv(conda.env, required = TRUE)
  cat("\n")
  cli_alert_success("Conda environment was activated successfully.\n")

}
