#' @title Task Generation
#'
#' @description This function generates the task that controls the updating or
#' (re)building operation. Additionally, it creates the \code{post.state}
#' object. Upon successful (re)building/update, the \code{state.json} file is
#' updated with information from this \code{post.state} object.
#'
#' @param target.path [Optional] Path to the directory. If not provided, the
#' user is prompted to enter a path. Default value is \code{NULL}.
#'
#' @param operation.code [Optional] For updating, use \code{1}, and for
#' (re)building, use \code{2}. If not provided, the user is prompted to enter a
#' path. The default value is \code{NULL}.
#'
#' @return A list containing the \code{task} and \code{post.state} objects.
#'
#' @export
#'
#' @examples
#' # Example 1: Run task generation without an operation code and a target path
#' \dontrun{
#' result  <- GenerateTask()
#' print(result)
#' }
#'
#' # Example 2: Run task generation with an operation code and a target path
#' \dontrun{
#' result <- GenerateTask(target.path = "/path/to/custom/directory",
#' operation.code = 2)
#' }
#'
#' @import lubridate
#' @importFrom jsonlite fromJSON
#' @importFrom cli cli_alert_warning
#'
GenerateTask <- function(target.path = NULL, operation.code = NULL) {

  # ----------------------------------------------------------------------------
  # S1: Test parameters and check configuration file existence
  # ----------------------------------------------------------------------------

  # If 'target.path' is not provided or is invalid, prompt the user
  if (is.null(target.path) || !file.exists(target.path)) {
    target.path <- GeneratePath()
  }

  # Check if 'operation.code' is NULL, 1, or 2
  if (!is.null(operation.code) && !operation.code %in% c(1L, 2L)) {
    stop("'operation.code' must be NULL, 1, or 2. Stopping.")
  }

  setwd(target.path)

  # Check if configuration file exists
  config.path <- file.path(target.path, "config/config.json")
  if (!file.exists(config.path)) {
    stop("Configuration file is missing. Stopping.")
  }

  # Read configuration file
  config <- fromJSON(config.path, simplifyVector = FALSE)

  # ----------------------------------------------------------------------------
  # S2: Define one period from frequency
  # ----------------------------------------------------------------------------

  OnePeriod <- function(frequency) {
    if (frequency == "Daily") {
      return(1L)
    } else if (frequency == "Monthly") {
      return(months(1))
    } else if (frequency == "Yearly") {
      return(months(12))
    } else {
      stop("Invalid input. Please provide 'Daily', 'Monthly', or 'Yearly'.")
    }
  }

  one.period <- OnePeriod(config$SelectedFrequency)

  # ----------------------------------------------------------------------------
  # S3: Generate directories if they are missing and update configuration date
  # ----------------------------------------------------------------------------

  # Generate directories if they are missing
  folders <- c("data", "log", "cli", "state")
  for (folder in folders) {
    if (!dir.exists(folder)) {
      dir.create(folder, recursive = TRUE)
    }
  }

  # Set 'today' to today's date
  if (unlist(config$DateRange[2]) == "today"){
    config$DateRange[2] <- as.character(Sys.Date())
  }

  # ----------------------------------------------------------------------------
  # S5: Definition of the main session
  # ----------------------------------------------------------------------------

  main.session <- TRUE

  if (operation.code == 2L) { # (Re)build

    # Define dataset's post-state after a successful (re)build/update
    post.state <- list(
      DateRange = unlist(config$DateRange),
      SelectedCountries = unlist(config$SelectedCountries),
      SelectedFrequency = unlist(config$SelectedFrequency),
      SelectedDatasets = unlist(config$SelectedDatasets)
    )

    # Define task
    task <- list()
    task$Frequency <- unlist(config$SelectedFrequency)
    task$MainCountries <- unlist(config$SelectedCountries)
    task$MainDatasets <- unlist(config$SelectedDatasets)
    config.dates <- unlist(config$DateRange)#lapply(config$DateRange, as.Date)
    task$MainStartDate <- config.dates[1]
    task$MainEndDate <- config.dates[2]
    task$MainSession <- main.session

  } else { # Update

    # Check if state file exists
    state.path <- file.path(target.path, "state/state.json")
    if (!file.exists(state.path)) {
      stop("State file is missing. Stopping.")
    }

    # Read state file
    state <- fromJSON(state.path, simplifyVector = FALSE)
    task <- list()

    # Check 'SelectedFrequency'
    if (!identical(config$SelectedFrequency, state$SelectedFrequency)) {
      stop("SelectedFrequency is different in configuration and state.")
    } else {
      task$Frequency <- unlist(state$SelectedFrequency)
    }

    # Check 'SelectedCountries'
    unique.countries <- setdiff(config$SelectedCountries,
                                state$SelectedCountries)
    if (length(unique.countries) > 0L) {
      task$MainCountries <- unlist(c(state$SelectedCountries,
                                  paste0(unique.countries, "*")))
    } else {
      task$MainCountries <- unlist(state$SelectedCountries)
    }

    # Check 'SelectedDatasets'
    unique.datasets <- setdiff(config$SelectedDatasets,
                               state$SelectedDatasets)
    if (length(unique.datasets) > 0L) {
      task$MainDatasets <- unlist(c(state$SelectedDatasets,
                                 paste0(unique.datasets, "*")))
    } else {
      task$MainDatasets <- unlist(state$SelectedDatasets)
    }

    # Extract date ranges
    config.date <- as.Date(unlist(config$DateRange))
    state.date <- as.Date(unlist(state$DateRange))

    if (config.date[1] >= state.date[1] &  # State range includes
        config.date[2] <= state.date[2]) { # config range

      # Set the task date to the state date and warn the user
      task$MainStartDate <- state.date[1]
      task$MainEndDate <- state.date[2]
      cat("\n")
      cli_alert_warning(paste0("The specified 'DateRange' in the ",
                               "configuration is within the 'DateRange' of ",
                               "the state."))
      cli_alert_warning(paste0("The 'DateRange' from the state file will be ",
                               "applied.\n\n"))

      # Only execute the supplementary session
      main.session <- FALSE

    } else if (state.date[1] > config.date[1] &  # Config range includes
               state.date[2] < config.date[2]) { # state range

      non.overlap.start <- list()
      non.overlap.start <- list()

      non.overlap.start

      non.overlap.start <- c(config.date[1], state.date[2] + one.period)
      non.overlap.end <- c(state.date[1] - one.period, config.date[2])

      # Set the task date to the state date
      task$MainStartDate <- non.overlap.start
      task$MainEndDate <- non.overlap.end

    } else { # Config range extends beyond state range in one direction

      if(config.date[2] > state.date[2]){ # Ends later

        # Set the task date to the state date and warn the user if necessary
        task$MainStartDate <- state.date[2] + one.period
        task$MainEndDate <- config.date[2]
        if (config.date[1] > state.date[2]) {
          cli_alert_warning("Config and state ranges do not overlap.")
          cli_alert_info("Intermediate data will also be collected.")
        }

      } else { # Starts earlier

        # Set the task date to the state date and warn the user if necessary
        task$MainStartDate <- config.date[1]
        task$MainEndDate <- state.date[1] - one.period
        if (config.date[2] < state.date[1]) {
          cli_alert_warning("Config and state ranges do not overlap.")
          cli_alert_info("Intermediate data will also be collected.")
        }

      }
    }

    # Define dataset's post-state after a successful (re)build/update
    post.state <- list(
      DateRange = c(min(config.date[1], state.date[1]),
                    max(config.date[2], state.date[2])),
      SelectedCountries = unique(c(unlist(state$SelectedCountries),
                                   unlist(config$SelectedCountries))),
      SelectedFrequency = unlist(state$SelectedFrequency),
      SelectedDatasets = unique(c(unlist(state$SelectedDatasets),
                                  unlist(config$SelectedDatasets)))
    )

    task$MainSession <- main.session
  }

  # ----------------------------------------------------------------------------
  # S5: Definition of the supplementary session
  # ----------------------------------------------------------------------------

  # Check if "*" is present in either vector
  tagged.countries <- any(grepl("\\*", task$MainCountries))
  tagged.datasets <- any(grepl("\\*", task$MainDatasets))

  # Define a new object based on the conditions
  task$SuppSession <- ifelse(tagged.countries & tagged.datasets, 4L,
                        ifelse(tagged.datasets, 3L,
                          ifelse(tagged.countries, 2L, 1L)))

  # 'SuppSession' values:
  #   1: No tagged value
  #   2: At least one tagged country
  #   3: At least one tagged dataset
  #   4: At least one tagged country and dataset

  # Stop if no task is implemented
  if (task$SuppSession == 1 && !main.session) {
    stop("No task is implied by the configuration file. Stopping.")
  }

  # Set up date range and parameters for "*" elements
  if (task$SuppSession > 1L) {
    task$SuppDateRange <- unlist(state$DateRange)

    if (task$SuppSession == 2L) {
      tagged.countries <- grep("\\*", unlist(task$MainCountries),
                               value = TRUE)
      task$SuppCountries <- sub("\\*", "", tagged.countries)
      task$SuppDatasets <- unlist(task$MainDatasets)
    }

    if (task$SuppSession == 3L) {
      tagged.datasets <- grep("\\*", unlist(task$MainDatasets),
                              value = TRUE)
      task$SuppDatasets <- list(sub("\\*", "", tagged.datasets))
      task$SuppCountries <- task$MainCountries
    }

    if (task$SuppSession == 4L) {

      # Collect all datasets for tagged countries
      tagged.countries <- grep("\\*", unlist(task$MainCountries),
                               value = TRUE)
      task$SuppCountries[[1]] <- sub("\\*", "", tagged.countries)
      task$SuppDatasets[[1]] <- sub("\\*", "", unlist(task$MainDatasets))

      # Collect tagged datasets for every other country
      tagged.datasets <- grep("\\*", unlist(task$MainDatasets),
                              value = TRUE)
      task$SuppDatasets[[2]] <- sub("\\*", "", tagged.datasets)
      task$SuppCountries[[2]] <- setdiff(unlist(task$MainCountries),
                                           tagged.countries)
    }
  }

  task <- task[c(1, 6, 4:5, 2:3, 7:10)]
  return(list(task = task, post.state = post.state))
}
