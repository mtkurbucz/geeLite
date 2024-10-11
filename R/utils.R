# Internal Functions -----------------------------------------------------------

#' Validate Parameters
#'
#' Validates multiple parameters.
#' @details Validations performed:
#' - 'admin_lvl': Validates if it is \code{NULL}, \code{0}, or \code{1}.
#' - 'conda': Checks if it is an available Conda environment.
#' - 'file_path': Constructs a file path and checks if the file exists.
#' - 'keys': Ensures it is a non-empty list with valid entries.
#' - 'limit': Checks if it is a positive numeric value.
#' - 'new_values': Verifies it is a list with the same length as 'keys'.
#' - 'user': Checks if it is \code{NULL} or a character value.
#' - 'path': Checks if the directory exists.
#' - 'rebuild': Checks if it is a logical value.
#' - 'regions': Checks if the first two characters are letters.
#' - 'start': Checks if it is a valid date.
#' - 'verbose': Checks if it is a logical value.
#' @param params [mandatory] (list) Parameters to be validated.
#' @return Invisible \code{NULL} if all validations pass.
#' @keywords internal
#' @importFrom reticulate conda_list
#'
validate_params <- function(params) {

  for (name in names(params)) {
    value <- params[[name]]

    if (name == "admin_lvl") {

      if (!is.null(value) && !value %in% c(0, 1)) {
        stop("Invalid 'admin_lvl' parameter.\n",
             "Valid entries are 0, 1, or NULL.")
      }

    } else if (name == "conda") {

      if (!any(value %in% conda_list()$name)) {
        stop(paste0("Invalid 'conda' parameter.\n",
                    "Use 'reticulate::conda_list()$name' to retrieve ",
                    "available Conda environments."))
      }

    } else if (name == "file_path") {

      file_path <- file.path(params[["path"]], value)
      if (!file.exists(file_path)) {
        if (value == "config/config.json") {
          stop(sprintf("File not found: %s", value),
               "\nUse 'set_config' to generate the configuration file.")
        } else {
          stop(sprintf("File not found: %s", value),
               paste0("\nUse 'run_geelite' to generate the database and its ",
                      "supplementary files."))
        }
      }

    } else if (name == "keys") {

      if (!is.list(value) || length(value) == 0) {
        stop("Invalid 'key' parameter.\n",
             "It must be a non-empty list.")
      }
      valid_keys <- c("regions", "source", "limit")
      invalid_keys <- setdiff(as.character(map(value, 1)), valid_keys)
      if(length(invalid_keys) > 0) {
        stop(sprintf("Invalid 'keys' specified: %s", invalid_keys))
      }

    } else if (name == "limit") {

      if (!is.numeric(value) || value <= 0) {
        stop("Invalid 'limit' parameter.\n",
             "It must be a positive integer.")
      }

    } else if (name == "new_values") {

      if (!is.list(value) || length(value) != length(value)) {
        stop("Invalid 'new_values' parameter.\n",
             "It must be a list with the same length as 'keys'.")
      }

    } else if (name == "user") {

      if (!is.null(value) && !is.character(value))  {
        stop("Invalid 'user' parameter.\n",
             "It must be a character value or NULL.")
      }

    } else if (name == "path") {

      if (!dir.exists(value)) {
        stop(sprintf("Directory not found: %s", value))
      }

    } else if (name == "rebuild") {

      if (!is.logical(value)) {
        stop("Invalid 'rebuild' parameter.\n",
             "It must be a logical value.")
      }

    } else if (name == "regions") {

      first_two_chars <- substr(value, 1, 2)
      if (any(!grepl("[A-Za-z]", first_two_chars))) {
        stop("Invalid 'regions' parameter.\n",
             "Use 'fetch_regions' to retrieve valid region codes.")
      }

    } else if (name == "source") {

      validate_source_param(value)

    } else if (name == "start") {

      tryCatch({
        as.Date(value)
      }, error = function(err) {
        stop(paste0("Invalid 'start' parameter.\n'", value,
                    "' is not a valid date."))
      })

    } else if (name == "verbose") {

      if (!is.logical(value)) {
        stop("Invalid 'verbose' parameter.\n",
             "It must be a logical value.")
      }

    }
  }

  invisible(NULL)
}

# ------------------------------------------------------------------------------

#' Validate Source Parameter
#'
#' Checks the validity of the 'source' parameter.
#' @param source A list containing datasets and their associated bands and
#' statistics. It should follow a specific structure where each dataset is a
#' named list, each band within a dataset is also a named list, and each
#' statistic within a band is a non-empty character string.
#' @return \code{TRUE} if the 'source' parameter is valid.
#' @keywords internal
#'
validate_source_param <- function(source) {

  # Check if source is a non-empty list
  if (!is.list(source) || length(source) == 0) {
    stop("Invalid 'source' parameter.\n",
         "It must be a non-empty list.")
  }

  # Check each dataset
  for (dataset_name in names(source)) {
    if (!is.character(dataset_name)) {
      stop("Invalid 'source' parameter.\n",
           "Dataset name must be a character string.")
    }
    dataset <- source[[dataset_name]]
    if (length(dataset) == 0) {
      stop(paste0("Invalid 'source' parameter.\n",
                  "Dataset '", dataset_name, "' cannot be empty."))
    }

    # Check each band in the dataset
    for (band_name in names(dataset)) {
      if (!is.character(band_name)) {
        stop(paste0("Invalid band name '", band_name, "' in dataset '",
                    dataset_name, "'.\nBand name must be a character string."))
      }
      band <- dataset[[band_name]]
      if (!is.character(band) || length(band) == 0) {
        stop(paste0("Invalid band '", band_name, "' in dataset '", dataset_name,
                    "'.\nIt must contain non-empty character vectors."))
      }

      # Check each statistic (zonal_stats) in the band
      for (zonal_stat in band) {
        if (!is.character(zonal_stat) || zonal_stat == "") {
          stop(paste0("Invalid statistic '", zonal_stat, "' in band '",
                      band_name, "' of dataset '", dataset_name,
                      "'.\nIt must be a non-empty character string."))
        }
      }
    }
  }

  return(TRUE)
}

# ------------------------------------------------------------------------------

#' Validate and Process Parameters for Variable Selection and Data Processing
#'
#' Validates and processes the input parameters related to variable selection
#' and data processing in the \code{\link{read_db}} function. It ensures that
#' the variables, frequency, and functions provided are valid, correctly
#' formatted, and compatible with the available data.
#' @param variables [mandatory] (character or integer) Variable IDs or names to
#' be processed. Use \code{\link{fetch_vars}} to obtain valid variable names or
#' IDs. Accepts \code{"all"} to select all available variables.
#' @param variables_all [mandatory] (data.frame) Data frame containing all
#' available variables, typically obtained from \code{\link{fetch_vars}}.
#' @param prep_fun [mandatory] (function) Function used for pre-processing.
#' @param aggr_funs [mandatory] (function or list) Aggregation function(s).
#' @param postp_funs [mandatory] (function or list) Post-processing function(s).
#' @return A character vector of variable names to process.
#' @keywords internal
#'
validate_variables_param <- function(variables, variables_all, prep_fun,
                                     aggr_funs, postp_funs) {

  # Validate 'prep_fun': it should be a function
  if (!is.function(prep_fun)) {
    stop("Invalid 'prep_fun' parameter.\n",
         "It must be a function.")
  }

  # Append 'default' to the list of valid variable names
  valid_names <- append(variables_all$Variable, "default")

  # Validate 'aggr_funs'
  if (!is.list(aggr_funs)) {
    stop("Invalid 'aggr_funs' parameter.\n",
         "It must be a function or a list of functions.")
  }

  if (!all(names(aggr_funs) %in% valid_names)){
    stop("Invalid 'aggr_funs' parameter.\n",
         "Use 'fetch_vars' to obtain valid variable names.")
  }

  # Validate 'postp_funs'
  if (!is.list(postp_funs)) {
    stop("Invalid 'postp_funs' parameter.\n",
         "It must be a function or a list of functions.")
  }

  if (!all(sapply(names(postp_funs),
                  function(x) any(startsWith(x, valid_names))))) {
    stop("Invalid 'postp_funs' parameter.\n",
         "Use 'fetch_vars' to obtain valid variable names. If needed, refer to
         'aggr_funs' by their specified index.")
  }

  if (is.character(variables) && length(variables) == 1 && variables == "all") {
    variables <- variables_all$Variable
  } else {
    # variables can be IDs or Variable names
    if (is.numeric(variables)) {
      # IDs
      if (!all(variables %in% variables_all$ID)) {
        stop("Some variable IDs not found in the database.")
      }
      variables <- variables_all$Variable[
        match(variables, variables_all$ID)]
    } else if (is.character(variables)) {
      if (!all(variables %in% variables_all$Variable)) {
        stop("Some variable names not found in the database.")
      }
    } else {
      stop("Variables parameter should be 'all', variable IDs, or names.")
    }
  }
  return(variables)
}

# ------------------------------------------------------------------------------

#' Attempt to Connect to an SQLite Database with Retries
#'
#' This function tries to connect to an SQLite database using
#' \code{dbConnect()}. If the initial connection fails, it retries up to
#' \code{max_retries} times, waiting \code{wait_time} seconds between each
#' attempt. If the connection cannot be established after the maximum retries,
#' the function stops and throws an error.
#' @param db_path A string specifying the file path to the SQLite database.
#' @param max_retries An integer specifying the maximum number of retries if
#' the connection fails (default: \code{3}).
#' @param wait_time A numeric value indicating the number of seconds to wait
#' between retries (default: \code{5}).
#' @return A database connection object if the connection is successful.
#' @keywords internal
#' @importFrom RSQLite dbConnect SQLite
#'
db_connect <- function(db_path, max_retries = 3, wait_time = 5) {
  for (i in 1:max_retries) {
    con <- tryCatch({
      dbConnect(SQLite(), dbname = db_path)
    }, error = function(e) {
      message("Failed to connect on attempt ", i, ": ", e$message)
      return(NULL)
    })

    if (!is.null(con)) {
      return(con)  # Connection successful
    }

    # If connection failed, wait before retrying
    Sys.sleep(wait_time)
  }

  # If all retries failed, stop and report the issue
  stop("Failed to connect to the database after ", max_retries,
       " attempts. Check the file path or server load.")
}

# ------------------------------------------------------------------------------

#' Output Message
#'
#' Outputs message if verbose mode is \code{TRUE}.
#' @param message [mandatory] (list) The message to be displayed.
#' @param verbose [mandatory] (logical) Flag indicating whether to display the
#' message.
#' @keywords internal
#' @importFrom cli cli_alert_info
#'
output_message <- function(message, verbose) {
  if (verbose) {
    cat("\n")
    for(i in seq_along(message)){
      cli_alert_info(message[[i]])
    }
    cat("\n")
  }
}
