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
             "Use 'reticulate::conda_list()$name' to retrieve available Conda ",
             "environments."))
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

      # Check each statistic (stats) in the band
      for (stat in band) {
        if (!is.character(stat) || stat == "") {
          stop(paste0("Invalid statistic '", stat, "' in band '", band_name,
                      "' of dataset '", dataset_name,
                      "'.\nIt must be a non-empty character string."))
        }
      }
    }
  }

  return(TRUE)
}

# ------------------------------------------------------------------------------

#' Validate Tables Parameter
#'
#' Validates the 'tables' parameter and determines which tables to read.
#' @param tables [mandatory] (character or integer) A vector specifying tables
#' to be read.
#' @param tables_all [mandatory] (data.frame) All available tables.
#' @param freq [mandatory] (character) Specifies the frequency for aggregation
#' (options: \code{NULL}, \code{"month"}, \code{"year"}).
#' @param freq_stats [optional] (character) A character vector of statistical
#' functions aggregation is based on (options: \code{NULL}, \code{"mean"},
#' \code{"median"}, \code{"min"}, \code{"max"}, \code{"sd"}).
#' @return A character vector of valid table names.
#' @keywords internal
#'
validate_tables_param <- function(tables, tables_all, freq, freq_stats) {
  
  # Define valid options for 'freq' and 'freq_stats'
  valid_freq <- c(NULL, "month", "year")
  valid_freq_stats <- c(NULL, "mean", "median", "min", "max", "sd")
  
  # Check if only one of 'freq' or 'freq_stats' is specified
  if (is.null(freq) != is.null(freq_stats)) {
    stop("Both 'freq' and 'freq_stats' must either be both NULL or both specified.")
  }
  
  # Validate 'freq' if it is not NULL
  if (!is.null(freq)) {
    if (!(freq %in% valid_freq)) {
      stop("Invalid 'freq' parameter.\n",
           "Valid options are ",
           paste(sprintf("'%s'", valid_freq[!is.null(valid_freq)]),
                 collapse = ", "), ".")
    }
  }
  
  # Validate 'freq_stats' if it is not NULL
  if (!is.null(freq_stats)) {
    if (!all(freq_stats %in% valid_freq_stats)) {
      stop("Invalid 'freq_stats' parameter.\n",
           "Valid options are ",
           paste(sprintf("'%s'", valid_freq_stats[!is.null(valid_freq_stats)]),
                 collapse = ", "), ".")
    }
  }
  
  if (any(tables == "all")) {
    tables <- tables_all$name
  } else if (is.numeric(tables)) {
    tables <- tables_all$name[tables]
    tables <- tables[!is.na(tables)]
  } else if (is.character(tables)) {
    tables <- intersect(tables, tables_all$name)
  } else {
    tables <- NULL
  }
  
  if (length(tables) == 0) {
    stop("Invalid 'tables' parameter.\n",
         "Use 'fetch_tables' to retrieve valid table names.")
  }
  
  if (!is.null(freq_stats)) {
    if (!all(freq_stats %in% valid_freq_stats)) {
      stop("Invalid 'freq_stats' parameter. Valid entries are ",
           paste(valid_freq_stats, collapse = ", "), ".")
    }
  }
  
  return(tables)
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
