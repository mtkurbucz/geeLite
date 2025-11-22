# Internal Functions -----------------------------------------------------------

#' Validate Parameters
#'
#' Validates multiple parameters.
#' @details The following validations are performed:
#' - 'admin_lvl': Ensures it is \code{NULL}, \code{0}, or \code{1}.
#' - 'conda': Verifies if it is an available Conda environment.
#' - 'file_path': Constructs a file path and checks if the file exists.
#' - 'keys': Ensures it is a non-empty list with valid entries.
#' - 'limit': Ensures it is a positive numeric value.
#' - 'mode': Ensures it is 'local' or 'drive'.
#' - 'new_values': Ensures it is a list with the same length as 'keys'.
#' - 'user': Verifies it is \code{NULL} or a character value.
#' - 'path': Verifies if the directory exists.
#' - 'rebuild': Verifies it is a logical value.
#' - 'regions': Ensures the first two characters are letters.
#' - 'start': Ensures it is a valid date.
#' - 'verbose': Verifies it is a logical value.
#' @param params [mandatory] (list) A list of parameters to be validated.
#' @return Returns \code{NULL} invisibly if all validations pass.
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

    } else if (name == "mode") {

      if (!value %in% c("local", "drive")) {
        stop("Invalid 'mode' parameter.\n",
             "It must be either 'local' or 'drive'.")
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

      if (file.access(value, 2) != 0) {
        stop(sprintf("Directory is not writable: %s", value))
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
#' @param source [mandatory] (list) A list containing datasets, each with its
#'   associated bands and statistics. The structure should follow a nested
#'   format where each dataset is a named list, each band within a dataset is
#'   also a named list, and each statistic within a band is a non-empty
#'   character string.
#' @return Returns \code{TRUE} if the 'source' parameter is valid. Throws an
#'   error if the parameter is invalid.
#' @keywords internal
#'
validate_source_param <- function(source) {

  # Check if 'source' is a non-empty list
  if (!is.list(source) || length(source) == 0) {
    stop("Invalid 'source' parameter.\n",
         "It must be a non-empty list.")
  }

  # Check each dataset in 'source'
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

    # Check each band within the dataset
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

      # Check each statistic (zonal_stat) within the band
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
#' Validates and processes input parameters related to variable selection and
#' data processing in the \code{\link{read_db}} function. It ensures that the
#' variables, frequency, and functions provided are valid, correctly formatted,
#' and compatible with the available data.
#' @param variables [mandatory] (character or integer) Variable IDs or names to
#'   be processed. Use \code{\link{fetch_vars}} to obtain valid variable names
#'   or IDs. Accepts \code{"all"} to select all available variables.
#' @param variables_all [mandatory] (data.frame) A data frame containing all
#'   available variables, typically obtained from \code{\link{fetch_vars}}.
#' @param prep_fun [mandatory] (function) A function used for pre-processing.
#' @param aggr_funs [mandatory] (function or list) Aggregation function(s).
#' @param postp_funs [mandatory] (function or list) Post-processing function(s).
#' @return A character vector of variable names to process.
#' @keywords internal
#'
validate_variables_param <- function(variables, variables_all, prep_fun,
                                     aggr_funs, postp_funs) {

  # Validate 'prep_fun': it must be a function
  if (!is.function(prep_fun)) {
    stop("Invalid 'prep_fun' parameter.\n",
         "It must be a function.")
  }

  # Append 'default' to the list of valid variable names
  valid_names <- append(variables_all$Variable, "default")

  # Validate 'aggr_funs': must be a list of valid functions or a function
  if (!is.list(aggr_funs)) {
    stop("Invalid 'aggr_funs' parameter.\n",
         "It must be a function or a list of functions.")
  }

  # Ensure that all names in 'aggr_funs' are valid variable names
  if (!all(names(aggr_funs) %in% valid_names)){
    stop("Invalid 'aggr_funs' parameter.\n",
         "Use 'fetch_vars' to obtain valid variable names.")
  }

  # Validate 'postp_funs': must be a list of valid functions or a function
  if (!is.list(postp_funs)) {
    stop("Invalid 'postp_funs' parameter.\n",
         "It must be a function or a list of functions.")
  }

  # Ensure that all names in 'postp_funs' start with valid variable names
  if (!all(sapply(names(postp_funs),
                  function(x) any(startsWith(x, valid_names))))) {
    stop("Invalid 'postp_funs' parameter.\n",
         "Use 'fetch_vars' to obtain valid variable names. If needed, refer to
         'aggr_funs' by their specified index.")
  }

  # If 'variables' is "all", select all available variables
  if (is.character(variables) && length(variables) == 1 && variables == "all") {
    variables <- variables_all$Variable
  } else {
    # Validate that 'variables' are IDs or variable names
    if (is.numeric(variables)) {
      # Validate variable IDs
      if (!all(variables %in% variables_all$ID)) {
        stop("Some variable IDs not found in the database.")
      }
      # Convert IDs to variable names
      variables <- variables_all$Variable[
        match(variables, variables_all$ID)]
    } else if (is.character(variables)) {
      # Validate variable names
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
#' Tries to connect to an SQLite database using \code{dbConnect()}. If the
#' initial connection fails, it retries up to \code{max_retries} times, waiting
#' \code{wait_time} seconds between each attempt. If the connection cannot be
#' established after the maximum retries, the function stops and throws an
#' error.
#' @param db_path [mandatory] (character) A string specifying the file path to
#'   the SQLite database.
#' @param max_retries [mandatory] (integer) The maximum number of retries if
#'   the connection fails (default: \code{3}).
#' @param wait_time [mandatory] (numeric) The number of seconds to wait between
#'   retries (default: \code{5}).
#' @return A database connection object if the connection is successful.
#' @keywords internal
#' @importFrom RSQLite dbConnect SQLite
#'
db_connect <- function(db_path, max_retries = 3, wait_time = 5) {

  # Attempt to connect to the database with retries
  for (i in 1:max_retries) {
    con <- tryCatch({
      dbConnect(SQLite(), dbname = db_path)
    }, error = function(e) {
      # If connection fails, print the error message and retry
      message("Failed to connect on attempt ", i, ": ", e$message)
      return(NULL)
    })

    # If connection is successful, return it
    if (!is.null(con)) {
      return(con)  # Connection successful
    }

    # Wait for 'wait_time' seconds before retrying
    Sys.sleep(wait_time)
  }

  # If all retries fail, stop and report the issue
  stop("Failed to connect to the database after ", max_retries,
       " attempts. Check the file path or server load.")
}

# ------------------------------------------------------------------------------

#' Clean Contents or Entire Google Drive Folders by Name
#'
#' Searches for all Google Drive folders with the specified name and optionally
#' removes their contents and/or the folders themselves. Useful for cleaning up
#' scratch or export folders used by Earth Engine batch processes.
#' @param folder_name [mandatory] (character) Name of the folder(s) to search
#' for in Google Drive.
#' @param delete_folders [optional] (logical) If \code{TRUE}, the matched
#' folders themselves will be deleted after their contents are removed.
#' Default is \code{FALSE}.
#' @param verbose [optional] (logical) If \code{TRUE}, messages will be printed
#' about cleanup actions. Default is \code{TRUE}.
#' @keywords internal
#' @importFrom googledrive drive_find drive_ls drive_rm as_id
#'
clean_drive_folders_by_name <- function(folder_name,
                                        delete_folders = FALSE,
                                        verbose = TRUE) {

  # If folder_name is an empty string, skip the folder deletion part
  if (folder_name == "") {
    delete_folders <- FALSE
  }

  folders <- googledrive::drive_find(
    q = sprintf(
      "name = '%s' and mimeType = 'application/vnd.google-apps.folder'",
      folder_name
    )
  )

  if (nrow(folders) == 0) {
    if (verbose) {
      message(sprintf("No folder named '%s' found.", folder_name))
    }
    return()
  }

  for (i in seq_len(nrow(folders))) {
    folder_id <- googledrive::as_id(folders$id[i])
    files <- suppressMessages(googledrive::drive_ls(path = folder_id))

    if (nrow(files) > 0) {
      # Pattern: export_YYYY_MM_DD_HH_MM_SS_<digits>_YYYY_MM_DD_HH_MM_SS.csv
      valid_pattern <- paste0(
        "^export_\\d{4}_\\d{2}_\\d{2}_\\d{2}_\\d{2}_\\d{2}_\\d+_",
        "\\d{4}_\\d{2}_\\d{2}_\\d{2}_\\d{2}_\\d{2}\\.csv$"
      )
      invalid_files <- files[!grepl(valid_pattern, files$name), ]

      if (nrow(invalid_files) > 0) {
        stop(
          sprintf(
            paste0(
              "Folder '%s' contains unexpected files that do not match\n",
              "the geeLite export pattern. These may be user uploads:\n\n",
              "- %s\n\n",
              "To prevent accidental data loss, geeLite will not proceed.\n\n",
              "Please delete or relocate these files manually from Google ",
              "Drive.\n\n",
              "You can access the folder here:\n",
              "https://drive.google.com/drive/my-drive/%s"
            ),
            folder_name,
            paste(invalid_files$name, collapse = "\n- "),
            folder_name
          )
        )
      }

      suppressMessages(googledrive::drive_rm(files))
      if (verbose) {
        message(sprintf("Cleaned %d files from folder: %s",
                        nrow(files), folder_name))
      }
    }
  }

  if (delete_folders) {
    suppressMessages(googledrive::drive_rm(folders))
    if (verbose) {
      message(sprintf("Deleted %d folders named: %s",
                      nrow(folders), folder_name))
    }
  }
}

# ------------------------------------------------------------------------------

#' Output Message
#'
#' Outputs a message if verbose mode is \code{TRUE}.
#' @param message [mandatory] (list) The message to display.
#' @param verbose [mandatory] (logical) A flag indicating whether to display
#'   the message.
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

# ------------------------------------------------------------------------------

#' Check Google Earth Engine connection
#'
#' Returns \code{TRUE} if the user is authenticated with GEE via `rgee`, without
#' triggering interactive prompts. Useful in non-interactive contexts like
#' CRAN. Prints a message and returns \code{FALSE} if not.
#' @return A logical value: \code{TRUE} if authenticated with GEE, \code{FALSE}
#' otherwise (invisibly).
#' @keywords internal
#' @importFrom rgee ee_check_credentials
#'
check_rgee_ready <- function() {
  is_ready <- tryCatch({
    rgee::ee_check_credentials(quiet = TRUE)
  }, error = function(e) FALSE)
  if (!is_ready) {
    message(
      "\nGoogle Earth Engine is not connected.\n",
      "After installing with geeLite::gee_install(),\n",
      "run geeLite::set_depend() to authenticate.\n"
    )
    return(FALSE)
  }
  invisible(TRUE)
}

# ------------------------------------------------------------------------------

#' Internal Dummy Function for Declared Imports
#'
#' Ensures CRAN recognizes packages listed in `Imports:` that are indirectly
#' required but not explicitly used. Never called at runtime and has no side
#' effects.
#' @return NULL (invisible)
#' @keywords internal
#' @import rnaturalearthdata
#' @importFrom geojsonio geojson_list
#'
dummy_use_for_cran <- function() {
  if (FALSE) {
    invisible(geojson_list(list(type = "FeatureCollection")))
    invisible(loadNamespace("rnaturalearthdata"))
  }
  invisible(NULL)
}
