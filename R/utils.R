#' @title Process a Call Expression and Evaluate Arguments
#'
#' @description This function takes a call expression and evaluates its
#' arguments, ensuring symbols are evaluated to their values, lists are
#' preserved, and language objects are evaluated in the calling environment.
#'
#' @param call [mandatory] (list) The call expression captured using
#' match.call().
#'
#' @return A list of evaluated parameters.
#'
#' @keywords internal
#'
generate_params <- function(call) {

  params <- lapply(call[-1], function(arg) {
    if (is.symbol(arg)) {
      eval(arg, envir = parent.frame())
    } else if (is.list(arg)) {
      arg
    } else if (is.language(arg)) {
      eval(arg, envir = parent.frame())
    } else {
      arg
    }
  })

  return(params)
}

# ------------------------------------------------------------------------------

#' @title Validate Parameters
#'
#' @description This function validates parameters.
#'
#' @param params [mandatory] (list) A list containing parameters to validate.
#'
#' @details Validations performed:
#' - 'admin_lvl': Validates if it is NULL, 0, or 1.
#' - 'crs': Checks if it can be recognized by the \code{sf} package.
#' - 'file_path': Constructs a file path and checks if the file exists.
#' - 'keys': Ensures it is a non-empty list with valid entries.
#' - 'limit': Checks if it is a positive numeric value.
#' - 'new_values': Verifies it is a list with the same length as 'keys'.
#' - 'path': Checks if the directory exists.
#' - 'regions': Checks if the first two characters are letters.
#' - 'start': Checks if it is a valid date.
#' - 'verbose': Checks if it is a logical value.
#'
#' @return Invisible NULL if all validations pass.
#'
#' @keywords internal
#'
#' @importFrom sf st_crs
#'
validate_params <- function(params) {

  for (name in names(params)) {

    value <- params[[name]]

    if (name == "admin_lvl") {

      if (!is.null(value) && !value %in% c(0, 1)) {
        stop("Invalid 'admin_lvl' parameter. Valid entries are 0, 1, or NULL.")
      }

    } else if (name == "crs") {

      crs_obj <- tryCatch({
        st_crs(value)
      }, warning = function(warn) {
        stop("Invalid 'crs' parameter.")
      }, error = function(err) {
        stop("Invalid 'crs' parameter.")
      })

    } else if (name == "file_path") {

      file_path <- file.path(params[["path"]], value)
      if (!file.exists(file_path)) {
        stop(sprintf("File not found: %s", value))
      }

    } else if (name == "keys") {

      if (!is.list(value) || length(value) == 0) {
        stop("'keys' must be a non-empty list.")
      }
      valid_keys <- c("regions", "source", "limit")
      invalid_keys <- setdiff(as.character(map(value, 1)), valid_keys)
      if(length(invalid_keys) > 0) {
        stop(sprintf("Invalid keys specified: %s", invalid_keys))
      }

    } else if (name == "limit") {

      if (!is.numeric(value) || value <= 0) {
        stop("'limit' must be a positive numeric value.")
      }

    } else if (name == "new_values") {

      if (!is.list(value) || length(value) != length(value)) {
        stop("'new_values' must be a list with the same length as 'keys'.")
      }

    } else if (name == "path") {

      if (!dir.exists(value)) {
        stop(sprintf("Directory not found: %s", value))
      }

    } else if (name == "regions") {

      first_two_chars <- substr(value, 1, 2)
      if (any(!grepl("[A-Za-z]", first_two_chars))) {
        stop("Invalid 'regions' parameter.")
      }

    } else if (name == "source") {

      validate_source_param(value)

    } else if (name == "start") {

      tryCatch({
        as.Date(value)
      }, error = function(err) {
        stop(paste0("Invalid 'start' parameter. '", value,
                    "' is not a valid date."))
      })

    } else if (name == "verbose") {

      if (!is.logical(value)) {
        stop("'verbose' must be a logical value.")
      }

    }
  }

  invisible(NULL)
}

# ------------------------------------------------------------------------------

#' @title Validate Source Parameter
#'
#' @description Checks the validity of the 'source' parameter structure.
#'
#' @param source A list containing datasets and their associated bands and
#' statistics. It should follow a specific structure where each dataset is a
#' named list, each band within a dataset is also a named list, and each
#' statistic within a band is a non-empty character string.
#'
#' @return TRUE if the 'source' parameter is valid.
#'
validate_source_param <- function(source) {

  # Check if source is a list
  if (!is.list(source)) {
    stop("Invalid 'source' parameter. It must be a list.")
  }

  if (length(source) == 0) {
    stop("Invalid 'source' parameter. It cannot be empty.")
  }

  # Check each dataset
  for (dataset_name in names(source)) {
    if (!is.character(dataset_name)) {
      stop("Invalid dataset name. Dataset name must be a character string.")
    }
    dataset <- source[[dataset_name]]
    if (length(dataset) == 0) {
      stop(paste0("Invalid dataset '", dataset_name, "'. It cannot be empty."))
    }

    # Check each band in the dataset
    for (band_name in names(dataset)) {
      if (!is.character(band_name)) {
        stop(paste0("Invalid band name '", band_name, "' in dataset '",
                    dataset_name, "'. Band name must be a character string."))
      }
      band <- dataset[[band_name]]
      if (!is.character(band) || length(band) == 0) {
        stop(paste0("Invalid band '", band_name, "' in dataset '", dataset_name,
                    "'. It must contain non-empty character vectors."))
      }

      # Check each statistic (stats) in the band
      for (stat in band) {
        if (!is.character(stat) || stat == "") {
          stop(paste0("Invalid statistic '", stat, "' in band '", band_name,
                      "' of dataset '", dataset_name,
                      "'. It must be a non-empty character string."))
        }
      }
    }
  }

  # If all checks pass
  return(TRUE)
}

# ------------------------------------------------------------------------------

#' @title Output CLI Information
#'
#' @description Outputs a message if verbose mode is enabled.
#'
#' @param message [mandatory] (character) The message to be displayed.
#'
#' @param verbose [mandatory] (logical) Flag indicating whether to display the
#' message.
#'
#' @keywords internal
#'
#' @importFrom cli cli_alert_info
#'
output_info <- function(message, verbose) {
  if (verbose) {
    cat("\n")
    cli_alert_info(message)
    cat("\n")
  }
}
