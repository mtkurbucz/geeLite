#' @title Process a Call Expression and Evaluate Arguments
#'
#' @description This function takes a call expression and evaluates its
#' arguments, ensuring symbols are evaluated to their values, lists are
#' preserved, and language objects are evaluated in the calling environment.
#'
#' @param call [mandatory] (list) The call expression captured using
#' match.call().
#' @return A list of evaluated parameters.
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
#' @details
#' Validations performed:
#' - 'path': Checks if the directory exists.
#' - 'file_path': Constructs a file path and checks if the file exists.
#' - 'keys': Ensures it is a non-empty list with valid entries.
#' - 'new_values': Verifies it is a list with the same length as 'keys'.
#' - 'verbose': Checks if it is a logical value.
#' @return Invisible NULL if all validations pass.
#'
validate_params <- function(params) {

  for (name in names(params)) {

    value <- params[[name]]

    if (name == "path") {

      if (!dir.exists(value)) {
        stop(sprintf("Directory not found: %s", value))
      }

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

    } else if (name == "new_values") {

      if (!is.list(value) || length(value) != length(value)) {
        stop("'new_values' must be a list with the same length as 'keys'.")
      }

    } else if (name == "verbose") {

      if (!is.logical(value)) {
        stop("'verbose' must be a logical value.")
      }

    }
  }

  invisible(NULL)
}
