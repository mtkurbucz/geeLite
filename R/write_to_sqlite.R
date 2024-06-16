#' @title Writing Data to SQLite Database
#'
#' @description This function writes data to an SQLite database and creates it
#' if it does not exist. It connects to the specified SQLite database file,
#' creates a new table if it does not exist, and appends new values to the
#' existing table if it does.
#'
#' @param data [Mandatory] The data to be written to the SQLite database.
#'
#' @param table.name [Mandatory] The name of the table to write the data into.
#'
#' @param operation.code [Mandatory] For updating, use \code{1}, and for
#' (re)building, use \code{2}. If not provided, the user is prompted to enter a
#' path.
#'
#' @param target.path [Optional] Path to the directory. If not provided, the
#' user is prompted to enter a path. Default value is \code{NULL}.
#'
#' @export
#'
#' @examples
#' # Example: Update 'my_table' in 'my.data' SQLite database
#' \dontrun{
#' WriteToSQLite(data = my.data, table.name = "my_table", operation.code = 1)
#' }
#'
#' @importFrom sf st_write
#' @importFrom RSQLite SQLite
#' @importFrom cli cli_alert_info
#'
WriteToSQLite <- function(data, table.name, operation.code,
                          target.path = NULL) {

  # ----------------------------------------------------------------------------
  # S1: Parameter test
  # ----------------------------------------------------------------------------

  # If 'target.path' is not provided or is invalid, prompt the user
  if (is.null(target.path) || !file.exists(target.path)) {
    target.path <- GeneratePath()
  }

  setwd(target.path)

  # ----------------------------------------------------------------------------
  # S2: Write data to SQLite database
  # ----------------------------------------------------------------------------

  dbPath <- "data/GFIData.db"

  # Check if the SQLite database file exists
  if (operation.code == 1L){

    # If the table exists, append new values as new rows
    st_write(obj = data, dsn = dbPath, layer = table.name, driver = "SQLite",
             append = TRUE, quiet = TRUE)

  } else {

    # If the database file doesn't exist, create a new one
    st_write(obj = data, dsn = dbPath, layer = table.name, driver = "SQLite",
             delete_dsn = TRUE, quiet = TRUE)
  }

  cat("\n")
  cli_alert_info("Data saved successfully. Check 'data/GFIData.db'.")
}
