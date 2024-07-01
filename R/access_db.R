# Main Functions ---------------------------------------------------------------

#' @title Retrieve and Print SQLite Table Names
#'
#' @description This function retrieves and prints the names of the available
#' SQLite tables in the specified database.
#'
#' @param path [mandatory] (character) Path to the root directory of the
#' generated database.
#'
#' @return A data frame containing the table names.
#'
#' @export
#'
#' @examples
#' # Example: Printing the available SQLite tables
#' \dontrun{
#'   fetch_tables(path = "path/to/root/directory")
#' }
#'
#' @importFrom RSQLite dbConnect dbDisconnect dbListTables
#'
fetch_tables <- function(path) {

  # Validate parameters
  params <- list(path = path, file_path = "data/geelite.db")
  validate_params(params)

  # Retrieve the list of tables
  db_path <- file.path(path, "data/geelite.db")
  con <- dbConnect(SQLite(), dbname = db_path)
  tables <- dbListTables(conn = con)
  dbDisconnect(con)

  # Filter out system tables and sort the table names
  tables <- filter_and_sort_tables(tables)

  return(tables)
}

#' @title Read Selected SQLite Tables
#'
#' @description Reads specified SQLite tables into a \code{list} object.
#'
#' @param path [mandatory] (character) The path to the root directory of the
#' generated database.
#'
#' @param tables [optional] (character or integer) The names or IDs of the
#' tables to be read. Use the \code{fetch_tables} function to identify available
#' table names and IDs. Defaults to \code{"all"}.
#'
#' @return A list where the first element (grid) is an \code{sf} object, and
#' subsequent elements are \code{data.frame} objects.
#'
#' @export
#'
#' @examples
#' # Example: Reading the "grid" table
#' \dontrun{
#'   db_list <- read_db(path = "path/to/root/directory",
#'                      tables = "grid")
#' }
#'
read_db <- function(path, tables = "all") {

  # Validate the "path" parameter
  params <- list(path = path, file_path = "data/geelite.db")
  validate_params(params)

  # Retrieve the list of all tables
  tables_all <- fetch_tables(path)

  # Determine which tables to read and validate the "tables" parameter
  tables <- validate_tables_param(tables, tables_all)

  # Read tables from the database
  db_list <- read_tables_from_db(path, tables)

  return(db_list)
}

# Internal Functions -----------------------------------------------------------

#' @title Filter and Sort Tables
#'
#' @description Filters out system tables and sorts the table names.
#'
#' @param tables A character vector of table names.
#'
#' @return A data frame with filtered and sorted table names.
#'
#' @keywords internal
#'
filter_and_sort_tables <- function(tables) {

  tables_drop <- c("grid", "spatial_ref_sys", "sqlite_sequence",
                   "geometry_columns")
  tables <- append("grid", sort(setdiff(tables, tables_drop)))
  tables <- data.frame(id = 1:length(tables), name = tables)

  return(tables)
}

# ------------------------------------------------------------------------------

#' @title Validate Tables Parameter
#'
#' @description Validates the 'tables' parameter and determines which tables to
#' read.
#'
#' @param tables A character or integer vector specifying tables to be read.
#'
#' @param tables_all A data frame of all available tables.
#'
#' @return A character vector of valid table names.
#'
#' @keywords internal
#'
validate_tables_param <- function(tables, tables_all) {

  if (any(tables == "all")) {
    tables <- tables_all$name
  } else if (is.numeric(tables)) {
    tables <- tables_all$name[tables]
    tables <- tables[!is.na(tables)]
  } else if (is.character(tables)) {
    tables <- intersect(tables, tables_all$name)
  } else {
    stop("Invalid 'tables' parameter.")
  }

  if (length(tables) == 0) {
    stop("No valid tables are specified. Please check 'tables' parameter.")
  }

  return(tables)
}

# ------------------------------------------------------------------------------

#' @title Read Tables from Database
#'
#' @description Reads the specified tables from the SQLite database.
#'
#' @param path The path to the root directory of the generated database.
#'
#' @param tables A character vector of table names to read.
#'
#' @return A list of tables read from the database.
#'
#' @keywords internal
#'
#' @importFrom sf st_read
#' @importFrom magrittr %>%
#' @importFrom dplyr rename select
#' @importFrom RSQLite dbConnect dbDisconnect dbListTables dbReadTable SQLite
#'
read_tables_from_db <- function(path, tables) {

  db_path <- file.path(path, "data/geelite.db")
  con <- dbConnect(SQLite(), dbname = db_path)

  # Read the "grid" table as an sf object
  db_list <- list(grid = st_read(con, "grid", quiet = TRUE) %>%
                    select(-1) %>% rename(geometry = GEOMETRY))

  # Read other specified tables as data frames
  if (any(!tables == "grid")) {
    tables <- setdiff(tables, "grid")
    for (table in tables) {
      db_list[[table]] <- dbReadTable(con, table, check.names = FALSE)
    }
  }

  dbDisconnect(con)
  return(db_list)
}
