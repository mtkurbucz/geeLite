#' @title Reading Selected SQLite Tables
#'
#' @description Reading selected SQLite tables into an \code{list} object.
#'
#' @param path [mandatory] (character) Path to the root directory of the
#' generated database.
#'
#' @param tables [optional] (character or integer) Names or IDs of the selected
#' tables. To identify the names and IDs of the available tables, use the
#' \code{get_tables} function (default: \code{"all"}).
#'
#' @return A list object with the first element (grid) as an sf object, and
#' subsequent elements (databases) as data frame objects.
#'
#' @export
#'
#' @examples
#' # Example: Reading the "grid" table
#' \dontrun{
#'   read_db(path = "path", tables = "grid")
#' }
#'
#' @importFrom sf st_read
#' @importFrom magrittr %>%
#' @importFrom dplyr rename select
#' @importFrom RSQLite dbConnect dbDisconnect dbListTables dbReadTable SQLite
#'
read_db <- function(path, tables = "all") {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  GEOMETRY <- NULL

  tables_all <- get_tables(path)

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
    stop("Invalid 'tables' parameter.")
  }

  # Convert "grid" table into an sf object
  db_file <- file.path(path, "data", "geelite.db")
  con <- dbConnect(SQLite(), dbname = db_file)
  db_list <- list(grid = st_read(con, "grid", quiet = TRUE) %>%
             select(-1) %>% rename(geometry = GEOMETRY))

  if (any(!tables == "grid")) {
    # Combine selected tables into one sf object
    tables <- setdiff(tables, "grid")
    for (table in tables) {
      db_list[[table]] <- dbReadTable(con, table, check.names = FALSE)
    }
  }

  dbDisconnect(con)
  return(db_list)
}
