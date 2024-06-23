#' @title Printing the Names of the Available SQLite Tables
#'
#' @description This function prints the names of the available SQLite tables.
#'
#' @param path [mandatory] (character) Path to the root directory of the
#' generated database.
#'
#' @return Table names.
#'
#' @export
#'
#' @examples
#' # Example: Printing the available SQLite tables
#' \dontrun{
#'   get_tables(path = "path")
#' }
#'
#' @importFrom RSQLite dbConnect dbGetQuery dbListTables
#'
get_tables <- function(path) {

  db_file <- file.path(path, "data", "geelite.db")
  if (!file.exists(db_file)) {stop("Database does not exist.")}

  con <- dbConnect(SQLite(), dbname = db_file)
  tables <- dbListTables(conn = con)
  dbDisconnect(con)
  tables_drop <- c("grid",
                   "spatial_ref_sys",
                   "sqlite_sequence",
                   "geometry_columns")
  tables <- append("grid", sort(setdiff(tables, tables_drop)))
  tables <- data.frame(id = 1:length(tables), name = tables)

  return(tables)
}
