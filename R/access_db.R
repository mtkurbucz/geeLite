# Main Functions ---------------------------------------------------------------

#' Fetch Names of SQLite Tables
#'
#' Reads and prints the names of available SQLite tables in the specified
#' directory of the generated database (\code{data/geelite.db}).
#' @param path [mandatory] (character) Path to the root directory of the
#' generated database.
#' @return A data frame containing the table names.
#' @export
#' @examples
#' # Example: Printing the names of available SQLite tables
#' \dontrun{
#'   fetch_tables(path = "path/to/root/directory")
#' }
#' @importFrom RSQLite dbConnect dbDisconnect dbListTables SQLite
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

# ------------------------------------------------------------------------------

#' Read Selected SQLite Tables
#'
#' Reads SQLite tables from the specified directory of the generated database
#' (\code{data/geelite.db}) into a list object.
#' @param path [mandatory] (character) Path to the root directory of the
#' generated database.
#' @param tables [optional] (character or integer) Names or IDs of the tables
#' to be read. Use the \code{fetch_tables} function to identify available table
#' names and IDs (default: \code{"all"}).
#' @param freq [optional] (character) Specifies the frequency to aggregate the
#' data (options: \code{NULL}, \code{"month"}, \code{"year"}). Default is
#' \code{NULL}, which means no aggregation.
#' @param freq_stats [optional] (character) A character vector of statistical
#' functions aggregation is based on (options: \code{NULL}, \code{"mean"},
#' \code{"median"}, \code{"min"}, \code{"max"}, \code{"sd"}). Default is
#' \code{NULL}, which means no aggregation.
#' @return A list where the first element ('grid') is an simple feature (sf)
#' object, and subsequent elements are data frame objects.
#' @export
#' @examples
#' # Example: Reading the 'grid' table
#' \dontrun{
#'   db_list <- read_db(path = "path/to/root/directory",
#'                      tables = "grid")
#' }
#'
read_db <- function(path, tables = "all", freq = NULL, freq_stats = NULL) {
  
  # Validate the 'path' parameter and retrieve the list of all tables
  tables_all <- fetch_tables(path)
  
  # Determine which tables to read and validate the 'tables' parameter
  tables <- validate_tables_param(tables, tables_all, freq, freq_stats)
  
  # Read tables from the database
  db_list <- read_tables(path, tables, freq, freq_stats)
  
  return(db_list)
}

# Internal Functions -----------------------------------------------------------

#' Filter and Sort Tables
#'
#' Filters out system tables and sorts the table names.
#' @param tables [mandatory] (character or integer) A vector of table names.
#' @return A data frame with filtered and sorted table names.
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

#' Read Tables from Database
#'
#' Reads the specified tables from the SQLite database.
#' @param path [mandatory] (character) Path to the root directory of the
#' generated database.
#' @param tables [mandatory] (character) A vector of table names to be read.
#' @param freq [mandatory] (character) Specifies the frequency to aggregate the
#' data (options: \code{"month"}, \code{"year"}).
#' @param freq_stats [optional] (character) A character vector of statistical
#' functions aggregation is based on (options: \code{NULL}, \code{"mean"},
#' \code{"median"}, \code{"min"}, \code{"max"}, \code{"sd"}).
#' @return A list of tables read from the database.
#' @keywords internal
#' @importFrom sf st_read
#' @importFrom magrittr %>%
#' @importFrom dplyr rename select
#' @importFrom RSQLite dbConnect dbDisconnect dbListTables dbReadTable SQLite
#'
read_tables <- function(path, tables, freq = NULL, freq_stats = NULL) {
  
  # To avoid 'no visible binding for global variable' messages (CRAN test)
  GEOMETRY <- NULL
  
  # Connect to the SQLite database
  db_path <- file.path(path, "data/geelite.db")
  con <- dbConnect(SQLite(), dbname = db_path)
  
  # Read the 'grid' table as an sf object
  db_list <- list(grid = st_read(con, "grid", quiet = TRUE) %>%
                    select(-1) %>% rename(geometry = GEOMETRY))
  
  # Read other specified tables as data frames
  if (any(!tables == "grid")) {
    tables <- setdiff(tables, "grid")
    for (table in tables) {
      if (is.null(freq)) {
        db_list[[table]] <- dbReadTable(con, table, check.names = FALSE)
      } else {
        db_list[[table]] <- aggr_by_freq(
          table = dbReadTable(con, table, check.names = FALSE),
          freq = freq,
          freq_stats = freq_stats
        )
      }
    }
  }
  
  # Disconnect from the SQLite database
  dbDisconnect(con)
  return(db_list)
}

# ------------------------------------------------------------------------------

#' Aggregate Data by Frequency
#'
#' This function aggregates data from a wide-format data frame according to a
#' specified frequency and selected statistical functions.
#' @param table [mandatory] (data.frame) A wide-format data frame object of the
#' generated SQLite table.
#' @param freq [mandatory] (character) Specifies the frequency to aggregate the
#' data (options: \code{"month"}, \code{"year"}).
#' @param freq_stats [optional] (character) A character vector of statistical
#' functions aggregation is based on (options: \code{NULL}, \code{"mean"},
#' \code{"median"}, \code{"min"}, \code{"max"}, \code{"sd"}).
#' @return A data frame in wide format where columns represent aggregated values
#' for each frequency period and statistic applied.
#' @keywords internal
#' @importFrom magrittr %>%
#' @importFrom lubridate floor_date ymd
#' @importFrom tidyr matches pivot_longer pivot_wider
#' @importFrom dplyr arrange bind_rows group_by mutate summarise
#'
aggr_by_freq <- function(table, freq, freq_stats) {
  
  # To avoid 'no visible binding for global variable' messages (CRAN test)
  band <- freq_date <- id <- stat <- value <- NULL
  
  # Convert to long format
  df_long <- table %>%
    pivot_longer(matches("_"), names_to = "date", values_to = "value") %>%
    mutate(date = ymd(gsub("_", "-", date))) %>%
    select(-matches("_"))
  
  # Define the frequency
  if (freq == "month") {
    df_long <- df_long %>%
      mutate(freq_date = floor_date(date, "month"))
  } else if (freq == "year") {
    df_long <- df_long %>%
      mutate(freq_date = floor_date(date, "year"))
  }
  
  # Initialize an empty list to store the aggregated data frames
  list_aggr <- list()
  
  # Loop through each statistic and aggregate the data
  for (freq_stat in freq_stats) {
    fun <- match.fun(freq_stat)
    df_aggr <- df_long %>%
      group_by(id, band, stat, freq_date) %>%
      summarise(value = fun(value, na.rm = TRUE), .groups = "drop") %>%
      mutate(freq_stat = freq_stat)
    
    # Append the result to the list
    list_aggr[[freq_stat]] <- df_aggr
  }
  
  # Combine all results into a single data frame
  df_aggr <- bind_rows(list_aggr)
  
  # Convert back to wide format if needed and format the date columns
  df_wide <- df_aggr %>%
    mutate(freq_date = format(freq_date, "%Y_%m_%d")) %>%
    pivot_wider(names_from = freq_date, values_from = value) %>%
    arrange(id, band, stat, freq_stat)
  
  return(df_wide)
}
