# Main Functions ---------------------------------------------------------------

#' Fetch Table Names Along with Band Information from an SQLite Database
#'
#' Reads and prints the names of available SQLite tables and their associated
#' bands in the specified directory of the generated database
#' (\code{data/geelite.db}).
#' @param path [mandatory] (character) Path to the root directory of the
#' generated database.
#' @param print_output (logical) If \code{TRUE}, prints the output in a
#' markdown format; if \code{FALSE}, returns a data frame object (default:
#' \code{TRUE}).
#' @return A data frame containing the table names.
#' @export
#' @examples
#' # Example: Printing the names of available SQLite tables
#' \dontrun{
#'   fetch_tables(path = "path/to/db")
#' }
#' @importFrom RSQLite dbConnect dbDisconnect dbListTables dbReadTable SQLite
#' @importFrom dplyr arrange distinct pull select
#' @importFrom knitr kable
#'
fetch_tables <- function(path, print_output = TRUE) {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  band <- NULL

  # Validate parameters
  params <- list(path = path, file_path = "data/geelite.db")
  validate_params(params)

  # Retrieve the list of tables
  db_path <- file.path(path, "data/geelite.db")
  con <- dbConnect(SQLite(), dbname = db_path)
  tables <- dbListTables(conn = con)

  # Filter out system tables and sort the table names
  tables_drop <- c("grid", "spatial_ref_sys", "sqlite_sequence",
                   "geometry_columns")
  tables <- append("grid", sort(setdiff(tables, tables_drop)))
  tables <- data.frame(id = 1:length(tables), name = tables)

  # Initialize bands column
  tables$bands <- "-"
  number_of_tables <- nrow(tables)

  # Add available bands for each table
  if (number_of_tables > 1) {
    for (i in 2:number_of_tables) {
      unique_bands <- dbReadTable(con, tables$name[i], check.names = FALSE) %>%
        select(band) %>%
        distinct() %>%
        arrange(band) %>%
        pull(band)
      tables$bands[i] <- list(unique_bands)
    }
  }

  dbDisconnect(con)

  # Print or return the output based on user preference
  if (print_output) {
    print(
      kable(tables, format = "markdown", col.names = c("ID", "Table", "Bands"))
    )
  } else {
    return(tables)
  }
}

# ------------------------------------------------------------------------------

#' Read and Transform SQLite Tables
#'
#' Reads SQLite tables from the specified directory of the generated database
#' (\code{data/geelite.db}), transforms the data into a daily frequency, then
#' reprojects it to the desired frequency (e.g., weekly, monthly). The result
#' is returned as a list object.
#' @param path [mandatory] (character) Path to the root directory of the
#' generated database.
#' @param tables [optional] (character or integer) Names or IDs of the tables
#' to be read. Use the \code{fetch_tables} function to identify available table
#' names and IDs (default: \code{"all"}).
#' @param freq [optional] (character) Specifies the frequency to aggregate the
#' data (options: \code{"day"}, \code{"week"}, \code{"month"},
#' \code{"bimonth"}, \code{"quarter"}, \code{"season"}, \code{"halfyear"},
#' \code{"year"}). The default is \code{"month"}.
#' @param prep_fun [optional] (function) A single function used for
#' pre-processing the time series data before aggregation. This function
#' converts the data to a daily frequency and applies any necessary data
#' transformation or imputation. Default is linear interpolation:
#' \code{imputeTS::na_interpolation(x, option = "linear")}.
#' @param aggr_funs [optional] (function or list) Specifies the aggregation
#' function(s) to be applied to the data. This can be a single function or a
#' list of functions (default: \code{mean(x, na.rm = TRUE)}).
#' @return A list where the first element ('grid') is an simple feature (sf)
#' object, and subsequent elements are data frame objects.
#' @export
#' @examples
#' # Example: Reading the 'grid' table
#' \dontrun{
#'   db_list <- read_db(path = "path/to/db",
#'                      tables = "grid")
#' }
#' @importFrom imputeTS na_interpolation
#'
read_db <- function(path, tables = "all", freq = "month",
                    prep_fun = function(x)
                    na_interpolation(x, option = "linear"),
                    aggr_funs = function(x) mean(x, na.rm = TRUE)) {

  # Convert 'aggr_funs' to a list if a single function is provided
  if (is.function(aggr_funs)) {
    aggr_funs <- list(aggr_funs)
  }

  # Validate the 'path' parameter and retrieve the list of all tables
  tables_all <- fetch_tables(path, print_output = FALSE)

  # Determine which tables to read and validate the 'tables' parameter
  tables <- validate_tables_param(tables, tables_all, freq, prep_fun, aggr_funs)

  # Read tables from the database
  db_list <- read_tables(path, tables = tables, freq, prep_fun, aggr_funs)

  return(db_list)
}

# Internal Functions -----------------------------------------------------------

#' Read Tables from Database
#'
#' Reads the specified tables from the SQLite database.
#' @param path [mandatory] (character) Path to the root directory of the
#' generated database.
#' @param tables [mandatory] (character) A vector of table names to be read.
#' @param freq [mandatory] (character) Specifies the frequency to aggregate the
#' data (options: \code{"day"}, \code{"week"}, \code{"month"},
#' \code{"bimonth"}, \code{"quarter"}, \code{"season"}, \code{"halfyear"},
#' \code{"year"}).
#' @param prep_fun [mandatory] (function) A single function used for
#' pre-processing the time series data before aggregation. This function
#' converts the data to a daily frequency and applies any necessary data
#' transformation or imputation.
#' @param aggr_funs [mandatory] (function or list) Specifies the aggregation
#' function(s) to be applied to the data. This can be a single function or a
#' list of functions.
#' @return A list of tables read from the database.
#' @keywords internal
#' @importFrom sf st_read
#' @importFrom magrittr %>%
#' @importFrom dplyr rename select
#' @importFrom RSQLite dbConnect dbDisconnect dbListTables dbReadTable SQLite
#'
read_tables <- function(path, tables, freq, prep_fun, aggr_funs) {

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
          prep_fun = prep_fun,
          aggr_funs = aggr_funs
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
#' data (options: \code{"day"}, \code{"week"}, \code{"month"},
#' \code{"bimonth"}, \code{"quarter"}, \code{"season"}, \code{"halfyear"},
#' \code{"year"}).
#' @param prep_fun [mandatory] (function) A single function used for
#' pre-processing the time series data before aggregation. This function
#' converts the data to a daily frequency and applies any necessary data
#' transformation or imputation.
#' @param aggr_funs [mandatory] (function or list) Specifies the aggregation
#' function(s) to be applied to the data. This can be a single function or a
#' list of functions.
#' @return A data frame in wide format where columns represent aggregated values
#' for each frequency period and statistic applied.
#' @keywords internal
#' @importFrom magrittr %>%
#' @importFrom lubridate floor_date ymd
#' @importFrom tidyr all_of pivot_longer pivot_wider
#' @importFrom dplyr arrange bind_rows group_by mutate summarise
#'
aggr_by_freq <- function(table, freq, prep_fun, aggr_funs) {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  . <- band <- freq_date <- id <- zonal_stat <- value <- NULL

  # Identify date columns
  date_cols <- names(table) %>%
    grep("_", ., value = TRUE) %>%
    grep("zonal_stat", ., value = TRUE, invert = TRUE)

  # Convert to long format
  df_long <- table %>%
    pivot_longer(all_of(date_cols), names_to = "date", values_to = "value") %>%
    mutate(date = ymd(gsub("_", "-", date)))

  # Expand to daily frequency, filling in missing dates
  df_exp <- expand_to_daily(df_long, prep_fun)

  # Define the frequency
  df_exp <- df_exp %>%
    mutate(freq_date = floor_date(date, freq))

  # Initialize an empty list to store the aggregated data frames
  list_aggr <- list()

  # Loop through each statistic and aggregate the data
  for (aggr_fun in aggr_funs) {

    # Extract the body of the function and convert it to a string
    prep_fun_name <- deparse(body(prep_fun))
    aggr_fun_name <- deparse(body(aggr_fun))

    # Apply the function to summarize and group the data
    df_aggr <- df_exp %>%
      group_by(id, band, zonal_stat, freq_date) %>%
      summarise(value = aggr_fun(value), .groups = "drop") %>%
      mutate(prep_fun = prep_fun_name,
             aggr_fun = aggr_fun_name)

    # Append the result to the list
    list_aggr[[aggr_fun_name]] <- df_aggr
  }

  # Combine all results into a single data frame
  df_aggr <- bind_rows(list_aggr)

  # Convert back to wide format if needed and format the date columns
  df_wide <- df_aggr %>%
    mutate(freq_date = format(freq_date, "%Y_%m_%d")) %>%
    pivot_wider(names_from = freq_date, values_from = value) %>%
    arrange(id, band, zonal_stat, prep_fun, aggr_fun)

  return(df_wide)
}

# ------------------------------------------------------------------------------

#' Expand Data to Daily Frequency
#'
#' This function expands the input data frame (\code{df_long}) to a daily
#' frequency, filling in any missing dates within the observed range. A
#' preprocessing function (\code{prep_fun}) is applied to handle missing or
#' interpolated values.
#' @param df_long [mandatory] (data.frame) A long-format data frame with at
#' least the columns \code{id}, \code{band}, \code{zonal_stat}, and \code{date}.
#' @param prep_fun [mandatory] (function) A single function used for
#' pre-processing the time series data before aggregation. This function
#' converts the data to a daily frequency and applies any necessary data
#' transformation or imputation.
#' @return A data frame with daily dates and preprocessed \code{value} column.
#' @keywords internal
#' @importFrom dplyr group_by mutate ungroup
#' @importFrom tidyr complete
#'
expand_to_daily <- function(df_long, prep_fun) {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  id <- band <- value <- zonal_stat <- NULL

  # Generate the full range of dates
  date_range <- seq(min(df_long$date), max(df_long$date), by = "day")

  # Expand the data to daily frequency and applying the preprocessing function
  df_exp <- df_long %>%
    group_by(id, band, zonal_stat) %>%
    complete(date = date_range) %>%
    mutate(value = prep_fun(value)) %>%
    ungroup()

  return(df_exp)
}
