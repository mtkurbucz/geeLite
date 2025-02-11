# Main Functions -------------------------------------------------------------

#' Fetch Variable Information from an SQLite Database
#'
#' Displays information on the available variables in the SQLite database
#' (\code{data/geelite.db}).
#' @param path [mandatory] (character) Path to the root directory of the
#'   generated database.
#' @param format [mandatory] (character) A character string. Possible values
#'   are \code{"data.frame"} (default) to return a \code{data.frame} object,
#'   or one of \code{"markdown"}, \code{"latex"}, \code{"html"}, \code{"pipe"}
#'   (Pandoc's pipe tables), \code{"simple"} (Pandoc's simple tables), and
#'   \code{"rst"} to be passed on to knitr for formatting.
#' @return Returns the variable information in the selected format. If
#'   \code{format = "data.frame"}, a \code{data.frame} is returned. For other
#'   formats, the output is printed in the specified format and \code{NULL} is
#' @export
#' @examples
#' # Example: Printing the available variables
#' \dontrun{
#'   fetch_vars(path = "path/to/db")
#' }
#' @importFrom RSQLite dbDisconnect dbListTables dbReadTable SQLite
#' @importFrom dplyr arrange distinct pull select filter bind_rows
#' @importFrom lubridate ymd
#' @importFrom knitr kable
#'
fetch_vars <- function(path, format = c("data.frame", "markdown", "latex",
                                        "html", "pipe", "simple", "rst")) {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  band <- zonal_stat <- NULL

  # Ensure that 'format' is one of the allowed options
  format <- match.arg(format)

  # Validate parameters
  params <- list(path = path, file_path = "data/geelite.db")
  validate_params(params)

  # Retrieve the list of tables
  db_path <- file.path(path, "data/geelite.db")
  if (!file.exists(db_path)) {
    stop(paste0("Database file does not exist at: ", db_path))
  }

  # Establish a connection to the SQLite database
  con <- db_connect(db_path = db_path)

  # Ensure the connection is closed when the function exits
  on.exit(dbDisconnect(con), add = TRUE)

  # List all tables in the connected SQLite database
  tables <- dbListTables(conn = con)

  # Filter out system tables, the 'grid' table, and sort the table names
  tables_drop <- c("grid", "spatial_ref_sys", "sqlite_sequence",
                   "geometry_columns")
  tables <- sort(setdiff(tables, tables_drop))
  tables_df <- data.frame(Table = tables, stringsAsFactors = FALSE)

  # Initialize a list to hold detailed information
  detailed_info_list <- list()

  id_counter <- 1 # Start ID counter from 1

  # Process each table to extract variable information
  for (i in seq_len(nrow(tables_df))) {
    table_name <- tables_df$Table[i]

    # Read the table data from the database
    table_data <- dbReadTable(con, table_name, check.names = FALSE)

    # Extract date columns matching the YYYY_MM_DD format
    date_cols <- grep("^\\d{4}_\\d{2}_\\d{2}$", names(table_data), value = TRUE)
    dates <- ymd(date_cols)

    # Determine the first and last dates and calculate the mean interval
    first_date <- min(dates, na.rm = TRUE)
    last_date <- max(dates, na.rm = TRUE)
    mean_interval <- round(mean(as.numeric(diff(dates)), na.rm = TRUE), 2)

    # Identify unique combinations of band and zonal_stat
    unique_bands_stats <- table_data %>%
      select(band, zonal_stat) %>%
      distinct() %>%
      arrange(band, zonal_stat)

    # For each unique combination, create a row in the detailed_info_list
    for (j in seq_len(nrow(unique_bands_stats))) {
      band_name <- unique_bands_stats$band[j]
      stat_name <- unique_bands_stats$zonal_stat[j]
      variable_name <- paste(table_name, band_name, stat_name, sep = "/")
      tmp <- data.frame(
        ID = id_counter,
        Variable = variable_name,
        Table = table_name,
        Band = band_name,
        Stat = stat_name,
        Start = first_date,
        End = last_date,
        Freq_Days = mean_interval,
        stringsAsFactors = FALSE
      )
      detailed_info_list[[length(detailed_info_list) + 1]] <- tmp
      id_counter <- id_counter + 1
    }
  }

  # Combine all data frames in the list into one data frame
  detailed_info <- do.call(rbind, detailed_info_list)

  # Print or return the output based on user preference
  if (format != "data.frame") {
    print(
      kable(detailed_info, format = format,
            col.names = c("ID", "Variable", "Table", "Band", "Stat",
                          "Start", "End", "Freq (Days)"))
    )
  } else {
    return(detailed_info)
  }
}

# ------------------------------------------------------------------------------

#' Reading, Aggregating, and Processing the SQLite Database
#'
#' Reads, aggregates, and processes the SQLite database
#'   (\code{data/geelite.db}).
#' @param path [mandatory] (character) Path to the root directory of the
#'   generated database.
#' @param variables [optional] (character or integer) Names or IDs of the
#'   variables to be read. Use the \code{fetch_vars} function to identify
#'   available variables and IDs (default: \code{"all"}).
#' @param freq [optional] (character) The frequency for data aggregation.
#'   Options include \code{"day"}, \code{"week"}, \code{"month"},
#'   \code{"bimonth"}, \code{"quarter"}, \code{"season"}, \code{"halfyear"},
#'   \code{"year"} (default: \code{"month"}).
#' @param prep_fun [optional] (function or \code{NULL}) A function for
#'   pre-processing time series data prior to aggregation. If \code{NULL}, a
#'   default linear interpolation (via \code{\link{linear_interp}}) will be
#'   used for daily-frequency data. If non-daily, the default behavior simply
#'   returns the vector without interpolation.
#' @param aggr_funs [optional] (function or list) A function or a list of
#'   functions for aggregating data to the specified frequency (\code{freq}).
#'   Users can directly refer to variable names or IDs. The default function is
#'   the mean: \code{function(x) mean(x, na.rm = TRUE)}.
#' @param postp_funs [optional] (function or list) A function or list of
#'   functions applied to the time series data of a single bin after
#'   aggregation. Users can directly refer to variable names or IDs. The
#'   default is \code{NULL}, indicating no post-processing.
#' @return A list where the first element (\code{grid}) is a simple feature
#'   (sf) object, and subsequent elements are data frame objects corresponding
#'   to the variables.
#' @export
#' @examples
#' # Example: Reading variables by IDs
#' \dontrun{
#' db_list <- read_db(path = "path/to/db",
#'   variables = c(1, 3))
#' }
#'
read_db <- function(
    path,
    variables = "all",
    freq = c("month", "day", "week", "bimonth", "quarter",
             "season", "halfyear", "year"),
    prep_fun = NULL,
    aggr_funs = function(x) mean(x, na.rm = TRUE),
    postp_funs = NULL
) {
  # Validate 'freq' parameter
  freq <- match.arg(freq)

  # If user has specified "external" post-processing
  if (identical(postp_funs, "external")) {
    postp_funs <- load_external_postp(path)
  }

  # Ensure 'aggr_funs' is a named list
  if (is.function(aggr_funs) || (is.list(aggr_funs) &&
                                 all(sapply(aggr_funs, is.function)))) {
    aggr_funs <- list(default = aggr_funs)
  }

  # Ensure 'postp_funs' is a named list
  if (is.function(postp_funs) ||
      (is.list(postp_funs) && all(sapply(postp_funs, is.function))) ||
      is.null(postp_funs)) {
    postp_funs <- list(default = postp_funs)
  }

  # If no prep_fun was provided, default to linear interpolation
  if (is.null(prep_fun)) {
    prep_fun <- function(x) {
      linear_interp(x)
    }
  }

  # Validate 'path' and obtain the list of available variables
  variables_all <- fetch_vars(path, format = "data.frame")

  # Validate parameters and determine which variables to read
  variables <- validate_variables_param(variables, variables_all,
                                        prep_fun, aggr_funs, postp_funs)

  # Read and process data for the selected variables
  db_list <- read_variables(path, variables, freq, prep_fun,
                            aggr_funs, postp_funs)

  return(db_list)
}

# ------------------------------------------------------------------------------

#' Initialize Post-Processing Folder and Files
#'
#' Creates a \code{postp} folder at the specified path and adds two empty
#' files: \code{structure.json} and \code{functions.R}.
#' @param path [mandatory] \code{character} The path to the root directory
#'   where the \code{postp} folder should be created.
#' @param verbose [optional] (logical) Display messages (default: \code{TRUE}).
#' @details
#' The \code{structure.json} file is initialized with a default JSON structure:
#' \code{"default": null}. This file is intended for mapping variables to
#' post-processing functions. The \code{functions.R} file is created with a
#' placeholder comment indicating where to define the R functions for
#' post-processing. If the \code{postp} folder already exists, an error will
#' be thrown to prevent overwriting existing files.
#' @examples
#' \dontrun{
#' # Initialize post-processing folder and files in the database root directory
#' init_postp("/path/to/database")
#' }
#' @export
#'
init_postp <- function(path, verbose = TRUE) {

  # Define the 'postp' folder and file paths
  postp_folder <- file.path(path, "postp")
  json_file <- file.path(postp_folder, "structure.json")
  r_script_file <- file.path(postp_folder, "functions.R")

  # Check if the 'postp' folder already exists
  if (dir.exists(postp_folder)) {
    stop(paste0("The 'postp' folder already exists at: ", postp_folder))
  }

  # Create the 'postp' folder
  dir.create(postp_folder, showWarnings = FALSE)

  # Create an empty 'structure.json' file
  writeLines('{\n  "default": null\n}', json_file)

  # Create an empty 'functions.R' file
  writeLines("# Define your post-processing functions here\n", r_script_file)

  # Prepare messages for structure and functions file initialization
  message <- list(
    "Structure file initialized: 'postp/structure.json'.",
    "Function file initialized: 'postp/functions.R'."
  )

  # Output information if 'verbose' is TRUE
  output_message(message, verbose)

}

# Internal Functions -----------------------------------------------------------

#' Simple Linear Interpolation
#'
#' Replaces \code{NA} values with linear interpolation.
#' @param x [mandatory] (numeric) A numeric vector possibly containing
#'   \code{NA} values.
#' @return A numeric vector of the same length as \code{x}, with \code{NA}
#'   values replaced by linear interpolation.
#' @keywords internal
#' @importFrom stats approx
#'
linear_interp <- function(x) {
  # If all values are NA, return as-is
  if (all(is.na(x))) {
    return(x)
  }

  # Identify non-NA indices
  idx <- which(!is.na(x))

  # If fewer than 2 non-NA values, cannot interpolate meaningfully
  if (length(idx) < 2) {
    return(x)
  }

  # Perform linear interpolation using approx
  # rule = 2 => extend boundary values if the first or last are NA
  approx(
    x      = idx,
    y      = x[idx],
    xout   = seq_along(x),
    method = "linear",
    rule   = 2
  )$y
}

# ------------------------------------------------------------------------------

#' Read Variables from Database
#'
#' Reads the specified variables from the SQLite database.
#' @param path [mandatory] (character) Path to the root directory of the
#'   generated database.
#' @param variables [mandatory] (character) A vector of variable names to read.
#' @param freq [mandatory] (character) Specifies the frequency to aggregate the
#'   data.
#' @param prep_fun [mandatory] (function) Function used for pre-processing.
#' @param aggr_funs [mandatory] (function or list) Aggregation function(s).
#' @param postp_funs [mandatory] (function or list) Post-processing function(s).
#' @return A list of variables read from the database.
#' @keywords internal
#' @importFrom sf st_read
#' @importFrom magrittr %>%
#' @importFrom dplyr rename select filter
#' @importFrom RSQLite dbDisconnect dbReadTable SQLite
#'
read_variables <- function(path, variables, freq, prep_fun,
                           aggr_funs, postp_funs) {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  id <- band <- GEOMETRY <- zonal_stat <- aggregation <- preprocess <- NULL
  postprocess <- NULL

  # Connect to the SQLite database
  db_path <- file.path(path, "data/geelite.db")
  if (!file.exists(db_path)) {
    stop(paste0("Database file does not exist at: ", db_path))
  }

  # Establish a connection to the SQLite database
  con <- db_connect(db_path = db_path)

  # Ensure the connection is closed when the function exits
  on.exit(dbDisconnect(con), add = TRUE)

  # Read the 'grid' table as an sf object
  grid_sf <- tryCatch(
    sf::st_read(con, "grid", quiet = TRUE) %>%
      select(-1) %>% rename(geometry = GEOMETRY),
    error = function(e) {
      stop("Failed to read the 'grid' table: ", e$message)
    }
  )

  # Initialize the list to store grid and variable data
  db_list <- list(grid = grid_sf)

  # Retrieve all available variables from the database
  variables_all <- fetch_vars(path, format = "data.frame")

  # Match the requested variables with the available ones
  variables_info <- variables_all[match(variables, variables_all$Variable), ]

  # Initialize a list to store data for each variable
  variables_data <- vector("list", length(variables))

  last_table_read <- NULL # Track the last table read to optimize reads
  table_data_cache <- list() # Cache tables to avoid redundant reads

  # Iterate over each variable to extract and process data
  for (i in seq_len(nrow(variables_info))) {
    var_info <- variables_info[i, ]
    variable_name <- var_info$Variable
    table_name <- var_info$Table
    band_name <- var_info$Band
    stat_name <- var_info$Stat

    # Read the table data if it hasn't been read already
    if (is.null(last_table_read) || last_table_read != table_name) {
      if (!table_name %in% names(table_data_cache)) {
        table_data <- tryCatch(
          dbReadTable(con, table_name, check.names = FALSE),
          error = function(e) {
            warning(paste("Failed to read table:", table_name, "-", e$message))
            return(NULL)
          }
        )
        table_data_cache[[table_name]] <- table_data
      } else {
        table_data <- table_data_cache[[table_name]]
      }
      last_table_read <- table_name # Update the last table read
    } else {
      table_data <- table_data_cache[[table_name]]
    }

    # Skip processing if the table data couldn't be read
    if (is.null(table_data)) {
      warning(paste("Skipping variable due to missing table:", table_name))
      next
    }

    # Filter the table data for the current variable's band and stat
    table_data_subset <- table_data %>%
      filter(band == band_name & zonal_stat == stat_name)

    # Skip if no data is found for the current variable
    if (nrow(table_data_subset) == 0) {
      warning(paste("No data found for variable", variable_name))
      next
    }

    if (is.null(freq)) {

      # If no aggregation is specified, prepare the data accordingly
      variable_data <- table_data_subset %>%
        select(-band, -zonal_stat) %>%
        transform(preprocess = NA_character_,
                  aggregation = NA_character_,
                  postprocess = NA_character_) %>%
        subset(select = c("id", "aggregation", "preprocess", "postprocess",
                          setdiff(names(table_data_subset),
                                  c("band", "zonal_stat"))))

    } else {

      # If aggregation is specified, proceed with aggregating the data
      # Capture the body of the pre-processing function as a string
      preprocess_body <- paste(deparse(body(prep_fun)), collapse = "\n")

      # Perform aggregation based on the specified frequency and functions
      variable_data <- aggr_by_freq(
        table = table_data_subset,
        freq = freq,
        prep_fun = prep_fun,
        aggr_funs = aggr_funs,
        postp_funs = postp_funs,
        variable_name = variable_name,
        preprocess_body = preprocess_body
      )

    }

    # Store the processed data for the current variable
    variables_data[[i]] <- variable_data
  }

  # Assign names to the list elements based on variable names
  names(variables_data) <- variables

  # Combine the grid data with all variable data into a single list
  db_list <- c(db_list, variables_data)

  return(db_list)
}

# ------------------------------------------------------------------------------

#' Aggregate Data by Frequency
#'
#' Aggregates data from a wide-format data frame according to a specified
#' frequency and applies aggregation and post-processing functions.
#' @param table [mandatory] (data.frame) A wide-format data frame.
#' @param freq [mandatory] (character) Specifies the frequency to aggregate the
#'   data.
#' @param prep_fun [mandatory] (function) Function used for pre-processing.
#' @param aggr_funs [mandatory] (function or list) Aggregation function(s).
#' @param postp_funs [mandatory] (function or list) Post-processing function(s).
#' @param variable_name [mandatory] (character) Name of the current variable.
#' @param preprocess_body [mandatory] (character) Body of the \code{prep_fun}
#'   function.
#' @return A data frame in wide format with aggregated values.
#' @keywords internal
#' @importFrom magrittr %>%
#' @importFrom reshape2 dcast melt
#' @importFrom lubridate floor_date ymd
#' @importFrom tidyr all_of pivot_longer pivot_wider
#' @importFrom dplyr arrange bind_rows group_by mutate summarise
#'
aggr_by_freq <- function(table, freq, prep_fun, aggr_funs,
                         postp_funs, variable_name, preprocess_body) {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  . <- id <- value <- freq_date <- aggregation <- postprocess <- NULL

  # Identify columns that represent dates (excluding 'zonal_stat' if present)
  date_cols <- grep("_", names(table), value = TRUE)
  date_cols <- setdiff(date_cols, grep("zonal_stat", date_cols, value = TRUE))

  # Convert the wide-format data to long format for easier manipulation
  df_long <- melt(table, id.vars = c("id", "band", "zonal_stat"),
                  measure.vars = date_cols,
                  variable.name = "date", value.name = "value")

  # Convert the 'date' column from character to Date object
  df_long$date <- as.Date(gsub("_", "-", df_long$date), format = "%Y-%m-%d")

  # Expand the data to daily frequency, filling in missing dates
  df_exp <- expand_to_daily(df_long, prep_fun)

  # Create a new column 'freq_date' that floors the date to the specified 'freq'
  df_exp <- df_exp %>%
    mutate(freq_date = floor_date(date, freq))

  # Retrieve the aggregation function(s) for the current variable
  funcs <- aggr_funs[[variable_name]]
  if (is.null(funcs)) {
    funcs <- aggr_funs[["default"]]
  }
  if (is.null(funcs)) {
    stop(paste("No aggregation function specified for variable",
               variable_name))
  }
  if (is.function(funcs)) {
    funcs <- list(funcs)
  }

  # Initialize a list to store aggregation results
  aggr_results <- list()

  # Iterate over each aggregation function to process the data
  for (i in seq_along(funcs)) {
    aggr_fun <- funcs[[i]]
    # Capture the body of the aggregation function as a string
    aggregation_body <- paste(deparse(body(aggr_fun)), collapse = "\n")

    # Perform grouping and aggregation
    df_group <- df_exp %>%
      group_by(id, freq_date) %>%
      summarise(value = aggr_fun(value), .groups = "drop") %>%
      mutate(aggregation = aggregation_body,
             preprocess = preprocess_body)

    # Append the aggregated data to the results list
    aggr_results[[i]] <- df_group
  }

  # Combine all aggregation results into a single data frame
  df_combined_aggr <- bind_rows(aggr_results)

  # Retrieve post-processing functions for the current variable
  postp_funcs_var <- postp_funs[[variable_name]]
  if (is.null(postp_funcs_var)) {
    postp_funcs_var <- postp_funs[["default"]]
  }

  if (is.null(postp_funcs_var)) {
    # If no post-processing functions, assign NA to 'postprocess' column
    df_final <- transform(df_combined_aggr, postprocess = NA_character_)
  } else {
    # If post-processing functions are provided, apply each function
    if (is.function(postp_funcs_var)) {
      postp_funcs_var <- list(postp_funcs_var)
    }
    postp_results <- list()
    for (i in seq_along(postp_funcs_var)) {
      postp_fun <- postp_funcs_var[[i]]
      # Capture the body of the post-processing function as a string
      postprocess_body <- paste(deparse(body(postp_fun)), collapse = "\n")

      # Apply the post-processing function to the 'value' column
      df_postp <- transform(df_combined_aggr,
                            value = postp_fun(value),
                            postprocess = postprocess_body)

      # Append the post-processed data to the results list
      postp_results[[i]] <- df_postp
    }
    # Combine all post-processed data into a single data frame
    df_final <- bind_rows(postp_results)
  }

  # Convert the long-format aggregated data back to wide format
  df_wide <- dcast(df_final, id + aggregation + postprocess ~ freq_date,
                   value.var = "value")

  # Order the data frame by 'id', 'aggregation', and 'postprocess' columns
  df_wide <- df_wide[order(df_wide$id,
                           df_wide$aggregation,
                           df_wide$postprocess), ]

  return(df_wide) # Return the aggregated wide-format data frame
}

# ------------------------------------------------------------------------------

#' Expand Data to Daily Frequency
#'
#' Expands the input data frame to a daily frequency, filling in any missing
#' dates within the observed range.
#' @param df_long [mandatory] (data.frame) A long-format data frame with at
#'   least the columns \code{id} and \code{date}.
#' @param prep_fun [mandatory] (function) Function used for pre-processing.
#' @return A data frame with daily dates and preprocessed \code{value} column.
#' @keywords internal
#' @importFrom dplyr group_by mutate ungroup
#' @importFrom tidyr complete
#'
expand_to_daily <- function(df_long, prep_fun) {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  id <- value <- NULL

  # Validate that 'df_long' contains the required columns
  required_cols <- c("id", "date", "value")
  if (!all(required_cols %in% names(df_long))) {
    stop("df_long must contain 'id', 'date', and 'value' columns.")
  }

  # Generate a sequence of all dates within the range of the data
  date_range <- seq(min(df_long$date, na.rm = TRUE),
                    max(df_long$date, na.rm = TRUE), by = "day")

  # Expand the data to include all dates for each 'id' and apply pre-processing
  df_exp <- df_long %>%
    group_by(id) %>%
    complete(date = date_range) %>% # Fill in missing dates
    mutate(value = prep_fun(value)) %>% # Apply the pre-processing function
    ungroup()

  return(df_exp)
}

# ------------------------------------------------------------------------------

#' Source an R Script with Notifications About Functions Loaded
#'
#' Sources an R script into a dedicated environment and lists the functions
#' that have been loaded.
#' @param file [mandatory] (character) A character string specifying the path
#'   to the R script to be sourced.
#' @return An environment containing the functions loaded from the sourced file.
#' @keywords internal
#'
source_with_notification <- function(file) {

  # Validate 'file' parameter
  if (!is.character(file) || length(file) != 1) {
    stop("'file' must be a single character string.")
  }

  # Check if the file exists
  if (!file.exists(file)) {
    stop(paste0("The specified file does not exist: ", file))
  }

  # Create a new environment for sourcing
  source_env <- new.env()

  # Source the file into the new environment
  tryCatch(
    sys.source(file, envir = source_env),
    error = function(e) {
      stop("Failed to source the file: ", e$message)
    }
  )

  # Get the list of functions in the source environment after sourcing
  loaded_functions <- ls(envir = source_env, pattern = "^.+$", all.names = TRUE)

  return(source_env)
}

# ------------------------------------------------------------------------------

#' Load External Post-Processing Functions
#'
#' Loads post-processing functions and their configuration from an external
#' folder named \code{postp}, located in the root directory of the database.
#' The folder must contain two files: \code{structure.json} (which defines the
#' post-processing configuration) and \code{functions.R} (which contains the R
#' function definitions to be used for post-processing). The function checks
#' for these files and loads the JSON configuration and sources the R script.
#' If the required files are missing, it stops execution and notifies the user
#' with instructions on how to set up the files correctly.
#' @param path [mandatory] (character) The path to the root directory
#'   where the database is located.
#' @return Returns a list of post-processing functions loaded from the
#'   \code{structure.json} file. The functions defined in \code{functions.R} are
#'   sourced and made available in the returned environment.
#' @note The \code{postp} folder must contain two files: \code{structure.json}
#'   and \code{functions.R}. The \code{structure.json} file contains mappings of
#'   variables to the post-processing functions, while \code{functions.R}
#'   contains the actual function definitions that will be used for
#'   post-processing.
#' @keywords internal
#' @importFrom jsonlite fromJSON
#'
load_external_postp <- function(path) {

  # Define the paths to the 'postp' folder and its required files
  postp_folder <- file.path(path, "postp")
  json_file <- file.path(postp_folder, "structure.json")
  r_script_file <- file.path(postp_folder, "functions.R")

  # Check if the 'postp' folder exists
  if (!dir.exists(postp_folder)) {
    stop(paste0(
      "The 'postp' folder is missing in the database root directory.\n",
      "Run 'init_postp()' to create the folder and initialize ",
      "'structure.json' and 'functions.R'."
    ))
  }

  # Check if the 'structure.json' file exists within the 'postp' folder
  if (!file.exists(json_file)) {
    stop(paste0(
      "The 'structure.json' file is missing in the 'postp' folder.\n",
      "To use 'external', make sure to include a valid 'structure.json' file ",
      "in the 'postp' folder."
    ))
  }

  # Check if the 'functions.R' file exists within the 'postp' folder
  if (!file.exists(r_script_file)) {
    stop(paste0(
      "The 'functions.R' file is missing in the 'postp' folder.\n",
      "To use 'external', make sure to include a valid 'functions.R' file ",
      "in the 'postp' folder."
    ))
  }

  # Load the post-processing functions configuration from the JSON file
  postp_funs_json <- tryCatch(
    jsonlite::fromJSON(json_file),
    error = function(e) {
      stop("Failed to parse 'structure.json': ", e$message)
    }
  )

  # Source the R script to load function definitions into a new environment
  source_env <- source_with_notification(r_script_file)

  # Convert function names from JSON to actual function objects in R
  postp_funs <- lapply(postp_funs_json, function(fun_list) {
    if (is.null(fun_list)) {
      return(NULL) # Return NULL if no functions are specified
    }
    # Flatten and map each function name to its corresponding R function object
    unlist(lapply(fun_list, function(fun_names) {
      lapply(fun_names, function(fun_name) {
        if (!exists(fun_name, envir = source_env)) {
          stop(paste0("Function '", fun_name,
                      "' not found in the sourced environment."))
        }
        fun_obj <- get(fun_name, envir = source_env)
        if (!is.function(fun_obj)) {
          stop(paste0("'", fun_name, "' is not a valid function."))
        }
        return(fun_obj)
      })
    }), recursive = FALSE)
  })

  # Return the loaded post-processing functions
  return(postp_funs)
}
