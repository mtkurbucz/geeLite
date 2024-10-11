# Main Functions ---------------------------------------------------------------

#' Fetch Variable Information from an SQLite Database
#'
#' Displays information on the available variables in the SQLite database
#' (\code{data/geelite.db}).
#' @param path [mandatory] (character) Path to the root directory of the
#' generated database.
#' @param format [mandatory] (character) A character string. Possible values
#' are data.frame (default) to return a \code{data.frame} object, or one of
#' \code{latex}, \code{html}, \code{pipe} (Pandoc's pipe tables), \code{simple}
#' (Pandoc's simple tables), and \code{rst} to be passed on to knitr for
#' formatting.
#' @return Returns the variable information in the selected format. If
#' \code{format = "data.frame"}, a \code{data.frame} is returned. For other
#' formats, the output is printed in the specified format and \code{NULL} is
#' returned.
#' @export
#' @examples
#' # Example: Printing the available variables
#' \dontrun{
#'   fetch_vars(path = "path/to/db")
#' }
#' @importFrom RSQLite dbDisconnect dbListTables dbReadTable SQLite
#' @importFrom dplyr arrange distinct pull select filter
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
  con <- db_connect(db_path = db_path)
  tables <- dbListTables(conn = con)

  # Filter out system tables, the 'grid' table, and sort the table names
  tables_drop <- c("grid", "spatial_ref_sys", "sqlite_sequence",
                   "geometry_columns")
  tables <- sort(setdiff(tables, tables_drop))
  tables_df <- data.frame(Table = tables, stringsAsFactors = FALSE)

  # Initialize a data frame to hold the detailed information
  detailed_info <- data.frame()

  id_counter <- 1 # Start ID counter from 1

  # Process each table
  for (i in seq_len(nrow(tables_df))) {
    table_name <- tables_df$Table[i]

    # Read the table, extract date columns, and get unique bands and statistics
    table_data <- dbReadTable(con, table_name, check.names = FALSE)
    date_cols <- grep("^\\d{4}_\\d{2}_\\d{2}$", names(table_data), value = TRUE)
    dates <- ymd(date_cols)

    # Find the first and last date, and calculate the native frequency
    first_date <- min(dates)
    last_date <- max(dates)
    mean_interval <- round(mean(as.numeric(diff(dates))),2)

    unique_bands_stats <- table_data %>%
      select(band, zonal_stat) %>%
      distinct() %>%
      arrange(band, zonal_stat)

    # For each combination of band and stat, create a row
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
      detailed_info <- rbind(detailed_info, tmp)
      id_counter <- id_counter + 1
    }
  }

  dbDisconnect(con)

  # Print or return the output based on user preference
  if (format != "data.frame") {
    print(
      kable(detailed_info, format = format,
            col.names = c("ID", "Variable", "Table", "Band", "Stat", "Start",
                          "End", "Freq (Days)"))
    )
  } else {
    return(detailed_info)
  }
}

# ------------------------------------------------------------------------------

#' Reading, Aggregating, and Processing the SQLite Database
#'
#' Reads, aggregates, and processes the SQLite database
#' (\code{data/geelite.db}).
#' @param path [mandatory] (character) Path to the root directory of the
#' generated database.
#' @param variables [optional] (character or integer) Names or IDs of the
#' variables to be read. Use the \code{fetch_vars} function to identify
#' available variables and IDs (default: \code{"all"}).
#' @param freq [optional] (character) The frequency for data aggregation.
#' Options include \code{"day"}, \code{"week"}, \code{"month"},
#' \code{"bimonth"}, \code{"quarter"}, \code{"season"}, \code{"halfyear"},
#' \code{"year"} (default: \code{"month"}).
#' @param prep_fun [optional] (function) A function for pre-processing time
#' series data prior to aggregation. For daily frequency, it handles missing
#' values using the specified method. The default is linear interpolation:
#' \code{function(x) na_interpolation(x, option = "linear")}, from the
#' \code{imputeTS} package.
#' @param aggr_funs [optional] (function or list) A function or a list of
#' functions for aggregating data to the specified frequency (\code{freq}).
#' Users can directly refer to variable names or IDs. The default function is
#' the mean: \code{function(x) mean(x, na.rm = TRUE)}.
#' @param postp_funs [optional] (function or list) A function or list of
#' functions applied to the time series data of a single bin after aggregation.
#' Users can directly refer to variable names or IDs. The default is
#' \code{NULL}, indicating no post-processing.
#' @return A list where the first element ('grid') is a simple feature (sf)
#' object, and subsequent elements are data frame objects corresponding to the
#' variables.
#' @export
#' @examples
#' # Example: Reading variables by IDs
#' \dontrun{
#' db_list <- read_db(path = "path/to/db",
#' variables = c(1, 3))
#' }
#' @importFrom imputeTS na_interpolation
#'
read_db <- function(path, variables = "all", freq = c("month", "day", "week",
                    "bimonth", "quarter", "season", "halfyear", "year"),
                    prep_fun = function(x)
                      na_interpolation(x, option = "linear"),
                    aggr_funs = function(x) mean(x, na.rm = TRUE),
                    postp_funs = NULL) {

  # Validate 'freq' parameter
  freq <- match.arg(freq)

  # Check if the 'postp_funs' is set to "external"
  if (identical(postp_funs, "external")) {
    postp_funs <- load_external_postp(path)
  }

  # Ensure 'aggr_funs' and 'postp_funs' are lists, name 'default' if unnamed
  if (is.function(aggr_funs) || all(sapply(aggr_funs, is.function))) {
    aggr_funs <- list(default = aggr_funs)
  }
  if (is.function(postp_funs) || all(sapply(postp_funs, is.function)) ||
      is.null(postp_funs)) {
    postp_funs <- list(default = postp_funs)
  }

  # Validate the 'path' parameter and retrieve the list of all variables
  variables_all <- fetch_vars(path, format = "data.frame")

  # Validate parameters and determine which variables to read
  variables <- validate_variables_param(variables, variables_all, prep_fun,
                                        aggr_funs, postp_funs)

  # Read data from the database for the selected variables
  db_list <- read_variables(path, variables, freq, prep_fun,
                            aggr_funs, postp_funs)

  return(db_list)
}

# ------------------------------------------------------------------------------

#' Initialize post-processing folder and files
#'
#' This function creates a \code{postp} folder at the specified path and adds
#' two empty files: \code{structure.json} and \code{functions.R}. These files
#' are intended to hold the configuration and post-processing functions used in
#' the application.
#' @param path \code{character}. The path to the root directory where the
#' \code{postp} folder should be created. The \code{postp} folder will contain
#' the \code{structure.json} and \code{functions.R} files.
#' @param verbose [optional] (logical) Display messages (default: \code{TRUE}).
#' @return This function does not return a value. It creates a folder and files
#' as a side effect.
#' @details
#' The \code{structure.json} file is initialized with a default JSON structure:
#' \code{"default": null}. This file is intended for mapping variables to
#' post-processing functions. The \code{functions.R} file is created with a
#' placeholder comment indicating where to define the R functions for
#' post-processing. If the `\code{postp} folder already exists, an error will
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

#' Read Variables from Database
#'
#' Reads the specified variables from the SQLite database.
#' @param path [mandatory] (character) Path to the root directory of the
#' generated database.
#' @param variables [mandatory] (character) A vector of variable names to read.
#' @param freq [mandatory] (character) Specifies the frequency to aggregate the
#' data.
#' @param prep_fun [mandatory] (function) Function used for pre-processing.
#' @param aggr_funs [mandatory] (function or list) Aggregation function(s).
#' @param postp_funs [optional] (function or list) Post-processing function(s).
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
  con <- db_connect(db_path = db_path)

  # Read the 'grid' table as an sf object
  db_list <- list(grid = st_read(con, "grid", quiet = TRUE) %>%
                    select(-1) %>% rename(geometry = GEOMETRY))

  # Get the mapping of variables to tables, bands, and stats
  variables_all <- fetch_vars(path, format = "data.frame")
  variables_info <- variables_all[match(variables, variables_all$Variable), ]

  # Initialize a list to store variables data
  variables_data <- vector("list", length(variables))

  last_table_read <- NULL

  # Process each variable
  for (i in seq_len(nrow(variables_info))) {
    var_info <- variables_info[i, ]
    variable_name <- var_info$Variable
    table_name <- var_info$Table
    band_name <- var_info$Band
    stat_name <- var_info$Stat

    # Read the table data if not already read
    if (is.null(last_table_read) || last_table_read != table_name) {
      table_data <- dbReadTable(con, table_name, check.names = FALSE)
      last_table_read <- table_name
    }

    # Filter the data for this variable
    table_data_subset <- table_data %>%
      filter(band == band_name & zonal_stat == stat_name)

    if (nrow(table_data_subset) == 0) {
      warning(paste("No data found for variable", variable_name))
      next
    }

    if (is.null(freq)) {
      # No aggregation, set preprocess, aggregation, postprocess to NA
      variable_data <- table_data_subset %>%
        select(-band, -zonal_stat) %>%
        mutate(preprocess = NA_character_,
               aggregation = NA_character_,
               postprocess = NA_character_) %>%
        select(id, aggregation, preprocess, postprocess, everything())
    } else {
      # Get the body of the prep_fun as text
      preprocess_body <- paste(deparse(body(prep_fun)), collapse = "\n")
      # Perform aggregation
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

    # Store the data in the list, maintaining the order
    variables_data[[i]] <- variable_data
  }

  names(variables_data) <- variables

  # Disconnect from the SQLite database
  dbDisconnect(con)

  # Combine the grid and variables data
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
#' data.
#' @param prep_fun [mandatory] (function) Function used for pre-processing.
#' @param aggr_funs [mandatory] (function or list) Aggregation function(s).
#' @param postp_funs [optional] (function or list) Post-processing function(s).
#' @param variable_name [mandatory] (character) Name of the current variable.
#' @param preprocess_body [mandatory] (character) Body of the prep_fun function.
#' @return A data frame in wide format with aggregated values.
#' @keywords internal
#' @importFrom magrittr %>%
#' @importFrom lubridate floor_date ymd
#' @importFrom tidyr all_of pivot_longer pivot_wider
#' @importFrom dplyr arrange bind_rows group_by mutate summarise
#'
aggr_by_freq <- function(table, freq, prep_fun, aggr_funs,
                         postp_funs, variable_name, preprocess_body) {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  . <- id <- value <- freq_date <- aggregation <- postprocess <- NULL

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

  # Get the aggregation function(s)
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

  aggr_results <- list()
  for (i in seq_along(funcs)) {
    aggr_fun <- funcs[[i]]
    aggregation_body <- paste(deparse(body(aggr_fun)), collapse = "\n")
    df_group <- df_exp %>%
      group_by(id, freq_date) %>%
      summarise(value = aggr_fun(value), .groups = "drop") %>%
      mutate(aggregation = aggregation_body,
             preprocess = preprocess_body)
    aggr_results[[i]] <- df_group
  }
  df_combined_aggr <- bind_rows(aggr_results)

  # Apply post-processing functions independently and store results
  postp_funcs <- postp_funs[[variable_name]]
  if (is.null(postp_funcs)) {
    postp_funcs <- postp_funs[["default"]]
  }
  if (is.null(postp_funcs)) {
    # No post-processing functions, keep the aggregated data as is
    df_final <- df_combined_aggr %>%
      mutate(postprocess = NA_character_)
  } else {
    if (is.function(postp_funcs)) {
      postp_funcs <- list(postp_funcs)
    }
    postp_results <- list()
    for (i in seq_along(postp_funcs)) {
      postp_fun <- postp_funcs[[i]]
      postprocess_body <- paste(deparse(body(postp_fun)), collapse = "\n")
      df_postp <- df_combined_aggr %>%
        mutate(value = postp_fun(value),
               postprocess = postprocess_body)
      postp_results[[i]] <- df_postp
    }
    df_final <- bind_rows(postp_results)
  }

  # Convert back to wide format and format the date columns
  df_wide <- df_final %>%
    mutate(freq_date = format(freq_date, "%Y_%m_%d")) %>%
    pivot_wider(names_from = freq_date, values_from = value) %>%
    arrange(id, aggregation, postprocess)

  return(df_wide)
}

# ------------------------------------------------------------------------------

#' Expand Data to Daily Frequency
#'
#' Expands the input data frame to a daily frequency, filling in any missing
#' dates within the observed range.
#' @param df_long [mandatory] (data.frame) A long-format data frame with at
#' least the columns \code{id} and \code{date}.
#' @param prep_fun [mandatory] (function) Function used for pre-processing.
#' @return A data frame with daily dates and preprocessed \code{value} column.
#' @keywords internal
#' @importFrom dplyr group_by mutate ungroup
#' @importFrom tidyr complete
#'
expand_to_daily <- function(df_long, prep_fun) {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  id <- value <- NULL

  # Generate the full range of dates
  date_range <- seq(min(df_long$date), max(df_long$date), by = "day")

  # Expand the data to daily frequency and apply the preprocessing function
  df_exp <- df_long %>%
    group_by(id) %>%
    complete(date = date_range) %>%
    mutate(value = prep_fun(value)) %>%
    ungroup()

  return(df_exp)
}

# ------------------------------------------------------------------------------

#' Source an R script with notifications about functions loaded and overwritten
#'
#' This function sources an R script, listing the functions that have been loaded 
#' into the global environment. It also notifies the user if any functions 
#' from the sourced file overwrite existing functions in the global environment.
#'
#' @param file A character string specifying the path to the R script to be sourced.
#' @details
#' The function compares the functions in the global environment before and after 
#' sourcing the specified file. It identifies newly loaded functions and any 
#' functions that have been overwritten. A message is displayed with the names 
#' of the new functions, and a warning is given for overwritten functions.
#' 
#' @return This function does not return any value. It is used for its side effect 
#' of sourcing an R script and printing messages about loaded and overwritten functions.
#' @examples
#' \dontrun{
#'   # Source 'functions.R' and get notifications about loaded functions
#'   source_with_notification("functions.R")
#' }
#' 
#' @export
source_with_notification <- function(file) {
  # Get the list of functions in the global environment before sourcing
  before <- ls(envir = .GlobalEnv, pattern = "function$")
  
  # Source the file
  source(file)
  
  # Get the list of functions in the global environment after sourcing
  after <- ls(envir = .GlobalEnv, pattern = "function$")
  
  # Determine which functions are new
  new_functions <- setdiff(after, before)
  
  # Determine which functions have been overwritten
  overwritten_functions <- intersect(after, before)
  
  # Output the results
  if (length(new_functions) > 0) {
    cat("New functions loaded from", file, ":\n")
    cat(paste(new_functions, collapse = ", "), "\n")
  } else {
    cat("No new functions loaded from", file, "\n")
  }
  
  if (length(overwritten_functions) > 0) {
    cat("Warning: The following functions have been overwritten:\n")
    cat(paste(overwritten_functions, collapse = ", "), "\n")
  }
}

#' Load External Post-Processing Functions
#'
#' This function loads post-processing functions and their configuration from
#' an external folder named \code{postp}, located in the root directory of the
#' database. The folder must contain two files: \code{structure.json} (which
#' defines the post-processing configuration) and \code{functions.R} (which
#' contains the R function definitions to be used for post-processing).
#'
#' The function checks for these files and loads the JSON configuration and
#' sources the R script. If the required files are missing, it stops execution
#' and notifies the user with instructions on how to set up the files correctly.
#' @param path [mandatory] \code{character} The path to the root directory
#' where the database is located.
#' @return Returns a list of post-processing functions loaded from the
#' \code{structure.json} file. The functions defined in \code{functions.R} are
#' sourced and made available in the global environment.
#' @note The \code{postp} folder must contain two files: \code{structure.json}
#' and \code{functions.R}. The \code{structure.json} file contains mappings of
#' variables to the post-processing functions, while \code{functions.R}
#' contains the actual function definitions that will be used for
#' post-processing.
#' @keywords internal
#' @importFrom jsonlite fromJSON
#'
load_external_postp <- function(path) {

  # Define the postp folder and file paths
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

  # Check if the JSON file exists
  if (!file.exists(json_file)) {
    stop(paste0(
      "The 'structure.json' file is missing in the 'postp' folder.\n",
      "To use 'external', make sure to include a valid 'structure.json' file ",
      "in the 'postp' folder."
    ))
  }

  # Check if the R script file exists
  if (!file.exists(r_script_file)) {
    stop(paste0(
      "The 'functions.R' file is missing in the 'postp' folder.\n",
      "To use 'external', make sure to include a valid 'functions.R' file ",
      "in the 'postp' folder."
    ))
  }

  # Load the post-processing functions configuration from the JSON file
  postp_funs_json <- fromJSON(json_file)

  # Source the R script to load function definitions
  source_with_notification(r_script_file)

  # Convert function names from JSON to actual function objects in R
  postp_funs <- lapply(postp_funs_json, function(fun_list) {
    if (is.null(fun_list)) {
      return(NULL)
    }
    # Flatten and map each function name to an actual R function object
    unlist(lapply(fun_list, function(fun_names) {
      lapply(fun_names, function(fun_name) {
        if (!exists(fun_name, envir = .GlobalEnv)) {
          stop(paste0("Function '", fun_name,
                      "' not found in the global environment."))
        }
        fun_obj <- get(fun_name, envir = .GlobalEnv)
        if (!is.function(fun_obj)) {
          stop(paste0("'", fun_name, "' is not a valid function."))
        }
        return(fun_obj)
      })
    }), recursive = FALSE)
  })

  # Return the loaded post-processing functions for use in the main function
  return(postp_funs)
}
