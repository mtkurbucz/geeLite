# Main Function ----------------------------------------------------------------

#' Build and Update the Grid Statistics Database
#'
#' Collects and stores grid statistics from Google Earth Engine (GEE) data in
#' SQLite format (\code{data/geelite.db}), initializes CLI files
#' (\code{cli/...}), and initializes or updates the state
#' (\code{state/state.json}) and log (\code{log/log.txt}) files.
#' @param path [mandatory] (character) Path to the root directory of the
#' generated database.
#' @param conda [optional] (character) Name of the virtual Conda environment
#' installed and used by the \code{rgee} package (default: \code{"rgee"}).
#' @param user [optional] (character) Used to create a directory within the
#' path \code{~/.config/earthengine/}. This directory stores all the
#' credentials associated with a specific Google account (default: \code{NULL}).
#' @param rebuild [optional] (logical) If set to \code{TRUE}, the database and
#' its supplementary files will be overwritten based on the configuration file
#' (default: \code{FALSE}).
#' @param verbose [optional] (logical) Display computation status and messages
#' (default: \code{TRUE}).
#' @export
#' @examples
#' # Example: Build a Grid Statistics Database
#' \dontrun{
#'   run_geelite(path = "path/to/root/directory")
#' }
#'
run_geelite <- function(path, conda = "rgee", user = NULL, rebuild = FALSE,
                        verbose = TRUE) {

  # Validate parameters
  params <- list(path = path, conda = conda, user = user, rebuild = rebuild,
                 verbose = verbose)
  validate_params(params)

  # Set the working directory
  setwd(path)

  # Collect and store grid statistics and create or update supplementary files
  print_version(verbose)            # Display version if 'verbose' is TRUE
  set_dirs(rebuild)                 # Create required subdirectories in the path
  task <- get_task()                # Define task using config and state files
  grid <- get_grid(task)            # Generate the grid based on the task
  set_depend(conda, user, verbose)  # Activate necessary dependencies
  compile_db(task, grid, verbose)   # Build or update the database
  set_cli(path, FALSE)              # Initialize CLI files

}

# Internal Functions -----------------------------------------------------------

#' Display geeLite Package Version
#'
#' Displays the version of the \code{geeLite} package along with formatted
#' headers.
#' @param verbose [mandatory] (logical) If \code{TRUE}, the version of the
#' \code{geeLite} package is printed.
#' @keywords internal
#' @importFrom cli cli_h1
#' @importFrom utils packageVersion
#'
print_version <- function(verbose) {
  if (verbose) {
    version <- as.character(packageVersion("geeLite"))
    cli_h1("")
    cat("\033[1mgeeLite R Package - Version:", version, "\033[0m")
    cli_h1("")
    cat("\n")
  }
}

# ------------------------------------------------------------------------------

#' Generate Necessary Directories
#'
#' Generates \code{"data"}, \code{"log"}, \code{"cli"}, and \code{"state"}
#' subdirectories at the specified path.
#' @param rebuild [optional] (logical) If \code{TRUE}, existing directories
#' will be removed and recreated.
#' @keywords internal
#'
set_dirs <- function(rebuild) {
  dirs <- c("data", "log", "cli", "state")
  for (dir in dirs) {
    if (dir.exists(dir) && rebuild) {
      # If the directory exists and rebuild is TRUE, remove and recreate it
      unlink(dir, recursive = TRUE)
      dir.create(dir)
    } else if (!dir.exists(dir)) {
      # If the directory does not exist, create it
      dir.create(dir)
    }
  }
}

# ------------------------------------------------------------------------------

#' Generate Session Task
#'
#' Generates a session task based on the configuration and state files.
#' @return A list representing the session task.
#' @keywords internal
#' @importFrom jsonlite fromJSON
#'
get_task <- function() {

  config <- fromJSON("config/config.json")
  state_path <- "state/state.json"

  if (!file.exists(state_path)) {
    # In the case of (re)build, the session is driven by the configuration file
    task <- config
  } else {
    # New values are marked with '+' and removed values with '-'
    state <- fromJSON(state_path)
    task <- list()
    task$regions <- compare_vectors(config$regions, state$regions)
    task$source <- compare_lists(config$source, state$source)
    task$start <- state$start
    task$scale <- state$scale
    task$resol <- state$resol
    task$limit <- config$limit
  }

  return(task)
}

# ------------------------------------------------------------------------------

#' Compare Vectors and Highlight Differences
#'
#' Compares two vectors and indicates added ('+') and removed ('-') values.
#' @param vector_1 [mandatory] (character or integer) First vector to compare.
#' @param vector_2 [mandatory] (character or integer) Second vector to compare.
#' @return A vector showing added and removed values marked with '+' and '-'.
#' @keywords internal
#'
compare_vectors <- function(vector_1, vector_2) {

  added <- setdiff(vector_1, vector_2)
  removed <- setdiff(vector_2, vector_1)
  common <- intersect(vector_1, vector_2)

  result <- c(paste0("+", added), common, paste0("-", removed))
  result <- result[nchar(result) > 1]

  return(result)
}

# ------------------------------------------------------------------------------

#' Compare Lists and Highlight Differences
#'
#' Compares two lists and mark new values with '+' and removed values with '-'.
#' @param list_1 [mandatory] (list) First list to compare.
#' @param list_2 [mandatory] (list) Second list to compare.
#' @return A list showing added and removed values marked with '+' and '-'.
#' @keywords internal
#'
compare_lists <- function(list_1, list_2) {

  result <- list()

  # Compare elements in 'list_1' with 'list_2'
  for (key_1 in names(list_1)) {
    if (key_1 %in% names(list_2)) {
      result[[key_1]] <- list()
      # Compare elements in 'list_1[[key_1]]' with 'list_2[[key_1]]'
      for (key_2 in names(list_1[[key_1]])) {
        if (key_2 %in% names(list_2[[key_1]])) {
          # Compare vectors within nested lists using compare_vectors function
          result[[key_1]][[key_2]] <- compare_vectors(list_1[[key_1]][[key_2]],
                                                      list_2[[key_1]][[key_2]])
        } else {
          # Mark added elements in 'list_1[[key_1]]'
          result[[key_1]][[paste0("+", key_2)]] <- list_1[[key_1]][[key_2]]
        }
      }
      # Check for removed elements in 'list_2[[key_1]]'
      for (key_2 in names(list_2[[key_1]])) {
        if (!(key_2 %in% names(list_1[[key_1]]))) {
          result[[key_1]][[paste0("-", key_2)]] <- list_2[[key_1]][[key_2]]
        }
      }
    } else {
      # Mark added elements in 'list_1' that are not in 'list_2'
      result[[paste0("+", key_1)]] <- list_1[[key_1]]
    }
  }

  # Check for removed elements in 'list_2' that are not in 'list_1'
  for (key_1 in names(list_2)) {
    if (!(key_1 %in% names(list_1))) {
      result[[paste0("-", key_1)]] <- list_2[[key_1]]
    }
  }

  return(result)
}

# ------------------------------------------------------------------------------

#' Set Dependencies
#'
#' Authenticates the GEE account and activates the specified Conda environment.
#' @param conda [optional] (character) Name of the virtual Conda environment
#' installed and used by the \code{rgee} package (default: \code{"rgee"}).
#' @param user [optional] (character) Used to create a directory within the
#' path \code{~/.config/earthengine/}. This directory stores all the
#' credentials associated with a specific Google account (default: \code{NULL}).
#' @param verbose [optional] (logical) Display messages (default: \code{TRUE}).
#' @keywords internal
#' @importFrom cli rule
#' @importFrom crayon bold blue green
#' @importFrom reticulate use_condaenv py_config py_run_string
#' @importFrom rgee ee_get_earthengine_path ee_Initialize ee_user_info
#'
set_depend <- function(conda = "rgee", user = NULL, verbose = TRUE) {

  # Activate the specified Conda environment
  use_condaenv(conda, required = TRUE)

  tryCatch({

    # Attempt to authenticate and initialize GEE
    ee_Initialize(user = user, quiet = !verbose)

  }, error = function(e) {

    # Define the GEE credentials path based on the user's subfolder
    if (!is.null(user)) {
      # Construct the full path to the user's credentials
      credentials_path <- paste0(
        gsub("\\\\", "/", ee_get_earthengine_path()), user, "/credentials"
      )

      # Run Python code to initialize GEE using the custom credentials path
      py_run_string(paste0(
        "import ee; ee.Initialize(credentials='", credentials_path, "')"
      ))

    } else {
      # If no user is specified, initialize GEE with default credentials
      py_run_string("import ee; ee.Initialize()")
    }

    # Display session information if verbose is TRUE
    if (verbose) {
      account_info <- ee_user_info(quiet = TRUE)[1]
      py_path_info <- py_config()$python

      cat(
        green("\u2714"),
        blue("Earth Engine account:"),
        bold(green(account_info)),
        "\n"
      )

      cat(
        green("\u2714"),
        blue("Python Path:"),
        bold(green(py_path_info)),
        "\n"
      )

      cat("\n", rule(), "\n")
    }

  })

  if (verbose) {
    cat("\n")
  }
}

# ------------------------------------------------------------------------------

#' Obtain H3 Hexagonal Grid
#'
#' Retrieves or creates the grid for the task based on the specified regions
#' and resolution.
#' @param task [mandatory] (list) Session task that specifies the parameters
#' for data collection.
#' @keywords internal
#' @return A simple features (sf) object containing grid data.
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom sf st_crs st_geometry
#'
get_grid <- function(task) {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  iso <- NULL

  # Create a list from the 'regions' vector based on its marked elements:
  # - $add:     Regions marked with '+' (to be added)
  # - $drop:    Regions marked with '-' (to be dropped)
  # - $use:     Regions that are unmarked or marked with '+'
  # - $use_add: TRUE for regions in $use marked with '+'
  regions <- process_vector(task$regions)
  db_path <- "data/geelite.db"

  if (file.exists(db_path)) { # Update existing grid data

    # Read grid, excluding regions to be removed ('-')
    grid <- read_grid() %>% filter(!iso %in% regions$drop)

    # Add new regions ('+')
    if (length(regions$add) > 0) {

      # Transform the grid format to facilitate merge with added bins
      grid <- grid %>%
        select(-attr(st_geometry(grid), "geometry")) %>%
        as.data.frame()

      # Define shapes of added regions and create bins based on them
      shapes <- get_shapes(regions$add)
      grid_add <- get_bins(shapes, task$resol)

      # Combine existing grid with newly added bins
      grid <- rbind(grid, grid_add)
      sf::st_crs(grid$geometry) <- sf::st_crs(grid_add$geometry)$input

    }

    # Update grid if new regions ('+') or removed regions ('-') exist
    if (length(regions$add) > 0 || length(regions$drop) > 0) {
      write_grid(grid)
    }

  } else { # (Re)build grid data if no existing database is found

    # Define shapes of regions and create bins based on them
    shapes <- get_shapes(regions$use)
    grid <- get_bins(shapes, task$resol)

    # Write grid
    write_grid(grid)

  }

  return(grid)
}

# ------------------------------------------------------------------------------

#' Get Shapes for Specified Regions
#'
#' Retrieves the shapes of specified regions, which can be at the country or
#' state level.
#' @param regions [mandatory] (character) A vector containing ISO 3166-2 region
#' codes. Country codes are two characters long, while state codes contain
#' additional characters.
#' @return A simple features (sf) object containing the shapes of the specified
#' regions.
#' @keywords internal
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select rename
#' @importFrom rnaturalearth ne_countries ne_states
#'
get_shapes <- function(regions) {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  . <- geometry <- iso_a2_eh <- iso_3166_2 <- NULL

  # Separate regions into country-level and state-level based on string length
  country <- regions[nchar(regions) == 2]
  state <- regions[nchar(regions) > 2]

  # If there are country-level regions
  if (length(country) > 0) {
    # Retrieve country shapes and filter by the specified regions
    shapes <- ne_countries(scale = "medium", returnclass = "sf") %>%
      filter(iso_a2_eh %in% regions) %>%
      select(iso_a2_eh, geometry) %>%
      rename(iso = iso_a2_eh)
  }

  # If there are state-level regions
  if (length(state) > 0) {
    # Retrieve state shapes and filter by the specified regions
    states <- ne_states(returnclass = "sf") %>%
      .[.$iso_3166_2 %in% regions, c("iso_3166_2", "geometry")] %>%
      rename(iso = iso_3166_2)
    # Combine country and state shapes if country shapes exist
    if (exists("shapes")) {
      shapes <- rbind(shapes, states)
    } else {
      shapes <- states
    }
  }

  return(shapes)
}

# ------------------------------------------------------------------------------

#' Get H3 Bins for Shapes
#'
#' Generates H3 bins for the provided shapes at the specified resolution.
#' @param shapes [mandatory] (sf) A simple features object containing
#' geometries used for generating H3 bins.
#' @param resol [mandatory] (integer) An integer specifying the resolution of
#' the H3 grid.
#' @return A data frame containing the H3 bins with columns for region ISO
#' 3166-2 codes, bin IDs, and geometry.
#' @keywords internal
#' @importFrom dplyr select
#' @importFrom h3jsr cell_to_polygon polygon_to_cells
#'
get_bins <- function(shapes, resol) {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  geometry <- id <- iso <- NULL

  # Iterate over each shape
  for (i in 1:nrow(shapes)) {

    # Get the current shape
    shape <- shapes[i,]

    # Generate H3 bin IDs for the shape
    bin_id <- polygon_to_cells(shape$geometry, res = resol)

    # Convert bin IDs to polygons
    bin <- as.data.frame(cell_to_polygon(bin_id))

    if (i == 1) {
      # Initialize the bins data frame for the first shape
      bin$iso <- shapes$iso[i]
      bin$id <- unlist(bin_id)
      bins <- bin
    } else {
      # Append the bin data for subsequent shapes
      bin$iso <- shapes$iso[i]
      bin$id <- unlist(bin_id)
      bins <- rbind(bins, bin)
    }
  }

  # Select relevant columns
  bins <- bins %>% select(iso, id, geometry)
  return(bins)
}

# ------------------------------------------------------------------------------

#' Read Grid from Database
#'
#' Reads the H3 grid from the specified SQLite database
#' (\code{data/geelite.db}).
#' @return A simple features (sf) object containing the grid data.
#' @keywords internal
#' @importFrom sf st_read
#' @importFrom magrittr %>%
#' @importFrom dplyr rename select
#' @importFrom RSQLite dbConnect dbDisconnect SQLite
#'
read_grid <- function() {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  GEOMETRY <- NULL

  # Connect to the SQLite database
  con <- dbConnect(SQLite(), dbname = "data/geelite.db")

  # Read the grid table from the database
  grid <- st_read(con, "grid", quiet = TRUE) %>%
    select(-1) %>% rename(geometry = GEOMETRY)

  # Disconnect from the database
  dbDisconnect(con)

  return(grid)
}

# ------------------------------------------------------------------------------

#' Write Grid to Database
#'
#' Writes the H3 grid to the specified SQLite database (\code{data/geelite.db}).
#' @param grid [mandatory] (sf) Simple features object containing the grid data
#' to be written into the database.
#' @keywords internal
#' @importFrom sf st_write
#'
write_grid <- function(grid) {
  # Write the grid data frame to the SQLite database
  st_write(obj = grid, dsn = "data/geelite.db", layer = "grid",
           driver = "SQLite", append = FALSE, quiet = TRUE)
}

# ------------------------------------------------------------------------------

#' Collect and Process Grid Statistics
#'
#' This function retrieves and processes grid statistics from Google Earth
#' Engine (GEE) based on the specified session task. The collected data is
#' stored in SQLite format (\code{data/geelite.db}), along with supplementary
#' files such as CLI files (\code{cli/...}), the state file
#' (\code{state/state.json}), and the log file (\code{log/log.txt}).
#' @param task [mandatory] (list) Session task that specifies the parameters
#' for data collection.
#' @param grid [mandatory] (sf) Simple features object containing the
#' geometries of the regions of interest.
#' @param verbose [mandatory] (logical) Display messages and progress status.
#' @keywords internal
#' @importFrom purrr map
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom stats setNames
#' @importFrom progress progress_bar
#' @importFrom RSQLite dbConnect dbReadTable
#'
compile_db <- function(task, grid, verbose) {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  id <- stat <- NULL

  # Initialize an empty list to track 'datasets', 'bands', and 'stats'
  source_for_state <- list()

  # Set up progress bar if 'verbose' is TRUE
  pb <- set_progress_bar(verbose)

  # Define the path for the state file and verify its existence
  state_path <- "state/state.json"
  database_new <- !file.exists(state_path)

  # Determine the scale for data collection, if specified
  scale <- if (length(task$scale) > 0) task$scale else NULL

  # Get reducers for grid statistics calculation
  reducers <- get_reducers()

  # Create a list from the 'regions' vector based on its marked elements:
  # - $add:     Regions marked with '+' (to be added)
  # - $drop:    Regions marked with '-' (to be dropped)
  # - $use:     Regions that are unmarked or marked with '+'
  # - $use_add: TRUE for regions in $use marked with '+'
  regions <- process_vector(task$regions)
  regions_new <- regions$use_add

  # Flag the bins of the added regions in the 'grid'
  grid$add <- grid$iso %in% regions$add

  # Process 'datasets' similar to 'regions'
  datasets <- process_vector(names(task$source))

  # If updating: Remove tables from database marked with '-'
  remove_tables(datasets$drop)

  # Iterate through each 'dataset'
  for (i in seq_along(datasets$use)) {

    # Select the current 'dataset' and determine if it is newly added ('+')
    dataset <- datasets$use[i]
    dataset_new <- datasets$use_add[i]

    # Process 'bands' similar to 'regions'
    bands <- process_vector(names(task$source[[i]]))

    # If updating:
    # - Retrieve the 'table' related to the current 'dataset'
    # - Identify the most recent date recorded in the table
    if (!database_new && !dataset_new) {
      con <- dbConnect(SQLite(), dbname = "data/geelite.db")
      db_table <- dbReadTable(con, dataset, check.names = FALSE) %>%
        filter(id %in% grid$id)
      latest_date <- as.Date(gsub("_", "-", colnames(db_table)[ncol(db_table)]))
      dbDisconnect(con)
    }

    # Iterate through each 'band'
    for (j in seq_along(bands$use)) {

      # Process 'stats' similarly to 'regions' and assign functions
      stats <- process_vector(vector = task$source[[i]][[j]])
      stat_funs <- map(stats$use, ~ reducers[[.]])
      stats_new <- stats$use_add

      # Select the current 'band' and determine if it is newly added ('+')
      band <- bands$use[j]
      band_new <- bands$use_add[j]

      # Update the 'source_for_state' list
      source_for_state[[dataset]][[band]] <- stats$use

      # If updating: Remove 'bands' and 'stats' from 'db_table' marked with '-'
      if (j == 1) {
        if (length(bands$drop) > 0 || length(stats$drop) > 0) {
          db_table <- db_table %>%
            filter(!(band %in% bands$drop) & !(stat %in% stats$drop))
        }
      }

      # Determine the type of current data collection cases:
      # - 1: All build (db: new | (all regs | dataset | band | all stats): '+')
      # - 2: All update (db: !new & all elements: unmarked)
      # - 3: Mixed (any regs: '+' | any but not all stats: '+')
      cases <- get_cases(database_new, dataset_new, band_new, stats_new,
                         regions_new)

      # Collect images and related information:
      # - $build:        Images for the building procedure
      # - $update:       Images for the updating procedure
      # - $batch_size:   Batch size
      # - $skip_band:    Band is up-to-date & (regs | stats: unmarked)
      # - $skip_update:  Band is up-to-date & (any regs | any stats: '+')
      images <- get_images(task, cases, dataset, band, regions_new,
                           get0("latest_date", ifnotfound = NULL))

      # Skip 'band' if it is up-to-date and 'regions' and 'stats' are unmarked
      if (!images$skip_band) {

        # Divide 'grid' into batches:
        # - $b1: Batch_1
        # - $b2: Batch_2 (only if some, but not all, regs: '+')
        batches <- get_batches(cases, grid, images$batch_size)

        # Remove unmarked 'stats' if 'band' is up-to-date and no new regs added
        if (images$skip_update && !any(regions_new)) {
          stats$use <- stats$use[stats$use_add]
          stat_funs <- stat_funs[stats$use_add]
        }

        # Determine the step size for the progress bar
        source_length <- length(datasets$use) * length(bands$use) *
          length(stats$use)
        pb_step <- 100 / source_length / length(batches$b1)

        # Iterate through each 'stat'
        for (k in seq_along(stats$use)) {

          # Determine the type of the current data collection case:
          # - 1: Build (If 'cases' == 1 | stats_new[k])
          # - 2: Update (If 'cases' == 2 | !stats_new[k])
          case <- ifelse(cases %in% c(1, 2), cases, ifelse(stats_new[k], 1, 2))

          # Iterate through each 'batch'
          for (l in seq_along(batches$b1)) {

            # Update the progress bar display
            if (verbose) {
              pb$tick(pb_step)
            }

            # Collect grid statistics from GEE
            grid_stats <- extract_grid_stats(
              case = case,
              images = images,
              grid = grid,
              batch_1 = batches$b1[[l]],
              batch_2 = batches$b2[[l]],
              band = band,
              stat = stats$use[k],
              stat_fun = stat_funs[[k]],
              scale = scale,
              region_new = any(regions_new),
              skip_update = images$skip_update,
              grid_stats = get0("grid_stats", ifnotfound = NULL)
            )

          }
        }
      }
    }

    # Check if 'grid_stats' exists
    if (exists("grid_stats")) {

      # Write 'grid_stats' to the database
      write_grid_stats(
        database_new = database_new,
        dataset_new = dataset_new,
        dataset = datasets$use[i],
        grid_stats = grid_stats,
        db_table = get0("db_table", ifnotfound = NULL)
      )

    }
  }

  # Check if 'grid_stats' exists
  if (exists("grid_stats")) {

    # Write the state file
    write_state_file(task, regions$use, source_for_state)

    # Write the log file
    write_log_file(!database_new)

    # Print output messages if 'verbose' is TRUE
    output_message(gen_messages(!database_new), verbose)

  } else {
    cli_alert_info("Database is up-to-date.")
    cat("\n")
  }

}

# ------------------------------------------------------------------------------

#' Set Progress Bar
#'
#' Initializes a progress bar if 'verbose' is \code{TRUE}.
#' @param verbose [mandatory] (logical) If \code{TRUE}, a progress bar is
#' initialized.
#' @return A progress bar (environment) if 'verbose' is \code{TRUE}, or
#' \code{NULL} if \code{FALSE}.
#' @keywords internal
#' @importFrom progress progress_bar
#'
set_progress_bar <- function(verbose) {
  if (verbose) {
    pb <- progress_bar$new(
      format = "Progress: [:bar] :percent, eta: :eta",
      total = 100,
      width = 80,
      clear = FALSE
    )
  } else {
    pb <- NULL
  }
  return(pb)
}

# ------------------------------------------------------------------------------

#' Process Marked Vector
#'
#' Generates a list categorizing items based on their marks: items to be added
#' ('+'), items to be dropped ('-'), items to be used (unmarked or marked with
#' '+'), and indices of '+' items within the used category.
#' @param vector [mandatory] (character) A character vector containing elements
#' marked with '+' and '-' prefixes.
#' @return A list with the following components:
#' \describe{
#'  \item{$add}{Items marked with '+'}
#'  \item{$drop}{Items marked with '-'}
#'  \item{$use}{Items that are unmarked or marked with '+'}
#'  \item{$use_add}{TRUE for items marked with '+' within the $use category}
#' }
#' @keywords internal
#' @importFrom stringr str_detect str_sub
#'
process_vector <- function(vector) {

  # Identify indices for items marked with '-' or '+'
  drop_idx <- str_detect(vector, "^\\-")  # Index for items marked with '-'
  add_idx <- str_detect(vector, "^\\+")   # Index for items marked with '+'

  # Filter out items not marked with '-'
  use <- vector[!drop_idx]

  # Identify indices of items in 'use' marked with '+'
  use_add <- str_detect(use, "^\\+")

  # Remove '+' prefix from items marked with '+'
  use[use_add] <- str_sub(use[use_add], 2)

  return(list(
    drop = str_sub(vector[drop_idx], 2),  # Extract items marked with '-'
    add = str_sub(vector[add_idx], 2),    # Extract items marked with '+'
    use = use,                            # Final list of items to use
    use_add = use_add                     # Indices of '+' items in use
  ))
}

# ------------------------------------------------------------------------------

#' Get Reducers
#'
#' Initializes a list of reducers for grid statistics calculation.
#' @return A list of available reducers.
#' @keywords internal
#'
get_reducers <- function() {
  return(list(
    mean = rgee::ee$Reducer$mean(),
    sum = rgee::ee$Reducer$sum(),
    max = rgee::ee$Reducer$max(),
    min = rgee::ee$Reducer$min(),
    median = rgee::ee$Reducer$median(),
    sd = rgee::ee$Reducer$stdDev()
  ))
}

# ------------------------------------------------------------------------------

#' Remove Tables from the Database
#'
#' Removes tables from the database if their corresponding dataset is initially
#' marked for deletion ('-').
#' @param tables_drop [mandatory] (character) A character vector of tables to
#' be deleted.
#' @keywords internal
#' @importFrom RSQLite dbConnect dbDisconnect dbRemoveTable SQLite
#'
remove_tables <- function(tables_drop) {
  if (length(tables_drop) > 0) {
    con <- dbConnect(SQLite(), dbname = "data/geelite.db")
    dbRemoveTable(con, tables_drop)
    dbDisconnect(con)
  }
}

# ------------------------------------------------------------------------------

#' Determine the Cases of Data Collection Requests
#'
#' Determines the cases of data collection requests based on the markers of
#' 'datasets', 'bands', and 'stats'.
#' @param database_new [mandatory] (logical) A logical value indicating whether
#' the database is new.
#' @param dataset_new [mandatory] (logical) A logical value indicating whether
#' the dataset is new.
#' @param band_new [mandatory] (logical) A logical value indicating whether the
#' band is new.
#' @param stats_new [mandatory] (logical) A logical vector indicating which
#' statistics are new.
#' @param regions_new [mandatory] (logical) A logical vector indicating which
#' regions are new.
#' @return An integer indicating the processing cases as follows:
#' \describe{
#'  \item{1}{All build}
#'  \item{2}{All update}
#'  \item{3}{Mixed}
#' }
#' @keywords internal
#'
get_cases <- function(database_new, dataset_new, band_new, stats_new,
                      regions_new) {

  if (database_new || all(regions_new) || dataset_new || band_new ||
      all(stats_new)) {
    cases <- 1
  } else if (!any(stats_new) && !any(regions_new)) {
    cases <- 2
  } else {
    cases <- 3
  }

  return(cases)
}

# ------------------------------------------------------------------------------

#' Retrieve Images and Related Information
#'
#' Retrieves images and related information from Google Earth Engine (GEE)
#' based on the specified session task.
#' @param task [mandatory] (list) Session task specifying parameters for data
#' collection.
#' @param cases [mandatory] (integer) Type of data collection request
#' (\code{1}: All build, \code{2}: All update, \code{3}: Mixed).
#' @param dataset [mandatory] (character) Name of the GEE dataset.
#' @param band [mandatory] (character) Name of the band.
#' @param regions_new [mandatory] (logical) A logical vector indicating which
#' regions are new.
#' @param latest_date [mandatory] (date) The most recent data available in the
#' related SQLite table. Set to \code{NULL} during the (re)building procedure.
#' @return List containing retrieved images and related information as follows:
#' \describe{
#'  \item{$build}{Images for the building procedure}
#'  \item{$update}{Images for the updating procedure}
#'  \item{$batch_size}{Batch size}
#'  \item{$skip_band}{TRUE if 'band' is up-to-date and can be skipped}
#'  \item{$skip_update}{TRUE if 'band' is up-to-date but cannot be skipped}
#' }
#' @keywords internal
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom tidyrgee as_tidyee
#'
get_images <- function(task, cases, dataset, band, regions_new, latest_date) {

  # Initialize the images list and logical parameters
  images <- list()
  skip_band <- FALSE
  skip_update <- FALSE

  if (cases == 1) {

    # Case 1: All build - Collect images from the starting date
    images$build <- as_tidyee(
      rgee::ee$ImageCollection(dataset)$select(band)
    ) %>% filter(date >= task$start)
    # Calculate batch size
    images$batch_size <- floor(task$limit / length(images$build$vrt$date))

  } else if (cases == 2) {

    # Case 2: All update - Collect images starting from the last available data
    images$update <- as_tidyee(
      rgee::ee$ImageCollection(dataset)$select(band)
    ) %>% filter(date >= task$start)

    # Check if the latest date in the dataset is up to date
    if (latest_date >= max(as.Date(images$update$vrt$date))) {
      # Skip update if there is no new data
      skip_update <- TRUE
      if (!any(regions_new)) {
        # If there are no new regions, skip the band
        skip_band <- TRUE
      }
    } else {
      # Collect images newer than the latest processed date
      images$update <- images$update %>% filter(date > latest_date)
      # Calculate batch size
      images$batch_size <- floor(task$limit / length(images$update$vrt$date))
    }

  } else if (cases == 3) {

    # Case 3: Mixed - Collect images from the starting date
    images$build <- as_tidyee(
      rgee::ee$ImageCollection(dataset)$select(band)
    ) %>% filter(date >= task$start)
    # Calculate batch size
    images$batch_size <- floor(task$limit / length(images$build$vrt$date))

    # Check if there are images newer than the latest processed date
    if (latest_date < max(as.Date(images$build$vrt$date))) {
      # Collect images newer than the latest processed date
      images$update <- as_tidyee(
        rgee::ee$ImageCollection(dataset)$select(band)
      ) %>% filter(date > latest_date)
    } else {
      # Skip update if there is no new data
      skip_update <- TRUE
    }
  }

  # Add logical parameters to the images list
  images$skip_band <- skip_band
  images$skip_update <- skip_update

  return(images)
}

# ------------------------------------------------------------------------------

#' Divide Grid Object into Multiple Batches
#'
#' Divides the 'grid' object into multiple batches based on the specified
#' batch size and the type of data collection request ('cases').
#' @param cases [mandatory] (integer) Type of data collection request
#' (\code{1}: All build, \code{2}: All update, \code{3}: Mixed).
#' @param grid [mandatory] (sf) Simple features object containing the
#' geometries of the regions of interest.
#' @param batch_size [mandatory] (integer) Maximum size of each batch.
#' @return A list containing two batches:
#' \describe{
#'  \item{$b1}{List of primary batches}
#'  \item{$b2}{List of additional batches (only applicable if 'cases' == 3)}
#' }
#' @keywords internal
#'
get_batches <- function(cases, grid, batch_size) {

  if (cases %in% c(1, 2)) {

    # Get batch for primary data (no additional batch needed)
    b1 <- get_batch(grid = grid, batch_size = batch_size)
    b2 <- NULL

  } else if (cases == 3) {

    # Calculate the maximum number of bins and batch number
    bin_num_max <- max(sum(!grid$add), sum(grid$add))
    batch_num <- ceiling(bin_num_max / batch_size)

    # Get batch for primary and additional data
    b1 <- get_batch(grid = grid[!grid$add, ], batch_num = batch_num)
    b2 <- get_batch(grid = grid[grid$add, ], batch_num = batch_num)

  }

  return(list(
    b1 = b1,
    b2 = b2
  ))
}

# ------------------------------------------------------------------------------

#' Divide Grid Into Batches
#'
#' Divides the 'grid' object into batches based on either the specified batch
#' size or the number of batches.
#' @param grid [mandatory] (sf) Simple features object containing the
#' geometries of the regions of interest.
#' @param batch_size [optional] (integer) Maximum size of each batch
#' (default: \code{NULL}). If \code{NULL}, 'batch_num' must be specified.
#' @param batch_num [optional] (integer) Number of batches (default:
#' \code{NULL}). If \code{NULL}, 'batch_size' must be specified.
#' @return A list containing the batches.
#' @keywords internal
#'
get_batch <- function(grid, batch_size = NULL, batch_num = NULL) {

  # Determine the number of bins
  bin_num <- nrow(grid)

  if (is.null(batch_num)) {
    # Calculate 'batch_num' based on the specified 'batch_size'
    batch_num <- ceiling(bin_num / batch_size)
  } else {
    # Calculate 'batch_size' based on the specified 'batch_num'
    batch_size <- ceiling(bin_num / batch_num)
  }

  # Divide bins into batches
  batch_id <- split(1:bin_num, rep(1:batch_num, each = batch_size)[1:bin_num])

  return(batch_id)
}

# ------------------------------------------------------------------------------

#' Extract Grid Statistics
#'
#' Extracts grid statistics from Google Earth Engine (GEE) images based on
#' specified parameters.
#' @param case [mandatory] (logical) Type of data collection request (\code{1}:
#' Build, \code{2}: Update).
#' @param images [mandatory] (list) List containing GEE images and related
#' information.
#' @param grid [mandatory] (sf) Simple features object containing the
#' geometries of the regions of interest.
#' @param batch_1 [mandatory] (integer) Batch from primary batches.
#' @param batch_2 [mandatory] (integer) Batch from additional batches.
#' @param band [mandatory] (character) Name of the band to extract statistics
#' from.
#' @param stat [mandatory] (character) Name of the statistical measure applied
#' for grid statistics calculation.
#' @param stat_fun [mandatory] (function) Function used to calculate the grid
#' statistic.
#' @param scale [mandatory] (numeric) Scale of images before processing.
#' @param region_new [mandatory] (logical) Vector indicating whether new
#' regions are to be processed.
#' @param skip_update [mandatory] (logical) Logical value indicating whether
#' the 'band' is up-to-date.
#' @param grid_stats [mandatory] (list) A list containing previously calculated
#' grid statistics.
#' @return A data frame containing grid statistics for both building and
#' updating processes.
#' @keywords internal
#' @importFrom purrr map
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_rows filter mutate rename_all select slice
#'
extract_grid_stats <- function(case, images, grid, batch_1, batch_2, band,
                               stat, stat_fun, scale, region_new, skip_update,
                               grid_stats) {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  add <- NULL

  if (case == 1) { # Build

    # Extract statistics for building procedure
    batch_stat_build <- extract_batch_stat(
      imgs = images$build$ee_ob,
      grid = grid %>% slice(batch_1),
      dates = images$build$vrt$date,
      band = band,
      stat = stat,
      stat_fun = stat_fun,
      scale = scale
    )

    # Update grid statistics for building procedure
    grid_stats_build <- update_grid_stats(
      grid_stat = grid_stats$build,
      batch_stat = batch_stat_build
    )

  } else if (case == 2) { # Update

    if (!skip_update) {

      # Extract statistics for updating procedure (existing regions)
      batch_stat_update <- extract_batch_stat(
        imgs = images$update$ee_ob,
        grid = grid %>% filter(!add) %>% slice(batch_1),
        dates = images$update$vrt$date,
        band = band,
        stat = stat,
        stat_fun = stat_fun,
        scale = scale
      )

      # Update grid statistics for updating procedure (existing regions)
      grid_stats_update <- update_grid_stats(
        grid_stat = grid_stats$update,
        batch_stat = batch_stat_update
      )
    }

    # Extract statistics for additional new regions (if any)
    if (any(region_new)) {
      batch_stat_build <- extract_batch_stat(
        imgs = images$build$ee_ob,
        grid = grid %>% filter(add) %>% slice(batch_2),
        dates = images$build$vrt$date,
        band = band,
        stat = stat,
        stat_fun = stat_fun,
        scale = scale
      )

      # Update grid statistics for additional new regions
      grid_stats_build <- update_grid_stats(
        grid_stat = grid_stats$build,
        batch_stat = batch_stat_build
      )
    }
  }

  return(list(
    build = get0("grid_stats_build", ifnotfound = NULL),
    update = get0("grid_stats_update", ifnotfound = NULL)
  ))
}

# ------------------------------------------------------------------------------

#' Extract Grid Statistics for a Batch of Bins
#'
#' Extracts grid statistics from Google Earth Engine (GEE) images for a batch
#' of bins.
#' @param imgs [mandatory] (ImageCollection) GEE object containing images.
#' @param grid [mandatory] (sf) Simple features object containing the
#' geometries of the regions of interest.
#' @param dates [mandatory] (date) Dates when the targeted GEE dataset is
#' available, defining the time span for data extraction.
#' @param band [mandatory] (character) Name of the band to extract statistics
#' from.
#' @param stat [mandatory] (character) Name of the statistical measure applied
#' for grid statistics calculation.
#' @param stat_fun [mandatory] (function) Function used to calculate the grid
#' statistic.
#' @param scale [mandatory] (numeric) Scale of images before processing.
#' @return A data frame containing grid statistics for the specified batch of
#' bins.
#' @keywords internal
#' @importFrom magrittr %>%
#' @importFrom rgee ee_extract
#' @importFrom dplyr mutate select rename_all
#' @importFrom dplyr everything mutate rename_all select
#'
extract_batch_stat <- function(imgs, grid, dates, band, stat, stat_fun, scale) {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  id <- NULL

  # Extract grid statistics from images
  suppressMessages(
    batch_stat <- ee_extract(
      x = imgs,
      y = grid$geometry,
      fun = stat_fun,
      sf = FALSE,
      scale = scale,
      quiet = TRUE
    )
  )

  batch_stat <- batch_stat %>%
    # Add 'id', 'band', and 'stat' columns
    mutate(id = grid$id, band = band, stat = stat) %>%
    # Reorder columns
    select(id, band, stat, everything()) %>%
    # Rename columns by replacing hyphens with underscores in dates
    rename_all( ~ c("id", "band", "stat", gsub("-", "_", as.character(dates))))

  return(batch_stat)
}

# ------------------------------------------------------------------------------

#' Update Grid Statistics
#'
#' Updates existing grid statistics with newly calculated statistics.
#' @param grid_stat [optional] (data.frame) Existing data frame of grid
#' statistics to append the newly calculated statistics to.
#' @param batch_stat [mandatory] (data.frame) New data frame of grid statistics
#' to append to the existing statistics.
#' @return A data frame containing the updated grid statistics.
#' @keywords internal
#'
update_grid_stats <- function(grid_stat, batch_stat) {

  # Check if 'grid_stats' is NULL
  if (is.null(grid_stat)) {
    # If not, return 'batch_stat'
    grid_stats <- batch_stat
  } else {
    # If it exists, append 'batch_stat' to 'grid_stats'
    grid_stats <- rbind(grid_stat, batch_stat)
  }

  return(grid_stats)
}

# ------------------------------------------------------------------------------

#' Write Grid Statistics to Database
#'
#' Writes grid statistics to the SQLite database.
#' @param database_new [mandatory] (logical) A logical value indicating whether
#' the database is new.
#' @param dataset_new [mandatory] (logical) A logical value indicating whether
#' the dataset is new.
#' @param dataset [mandatory] (character) Name of the dataset to initialize or
#' update in the SQLite database.
#' @param db_table [mandatory] (data.frame) The table to be updated or
#' retrieved from the SQLite database. Set to \code{NULL} during the
#' (re)building procedure.
#' @param grid_stats [mandatory] (list) List containing grid statistics
#' separately for (re)building and updating procedures.
#' @keywords internal
#' @importFrom RSQLite dbConnect dbDisconnect dbWriteTable dbRemoveTable SQLite
#'
write_grid_stats <- function(database_new, dataset_new, dataset, db_table,
                             grid_stats) {

  # Check if 'database' or dataset is new
  if (database_new || dataset_new) {

    # Write the 'grid_stats$build' data to the specified dataset
    con <- dbConnect(RSQLite::SQLite(), dbname = "data/geelite.db")
    dbWriteTable(conn = con, name = dataset, value = grid_stats$build,
                 append = TRUE, row.names = FALSE)
    dbDisconnect(con)

  } else {

    # Check if 'grid_stats$update' is not NULL
    if (!is.null(grid_stats$update)) {
      # Merge the existing table with 'grid_stats$update'
      db_table <- merge(db_table, grid_stats$update,
                        by = c("id", "band", "stat"), all.x = TRUE)
    }

    # Check if 'grid_stats$build' is not NULL
    if (!is.null(grid_stats$build)) {
      # Append grid_stats_build to the table
      db_table <- rbind(db_table, grid_stats$build)
    }

    # Write the updated table to the specified dataset
    con <- dbConnect(SQLite(), dbname = "data/geelite.db")
    dbWriteTable(conn = con, name = dataset, value = db_table,
                 row.names = FALSE, overwrite = TRUE)
    dbDisconnect(con)

  }

  # Remove 'grid_stats' and 'table' objects from memory
  rm(grid_stats)
  rm(db_table)

}

# ------------------------------------------------------------------------------

#' Write State File
#'
#' Writes the state file to the specified directory within the generated
#' database (\code{state/state.json}).
#' @param task [mandatory] (list) Session task specifying parameters for data
#' collection.
#' @param regions [mandatory] (character) A vector containing ISO 3166-2 region
#' codes. Country codes are two characters long, while state codes contain
#' additional characters.
#' @param source_for_state [mandatory] (list) A list containing information
#' regarding the collected data.
#' @keywords internal
#' @importFrom jsonlite write_json
#'
write_state_file <- function(task, regions, source_for_state) {
  state <- task
  state$regions <- regions
  state$source <- source_for_state
  write_json(state, "state/state.json", pretty = TRUE)
}

# ------------------------------------------------------------------------------

#' Write Log File
#'
#' Writes the log file to the specified directory within the generated database
#' (\code{log/log.txt}).
#' @param database_new [mandatory] (logical) A logical value indicating whether
#' the database is new.
#' @keywords internal
#'
write_log_file <- function(database_new) {

  # Get current system time in the format "YYYY-MM-DD HH:MM"
  sys_time <- format(Sys.time(), "%Y-%m-%d %H:%M")

  # Determine log message based on whether database is new or updated
  if (database_new) {
    log_message <- paste0("[Build]:   ", sys_time)
  } else {
    log_message <- paste0("[Update]: ", sys_time)
  }

  # Check if the log file exists
  if (file.exists("log/log.txt")) {
    # Append log message to the existing log file
    cat(sprintf("%s\n", log_message), file = "log/log.txt", append = TRUE)
  } else {
    # Create a new log file and write the log message
    cat(sprintf("%s\n", log_message), file = "log/log.txt")
  }

}

# ------------------------------------------------------------------------------

#' Define Output Messages
#'
#' Defines output messages.
#' @param database_new [mandatory] (logical) A logical value indicating whether
#' the database is new.
#' @keywords internal
#' @return A list of output messages.
#'
gen_messages <- function(database_new) {

  if (database_new) {
    message <- list(
      "Database successfully built: 'data/geelite.db'.",
      "State file generated: 'state/state.json'.",
      "CLI scripts generated: 'cli/R functions'."
    )
  } else {
    message <- list(
      "Database successfully updated: 'data/geelite.db'.",
      "State file updated: 'state/state.json'.",
      "CLI scripts updated: 'cli/R functions'."
    )
  }

  return(message)
}
