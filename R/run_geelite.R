# Main Function ----------------------------------------------------------------

#' Build and Update the Grid Statistics Database
#'
#' Collects and stores grid statistics from Google Earth Engine (GEE) data in
#' SQLite format (\code{data/geelite.db}), initializes CLI files
#' (\code{cli/...}), and initializes or updates the state
#' (\code{state/state.json}) and log (\code{log/log.txt}) files.
#' @param path [mandatory] (character) Path to the root directory of the
#'   generated database.
#' @param conda [optional] (character) Name of the virtual Conda environment
#'   used by the \code{rgee} package (default: \code{"rgee"}).
#' @param user [optional] (character) Specifies the Google account directory
#'   within \code{~/.config/earthengine/}. This directory stores credentials
#'   for a specific Google account (default: \code{NULL}).
#' @param rebuild [optional] (logical) If \code{TRUE}, the database and its
#'   supplementary files are overwritten based on the configuration file
#'   (default: \code{FALSE}).
#' @param mode [optional] (character) Mode of data extraction. Currently
#'   supports \code{"local"} or \code{"drive"} (for larger exports via Google
#'   Drive). Defaults to \code{"local"}.
#' @param verbose [optional] (logical) Display computation status and messages
#'   (default: \code{TRUE}).
#' @return Invisibly returns NULL, called for side effects.
#' @export
#' @examples
#' # Example: Build a Grid Statistics Database
#' \dontrun{
#'   run_geelite(path = "path/to/db")
#' }
#' @importFrom utils flush.console
#'
run_geelite <- function(path,
                        conda = "rgee",
                        user = NULL,
                        rebuild = FALSE,
                        mode = "local",
                        verbose = TRUE) {

  # Validate
  params <- list(
    path = path,
    conda = conda,
    user = user,
    rebuild = rebuild,
    mode = mode,
    verbose = verbose
  )
  validate_params(params)

  # Switch WD
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(path)

  # Print version
  print_version(verbose)

  # Initialize dependencies
  set_depend(
    conda   = conda,
    user    = user,
    drive   = (mode == "drive"),
    verbose = verbose
  )

  # Create subdirectories
  set_dirs(rebuild = rebuild)

  # Merge config + state => task
  task <- get_task()

  # Build or read grid
  grid <- get_grid(task)
  if (verbose) {
    cat("Waiting for progress...\r")
    flush.console()
  }

  # Main compilation
  compile_db(
    task = task,
    grid = grid,
    mode = mode,
    verbose = verbose
  )

  # Possibly init CLI
  cli_path <- file.path(path, "cli")
  if (!dir.exists(cli_path)) {
    dir.create(cli_path)
  }
  cli_files <- list.files(cli_path, pattern = "\\.R$", full.names = TRUE)
  if (length(cli_files) == 0) {
    set_cli(path = path, verbose = FALSE)
  }

  invisible(NULL)
}

# Internal Functions -----------------------------------------------------------

#' Display geeLite Package Version
#'
#' Displays the version of the \code{geeLite} package with formatted headers.
#' @param verbose [mandatory] (logical) If \code{TRUE}, the version of the
#'   \code{geeLite} package is printed.
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

#' Set Dependencies
#'
#' Authenticates the Google Earth Engine (GEE) account and activates the
#' specified Conda environment.
#' @param conda [optional] (character) Name of the virtual Conda environment
#'   used by the \code{rgee} package (default: \code{"rgee"}).
#' @param user [optional] (character) Specifies the Google account directory
#'   within \code{~/.config/earthengine/}. This directory stores credentials
#'   for a specific Google account (default: \code{NULL}).
#' @param drive [optional] (logical) If \code{TRUE}, initializes Google Drive
#'   authentication for tasks involving Drive exports (default: \code{FALSE}).
#' @param verbose [optional] (logical) Display messages (default: \code{TRUE}).
#' @keywords internal
#' @importFrom reticulate use_condaenv py_run_string
#' @importFrom rgee ee_clean_user_credentials ee_get_earthengine_path
#' @importFrom rgee ee_Initialize
#'
set_depend <- function(conda = "rgee",
                       user = NULL,
                       drive = FALSE,
                       verbose = TRUE) {

  # Activate the specified Conda environment
  reticulate::use_condaenv(conda, required = TRUE)

  attempt <- 1
  success <- FALSE

  # Attempt to initialize Earth Engine; try up to two times
  while (attempt <= 2 && !success) {

    # Try the standard Earth Engine initialization
    tryCatch({
      suppressMessages({
        rgee::ee_Initialize(user = user, drive = drive, quiet = TRUE)
      })
      success <- TRUE
    }, error = function(e) {

      # Handle keyboard interrupt errors immediately
      if (grepl("KeyboardInterrupt", e$message)) {
        stop(e)
      }

      # Attempt fallback initialization with credential cleaning if necessary
      tryCatch({
        if (!is.null(user)) {
          if (!exists("credentials_path")) {
            credentials_path <- paste0(
              gsub("\\\\", "/", rgee::ee_get_earthengine_path()),
              user, "/credentials"
            )
          }
          reticulate::py_run_string(sprintf(
            "import ee; ee.Initialize(credentials='%s')", credentials_path
          ))
        } else {
          reticulate::py_run_string("import ee; ee.Initialize()")
        }
        success <- TRUE
      }, error = function(e2) {
        rgee::ee_clean_user_credentials(user = user)
      })
    })
    attempt <- attempt + 1
  }

  # Stop execution with an error message if initialization was not successful
  if (!success) {
    stop(
      "It looks like your EE credential has expired.\n",
      "Try running rgee::ee_Authenticate() again or clean your credentials ",
      "with rgee::ee_clean_user_credentials()."
    )
  }

  # Print Earth Engine initialization message if verbose mode is enabled
  if (verbose) {
    gee_message(user)
  }
}

# ------------------------------------------------------------------------------

#' Print Google Earth Engine and Python Environment Information
#'
#' Prints information about the Google Earth Engine (GEE) and Python
#' environment.
#' @param user [mandatory] (character) Specifies the Google account directory
#'   for which information is displayed.
#' @keywords internal
#' @importFrom cli rule
#' @importFrom crayon green blue
#' @importFrom utils packageVersion
#' @importFrom reticulate py_config
#' @importFrom rgee ee_version ee_user_info
#'
gee_message <- function (user) {

  # Retrieve version and configuration details
  rgee_version <- as.character(utils::packageVersion("rgee"))
  ee_version <- as.character(rgee::ee_version())
  user_info <- if (is.null(user)) "not defined" else user
  account_info <- rgee::ee_user_info(quiet = TRUE)[1]
  py_path_info <- reticulate::py_config()$python

  # Create header with version information
  header <- cli::rule(
    left = paste0("rgee ", rgee_version),
    right = paste0("earthengine-api ", ee_version)
  )

  # Display the header and key configuration details
  cat(header, "\n")
  cat(
    crayon::green("\u2714"),
    crayon::blue("User:"),
    crayon::green(user_info), "\n"
  )
  cat(
    crayon::green("\u2714"),
    crayon::blue("Initializing Google Earth Engine:"),
    crayon::green("DONE!"), "\n"
  )
  cat(
    crayon::green("\u2714"),
    crayon::blue("Earth Engine account:"),
    crayon::green(account_info), "\n"
  )
  cat(
    crayon::green("\u2714"),
    crayon::blue("Python path:"),
    crayon::green(py_path_info), "\n"
  )
  cat(cli::rule(), "\n\n")
}

# ------------------------------------------------------------------------------

#' Generate Necessary Directories
#'
#' Generates \code{"data"}, \code{"log"}, \code{"cli"}, and \code{"state"}
#' subdirectories at the specified path.
#' @param rebuild [optional] (logical) If \code{TRUE}, existing directories
#'   will be removed and recreated.
#' @keywords internal
#'
set_dirs <- function(rebuild) {

  # Define subdirectories and state file path
  dirs <- c("data", "log", "cli", "state")
  state_path <- "state/state.json"

  # Remove and recreate directories if needed
  for (dir in dirs) {
    if (dir.exists(dir) &&
        (rebuild || (!file.exists(state_path) && dir != "cli"))) {
      unlink(dir, recursive = TRUE)
      dir.create(dir)
    } else if (!dir.exists(dir)) {
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

  # Load configuration and state file
  config <- jsonlite::fromJSON("config/config.json")
  state_path <- "state/state.json"

  # Return config if no state file exists
  if (!file.exists(state_path)) {
    return(config)
  }

  # Compare config with state and update task
  state <- jsonlite::fromJSON(state_path)
  task <- list()
  task$regions <- compare_vectors(config$regions, state$regions)
  task$source <- compare_lists(config$source, state$source)
  task$start <- state$start
  task$scale <- state$scale
  task$resol <- state$resol
  task$limit <- config$limit

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
  added   <- setdiff(vector_1, vector_2)
  removed <- setdiff(vector_2, vector_1)
  common  <- intersect(vector_1, vector_2)
  result  <- c(paste0("+", added), common, paste0("-", removed))
  result  <- result[nchar(result) > 1]
  return(result)
}

# ------------------------------------------------------------------------------

#' Compare Lists and Highlight Differences
#'
#' Compares two lists and marks new values with '+' and removed values with '-'.
#' @param list_1 [mandatory] (list) First list to compare.
#' @param list_2 [mandatory] (list) Second list to compare.
#' @return A list showing added and removed values marked with '+' and '-'.
#' @keywords internal
#'
compare_lists <- function(list_1, list_2) {

  result <- list()
  # Compare keys and track added/removed elements
  for (key_1 in names(list_1)) {
    if (key_1 %in% names(list_2)) {
      result[[key_1]] <- list()
      for (key_2 in names(list_1[[key_1]])) {
        if (key_2 %in% names(list_2[[key_1]])) {
          result[[key_1]][[key_2]] <-
            compare_vectors(list_1[[key_1]][[key_2]], list_2[[key_1]][[key_2]])
        } else {
          result[[key_1]][[paste0("+", key_2)]] <- list_1[[key_1]][[key_2]]
        }
      }
      for (key_2 in names(list_2[[key_1]])) {
        if (!(key_2 %in% names(list_1[[key_1]]))) {
          result[[key_1]][[paste0("-", key_2)]] <- list_2[[key_1]][[key_2]]
        }
      }
    } else {
      result[[paste0("+", key_1)]] <- list_1[[key_1]]
    }
  }
  # Track removed keys
  for (key_1 in names(list_2)) {
    if (!(key_1 %in% names(list_1))) {
      result[[paste0("-", key_1)]] <- list_2[[key_1]]
    }
  }
  return(result)
}

# ------------------------------------------------------------------------------

#' Obtain H3 Hexagonal Grid
#'
#' Retrieves or creates the grid for the task based on the specified regions
#' and resolution.
#' @param task [mandatory] (list) Session task that specifies the parameters
#'   for data collection.
#' @return A simple features (sf) object containing grid data.
#' @keywords internal
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom sf st_crs st_as_sf st_set_crs st_geometry
#'
get_grid <- function(task) {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  .data <- 0

  regions <- process_vector(task$regions)
  db_path <- "data/geelite.db"

  # Load grid from database if it exists
  if (file.exists(db_path)) {
    grid <- read_grid() %>%
      dplyr::filter(!.data$iso %in% regions$drop) %>%
      sf::st_as_sf(sf_column_name = "geometry")

    # Set coordinate reference system if missing
    if (is.na(sf::st_crs(grid))) {
      grid <- sf::st_set_crs(grid, 4326)
    }

    # Add new regions to the grid if necessary
    if (length(regions$add) > 0) {
      shapes <- get_shapes(regions$add)
      grid_add <- get_bins(shapes, task$resol) %>%
        sf::st_as_sf(sf_column_name = "geometry")
      if (is.na(sf::st_crs(grid_add))) {
        grid_add <- sf::st_set_crs(grid_add, 4326)
      }
      grid <- rbind(grid, grid_add)
    }

    # Write updated grid if regions were modified
    if (length(regions$add) > 0 || length(regions$drop) > 0) {
      write_grid(grid)
    }
  } else {
    # Create a new grid if database does not exist
    shapes <- get_shapes(regions$use)
    grid <- get_bins(shapes, task$resol)
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
#'   codes. Country codes are two characters long, while state codes contain
#'   additional characters.
#' @return A simple features (sf) object containing the shapes of the specified
#'   regions.
#' @keywords internal
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select rename
#' @importFrom rnaturalearth ne_countries ne_states
#'
get_shapes <- function(regions) {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  . <- .data <- 0

  country <- regions[nchar(regions) == 2]
  state   <- regions[nchar(regions) > 2]

  # Retrieve country-level shapes
  if (length(country) > 0) {
    shapes <- rnaturalearth::ne_countries(
      scale = "medium", returnclass = "sf"
    ) %>%
      dplyr::filter(.data$iso_a2_eh %in% regions) %>%
      dplyr::select("iso_a2_eh", "geometry") %>%
      dplyr::rename(iso = "iso_a2_eh")
  }

  # Retrieve state-level shapes
  if (length(state) > 0) {
    states <- rnaturalearth::ne_states(returnclass = "sf") %>%
      subset(.$iso_3166_2 %in% regions, c("iso_3166_2", "geometry")) %>%
      dplyr::rename(iso = .data$iso_3166_2)

    # Merge country and state shapes if both exist
    shapes <- if (exists("shapes")) rbind(shapes, states) else states
  }

  return(shapes)
}

# ------------------------------------------------------------------------------

#' Get H3 Bins for Shapes
#'
#' Generates H3 bins for the provided shapes at the specified resolution.
#' @param shapes [mandatory] (sf) A simple features object containing
#'   geometries used for generating H3 bins.
#' @param resol [mandatory] (integer) An integer specifying the resolution of
#'   the H3 grid.
#' @return A data frame containing the H3 bins with columns for region ISO
#'   3166-2 codes, bin IDs, and geometry.
#' @keywords internal
#' @importFrom dplyr select
#' @importFrom h3jsr cell_to_polygon polygon_to_cells
#'
get_bins <- function(shapes, resol) {

  # Generate H3 bins for each shape
  for (i in seq_len(nrow(shapes))) {
    shape  <- shapes[i, ]
    bin_id <- h3jsr::polygon_to_cells(shape$geometry, res = resol)
    bin_df <- as.data.frame(h3jsr::cell_to_polygon(bin_id))
    bin_df$iso <- shapes$iso[i]
    bin_df$id  <- unlist(bin_id)

    # Initialize or append to the bins dataframe
    bins <- if (i == 1) bin_df else rbind(bins, bin_df)
  }

  return(dplyr::select(bins, "iso", "id", "geometry"))
}

# ------------------------------------------------------------------------------

#' Read Grid from Database
#'
#' Reads the H3 grid from the specified SQLite database
#'   (\code{data/geelite.db}).
#' @return A simple features (sf) object containing the grid data.
#' @keywords internal
#' @importFrom sf st_read
#' @importFrom magrittr %>%
#' @importFrom dplyr rename select
#' @importFrom RSQLite dbDisconnect SQLite
#'
read_grid <- function() {

  # Connect to the database and read the grid table
  con <- db_connect(db_path = "data/geelite.db")
  grid <- sf::st_read(con, "grid", quiet = TRUE) %>%
    dplyr::select(-1) %>%
    dplyr::rename(geometry = "GEOMETRY")
  RSQLite::dbDisconnect(con)

  # Convert to data frame while preserving geometry
  grid_df <- as.data.frame(grid)
  grid_df$geometry <- sf::st_geometry(grid)

  return(grid_df)
}


# ------------------------------------------------------------------------------

#' Write Grid to Database
#'
#' Writes the H3 grid to the specified SQLite database (\code{data/geelite.db}).
#' @param grid [mandatory] (sf) Simple features object containing the grid data
#'   to be written into the database.
#' @keywords internal
#' @importFrom sf st_write
#'
write_grid <- function(grid) {
  sf::st_write(
    obj = grid,
    dsn = "data/geelite.db",
    layer = "grid",
    driver = "SQLite",
    append = FALSE,
    quiet = TRUE
  )
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
#'   for data collection.
#' @param grid [mandatory] (sf) Simple features object containing the
#'   geometries of the regions of interest.
#' @param mode [optional] (character) Mode of data extraction. Currently
#'   supports \code{"local"} or \code{"drive"} (for larger exports via Google
#'   Drive). Defaults to \code{"local"}.
#' @param verbose [mandatory] (logical) Display messages and progress status.
#' @keywords internal
#' @importFrom purrr map
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom stats setNames
#' @importFrom cli cli_alert_info
#' @importFrom RSQLite dbReadTable
#' @importFrom data.table rbindlist
#' @importFrom progress progress_bar
#'
compile_db <- function(task, grid, mode, verbose) {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  .data <- id <- 0

  skip_session    <- TRUE
  source_for_state<- list()
  pb             <- set_progress_bar(verbose)

  state_path     <- "state/state.json"
  database_new   <- !file.exists(state_path)
  scale          <- if (length(task$scale) > 0) task$scale else NULL
  reducers       <- get_reducers()

  # Mark "add" polygons
  regions       <- process_vector(task$regions)
  regions_new   <- regions$use_add
  grid$add      <- grid$iso %in% regions$add

  # Possibly remove dropped dataset tables
  datasets      <- process_vector(names(task$source))
  remove_tables(datasets$drop)

  for (i in seq_along(datasets$use)) {
    dataset      <- datasets$use[i]
    dataset_new  <- datasets$use_add[i]
    bands        <- process_vector(names(task$source[[i]]))
    grid_stats   <- list(build = NULL, update = NULL)

    db_table <- NULL  # define it here

    # Then check if we can load the table
    if (!database_new && !dataset_new) {
      con <- db_connect("data/geelite.db")
      if (RSQLite::dbExistsTable(con, dataset)) {
        db_table <- RSQLite::dbReadTable(con, dataset, check.names = FALSE) %>%
          dplyr::filter(id %in% grid$id)
        latest_date <- as.Date(
          gsub("_", "-", colnames(db_table)[ncol(db_table)]))
      } else {
        db_table <- NULL
        latest_date <- as.Date("1900-01-01")
      }
      RSQLite::dbDisconnect(con)
    }

    # For the progress bar
    filtered_source <- lapply(task$source, function(ds) {
      lapply(ds, function(st) {
        st[!grepl("^-", st)]
      })
    })
    source_length <- sum(unlist(lapply(filtered_source, function(ds) {
      sapply(ds, length)
    })))
    pb_step <- 100 / source_length

    # Each band
    for (j in seq_along(bands$use)) {
      band      <- bands$use[j]
      band_new  <- bands$use_add[j]
      stats     <- process_vector(task$source[[i]][[j]])
      stat_funs <- purrr::map(stats$use, ~ reducers[[.]])
      stats_new <- stats$use_add

      # Possibly remove dropped band/stats
      if (j == 1 && !is.null(db_table)) {
        if (length(bands$drop) > 0 || length(stats$drop) > 0) {
          db_table <- db_table %>%
            dplyr::filter(!(.data$band %in% bands$drop) &
                            !(.data$zonal_stat %in% stats$drop))
        }
      }

      # Cases: new dataset, new band, new stats, new polygons, or partial update
      cases   <- get_cases(
        database_new, dataset_new, band_new, stats_new, regions_new)
      images  <- get_images(
        task, cases, dataset, band, regions_new,
        get0("latest_date", ifnotfound = NULL))

      # If no update needed for band
      if (images$skip_band) {
        if (!is.null(pb)) pb$tick(pb_step)
        next
      }

      # Batches
      batches <- get_batches(cases, grid, images$batch_size)

      # If skipping update but new regions, keep building stats
      if (images$skip_update && !any(regions_new)) {
        # Only the new stats
        stats$use     <- stats$use[stats$use_add]
        stat_funs     <- stat_funs[stats$use_add]
      }

      # For each requested stat
      for (k in seq_along(stats$use)) {
        current_stat <- stats$use[k]
        stat_fun     <- stat_funs[[k]]
        # 1 or 2 => full build or partial update, 3 => build + update
        case_code <- ifelse(cases %in% c(1, 2), cases,
                            ifelse(stats_new[k], 1, 2))

        # Build or Update
        # ----------------------------------------------------------------------
        # Chunk-based export for local mode, big-batch export for Drive mode
        if (mode == "local") {
          # local mode => chunk by chunk
          # --------------------------------------------------------------------
          chunk_dfs_build <- list()
          chunk_dfs_update<- list()

          # If case_code == 1 => only build
          if (case_code == 1 && !is.null(batches$b1)) {
            # Combine results from all b1
            for (chunk_sf in batches$b1) {
              # In local mode: direct ee_extract
              chunk_result <- local_chunk_extract(
                sf_chunk  = chunk_sf,
                imgs      = images$build$ee_ob,
                dates     = images$build$vrt$date,
                band      = band,
                stat      = current_stat,
                stat_fun  = stat_fun,
                scale     = scale
              )
              chunk_dfs_build[[length(chunk_dfs_build) + 1]] <- chunk_result
              if (!is.null(pb)) pb$tick(pb_step / max(1, length(batches$b1)))
            }
            # Merge
            if (length(chunk_dfs_build) > 0) {
              grid_stats$build <- update_grid_stats(
                grid_stats$build,
                data.table::rbindlist(chunk_dfs_build, fill = TRUE)
              )
            }
          }

          # If case_code == 2 => only update
          if (case_code == 2 && !images$skip_update && !is.null(batches$b1)) {
            for (chunk_sf in batches$b1) {
              chunk_result <- local_chunk_extract(
                sf_chunk  = chunk_sf,
                imgs      = images$update$ee_ob,
                dates     = images$update$vrt$date,
                band      = band,
                stat      = current_stat,
                stat_fun  = stat_fun,
                scale     = scale
              )
              chunk_dfs_update[[length(chunk_dfs_update) + 1]] <- chunk_result
              if (!is.null(pb)) pb$tick(pb_step / max(1, length(batches$b1)))
            }
            if (length(chunk_dfs_update) > 0) {
              grid_stats$update <- update_grid_stats(
                grid_stats$update,
                data.table::rbindlist(chunk_dfs_update, fill = TRUE)
              )
            }
          }

          # If case_code == 3 => build + update
          # build => new polygons; update => new dates
          if (case_code == 3) {
            # update existing polygons
            if (!images$skip_update && !is.null(batches$b1)) {
              for (chunk_sf in batches$b1) {
                chunk_result <- local_chunk_extract(
                  sf_chunk  = chunk_sf,
                  imgs      = images$update$ee_ob,
                  dates     = images$update$vrt$date,
                  band      = band,
                  stat      = current_stat,
                  stat_fun  = stat_fun,
                  scale     = scale
                )
                chunk_dfs_update[[length(chunk_dfs_update) + 1]] <- chunk_result
                if (!is.null(pb)) pb$tick(pb_step / max(1, length(batches$b1)))
              }
            }
            # build new polygons
            if (!is.null(batches$b2)) {
              for (chunk_sf in batches$b2) {
                chunk_result <- local_chunk_extract(
                  sf_chunk  = chunk_sf,
                  imgs      = images$build$ee_ob,
                  dates     = images$build$vrt$date,
                  band      = band,
                  stat      = current_stat,
                  stat_fun  = stat_fun,
                  scale     = scale
                )
                chunk_dfs_build[[length(chunk_dfs_build) + 1]] <- chunk_result
                if (!is.null(pb)) pb$tick(pb_step / max(1, length(batches$b2)))
              }
            }
            # Merge
            if (length(chunk_dfs_build) > 0) {
              grid_stats$build <- update_grid_stats(
                grid_stats$build,
                data.table::rbindlist(chunk_dfs_build, fill = TRUE)
              )
            }
            if (length(chunk_dfs_update) > 0) {
              grid_stats$update <- update_grid_stats(
                grid_stats$update,
                data.table::rbindlist(chunk_dfs_update, fill = TRUE)
              )
            }
          }

        } else {
          # drive mode => big-batch approach for fewer tasks
          # --------------------------------------------------------------------
          # Merge b1/b2 polygons for fewer large exports per band/stat via
          # `extract_drive_stats_parallel()`.
          all_chunks_b1 <- if (!is.null(batches$b1)) batches$b1 else list()
          all_chunks_b2 <- if (!is.null(batches$b2)) batches$b2 else list()

          # If case_code == 1 => only build
          if (case_code == 1 && length(all_chunks_b1) > 0) {
            drive_df <- extract_drive_stats_parallel(
              sf_chunks  = all_chunks_b1,
              imgs       = images$build$ee_ob,
              band       = band,
              stat       = current_stat,
              stat_fun   = stat_fun,
              scale      = scale,
              limit      = task$limit, # to group polygons
              folder     = "geelite_drive_exports",
              user       = task$user
            )
            grid_stats$build <- update_grid_stats(grid_stats$build, drive_df)
            if (!is.null(pb)) pb$tick(pb_step)
          }

          # If case_code == 2 => only update
          if (case_code == 2 &&
              !images$skip_update &&
              length(all_chunks_b1) > 0) {
            drive_df <- extract_drive_stats_parallel(
              sf_chunks  = all_chunks_b1,
              imgs       = images$update$ee_ob,
              band       = band,
              stat       = current_stat,
              stat_fun   = stat_fun,
              scale      = scale,
              limit      = task$limit,
              folder     = "geelite_drive_exports",
              user       = task$user
            )
            grid_stats$update <- update_grid_stats(grid_stats$update, drive_df)
            if (!is.null(pb)) pb$tick(pb_step)
          }

          # If case_code == 3 => build new polygons + update existing ones
          if (case_code == 3) {
            # update existing polygons
            if (!images$skip_update && length(all_chunks_b1) > 0) {
              drive_df_up <- extract_drive_stats_parallel(
                sf_chunks  = all_chunks_b1,
                imgs       = images$update$ee_ob,
                band       = band,
                stat       = current_stat,
                stat_fun   = stat_fun,
                scale      = scale,
                limit      = task$limit,
                folder     = "geelite_drive_exports",
                user       = task$user
              )
              grid_stats$update <- update_grid_stats(
                grid_stats$update, drive_df_up)
              if (!is.null(pb)) pb$tick(pb_step / 2)
            }
            # build new polygons
            if (length(all_chunks_b2) > 0) {
              drive_df_bd <- extract_drive_stats_parallel(
                sf_chunks  = all_chunks_b2,
                imgs       = images$build$ee_ob,
                band       = band,
                stat       = current_stat,
                stat_fun   = stat_fun,
                scale      = scale,
                limit      = task$limit,
                folder     = "geelite_drive_exports",
                user       = task$user
              )
              grid_stats$build <- update_grid_stats(
                grid_stats$build, drive_df_bd)
              if (!is.null(pb)) pb$tick(pb_step / 2)
            }
          }
        }
        # ----------------------------------------------------------------------
      } # end of for each stat
      # Keep track for state
      source_for_state[[dataset]][[band]] <- stats$use
    } # end of for each band

    # Write grid stats if any new data
    if (!is.null(grid_stats$build) || !is.null(grid_stats$update)) {
      write_grid_stats(
        database_new = database_new,
        dataset_new  = dataset_new,
        dataset      = dataset,
        grid_stats   = grid_stats,
        db_table     = get0("db_table", ifnotfound = NULL)
      )
      skip_session <- FALSE
    }
  } # end of for each dataset

  if (skip_session) {
    cli_alert_info("Database is up-to-date.")
    cat("\n")
  } else {
    write_state_file(task, regions$use, source_for_state)
    write_log_file(!database_new)
    output_message(gen_messages(!database_new), verbose)
  }
}

# ------------------------------------------------------------------------------

#' Set Progress Bar
#'
#' Initializes a progress bar if 'verbose' is \code{TRUE}.
#' @param verbose [mandatory] (logical) If \code{TRUE}, a progress bar is
#'   initialized.
#' @return A progress bar (environment) if 'verbose' is \code{TRUE}, or
#'   \code{NULL} if \code{FALSE}.
#' @keywords internal
#' @importFrom progress progress_bar
#'
set_progress_bar <- function(verbose) {
  if (verbose) {
    pb <- progress::progress_bar$new(
      format = "Progress: |:bar| :percent | Elapsed: :elapsed | ETA: :eta",
      total  = 100,
      width  = 80,
      clear  = FALSE
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
#'   marked with '+' and '-' prefixes.
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
  drop_idx <- stringr::str_detect(vector, "^\\-")
  add_idx  <- stringr::str_detect(vector, "^\\+")

  # Filter out items not marked with '-'
  use      <- vector[!drop_idx]

  # Identify indices of items in 'use' marked with '+'
  use_add  <- stringr::str_detect(use, "^\\+")

  # Remove '+' prefix from items marked with '+'
  use[use_add] <- stringr::str_sub(use[use_add], 2)

  return(list(
    drop    = stringr::str_sub(vector[drop_idx], 2),
    add     = stringr::str_sub(vector[add_idx], 2),
    use     = use,
    use_add = use_add
  ))
}

# ------------------------------------------------------------------------------

#' Create or Open the Database Connection
#'
#' @param db_path [mandatory] (character) Path to the SQLite file.
#' @keywords internal
#' @importFrom RSQLite dbConnect SQLite
#'
db_connect <- function(db_path) {
  RSQLite::dbConnect(RSQLite::SQLite(), db_path)
}

# ------------------------------------------------------------------------------

#' Get Reducers
#'
#' Initializes a list of reducers for grid statistics calculation.
#' @return A list of available reducers.
#' @keywords internal
#' @import rgee
#'
get_reducers <- function() {
  list(
    mean   = rgee::ee$Reducer$mean(),
    sum    = rgee::ee$Reducer$sum(),
    max    = rgee::ee$Reducer$max(),
    min    = rgee::ee$Reducer$min(),
    median = rgee::ee$Reducer$median(),
    sd     = rgee::ee$Reducer$stdDev()
  )
}

# ------------------------------------------------------------------------------

#' Remove Tables from the Database
#'
#' Removes tables from the database if their corresponding dataset is initially
#' marked for deletion ('-').
#' @param tables_drop [mandatory] (character) A character vector of tables to
#'   be deleted.
#' @keywords internal
#' @importFrom RSQLite dbDisconnect dbRemoveTable SQLite
#'
remove_tables <- function(tables_drop) {
  if (length(tables_drop) > 0) {
    con <- db_connect(db_path = "data/geelite.db")
    for (td in tables_drop) {
      if (RSQLite::dbExistsTable(con, td)) {
        RSQLite::dbRemoveTable(con, td)
      }
    }
    RSQLite::dbDisconnect(con)
  }
}

# ------------------------------------------------------------------------------

#' Determine the Cases of Data Collection Requests
#'
#' Determines the cases of data collection requests based on the markers of
#' 'datasets', 'bands', and 'stats'.
#' @param database_new [mandatory] (logical) A logical value indicating whether
#'   the database is new.
#' @param dataset_new [mandatory] (logical) A logical value indicating whether
#'   the dataset is new.
#' @param band_new [mandatory] (logical) A logical value indicating whether the
#'   band is new.
#' @param stats_new [mandatory] (logical) A logical vector indicating which
#'   statistics are new.
#' @param regions_new [mandatory] (logical) A logical vector indicating which
#'   regions are new.
#' @return An integer indicating the processing cases as follows:
#' \describe{
#'  \item{1}{All build}
#'  \item{2}{All update}
#'  \item{3}{Mixed}
#' }
#' @keywords internal
#'
get_cases <- function(database_new, dataset_new, band_new,
                      stats_new, regions_new) {
  if (database_new || all(regions_new) || dataset_new ||
      band_new || all(stats_new)) {
    1
  } else if (!any(stats_new) && !any(regions_new)) {
    2
  } else {
    3
  }
}

# ------------------------------------------------------------------------------

#' Retrieve Images and Related Information
#'
#' Retrieves images and related information from Google Earth Engine (GEE)
#' based on the specified session task.
#' @param task [mandatory] (list) Session task specifying parameters for data
#'   collection.
#' @param cases [mandatory] (integer) Type of data collection request
#'   (\code{1}: All build, \code{2}: All update, \code{3}: Mixed).
#' @param dataset [mandatory] (character) Name of the GEE dataset.
#' @param band [mandatory] (character) Name of the band.
#' @param regions_new [mandatory] (logical) A logical vector indicating which
#'   regions are new.
#' @param latest_date [mandatory] (date) The most recent data available in the
#'   related SQLite table. Set to \code{NULL} during the (re)building procedure.
#' @return List containing retrieved images and related information as follows:
#' \describe{
#'  \item{$build}{Images for the building procedure}
#'  \item{$update}{Images for the updating procedure}
#'  \item{$batch_size}{Batch size}
#'  \item{$skip_band}{TRUE if 'band' is up-to-date and can be skipped}
#'  \item{$skip_update}{TRUE if 'band' is up-to-date but cannot be skipped}
#' }
#' @keywords internal
#' @import rgee
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom tidyrgee as_tidyee
#'
get_images <- function(task, cases, dataset, band, regions_new, latest_date) {

  # Initialize the images list and logical parameters
  images <- list()
  skip_band <- FALSE
  skip_update <- FALSE

  # If we are building from scratch or brand new band/dataset
  if (cases == 1) {

    # only build
    images$build <- tidyrgee::as_tidyee(
      rgee::ee$ImageCollection(dataset)$select(band)
    ) %>%
      dplyr::filter(date >= task$start)
    images$batch_size <- floor(task$limit / length(images$build$vrt$date))

  } else if (cases == 2) {

    # only update
    images$update <- tidyrgee::as_tidyee(
      rgee::ee$ImageCollection(dataset)$select(band)
    ) %>%
      dplyr::filter(date >= task$start)
    # Check if we already have the max date
    if (!is.null(latest_date) &&
        latest_date >= max(as.Date(images$update$vrt$date))) {
      skip_update <- TRUE
      if (!any(regions_new)) skip_band <- TRUE
    } else {
      images$update <- images$update %>% dplyr::filter(date > latest_date)
      images$batch_size <- floor(task$limit / length(images$update$vrt$date))
    }

  } else if (cases == 3) {

    # partial: build + update
    images$build <- tidyrgee::as_tidyee(
      rgee::ee$ImageCollection(dataset)$select(band)
    ) %>%
      dplyr::filter(date >= task$start)
    images$batch_size <- floor(task$limit / length(images$build$vrt$date))

    if (!is.null(latest_date) &&
        latest_date < max(as.Date(images$build$vrt$date))) {
      images$update <- tidyrgee::as_tidyee(
        rgee::ee$ImageCollection(dataset)$select(band)
      ) %>%
        dplyr::filter(date > latest_date)
    } else {
      skip_update <- TRUE
    }
  }

  # Add logical parameters to the images list
  images$skip_band   <- skip_band
  images$skip_update <- skip_update

  return(images)
}

# ------------------------------------------------------------------------------

#' Produce Batches for Build/Update Mixed Cases
#'
#' Divides the grid into one or two lists of chunked sf objects,
#' depending on the data-collection case (1,2,3).
#' @param cases [mandatory] (integer) 1=All build, 2=All update, 3=Mixed.
#' @param grid [mandatory] (sf) The sf object (grid) containing column 'add'
#'   to distinguish existing vs. new rows.
#' @param batch_size [mandatory] (integer) If \code{cases} = 1 or 2, we'll call
#'   \code{get_batch(grid,batch_size=batch_size)}.
#' @return (list) A list of two elements, \code{b1} and \code{b2}, each a list
#'   of sf subsets (chunks). \code{b2} might be \code{NULL} if not needed.
#' @keywords internal
#'
get_batches <- function(cases, grid, batch_size) {

  # Limit batch size to grid size
  batch_size <- min(nrow(grid), batch_size)

  # Process entire grid if case 1 (build) or 2 (update)
  if (cases %in% c(1, 2)) {
    b1 <- get_batch(grid, batch_size = batch_size)
    b2 <- NULL
  } else {
    # Case 3: Separate existing and new polygons
    existing <- grid[!grid$add, ]
    new_bins <- grid[grid$add, ]

    # Create batches if data exists
    b1 <- if (nrow(existing) > 0) get_batch(existing, batch_size) else NULL
    b2 <- if (nrow(new_bins) > 0) get_batch(new_bins, batch_size) else NULL
  }

  list(b1 = b1, b2 = b2)
}

# ------------------------------------------------------------------------------

#' Create Batches from an sf Object
#'
#' Divides an sf object (\code{grid}) into a list of chunks, either based on a
#' specified number of batches (\code{batch_num}) or a maximum chunk size
#' (\code{batch_size}).
#' @param grid [mandatory] (sf) The sf object to be split into chunks.
#' @param batch_size [optional] (integer) Maximum rows per chunk. Must be
#'   set if \code{batch_num} is NULL.
#' @param batch_num [optional] (integer) Total number of chunks to create.
#'   Must be set if \code{batch_size} is NULL.
#' @return (list) A list of sf objects (chunks).
#' @keywords internal
#'
get_batch <- function(grid, batch_size = NULL, batch_num = NULL) {

  # Determine the number of bins
  nrows <- nrow(grid)

  # Derive batch_num or batch_size as needed
  if (!is.null(batch_size)) {
    # batch_num = total # of chunks
    batch_num <- ceiling(nrows / batch_size)
  } else {
    # batch_size = # of rows per chunk
    batch_size <- ceiling(nrows / batch_num)
  }

  # Generate chunk boundaries
  chunk_indices <- seq(1, nrows, by = batch_size)

  # Create actual chunk subsets
  lapply(chunk_indices, function(i) {
    grid[i:min(i + batch_size - 1, nrows), ]
  })
}

# ------------------------------------------------------------------------------

#' Perform a Single Drive Export for Multiple Geometry Chunks
#'
#' Exports multiple geometry chunks to Google Drive in a single batch task.
#' The function processes spatial data using Google Earth Engine (GEE) and
#' exports results in CSV format.
#' @param sf_list [mandatory] (list) A list of sf data.frames representing
#'   geometry chunks to be processed.
#' @param imgs [mandatory] (ee$ImageCollection) The Earth Engine image
#'   collection to extract statistics from.
#' @param stat_fun [mandatory] (ee$Reducer) The reducer function to apply
#'   to extract statistics.
#' @param band [mandatory] (character) The band name from the image collection
#'   (e.g., "NDVI").
#' @param stat [mandatory] (character) The statistical function name (e.g.,
#' "mean").
#' @param scale [mandatory] (numeric) The spatial resolution in meters for
#'   'reduceRegions'.
#' @param folder [optional] (character) Name of the Google Drive folder
#'   where the export will be stored. Default is \code{"geelite_drive_exports"}.
#' @param user [optional] (character) If multiple rgee user profiles exist,
#'   specify the user profile directory.
#' @param description [optional] (character) A custom description for the
#'   export task. Default is \code{"geelite_export"}.
#' @return (data.frame) A data frame containing extracted statistics with
#'   columns \code{id}, \code{band}, \code{zonal_stat}, and date-based values.
#' @keywords internal
#' @importFrom rgee sf_as_ee ee_table_to_drive ee_monitoring ee_drive_to_local
#' @importFrom dplyr mutate select rename relocate all_of
#' @importFrom googledrive drive_ls drive_rm
#' @importFrom tidyr pivot_wider
#' @importFrom data.table fread
#' @importFrom digest digest
#'
batch_drive_export <- function(sf_list,
                               imgs,
                               stat_fun,
                               band,
                               stat,
                               scale,
                               folder = "geelite_drive_exports",
                               user = NULL,
                               description = "geelite_export") {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  'system:index' <- value <- 0

  # Ensure messages are suppressed
  options(googledrive_quiet = TRUE)

  # Combine multiple sf chunks
  big_sf <- do.call(rbind, sf_list)
  big_sf <- sf::st_as_sf(big_sf, crs = 4326)

  # Convert to ee FeatureCollection
  big_ee <- rgee::sf_as_ee(big_sf)

  # Create the export task
  task <- rgee::ee_table_to_drive(
    collection    = imgs$map(rgee::ee_utils_pyfunc(function(img) {
      img$reduceRegions(
        collection = big_ee,
        reducer    = stat_fun,
        scale      = scale
      )
    }))$flatten(),
    description   = description,
    folder        = folder,
    fileNamePrefix= paste0(description, "_", digest::digest(big_sf)),
    fileFormat    = "CSV"
  )
  task$start()

  rgee::ee_monitoring(task, quiet = TRUE)
  info <- rgee::ee$batch$Task$status(task)
  if (!is.null(info$error_message)) {
    stop("Google Drive export failed: ", info$error_message)
  }
  if (is.null(info$destination_uris)) {
    stop("Drive export did not produce 'destination_uris'. Possibly empty ",
         "data or non-Drive export.")
  }

  # Download CSV
  tmp_file <- tempfile(fileext = ".csv")
  rgee::ee_drive_to_local(task = task, dsn = tmp_file)
  result <- data.table::fread(tmp_file)
  unlink(tmp_file)

  # Clean up Drive
  drive_files <- googledrive::drive_ls(path = folder)
  if (nrow(drive_files) > 0) {
    googledrive::drive_rm(drive_files)
    #message("Temporary Drive exports removed from '", folder, "'.")
  }

  # Tidy the result
  value_col <- colnames(result)[ncol(result) - 1]
  df <- result %>%
    dplyr::mutate(date = sub("_\\d+$", "", `system:index`)) %>%
    dplyr::select("id", dplyr::all_of(value_col), "date") %>%
    dplyr::rename(value = dplyr::all_of(value_col))

  df_wide <- tidyr::pivot_wider(df, names_from = date, values_from = value) %>%
    dplyr::mutate(band = band, zonal_stat = stat) %>%
    dplyr::relocate("id", "band", "zonal_stat")

  return(df_wide)
}

# ------------------------------------------------------------------------------

#' Extract Large-Scale Statistics in Drive Mode with Fewer Tasks
#'
#' Batches multiple geometry chunks into fewer \code{ee_table_to_drive} tasks,
#' reducing overhead and leveraging Google Earth Engine's parallel processing.
#' @param sf_chunks [mandatory] (list) A list of sf data frames representing
#'   geometry chunks.
#' @param imgs [mandatory] (ee$ImageCollection) The Earth Engine image
#'   collection to extract statistics from.
#' @param band [mandatory] (character) The band name (e.g., "NDVI").
#' @param stat [mandatory] (character) The statistical function to apply
#'   (e.g., "mean").
#' @param stat_fun [mandatory] (ee$Reducer) The Earth Engine reducer function.
#' @param scale [mandatory] (numeric) The spatial resolution in meters for
#'   reduceRegions.
#' @param limit [optional] (integer) Maximum number of polygons in a single
#'   Earth Engine export. Defaults to \code{1000}.
#' @param folder [optional] (character) Name of the Google Drive folder where
#'   exports will be stored. Defaults to \code{"geelite_drive_exports"}.
#' @param user [optional] (character) GEE user profile name, if applicable.
#'
#' @return (data.frame) A merged data frame containing extracted statistics
#'   from all Drive exports.
#' @keywords internal
#' @importFrom data.table rbindlist
#'
extract_drive_stats_parallel <- function(sf_chunks,
                                         imgs,
                                         band,
                                         stat,
                                         stat_fun,
                                         scale,
                                         limit  = 1000,
                                         folder = "geelite_drive_exports",
                                         user   = NULL) {

  # Group geometry chunks so each combined group has <= limit polygons
  chunk_sizes <- sapply(sf_chunks, nrow)
  grouped_lists <- list()
  current_list  <- list()
  current_count <- 0

  for (sf_chunk in sf_chunks) {
    nfeats <- nrow(sf_chunk)
    if ((current_count + nfeats) > limit && current_count > 0) {
      grouped_lists[[length(grouped_lists) + 1]] <- current_list
      current_list  <- list(sf_chunk)
      current_count <- nfeats
    } else {
      current_list[[length(current_list) + 1]] <- sf_chunk
      current_count <- current_count + nfeats
    }
  }
  if (length(current_list) > 0) {
    grouped_lists[[length(grouped_lists) + 1]] <- current_list
  }

  # For each group, do one Drive export
  results <- list()
  for (i in seq_along(grouped_lists)) {
    desc <- sprintf("geelite_drive_batch_%s_%02d", band, i)
    df_batch <- batch_drive_export(
      sf_list     = grouped_lists[[i]],
      imgs        = imgs,
      stat_fun    = stat_fun,
      band        = band,
      stat        = stat,
      scale       = scale,
      folder      = folder,
      user        = user,
      description = desc
    )
    results[[i]] <- df_batch
  }

  # Merge all
  final_df <- data.table::rbindlist(results, fill = TRUE)
  return(as.data.frame(final_df))
}

# ------------------------------------------------------------------------------

#' Extract Statistics Locally for a Single Geometry Chunk
#'
#' Computes statistical summaries for a given spatial feature (\code{sf_chunk})
#' from an Earth Engine \code{ee$ImageCollection} over a specified date range.
#' This function extracts values for a specific band and applies a chosen
#' reducer.
#' @param sf_chunk [mandatory] (sf) An sf data frame containing geometry.
#' @param imgs [mandatory] (ee$ImageCollection) The Earth Engine image collection
#'   to extract statistics from.
#' @param dates [mandatory] (character) A vector of date strings corresponding
#'   to images in the collection.
#' @param band [mandatory] (character) The name of the band to extract.
#' @param stat [mandatory] (character) The statistical function to apply
#'   (e.g., "mean").
#' @param stat_fun [mandatory] (ee$Reducer) The Earth Engine reducer function.
#' @param scale [mandatory] (numeric) The spatial resolution in meters for
#'   reduce operations.
#' @return (data.frame) A data frame containing extracted statistics with
#'   columns \code{id}, \code{band}, \code{zonal_stat}, and date-based values.
#' @keywords internal
#' @importFrom rgee ee_extract
#' @importFrom dplyr everything mutate rename_all select
#'
local_chunk_extract <- function(sf_chunk, imgs, dates, band,
                                stat, stat_fun, scale) {

  # Extract statistics from Earth Engine for the given geometry
  suppressMessages({
    batch_stat <- rgee::ee_extract(
      x     = imgs,
      y     = sf_chunk$geometry,
      fun   = stat_fun,
      sf    = FALSE,
      scale = scale,
      quiet = TRUE
    )
  })

  # Format and structure the extracted data
  batch_stat <- batch_stat %>%
    dplyr::mutate(
      id         = sf_chunk$id,
      band       = band,
      zonal_stat = stat
    ) %>%
    dplyr::select("id", "band", "zonal_stat", dplyr::everything()) %>%
    dplyr::rename_all(~ c(
      "id", "band", "zonal_stat",
      gsub("-", "_", as.character(dates))
    ))

  return(batch_stat)
}

# ------------------------------------------------------------------------------

#' Update Grid Statistics
#'
#' Updates existing grid statistics with newly calculated statistics.
#' @param grid_stat [optional] (data.frame) Existing data frame of grid
#'   statistics to append the newly calculated statistics to.
#' @param batch_stat [mandatory] (data.frame) New data frame of grid statistics
#'   to append to the existing statistics.ű
#' @return (data.frame) A combined data frame with missing columns filled as NA.
#' @return A data frame containing the updated grid statistics.
#' @keywords internal
#' @importFrom data.table rbindlist
#'
update_grid_stats <- function(grid_stat, batch_stat) {
  if (is.null(grid_stat)) {
    return(batch_stat)
  }
  combined <- data.table::rbindlist(list(grid_stat, batch_stat), fill = TRUE)
  return(as.data.frame(combined))
}

# ------------------------------------------------------------------------------

#' Write Grid Statistics to Database
#'
#' Writes grid statistics to the SQLite database.
#' @param database_new [mandatory] (logical) A logical value indicating whether
#'   the database is new.
#' @param dataset_new [mandatory] (logical) A logical value indicating whether
#'   the dataset is new.
#' @param dataset [mandatory] (character) Name of the dataset to initialize or
#'   update in the SQLite database.
#' @param db_table [mandatory] (data.frame) The table to be updated or
#'   retrieved from the SQLite database. Set to \code{NULL} during the
#'   (re)building procedure.
#' @param grid_stats [mandatory] (list) List containing grid statistics
#'   separately for (re)building and updating procedures.
#' @keywords internal
#' @importFrom RSQLite dbDisconnect dbWriteTable dbRemoveTable SQLite
#'
write_grid_stats <- function(database_new, dataset_new, dataset,
                             db_table, grid_stats) {

  con <- db_connect("data/geelite.db")

  if (database_new || dataset_new || is.null(db_table)) {
    # create new table from build data
    if (!is.null(grid_stats$build)) {
      RSQLite::dbWriteTable(
        conn      = con,
        name      = dataset,
        value     = grid_stats$build,
        append    = TRUE,
        row.names = FALSE
      )
    }
  } else {
    # We have an existing table => merge updates
    if (!is.null(grid_stats$update)) {
      db_table <- merge(db_table, grid_stats$update,
                        by = c("id", "band", "zonal_stat"), all.x = TRUE)
    }
    if (!is.null(grid_stats$build)) {
      db_table <- rbind(db_table, grid_stats$build)
    }
    RSQLite::dbWriteTable(
      conn      = con,
      name      = dataset,
      value     = db_table,
      row.names = FALSE,
      overwrite = TRUE
    )
  }
  RSQLite::dbDisconnect(con)
  rm(grid_stats, db_table)
}

# ------------------------------------------------------------------------------

#' Write State File
#'
#' Writes the state file to the specified directory within the generated
#' database (\code{state/state.json}).
#' @param task [mandatory] (list) Session task specifying parameters for data
#'   collection.
#' @param regions [mandatory] (character) A vector containing ISO 3166-2 region
#'   codes. Country codes are two characters long, while state codes contain
#'   additional characters.
#' @param source_for_state [mandatory] (list) A list containing information
#'   regarding the collected data.
#' @keywords internal
#' @importFrom jsonlite write_json
#'
write_state_file <- function(task, regions, source_for_state) {
  state <- task
  state$regions <- regions
  state$source  <- source_for_state
  jsonlite::write_json(state, "state/state.json", pretty = TRUE)
}

# ------------------------------------------------------------------------------

#' Write Log File
#'
#' Writes the log file to the specified directory within the generated
#' database (\code{log/log.txt}).
#' @param database_new [mandatory] (logical) A logical value indicating whether
#'   the database is new.
#' @keywords internal
#'
write_log_file <- function(database_new) {
  sys_time <- format(Sys.time(), "%Y-%m-%d %H:%M")
  if (database_new) {
    log_message <- paste0("[Build]:   ", sys_time)
  } else {
    log_message <- paste0("[Update]: ", sys_time)
  }
  if (file.exists("log/log.txt")) {
    cat(sprintf("%s\n", log_message), file = "log/log.txt", append = TRUE)
  } else {
    cat(sprintf("%s\n", log_message), file = "log/log.txt")
  }
}

# ------------------------------------------------------------------------------

#' Define Output Messages
#'
#' Defines output messages based on whether the database is new or updated.
#' @param database_new [mandatory] (logical) A logical value indicating whether
#'   the database is new.
#' @keywords internal
#' @return A list of output messages.
#'
gen_messages <- function(database_new) {
  if (database_new) {
    list(
      "Database successfully built: 'data/geelite.db'.",
      "State file generated: 'state/state.json'.",
      "CLI scripts generated: 'cli/R functions'."
    )
  } else {
    list(
      "Database successfully updated: 'data/geelite.db'.",
      "State file updated: 'state/state.json'.",
      "CLI scripts updated: 'cli/R functions'."
    )
  }
}
