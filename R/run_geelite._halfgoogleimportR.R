# Main Function ----------------------------------------------------------------

#' Build and Update the Grid Statistics Database
#'
#' Collects and stores grid statistics from Google Earth Engine (GEE) data in
#' SQLite format (\code{data/geelite.db}), initializes CLI files
#' (\code{cli/...}), and initializes or updates the state
#' (\code{state/state.json}) and log (\code{log/log.txt}) files.
#' @param path [mandatory] (character) The path to the root directory of the
#'   generated database. This must be a writable, non-temporary directory.
#'   Avoid using the home directory (~), the current working directory, or the
#'   package directory.
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
#'   run_geelite(path = tempdir())
#' }
#' @importFrom utils flush.console
#' @importFrom googledrive drive_find drive_ls drive_rm as_id
#'
run_geelite <- function(path,
                        conda = "rgee",
                        user = NULL,
                        rebuild = FALSE,
                        mode = "local",
                        verbose = TRUE) {

  # Convert to absolute path and check existence
  path <- normalizePath(path, mustWork = FALSE)

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
    conda = conda,
    user = user,
    verbose = verbose
  )

  # Create subdirectories
  set_dirs(rebuild = rebuild)

  # Merge config + state => task
  task <- get_task()

  # Build or read grid
  grid <- get_grid(task)
  if (verbose) {
    cat("\n")
    if (mode == "local") {
      cat("> Extracting data from Earth Engine...")
    } else if (mode == "drive") {
      cat("> Uploading data for remote processing...")
    }
    flush.console()
  }

  # Clean up any existing temporary Drive export folders
  if (mode == "drive") {
    clean_drive_folders_by_name(
      folder_name = ".geelite_tmp_drive",
      delete_folders = TRUE,
      verbose = FALSE
    )
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
#' Activates the Conda environment used for Earth Engine, ensures that
#' authentication exists if needed, and initializes the Earth Engine Python API.
#' @param conda [optional] (character) Name of the Conda environment
#'   (default: "rgee").
#' @param user [optional] (character) Google account directory under
#'   ~/.config/earthengine. Typically NULL unless multiple accounts are used.
#' @param drive [optional] (logical) Whether to enable Google Drive access
#'   (default: TRUE).
#' @param verbose [optional] (logical) Whether to print a success message at
#'   the end.
#' @keywords internal
#' @importFrom rgee ee_Authenticate
#' @importFrom gargle gargle_oauth_cache
#' @importFrom reticulate use_condaenv import
#' @importFrom googledrive drive_auth drive_user
#'
set_depend <- function(conda = "rgee",
                       user = NULL,
                       drive = TRUE,
                       verbose = TRUE) {

  # Activate the conda environment
  reticulate::use_condaenv(conda, required = TRUE)

  # Try importing the Earth Engine Python module
  ee <- tryCatch(
    reticulate::import("ee"),
    error = function(e) {
      stop(
        "Failed to import the Python 'ee' module. ",
        "Verify that 'earthengine-api' is installed in the '",
        conda, "' environment.\n\n", e$message, call. = FALSE
      )
    }
  )
  initialized <- FALSE
  # First attempt: Initialize using existing Python credentials
  tryCatch(
    {
      ee$Initialize()
      initialized <- TRUE
    },
    error = function(e) {
      # Silent fallback; authentication will be attempted next
    }
  )
  # Second attempt: Authenticate with rgee, then retry initialization
  if (!initialized) {
    # Run rgee authentication (this triggers browser-based token workflow)
    tryCatch(
      rgee::ee_Authenticate(user = user, drive = drive),
      error = function(e) {
        stop(
          "Earth Engine authentication failed.\n\n",
          e$message,
          call. = FALSE
        )
      }
    )
    # Re-import the ee module (to ensure credentials are now available)
    ee <- tryCatch(
      reticulate::import("ee"),
      error = function(e) {
        stop(
          "The 'ee' Python module became unavailable after authentication.\n",
          "Please check your Python environment.\n\n",
          e$message,
          call. = FALSE
        )
      }
    )
    # Final attempt: initialize Python EE API
    tryCatch(
      {
        ee$Initialize()
        initialized <- TRUE
      },
      error = function(e) {
        stop(
          "Earth Engine could not be initialized after authentication.\n",
          "Ensure that your Google Cloud project is registered for ",
          "Earth Engine\nand that your credentials are valid.\n\n",
          e$message,
          call. = FALSE
        )
      }
    )
  }

  # Google Drive token detection (for drive mode)
  drive_email <- NULL
  if (drive &&
      requireNamespace("googledrive", quietly = TRUE) &&
      requireNamespace("gargle", quietly = TRUE)) {

    # Make gargle/drive quieter during internal auth
    old_opt <- options(
      gargle_oauth_email = TRUE,
      gargle_quiet       = TRUE
    )
    on.exit(options(old_opt), add = TRUE)

    # Try to reuse an existing cached token non-interactively
    tryCatch(
      {
        suppressMessages(
          suppressWarnings(
            googledrive::drive_auth(
              cache = gargle::gargle_oauth_cache()
            )
          )
        )

        # If auth worked, try to get the user email
        drive_email <- tryCatch(
          {
            du <- suppressMessages(googledrive::drive_user())
            # drive_user() usually returns a list with "emailAddress"
            if (!is.null(du[["emailAddress"]])) {
              du[["emailAddress"]]
            } else if (!is.null(du[["user"]][["emailAddress"]])) {
              du[["user"]][["emailAddress"]]
            } else {
              NULL
            }
          },
          error = function(e) NULL
        )
      },
      error = function(e) {
        # No cached token or non-interactive failure → ignore silently
        drive_email <<- NULL
      }
    )
  }

  # Print Earth Engine initialization message if verbose mode is enabled
  if (verbose) {
    gee_message(user = user, drive_email = drive_email)
  }
  invisible(TRUE)
}

# ------------------------------------------------------------------------------

#' Print Earth Engine and Python Environment Information
#'
#' Prints a summary of the Earth Engine and Python environment used by
#'   \code{geeLite}, after successful initialization.
#' @param user [optional] (character) Optional Google account identifier to
#'   display.
#' @param drive_email [optional] (character) Optional Google Drive email if a
#'   cached token was found. Used only for messaging; can be NULL.
#' @keywords internal
#' @importFrom cli rule
#' @importFrom crayon green blue
#' @importFrom utils packageVersion
#' @importFrom reticulate py_config import
#'
gee_message <- function(user = NULL, drive_email = NULL) {

  # Get versions and paths safely
  rgee_version <- tryCatch(
    as.character(utils::packageVersion("rgee"))[1],
    error = function(e) "unknown"
  )

  ee_module <- tryCatch(
    reticulate::import("ee"),
    error = function(e) NULL
  )

  ee_version <- if (!is.null(ee_module)) {
    tryCatch(
      as.character(ee_module$`__version__`)[1],
      error = function(e) "unknown"
    )
  } else {
    "unknown"
  }

  # Retrieve Python configuration
  py_info <- tryCatch(reticulate::py_config(), error = function(e) NULL)

  python_ver <- tryCatch(
    as.character(py_info$version)[1],
    error = function(e) "unknown"
  )

  python_path <- tryCatch(
    as.character(py_info$python)[1],
    error = function(e) "unknown"
  )

  # Header with rgee + earthengine-api versions
  cat(
    cli::rule(
      left  = paste0("rgee ", rgee_version),
      right = paste0("earthengine-api ", ee_version)
    ),
    "\n"
  )

  # Body lines
  cat(
    crayon::green("[OK] "),
    crayon::blue("User: "),
    if (is.null(user)) "not specified" else user,
    "\n", sep = ""
  )

  cat(
    crayon::green("[OK] "),
    crayon::blue("Earth Engine API version: "),
    ee_version,
    "\n", sep = ""
  )

  cat(
    crayon::green("[OK] "),
    crayon::blue("Python executable: "),
    python_path,
    "\n", sep = ""
  )

  cat(
    crayon::green("[OK] "),
    crayon::blue("Python version: "),
    python_ver,
    "\n", sep = ""
  )

  # Optional Google Drive info line, only if we have an email
  if (!is.null(drive_email)) {
    cat(
      crayon::green("[OK] "),
      crayon::blue("Google Drive token: "),
      drive_email,
      "\n", sep = ""
    )
  }

  # Footer line
  cat(cli::rule(), "\n", sep = "")
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
  state <- regions[nchar(regions) > 2]

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
#'   3166 codes, bin IDs, and geometry.
#' @keywords internal
#' @importFrom dplyr select
#' @importFrom h3jsr cell_to_polygon polygon_to_cells
#'
get_bins <- function(shapes, resol) {

  # Generate H3 bins for each shape
  for (i in seq_len(nrow(shapes))) {
    shape <- shapes[i, ]
    bin_id <- h3jsr::polygon_to_cells(shape$geometry, res = resol)
    bin_df <- as.data.frame(h3jsr::cell_to_polygon(bin_id))
    bin_df$iso <- shapes$iso[i]
    bin_df$id <- unlist(bin_id)

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

  # Initialize session settings, database state, and processing utilities
  skip_session <- TRUE
  source_for_state <- list()
  pb <- set_progress_bar(verbose)
  state_path <- "state/state.json"
  database_new <- !file.exists(state_path)
  scale <- if (length(task$scale) > 0) task$scale else NULL
  reducers <- get_reducers()

  # Mark "add" polygons
  regions <- process_vector(task$regions)
  regions_new <- regions$use_add
  grid_size <- nrow(grid)
  grid$add <- grid$iso %in% regions$add

  # Possibly remove dropped dataset tables
  datasets <- process_vector(names(task$source))
  remove_tables(datasets$drop)

  for (i in seq_along(datasets$use)) {
    dataset <- datasets$use[i]
    dataset_new <- datasets$use_add[i]
    bands <- process_vector(names(task$source[[i]]))
    grid_stats <- list(build = NULL, update = NULL)
    db_table <- NULL

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
      band <- bands$use[j]
      band_new <- bands$use_add[j]
      stats <- process_vector(task$source[[i]][[j]])
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
      cases <- get_cases(database_new, dataset_new, band_new, stats_new,
                         regions_new)

      # Collect images
      images <- get_images(task, mode, cases, dataset, band, regions_new,
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
        stats$use <- stats$use[stats$use_add]
        stat_funs <- stat_funs[stats$use_add]
      }

      # For each requested stat
      for (k in seq_along(stats$use)) {
        current_stat <- stats$use[k]
        stat_fun <- stat_funs[[k]]
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
          chunk_dfs_update <- list()

          # If case_code == 1 => only build
          if (case_code == 1 && !is.null(batches$b1)) {
            # Combine results from all b1
            for (chunk_sf in batches$b1) {
              # In local mode: direct ee_extract
              chunk_result <- local_chunk_extract(
                sf_chunk = chunk_sf,
                imgs = images$build$ee_ob,
                dates = images$build$vrt$date,
                band = band,
                stat = current_stat,
                stat_fun = stat_fun,
                scale = scale
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
                sf_chunk = chunk_sf,
                imgs = images$update$ee_ob,
                dates = images$update$vrt$date,
                band = band,
                stat = current_stat,
                stat_fun = stat_fun,
                scale = scale
              )
              chunk_dfs_update[[length(chunk_dfs_update) + 1]] <- chunk_result
              if (!is.null(pb)) pb$tick(pb_step / length(batches$b1))
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
                  sf_chunk = chunk_sf,
                  imgs = images$update$ee_ob,
                  dates = images$update$vrt$date,
                  band = band,
                  stat = current_stat,
                  stat_fun = stat_fun,
                  scale = scale
                )
                chunk_dfs_update[[length(chunk_dfs_update) + 1]] <- chunk_result
                if (!is.null(pb)) pb$tick(pb_step /
                                            (length(batches$b1) + length(batches$b2)))
              }
            }
            # build new polygons
            if (!is.null(batches$b2)) {
              for (chunk_sf in batches$b2) {
                chunk_result <- local_chunk_extract(
                  sf_chunk = chunk_sf,
                  imgs = images$build$ee_ob,
                  dates = images$build$vrt$date,
                  band = band,
                  stat = current_stat,
                  stat_fun = stat_fun,
                  scale = scale
                )
                chunk_dfs_build[[length(chunk_dfs_build) + 1]] <- chunk_result
                if (!is.null(pb)) pb$tick(pb_step /
                                            (length(batches$b1) + length(batches$b2)))
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
          # `extract_drive_stats`
          all_chunks_b1 <- if (!is.null(batches$b1)) batches$b1 else list()
          all_chunks_b2 <- if (!is.null(batches$b2)) batches$b2 else list()

          # If case_code == 1 => only build
          if (case_code == 1 && length(all_chunks_b1) > 0) {

            drive_df <- extract_drive_stats(
              sf_chunks = all_chunks_b1,
              imgs = images$build$ee_ob,
              band = band,
              stat = current_stat,
              stat_fun = stat_fun,
              scale = scale,
              user = task$user,
              pb = pb,
              pb_step = pb_step/length(all_chunks_b1)
            )
            grid_stats$build <- update_grid_stats(grid_stats$build,
                                                  drive_df)
          }

          # If case_code == 2 => only update
          if (case_code == 2 &&
              !images$skip_update &&
              length(all_chunks_b1) > 0) {
            drive_df <- extract_drive_stats(
              sf_chunks = all_chunks_b1,
              imgs = images$update$ee_ob,
              band = band,
              stat = current_stat,
              stat_fun = stat_fun,
              scale = scale,
              user = task$user,
              pb = pb,
              pb_step = pb_step/length(all_chunks_b1)
            )
            grid_stats$update <- update_grid_stats(grid_stats$update,
                                                   drive_df)
          }

          # If case_code == 3 => build new polygons + update existing ones
          if (case_code == 3) {
            # update existing polygons
            if (!images$skip_update && length(all_chunks_b1) > 0) {
              drive_df_up <- extract_drive_stats(
                sf_chunks = all_chunks_b1,
                imgs = images$update$ee_ob,
                band = band,
                stat = current_stat,
                stat_fun = stat_fun,
                scale = scale,
                user = task$user,
                pb = pb,
                pb_step = pb_step/(length(all_chunks_b1)+length(all_chunks_b2))
              )
              grid_stats$update <- update_grid_stats(grid_stats$update,
                                                     drive_df_up)
            }
            # build new polygons
            if (length(all_chunks_b2) > 0) {
              drive_df_bd <- extract_drive_stats(
                sf_chunks = all_chunks_b2,
                imgs = images$build$ee_ob,
                band = band,
                stat = current_stat,
                stat_fun = stat_fun,
                scale = scale,
                user = task$user,
                pb = pb,
                pb_step = pb_step/(length(all_chunks_b1)+length(all_chunks_b2))
              )
              grid_stats$build <- update_grid_stats(grid_stats$build,
                                                    drive_df_bd)
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
        dataset_new = dataset_new,
        dataset = dataset,
        grid_stats = grid_stats,
        db_table = get0("db_table", ifnotfound = NULL)
      )
      skip_session <- FALSE
    }
  }

  # Ensure progress bar reaches 100% if not already finished
  if (!is.null(pb) && !pb$finished) pb$update(1)

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
  add_idx <- stringr::str_detect(vector, "^\\+")

  # Filter out items not marked with '-'
  use <- vector[!drop_idx]

  # Identify indices of items in 'use' marked with '+'
  use_add <- stringr::str_detect(use, "^\\+")

  # Remove '+' prefix from items marked with '+'
  use[use_add] <- stringr::str_sub(use[use_add], 2)

  return(list(
    drop = stringr::str_sub(vector[drop_idx], 2),
    add = stringr::str_sub(vector[add_idx], 2),
    use = use,
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
    mean = rgee::ee$Reducer$mean(),
    sum = rgee::ee$Reducer$sum(),
    max = rgee::ee$Reducer$max(),
    min = rgee::ee$Reducer$min(),
    median = rgee::ee$Reducer$median(),
    sd = rgee::ee$Reducer$stdDev()
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
#' @param mode [mandatory] (character) Mode of data extraction. Currently
#'   supports \code{"local"} or \code{"drive"} (for larger exports via Google
#'   Drive). Defaults to \code{"local"}.
#' @param cases [mandatory] (integer) Type of data collection request
#'   (\code{1}: All build, \code{2}: All update, \code{3}: Mixed).
#' @param dataset [mandatory] (character) Name of the GEE dataset.
#' @param band [mandatory] (character) Name of the band.
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
#'
get_images <- function(task, mode, cases, dataset, band, regions_new,
                       latest_date) {

  # Initialize the images list and logical parameters
  images <- list(build = NULL, update = NULL)
  skip_band <- FALSE
  skip_update <- FALSE

  # Base ImageCollection (band selected)
  ic_base <- rgee::ee$ImageCollection(dataset)$select(band)

  # Case 1: build only (new database / dataset / band / stats / regions)
  if (cases == 1) {

    ic_build <- ic_base$filterDate(task$start, NULL)
    images$build <- ee_as_tidyee(ic_build)

  } else if (cases == 2) {

    # Case 2: update only (no new stats, no new regions)
    ic_update <- ic_base$filterDate(task$start, NULL)
    images$update <- ee_as_tidyee(ic_update)

    # If we already have the most recent date in the table
    if (!is.null(latest_date) &&
        length(images$update$vrt$date) > 0 &&
        latest_date >= max(as.Date(images$update$vrt$date))) {

      skip_update <- TRUE
      if (!any(regions_new)) {
        skip_band <- TRUE
      }

    } else if (!is.null(latest_date) &&
               length(images$update$vrt$date) > 0) {

      # Restrict update images to dates strictly after latest_date
      ic_update2 <- ic_base$filterDate(
        format(latest_date + 1, "%Y-%m-%d"), NULL
      )
      images$update <- ee_as_tidyee(ic_update2)
    }

  } else if (cases == 3) {

    # Case 3: mixed (some new stats/regions, some existing)
    ic_build <- ic_base$filterDate(task$start, NULL)
    images$build <- ee_as_tidyee(ic_build)

    if (!is.null(latest_date) &&
        length(images$build$vrt$date) > 0 &&
        latest_date < max(as.Date(images$build$vrt$date))) {

      ic_update <- ic_base$filterDate(
        format(latest_date + 1, "%Y-%m-%d"), NULL
      )
      images$update <- ee_as_tidyee(ic_update)

    } else {
      skip_update <- TRUE
    }
  }

  # Determine the number of images for batching
  n_images <- if (!is.null(images$build)) {
    length(images$build$vrt$date)
  } else if (!is.null(images$update)) {
    length(images$update$vrt$date)
  } else {
    0L
  }

  if (n_images == 0L) {
    # No images found for this band → nothing to do
    skip_band <- TRUE
    images$batch_size <- 1L
  } else {
    images$batch_size <- if (mode == "local") {
      max(floor(task$limit / n_images), 1L)
    } else {
      max(task$limit, 1L)
    }
  }

  images$skip_band <- skip_band
  images$skip_update <- skip_update

  return(images)
}

# ------------------------------------------------------------------------------

#' Minimal Replacement for tidyrgee::as_tidyee
#'
#' Creates a lightweight representation of an Earth Engine
#' \code{ee$ImageCollection}, containing the original collection and its
#' associated dates extracted from \code{system:time_start}.
#' @return A list with:
#'   \code{ee_ob} (the original ImageCollection) and
#'   \code{vrt} (list containing a \code{date} vector).
#' @param ic [mandatory] (ee$ImageCollection) The Earth Engine image collection
#'   to convert.
#' @keywords internal
#' @import rgee
ee_as_tidyee <- function(ic) {
  # Try to extract 'system:time_start' as a list of POSIX times
  times <- tryCatch(
    rgee::ee$List(ic$aggregate_array("system:time_start"))$getInfo(),
    error = function(e) NULL
  )
  if (is.null(times) || length(times) == 0) {
    dates <- as.Date(character())
  } else {
    times <- unlist(times)
    dates <- as.Date(
      as.POSIXct(times / 1000, origin = "1970-01-01", tz = "UTC")
    )
  }
  list(
    ee_ob = ic,
    vrt   = list(date = dates)
  )
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
#' @param folder [optional] (character) Name of the Google Drive folder where
#'   exports will be stored. Defaults to \code{".geelite_tmp_drive"}.
#' @param user [optional] (character) GEE user profile name, if applicable.
#' @param pb [mandatory] (Progress bar object) A progress bar instance from
#'   \code{progress::progress_bar} or similar package. Used to track task
#'   progress.
#' @param pb_step [mandatory] (numeric) The step size for updating the
#'   progress bar.
#' @return (data.frame) A merged data frame containing extracted statistics
#'   from all Drive exports.
#' @keywords internal
#' @importFrom data.table rbindlist
#'
extract_drive_stats <- function(sf_chunks,
                                imgs,
                                band,
                                stat,
                                stat_fun,
                                scale,
                                folder = ".geelite_tmp_drive",
                                user = NULL,
                                pb,
                                pb_step) {

  # For each group, do one Drive export
  results <- list()
  for (i in seq_along(sf_chunks)) {
    desc <- sprintf("geelite_drive_batch_%s_%02d", band, i)
    df_batch <- batch_drive_export(
      sf_list = sf_chunks[[i]],
      imgs = imgs,
      stat_fun = stat_fun,
      band = band,
      stat = stat,
      scale = scale,
      folder = folder,
      user = user,
      description = desc
    )
    results[[i]] <- df_batch
    if (!is.null(pb)) pb$tick(pb_step)
  }

  # Merge all
  final_df <- data.table::rbindlist(results, fill = TRUE)
  return(as.data.frame(final_df))
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
#'   where the export will be stored. Default is \code{".geelite_tmp_drive"}.
#' @param user [optional] (character) If multiple rgee user profiles exist,
#'   specify the user profile directory.
#' @param description [optional] (character) A custom description for the
#'   export task. Default is \code{"geelite_export"}.
#' @param verbose [optional] (logical) If \code{TRUE}, progress messages will
#'   be printed. Defaults to \code{FALSE}.
#' @return (data.frame) A data frame containing extracted statistics with
#'   columns \code{id}, \code{band}, \code{zonal_stat}, and date-based values.
#' @keywords internal
#' @importFrom rgee sf_as_ee ee_table_to_drive ee_monitoring ee_drive_to_local
#' @importFrom dplyr mutate select rename relocate all_of
#' @importFrom tidyr pivot_wider
#' @importFrom data.table fread
#'
batch_drive_export <- function(sf_list,
                               imgs,
                               stat_fun,
                               band,
                               stat,
                               scale,
                               folder = ".geelite_tmp_drive",
                               user = NULL,
                               description = "geelite_export",
                               verbose = FALSE) {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  'system:index' <- value <- 0

  # Function for conditional logging
  log_message <- function(msg) {
    if (verbose) message(msg)
  }

  # Combine multiple sf chunks and convert to ee FeatureCollection
  big_sf <- sf::st_as_sf(sf_list, crs = 4326)
  suppressPackageStartupMessages({
    big_ee <- rgee::sf_as_ee(big_sf)
  })

  # Ensure messages are suppressed
  options(googledrive_quiet = TRUE)

  export_task_func <- function() {
    clean_filename <- gsub("[^a-zA-Z0-9]", "_", paste0("export_", Sys.time()))

    task <- rgee::ee_table_to_drive(
      collection = imgs$map(rgee::ee_utils_pyfunc(function(img) {
        img$reduceRegions(
          collection = big_ee,
          reducer = stat_fun,
          scale = scale
        )
      }))$flatten(),
      description = description,
      folder = folder,
      fileNamePrefix = clean_filename,
      fileFormat = "CSV"
    )

    task$start()
    return(task)
  }

  task <- export_task_func()

  max_retries <- 5
  attempt <- 1
  success <- FALSE

  while (attempt <= max_retries && !success) {
    tryCatch({
      status <- rgee::ee$batch$Task$status(task)

      if (status$state %in% c("COMPLETED")) {
        success <- TRUE
        uris <- status$destination_uris
        log_message(paste("Export succeeded:", basename(uris)))

      } else if (status$state %in% c("FAILED", "CANCELED")) {
        log_message(paste("Task failed. Attempt", attempt))
        task <- export_task_func()
        attempt <- attempt + 1

      } else {
        log_message("Task still running...")
        Sys.sleep(10)
      }

    }, error = function(e) {
      log_message(paste("Error monitoring task:", e$message))
      attempt <- attempt + 1
    })
  }

  tmp_file <- tempfile(fileext = ".csv")
  suppressMessages(rgee::ee_drive_to_local(task = task, dsn = tmp_file))
  result <- data.table::fread(tmp_file)
  unlink(tmp_file)

  clean_drive_folders_by_name(
    folder_name = ".geelite_tmp_drive",
    delete_folders = TRUE,
    verbose = FALSE
  )

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

#' Extract Statistics Locally for a Single Geometry Chunk
#'
#' Computes statistical summaries for a given spatial feature (\code{sf_chunk})
#' from an Earth Engine \code{ee$ImageCollection} over a specified date range.
#' This function extracts values for a specific band and applies a chosen
#' reducer.
#' @param sf_chunk [mandatory] (sf) An sf data frame containing geometry.
#' @param imgs [mandatory] (ee$ImageCollection) The Earth Engine image
#'   collection to extract statistics from.
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
      x = imgs,
      y = sf_chunk$geometry,
      fun = stat_fun,
      sf = FALSE,
      scale = scale,
      quiet = TRUE
    )
  })

  # Format and structure the extracted data
  batch_stat <- batch_stat %>%
    dplyr::mutate(
      id = sf_chunk$id,
      band = band,
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
        conn = con,
        name = dataset,
        value = grid_stats$build,
        append = TRUE,
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
      conn = con,
      name = dataset,
      value = db_table,
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
  state$source <- source_for_state
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
