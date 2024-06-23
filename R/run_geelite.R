#' @title Collecting and Storing GEE Data
#'
#' @description Collects and stores GEE data, updating states and log files
#' (\code{path/data/geelite.db}).
#'
#' @param path [mandatory] (character) Path to the root directory of the
#' generated database.
#'
#' @param conda [optional] (character) Name of the virtual Conda environment
#' installed and used by the rgee package (default: \code{"rgee"}).
#'
#' @param user [optional] (character) Used to create a folder within the path
#' \code{~/.config/earthengine/}. This folder stores all the credentials
#' associated with a specific Google identity (default: \code{NULL}).
#'
#' @param rebuild [optional] (logical) If set to \code{TRUE}, the database and
#' its supplementary files will be overwritten based on the configuration file
#' (default: \code{FALSE}).
#'
#' @export
#'
#' @examples
#' # Example: Data collection
#' \dontrun{
#'   run_geelite(path = "path")
#' }
#'
#' @importFrom purrr map
#' @importFrom magrittr %>%
#' @importFrom stats setNames
#' @importFrom tidyr replace_na
#' @importFrom tidyrgee as_tidyee
#' @importFrom utils packageVersion
#' @importFrom progress progress_bar
#' @importFrom cli cli_alert_info cli_h1
#' @importFrom jsonlite fromJSON write_json
#' @importFrom rgee ee_extract ee_Initialize
#' @importFrom sf st_as_sf st_crs st_read st_write
#' @importFrom rnaturalearth ne_countries ne_states
#' @importFrom reticulate use_condaenv py_run_string
#' @importFrom h3jsr cell_to_polygon polygon_to_cells
#' @importFrom dplyr everything filter mutate rename rename_all select
#' @importFrom stringr str_detect str_extract str_replace_all str_replace
#' @importFrom stringr str_sub
#' @importFrom RSQLite dbConnect dbDisconnect dbWriteTable dbRemoveTable SQLite
#' @importFrom RSQLite dbReadTable
#'
run_geelite <- function(path, conda = "rgee", user = NULL, rebuild = FALSE) {

  version <- as.character(packageVersion("geeLite"))
  cli_h1("")
  cat("\033[1mgeeLite R Package - Version:", version, "\033[0m")
  cli_h1("")
  cat("\n")

  setwd(path)

  gen_folders(rebuild)            # create subfolders within the path directory
  task <- get_task()              # define task
  grid <- get_grid(task)          # get grid
  set_depend(conda, user)         # activate dependencies
  pull_data(task, grid)           # build/update database
  set_cli(path)                   # set CLI files

}

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

gen_folders <- function(rebuild) {

  folders <- c("data", "log", "cli", "state")
  for (folder in folders) {
    # If the folder exists and rebuild is TRUE, remove and recreate the folder
    if (dir.exists(folder) && rebuild) {
      unlink(folder, recursive = TRUE)
      dir.create(folder)
    }
    # If the folder does not exist, create it
    else if (!dir.exists(folder)) {
      dir.create(folder)
    }
  }

}

# ------------------------------------------------------------------------------

get_task <- function() {

  config <- fromJSON("config/config.json")
  state_file <- "state/state.json"

  if (!file.exists(state_file)) {
    task <- config
  } else {
    # Mark new values with "+" and removed values with "-"
    state <- fromJSON(state_file)
    task <- list()
    task$regions <- comp_vects(config$regions, state$regions)
    task$source <- comp_lists(config$source, state$source)
    task$start <- state$start
    task$scale <- state$scale
    task$resol <- state$resol
    task$limit <- config$limit
    task$crs <- state$crs
  }

  return(task)
}

# ------------------------------------------------------------------------------

comp_vects <- function(x, y) {

  common <- intersect(x, y)
  only_x <- setdiff(x, y)
  only_y <- setdiff(y, x)

  z <- c(paste0("+", only_x), common, paste0("-", only_y))
  z <- z[nchar(z) > 1]

  return(z)
}

# ------------------------------------------------------------------------------

comp_lists <- function(x, y) {

  z <- list()
  for (key_1 in names(x)) {
    if (key_1 %in% names(y)) {
      z[[key_1]] <- list()
      for (key_2 in names(x[[key_1]])) {
        if (key_2 %in% names(y[[key_1]])) {
          z[[key_1]][[key_2]] <- comp_vects(x[[key_1]][[key_2]],
                                            y[[key_1]][[key_2]])
        } else {
          z[[key_1]][[paste0("+", key_2)]] <- x[[key_1]][[key_2]]
        }
      }
      for (key_2 in names(y[[key_1]])) {
        if (!(key_2 %in% names(x[[key_1]]))) {
          z[[key_1]][[paste0("-", key_2)]] <- y[[key_1]][[key_2]]
        }
      }
    } else {
      z[[paste0("+", key_1)]] <- x[[key_1]]
    }
  }

  for (key_1 in names(y)) {
    if (!(key_1 %in% names(x))) {
      z[[paste0("-", key_1)]] <- y[[key_1]]
    }
  }

  return(z)
}

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

set_depend <- function(conda, user) {

  use_condaenv(conda, required = TRUE)

  ee_Initialize(user = user)
  cat("\n")

}

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

get_grid <- function(task) {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  fips <- NULL

  regions <- task$regions
  idx_drop <- str_detect(regions, "^\\-")
  regions_drop <- regions[idx_drop]
  idx_add <- str_detect(regions, "^\\+")
  regions_add <- str_sub(regions[idx_add], 2)

  db_file <- "data/geelight.db"

  if (file.exists(db_file)) {

    # Read grid, excluding regions to be removed (-)
    grid <- read_grid() %>% filter(!fips %in% regions_drop)

    # Add new regions (+)
    if (any(idx_add)) {
      shapes <- get_shapes(regions_add)
      grid_add <- get_bins(shapes, task$resol)
      grid <- rbind(grid, st_as_sf(grid_add, crs = st_crs(grid)))
    }

    # Write grid if at least one region is added (+) or removed (-)
    if (any(idx_add, idx_drop)) {
      write_grid(grid)
    }

  } else {

    # In case of (re)build, define and write the grid
    shapes <- get_shapes(regions)
    grid <- get_bins(shapes, task$resol)
    write_grid(grid)

  }

  return(grid)
}

# ------------------------------------------------------------------------------

get_shapes <- function(regions) {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  . <- states <- fips_10 <- geometry <- NULL

  country <- regions[nchar(regions) == 2]
  state <- regions[nchar(regions) > 2]

  if (!length(country) == 0){
    shapes <- ne_countries(scale = "medium", returnclass = "sf") %>%
      filter(fips_10 %in% regions) %>%
      select(fips_10, geometry) %>%
      rename(fips = fips_10)
  }

  if (!length(state) == 0){
    ne_states(returnclass = "sf") %>%
      .[.$fips %in% regions, c("fips", "geometry")]
    if (exists("regions")) {
      shapes <- rbind(regions, states)
    } else {
      shapes <- states
    }
  }

  return(shapes)
}

# ------------------------------------------------------------------------------

get_bins <- function(shapes, resol) {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  id <- fips <- geometry <- NULL

  for (i in 1:nrow(shapes)) {

    shape <- shapes[i,]
    bin_id <- polygon_to_cells(shape$geometry, res = resol)
    bin <- as.data.frame(cell_to_polygon(bin_id))

    if (i == 1) {
      bin$fips <- shapes$fips[i]
      bin$id <- unlist(bin_id)
      bins <- bin
    } else {
      bin$fips <- shapes$fips[i]
      bin$id <- unlist(bin_id)
      bins <- rbind(bins, bin)
    }
  }

  bins <- bins %>% select(fips, id, geometry)
  return(bins)
}

# ------------------------------------------------------------------------------

read_grid <- function() {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  GEOMETRY <- NULL

  con <- dbConnect(SQLite(), dbname = "data/geelite.db")
  grid <- st_read(con, "grid", quiet = TRUE) %>%
    select(-1) %>% rename(geometry = GEOMETRY)
  dbDisconnect(con)

  return(grid)
}

# ------------------------------------------------------------------------------

write_grid <- function(grid) {

  st_write(obj = grid, dsn = "data/geelite.db", layer = "grid",
           driver = "SQLite", append = FALSE, quiet = TRUE)

}

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

pull_data <- function(task, grid) {

  # To avoid 'no visible binding for global variable' messages (CRAN test)
  id <- ee <- fips <- stat <- GEOMETRY <- NULL

  # Setting the progress bar
  pb <- progress_bar$new(
    format = "Progress: [:bar] :percent, eta: :eta",
    total = 100,
    width = 80,
    clear = FALSE
  )

  source <- list()
  db_file <- "data/geelite.db"
  state_file <- "state/state.json"
  state_exists <- file.exists(state_file)

  regions <- task$regions
  idx_drop <- str_detect(regions, "^\\-")
  regions_drop <- regions[idx_drop]
  idx_add <- str_detect(regions, "^\\+")
  regions_add <- regions[idx_add]

  # Organize new regions into a separate grid object
  if (any(idx_add)) {
    grid_add <- grid %>% filter(fips %in% regions_add)
    grid <- grid %>% filter(!fips %in% regions_add)
  }

  datasets <- names(task$source)
  scale <- if (length(task$scale) == 0) NULL else task$scale

  reducers <- list(
    mean = rgee::ee$Reducer$mean(),
    sum = rgee::ee$Reducer$sum(),
    max = rgee::ee$Reducer$max(),
    min = rgee::ee$Reducer$min(),
    median = rgee::ee$Reducer$median(),
    sd = rgee::ee$Reducer$stdDev()
  )

  # Remove tables if necessary
  idx_drop <- str_detect(datasets, "^\\-")
  table_drop <- str_sub(datasets[idx_drop], 2)
  if (length(table_drop) > 0) {
    con <- dbConnect(SQLite(), dbname = db_file)
    dbRemoveTable(con, table_drop) %>% rename(geometry = GEOMETRY)
    dbDisconnect(con)
    datasets <- datasets[!idx_drop]
  }

  for (i in seq_along(datasets)) {
    dataset <- datasets[i]
    bands_raw <- names(task$source[[i]])
    bands <- bands_raw[!str_detect(bands_raw, "^\\-")]

    if (str_detect(dataset, "^\\+")) {
      dataset <- substring(dataset, 2)
      dataset_new <- TRUE
    } else if (state_exists) {
      con <- dbConnect(SQLite(), dbname = db_file)
      table <- dbReadTable(con, dataset, check.names = FALSE) %>%
        filter(!id %in% grid$id[grid$fips %in% regions_drop])
      latest_date <- as.Date(gsub("_", "-", colnames(table)[ncol(table)]))
      dataset_new <- FALSE
    } else {
      dataset_new <- TRUE
    }

    for (j in seq_along(bands)) {
      skip_band <- FALSE
      band <- bands[j]
      stats_raw <- task$source[[i]][[j]]
      stats <- stats_raw[!str_detect(stats_raw, "^\\-")]
      stats_new <- str_detect(stats, "^\\+")
      stats[stats_new] <- str_sub(stats[stats_new], 2)
      stat_funs <- map(stats, ~reducers[[.]])

      if (j == 1) {
        idx_drop_band <- str_detect(bands_raw, "^\\-")
        bands_drop <- str_sub(bands_raw[idx_drop_band], 2)
        idx_drop_stat <- str_detect(stats_raw, "^\\-")
        stats_drop <- str_sub(stats_raw[idx_drop_stat], 2)

        if (any(idx_drop_band, idx_drop_stat)) {
          table <- table %>%
            filter(!(band %in% bands_drop) & !(stat %in% stats_drop))
        }
      }

      if (str_detect(band, "^\\+")) {
        band <- substring(band, 2)
        band_new <- TRUE
      } else {
        band_new <- FALSE
      }

      case <- if (any(dataset_new, band_new, all(stats_new))) {
        1
      } else if (!any(stats_new)) {
        2
      } else {
        3
      }

      source <- append(
        source,
        setNames(list(setNames(list(stats), band)), dataset)
      )

      if (case == 1) {
        imgs_build <- as_tidyee(
          rgee::ee$ImageCollection(dataset)$select(band)
        ) %>% filter(date >= task$start)
        dates_build <- imgs_build$vrt$date
        batch_size <- floor(task$limit / length(dates_build))
      } else if (case == 2) {
        imgs_update <- as_tidyee(
          rgee::ee$ImageCollection(dataset)$select(band)
        )
        if (latest_date > max(as.Date(imgs_update$vrt$date))) {
          imgs_update <- imgs_update %>% filter(date > latest_date)
          dates_update <- imgs_update$vrt$date
          batch_size <- floor(task$limit / length(dates_update))
        } else {
          skip_band <- TRUE
        }
      } else {
        imgs_build <- as_tidyee(
          rgee::ee$ImageCollection(dataset)$select(band)
        ) %>% filter(date >= task$start)
        dates_build <- imgs_build$vrt$date
        imgs_update <- as_tidyee(
          rgee::ee$ImageCollection(dataset)$select(band)
        )
        if (latest_date > max(as.Date(imgs_update$vrt$date))) {
          imgs_update <- imgs_update %>% filter(date > latest_date)
          dates_update <- imgs_update$vrt$date
          batch_size <- floor(task$limit / length(dates_update))
        } else {
          stats <- stats[stats_new]
          stats_new <- rep(TRUE, length(stats))
          batch_size <- floor(task$limit / length(dates_build))
        }
      }

      if (!skip_band) {
        num_bin <- nrow(grid)
        num_batch <- ceiling(num_bin / batch_size)
        batch_id <- split(1:num_bin, rep(1:num_batch, batch_size)[1:num_bin])

        pb_step <- 100 / length(datasets) / length(bands) /
          length(stats) / num_batch

        for (k in seq_along(stats)) {
          build <- any(case == 1, case == 3 && stats_new[k])

          for (l in seq_along(batch_id)) {
            pb$tick(pb_step)
            bins <- batch_id[[l]]

            if (build) {
              suppressMessages(
                batch_stat_build <- ee_extract(
                  x = imgs_build$ee_ob,
                  y = grid$geometry[bins],
                  fun = stat_funs[[k]],
                  sf = FALSE,
                  scale = scale,
                  quiet = TRUE
                ) %>% mutate(
                  id = grid$id[bins], band = band, stat = stats[k]
                ) %>% select(
                  id, band, stat, everything()
                ) %>% rename_all(
                  ~ c("id", "band", "stat", gsub("-", "_",
                                                 as.character(dates_build)))
                )
              )

              if (!exists("grid_stats_build")) {
                grid_stats_build <- batch_stat_build
              } else {
                grid_stats_build <- rbind(grid_stats_build, batch_stat_build)
              }
            } else {
              suppressMessages(
                batch_stat_update <- ee_extract(
                  x = imgs_update$ee_ob,
                  y = grid$geometry[bins],
                  fun = stat_funs[[k]],
                  sf = FALSE,
                  scale = scale,
                  quiet = TRUE
                ) %>% mutate(
                  id = grid$id[bins], band = band, stat = stats[k]
                ) %>% select(
                  id, band, stat, everything()
                ) %>% rename_all(
                  ~ c("id", "band", "stat", gsub("-", "_",
                                                 as.character(dates_update)))
                )
              )

              if (all(any(idx_add), min(bins) <= nrow(grid_add))) {
                bins_add <- bins[bins <= nrow(grid_add)]

                suppressMessages(
                  batch_stat_build <- ee_extract(
                    x = imgs_build$ee_ob,
                    y = grid_add$geometry[bins_add],
                    fun = stat_funs[[k]],
                    sf = FALSE,
                    scale = scale,
                    quiet = TRUE
                  ) %>% mutate(
                    id = grid_add$id[bins_add], band = band, stat = stats[k]
                  ) %>% select(
                    id, band, stat, everything()
                  ) %>% rename_all(
                    ~ c("id", "band", "stat", gsub("-", "_",
                                                   as.character(dates_build)))
                  )
                )

                if (!exists("grid_stats_build")) {
                  grid_stats_build <- batch_stat_build
                } else {
                  grid_stats_build <- rbind(grid_stats_build, batch_stat_build)
                }
              }

              if (!exists("grid_stats_update")) {
                grid_stats_update <- batch_stat_update
              } else {
                grid_stats_update <- rbind(grid_stats_update, batch_stat_update)
              }
            }
          }
        }
      }
    }

    if (dataset_new) {
      con <- dbConnect(RSQLite::SQLite(), dbname = db_file)
      dbWriteTable(
        conn = con, name = dataset, value = grid_stats_build,
        append = TRUE, row.names = FALSE
      )
      dbDisconnect(con)
      rm(grid_stats_build)
    } else {
      if (exists("grid_stats_update")) {
        table <- merge(table, grid_stats_update, by = c("id", "band", "stat"),
                       all.x = TRUE)
        rm(grid_stats_update)
      }

      if (exists("grid_stats_build")) {
        table <- rbind(table, grid_stats_build)
        rm(grid_stats_build)
      }

      dbWriteTable(
        conn = con, name = dataset, value = table, row.names = FALSE,
        overwrite = TRUE
      )
      dbDisconnect(con)
      rm(table)
    }
  }

  # Write state file
  state <- task
  state$regions <- regions
  state$source <- source
  write_json(state, state_file, pretty = TRUE)

  # Write log file
  write_log(!state_exists)

  cat("\n")
  if (!state_exists) {
    cli_alert_info("Database successfully built: 'data/geelite.db'.")
    cli_alert_info("State file generated: 'state/state.json'.")
    cli_alert_info("CLI scripts generated: 'cli/R functions'.")
  } else {
    cli_alert_info("Database successfully updated: 'data/geelite.db'.")
    cli_alert_info("State file updated: 'state/state.json'.")
    cli_alert_info("CLI scripts updated: 'cli/R functions'.")
  }
  cat("\n")
}


# ------------------------------------------------------------------------------

write_log <- function(build) {

  sys_time <- format(Sys.time(), "%Y-%m-%d %H:%M")

  if (build) {
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
