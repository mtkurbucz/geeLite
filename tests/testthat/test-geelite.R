suppressPackageStartupMessages({
  library(testthat)
  library(withr)
})

test_that("geeLite package pipeline works in local mode", {

  # Suggested / optional packages: skip test if any are missing (CRAN-friendly)
  skip_if_not_installed("rnaturalearthdata")
  skip_if_not_installed("geojsonio")
  skip_if_not_installed("jsonlite")
  skip_if_not_installed("RSQLite")
  skip_if_not_installed("withr")

  # If Earth Engine / rgee is not ready, skip to avoid false failures on CRAN
  if (!check_rgee_ready()) {
    skip("rgee is not ready / Earth Engine auth not available")
  }

  # Set up isolated test environment
  test_path <- local_tempdir()

  # ----------------------------------------------------------------------------
  # Step 1: Generate configuration file
  # ----------------------------------------------------------------------------

  regions <- c("SO", "YE")
  source <- list(
    "MODIS/061/MOD13A2" = list(
      "NDVI" = c("mean", "sd")
    )
  )
  start <- "2020-01-01"
  resol <- 3

  set_config(
    path = test_path,
    regions = regions,
    source = source,
    start = start,
    resol = resol,
    verbose = FALSE
  )

  # Check that config file was created
  config_file <- file.path(test_path, "config/config.json")
  expect_true(file.exists(config_file))

  # ----------------------------------------------------------------------------
  # Step 2: Create database
  # ----------------------------------------------------------------------------

  run_geelite(
    path = test_path,
    mode = "local",
    verbose = FALSE
  )

  # Check that database file was created
  db_file <- file.path(test_path, "data/geelite.db")
  expect_true(file.exists(db_file))

  # Validate database contents
  con <- dbConnect(SQLite(), dbname = db_file)
  df <- dbReadTable(con, "MODIS/061/MOD13A2")
  expect_equal(sort(unique(df$band)), sort("NDVI"))
  expect_equal(sort(unique(df$zonal_stat)), sort(c("mean", "sd")))
  dbDisconnect(con)

  # Test get_config and get_state consistency
  config <- get_config(path = test_path)
  state  <- get_state(path = test_path)
  expect_equal(config, state)

  # Test read_db output structure
  db <- read_db(path = test_path)
  expect_equal(
    names(db),
    c("grid",
      "MODIS/061/MOD13A2/NDVI/mean",
      "MODIS/061/MOD13A2/NDVI/sd")
  )
  rm(db)

  # ----------------------------------------------------------------------------
  # Step 3: Modify configuration file
  # ----------------------------------------------------------------------------

  modify_config(
    path = test_path,
    keys = list("regions", c("source", "MODIS/061/MOD13A2", "NDVI")),
    new_values = list(c("SO", "KE"), c("mean", "max")),
    verbose = FALSE
  )

  # Check that config was updated
  config <- fromJSON(config_file)
  expected_source <- list(
    `MODIS/061/MOD13A2` = list(
      NDVI = c("mean", "max")
    )
  )
  expect_equal(config$source, expected_source)

  # ----------------------------------------------------------------------------
  # Step 4: Update database
  # ----------------------------------------------------------------------------

  run_geelite(
    path = test_path,
    mode = "local",
    verbose = FALSE
  )

  # Validate updated database contents
  con <- dbConnect(SQLite(), dbname = db_file)
  df <- dbReadTable(con, "MODIS/061/MOD13A2")
  expect_equal(sort(unique(df$band)), sort("NDVI"))
  expect_equal(sort(unique(df$zonal_stat)), sort(c("mean", "max")))
  grid <- dbReadTable(con, "grid")
  expect_equal(sort(unique(grid$iso)), sort(c("SO", "KE")))
  dbDisconnect(con)
})
