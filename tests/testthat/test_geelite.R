pkgs <- c("rnaturalearthdata", "geojsonio", "withr")
if (length(pkgs <- setdiff(pkgs, rownames(installed.packages()))))
  install.packages(pkgs, repos = "https://cloud.r-project.org")
rm(pkgs)

suppressMessages({
  suppressWarnings({
    library(rnaturalearthdata)
    library(geojsonio)
    library(jsonlite)
    library(RSQLite)
    library(withr)
  })
})

test_that("Testing geeLite Package Pipeline", {

  # Set up test environment
  test_path <- local_tempdir()

  # ----------------------------------------------------------------------------
  # Step 1: Generate configuration file
  # ----------------------------------------------------------------------------

  regions <- c("SO", "YM")
  source <- list(
    "MODIS/006/MOD13A2" = list(
      "NDVI" = c("mean", "sd")
    )
  )
  start <- "2020-01-01"
  resol <- 3

  set_config(path = test_path,
             regions = regions,
             source = source,
             start = start,
             resol = resol,
             verbose = FALSE)

  # Check if the config file is created
  config_file <- file.path(test_path, "config", "config.json")
  expect_true(file.exists(config_file))

  # ----------------------------------------------------------------------------
  # Step 2: Create database
  # ----------------------------------------------------------------------------

  run_geelite(path = test_path)

  # Check if the database is created
  db_file <- file.path(test_path, "data", "geelite.db")
  expect_true(file.exists(db_file))

  # Check the content of the database
  con <- dbConnect(SQLite(), dbname = db_file)
  df <- dbReadTable(con, "MODIS/006/MOD13A2")
  expect_equal(sort(unique(df$band)), sort("NDVI"))
  expect_equal(sort(unique(df$stat)), sort(c("mean", "sd")))
  dbDisconnect(con)

  # Testing get_config and get_state functions
  config <- get_config(path = test_path)
  state <- get_state(path = test_path)
  expect_equal(config, state)

  # Testing read_db function
  db <- read_db(path = test_path)
  expect_equal(names(db), c("grid", "MODIS/006/MOD13A2"))
  rm(db)

  # ----------------------------------------------------------------------------
  # Step 3: Modify configuration file
  # ----------------------------------------------------------------------------

  mod_config(path = test_path,
             target = list(c("source", "MODIS/006/MOD13A2", "NDVI")),
             values = list(c("mean", "max")))

  # Check if the config file is updated
  config <- fromJSON(config_file)
  expected_source <- list(
    `MODIS/006/MOD13A2` = list(
      NDVI = c("mean", "max")
    )
  )
  expect_equal(config$source, expected_source)

  # ----------------------------------------------------------------------------
  # Step 4: Update database
  # ----------------------------------------------------------------------------

  run_geelite(path = test_path, verbose = FALSE)

  # Check the content of the updated database
  con <- dbConnect(SQLite(), dbname = db_file)
  df <- dbReadTable(con, "MODIS/006/MOD13A2")
  expect_equal(sort(unique(df$band)), sort(c("NDVI")))
  expect_equal(sort(unique(df$stat)), sort(c("mean", "max")))
  dbDisconnect(con)

})
