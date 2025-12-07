suppressPackageStartupMessages({
  library(withr)
})

# Use a consistent timezone for all tests
withr::local_timezone("UTC")

# Optional: if CI/user provides a conda env name, try to bind it quietly.
# CRAN-safe: we only read env vars, we do not set them.
local({
  env <- Sys.getenv("GEELITE_CONDA_ENV", unset = NA)

  if (!is.na(env) && nzchar(env)) {
    try(reticulate::use_condaenv(env, required = TRUE), silent = TRUE)
  }

  # Never authenticate or open a browser in tests.
  # Only attempt silent initialization if credentials already exist.
  try(rgee::ee_Initialize(drive = FALSE, quiet = TRUE), silent = TRUE)
})
