# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(geeLite)

# Apply UTC time zone for all tests in the geeLite package
withr::local_timezone("UTC")

# List required suggested packages
required_pkgs <- c(
  "rnaturalearthdata", "geojsonio", "withr", "jsonlite", "RSQLite"
  )

# Identify missing packages
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace,
                                      logical(1), quietly = TRUE)]

# Load optional packages with suppressed startup messages and warnings
if (length(missing_pkgs) == 0) {
  test_check("geeLite")
} else {
  message("Skipping tests because required packages are missing: ",
          paste(missing_pkgs, collapse = ", "))
}
