#!/usr/bin/env Rscript

suppressMessages(suppressWarnings({
  if (!requireNamespace("optparse", quietly = TRUE)) {
    stop(paste(
      "Package 'optparse' is required but not installed.",
      "Please install it manually."
    ))
  }
  library(optparse)
  library(geeLite)
}))

option_list <- list(
  make_option(c("--admin_lvl"), type = "integer", default = 0, help =
  paste0("[optional] Specifies the administrative level. Use 0 for ",
  "country-level, 1 for state-level, or NULL to include all regions."))
)

option_parser <- OptionParser(
  usage = "usage: %prog [options]",
  option_list = option_list,
  description = "Fetch ISO 3166 country and subdivision codes."
)

args <- parse_args(option_parser)

fetch_regions(
  admin_lvl = args$admin_lvl
)
