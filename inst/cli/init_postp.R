#!/usr/bin/env Rscript

# This line is used by the 'set_cli' function

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
  make_option(c("--verbose"), type = "logical", default = TRUE,
              help = "[optional] Display messages [default: TRUE]")
)

option_parser <- OptionParser(
  usage = "usage: %prog [options]",
  option_list = option_list,
  description = "Initialize post-processing folder and files."
)
args <- parse_args(option_parser)

init_postp(
  path = path,
  verbose = args$verbose
)
