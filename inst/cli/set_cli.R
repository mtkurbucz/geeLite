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
  make_option(c("--path"), type = "character", help = paste0("[mandatory] ",
  "Path to the root directory of the generated database.")),
  make_option(c("--verbose"), type = "logical", default = TRUE,
              help = "[optional] Display messages.")
)

option_parser <- OptionParser(
  usage = "usage: %prog [options]",
  option_list = option_list,
  description = "Initialize CLI files."
)

args <- parse_args(option_parser)

if (is.null(args$path)) {
  stop("Missing required argument: --path")
}

set_cli(
  path = args$path,
  verbose = args$verbose
)
