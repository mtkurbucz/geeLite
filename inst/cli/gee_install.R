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
  make_option(c("--conda"), type = "character", default = "rgee", help =
                paste0("[optional] Name of the virtual Conda environment ",
                       "installed and used by the 'rgee' package")),
  make_option( c("--python_version"), type = "character", default = "3.10",
               help = paste0("[optional] Python version to use in Conda ",
                             "environment")),
  make_option(c("--force_recreate"), type = "logical", default = FALSE,
              help = paste0("[optional] Force recreate Conda env if it exists"))
)

option_parser <- OptionParser(
  usage = "usage: %prog [options]",
  option_list = option_list,
  description = "Installs and configures a Conda environment for 'rgee'."
)

args <- parse_args(option_parser)

gee_install(
  conda = args$conda,
  python_version = args$python_version,
  force_recreate = args$force_recreate
)
