# This line is used by the 'set_cli' function

pkg <- "optparse"
if (length(pkg <- setdiff(pkg, rownames(installed.packages()))))
install.packages(pkg)
rm(pkg)
library(optparse)
library(geeLite)

option_list <- list(
  make_option(c("--conda"), type = "character", default = "rgee", help =
  paste0("[optional] Name of the virtual Conda environment installed and used ",
  "by the rgee package")),
  make_option(c("--user"), type = "character", default = NULL, help = paste0(
  "[optional] Used to create a folder within the path ",
  "~/.config/earthengine/.")),
  make_option(c("--rebuild"), type = "logical", default = FALSE, help = paste0(
  "[optional] If set to TRUE, the database and its supplementary files will ",
  "be overwritten based on the configuration file")),
  make_option(c("--mode"), type = "character", default = "local", help = paste0(
    "[optional] Mode of data extraction. Currently supports 'local' or
    'drive'.")),
  make_option(c("--verbose"), type = "logical", default = TRUE,
  help = "[optional] Display computation status and messages.")
)

option_parser <- OptionParser(
  usage = paste0("Usage: run_geelite.R --conda [conda] --rebuild [rebuild] ",
  "--verbose [verbose]"),
  option_list = option_list
)

args <- parse_args(option_parser)

run_geelite(path = path,
            conda = args$conda,
            user = args$user,
            rebuild = args$rebuild,
            mode = args$mode,
            verbose = args$verbose)
