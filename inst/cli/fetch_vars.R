# This line is used by the 'set_cli' function

pkg <- "optparse"
if (length(pkg <- setdiff(pkg, rownames(installed.packages()))))
  install.packages(pkg)
rm(pkg)
library(optparse)
library(geeLite)

option_list <- list(
  make_option(c("--format"), type = "character", default = "data.frame", help =
  paste0("[optional] Possible values are 'data.frame' to return a ",
         "'data.frame' object, or one of 'latex', 'html', 'pipe' (Pandoc's ",
         "pipe tables), 'simple' (Pandoc's simple tables), and 'rst' for ",
         "formatting with knitr."))
)

option_parser <- OptionParser(
  usage = paste0("Usage: fetch_tables.R --format [format]"),
  option_list = option_list
)

args <- parse_args(option_parser)

fetch_vars(path = path,
           format = args$format)
