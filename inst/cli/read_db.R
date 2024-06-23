# This line is used by the set_cli function

pkg <- "optparse"
if (length(pkg <- setdiff(pkg, rownames(installed.packages()))))
  install.packages(pkg)
rm(pkg)
library(optparse)
library(geeLite)

option_list <- list(
  make_option(c("--tables"), type = "character", default = "all", help = paste0(
  "[optional] Names or IDs of the selected tables. To identify available ",
  "tables and their names or IDs, use the get_tables function."))
)

option_parser <- OptionParser(
  usage = "Usage: run_geelite.R --conda [conda] --rebuild [rebuild]",
  option_list = option_list
)

args <- parse_args(option_parser)
args$tables <- type.convert(args$tables, as.is = TRUE)

read_db(path = path,
        tables = args$tables)
