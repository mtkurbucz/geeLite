# This line is used by the 'set_cli' function

pkg <- "optparse"
if (length(pkg <- setdiff(pkg, rownames(installed.packages()))))
  install.packages(pkg)
rm(pkg)
library(optparse)
library(geeLite)

option_list <- list(
  make_option(c("--print_output"), type = "logical", default = TRUE, help =
  paste0("[optional] Set to TRUE to print output in markdown format; set to ",
  "FALSE to return a data frame object."))
)

option_parser <- OptionParser(
  usage = paste0("Usage: fetch_tables.R --print_output [print_output]"),
  option_list = option_list
)

args <- parse_args(option_parser)

fetch_tables(path = path,
             print_output = args$print_output)
