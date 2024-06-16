pkg <- "optparse"
if (length(pkg <- setdiff(pkg, rownames(installed.packages()))))
install.packages(pkg)
rm(pkg)

library(optparse)
library(geeLite)

option_list <- list(
  make_option(c("--path"), type = "character", help = paste0("[mandatory] ",
  "Path to the root directory of the generated database."))
)

option_parser <- OptionParser(
  usage = paste0("Usage: set_cli.R --path [path]"),
  option_list = option_list
)

args <- parse_args(option_parser)

set_cli(args$path)
