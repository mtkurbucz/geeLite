# This line is used by the 'set_cli' function

pkg <- "optparse"
if (length(pkg <- setdiff(pkg, rownames(installed.packages()))))
  install.packages(pkg)
rm(pkg)
library(optparse)
library(geeLite)

option_list <- list(
  make_option(c("--verbose"), type = "logical", default = TRUE,
              help = "[optional] Display messages.")
)

option_parser <- OptionParser(
  usage = paste0("Usage: init_postp.R --verbose [verbose]"),
  option_list = option_list
)

args <- parse_args(option_parser)

init_postp(
  path = path,
  verbose = args$verbose
)
