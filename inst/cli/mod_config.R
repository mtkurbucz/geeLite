# This line is used by the set_cli function

pkg <- "optparse"
if (length(pkg <- setdiff(pkg, rownames(installed.packages()))))
  install.packages(pkg)
rm(pkg)
library(optparse)
library(geeLite)

option_list <- list(
  make_option(c("--target"), type = "list", help = paste0("[mandatory] ",
  "Specifies the path to the values to be replaced."),
  make_option(c("--values"), type = "list", help = paste0("[mandatory] New ",
  "values to replace the original ones.")))
)

option_parser <- OptionParser(
  usage = "Usage: mod_config.R --target [target] --values [values]",
  option_list = option_list
)

args <- parse_args(option_parser)

mod_config(path = path,
           target = args$target,
           values = args$values)
