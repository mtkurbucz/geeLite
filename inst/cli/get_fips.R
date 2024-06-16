pkg <- "optparse"
if (length(pkg <- setdiff(pkg, rownames(installed.packages()))))
  install.packages(pkg)
rm(pkg)
library(optparse)
library(geeLite)

option_list <- list(
  make_option(c("--type"), type = "character", default = "country", help =
  paste0("[optional] Type of the regions to be printed (options: 'state', ",
  "'country', 'all')."))
)

option_parser <- OptionParser(
  usage = "Usage: get_fips.R --type [type]",
  option_list = option_list
)

args <- parse_args(option_parser)

get_fips(type = args$type)
