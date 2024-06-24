pkg <- "optparse"
if (length(pkg <- setdiff(pkg, rownames(installed.packages()))))
  install.packages(pkg)
rm(pkg)
library(optparse)
library(geeLite)

option_list <- list(
  make_option(c("--admin_lvl"), type = "integer", default = 0, help =
  paste0("[optional] Specifies the administrative level. Use 0 for ",
  "country-level, 1 for state-level, or NULL to include all regions."))
)

option_parser <- OptionParser(
  usage = "Usage: get_regids.R --admin_lvl [admin_lvl]",
  option_list = option_list
)

args <- parse_args(option_parser)

get_regids(admin_lvl = args$admin_lvl)
