pkg <- "optparse"
if (length(pkg <- setdiff(pkg, rownames(installed.packages()))))
install.packages(pkg)
rm(pkg)

suppressMessages(suppressWarnings({
  library(optparse)
  library(geeLite)
}))

option_list <- list(
  make_option(c("--admin_lvl"), type = "integer", default = 0, help =
  paste0("[optional] Specifies the administrative level. Use 0 for ",
  "country-level, 1 for state-level, or NULL to include all regions."))
)

option_parser <- OptionParser(
  usage = "usage: %prog [options]",
  option_list = option_list,
  description = "Fetch ISO 3166-2 region codes."
)

args <- parse_args(option_parser)

fetch_regions(
  admin_lvl = args$admin_lvl
)
