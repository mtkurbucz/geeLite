# This line is used by the set_cli function

pkg <- "optparse"
if (length(pkg <- setdiff(pkg, rownames(installed.packages()))))
  install.packages(pkg)
rm(pkg)
library(optparse)
library(geeLite)

option_list <- list(
  make_option(c("--regions"), type = "character", help = paste0("[mandatory] ",
  "FIPS codes of the regions of interest (two-letter country code, followed ",
  "by an optional two-digit state code).")),
  make_option(c("--source"), type = "character", help = paste0("[mandatory] ",
  "Description of GEE datasets of interest. It is a nested list with three ",
  "levels (datasets, bands, and stats).")),
  make_option(c("--resol"), type = "integer", help = paste0("[mandatory] ",
  "Resolution of the H3 bin.")),
  make_option(c("--scale"), type = "integer", default = NULL, help = paste0(
  "[optional] Scale of images before processing.")),
  make_option(c("--start"), type = "character", default = "2000-01-01", help =
  paste0("[optional] First date of the data collection.")),
  make_option(c("--limit"), type = "integer", default = 10000, help = paste0(
  "[optional] GEE’s limit to process multiple bins simultaneously. Limit - 1 ",
  "bins will be processed at the same time.")),
  make_option(c("--crs"), type = "integer", default = NULL, help = paste0(
  "[optional] CRS to be assigned to the dataset (default is the shapefile's ",
  "CRS)."))
)

option_parser <- OptionParser(
  usage = paste0("Usage: set_config.R --regions [regions] --source [source] --resol ",
  "[resol] --scale [scale] --start [start] --limit [limit] --crs [crs]"),
  option_list = option_list
)

args <- parse_args(option_parser)
args$regions <- unlist(strsplit(args$regions, " "))
args$source <- eval(parse(text = args$source))

set_config(
  path = path,
  regions = args$regions,
  source = args$source,
  resol = args$resol,
  scale = args$scale,
  start = args$start,
  limit = args$limit,
  crs = args$crs
)
