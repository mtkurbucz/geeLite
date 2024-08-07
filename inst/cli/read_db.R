# This line is used by the 'set_cli' function

pkg <- "optparse"
if (length(pkg <- setdiff(pkg, rownames(installed.packages()))))
  install.packages(pkg)
rm(pkg)
library(optparse)
library(geeLite)

option_list <- list(
  make_option(c("--tables"), type = "character", default = "all", help = paste0(
  "[optional] Names or IDs of the selected tables. To identify available ",
  "tables and their names or IDs, use the 'get_tables' function.")),
  make_option(c("--freq"), type = "character", default = NULL, help =
  paste0("[optional] Specifies the frequency for aggregation. Valid entries are
  NULL, 'month', 'year'. Default is NULL, which means no aggregation.")),
  make_option(c("--freq_stats"), type = "character", default = NULL, help =
  paste0("[optional] A character vector of statistical functions aggregation is
  based on. Valid entries are NULL, 'mean', 'median', 'min', 'max', 'sd'.
  Default is NULL, which means no aggregation."))
)

option_parser <- OptionParser(
  usage = paste0("Usage: read_db.R --table [table] --freq [freq] --freq_stats ",
  "[freq_stats]"),
  option_list = option_list
)

args <- parse_args(option_parser)
args$tables <- type.convert(args$tables, as.is = TRUE)
args$freq_stats <- unlist(strsplit(args$freq_stats, " "))

read_db(path = path,
        tables = args$tables,
        freq = args$freq,
        freq_stats = args$freq_stats)
