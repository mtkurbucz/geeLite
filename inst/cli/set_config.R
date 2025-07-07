# This line is used by the 'set_cli' function

pkg <- "optparse"
if (length(pkg <- setdiff(pkg, rownames(installed.packages()))))
install.packages(pkg)
rm(pkg)

suppressMessages(suppressWarnings({
  library(optparse)
  library(geeLite)
}))

option_list <- list(
  make_option(c("--regions"), type = "character", help = paste0("[mandatory] ",
  "FIPS codes of the regions of interest (two-letter country code, followed ",
  "by an optional two-digit state code).")),
  make_option(c("--source"), type = "character", help = paste0("[mandatory] ",
  "Description of GEE datasets. Should be a quoted nested list of datasets, ",
  "bands, and zonal statistics.")),
  make_option(c("--resol"), type = "integer", help = paste0("[mandatory] ",
  "Resolution of the H3 bin.")),
  make_option(c("--scale"), type = "integer", default = NULL,
  help = paste0("[optional] Nominal image resolution (in meters) for ",
  "processing. If not specified (NULL), a default of 1000 is used.")),
  make_option(c("--start"), type = "character", default = "2020-01-01",
  help = "[optional] First date of the data collection."),
  make_option(c("--limit"), type = "integer", default = 10000, help = paste0(
  "[optional] In 'local'' mode, `limit / dates` sets batch size; in 'drive'
  mode, `limit` is the max features per export.")),
  make_option(c("--verbose"), type = "logical", default = TRUE,
  help = "[optional] Display messages.")
)

option_parser <- OptionParser(
  usage = "usage: %prog [options]",
  option_list = option_list,
  description = "Initialize the configuration file."
)

args <- parse_args(option_parser)

if (is.null(args$regions) || args$regions == "" ||
    is.null(args$source)  || args$source == ""  ||
    is.null(args$resol)) {
  print_help(option_parser)
  stop("Missing required arguments: --regions, --source, and/or --resol")
}

args$regions <- unlist(strsplit(args$regions, " "))
args$source <- tryCatch(
  eval(parse(text = args$source)),
  error = function(e) stop("Invalid format for --source: ", e$message)
)

set_config(
  path = path,
  regions = args$regions,
  source = args$source,
  resol = args$resol,
  scale = args$scale,
  start = args$start,
  limit = args$limit,
  verbose = args$verbose
)
