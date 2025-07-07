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
  make_option(c("--verbose"), type = "logical", default = TRUE,
    help = "[optional] Display messages.")
)

option_parser <- OptionParser(
  usage = "usage: %prog [options]",
  option_list = option_list,
  description = "Initialize post-processing folder and files."
)

args <- parse_args(option_parser)

init_postp(
  path = path,
  verbose = args$verbose
)
