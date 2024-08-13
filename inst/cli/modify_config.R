# This line is used by the 'set_cli' function

pkg <- "optparse"
if (length(pkg <- setdiff(pkg, rownames(installed.packages()))))
install.packages(pkg)
rm(pkg)
library(optparse)
library(geeLite)

option_list <- list(
  make_option(c("--keys"), type = "character", help = paste0("[mandatory] ",
  "Specifies the path to the values in the configuration file that need ",
  "updating.")),
  make_option(c("--new_values"), type = "character", help =
  paste0("[mandatory] New values to replace the original values specified by ",
  "'keys'."))
)

option_parser <- OptionParser(
  usage = "Usage: modify_config.R --keys [keys] --new_values [new_values]",
  option_list = option_list
)

args <- parse_args(option_parser)
args$keys <- eval(parse(text = args$keys))
args$new_values <- eval(parse(text = args$new_values))

modify_config(path = path,
              keys = args$keys,
              new_values = args$new_values)
