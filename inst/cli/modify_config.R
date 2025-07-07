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
  make_option(c("--keys"), type = "character", help = paste0("[mandatory] ",
  "Specifies the path to the values in the configuration file that need ",
  "updating.")),
  make_option(c("--new_values"), type = "character", help =
  paste0("[mandatory] New values to replace the original values specified by ",
  "'keys'."))
)

option_parser <- OptionParser(
  usage = "usage: %prog [options]",
  option_list = option_list,
  description = "Modify configuration file."
)

args <- parse_args(option_parser)

if (is.null(args$keys) || is.null(args$new_values)) {
  stop("Both --keys and --new_values are required.")
}

tryCatch({
  args$keys <- eval(parse(text = args$keys))
  args$new_values <- eval(parse(text = args$new_values))
}, error = function(e) {
  stop("Invalid format for --keys or --new_values: ", e$message)
})

modify_config(
  path = path,
  keys = args$keys,
  new_values = args$new_values
)
