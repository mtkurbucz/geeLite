pkg <- "optparse"
if (length(pkg <- setdiff(pkg, rownames(installed.packages()))))
install.packages(pkg)
rm(pkg)
library(optparse)
library(geeLite)

option_list <- list(
  make_option(c("--conda"), type = "character", default = "rgee", help =
                paste0("[optional] Name of the virtual Conda environment ",
                "installed and used by the 'rgee' package"))
)

option_parser <- OptionParser(
  usage = "Usage: gee_install.R --conda [conda]",
  option_list = option_list
)

gee_install(conda = args$conda)
