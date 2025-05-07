
<!-- README.md is generated from README.Rmd. Please edit that file -->

# geeLite R Package

<!-- badges: start -->
<!-- badges: end -->

This package streamlines the process of building, managing, and updating
local SQLite databases that contain geospatial features extracted from
Google Earth Engine (GEE).


## Installation

``` r
# install.packages("devtools")
devtools::install_github("mtkurbucz/geeLite")
geeLite::gee_install()
```

## Usage

1) Loading the package:

``` r
library(geeLite)
```

2) Setting the configuration file:

``` r
path <- "path/to/db"

set_config(path = path,
           regions = c("SO", "YE"),
           source = list(
              "MODIS/061/MOD13A2" = list(
                "NDVI" = c("mean", "sd")
              )
           ),
           resol = 3,
           start = "2020-01-01")
```

3) Collecting GEE data based on the configuration file:

``` r
run_geelite(path = path)
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#> geeLite R Package - Version: 0.1.0
#> ────────────────────────────────────────────────────────────────────────────────
#> 
#> ── rgee 1.1.7 ─────────────────────────────────────── earthengine-api 0.1.370 ── 
#>  ✔ User: not defined 
#>  ✔ Initializing Google Earth Engine:  DONE!
#>  ✔ Earth Engine account: user
#>  ✔ Python path: C:/.../AppData/Local/r-miniconda/envs/rgee/python.exe 
#> ────────────────────────────────────────────────────────────────────────────────
#>
#> ℹ Database built successfully.
```

4) Modifying the configuration file:

``` r
modify_config(path = path,
              keys = list(
                c("source", "MODIS/061/MOD13A2", "NDVI"),
                c("source", "MODIS/061/MOD13A2", "EVI")
              ),
              new_values = list(
                c("mean", "min", "max"),
                c("mean", "sd")
              ))
```

5) Updating the database based on the configuration file:

``` r
run_geelite(path = path)
#> 
#> ────────────────────────────────────────────────────────────────────────────────
#> geeLite R Package - Version: 0.1.0
#> ────────────────────────────────────────────────────────────────────────────────
#> 
#> ── rgee 1.1.7 ─────────────────────────────────────── earthengine-api 0.1.370 ── 
#>  ✔ User: not defined 
#>  ✔ Initializing Google Earth Engine:  DONE!
#>  ✔ Earth Engine account: user
#>  ✔ Python path: C:/.../AppData/Local/r-miniconda/envs/rgee/python.exe
#> ────────────────────────────────────────────────────────────────────────────────
#>
#> ℹ Database updated successfully.
```

6) Reading the generated database:

``` r
# Fetch SQLite database:
# 1) Convert data to daily format and apply default linear interpolation ('prep_fun').
# 2) Aggregate data to default monthly frequency ('freq') using mean and standard deviation aggregation ('aggr_funs').

db <- read_db(path = path, aggr_funs = list(
  function(x) mean(x, na.rm = TRUE),
  function(x) sd(x, na.rm = TRUE))
)
```

## Drive Mode

To efficiently handle large data requests, `geeLite` provides a `drive` mode. 
In this mode, data are exported from Google Earth Engine to Google Drive in 
parallel batches and then imported into your local SQLite database. Make sure 
you have sufficient Google Drive storage available before using this mode.

``` r
# Collect and store data using drive mode
run_geelite(path = path, mode = "drive")
```

## Command-Line Interface (CLI) Usage

You can execute the previous example using the command-line interface (CLI) as 
follows:

``` bash
# Setting the CLI files
Rscript /path/to/geeLite/cli/set_cli.R --path "path/to/db"

# Change directory to where the database will be generated
cd "path/to/db"

# Setting the configuration file
Rscript cli/set_config.R --regions "SO YE" --source "list('MODIS/061/MOD13A2' = list('NDVI' = c('mean', 'min')))" --resol 3 --start "2020-01-01"

# Collecting GEE data based on the configuration file
Rscript cli/run_geelite.R

# Modifying the configuration file
Rscript cli/modify_config.R --keys "list(c('source', 'MODIS/061/MOD13A2', 'NDVI'), c('source', 'MODIS/061/MOD13A2', 'EVI'))" --new_values "list(c('mean', 'min', 'max'), c('mean', 'sd'))"

# Updating the database based on the configuration file
Rscript cli/run_geelite.R
```

## Further documentation

A pdf manual with additional documentation and html file with example use are provided in [/docs/](./docs/).

## Acknowledgments

Funding by the World Bank’s Food Systems 2030 (FS2030) Multi-Donor Trust Fund program (TF0C0728 and TF0C7822) is gratefully acknowledged. We thank Andres Chamorro and Ben P. Stewart for code testing and comments, as well as Steve Penson, David Newhouse and Alia J. Aghjanian for helpful comments and input. This paper reflects the views of the authors and does not reflect the official views of the World Bank, its Executive Directors, or the countries they represent. This paper reflects the views of the authors, and does not reflect the official views of the World Bank, its Executive Directors, or the countries they represent.
