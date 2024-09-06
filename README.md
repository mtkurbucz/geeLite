
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
           start = "2010-01-01")
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
              target = list(
                c("source", "MODIS/061/MOD13A2", "NDVI"),
                c("source", "MODIS/061/MOD13A2", "EVI")
              ),
              values = list(
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

6) Reading the Generated Database:

``` r
# Reading SQLite tables:
db <- read_db(path = "path/to/db")

# Reading SQLite tables aggregated by monthly mean:
db_aggr <- read_db(path = "path/to/db", freq = "month", temp_stats = "mean")
```

## Command-Line Interface (CLI) Usage

You can execute the previous example using the command-line interface (CLI) as follows:

``` bash
# Setting the CLI files:
Rscript /path/to/geeLite/cli/set_cli.R --path "path/to/db"

# Change directory to where the database will be generated:
cd "path/to/db"

# Setting the configuration file:
Rscript cli/set_config.R --regions "SO YE" --source "list('MODIS/061/MOD13A2' = list('NDVI' = c('mean', 'min')))" --resol 3 --start "2020-01-01"

# Collecting GEE data based on the configuration file:
Rscript cli/run_geelite.R

# Modifying the configuration file:
Rscript cli/modify_config.R --target "list(c('source', 'MODIS/061/MOD13A2', 'NDVI'), c('source', 'MODIS/061/MOD13A2', 'EVI'))" --values "list(c('mean', 'min', 'max'), c('mean', 'sd'))"

# Updating the database based on the configuration file:
Rscript cli/run_geelite.R
```
