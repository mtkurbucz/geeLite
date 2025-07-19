
<!-- README.md is generated from README.Rmd. Please edit that file -->

# geeLite R Package

<!-- badges: start -->
[![R-hub check](https://github.com/mtkurbucz/geeLite/actions/workflows/rhub.yaml/badge.svg)](https://github.com/mtkurbucz/geeLite/actions/workflows/rhub.yaml)
<!-- badges: end -->

This package streamlines the process of building, managing, and updating
local `'SQLite'` databases that contain geospatial features extracted from
'Google Earth Engine' ('GEE').

<blockquote style="font-size: 85%; font-style: italic; border: 1px solid #ccc; padding: 10px; background-color: #f9f9f9;">
For a full description of the methodology, implementation, and use cases, see the accompanying World Bank Policy Research Working Paper: Kurbucz, Marcell T.; Andrée, Bo Pieter Johannes. (2025). <i>Building and Managing Local Databases from Google Earth Engine with the geeLite R Package.</i> Policy Research Working Paper 11115, World Bank. <a href="http://hdl.handle.net/10986/43165">http://hdl.handle.net/10986/43165</a>. License: CC BY 3.0 IGO.
</blockquote>

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
#> geeLite R Package
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
#> geeLite R Package
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

To efficiently handle large data requests, `'geeLite'` provides a `'drive'` 
mode. In this mode, data are exported from 'Google Earth Engine' to 
'Google Drive' in parallel batches and then imported into your local 'SQLite' 
database. Ensure sufficient available storage on your linked Google Drive 
account before using this mode.

``` r
# Collect and store data using drive mode
run_geelite(path = path, mode = "drive")
```

## Command-Line Interface (CLI) Usage

The `geeLite` package includes a command-line interface (CLI) for advanced 
users and automation workflows. All major operations can be performed directly 
from the terminal using `Rscript`.

The following example demonstrates how to configure and manage a database using 
the CLI:

``` bash
# Set the CLI files
Rscript /path/to/geeLite/cli/set_cli.R --path "path/to/db"

# Navigate to the directory where the database will be generated
cd "path/to/db"

# Create a configuration file
Rscript cli/set_config.R --regions "SO YE" --source "list('MODIS/061/MOD13A2' = list('NDVI' = c('mean', 'min')))" --resol 3 --start "2020-01-01"

# Run the data collection based on the configuration
Rscript cli/run_geelite.R

# Modifying the configuration file
Rscript cli/modify_config.R --keys "list(c('source', 'MODIS/061/MOD13A2', 'NDVI'), c('source', 'MODIS/061/MOD13A2', 'EVI'))" --new_values "list(c('mean', 'min', 'max'), c('mean', 'sd'))"

# Update the database with the modified configuration
Rscript cli/run_geelite.R
```

## Citation

If you use `geeLite` in your research, please cite:

<blockquote style="font-size: 90%; font-style: italic; border: 1px solid #ccc; padding: 10px; background-color: #f9f9f9;">
Kurbucz, Marcell T.; Andrée, Bo Pieter Johannes. (2025). <i>Building and Managing Local Databases from Google Earth Engine with the geeLite R Package.</i> Policy Research Working Paper 11115. © World Bank. <a href="http://hdl.handle.net/10986/43165">http://hdl.handle.net/10986/43165</a>. License: CC BY 3.0 IGO.
</blockquote>

## Further Documentation

Additional documentation and usage examples are available at:
[https://github.com/mtkurbucz/geeLite/tree/main/docs](https://github.com/mtkurbucz/geeLite/tree/main/docs)

## Data Availability Statement

All geospatial datasets are retrieved from the [Google Earth Engine public data catalog](https://developers.google.com/earth-engine/datasets). Users must have a registered Google account with GEE access. No proprietary or restricted data are used in this package.

## Acknowledgments

Funding by the World Bank’s Food Systems 2030 (FS2030) Multi-Donor Trust Fund program (TF0C0728 and TF0C7822) is gratefully acknowledged. We thank Andres Chamorro and Ben P. Stewart for code testing and comments, as well as Steve Penson, David Newhouse and Alia J. Aghjanian for helpful comments and input. This paper reflects the views of the authors and does not reflect the official views of the World Bank, its Executive Directors, or the countries they represent.
