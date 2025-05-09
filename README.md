
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lu.ml

lu.ml is an R package that allows you to create the xgboost-based
machine learning instrument from Lutz and Sand.

## Installation

``` r
# install.packages("remotes")
remotes::install_github("ChandlerLutz/lu.ml")
```

## Example

This example compute the LU-ML instrument for Mian and Sufi (2014,
Econometrica):

``` r
library(data.table)
library(lu.ml)

# Load the Mian-Sufi 2014 data
data(dt_mian_sufi_2014)
# Load the county-level land unavailability data for 2010
data(dt_cnty_lu_2010)

# The house price data must have the columns
# `GEOID`, `index` (of type `Date`) and `hp.target`
dt_mian_sufi_2014 <- dt_mian_sufi_2014 |> 
  _[, .(GEOID = fips, index = as.Date("2002-01-01"), hp.target = house.net.worth)] |> 
  _[!is.na(hp.target)]

# Prepare the land unavailability data: select GEOID and land unavailability columns
# Note that the GEOID must match between the house price data and
# the land unavailability data
dt_cnty_lu_2010 <- dt_cnty_lu_2010 |>
  _[, .(GEOID, unavailable = .SD),
    .SDcols = grep("unavailable", names(dt_cnty_lu_2010), value = TRUE)]

out <- lu_ml_xgboost_time_varying(DT.hp = dt_mian_sufi_2014, DT.lu = dt_cnty_lu_2010)
#> [1] "2002-01-01"

# The column `lu_ml` is the instrument
out[, cor(hp.target, lu_ml_xgboost)]
#> [1] 0.6617946
print(out)
#> Key: <GEOID, index>
#>       GEOID      index    hp.target lu_ml_xgboost
#>      <char>     <Date>        <num>         <num>
#>   1:  01001 2002-01-01  0.004081271  -0.012291229
#>   2:  01003 2002-01-01 -0.054877229  -0.047811000
#>   3:  01073 2002-01-01 -0.019054536  -0.045489953
#>   4:  01083 2002-01-01  0.009087154  -0.019329195
#>   5:  01089 2002-01-01  0.007998732  -0.020065621
#>  ---                                             
#> 932:  55133 2002-01-01 -0.023076424  -0.049685491
#> 933:  55139 2002-01-01 -0.007629516  -0.046950353
#> 934:  55141 2002-01-01 -0.034771975  -0.112442631
#> 935:  56021 2002-01-01 -0.025751241  -0.036947956
#> 936:  56025 2002-01-01  0.003155373  -0.007740992
```
