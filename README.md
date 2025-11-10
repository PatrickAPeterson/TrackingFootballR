
<!-- README.md is generated from README.Rmd. Please edit that file -->

# trackingfootballR

<!-- badges: start -->

<!-- badges: end -->

The goal of trackingfootballR is to to provide a clean method of parsing
data from the Tracking Football API

## Installation

You can install the development version of trackingfootballR from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("ajplum/trackingfootballR")
```

## Example

This is a basic example which shows you how to utilize the functions of
trackingfootballR and return JSON listed data:

``` r
library(trackingfootballR)

api_key = "YOUR-API-KEY"

# hs_player_details <- get_all_hs_player_details(api_key, updated_since = "2025-11-10")
# ncaa_player_details <- get_all_ncaa_player_details(api_key, updated_since = "2025-11-10")
```
