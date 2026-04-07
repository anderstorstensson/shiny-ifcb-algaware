# Compute 0-20m depth-averaged chlorophyll from CTD data

Compute 0-20m depth-averaged chlorophyll from CTD data

## Usage

``` r
compute_ctd_chl_avg(ctd_data)
```

## Arguments

- ctd_data:

  Data frame from
  [`read_cnv_folder()`](https://nodc-sweden.github.io/ifcb-algaware/reference/read_cnv_folder.md).

## Value

Data frame with station_short, latitude, longitude, chl_mean.
