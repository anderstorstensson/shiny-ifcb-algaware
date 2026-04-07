# Compute 0-20m depth-averaged chlorophyll from LIMS bottle data

Uses discrete bottle samples only (excludes hose/SLA samples). Duplicate
station rows arising from slightly varying coordinates are collapsed by
aggregating on `station_short` only.

## Usage

``` r
compute_lims_chl_avg(lims_data)
```

## Arguments

- lims_data:

  Data frame from
  [`read_lims_data()`](https://nodc-sweden.github.io/ifcb-algaware/reference/read_lims_data.md).

## Value

Data frame with station_short, latitude, longitude, chl_mean.
