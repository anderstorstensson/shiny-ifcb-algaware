# Read LIMS discrete chlorophyll data (AlgAware-matched)

Read LIMS discrete chlorophyll data (AlgAware-matched)

## Usage

``` r
read_lims_data(lims_path, algaware_stations)
```

## Arguments

- lims_path:

  Path to LIMS data.txt file.

- algaware_stations:

  Data frame from
  [`load_algaware_stations()`](https://nodc-sweden.github.io/ifcb-algaware/reference/load_algaware_stations.md).

## Value

A data.frame with columns: station_short, station_name, coast,
sample_date, sample_month, DEPH, CPHL, latitude, longitude. NULL if file
not found or no valid data.
