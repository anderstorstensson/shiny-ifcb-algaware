# Read LIMS discrete chlorophyll data (all standard stations)

Like
[`read_lims_data()`](https://nodc-sweden.github.io/ifcb-algaware/reference/read_lims_data.md)
but matches against all standard monitoring stations instead of just
AlgAware stations.

## Usage

``` r
read_lims_data_all(lims_path, station_mapper, standard_stations)
```

## Arguments

- lims_path:

  Path to LIMS data.txt file.

- station_mapper:

  Data frame from
  [`load_station_mapper()`](https://nodc-sweden.github.io/ifcb-algaware/reference/load_station_mapper.md).

- standard_stations:

  Data frame from
  [`load_standard_stations()`](https://nodc-sweden.github.io/ifcb-algaware/reference/load_standard_stations.md).

## Value

A data.frame with columns: canonical_name, region, sample_date,
sample_month, DEPH, CPHL, latitude, longitude. NULL if file not found or
no valid data.
