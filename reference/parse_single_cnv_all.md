# Parse a single CNV file using station mapper and standard stations

Parse a single CNV file using station mapper and standard stations

## Usage

``` r
parse_single_cnv_all(file_path, station_mapper, standard_stations)
```

## Arguments

- file_path:

  Path to a .cnv file.

- station_mapper:

  Data frame from
  [`load_station_mapper()`](https://nodc-sweden.github.io/ifcb-algaware/reference/load_station_mapper.md).

- standard_stations:

  Data frame from
  [`load_standard_stations()`](https://nodc-sweden.github.io/ifcb-algaware/reference/load_standard_stations.md).

## Value

A data.frame with profile data including canonical_name and region, or
NULL if parsing fails or station cannot be matched.
