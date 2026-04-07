# Normalize a raw station name to the canonical name

Looks up in the station mapper, then falls back to standard_stations
direct match.

## Usage

``` r
normalize_station_name(raw_name, station_mapper, standard_stations = NULL)
```

## Arguments

- raw_name:

  Character raw station name from CNV header or LIMS STATN column.

- station_mapper:

  Data frame from
  [`load_station_mapper()`](https://nodc-sweden.github.io/ifcb-algaware/reference/load_station_mapper.md).

- standard_stations:

  Data frame from
  [`load_standard_stations()`](https://nodc-sweden.github.io/ifcb-algaware/reference/load_standard_stations.md).
  If provided, used as fallback when mapper does not match.

## Value

Canonical station name string, or NA_character\_ if no match.
