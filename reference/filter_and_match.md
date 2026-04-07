# Filter and match metadata to stations

Filters the full dashboard metadata by cruise number or date range and
spatially matches the resulting bins to AlgAware monitoring stations.

## Usage

``` r
filter_and_match(
  dashboard_metadata,
  selection_mode,
  cruise,
  date_range,
  extra_stations
)
```

## Arguments

- dashboard_metadata:

  Data frame from
  [`fetch_dashboard_metadata()`](https://nodc-sweden.github.io/ifcb-algaware/reference/fetch_dashboard_metadata.md).

- selection_mode:

  Character; `"cruise"` or `"date"`.

- cruise:

  Cruise number string (used when `selection_mode = "cruise"`).

- date_range:

  Length-2 Date vector (used when `selection_mode = "date"`).

- extra_stations:

  List of extra station definitions (from settings).

## Value

A list with `matched` (data frame) and `cruise_info` (string).
