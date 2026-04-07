# Match a canonical station name against historical Chl-a statistics

Handles encoding differences between YAML (UTF-8) and stats file
(latin1).

## Usage

``` r
match_stats_station(canonical_name, chl_stats)
```

## Arguments

- canonical_name:

  Canonical station name string.

- chl_stats:

  Data frame from
  [`load_chl_statistics()`](https://nodc-sweden.github.io/ifcb-algaware/reference/load_chl_statistics.md).

## Value

Data frame (subset of chl_stats) for that station, or NULL.
