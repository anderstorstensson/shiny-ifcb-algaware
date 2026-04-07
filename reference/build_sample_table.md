# Build the sample summary table

Transforms raw matched metadata into the display table used by the
samples module. Pure function — no Shiny dependency.

## Usage

``` r
build_sample_table(matched_metadata_all, excluded_samples, raw_dir = NULL)
```

## Arguments

- matched_metadata_all:

  Data frame with at least columns `pid`, `STATION_NAME`,
  `STATION_NAME_SHORT`, `COAST`, and `sample_time`.

- excluded_samples:

  Character vector of PIDs currently excluded.

- raw_dir:

  Path to the raw data directory containing `.roi` files, or `NULL` to
  skip file-size lookup.

## Value

A data frame with columns `Status`, `pid`, `STATION_NAME_SHORT`,
`STATION_NAME`, `Region`, `Time`, and `ROI_MB`.
