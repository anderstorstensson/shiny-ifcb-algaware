# Parse a single CNV file (AlgAware-matched)

Parse a single CNV file (AlgAware-matched)

## Usage

``` r
parse_single_cnv(file_path, algaware_stations)
```

## Arguments

- file_path:

  Path to a .cnv file.

- algaware_stations:

  Data frame from
  [`load_algaware_stations()`](https://nodc-sweden.github.io/ifcb-algaware/reference/load_algaware_stations.md).

## Value

A data.frame with profile data, or NULL if parsing fails or station is
unmatched.
