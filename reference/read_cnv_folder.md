# Parse all CNV files from a folder tree (AlgAware-matched, for chl map)

Parse all CNV files from a folder tree (AlgAware-matched, for chl map)

## Usage

``` r
read_cnv_folder(cnv_folder, algaware_stations)
```

## Arguments

- cnv_folder:

  Path to parent Cnv/ folder (searched recursively).

- algaware_stations:

  Data frame from
  [`load_algaware_stations()`](https://nodc-sweden.github.io/ifcb-algaware/reference/load_algaware_stations.md).

## Value

A data.frame or NULL.
