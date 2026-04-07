# Parse all CNV files from a folder tree (all standard stations)

Reads every .cnv file in a folder tree and matches stations against the
standard monitoring station list (not just AlgAware stations). Use for
CTD profile and time-series plots.

## Usage

``` r
read_cnv_folder_all(cnv_folder, station_mapper, standard_stations)
```

## Arguments

- cnv_folder:

  Path to parent Cnv/ folder (searched recursively).

- station_mapper:

  Data frame from
  [`load_station_mapper()`](https://nodc-sweden.github.io/ifcb-algaware/reference/load_station_mapper.md).

- standard_stations:

  Data frame from
  [`load_standard_stations()`](https://nodc-sweden.github.io/ifcb-algaware/reference/load_standard_stations.md).

## Value

A data.frame with columns: canonical_name, region, latitude, longitude,
pressure_dbar, chl_fluorescence, cruise, sample_date, file_path. Returns
NULL if no valid files found.
