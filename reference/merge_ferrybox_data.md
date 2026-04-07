# Collect and merge ferrybox chlorophyll data

Fetches ferrybox data for the cruise sample timestamps, extracts
chlorophyll fluorescence (parameter 8063, QC-approved values only), and
computes a per-station mean that is merged into `station_summary`.
Returns an empty data frame (no error) when the ferrybox path is not
configured or no matching data is found.

## Usage

``` r
merge_ferrybox_data(config, matched, station_summary)
```

## Arguments

- config:

  Reactive values with settings (`ferrybox_path`).

- matched:

  Station-matched metadata with `sample_time` column.

- station_summary:

  Aggregated station data to receive `chl_mean`.

## Value

A list with `station_summary`, `chl_summary`, and `ferrybox_data`
fields.
