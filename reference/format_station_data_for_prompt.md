# Format station data as a text summary for the LLM prompt

Format station data as a text summary for the LLM prompt

## Usage

``` r
format_station_data_for_prompt(
  station_data,
  taxa_lookup = NULL,
  unclassified_pct = NULL
)
```

## Arguments

- station_data:

  Data frame with station_summary rows for one visit.

- taxa_lookup:

  Optional taxa lookup with `HAB` and `warning_level` columns.

## Value

Character string describing the station data.
