# Format cruise-level data summary for LLM prompt

Format cruise-level data summary for LLM prompt

## Usage

``` r
format_cruise_summary_for_prompt(
  station_summary,
  taxa_lookup = NULL,
  unclassified_fractions = NULL
)
```

## Arguments

- station_summary:

  Full station_summary data frame.

- taxa_lookup:

  Optional taxa lookup with `HAB` and `warning_level` columns.

## Value

Character string with cruise-level overview.
