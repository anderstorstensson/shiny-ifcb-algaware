# Get top taxa by total biovolume from a wide summary

Get top taxa by total biovolume from a wide summary

## Usage

``` r
get_top_taxa(wide_summary, n_taxa = 10L)
```

## Arguments

- wide_summary:

  Wide-format summary from
  [`create_wide_summary()`](https://nodc-sweden.github.io/ifcb-algaware/reference/create_wide_summary.md).

- n_taxa:

  Number of top taxa to return. Default 10.

## Value

Character vector of scientific names ordered by descending biovolume.
