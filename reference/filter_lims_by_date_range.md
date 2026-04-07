# Filter a data frame to a date range

Keeps rows where `sample_date` is non-NA and falls within `date_min` to
`date_max` (inclusive). Used to restrict LIMS data to the cruise date
window.

## Usage

``` r
filter_lims_by_date_range(df, date_min, date_max)
```

## Arguments

- df:

  Data frame with a `sample_date` column.

- date_min:

  Minimum date (inclusive).

- date_max:

  Maximum date (inclusive).

## Value

Row-filtered `df`, or `df` unchanged if empty.
