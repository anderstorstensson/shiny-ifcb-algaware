# Extract the current year from CTD data

Returns the year of the most recent sample date, or the current year if
no dates are available.

## Usage

``` r
extract_current_year_from_ctd(ctd_data_full)
```

## Arguments

- ctd_data_full:

  Data frame with a `sample_date` column.

## Value

Integer year.
