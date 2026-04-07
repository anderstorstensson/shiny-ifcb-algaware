# Extract month and year from cruise info string

Parses a string like "RV Svea March cruise, 2026-03-15 to 2026-03-22"
and returns "March 2026".

## Usage

``` r
extract_month_year(cruise_info)
```

## Arguments

- cruise_info:

  Cruise information string.

## Value

Character string with month and year, or empty string.
