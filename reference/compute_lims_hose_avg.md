# Compute chlorophyll from LIMS hose (integrated 0-10 m) samples

Hose samples have `"SLA"` in their `SMPNO` field (e.g.
`"2026-N14_FALKENBERG-SLA_0-10-017_SYNC"`). They represent a
water-column integration over 0-10 m and require no depth averaging.

## Usage

``` r
compute_lims_hose_avg(lims_data)
```

## Arguments

- lims_data:

  Data frame from
  [`read_lims_data()`](https://nodc-sweden.github.io/ifcb-algaware/reference/read_lims_data.md)
  (must include the `SMPNO` column).

## Value

Data frame with station_short, latitude, longitude, chl_mean, or an
empty data frame if no hose samples are found.
