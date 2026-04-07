# Create a Chl-a time series with optional historical statistics

Plots smooth spline curves through 12 historical monthly means and the
std-dev ribbon (when available) plus current-year bottle observations.
The spline uses a cyclic boundary extension so the curve spans the full
calendar year. Current-year observations are shown as points only.

## Usage

``` r
create_chl_timeseries(
  station_lims,
  station_stats,
  station_label,
  current_year,
  show_x_axis = TRUE
)
```

## Arguments

- station_lims:

  Data frame of LIMS data for one station (depth-filtered to \\\le 20\\
  m). May be NULL.

- station_stats:

  Data frame of historical statistics (12 months) from
  [`load_chl_statistics()`](https://nodc-sweden.github.io/ifcb-algaware/reference/load_chl_statistics.md).
  May be NULL.

- station_label:

  Character label.

- current_year:

  Integer year.

- show_x_axis:

  Logical; render month labels on the x-axis.

## Value

A ggplot object, or NULL if there is nothing to plot.
