# Create a regional CTD composite figure (all standard stations)

Produces a multi-station patchwork figure for one geographic region.
Each row shows one station: left panel is the CTD fluorescence profile
(0–50 m, deduplicated casts, shared x-scale), right panel is the Chl-a
time series with smooth spline historical statistics.

## Usage

``` r
create_ctd_region_figure(
  ctd_data_full,
  lims_data_full = NULL,
  chl_stats,
  standard_stations,
  region,
  current_year,
  force_two_columns = FALSE
)
```

## Arguments

- ctd_data_full:

  Data frame from
  [`read_cnv_folder_all()`](https://nodc-sweden.github.io/ifcb-algaware/reference/read_cnv_folder_all.md).

- lims_data_full:

  Data frame from
  [`read_lims_data_all()`](https://nodc-sweden.github.io/ifcb-algaware/reference/read_lims_data_all.md),
  or NULL.

- chl_stats:

  Data frame from
  [`load_chl_statistics()`](https://nodc-sweden.github.io/ifcb-algaware/reference/load_chl_statistics.md).

- standard_stations:

  Data frame from
  [`load_standard_stations()`](https://nodc-sweden.github.io/ifcb-algaware/reference/load_standard_stations.md).

- region:

  Character region name.

- current_year:

  Integer year.

- force_two_columns:

  Logical; always use a two-column layout even without LIMS data (right
  column filled with spacers). Default FALSE.

## Value

A patchwork object, or NULL if there are no data for the region.

## Details

When `force_two_columns = TRUE` (used for report output), a spacer
column is added even when no LIMS data are available, keeping the
profile panels at a consistent width across all regions.

Month labels appear only on the bottom station row (English
abbreviations, locale-independent).
