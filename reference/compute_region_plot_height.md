# Compute plot height for a CTD region panel

Height scales with the number of unique stations in the region so that
dense regions get more vertical space.

## Usage

``` r
compute_region_plot_height(ctd_data_full, region)
```

## Arguments

- ctd_data_full:

  Data frame with `canonical_name` and `region` columns.

- region:

  Character region name.

## Value

Character CSS height string (e.g. `"780px"`).
