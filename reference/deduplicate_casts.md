# Remove duplicate CTD casts for the same station/date

When a single station visit produces several CNV files (upcast +
downcast, or multiple instruments), this function keeps only the most
representative cast: upcasts (filenames beginning with a lowercase `u`
followed by an uppercase letter) are dropped first, then if multiple
downcasts remain for the same date the deepest is kept (ties broken
alphabetically by filename).

## Usage

``` r
deduplicate_casts(ctd_data)
```

## Arguments

- ctd_data:

  Data frame with at least columns `file_path`, `pressure_dbar`, and
  `sample_date`.

## Value

Filtered data frame.
