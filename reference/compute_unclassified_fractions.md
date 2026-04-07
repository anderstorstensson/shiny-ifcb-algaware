# Compute the percentage of unclassified images per station visit

Compute the percentage of unclassified images per station visit

## Usage

``` r
compute_unclassified_fractions(classifications, matched_metadata)
```

## Arguments

- classifications:

  Classification data.frame with `sample_name` and `class_name`.

- matched_metadata:

  Metadata with `pid`, `STATION_NAME`, `sample_time` matched to
  stations.

## Value

A named list mapping visit_id to the percentage (0-100) of unclassified
images at that visit.
