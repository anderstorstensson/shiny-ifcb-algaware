# Merge Custom Classes into Taxa Lookup

Appends custom class entries to a taxa lookup data frame for use in
report generation. Only adds classes not already present.

## Usage

``` r
merge_custom_taxa(taxa_lookup, custom_classes)
```

## Arguments

- taxa_lookup:

  Data frame with columns `clean_names`, `name`, `AphiaID`, `HAB`,
  `italic`.

- custom_classes:

  Data frame with the same columns plus `is_diatom`.

## Value

A new data frame combining both inputs (without duplicates).
