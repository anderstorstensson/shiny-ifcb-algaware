# Get ROI information for a specific taxon in a set of samples

Get ROI information for a specific taxon in a set of samples

## Usage

``` r
get_taxon_rois(classifications, taxa_lookup, taxon_name, sample_ids)
```

## Arguments

- classifications:

  Classification data.frame with `sample_name`, `roi_number`,
  `class_name`.

- taxa_lookup:

  Taxa lookup table.

- taxon_name:

  Scientific name of the taxon.

- sample_ids:

  Character vector of sample PIDs to search within.

## Value

A data.frame subset of classifications matching the taxon.
