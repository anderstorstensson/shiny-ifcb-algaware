# Create mosaics for top taxa in a region

Extracts PNGs from .roi files and creates mosaic images for the top N
taxa by biovolume.

## Usage

``` r
create_region_mosaics(
  wide_summary,
  classifications,
  sample_ids,
  raw_data_path,
  taxa_lookup,
  n_taxa = 5L,
  n_images = 32L,
  scale_micron_factor = NULL,
  scale_bar_um = 5,
  temp_dir = tempdir()
)
```

## Arguments

- wide_summary:

  Wide-format summary from
  [`create_wide_summary()`](https://nodc-sweden.github.io/ifcb-algaware/reference/create_wide_summary.md).

- classifications:

  Classification data.frame with `sample_name`, `roi_number`,
  `class_name`.

- sample_ids:

  Character vector of sample PIDs for this region.

- raw_data_path:

  Path to raw data (for .roi files).

- taxa_lookup:

  Taxa lookup table.

- n_taxa:

  Number of top taxa to create mosaics for.

- n_images:

  Number of images per mosaic.

- scale_micron_factor:

  Optional numeric microns-per-pixel factor for drawing scale bars in
  extracted PNGs.

- scale_bar_um:

  Scale bar length in microns. Default 5.

- temp_dir:

  Temporary directory for extracted PNGs.

## Value

A named list of magick image objects (names = taxon names).
