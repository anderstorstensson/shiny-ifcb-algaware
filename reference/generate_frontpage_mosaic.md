# Generate a numbered frontpage mosaic

Extracts one random image per top taxon, annotates each with a sequence
number, and composes them into a single mosaic. Returns the mosaic image
together with the ordered taxa names for a figure caption.

## Usage

``` r
generate_frontpage_mosaic(
  classifications,
  taxa_lookup,
  samples,
  raw_data_path,
  non_bio,
  n_images = 15L,
  wide_summary = NULL,
  scale_micron_factor = NULL,
  scale_bar_um = 5,
  temp_dir = tempdir()
)
```

## Arguments

- classifications:

  Classification data.frame.

- taxa_lookup:

  Taxa lookup table.

- samples:

  Character vector of sample PIDs for this region.

- raw_data_path:

  Path to raw data directory (contains .roi files).

- non_bio:

  Character vector of non-biological class names to exclude.

- n_images:

  Number of taxa/images to include. Default 15.

- wide_summary:

  Optional wide-format summary from
  [`create_wide_summary()`](https://nodc-sweden.github.io/ifcb-algaware/reference/create_wide_summary.md)
  for ranking taxa by biovolume concentration.

- scale_micron_factor:

  Optional numeric microns-per-pixel factor for drawing scale bars in
  extracted PNGs.

- scale_bar_um:

  Scale bar length in microns. Default 5.

- temp_dir:

  Temporary directory for extracted PNGs.

## Value

A list with `mosaic` (magick image) and `taxa` (character vector of taxa
in numbered order), or `NULL`.
