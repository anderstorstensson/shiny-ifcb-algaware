# Extract a single random image for a taxon

Picks one random ROI from the given taxon and extracts it as a PNG file.

## Usage

``` r
extract_random_taxon_image(
  taxon_name,
  classifications,
  taxa_lookup,
  sample_ids,
  raw_data_path,
  temp_dir,
  exclude_rois = NULL,
  scale_micron_factor = NULL,
  scale_bar_um = 5
)
```

## Arguments

- taxon_name:

  Scientific name of the taxon.

- classifications:

  Classification data.frame.

- taxa_lookup:

  Taxa lookup table.

- sample_ids:

  Character vector of sample PIDs.

- raw_data_path:

  Path to raw data (for .roi files).

- temp_dir:

  Temporary directory for extracted PNGs.

- exclude_rois:

  Optional data.frame with `sample_name` and `roi_number` columns to
  exclude from selection.

- scale_micron_factor:

  Optional numeric microns-per-pixel factor for drawing scale bars in
  extracted PNGs.

- scale_bar_um:

  Scale bar length in microns. Default 5.

## Value

A list with `path`, `taxon`, `sample_name`, `roi_number`, and
`n_available`, or NULL if no image found.
