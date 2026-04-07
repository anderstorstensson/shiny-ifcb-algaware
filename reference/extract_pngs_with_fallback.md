# Extract IFCB PNGs with fallback if scale-bar rendering fails

Retries extraction without a scale bar when the first attempt fails.
This avoids losing taxa from mosaics due to edge cases in ROI rendering.

## Usage

``` r
extract_pngs_with_fallback(
  roi_file,
  out_folder,
  roi_numbers,
  scale_bar_um = 5,
  scale_micron_factor = NULL
)
```

## Arguments

- roi_file:

  Path to a .roi file.

- out_folder:

  Output directory for extracted PNGs.

- roi_numbers:

  Integer ROI numbers to extract.

- scale_bar_um:

  Scale bar length in microns. Default 5.

- scale_micron_factor:

  Optional microns-per-pixel factor.

## Value

TRUE if an extraction attempt completed without error, FALSE otherwise.
